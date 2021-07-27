module SVerilog (toSV) where

import Prelude hiding ((<>))
import MyPpr
import Syn (collectInputVars)
import SynType (synType, showSV)
import NameX (moduleStringMaybe, isInModule)
import TypeX
import VHText (VHText(..), rightSep, vcatAll, hcat, vcat, indent)
import TextExpr
import JobGraph

import qualified Outputable as O
import CoreSyn
import Name (Name, getOccString, getName, mkSystemVarName, nameUnique, NamedThing)
import Module (moduleName, moduleNameString)
import Var (varName, varType, varUnique, Var, mkLocalVar, isLocalVar, isCoVar)
import Type (Type(..), splitFunTys)
import TyCon
import DataCon (dataConRepType, dataConTag)
import UniqSupply (UniqSM, initUs_, mkSplitUniqSupply, getUniqueM)
import IdInfo (IdDetails(VanillaId), vanillaIdInfo)
import FastString (FastString, mkFastString)
import Literal

import Data.List
import ListX (stripAnyPrefix)
import Data.Bits (finiteBitSize)
import Data.Maybe (isJust)
import Algebra.Graph (overlay, isEmpty)

-- Convert a CoreBind to a SVerilog module, represented by VHText, and
-- the module's output signal, represented by a Var
toSV :: CoreBind -> IO (VHText, Var)
toSV (NonRec b e) = toVModule b e
toSV (Rec bs) = error "Recursive bindings"

-- | A mobule job is to convert a haskell bind syntax to SV statement
data ModuleJob = BindJob [Var] CoreExpr
                 -- For printing a bind of vars to expression
               | DefJob Var
           

instance Eq ModuleJob where
  (BindJob v0 _) == (BindJob v1 _) = v0 == v1
  (DefJob v0) == (DefJob v1) = v0 == v1
  j0 == j1 = False

type ModuleJobGraph = JobGraph ModuleJob VHText

-- Translate a CoreBind, which has been splitted into a CoreBndr and a
-- CoreExpr into SVerilog text, formatted in VHText, and the created
-- output var for wrapper files
toVModule :: CoreBndr -> CoreExpr -> IO (VHText, Var)
toVModule b e =
  do us <- mkSplitUniqSupply 'a'
     let (_, otype) = splitFunTys $ varType b
         vo = initUs_ us $ mkAutoVar (mkFastString "o") otype
         vis = collectInputVars e
         head = Text ("module " ++ getOccString b ++ "(")
                `hcat`
                -- Port definition
                rightSep "," (vcatAll (outputDef vo:map inputDef vis))
                `hcat`
                Text ");"
         -- Initial job graph. Contains one to-do BindJob and one done
         -- DefJob for the output variable. The DefJob has empty
         -- result as the output variable is already defined in the
         -- port list.
         initJG = vertexTodo (BindJob [vo] e) `overlay`
                  vertexDone (DefJob vo) Empty
         doneJG = doAllJobs synInModule initJG
         (body, remain) = resultTopoFoldr (flip vcat) Empty doneJG
     return (if isEmpty remain
              then (head `vcat` indent 2 body `vcat` Text "endmodule",
                    vo)
              else error "Cyclic dependency found on module body")

-- | Generate the output port definition
outputDef :: Var -> VHText
outputDef v = Text ("output " ++ varDef v ++ " " ++ varVId v)

-- | Generate the input port definition
inputDef :: Var -> VHText
inputDef v = Text ("input " ++ varDef v ++ " " ++ varVId v)

varDef :: Var -> String
varDef = showSV . synType . varType

newUniqueName :: FastString -> UniqSM Name
newUniqueName str = do u <- getUniqueM
                       return $ mkSystemVarName u str

mkAutoVar :: FastString -> Type -> UniqSM Var
mkAutoVar vname vtype = do n <- newUniqueName vname
                           return $ mkLocalVar VanillaId n vtype vanillaIdInfo


-- | Synthesize in-module declarations and statements
synInModule :: ModuleJob -> ([ModuleJob], VHText)
synInModule (BindJob vs e)
  = let (js, stmt) = getExpr e
        combStmt = Text ("always_comb " ++ concatSV (map varVId vs) ++ " = " ++
                         showCommon stmt ++ ";")
    in (map DefJob vs ++ js, combStmt)
  -- Assuming the binder can always be implemented by combinational
  -- logic
synInModule (DefJob v) = ([], Text (varDef v ++ " " ++ varVId v ++ ";"))

concatSV :: [String] -> String
concatSV (s:[]) = s -- Only one var
concatSV s = "{" ++ intercalate ", " s ++ "}"

justExpr :: TextExpr -> ([ModuleJob], TextExpr)
justExpr expr = ([], expr)

addJobs :: [ModuleJob] -> ([ModuleJob], a) -> ([ModuleJob], a)
addJobs js (jobs, expr) = (js ++ jobs, expr)

getExpr :: CoreExpr -> ([ModuleJob], TextExpr)
getExpr (App e arg) = let (j0, stmt0) = getExpr e
                          (j1, stmt1) = getExpr arg
                      in (j0 ++ j1, apply stmt0 stmt1)
getExpr (Var v) = justExpr $ getVarExpr v
getExpr (Lam b exp) = getExpr exp
getExpr (Case ce v t alts) = getCaseExpr ce v t alts
getExpr (Lit (LitNumber _ v _)) = justExpr $ LeafExpr $ show v
getExpr (Lit lit) = if lit == castLongintLit
                    then justExpr $ Func "longint'" []
                         -- Built-in case function acting as the rhs
                         -- equivalent of integer data constructors.
                    else error $ "Unknown literal " ++ O.showSDocUnsafe (O.ppr lit)
getExpr (Type t) = justExpr $ LeafExpr $ O.showSDocUnsafe $ O.ppr t
getExpr e = error ("Unexpected expression in getExpr: " ++
                     (O.showSDocUnsafe $ myPprExpr e))

castLongintLit = mkLitString "longint'"
-- Return the integer name of give byte-width
svIntName :: Int -> String
svIntName 1 = "byte"
svIntName 2 = "shortint"
svIntName 4 = "int"
svIntName _ = "longint" -- Assuming 4 bytes

getVarExpr :: Var -> TextExpr
getVarExpr v
  | Just (tname, fname) <- splitBuiltinTypeFuncMaybe vname
  = getBuiltInExpr tname fname
  | Just bw <- ofIntCtorName v
  = Func (svIntName bw ++ "'") []
  -- logical and/or
  | vname == "||" 
  = opOr
  | vname == "&&"
  = opAnd
  --logical not
  | vname == "not"
  = opNot
  | isInModule v "GHC.Prim"
  = getPrimExpr v
  | vname == "True" && isInModule v "GHC.Types" 
  = LeafExpr "1'b1"
  | vname == "False" && isInModule v "GHC.Types"
  = LeafExpr "1'b0"
  | otherwise
  -- Just a variable, print its name
  = LeafExpr $ varVId v
  where vname = getOccString $ varName v

-- | Try to retrieve type and function from a string
-- Return Nothing if failed
splitBuiltinTypeFuncMaybe :: String -> Maybe (String, String)
splitBuiltinTypeFuncMaybe n
  | Just (_, tail) <- stripAnyPrefix ["$fNum", "$fBits"] n
  -- Split a string of form like "$f[CNAME][TYPE]_$c[FUNC]", where
  --  [CNAME] must be either "Num" or "Bits", [TYPE] must be "Int",
  --  "Int8/16/32/34", "Word" or "Word8/16/32/64"
  = do (tname, tail1) <- stripAnyPrefix ["Int", "Word" ] tail
       (wname, tail2) <- stripAnyPrefix ["8", "16", "32", "64", ""] tail1
       fname <- stripPrefix "_$c" tail2
       return (tname ++ wname, fname)
  | Just (fname, tail) <- stripAnyPrefix ["eq", "neq", "lt", "le", "gt", "ge"] n
  -- Split a string of form like "[FUNC][TYPE]", where [FUNC] must be
  -- one of the above list, and [TYPE] must be "Int", "Int8/16/32/64",
  -- "Word" or "Word8/16/32/64"
  = do (tname, tail1) <- stripAnyPrefix ["Int", "Word"] tail
       _ <- elemIndex tail1 ["8", "16", "32", "64", ""]
       return (tail, fname)
  | otherwise
  = Nothing

-- | Whether the name of something is one of the builtin integer type
-- constructors of I#, I8#, I16#, I32#, I64# in GHC.Int and W#, W8#,
-- W16#, W32#, W64# in GHC.Word. If so, return the integer's
-- byte-width. Otherwise, return Nothing
ofIntCtorName :: NamedThing a => a -> Maybe Int
ofIntCtorName thing
  | Just s <- moduleStringMaybe thing,
    s `elem` ["GHC.Types", "GHC.Int", "GHC.Word"]
  = case getOccString $ getName thing of
      (s:ss) -> if s == 'I' || s == 'W'
                then case ss of
                       "#" -> Just (finiteBitSize (0::Int) `quot` 8)
                       "8#" -> Just 1
                       "16#" -> Just 2
                       "32#" -> Just 4
                       "64#" -> Just 8
                       otherwise -> Nothing
                else Nothing
      otherwise -> Nothing
  | otherwise
  = Nothing
  where nstr = getOccString $ getName thing

getBuiltInExpr :: String -> String -> TextExpr
getBuiltInExpr tname fname
  | fname == "+" = opPlus
  | fname == "-" = opMinus
  | fname == "*" = opMul
  | fname == "negate" = opNeg
  | fname == "abs" = if "Int" `isPrefixOf` tname
                     then Func ("hada::abs" ++ (autoWidthStr $ drop 3 tname)) []
                     else Identity
  | fname == "signum" = Func (if "Int" `isPrefixOf` tname
                              then "hada::signum" ++ (autoWidthStr $ drop 3 tname)
                              else "hada::signumU" ++ (autoWidthStr $ drop 4 tname)) []
  | fname == "eq" = opEq
  | fname == "ne" = opNe
  | fname == "lt" = opLt
  | fname == "le" = opLe
  | fname == "gt" = opGt
  | fname == "ge" = opGe
  | fname == ".&." = opBitAnd
  | fname == ".|." = opBitOr
  | fname == "xor" = opXor
  | fname == "complement" = opComplement
  | otherwise = error $ "Unknown builtin function " ++ fname
  where autoWidthStr str =  case str of
                              [] -> if (maxBound::Word) == 0xFFFFFFFF
                                    then "32"
                                    else "64"
                              otherwise -> str

getPrimExpr :: Var -> TextExpr
getPrimExpr v
  -- ^ Construct boxed integer values from unboxed ones
  | vname == "plusWord#" = opPlus
  | vname == "+#" = opPlus
  | vname == "-#" = opMinus
  | vname == "*#" = opMul
  | Just tail <- stripPrefix "narrow" vname
  , Just (_, tail') <- stripAnyPrefix ["8", "16", "32"] tail
  , tail' == "Int#" || tail' == "Word#"
  -- Narrowing functions, ignored as the narrowing is done by the "hada::ctor" functions
  = Identity
  | vname == "uncheckedIShiftL#" ||
    vname == "uncheckedShiftL#"
  = opShL
  | vname == "uncheckedIShiftRA#" = opShRA
  | vname == "uncheckedShiftRL#" = opShRL
  | vname == "negateInt#" = opNeg
  | vname == "tagToEnum#"
  = MetaFunc"hada::tagToEnum"
  | vname == "==#" = opEq
  | vname == "andI#" = opBitAnd
  | vname == "orI#" = opBitOr
  | vname == "xorI#" = opXor
  | vname == "notI#" = opBitNeg
  | otherwise = error $ "Unknown prime var " ++ vname
  where vname = getOccString $ getName v

-- May add new jobs when converting case expression
getCaseExpr ::
  CoreExpr -> -- The case expression
  Var -> -- The bind var
  Type -> -- The type
  [Alt Var] -> -- The alternative list
  ([ModuleJob], TextExpr)
getCaseExpr ce cv ct ((altcon, vs, e):[])
  = addJobs newJobs $ getExpr e -- The last alternative, unconditional
  where newJobs
          | [] <- vs
          = [] -- No new jobs as there is no matched vars
          | otherwise
          = [BindJob vs ce']
        ce'
          | DataAlt datacon <- altcon
          , isJust $ ofIntCtorName datacon
          = App (Lit (mkLitString "longint'")) ce
          | otherwise
          = ce

getCaseExpr ce cv ct ((altcon, vs, e):moreAlts)
  = (jc : (jt ++ jf), condExpr cond true false)
  where cond = matchKey cv ++ " == " ++ matchValue altcon
        jc = BindJob [cv] ce
        (jt, true) = getExpr e
        (jf, false) = getCaseExpr ce cv ct moreAlts

-- Derive the pattern matching key from a variable
matchKey :: Var -> String
matchKey v
  | isBoolType $ varType v
  -- Matching againt a Bool, the var itself is the key.
  = varVId v
  | otherwise
  = error $ "Unknown match key for variable " ++ (getOccString v)

-- Derive the pattern matching value from an alternative constructor
matchValue :: AltCon -> String
matchValue altcon
  | DataAlt dataCon <- altcon
  , isBoolType $ dataConRepType dataCon
  = case dataConTag dataCon of
      0 -> "1'b1" -- True
      _ -> "1'b0" -- False


condExpr :: String -> TextExpr -> TextExpr -> TextExpr
condExpr cond true false = LeafExpr (cond ++ " ? " ++
                                     showCommon true ++ " : " ++
                                     showCommon false)

-- | Convert any string having character other than a-z, A-Z, 0-9 and
-- _ to escaped Verilog Identifier.
vId :: String -> String
vId id
  | all (\c -> ((c >= 'a' && c <= 'z') ||
                 (c >= 'A' && c <= 'Z') ||
                 (c >= '0' && c <= '9') ||
                 c == '_')) id
  = id -- Regular identifier, no escaping
  | otherwise
  = ('\\':id) ++ " "

varVId :: Var -> String
varVId = vId . (\v -> (getOccString $ varName v) ++ "_" ++ (show $ varUnique v))

-- Operators and their precedences
-- Unary !, ~, +, - : 14
-- {}, {{}}         : 13
-- ()               : 12
-- **               : 11
-- *, /, %          : 10
-- Binary +, -      :  9
-- <<, >>, <<<, >>> :  8
-- <, <=, >, >=     :  7
-- ==, !=, ===, !== :  6
-- &, ~&            :  5
-- ^, ~^            :  4
-- |, ~|            :  3
-- &&               :  2
-- ||               :  1
-- ?:               :  0
opComplement = unaryOp "~" 14
opNot = unaryOp "!" 14
opNeg = unaryOp "-" 14
opBitNeg = unaryOp "~" 14
opMul = binaryOp "*" 10
opPlus = binaryOp "+" 9
opMinus = binaryOp "-" 9
opShL = binaryOp "<<" 8
opShRA = binaryOp ">>>" 8
opShRL = binaryOp ">>" 8
opLt = binaryOp "<" 7
opLe = binaryOp "<=" 7
opGt = binaryOp ">" 7
opGe = binaryOp ">=" 7
opEq = binaryOp "==" 6
opNe = binaryOp "!=" 6
opBitAnd = binaryOp "&" 5
opXor = binaryOp "^" 4
opBitOr = binaryOp "|" 3
opAnd = binaryOp "&&" 2
opOr = binaryOp "||" 1
