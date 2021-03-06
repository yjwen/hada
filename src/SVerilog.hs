module SVerilog (toSV) where

import Prelude hiding ((<>))
import MyPpr
import Syn (collectInputVars)
import SynType (synType)
import NameX (moduleStringMaybe, isInModule)
import TypeX
import Outputable
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
import SDocExpr

toSV :: CoreBind -> IO (SDoc, Var)
toSV (NonRec b e) = toVModule b e
toSV (Rec bs) = error "Recursive bindings"

-- | A job is to convert a haskell syntax to SV statement
data Job = BindJob [Var] SDocExpr CoreExpr
           -- For printing a bind of vars to expression, optionally
           -- wrapped by a SDocExpr
         | DefJob Var
           

instance Eq Job where
  (==) j0 j1
    | (BindJob v0 df0 e0) <- j0,
      (BindJob v1 df1 e1) <- j1
    = v0 == v1
    | (DefJob v0) <- j0,
      (DefJob v1) <- j1
    = v0 == v1
    | otherwise
    = False

type Progress = ( [Job] -- Jobs undone
                , SDoc -- SV statements done
                )

-- Return the translated Verilog SDoc, and the created output var for
-- wrapper files
toVModule :: CoreBndr -> CoreExpr -> IO (SDoc, Var)
toVModule b e =
  do us <- mkSplitUniqSupply 'a'
     let (_, otype) = splitFunTys $ varType b
         vo = initUs_ us $ mkAutoVar (mkFastString "o") otype
         vis = collectInputVars e
     return ((text "module" <+> ppr b <+>
              -- Port definition
              parens (vcat $ punctuate (text ", ")  ((outputDef vo):(map inputDef vis))) <> semi
              $+$
              -- Body
              nest 2 (doAllJobs [] ([BindJob [vo] SDocIdentity e], empty))
              $+$
              text "endmodule")
            , vo)

-- | Generate the output port definition
outputDef :: Var -> SDoc
outputDef v = text "output" <+> varDef v <+> varVId v

-- | Generate the input port definition
inputDef :: Var -> SDoc
inputDef v = text "input" <+> varDef v <+> varVId v

varDef :: Var -> SDoc
varDef = ppr . synType . varType

newUniqueName :: FastString -> UniqSM Name
newUniqueName str = do u <- getUniqueM
                       return $ mkSystemVarName u str

mkAutoVar :: FastString -> Type -> UniqSM Var
mkAutoVar vname vtype = do n <- newUniqueName vname
                           return $ mkLocalVar VanillaId n vtype vanillaIdInfo


-- | Do all jobs in progress and return the resulted SV statements
doAllJobs :: [Job] -> Progress -> SDoc
doAllJobs doneJobs ((j:jobs), doc) 
  | elem j doneJobs = doAllJobs doneJobs (jobs, doc) -- j already done. Search more
  | otherwise = doAllJobs (j:doneJobs) $ doJob j (jobs, doc)
doAllJobs _ ([], doc) = doc -- No job remain. All done

-- | Do one job and update progress. SV statements will be
-- updated. Undone jobs may be updated as well if more jobs are
-- discovered when doing this job.
doJob :: Job -> Progress -> Progress
doJob (BindJob vs docfunc e)
  = let (js, stmt) = getExpr e
    in (addJobs js) . (addStmt (text "always_comb" <+> bindLHS vs <+> text "=" <+> ppr (apply docfunc stmt) <> semi))
  -- Assuming the binder can always be implemented by combinational
  -- logic
doJob (DefJob v) = addStmt (varDef v <+> varVId v <> semi)

bindLHS :: [Var] -> SDoc
bindLHS (v:[]) = varVId v -- Only one var
bindLHS vs = braces $ pprWithCommas varVId vs

addStmt :: SDoc -> Progress -> Progress
addStmt stmt (j, stmtDone) = (j, stmt $+$ stmtDone)

justDoc :: SDocExpr -> ([Job], SDocExpr)
justDoc sdoc = ([], sdoc)

addJobs :: [Job] -> ([Job], a) -> ([Job], a)
addJobs js (jobs, v) = (js ++ jobs, v)

getExpr :: CoreExpr -> ([Job], SDocExpr)
getExpr (App e arg) = let (j0, stmt0) = getExpr e
                          (j1, stmt1) = getExpr arg
                      in (j0 ++ j1, apply stmt0 stmt1)
getExpr (Var v) = justDoc $ getVarExpr v
getExpr (Lam b exp) = getExpr exp
getExpr (Case ce v t alts) = getCaseExpr ce v t alts
getExpr (Lit (LitNumber _ v _)) = justDoc $ SDocConst $ ppr v
getExpr (Type t) = justDoc $ literalSDocFunc $ ppr t
getExpr e = error ("Unexpected expression in getExpr: " ++
                     (showSDocUnsafe $ myPprExpr e))

getVarExpr :: Var -> SDocExpr
getVarExpr v
  | Just (tname, fname) <- splitBuiltinTypeFuncMaybe vname
  = getBuiltInExpr tname fname
  | ofIntCtorName v
  = funCallSDocFunc ("hada::cons" ++ init vname)
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
  = literalSDocFunc $ text "1'b1"
  | vname == "False" && isInModule v "GHC.Types"
  = literalSDocFunc $ text "1'b0"
  | otherwise
  -- Just a variable, print its name
  = literalSDocFunc $ varVId v
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
-- W16#, W32#, W64# in GHC.Word
ofIntCtorName :: NamedThing a => a -> Bool
ofIntCtorName thing
  | Just s <- moduleStringMaybe thing,
    s `elem` ["GHC.Types", "GHC.Int", "GHC.Word"]
  = case stripAnyPrefix ["I", "W"] nstr of
      Just (_, tail0) -> case stripAnyPrefix ["64", "32", "16", "8", ""] tail0 of
                           Just (_, tail1) -> tail1 == "#"
                           otherwise -> False
      otherwise -> False
  | otherwise
  = False
  where nstr = getOccString $ getName thing

getBuiltInExpr :: String -> String -> SDocExpr
getBuiltInExpr tname fname
  | fname == "+" = opPlus
  | fname == "-" = opMinus
  | fname == "*" = opMul
  | fname == "negate" = opNeg
  | fname == "abs" = if "Int" `isPrefixOf` tname
                     then funCallSDocFunc ("hada::abs" ++ (autoWidthStr $ drop 3 tname))
                     else SDocIdentity
  | fname == "signum" = if "Int" `isPrefixOf` tname
                        then funCallSDocFunc ("hada::signum" ++ (autoWidthStr $ drop 3 tname))
                        else funCallSDocFunc ("hada::signumU" ++ (autoWidthStr $ drop 4 tname))
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

getPrimExpr :: Var -> SDocExpr
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
  = SDocIdentity
  | vname == "uncheckedIShiftL#" ||
    vname == "uncheckedShiftL#"
  = opShL
  | vname == "uncheckedIShiftRA#" = opShRA
  | vname == "uncheckedShiftRL#" = opShRL
  | vname == "negateInt#" = opNeg
  | vname == "tagToEnum#"
  = SDocFunc maxBound [Body (text "hada::tagToEnum"),
                       Hole 0 maxBound,
                       Body lparen,
                       Hole 1 minBound,
                       Body rparen]
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
  ([Job], SDocExpr)
getCaseExpr ce cv ct ((altcon, vs, e):[])
  = addJobs newJobs $ getExpr e -- The last alternative, unconditional
  where newJobs
          | [] <- vs
          = [] -- No new jobs as there is no matched vars
          | otherwise
          = ((BindJob vs sdf ce) : (map DefJob vs))
        sdf
          | DataAlt datacon <- altcon
          , ofIntCtorName datacon
          = funCallSDocFunc ("hada::match" ++ (init $ getOccString $ getName datacon))
          | otherwise
          = SDocIdentity
getCaseExpr ce cv ct ((altcon, vs, e):moreAlts)
  = (jc ++ jt ++ jf, condExpr cond true false)
  where cond = SDocFunc 0 [Body (matchKey cv <+> text "==" <+> matchValue altcon)]
        jc = [BindJob [cv] SDocIdentity ce, DefJob cv]
        (jt, true) = getExpr e
        (jf, false) = getCaseExpr ce cv ct moreAlts

-- Derive the pattern match key from a variable
matchKey :: Var -> SDoc
matchKey v
  | isBoolType $ varType v
  -- Matching againt a Bool, the var itself is the key.
  = varVId v
  | otherwise
  = error $ "Unknown match key for variable " ++ (getOccString v)
  where vtype = varType v

-- Derive the pattern matching value from an alternative constructor
matchValue :: AltCon -> SDoc
matchValue altcon
  | DataAlt dataCon <- altcon
  , isBoolType $ dataConRepType dataCon
  = case dataConTag dataCon of
      0 -> text "1'b1" -- True
      _ -> text "1'b0" -- False


condExpr :: SDocExpr -> SDocExpr -> SDocExpr -> SDocExpr
condExpr cond true false = SDocFunc 0 [Body (ppr cond <+> text "?" <+>
                                             ppr true <+> text ":" <+>
                                             ppr false)]

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

varVId :: Var -> SDoc
varVId = text . vId . (\v -> (getOccString $ varName v) ++ "_" ++ (show $ varUnique v))

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
opComplement = unarySDocFunc "~" 14
opNot = unarySDocFunc "!" 14
opNeg = unarySDocFunc "-" 14
opBitNeg = unarySDocFunc "~" 14
opMul = binarySDocFunc "*" 10
opPlus = binarySDocFunc "+" 9
opMinus = binarySDocFunc "-" 9
opShL = binarySemiConst "<<" 8
opShRA = binarySemiConst ">>>" 8
opShRL = binarySemiConst ">>" 8
opLt = binarySDocFunc "<" 7
opLe = binarySDocFunc "<=" 7
opGt = binarySDocFunc ">" 7
opGe = binarySDocFunc ">=" 7
opEq = binarySDocFunc "==" 6
opNe = binarySDocFunc "!=" 6
opBitAnd = binarySDocFunc "&" 5
opXor = binarySDocFunc "^" 4
opBitOr = binarySDocFunc "|" 3
opAnd = binarySDocFunc "&&" 2
opOr = binarySDocFunc "||" 1
