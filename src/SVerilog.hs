module SVerilog (toSV) where

import Prelude hiding ((<>))
import MyPpr
import Syn (collectInputVars)
import SynType (synType)
import Util (nameModuleStringMaybe)
import Outputable
import CoreSyn
import Name (Name, getOccString, getName, mkSystemVarName, nameUnique, NamedThing)
import Module (moduleName, moduleNameString)
import Var (varName, varType, varUnique, Var, mkLocalVar, isLocalVar, isCoVar)
import Type (Type(..), splitFunTys, getTyVar)
import TyCon
import UniqSupply (UniqSM, initUs_, mkSplitUniqSupply, getUniqueM)
import IdInfo (IdDetails(VanillaId), vanillaIdInfo)
import FastString (FastString, mkFastString)
import Literal

import Data.List
import ListX (stripAnyPrefix)
import SDocFunc

toSV :: CoreBind -> IO (SDoc, Var)
toSV (NonRec b e) = toVModule b e
toSV (Rec bs) = error "Recursive bindings"

-- | A job is to convert a haskell syntax to SV statement
data Job = BindJob [Var] SDocFunc CoreExpr
           -- For printing a bind of vars to expression, optionally
           -- wrapped by a SDocFunc
         | DefJob Var
           

instance Eq Job where
  (==) j0 j1
    | (BindJob v0 df0 e0) <- j0,
      (BindJob v1 df1 e1) <- j1
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
              nest 2 (doAllJobs [] ([BindJob [vo] bypassSDocFunc e], empty))
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
    in (addJobs js) . (addStmt (text "always_comb" <+> bindLHS vs <+> text "=" <+> ppr (apply docfunc $ ppr stmt) <> semi))
  -- Assuming the binder can always be implemented by combinational
  -- logic
doJob (DefJob v) = addStmt (varDef v <+> varVId v <> semi)

bindLHS :: [Var] -> SDoc
bindLHS (v:[]) = varVId v -- Only one var
bindLHS vs = braces $ pprWithCommas varVId vs

addStmt :: SDoc -> Progress -> Progress
addStmt stmt (j, stmtDone) = (j, stmt $+$ stmtDone)

justDoc :: SDocFunc -> ([Job], SDocFunc)
justDoc sdoc = ([], sdoc)

addJobs :: [Job] -> ([Job], a) -> ([Job], a)
addJobs js (jobs, v) = (js ++ jobs, v)

getExpr :: CoreExpr -> ([Job], SDocFunc)
getExpr (App e arg) = let (j0, stmt0) = getExpr e
                          (j1, stmt1) = getExpr arg
                      in (j0 ++ j1, apply stmt0 (ppr stmt1))
getExpr (Var v) = justDoc $ getVarExpr v
getExpr (Lam b exp) = getExpr exp
getExpr (Case ce v t alts) = getCaseExpr ce alts
getExpr (Lit (LitNumber _ v _)) = justDoc $ SDocFunc [Body $ ppr v]
getExpr e = error ("Unexpected expression in getExpr: " ++
                     (showSDocUnsafe $ myPprExpr e))

getVarExpr :: Var -> SDocFunc
getVarExpr v
  | Just (tname, fname) <- splitBuiltinTypeFuncMaybe vname
  = getBuiltInExpr tname fname
  | ofIntCtorName v
  = bypassSDocFunc -- boxed integer values are treated identical as
                  -- unboxed ones
  | (a:b:[]) <- vname,
    a `elem` ['+', '-', '*'],
    b == '#'
    -- +#, -#, *# on unboxed int 
  = binarySDocFunc (a:[])
  | vname == "plusWord#"
  = binarySDocFunc "+"
  | Just tail <- stripPrefix "narrow" vname
  , Just (_, tail') <- stripAnyPrefix ["8", "16", "32"] tail
  , tail' == "Int#" || tail' == "Word#"
    -- Narrowing function
  = funCallSDocFunc ("hada::" ++ init vname)
  | otherwise
  -- Just a variable, print its name
  = SDocFunc [Body $ varVId v]
  where vname = getOccString $ varName v

-- | Try to retrieve type and function from a string
-- Return Nothing if failed
splitBuiltinTypeFuncMaybe :: String -> Maybe (String, String)
splitBuiltinTypeFuncMaybe n
  | Just (_, tail) <- stripAnyPrefix ["$fNum", "$fBits"] n
  -- Split a string of form like "$f[CNAME][TYPE]_$c[FUNC]", where
  --  [CNAME] must be either "Num" or "Bits", [TYPE] must be "Int",
  --  "Int8/16/32/34", "Word" or "Word8/16/32/64"
  = case stripAnyPrefix ["Int", "Word" ] tail of
      Just (tname, tail1) ->
        case stripAnyPrefix ["8", "16", "32", "64", ""] tail1 of
          Just (wname, tail2) ->
            case stripPrefix "_$c" tail2 of
              Just fname -> Just (tname ++ wname, fname)
              otherwise -> Nothing
          otherwise -> Nothing
      otherwise -> Nothing
  | Just (fname, tail) <- stripAnyPrefix ["eq", "neq", "lt", "le", "gt", "ge"] n
  -- Split a string of form like "[FUNC][TYPE]", where [FUNC] must be
  -- one of the above list, and [TYPE] must be "Int", "Int8/16/32/64",
  -- "Word" or "Word8/16/32/64"
  = case stripAnyPrefix ["Int", "Word"] tail of
      Just (tname, tail1) ->
        case elemIndex tail1 ["8", "16", "32", "64", ""] of
          Just _ -> Just (tail, fname)
          Nothing -> Nothing
      Nothing -> Nothing
  | otherwise
  = Nothing

-- | Whether the name of something is one of the builtin integer type
-- constructors of I#, I8#, I16#, I32#, I64# in GHC.Int and W#, W8#,
-- W16#, W32#, W64# in GHC.Word
ofIntCtorName :: NamedThing a => a -> Bool
ofIntCtorName thing
  | Just s <- nameModuleStringMaybe $ getName thing,
    s `elem` ["GHC.Types", "GHC.Int", "GHC.Word"]
  = case stripAnyPrefix ["I", "W"] nstr of
      Just (_, tail0) -> case stripAnyPrefix ["64", "32", "16", "8", ""] tail0 of
                           Just (_, tail1) -> tail1 == "#"
                           otherwise -> False
      otherwise -> False
  | otherwise
  = False
  where nstr = getOccString $ getName thing

getBuiltInExpr :: String -> String -> SDocFunc
getBuiltInExpr tname fname
  | fname == "+" = binarySDocFunc "+"
  | fname == "-" = binarySDocFunc "-"
  | fname == "*" = binarySDocFunc "*"
  | fname == "negate" = unarySDocFunc "-"
  | fname == "abs" = if "Int" `isPrefixOf` tname
                     then funCallSDocFunc ("hada::abs" ++ (autoWidthStr $ drop 3 tname))
                     else bypassSDocFunc
  | fname == "signum" = if "Int" `isPrefixOf` tname
                        then funCallSDocFunc ("hada::signum" ++ (autoWidthStr $ drop 3 tname))
                        else funCallSDocFunc ("hada::signumU" ++ (autoWidthStr $ drop 4 tname))
  | fname == "eq" = binarySDocFunc "=="
  | fname == "ne" = binarySDocFunc "!="
  | fname == "lt" = binarySDocFunc "<"
  | fname == "le" = binarySDocFunc "<="
  | fname == "gt" = binarySDocFunc ">"
  | fname == "ge" = binarySDocFunc ">="
  | fname == ".&." = binarySDocFunc "&"
  | fname == ".|." = binarySDocFunc "|"
  | fname == "xor" = binarySDocFunc "^"
  | fname == "complement" = unarySDocFunc "~"
  | otherwise = error $ "Unknown builtin function " ++ fname
  where autoWidthStr str =  case str of
                              [] -> if (maxBound::Word) == 0xFFFFFFFF
                                    then "32"
                                    else "64"
                              otherwise -> str

-- May add new jobs when converting case expression
getCaseExpr ::
  CoreExpr -> -- The case expression
  [Alt Var] -> -- The alternative list
  ([Job], SDocFunc)
getCaseExpr ce ((altcon, vs, e):[]) = addJobs newJobs $ getExpr e -- The last alternative, unconditional
  where newJobs = ((BindJob vs sdf ce) : (map DefJob vs))
        sdf
          | DataAlt datacon <- altcon
          , ofIntCtorName datacon
          = funCallSDocFunc ("hada::match" ++ (init $ getOccString $ getName datacon))
          | otherwise
          = bypassSDocFunc

getCaseExpr _ _ = error "Unsupported case expression"


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
