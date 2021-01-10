module SVerilog (toSV) where

import Prelude hiding ((<>))
import MyPpr
import Syn (collectInputVars)
import SynType (synType)
import Util (varIsInModule)
import Outputable
import CoreSyn
import Name (Name, nameModule, getOccString, getName, mkSystemVarName)
import Module (moduleName, moduleNameString)
import Var (varName, varType, Var, mkLocalVar)
import Type (Type(..), splitFunTys, getTyVar)
import TyCon
import UniqSupply (UniqSM, initUs_, mkSplitUniqSupply, getUniqueM)
import IdInfo (IdDetails(VanillaId), vanillaIdInfo)
import FastString (FastString, mkFastString)

import Data.List
import ListX (stripAnyPrefix)
import SDocFunc

import MyPpr -- For dumping

toSV :: CoreBind -> IO (SDoc, Var)
toSV (NonRec b e) = toVModule b e
toSV (Rec bs) = error "Recursive bindings"

-- | A job is to convert a haskell syntax to SV statement
data Job = BindJob Var CoreExpr

instance Eq Job where
  (==) j0 j1
    | (BindJob v0 e0) <- j0,
      (BindJob v1 e1) <- j1
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
              nest 2 (doAllJobs [] ([BindJob vo e], empty))
              $+$
              text "endmodule")
            , vo)

-- | Generate the output port definition
outputDef :: Var -> SDoc
outputDef v = text "output" <+> (ppr . synType . varType)  v <+> (ppr v)

-- | Generate the input port definition
inputDef :: Var -> SDoc
inputDef v = text "input" <+> (ppr . synType . varType) v <+> (ppr v)


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
doJob (BindJob v e)
  = addStmt (text "always_comb" <+> ppr v <+> text "=" <+> (ppr $ getVExpr e) <> semi)
  -- Assuming the binder can always be implemented by combinational
  -- logic

addStmt :: SDoc -> Progress -> Progress
addStmt stmt (j, stmtDone) = (j, stmt $+$ stmtDone)

getVExpr :: CoreExpr -> SDocFunc
getVExpr (App e arg) = apply (getVExpr e) (ppr $ getVExpr arg)
getVExpr (Var v)
  | Just (tname, fname) <- splitTypeFuncMaybe vname
  = getBuiltInExpr tname fname
  | otherwise
  = SDocFunc [Body $ ppr v]
  where vname = getOccString $ varName v

getVExpr (Lam b exp) = getVExpr exp
getVExpr e = error ("Unexpected expression in getVExpr: " ++
                     (showSDocUnsafe $ myPprExpr e))

-- | Try to retrieve type and function from a string
-- Return Nothing if failed
splitTypeFuncMaybe :: String -> Maybe (String, String)
splitTypeFuncMaybe n
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

getBuiltInExpr :: String -> String -> SDocFunc
getBuiltInExpr tname fname
  | fname == "+" = binaryExpr "+"
  | fname == "-" = binaryExpr "-"
  | fname == "*" = binaryExpr "*"
  | fname == "negate" = unaryExpr "-"
  | fname == "abs" = if "Int" `isPrefixOf` tname
                     then funCall ("hada::abs" ++ (autoWidthStr $ drop 3 tname))
                     else varExpr
  | fname == "signum" = if "Int" `isPrefixOf` tname
                        then funCall ("hada::signum" ++ (autoWidthStr $ drop 3 tname))
                        else funCall ("hada::signumU" ++ (autoWidthStr $ drop 4 tname))
  | fname == "eq" = binaryExpr "=="
  | fname == "ne" = binaryExpr "!="
  | fname == "lt" = binaryExpr "<"
  | fname == "le" = binaryExpr "<="
  | fname == "gt" = binaryExpr ">"
  | fname == "ge" = binaryExpr ">="
  | fname == ".&." = binaryExpr "&"
  | fname == ".|." = binaryExpr "|"
  | fname == "xor" = binaryExpr "^"
  | fname == "complement" = unaryExpr "~"
  | otherwise = error $ "Unknown builtin function " ++ fname
  where autoWidthStr str =  case str of
                              [] -> if (maxBound::Word) == 0xFFFFFFFF
                                    then "32"
                                    else "64"
                              otherwise -> str

binaryExpr :: String -> SDocFunc
binaryExpr op = SDocFunc ((Hole 0) : (Body (space <> text op <> space)) : (Hole 1) : [])

unaryExpr :: String -> SDocFunc
unaryExpr op = SDocFunc ((Body (text op)) : (Hole 0) : [])

varExpr :: SDocFunc
varExpr = SDocFunc (Hole 0 : [])

funCall :: String -> SDocFunc
funCall fName = SDocFunc (Body (text fName <> lparen) : Variadic [] : Body rparen : [])
