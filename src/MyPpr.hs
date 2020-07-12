module MyPpr where
import HscTypes (mg_binds, ModGuts)
import Name(Name, pprNameDefnLoc)
import CoreSyn(Bind(NonRec, Rec), CoreBind,
               Expr(Var, Lit, App, Lam, Let, Case, Cast, Tick, Type, Coercion))
-- import Var(Var, varName, varUnique, varType)
import Var
import Id
import IdInfo (pprIdDetails)
import Type
import Outputable

myPpr :: ModGuts -> SDoc
myPpr mod_guts = vcat $ map myPprBind $ mg_binds mod_guts

myPprName :: Name -> SDoc
myPprName n = text "Name" <+> (ppr $ n) <> (parens $ pprNameDefnLoc n)

myPprVar :: Var -> SDoc
myPprVar v =  (myPprName $ varName v) <> comma <+>
              text "Type" <+> (ppr $ varType v)

myPprId :: Id -> SDoc
myPprId id = ppr [isImplicitId id,
                  isDeadBinder id, 
                  isStrictId id,
                  isExportedId id,
                  isLocalId id,
                  isGlobalId id,
                  isRecordSelector id,
                  isNaughtyRecordSelector id,
                  isPatSynRecordSelector id,
                  isDataConRecordSelector id,
                  isDFunId id,
                  isPrimOpId id,
                  isFCallId id,
                  isDataConWorkId id,
                  isConLikeId id,
                  isBottomingId id,
                  hasNoBinding id]


myPprExpr :: Expr Var -> SDoc
myPprExpr e = case e of
                Var v -> text "Var" <+> (parens (myPprVar v))
                Lit literal -> text "Lit" <+> (ppr literal)
                App e args -> text "App" <+> parens (myPprExpr e) <+> (myPprExpr args)
                Lam b e -> text "Lam"
                Let b e -> text "Let"
                Case e b t alts -> text "Case"
                Cast e c -> text "Cast"
                Tick t e -> text "Tick"
                Type t -> text "Type" <+> ppr t
                Coercion c -> text "Coercion"


myPprBind :: CoreBind -> SDoc
myPprBind (NonRec v e) = hang (myPprVar v <+> text " = ") 2 (text "NonRec" <+> myPprExpr e)
myPprBind (Rec bs) = text "Rec"


