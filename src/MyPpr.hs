module MyPpr where
import Prelude hiding ((<>))
import HscTypes (mg_binds, ModGuts)
import Name(Name, pprNameDefnLoc)
import CoreSyn(Bind(NonRec, Rec), CoreBind,
               Expr(Var, Lit, App, Lam, Let, Case, Cast, Tick, Type, Coercion),
               Alt)
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
                Var v -> parens (text "Var" <+> ppr v)
                Lit literal -> parens (text "Lit" <+> ppr literal)
                App e arg -> parens (text "App" <+> myPprExpr e <+> myPprExpr arg)
                Lam b e -> parens (text "Lam" <+> ppr b <+> myPprExpr e)
                Let b e -> parens (text "Let")
                Case e b t alts -> parens (text "Case" <+> myPprExpr e <+> ppr b <+> ppr t <+> hsep (map myPprAlt alts))
                Cast e c -> parens (text "Cast")
                Tick t e -> parens (text "Tick")
                Type t -> parens (text "Type" <+> ppr t)
                Coercion c -> parens (text "Coercion")

myPprAlt :: (Alt Var -> SDoc)
myPprAlt (altcon, binds, expr) = parens (ppr altcon <+> parens (hsep (map ppr binds)) <+> myPprExpr expr)


myPprBind :: CoreBind -> SDoc
myPprBind (NonRec v e) = hang (myPprVar v <+> text " = ") 2 (text "NonRec" <+> myPprExpr e)
myPprBind (Rec bs) = text "Rec"


