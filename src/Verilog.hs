module Verilog where

import Outputable
import qualified CoreSyn as C
import Var
import Name

data Module = Module { moduleName :: String
                     , moduleInputs :: [Signal]
                     , moduleOutputs :: [Signal]
                     , moduleStatements :: [Statement]
                     }
             deriving Show

commaSep :: [SDoc] -> SDoc
commaSep [] = empty
commaSep (a:[]) = a
commaSep (a:as) = a <> comma <+> (commaSep as)

instance Outputable Module where
  ppr m = text "module" <+> text (moduleName m) <+> portNames <> semi $$ nest 2 body $$ (text "endmodule")
    where portNames = parens $ commaSep $ map (text . signalName) $ (moduleInputs m ++ moduleOutputs m)
          statements = vcat $ map ppr (moduleStatements m)
          body = inputPorts $$ outputPorts $$ statements
          inputPorts = vcat $ map ((text "input" <+>) . ppr) $ moduleInputs m
          outputPorts = vcat $ map ((text "output" <+>) . ppr) $ moduleOutputs m


data Signal = Signal { signalName :: String
                     , signalRange :: Maybe (Int, Int)
                     }
               deriving Show

defineSignal :: Signal -> SDoc
defineSignal (Signal n Nothing) = text n
defineSignal (Signal n (Just (l, r))) = brackets (int l <+> colon <+> int r) <+> text n <> semi

refSignal :: Signal -> SDoc
refSignal (Signal n Nothing) = text n
refSignal (Signal n (Just (l, r))) = text n <> brackets (int l <+> colon <+> int r)

instance Outputable Signal where
  ppr = defineSignal

data Statement = Assign { assignLHS :: Signal
                        , assignRHS :: Expr
                        } deriving Show
instance Outputable Statement where
  ppr (Assign lhs rhs) = text "assign" <+> refSignal lhs <+> equals <+> ppr rhs <> semi

data Expr = ConditionalExpr { condition :: Expr
                            , trueClause :: Expr
                            , falseClause :: Expr
                            }
          | BinaryExpr { binOp :: BinOp
                       , lhs :: Expr
                       , rhs :: Expr
                       }
          | SignalExpr {signal :: Signal}
          deriving (Show)

instance Outputable Expr where
  ppr (ConditionalExpr c tc fc) = (parens $ ppr c) <+> (char '?') <+> (ppr tc) <+> colon <+> (ppr fc)
  ppr (BinaryExpr op lhs rhs) = (ppr lhs) <+> (ppr op) <+> (ppr rhs)
  ppr (SignalExpr sig) = refSignal sig

data BinOp = LessThan | Minus deriving (Show)

instance Outputable BinOp where
  ppr LessThan = text "<"
  ppr Minus = text "-"


coreToVerilog :: C.CoreBind -> Maybe Module
coreToVerilog (C.NonRec v e) =  exprToVerilog (getOccString v) e
coreToVerilog (C.Rec _) = undefined

exprToVerilog :: Outputable a => String -> C.Expr a -> Maybe Module
exprToVerilog m exp = case exp of
                        (C.Lam v _) -> Just $ Module m [] [] []
                        otherwise -> Nothing

coreToVar :: C.CoreBind ->  Var
coreToVar (C.NonRec v _) = v
coreToVar _ = undefined
