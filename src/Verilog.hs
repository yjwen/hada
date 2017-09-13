module Verilog where

import DFG
import Outputable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

-- | `delim d s` appends the delimeter `d` to each element of `s`
-- except the last one.
delim :: SDoc -> [SDoc] -> [SDoc]
delim d [] = []
delim d (a:[]) = [a]
delim d (a:as) = (a <> d):(delim d as)

toVModule :: Graph -> SDoc
toVModule g = text "module" <+> text (graphName g) <+> ports <> semi $$ vcat statements $$(text "endmodule")
    where ports = parens $ vcat $ delim comma (inputs ++ outputs)
          inputs = map (declareSignal "input") $ graphInputs g
          outputs = map (declareSignal "output") $ Set.toList $ graphOutputs g
          statements = Set.foldr (\signal statements -> (signalStatement signal g:statements)) [] $ graphOutputs g

declareSignal :: String -> Signal -> SDoc
declareSignal head s =
  text head
  <+>
  case signalWidth s of
    1 -> empty
    w -> brackets ((int $ w -1) <+> colon <+> (int 0))
  <+>
  case signalID s of
    Left n -> text n
    Right id -> text $ "_" ++ show id
                                   
signalReference :: Signal -> SDoc
signalReference s = text $ case signalID s of
                             Left s -> s
                             Right id -> "anonymous_" ++ show id

signalStatement :: Signal -> Graph -> SDoc
signalStatement s g =
    case signalSource s g of
      Just (_, n) -> case n of
                       CaseNode o cond dflt branches ->
                           if length branches == 1
                           then text "assign" <+> signalReference o <+> text "=" <+> signalExpression s g <> semi
                           else error "Unsupported conditional signal."
                       _ -> error "Unsupported source node."
      _ -> error "Failed to get signal source node."

signalExpression :: Signal -> Graph -> SDoc
signalExpression s g =
    case signalSource s g of
      Just (nl, n) -> case n of
                        CaseNode o cond dflt branches ->
                            if length branches == 1
                            then let (trueValue, trueSignal) = head branches
                                     c = parens $
                                         if trueValue == 1
                                         then signalExpression cond g
                                         else signalExpression cond g <+> text "==" <+> bitLiteral trueValue 1
                                     t = signalExpression trueSignal g
                                     f = signalExpression dflt g
                                 in c <+> text "?" <+> t <+> text ":" <+> f
                            else error "Unsupported case node format."
                        BinNode o op lhs rhs ->
                            signalReference lhs <+> binOpText op <+> signalReference rhs
                                where binOpText LessThan = text "<"
                                      binOpText GreaterThan = text ">"
                                      binOpText Minus = text "-"
                                      binOpText Plus = text "+"
      _ -> error "Signal has no driver."

bitLiteral :: Integer -> Int -> SDoc
bitLiteral value width = int width <> text "'b" <>
                         (text $ showIntAtBase 2 intToDigit value "")

-- toStatements :: C.Expr Var -> Maybe [Statement]
-- toStatements (C.Case e b t alts) = Just $ [Comment $ showSDocUnsafe $ vcat 
--                                            ([text $show $ toConstr e,
--                                              ppr b
--                                             ] ++ (map pprCons alts))]
--   where pprCons (con, _, _) = case con of
--                                 C.DataAlt dataCon -> ppr $ dataConRepType dataCon
--                                 otherwise -> text "unknown"
-- toStatements _ = Nothing
