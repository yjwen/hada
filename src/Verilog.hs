module Verilog where

import DFG
import Outputable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | `delim d s` appends the delimeter `d` to each element of `s`
-- except the last one.
delim :: SDoc -> [SDoc] -> [SDoc]
delim d [] = []
delim d (a:[]) = [a]
delim d (a:as) = (a <> d):(delim d as)

toVModule :: Graph -> SDoc
toVModule g = text "module" <+> text (graphName g) <+> ports <> semi $$ (text "endmodule")
    where ports = parens $ vcat $ delim comma (inputs ++ outputs)
          inputs = map (declareSignal "input") $ graphInputs g
          outputs = map (declareSignal "output") $ Set.toList $ graphOutputs g

declareSignal :: String -> Signal -> SDoc
declareSignal head s =
  text head
  <+>
  case signalWidth s of
    Just w -> brackets ((int $ w -1) <+> colon <+> (int 0))
    Nothing -> empty
  <+>
  case signalID s of
    Left n -> text n
    Right _ -> error "Unnamed signal"
                                   

-- toStatements :: C.Expr Var -> Maybe [Statement]
-- toStatements (C.Case e b t alts) = Just $ [Comment $ showSDocUnsafe $ vcat 
--                                            ([text $show $ toConstr e,
--                                              ppr b
--                                             ] ++ (map pprCons alts))]
--   where pprCons (con, _, _) = case con of
--                                 C.DataAlt dataCon -> ppr $ dataConRepType dataCon
--                                 otherwise -> text "unknown"
-- toStatements _ = Nothing
