module PprDFG where
import DFG
import Outputable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
instance Outputable Graph where
    ppr g = hang head 2 (outputs $$ signals $$ nodes)
            where head = text "graph" <+> text (graphName g)
                  outputs = text "outputs:" <+> (vcat $ map ppr $ Set.toAscList $ graphOutputs g)
                  signals = text "signals:" <+> (vcat $ map ppr $ Map.toAscList $ graphSignals g)
                  nodes = text "nodes:" <+> (vcat $ map ppr $ Map.toAscList $ graphNodes g)

instance Outputable Signal where
    ppr s
      | SimpleSigType b <- signalType s
      = text "signal" <+> pprSignalName s <+> (ppr b)

pprSignalName :: Signal -> SDoc
pprSignalName s = case signalID s of
                    Left str -> doubleQuotes $ text str
                    Right id -> ppr '\\' <> ppr id

instance Outputable Node where
    ppr (CaseNode o cond dflt branches) = hang head 2 body
        where head = pprSignalName o <+> equals <+> text "case" <+> pprSignalName cond <+> text "of"
              body = (vcat $ map (\ (v, s) -> integer v <+> arrow <+> ppr s) branches) $$
                     text "else" <+> arrow <+> ppr dflt
    ppr (BinNode o op l r) = pprSignalName o <+> equals <+> ppr op <+> pprSignalName l <+> pprSignalName r
instance Outputable BinOp where
    ppr LessThan = parens $ char '<'
    ppr GreaterThan = parens $ char '>'
    ppr Minus = parens $ char '-'
    ppr Plus = parens $ char '+'
                      

