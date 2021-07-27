module TextExpr( TextExpr(..)
               , apply
               , binaryOp, unaryOp
               , showCommon
               ) where

data Operands = ZeroOp
              | OneOp TextExpr
              | TwoOps TextExpr TextExpr
  
-- Mimic the CoreExpr's expression application structure, with
-- function type information (either a regular function or an
-- operator) and expression precedence, for exporting to text of
-- various target languages.
data TextExpr = LeafExpr String
              -- An indecomposable expression, represented by a string
              | Func String [TextExpr]
              -- A regular function with arguments, arguments saved in
              -- reversed order
              | MetaFunc String
              -- A meta-function, expecting one type
              -- argument. Generate a normal func when the argument is
              -- applied.
              | BinaryOperator String Int Operands
              -- An operator that requires infix notation
              | UnaryOperator String Int (Maybe TextExpr)
              -- An unary opeator
              | Identity

apply :: TextExpr -> TextExpr -> TextExpr
apply (Func s args) e = Func s (e:args)
apply (BinaryOperator o i ops) e = BinaryOperator o i (app e ops)
  where app e ZeroOp = OneOp e
        app e (OneOp e0) = TwoOps e0 e
        app e (TwoOps _ _) = error ("3 operands applied to operator " ++ o)
apply (UnaryOperator o i Nothing) e = UnaryOperator o i (Just e)
apply (UnaryOperator o i _) e = error ("2 operands applied to unary operator " ++ o)
apply Identity e = e
apply (MetaFunc f) (LeafExpr s) = Func (f ++ s) []
apply (MetaFunc f) _ = error "MetaFunc is applied with a non-leaf argument"

-- Show a TextExpr in the most common syntax by which functions are
-- showed in f(a, b) style and operators are showed in a*(b+c) style
showCommon :: TextExpr -> String
showCommon (LeafExpr s) = s -- Assuming s is already in the common syntax
showCommon (Func fname args) = fname ++ "(" ++ showArgsCommon args ++ ")"
showCommon (BinaryOperator s prec ops)
  = case ops of
      TwoOps lhs rhs -> showCommon lhs ++ " " ++ s ++ " " ++ showCommonPrec prec rhs
      otherwise -> error ("Insufficient operand for operator " ++ s)
showCommon (UnaryOperator s prec opm)
  = case opm of
      Just o -> s ++ showCommonPrec prec o
      otherwise -> error ("Insufficient operand for operator " ++ s)
showCommon Identity = error "Showing an identity function"
showCommon (MetaFunc _) = error "Showing a meta-function"


-- Show comma separated text expressions in reverse order
showArgsCommon :: [TextExpr] -> String
showArgsCommon [] = ""
showArgsCommon (a:as) = foldl (\a -> \b -> showCommon b ++ ", " ++ a) (showCommon a) as

-- Show a TextExpr in the common syntax for a given precedence
showCommonPrec :: Int -> TextExpr -> String
showCommonPrec p e
  | BinaryOperator _ pe _ <- e = let estr = showCommon e
                           in if pe > p
                              then estr
                              else "(" ++ estr ++ ")"
  | otherwise = showCommon e -- Function calls and leaf expressions
                             -- always have higher precedence than
                             -- operators.

binaryOp :: String -> Int -> TextExpr
binaryOp o i = BinaryOperator o i ZeroOp

unaryOp :: String -> Int -> TextExpr
unaryOp o i = UnaryOperator o i Nothing
