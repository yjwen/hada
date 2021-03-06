-- Test various case expressions
module Case (andOrBool) where

andOrBool :: Bool -> Bool -> Bool -> Bool
andOrBool a b c = a && (b || c)

