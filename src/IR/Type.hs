module Type (Type(..), intersect) where

data Type = TyBool
          | TyInt
          | TyAny
          | TyArrow Type Type
          deriving (Eq)

instance Show Type where
  show TyBool = "bool"
  show TyInt = "int"
  show TyAny = "any"
  show (TyArrow t0 t1) =
    (case t0 of
       TyArrow _ _ -> "(" ++ show t0 ++ ")"
       otherwise -> show t0)
    ++
    "->" ++ show t1

-- Considering a type as a set of values, intersect a b return a type
-- that is subset of both a or b. If no such type exists, return
-- Nothing
intersect :: Type -> Type -> Maybe Type

-- For two arrow types, their intersection exists only when their
-- domain and codomain types have intersections respectively.
intersect (TyArrow a0 b0) (TyArrow a1 b1) =
  do r0 <- a0 `intersect` a1
     r1 <- b0 `intersect` b1
     return $ TyArrow r0 r1
intersect TyAny b = Just b
intersect a TyAny = Just a
intersect TyBool TyBool = Just TyBool
intersect TyInt TyInt = Just TyInt
intersect a b = Nothing

