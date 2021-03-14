module SDocLine (SDocLine(SDocLine), getSDocLine) where
import Outputable

data SDocLine = SDocLine SDoc

getSDocLine :: SDocLine -> SDoc
getSDocLine (SDocLine sdoc) = sdoc

instance Semigroup SDocLine where
  (SDocLine l0) <> (SDocLine l1) = SDocLine (l0 $+$ l1)

