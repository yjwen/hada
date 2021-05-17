module NameX (isInModule, moduleStringMaybe, getTupleArity) where

import Name (nameModule_maybe, NamedThing, getName, getOccString)
import Module (moduleName, moduleNameString)
import Data.Maybe (fromMaybe)

isInModule :: NamedThing a => a -> String -> Bool
isInModule a s = (fromMaybe False . fmap (s ==) . moduleStringMaybe) a

moduleStringMaybe :: NamedThing a => a -> Maybe String
moduleStringMaybe = fmap (moduleNameString . moduleName) . nameModule_maybe . getName

-- For a thing named in the form of tuple constructors like "(,,,,)",
-- return Just n where n is the tuple arity. Otherwise return Nothing
getTupleArity :: NamedThing a => a -> Maybe Int
getTupleArity a
  | isInModule a "GHC.Tuple"
  , not (null s)
  , head s == '('
  , (commas, last) <- span (== ',') (tail s)
  , last == ")"
  = Just (length commas + 1)
  | otherwise
  = Nothing
  where s = getOccString a

