module NameX (isInModule, moduleStringMaybe) where

import Name (nameModule_maybe, NamedThing, getName)
import Module (moduleName, moduleNameString)
import Data.Maybe (fromMaybe)

isInModule :: NamedThing a => a -> String -> Bool
isInModule a s = (fromMaybe False . fmap (s ==) . moduleStringMaybe) a

moduleStringMaybe :: NamedThing a => a -> Maybe String
moduleStringMaybe = fmap (moduleNameString . moduleName) . nameModule_maybe . getName

