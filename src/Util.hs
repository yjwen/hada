module Util (nameIsInModule, varIsInModule,
             nameModuleStringMaybe,
             varModuleStringMaybe) where

import Name (Name, nameModule_maybe)
import Module (moduleName, moduleNameString)
import Var (Var, varName)

nameIsInModule :: Name -> String -> Bool
nameIsInModule a = case nameModuleStringMaybe a of
                     Just s -> (s == )
                     Nothing -> \ _ -> False

varIsInModule :: Var -> String -> Bool
varIsInModule = nameIsInModule . varName

nameModuleStringMaybe :: Name -> Maybe String
nameModuleStringMaybe = fmap (moduleNameString . moduleName) . nameModule_maybe

varModuleStringMaybe :: Var -> Maybe String
varModuleStringMaybe = nameModuleStringMaybe . varName
