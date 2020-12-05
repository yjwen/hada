module Util (nameIsInModule, varIsInModule) where

import Name (Name, nameModule)
import Module (moduleName, moduleNameString)
import Var (Var, varName)

nameIsInModule :: Name -> String -> Bool
nameIsInModule =  (==) . moduleNameString . moduleName . nameModule

varIsInModule :: Var -> String -> Bool
varIsInModule = nameIsInModule . varName
