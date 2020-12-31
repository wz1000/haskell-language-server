module Main where

import ModuleInDependency

main :: IO ()
main = return ()

xxx = symbolDefinedInDependency

a = 2
b = a + 1
