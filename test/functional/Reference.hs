module Reference (tests) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad (forM)
import qualified Data.Set as Set
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Lens
import System.Directory (canonicalizePath)
import System.FilePath ((</>))
import System.Time.Extra (sleep)
import Test.Hls.Util
import Test.Tasty
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "references"
    [ testGroup "can get references to a symbol which is local to one module"
          [ testCase "can get references to symbols" $
            referenceTest ("src/References.hs", 4, 7)
                          YesIncludeDeclaration
                          [ ("src/References.hs", 4, 6)
                          , ("src/References.hs", 6, 0)
                          , ("src/References.hs", 6, 14)
                          , ("src/References.hs", 9, 7)
                          , ("src/References.hs", 10, 11)
                          ]

          , testCase "can get references to data constructor" $
            referenceTest ("src/References.hs", 13, 2)
                          YesIncludeDeclaration
                          [ ("src/References.hs", 13, 2)
                          , ("src/References.hs", 16, 14)
                          , ("src/References.hs", 19, 21)
                          ]

          , testCase "getting references works in the other module" $
            referenceTest ("src/OtherModule.hs", 6, 0)
                          YesIncludeDeclaration
                          [ ("src/OtherModule.hs", 6, 0)
                          , ("src/OtherModule.hs", 8, 16)
                          ]

          , testCase "getting references works in the Main module" $
            referenceTest ("exe/Main.hs", 9, 0)
                          YesIncludeDeclaration
                          [ ("exe/Main.hs", 9, 0)
                          , ("exe/Main.hs", 10, 4)
                          ]

          , testCase "getting references to main works" $
            referenceTest ("exe/Main.hs", 5, 0)
                          YesIncludeDeclaration
                          [ ("exe/Main.hs", 4, 0)
                          , ("exe/Main.hs", 5, 0)
                          ]

          , testCase "getting references in the other package" $
            referenceTest ("dependencyfoo/src/OtherModuleInDependency.hs", 2, 0)
                          YesIncludeDeclaration
                          [ ("dependencyfoo/src/OtherModuleInDependency.hs", 2, 0)
                          , ("dependencyfoo/src/OtherModuleInDependency.hs", 4, 13)
                          ]

          , expectFailBecause "references provider does not respect includeDeclaration parameter" $
            testCase "works when we ask to exclude declarations" $
            referenceTest ("src/References.hs", 4, 7)
                          NoExcludeDeclaration
                          [ ("src/References.hs", 6, 0)
                          , ("src/References.hs", 6, 14)
                          , ("src/References.hs", 9, 7)
                          , ("src/References.hs", 10, 11)
                          ]

          , testCase "INCORRECTLY returns declarations when we ask to exclude them" $
            referenceTest ("src/References.hs", 4, 7)
                          NoExcludeDeclaration
                          [ ("src/References.hs", 4, 6)
                          , ("src/References.hs", 6, 0)
                          , ("src/References.hs", 6, 14)
                          , ("src/References.hs", 9, 7)
                          , ("src/References.hs", 10, 11)
                          ]
          ]

    , testGroup "can get references to a symbol which is local to one package"
          [ testCase "can get references to symbol defined in a module we import" $
            referenceTest ("src/References.hs", 22, 4)
                          YesIncludeDeclaration
                          [ ("src/References.hs", 22, 4)
                          , ("src/OtherModule.hs", 0, 20)
                          , ("src/OtherModule.hs", 4, 0)
                          ]

          , testCase "can get references in modules that import us to symbols we define" $
            referenceTest ("src/OtherModule.hs", 4, 0)
                          YesIncludeDeclaration
                          [ ("src/References.hs", 22, 4)
                          , ("src/OtherModule.hs", 0, 20)
                          , ("src/OtherModule.hs", 4, 0)
                          ]

          , testCase "can get references to symbol defined in a module we import transitively" $
            referenceTest ("src/References.hs", 24, 4)
                          YesIncludeDeclaration
                          [ ("src/References.hs", 24, 4)
                          , ("src/OtherModule.hs", 0, 48)
                          , ("src/OtherOtherModule.hs", 2, 0)
                          ]

          , testCase "can get references in modules that import us transitively to symbols we define" $
            referenceTest ("src/OtherOtherModule.hs", 2, 0)
                          YesIncludeDeclaration
                          [ ("src/References.hs", 24, 4)
                          , ("src/OtherModule.hs", 0, 48)
                          , ("src/OtherOtherModule.hs", 2, 0)
                          ]
          ]

    , testGroup "can get references to a symbol which is local to one project"
          [ testCase "can get references to symbol defined in dependency" $
            referenceTest ("exe/Main.hs", 7, 6)
                          YesIncludeDeclaration
                          [ ("exe/Main.hs", 7, 6)
                          , ("dependencyfoo/src/ModuleInDependency.hs", 2, 0)
                          ]

          , testCase "can get references in our dependents to a symbol we define" $
            referenceTest ("dependencyfoo/src/ModuleInDependency.hs", 2, 0)
                          YesIncludeDeclaration
                          [ ("exe/Main.hs", 7, 6)
                          , ("dependencyfoo/src/ModuleInDependency.hs", 2, 0)
                          ]
          ]
    ]

-- | To locate a symbol, we provide a path to the file from the HLS root
-- directory, the line number, and the column number. (0 indexed.)
type SymbolLocation = (FilePath, Int, Int)

-- | When we ask for all references to symbol "foo", should the declaration "foo
-- = 2" be among the references returned?
data IncludeDeclaration =
    YesIncludeDeclaration
    | NoExcludeDeclaration

getReferences' :: SymbolLocation -> IncludeDeclaration -> Session [Location]
getReferences' (file, l, c) includeDeclaration = do
    doc <- openDoc file "haskell"
    getReferences doc (Position l c) $ toBool includeDeclaration
    where toBool YesIncludeDeclaration = True
          toBool NoExcludeDeclaration = False

expectSameLocations :: [Location] -> [SymbolLocation] -> Assertion
actual `expectSameLocations` expected = do
    let actual' =
            Set.map (\location -> (location ^. uri
                                   , location ^. range . start . line
                                   , location ^. range . start . character))
            $ Set.fromList actual
    expected' <- Set.fromList <$>
        (forM expected $ \(file, l, c) -> do
                              fp <- canonicalizePath $ referencesPath </> file
                              return (filePathToUri fp, l, c))
    actual' @?= expected'

referencesPath :: FilePath
referencesPath = "test/testdata/references"

referenceTestSession :: Session a -> IO a
referenceTestSession f = runSession hlsCommand fullCaps referencesPath $ do
    -- Preload all the files we need.
    -- TODO: Something needs to change ...
    -- These tests take forever anyway while HLS does stuff with cabal.
    _ <- openDoc "exe/Main.hs" "haskell"
    _ <- openDoc "src/OtherModule.hs" "haskell"
    _ <- openDoc "src/OtherOtherModule.hs" "haskell"
    _ <- openDoc "src/References.hs" "haskell"
    _ <- openDoc "dependencyfoo/src/ModuleInDependency.hs" "haskell"
    _ <- openDoc "dependencyfoo/src/OtherModuleInDependency.hs" "haskell"
    liftIO $ sleep 2
    f

-- | Given a location, lookup the symbol and all references to it. Make sure
-- they are the ones we expect.
referenceTest :: SymbolLocation -> IncludeDeclaration -> [SymbolLocation] -> Assertion
referenceTest loc includeDeclaration expected =
    referenceTestSession $ do
        actual <- getReferences' loc includeDeclaration
        liftIO $ actual `expectSameLocations` expected
