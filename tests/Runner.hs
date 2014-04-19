-- | Use "runhaskell Setup.hs test" or "cabal test" to run these tests.
module Main where

import Language.Haskell.Exts.Annotated
import qualified Language.Haskell.Exts as S -- S for "Simple", i.e. not annotated

import Test.Tasty hiding (defaultMain)
import Test.Tasty.Golden
import Test.Tasty.Golden.Manage
import System.FilePath
import System.FilePath.Find
import System.IO
import Control.Monad.Trans
import Extensions

main :: IO ()
main = do
  sources <- getTestFiles examplesDir
  defaultMain $ testGroup "Tests" $
    [ parserTests sources
    , exactPrinterTests sources
    , prettyPrinterTests sources
    , extensionProperties
    ]

-- | Where all the tests are to be found
examplesDir :: FilePath
examplesDir = "tests" </> "examples"

getTestFiles :: MonadIO m => FilePath -> m [FilePath]
getTestFiles dir = liftIO $ find (return True) (extension ==? ".hs" ||? extension ==? ".lhs") dir

parserTests :: [FilePath] -> TestTree -- {{{
parserTests sources = testGroup "Parser tests" $ do
  -- list monad
  file <- sources
  let
    out = file <.> "parser" <.> "out"
    golden = file <.> "parser" <.> "golden"
    run = do
      ast <-
        parseUTF8FileWithComments
          (defaultParseMode { parseFilename = file })
          file
      writeBinaryFile out $ show ast ++ "\n"
  return $ goldenVsFile (takeBaseName file) golden out run
-- }}}

exactPrinterTests :: [FilePath] -> TestTree -- {{{
exactPrinterTests sources = testGroup "Exact printer tests" $ do
  -- list monad
  file <- sources
  let
    out = file <.> "exactprinter" <.> "out"
    golden = file <.> "exactprinter" <.> "golden"
    run = do
      contents <- readUTF8File file
      let
        -- parse
        mbAst =
          parseFileContentsWithComments
            (defaultParseMode { parseFilename = file })
            contents
        -- try to pretty-print; summarize the test result
        result =
          case mbAst of
            f@ParseFailed{} -> show f
            ParseOk ast ->
              let
                printed = uncurry exactPrint ast
              in
                if printed == contents
                  then "Match"
                  else printed
      writeBinaryFile out $ result ++ "\n"
  return $ goldenVsFile (takeBaseName file) golden out run
-- }}}

prettyPrinterTests :: [FilePath] -> TestTree -- {{{
prettyPrinterTests sources = testGroup "Pretty printer tests" $ do
  -- list monad
  file <- sources
  let
    out = file <.> "prettyprinter" <.> "out"
    golden = file <.> "prettyprinter" <.> "golden"
    run = do
      contents <- readUTF8File file
      let
        -- parse
        mbAst =
          S.parseFileContentsWithMode
            (defaultParseMode { parseFilename = file })
            contents
        -- try to pretty-print; summarize the test result
        result =
          case mbAst of
            f@ParseFailed{} -> show f
            ParseOk ast -> prettyPrint ast
      writeBinaryFile out $ result ++ "\n"
  return $ goldenVsFile (takeBaseName file) golden out run
-- }}}

-- UTF8 utils {{{
readUTF8File :: FilePath -> IO String
readUTF8File fp = openFile fp ReadMode >>= \h -> do
        hSetEncoding h utf8
        hGetContents h

parseUTF8FileWithComments :: ParseMode -> FilePath -> IO (ParseResult (Module SrcSpanInfo, [Comment]))
parseUTF8FileWithComments p fp = readUTF8File fp >>= (return . parseFileContentsWithComments p)
-- }}}
