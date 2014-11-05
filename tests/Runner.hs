{-# LANGUAGE ViewPatterns #-}
-- | Use "runhaskell Setup.hs test" or "cabal test" to run these tests.
module Main where

import Language.Haskell.Exts.Annotated
import qualified Language.Haskell.Exts as S -- S for "Simple", i.e. not annotated

import Test.Tasty hiding (defaultMain)
import Test.Tasty.Golden
import Test.Tasty.Golden.Manage
import System.FilePath
import System.IO
import Control.Monad.Trans
import Control.Applicative
import Data.Generics
import Extensions
import Data.Text hiding (foldl)

main :: IO ()
main = do
  sources <- getTestFiles examplesDir
  defaultMain $ testGroup "Tests" $
    [ parserTests sources
    , exactPrinterTests sources
    , prettyPrinterTests sources
    , prettyParserTests sources
    , extensionProperties
    ]

-- | Where all the tests are to be found
examplesDir :: FilePath
examplesDir = "tests/examples"

getTestFiles :: MonadIO m => FilePath -> m [FilePath]
getTestFiles dir = liftIO $ findByExtension [".hs", ".lhs"] dir

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
      tmp <- readUTF8File file
      let
        contents = replaceUTF8Ops tmp
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
      tmp <- readUTF8File file
      let
        contents = replaceUTF8Ops tmp
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

prettyParserTests :: [FilePath] -> TestTree -- {{{
prettyParserTests sources = testGroup "Pretty-parser tests" $ do
  -- list monad
  file <- sources
  let
    out = file <.> "prettyparser" <.> "out"
    golden = file <.> "prettyparser" <.> "golden"
    run = do
      contents <- readUTF8File file
      let
        -- parse
        parse1Result :: ParseResult S.Module
        parse1Result =
          S.parseFileContentsWithMode
            (defaultParseMode { parseFilename = file })
            contents



        prettyResult :: ParseResult String
        prettyResult = prettyPrint <$> parse1Result

        parse2Result :: ParseResult (ParseResult S.Module)
        parse2Result = S.parseFileContents <$> prettyResult

        -- Even the un-annotated AST contains certain locations.
        -- Obviously, they may differ, so we have to erase them.
        eraseLocs :: S.Module -> S.Module
        eraseLocs = everywhere $ mkT $ const noLoc

        summary =
          case liftA3 (,,) parse1Result prettyResult parse2Result of
            f@ParseFailed{} -> show f
            ParseOk (eraseLocs -> ast1, pretty, mbAst2) ->
              case mbAst2 of
                f@ParseFailed{} ->
                  "Failed to parse output of pretty-printer:\n" ++
                  show f ++ "\n" ++
                  "The pretty-printer output follows.\n\n" ++
                  pretty
                ParseOk (eraseLocs -> ast2) ->
                  if ast1 == ast2
                    then "Match"
                    else
                      "Roundtrip test failed\n\n" ++
                      "AST 1:\n\n" ++
                      show ast1 ++ "\n\n" ++
                      "AST 2:\n\n" ++
                      show ast2 ++ "\n"

      writeBinaryFile out $ summary ++ "\n"
  return $ goldenVsFile (takeBaseName file) golden out run
-- }}}

-- UTF8 utils {{{
readUTF8File :: FilePath -> IO String
readUTF8File fp = openFile fp ReadMode >>= \h -> do
        hSetEncoding h utf8
        hGetContents h

parseUTF8FileWithComments :: ParseMode -> FilePath -> IO (ParseResult (Module SrcSpanInfo, [Comment]))
parseUTF8FileWithComments p fp = readUTF8File fp >>= (return . parseFileContentsWithComments p)

utf8Ops :: [(String, String)]
utf8Ops = [("∧", "&&"),("∨", "||"),("≡", "=="),("≠", "/="), ("≤", "<=")] ++
          [("≥", ">="),("≮", ">="),("≯", "<="),("π", "pi"), ("÷", "/")] ++
          [("⋅", "*"), ("∈", "`elem`"), ("∉", "`notElem`")] ++
          [("∷", "::"), ("⇒", "=>"), ("∀", "forall"), ("→", "->")] ++
          [("←", "<-"), ("⤙", "-<"), ("⤚", ">-"), ("⤛", "-<<")] ++
          [("⤜", ">>-"), ("★", "*"), ("≢", "/=")]

---------------------------------------
-- Replace one unicode operator to its plain text counter-part

replaceUTF8Op :: String -> String -> String -> String
replaceUTF8Op search replacement content =
    unpack $ replace (pack search) (pack replacement) (pack content)

---------------------------------------
-- Replace all unicode operator in the input string to their plain counter-part

replaceUTF8Ops :: String -> String
replaceUTF8Ops content =
    foldl (\ x (y, z) -> replaceUTF8Op y z x) content utf8Ops

-- }}}
