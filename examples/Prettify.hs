{-
-}
module Main
  ( main )
where

{- Standard Library Modules Imported -}
import System.Console.GetOpt
  ( getOpt
  , usageInfo
  , ArgOrder    ( .. )
  , OptDescr    ( .. )
  , ArgDescr    ( .. )
  )
import System.Directory
  ( createDirectoryIfMissing )
import System.Environment
  ( getArgs
  , getProgName
  )
import qualified System.FilePath as File
{- External Library Modules Imported -}
import qualified Language.Haskell.Exts.Parser as Parser
import Language.Haskell.Exts.Parser
  ( ParseMode   ( .. )
  , defaultParseMode
  , ParseResult ( .. )
  )
-- import qualified Language.Haskell.Exts.Pretty as Pretty
import Language.Haskell.Exts.Pretty
  ( prettyPrintWithMode
  , Pretty
  , PPHsMode              ( .. )
  , PPLayout              ( .. )
  , defaultMode
  )
{- Local Modules Imported -}
{- End of Imports -}

data CliFlag =
    CliHelp
  | CliVersion
  | CliOutputDir FilePath
  | CliPPLayout PPLayout
  | CliClassIndent  Int
  | CliDoIndent     Int
  | CliCaseIndent   Int
  | CliLetIndent    Int
  | CliWhereIndent  Int
  | CliOnsideIndent Int
  deriving Eq


options :: [ OptDescr CliFlag ]
options =
  [ Option   "h"     [ "help" ]
    (NoArg CliHelp)
    "Print the help message to standard out and then exit"

  , Option   "v"     [ "version" ]
    (NoArg CliVersion)
    "Print out the version of this program"

  , Option   ""      [ "output-dir" ]
    (ReqArg CliOutputDir     "DIR")
    "Redirect the output to a file of the same name in the new directory"

    {- Options for controlling the layout style of the pretty printing -}
  , Option   ""      [ "pp-classical" ]
    (NoArg $ CliPPLayout PPOffsideRule)
    "Use the classical layout rules"

  , Option   ""      [ "pp-semi-colon"   ]
    (NoArg $ CliPPLayout PPSemiColon)
    "Use the classical layout made explicit with semi-colons"

  , Option   ""      [ "pp-inline-decls" ]
    (NoArg $ CliPPLayout PPInLine)
    "inline decls, with newlines between them"

  , Option   ""      [ "pp-no-layout" ]
    (NoArg $ CliPPLayout PPNoLayout)
    "everything on a single line"

    {- Options for controlling individual indentation sizes -}
  , Option   ""      [ "class-indent" ]
    (ReqArg (CliClassIndent . read) "Int")
    "The indentation of a class or instance declaration"

  , Option   ""      [ "do-indent" ]
    (ReqArg (CliDoIndent . read)   "Int")
    "The indentation of do-expressions"

  , Option   ""      [ "case-indent" ]
    (ReqArg (CliCaseIndent . read) "Int")
    "The indentation of the body of a case expression"

  , Option   ""      [ "let-indent" ]
    (ReqArg (CliWhereIndent . read) "Int")
    "The indentation of the declarations in a let expression"

  , Option   ""      [ "where-indent" ]
    (ReqArg (CliWhereIndent . read) "Int")
    "The indentation of the declarations in a where clause"

  , Option   ""      [ "onside-indent" ]
    (ReqArg (CliOnsideIndent . read) "Int")
    "Indentation added for continuation lines that would otherwise be offside"
  ]

helpMessage :: String -> String
helpMessage progName =
  usageInfo progName options

versionMessage :: String -> String
versionMessage progName =
  progName ++ ": This is version 0.001"

-- | The main exported function
main :: IO ()
main = getArgs >>= processOptions

-- Process the command line options and arguments, this basically just checks
-- if there is a parse error in the command-line otherwise it passes on the
-- actual processing of the parsed arguments to 'processArgs'
processOptions :: [ String ] -> IO ()
processOptions cliArgs =
  case getOpt Permute  options cliArgs of
    (flags, args, [])       ->
      processArgs flags args
    (_flags, _args, errors) ->
      do progName <- getProgName
         ioError $ userError (concat errors ++ helpMessage progName)

-- We assume all of the arguments are files to process
processArgs :: [ CliFlag ] -> [ String ] -> IO ()
processArgs flags files
  | elem CliHelp flags    = getProgName >>= (putStrLn . helpMessage)
  | elem CliVersion flags = getProgName >>= (putStrLn . versionMessage)
  | otherwise             = mapM_ (processFile outputDir printMode) files
  where
  outputDir = case [ dir | CliOutputDir dir <- flags ] of
                [] -> Nothing
                l  -> Just $ head l

  printMode = foldl updateMode defaultMode flags

  updateMode :: PPHsMode -> CliFlag -> PPHsMode
  updateMode mode (CliHelp)           = mode
  updateMode mode (CliVersion)        = mode
  updateMode mode (CliOutputDir _)    = mode
  updateMode mode (CliPPLayout lay)   = mode { layout = lay }
  updateMode mode (CliClassIndent i)  = mode { classIndent = i }
  updateMode mode (CliDoIndent i)     = mode { doIndent = i }
  updateMode mode (CliCaseIndent i)   = mode { caseIndent = i }
  updateMode mode (CliLetIndent i)    = mode { letIndent = i }
  updateMode mode (CliWhereIndent i)  = mode { whereIndent = i }
  updateMode mode (CliOnsideIndent i) = mode { onsideIndent = i }


-- Our processing of the file is simply to parse the file followed by printing
-- the file out in a prettier format according to the options given on the
-- command-line.
processFile :: Maybe FilePath -> PPHsMode -> FilePath -> IO ()
processFile mDir printMode file =
  do contents <- readFile file
     let pResult   = Parser.parseModuleWithMode parseMode contents
         parseMode = defaultParseMode { parseFilename = file }
     case pResult of
       ParseOk hModule            ->
         outputSource $ prettyPrint hModule
       ParseFailed srcLoc message ->
         putStrLn $ unlines [ prettyPrint srcLoc
                            , message
                            ]
  where
  prettyPrint :: Pretty a => a -> String
  prettyPrint = prettyPrintWithMode printMode

  outputSource :: String -> IO ()
  outputSource contents =
    case mDir of
      Just dir -> do createDirectoryIfMissing True dir
                     writeFile (File.replaceDirectory file dir) contents
      Nothing  -> putStrLn contents

