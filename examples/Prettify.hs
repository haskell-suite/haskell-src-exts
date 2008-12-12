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
import System.Environment
  ( getArgs
  , getProgName
  )
{- External Library Modules Imported -}
import qualified Language.Haskell.Exts.Parser as Parser
import Language.Haskell.Exts.Parser
  ( ParseMode   ( .. )
  , ParseResult ( .. )
  )
-- import qualified Language.Haskell.Exts.Pretty as Pretty
import Language.Haskell.Exts.Pretty
  ( Pretty      ( .. )
  , prettyPrint
  )
import Language.Haskell.Exts.Syntax
  ( Module      ( .. ) )
{- Local Modules Imported -}
{- End of Imports -}

data CliFlag =
    CliHelp
  | CliVersion
  deriving Eq


options :: [ OptDescr CliFlag ]
options =
  [ Option   "h"     [ "help" ]
    (NoArg CliHelp)
    "Print the help message to standard out and then exit"

  , Option   "v"     [ "version" ]
    (NoArg CliVersion)
    "Print out the version of this program"
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
  | otherwise             = mapM_ processFile files

-- Our processing of a file is to simply count the words
-- in the file and output the number as a line.
processFile :: FilePath -> IO ()
processFile file =
  do contents <- readFile file
     let pResult   = Parser.parseModuleWithMode parseMode contents 
         parseMode = ParseMode { parseFilename = file }
     case pResult of
       ParseOk hModule            -> 
         putStrLn $ prettyPrint hModule
       ParseFailed srcLoc message -> 
         putStrLn $ unlines [ prettyPrint srcLoc
                            , message
                            ]

