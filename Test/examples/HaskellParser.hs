import qualified Language.Haskell.Exts.Annotated as Parser
import qualified Language.Haskell.Exts.Annotated.Syntax as Syn
import qualified Language.Haskell.Exts.Extension as Ext

parse originalFileName input = Parser.parseModuleWithMode parseMode input
    where
      parseMode :: Parser.ParseMode
      parseMode = Parser.defaultParseMode { Parser.parseFilename = originalFileName
                                          , Parser.extensions =
                                              Ext.glasgowExts ++
                                              [Ext.ExplicitForall]
                                          }
main =
    do s <- readFile "Bug.hs"
       let x = parse "Bug.hs" s
       putStrLn (show x)
