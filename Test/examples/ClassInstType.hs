module ClassInstType where

class Dir d where
  localDir :: d -> IO FilePath

instance Dir Directory where
  localDir (Local f) = return f

  localDir (Darcs {url=url,darcsVersion=Patch patch,subDirectory=subDir}) = do 
    tmp <- createTempDir 0 "haskelld"
    darcsOut <- runDarcsCommand tmp "get" ["--lazy","--to-match","hash "++ patch,url,"fs"]
    print darcsOut
    let (ExitSuccess,"",out) = darcsOut 
    print out
    return $ tmp </> "fs" </> subDir

type URL = String
