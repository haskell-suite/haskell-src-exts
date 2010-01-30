--getPermissions :: IO Permissions
getPermissions = do
  withFilePath $ do
  allocaBytes $ do
  throwErrnoIfMinus1_ 

--setPermissions :: IO ()
--setPermissions = undefined