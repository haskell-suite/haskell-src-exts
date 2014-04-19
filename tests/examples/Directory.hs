{-# LANGUAGE ForeignFunctionInterface, NondecreasingIndentation #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Directory
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- System-independent interface to directory manipulation.
--
-----------------------------------------------------------------------------

module System.Directory
   (
    -- $intro

    -- * Actions on directories
      createDirectory       -- :: FilePath -> IO ()
    , createDirectoryIfMissing  -- :: Bool -> FilePath -> IO ()
    , removeDirectory       -- :: FilePath -> IO ()
    , removeDirectoryRecursive  -- :: FilePath -> IO ()
    , renameDirectory       -- :: FilePath -> FilePath -> IO ()

    , getDirectoryContents      -- :: FilePath -> IO [FilePath]
    , getCurrentDirectory       -- :: IO FilePath
    , setCurrentDirectory       -- :: FilePath -> IO ()

    -- * Pre-defined directories
    , getHomeDirectory
    , getAppUserDataDirectory
    , getUserDocumentsDirectory
    , getTemporaryDirectory

    -- * Actions on files
    , removeFile        -- :: FilePath -> IO ()
    , renameFile                -- :: FilePath -> FilePath -> IO ()
    , copyFile                  -- :: FilePath -> FilePath -> IO ()

    , canonicalizePath
    , makeRelativeToCurrentDirectory
    , findExecutable

    -- * Existence tests
    , doesFileExist     -- :: FilePath -> IO Bool
    , doesDirectoryExist        -- :: FilePath -> IO Bool

    -- * Permissions

    -- $permissions

    , Permissions(
    Permissions,
    readable,       -- :: Permissions -> Bool
    writable,       -- :: Permissions -> Bool
    executable,     -- :: Permissions -> Bool
    searchable      -- :: Permissions -> Bool
      )

    , getPermissions            -- :: FilePath -> IO Permissions
    , setPermissions            -- :: FilePath -> Permissions -> IO ()
    , copyPermissions

    -- * Timestamps

    , getModificationTime       -- :: FilePath -> IO ClockTime
   ) where

import Prelude hiding ( catch )
import qualified Prelude

import Control.Monad (guard)
import System.Environment      ( getEnv )
import System.FilePath
import System.IO
import System.IO.Error hiding ( catch, try )
import Control.Monad           ( when, unless )
import Control.Exception.Base

import Foreign
import Foreign.C

{-# CFILES cbits/directory.c #-}

import System.Time             ( ClockTime(..) )

import GHC.IO.Exception ( IOException(..), IOErrorType(..), ioException )

import System.Posix.Types
import System.Posix.Internals
import qualified System.Win32 as Win32

{- $intro
A directory contains a series of entries, each of which is a named
reference to a file system object (file, directory etc.).  Some
entries may be hidden, inaccessible, or have some administrative
function (e.g. `.' or `..' under POSIX
<http://www.opengroup.org/onlinepubs/009695399/>), but in
this standard all such entries are considered to form part of the
directory contents. Entries in sub-directories are not, however,
considered to form part of the directory contents.

Each file system object is referenced by a /path/.  There is
normally at least one absolute path to each file system object.  In
some operating systems, it may also be possible to have paths which
are relative to the current directory.
-}

-----------------------------------------------------------------------------
-- Permissions

{- $permissions

 The 'Permissions' type is used to record whether certain operations are
 permissible on a file\/directory. 'getPermissions' and 'setPermissions'
 get and set these permissions, respectively. Permissions apply both to
 files and directories. For directories, the executable field will be
 'False', and for files the searchable field will be 'False'. Note that
 directories may be searchable without being readable, if permission has
 been given to use them as part of a path, but not to examine the
 directory contents.

Note that to change some, but not all permissions, a construct on the following lines must be used.

>  makeReadable f = do
>     p <- getPermissions f
>     setPermissions f (p {readable = True})

-}

data Permissions
 = Permissions {
    readable,   writable,
    executable, searchable :: Bool
   } deriving (Eq, Ord, Read, Show)

{- |The 'getPermissions' operation returns the
permissions for the file or directory.

The operation may fail with:

* 'isPermissionError' if the user is not permitted to access
  the permissions; or

* 'isDoesNotExistError' if the file or directory does not exist.

-}

getPermissions :: FilePath -> IO Permissions
getPermissions name = do
  withFilePath name $ \s -> do
  -- stat() does a better job of guessing the permissions on Windows
  -- than access() does.  e.g. for execute permission, it looks at the
  -- filename extension :-)
  --
  -- I tried for a while to do this properly, using the Windows security API,
  -- and eventually gave up.  getPermissions is a flawed API anyway. -- SimonM
  allocaBytes sizeof_stat $ \ p_stat -> do
  throwErrnoIfMinus1_ "getPermissions" $ c_stat s p_stat
  mode <- st_mode p_stat
  let usr_read   = mode .&. s_IRUSR
  let usr_write  = mode .&. s_IWUSR
  let usr_exec   = mode .&. s_IXUSR
  let is_dir = mode .&. s_IFDIR
  return (
    Permissions {
      readable   = usr_read  /= 0,
      writable   = usr_write /= 0,
      executable = is_dir == 0 && usr_exec /= 0,
      searchable = is_dir /= 0 && usr_exec /= 0
    }
   )

{- |The 'setPermissions' operation sets the
permissions for the file or directory.

The operation may fail with:

* 'isPermissionError' if the user is not permitted to set
  the permissions; or

* 'isDoesNotExistError' if the file or directory does not exist.

-}

setPermissions :: FilePath -> Permissions -> IO ()
setPermissions name (Permissions r w e s) = do
  allocaBytes sizeof_stat $ \ p_stat -> do
  withFilePath name $ \p_name -> do
    throwErrnoIfMinus1_ "setPermissions" $ do
      c_stat p_name p_stat
      mode <- st_mode p_stat
      let mode1 = modifyBit r mode s_IRUSR
      let mode2 = modifyBit w mode1 s_IWUSR
      let mode3 = modifyBit (e || s) mode2 s_IXUSR
      c_wchmod p_name mode3
 where
   modifyBit :: Bool -> CMode -> CMode -> CMode
   modifyBit False m b = m .&. (complement b)
   modifyBit True  m b = m .|. b

foreign import ccall unsafe "_wchmod"
   c_wchmod :: CWString -> CMode -> IO CInt

copyPermissions :: FilePath -> FilePath -> IO ()
copyPermissions source dest = do
  allocaBytes sizeof_stat $ \ p_stat -> do
  withFilePath source $ \p_source -> do
  withFilePath dest $ \p_dest -> do
    throwErrnoIfMinus1_ "copyPermissions" $ c_stat p_source p_stat
    mode <- st_mode p_stat
    throwErrnoIfMinus1_ "copyPermissions" $ c_wchmod p_dest mode

-----------------------------------------------------------------------------
-- Implementation

{- |@'createDirectory' dir@ creates a new directory @dir@ which is
initially empty, or as near to empty as the operating system
allows.

The operation may fail with:

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES]@

* 'isAlreadyExistsError' \/ 'AlreadyExists'
The operand refers to a directory that already exists.
@ [EEXIST]@

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'InvalidArgument'
The operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@

* 'NoSuchThing'
There is no path to the directory.
@[ENOENT, ENOTDIR]@

* 'ResourceExhausted'
Insufficient resources (virtual memory, process file descriptors,
physical disk space, etc.) are available to perform the operation.
@[EDQUOT, ENOSPC, ENOMEM, EMLINK]@

* 'InappropriateType'
The path refers to an existing non-directory object.
@[EEXIST]@

-}

createDirectory :: FilePath -> IO ()
createDirectory path = do
  Win32.createDirectory path Nothing

-- | @'createDirectoryIfMissing' parents dir@ creates a new directory
-- @dir@ if it doesn\'t exist. If the first argument is 'True'
-- the function will also create all parent directories if they are missing.
createDirectoryIfMissing :: Bool     -- ^ Create its parents too?
                 -> FilePath -- ^ The path to the directory you want to make
                 -> IO ()
createDirectoryIfMissing create_parents path0
  | create_parents = createDirs (parents path0)
  | otherwise      = createDirs (take 1 (parents path0))
  where
    parents = reverse . scanl1 (</>) . splitDirectories . normalise

    createDirs []         = return ()
    createDirs (dir:[])   = createDir dir throw
    createDirs (dir:dirs) =
      createDir dir $ \_ -> do
        createDirs dirs
        createDir dir throw

    createDir :: FilePath -> (IOException -> IO ()) -> IO ()
    createDir dir notExistHandler = do
      r <- try $ createDirectory dir
      case (r :: Either IOException ()) of
        Right ()                   -> return ()
        Left  e
          | isDoesNotExistError  e -> notExistHandler e
          -- createDirectory (and indeed POSIX mkdir) does not distinguish
          -- between a dir already existing and a file already existing. So we
          -- check for it here. Unfortunately there is a slight race condition
          -- here, but we think it is benign. It could report an exeption in
          -- the case that the dir did exist but another process deletes the
          -- directory and creates a file in its place before we can check
          -- that the directory did indeed exist.
          | isAlreadyExistsError e -> (do
              withFileStatus "createDirectoryIfMissing" dir $ \st -> do
                 isDir <- isDirectory st
                 if isDir then return ()
                          else throw e
              ) `catch` ((\_ -> return ()) :: IOException -> IO ())
          | otherwise              -> throw e

{- | @'removeDirectory' dir@ removes an existing directory /dir/.  The
implementation may specify additional constraints which must be
satisfied before a directory can be removed (e.g. the directory has to
be empty, or may not be in use by other processes).  It is not legal
for an implementation to partially remove a directory unless the
entire directory is removed. A conformant implementation need not
support directory removal in all situations (e.g. removal of the root
directory).

The operation may fail with:

* 'HardwareFault'
A physical I\/O error has occurred.
EIO

* 'InvalidArgument'
The operand is not a valid directory name.
[ENAMETOOLONG, ELOOP]

* 'isDoesNotExistError' \/ 'NoSuchThing'
The directory does not exist.
@[ENOENT, ENOTDIR]@

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@

* 'UnsatisfiedConstraints'
Implementation-dependent constraints are not satisfied.
@[EBUSY, ENOTEMPTY, EEXIST]@

* 'UnsupportedOperation'
The implementation does not support removal in this situation.
@[EINVAL]@

* 'InappropriateType'
The operand refers to an existing non-directory object.
@[ENOTDIR]@

-}

removeDirectory :: FilePath -> IO ()
removeDirectory path =
  Win32.removeDirectory path

-- | @'removeDirectoryRecursive' dir@  removes an existing directory /dir/
-- together with its content and all subdirectories. Be careful,
-- if the directory contains symlinks, the function will follow them.
removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive startLoc = do
  cont <- getDirectoryContents startLoc
  sequence_ [rm (startLoc </> x) | x <- cont, x /= "." && x /= ".."]
  removeDirectory startLoc
  where
    rm :: FilePath -> IO ()
    rm f = do temp <- try (removeFile f)
              case temp of
                Left e  -> do isDir <- doesDirectoryExist f
                              -- If f is not a directory, re-throw the error
                              unless isDir $ throw (e :: SomeException)
                              removeDirectoryRecursive f
                Right _ -> return ()

{- |'removeFile' /file/ removes the directory entry for an existing file
/file/, where /file/ is not itself a directory. The
implementation may specify additional constraints which must be
satisfied before a file can be removed (e.g. the file may not be in
use by other processes).

The operation may fail with:

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'InvalidArgument'
The operand is not a valid file name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError' \/ 'NoSuchThing'
The file does not exist.
@[ENOENT, ENOTDIR]@

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@

* 'UnsatisfiedConstraints'
Implementation-dependent constraints are not satisfied.
@[EBUSY]@

* 'InappropriateType'
The operand refers to an existing directory.
@[EPERM, EINVAL]@

-}

removeFile :: FilePath -> IO ()
removeFile path =
  Win32.deleteFile path

{- |@'renameDirectory' old new@ changes the name of an existing
directory from /old/ to /new/.  If the /new/ directory
already exists, it is atomically replaced by the /old/ directory.
If the /new/ directory is neither the /old/ directory nor an
alias of the /old/ directory, it is removed as if by
'removeDirectory'.  A conformant implementation need not support
renaming directories in all situations (e.g. renaming to an existing
directory, or across different physical devices), but the constraints
must be documented.

On Win32 platforms, @renameDirectory@ fails if the /new/ directory already
exists.

The operation may fail with:

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'InvalidArgument'
Either operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError' \/ 'NoSuchThing'
The original directory does not exist, or there is no path to the target.
@[ENOENT, ENOTDIR]@

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@

* 'ResourceExhausted'
Insufficient resources are available to perform the operation.
@[EDQUOT, ENOSPC, ENOMEM, EMLINK]@

* 'UnsatisfiedConstraints'
Implementation-dependent constraints are not satisfied.
@[EBUSY, ENOTEMPTY, EEXIST]@

* 'UnsupportedOperation'
The implementation does not support renaming in this situation.
@[EINVAL, EXDEV]@

* 'InappropriateType'
Either path refers to an existing non-directory object.
@[ENOTDIR, EISDIR]@

-}

renameDirectory :: FilePath -> FilePath -> IO ()
renameDirectory opath npath = do
   -- XXX this test isn't performed atomically with the following rename
   -- ToDo: use Win32 API
   withFileStatus "renameDirectory" opath $ \st -> do
   is_dir <- isDirectory st
   if (not is_dir)
    then ioException (ioeSetErrorString
                          (mkIOError InappropriateType "renameDirectory" Nothing (Just opath))
                          "not a directory")
    else do
   Win32.moveFileEx opath npath Win32.mOVEFILE_REPLACE_EXISTING

{- |@'renameFile' old new@ changes the name of an existing file system
object from /old/ to /new/.  If the /new/ object already
exists, it is atomically replaced by the /old/ object.  Neither
path may refer to an existing directory.  A conformant implementation
need not support renaming files in all situations (e.g. renaming
across different physical devices), but the constraints must be
documented.

The operation may fail with:

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'InvalidArgument'
Either operand is not a valid file name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError' \/ 'NoSuchThing'
The original file does not exist, or there is no path to the target.
@[ENOENT, ENOTDIR]@

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@

* 'ResourceExhausted'
Insufficient resources are available to perform the operation.
@[EDQUOT, ENOSPC, ENOMEM, EMLINK]@

* 'UnsatisfiedConstraints'
Implementation-dependent constraints are not satisfied.
@[EBUSY]@

* 'UnsupportedOperation'
The implementation does not support renaming in this situation.
@[EXDEV]@

* 'InappropriateType'
Either path refers to an existing directory.
@[ENOTDIR, EISDIR, EINVAL, EEXIST, ENOTEMPTY]@

-}

renameFile :: FilePath -> FilePath -> IO ()
renameFile opath npath = do
   -- XXX this test isn't performed atomically with the following rename
   -- ToDo: use Win32 API
   withFileOrSymlinkStatus "renameFile" opath $ \st -> do
   is_dir <- isDirectory st
   if is_dir
    then ioException (ioeSetErrorString
              (mkIOError InappropriateType "renameFile" Nothing (Just opath))
              "is a directory")
    else do
   Win32.moveFileEx opath npath Win32.mOVEFILE_REPLACE_EXISTING

{- |@'copyFile' old new@ copies the existing file from /old/ to /new/.
If the /new/ file already exists, it is atomically replaced by the /old/ file.
Neither path may refer to an existing directory.  The permissions of /old/ are
copied to /new/, if possible.
-}

copyFile :: FilePath -> FilePath -> IO ()
copyFile fromFPath toFPath =
    copy `Prelude.catch` (\exc -> throw $ ioeSetLocation exc "copyFile")
    where copy = bracket (openBinaryFile fromFPath ReadMode) hClose $ \hFrom ->
                 bracketOnError openTmp cleanTmp $ \(tmpFPath, hTmp) ->
                 do allocaBytes bufferSize $ copyContents hFrom hTmp
                    hClose hTmp
                    ignoreIOExceptions $ copyPermissions fromFPath tmpFPath
                    renameFile tmpFPath toFPath
          openTmp = openBinaryTempFile (takeDirectory toFPath) ".copyFile.tmp"
          cleanTmp (tmpFPath, hTmp)
              = do ignoreIOExceptions $ hClose hTmp
                   ignoreIOExceptions $ removeFile tmpFPath
          bufferSize = 1024

          copyContents hFrom hTo buffer = do
                  count <- hGetBuf hFrom buffer bufferSize
                  when (count > 0) $ do
                          hPutBuf hTo buffer count
                          copyContents hFrom hTo buffer

          ignoreIOExceptions io = io `catch` ioExceptionIgnorer
          ioExceptionIgnorer :: IOException -> IO ()
          ioExceptionIgnorer _ = return ()

-- | Given path referring to a file or directory, returns a
-- canonicalized path, with the intent that two paths referring
-- to the same file\/directory will map to the same canonicalized
-- path. Note that it is impossible to guarantee that the
-- implication (same file\/dir \<=\> same canonicalizedPath) holds
-- in either direction: this function can make only a best-effort
-- attempt.
canonicalizePath :: FilePath -> IO FilePath
canonicalizePath fpath =
    do path <- Win32.getFullPathName fpath
       return (normalise path)
        -- normalise does more stuff, like upper-casing the drive letter


-- | 'makeRelative' the current directory.
makeRelativeToCurrentDirectory :: FilePath -> IO FilePath
makeRelativeToCurrentDirectory x = do
    cur <- getCurrentDirectory
    return $ makeRelative cur x

-- | Given an executable file name, searches for such file in the
-- directories listed in system PATH. The returned value is the path
-- to the found executable or Nothing if an executable with the given
-- name was not found. For example (findExecutable \"ghc\") gives you
-- the path to GHC.
--
-- The path returned by 'findExecutable' corresponds to the
-- program that would be executed by 'System.Process.createProcess'
-- when passed the same string (as a RawCommand, not a ShellCommand).
--
-- On Windows, 'findExecutable' calls the Win32 function 'SearchPath',
-- which may search other places before checking the directories in
-- @PATH@.  Where it actually searches depends on registry settings,
-- but notably includes the directory containing the current
-- executable. See
-- <http://msdn.microsoft.com/en-us/library/aa365527.aspx> for more
-- details.
--
findExecutable :: String -> IO (Maybe FilePath)
findExecutable binary =
  Win32.searchPath Nothing binary ('.':exeExtension)


{- |@'getDirectoryContents' dir@ returns a list of /all/ entries
in /dir/.

The operation may fail with:

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'InvalidArgument'
The operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError' \/ 'NoSuchThing'
The directory does not exist.
@[ENOENT, ENOTDIR]@

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EACCES]@

* 'ResourceExhausted'
Insufficient resources are available to perform the operation.
@[EMFILE, ENFILE]@

* 'InappropriateType'
The path refers to an existing non-directory object.
@[ENOTDIR]@

-}

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents path =
  modifyIOError ((`ioeSetFileName` path) .
                 (`ioeSetLocation` "getDirectoryContents")) $ do
  bracket
     (Win32.findFirstFile (path </> "*"))
     (\(h,_) -> Win32.findClose h)
     (\(h,fdat) -> loop h fdat [])
  where
        -- we needn't worry about empty directories: adirectory always
        -- has at least "." and ".." entries
    loop :: Win32.HANDLE -> Win32.FindData -> [FilePath] -> IO [FilePath]
    loop h fdat acc = do
       filename <- Win32.getFindDataFileName fdat
       more <- Win32.findNextFile h fdat
       if more
          then loop h fdat (filename:acc)
          else return (filename:acc)
                 -- no need to reverse, ordering is undefined

{- |If the operating system has a notion of current directories,
'getCurrentDirectory' returns an absolute path to the
current directory of the calling process.

The operation may fail with:

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'isDoesNotExistError' \/ 'NoSuchThing'
There is no path referring to the current directory.
@[EPERM, ENOENT, ESTALE...]@

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EACCES]@

* 'ResourceExhausted'
Insufficient resources are available to perform the operation.

* 'UnsupportedOperation'
The operating system has no notion of current directory.

-}
getCurrentDirectory :: IO FilePath
getCurrentDirectory = do
  Win32.getCurrentDirectory

{- |If the operating system has a notion of current directories,
@'setCurrentDirectory' dir@ changes the current
directory of the calling process to /dir/.

The operation may fail with:

* 'HardwareFault'
A physical I\/O error has occurred.
@[EIO]@

* 'InvalidArgument'
The operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError' \/ 'NoSuchThing'
The directory does not exist.
@[ENOENT, ENOTDIR]@

* 'isPermissionError' \/ 'PermissionDenied'
The process has insufficient privileges to perform the operation.
@[EACCES]@

* 'UnsupportedOperation'
The operating system has no notion of current directory, or the
current directory cannot be dynamically changed.

* 'InappropriateType'
The path refers to an existing non-directory object.
@[ENOTDIR]@

-}

setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory path =
  Win32.setCurrentDirectory path

{- |The operation 'doesDirectoryExist' returns 'True' if the argument file
exists and is a directory, and 'False' otherwise.
-}

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist name =
   (withFileStatus "doesDirectoryExist" name $ \st -> isDirectory st)
   `catch` ((\ _ -> return False) :: IOException -> IO Bool)

{- |The operation 'doesFileExist' returns 'True'
if the argument file exists and is not a directory, and 'False' otherwise.
-}

doesFileExist :: FilePath -> IO Bool
doesFileExist name =
   (withFileStatus "doesFileExist" name $ \st -> do b <- isDirectory st; return (not b))
   `catch` ((\ _ -> return False) :: IOException -> IO Bool)

{- |The 'getModificationTime' operation returns the
clock time at which the file or directory was last modified.

The operation may fail with:

* 'isPermissionError' if the user is not permitted to access
  the modification time; or

* 'isDoesNotExistError' if the file or directory does not exist.

-}

getModificationTime :: FilePath -> IO ClockTime
getModificationTime name = do
 -- ToDo: use Win32 API
 withFileStatus "getModificationTime" name $ \ st -> do
 modificationTime st


withFileStatus :: String -> FilePath -> (Ptr CStat -> IO a) -> IO a
withFileStatus loc name f = do
  modifyIOError (`ioeSetFileName` name) $
    allocaBytes sizeof_stat $ \p ->
      withFilePath (fileNameEndClean name) $ \s -> do
        throwErrnoIfMinus1Retry_ loc (c_stat s p)

withFileOrSymlinkStatus :: String -> FilePath -> (Ptr CStat -> IO a) -> IO a
withFileOrSymlinkStatus loc name f = do
  modifyIOError (`ioeSetFileName` name) $
    allocaBytes sizeof_stat $ \p ->
      withFilePath name $ \s -> do
        throwErrnoIfMinus1Retry_ loc (lstat s p)

modificationTime :: Ptr CStat -> IO ClockTime
modificationTime stat = do
    mtime <- st_mtime stat
    let realToInteger = round . realToFrac :: Real a => a -> Integer
    return (TOD (realToInteger (mtime :: CTime)) 0)

isDirectory :: Ptr CStat -> IO Bool
isDirectory stat = do
  mode <- st_mode stat
  return (s_isdir mode)

fileNameEndClean :: String -> String
fileNameEndClean name = if isDrive name then addTrailingPathSeparator name
                                        else dropTrailingPathSeparator name

foreign import ccall unsafe "HsDirectory.h __hscore_S_IRUSR" s_IRUSR :: CMode
foreign import ccall unsafe "HsDirectory.h __hscore_S_IWUSR" s_IWUSR :: CMode
foreign import ccall unsafe "HsDirectory.h __hscore_S_IXUSR" s_IXUSR :: CMode
foreign import ccall unsafe "__hscore_S_IFDIR" s_IFDIR :: CMode


foreign import ccall unsafe "__hscore_long_path_size"
  long_path_size :: Int

{- | Returns the current user's home directory.

The directory returned is expected to be writable by the current user,
but note that it isn't generally considered good practice to store
application-specific data here; use 'getAppUserDataDirectory'
instead.

On Unix, 'getHomeDirectory' returns the value of the @HOME@
environment variable.  On Windows, the system is queried for a
suitable path; a typical path might be
@C:/Documents And Settings/user@.

The operation may fail with:

* 'UnsupportedOperation'
The operating system has no notion of home directory.

* 'isDoesNotExistError'
The home directory for the current user does not exist, or
cannot be found.
-}
getHomeDirectory :: IO FilePath
getHomeDirectory =
  modifyIOError ((`ioeSetLocation` "getHomeDirectory")) $ do
  r <- try $ Win32.sHGetFolderPath nullPtr Win32.cSIDL_PROFILE nullPtr 0
  case (r :: Either IOException String) of
    Right s -> return s
    Left  _ -> do
      r1 <- try $ Win32.sHGetFolderPath nullPtr Win32.cSIDL_WINDOWS nullPtr 0
      case r1 of
        Right s -> return s
        Left  e -> ioError (e :: IOException)

{- | Returns the pathname of a directory in which application-specific
data for the current user can be stored.  The result of
'getAppUserDataDirectory' for a given application is specific to
the current user.

The argument should be the name of the application, which will be used
to construct the pathname (so avoid using unusual characters that
might result in an invalid pathname).

Note: the directory may not actually exist, and may need to be created
first.  It is expected that the parent directory exists and is
writable.

On Unix, this function returns @$HOME\/.appName@.  On Windows, a
typical path might be

> C:/Documents And Settings/user/Application Data/appName

The operation may fail with:

* 'UnsupportedOperation'
The operating system has no notion of application-specific data directory.

* 'isDoesNotExistError'
The home directory for the current user does not exist, or
cannot be found.
-}
getAppUserDataDirectory :: String -> IO FilePath
getAppUserDataDirectory appName = do
  modifyIOError ((`ioeSetLocation` "getAppUserDataDirectory")) $ do
  s <- Win32.sHGetFolderPath nullPtr Win32.cSIDL_APPDATA nullPtr 0
  return (s++'\\':appName)

{- | Returns the current user's document directory.

The directory returned is expected to be writable by the current user,
but note that it isn't generally considered good practice to store
application-specific data here; use 'getAppUserDataDirectory'
instead.

On Unix, 'getUserDocumentsDirectory' returns the value of the @HOME@
environment variable.  On Windows, the system is queried for a
suitable path; a typical path might be
@C:\/Documents and Settings\/user\/My Documents@.

The operation may fail with:

* 'UnsupportedOperation'
The operating system has no notion of document directory.

* 'isDoesNotExistError'
The document directory for the current user does not exist, or
cannot be found.
-}
getUserDocumentsDirectory :: IO FilePath
getUserDocumentsDirectory = do
  modifyIOError ((`ioeSetLocation` "getUserDocumentsDirectory")) $ do
  Win32.sHGetFolderPath nullPtr Win32.cSIDL_PERSONAL nullPtr 0

{- | Returns the current directory for temporary files.

On Unix, 'getTemporaryDirectory' returns the value of the @TMPDIR@
environment variable or \"\/tmp\" if the variable isn\'t defined.
On Windows, the function checks for the existence of environment variables in
the following order and uses the first path found:

*
TMP environment variable.

*
TEMP environment variable.

*
USERPROFILE environment variable.

*
The Windows directory

The operation may fail with:

* 'UnsupportedOperation'
The operating system has no notion of temporary directory.

The function doesn\'t verify whether the path exists.
-}
getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = do
  Win32.getTemporaryDirectory

-- ToDo: This should be determined via autoconf (AC_EXEEXT)
-- | Extension for executable files
-- (typically @\"\"@ on Unix and @\"exe\"@ on Windows or OS\/2)
exeExtension :: String
exeExtension = "exe"
