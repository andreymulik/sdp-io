{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Trustworthy, CPP #-}

#if MIN_VERSION_fmr(0,3,0)
{-# LANGUAGE DataKinds #-}
#endif

{- |
    Module      :  System.IO.Classes
    Copyright   :  (c) Andrey Mulik 2020-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "System.IO.Classes" provides generalized path and file classes.
-}
module System.IO.Classes
(
  -- * Export
  module System.IO.Handle,
  module System.IO.Error,
  
  -- * Generalized path
  IsFilePath (..),
  
  -- * Generalized file
  IsFile (..), getContents, putContents, fileContents, withFile,
  readFile, writeFile, appendFile,
  
  -- ** Text IO
  IsTextFile (..), getLine, putStr, putStrLn, gets, puts
)
where

import Prelude ()
import SDP.SafePrelude

import Data.Functor.Identity
import Data.Default.Class
import Data.FilePath
import Data.Field

import qualified System.IO as IO
import System.IO.Handle
import System.IO.Error

import Control.Exception

default ()

--------------------------------------------------------------------------------

-- | 'IsFilePath' is type class of file path (data destination) representations.
class IsFilePath path
  where
    {- |
      @hOpen path mode@ open new text handle with block buffering and given
      @mode@ using @path@ identifier.
      
      This operation may fail with:
      
      * 'isAlreadyInUseError' if the file is already open and cannot be reopened
      * 'isDoesNotExistError' if the file doesn't exist
      * 'isPermissionError' if the user doesn't have permission to open the file
    -}
    hOpen :: (MonadIO io) => IOMode -> path -> io Handle
    hOpen mode path = hOpenWith mode path (BlockBuffering Nothing) False
    
    {- |
      @hOpenWith path mode buf bin@ open new handle with buffering mode @buf@,
      binary mode @bin@ and 'IOMode' @mode@ using @path@ identifier.
      
      This operation may fail with:

      * 'isAlreadyInUseError' if the file is already open and cannot be reopened
      * 'isDoesNotExistError' if the file does not exist
      * 'isPermissionError' if the user doesn't have permission to open the file
    -}
    hOpenWith :: (MonadIO io) => IOMode -> path -> BufferMode -> Bool -> io Handle
    
    -- | Open new temp file 'Handle' by given source identifier.
    hOpenTemp :: (MonadIO io) => path -> io (path, Handle)

--------------------------------------------------------------------------------

{- |
  'IsFile' is a type class that represents the contents of a file. 'IsFile' can
  work with any type of content, but there are two important limitations: you
  shouldn't use it to work with partial values and file types that can only
  represent one value of a given type.
  
  For example, you shouldn't use 'IsFile' to work with XML (JSON, BSON, etc.).
  But you can work with document streams (e.g. YAML), if you suddenly need
  something like this.
-}
class IsFile file
  where
    {- |
      @hGetContents hdl@ reads the contents of the file. Reading may be both
      lazily (in this case @hdl@ should be semi-closed until the file end is
      reached) or strictly. After reading the file, @hdl@ should be closed.
      
      Once a semi-closed handle becomes closed, the contents becomes fixed.
      The contents of this final value is only partially specified: it will
      contain at least all the items of the stream that were evaluated prior to
      the handle becoming closed.
      
      This operation may fail with:
      
      * 'isEOFError' if the end of file has been reached.
    -}
    hGetContents :: (MonadIO io) => Handle -> io file
    
    {- |
      @hPutContents hdl file@ writes the @file@ contents to @hdl@.
      
      This operation may fail with:
      
      * 'isFullError' if the device is full; or
      * 'isPermissionError' if another system resource limit would be exceeded.
      
      If 'hPutContents' changes the recording mode (buffering, binary/text),
      it should return the original Handle settings.
    -}
    hPutContents :: (MonadIO io) => Handle -> file -> io ()

--------------------------------------------------------------------------------

-- | Just 'hGetContents' 'stdin'.
getContents :: (MonadIO io, IsFile file) => io file
getContents =  hGetContents stdin

-- | Just 'hPutContents' 'stdout'.
putContents :: (MonadIO io, IsFile file) => file -> io ()
putContents =  hPutContents stdout

--------------------------------------------------------------------------------

{- |
  @withFile path mode act@ opens a file using 'hOpen' and passes the resulting
  handle to the computation @act@. The handle will be closed on exit from
  @withFile@, whether by normal termination or by raising an exception.
  
  If closing the handle raises an exception, then this exception will be raised
  by withFile rather than any exception raised by act.
-}
withFile :: (MonadIO io, IsFilePath path) => path -> IOMode -> (Handle -> IO a) -> io a
withFile path mode = liftIO . bracket (hOpen mode path) hClose

{- |
  The 'readFile' function reads a file and returns its contents.
  
  The specifics of reading a file (laziness/strictness, possible exceptions)
  depend on the type of resource and the 'hGetContents' implementation.
-}
readFile :: (MonadIO io, IsFilePath p, IsFile f) => p -> io f
readFile =  hOpen ReadMode >=> hGetContents

-- | @writeFile path file@ function writes the @file@ value, to the @path@.
writeFile :: (MonadIO io, IsFilePath p, IsFile f) => p -> f -> io ()
writeFile path file = withFile path WriteMode (`hPutContents` file)

-- | @appendFile path file@ appends the @file@ value, to the @path@.
appendFile :: (MonadIO io, IsFilePath p, IsFile f) => p -> f -> io ()
appendFile path file = withFile path AppendMode (`hPutContents` file)

--------------------------------------------------------------------------------

-- | 'IsTextFile' is a type class of text file representations.
class (IsFile text) => IsTextFile text
  where
    -- | Read one text line from handle.
    hGetLine :: (MonadIO io) => Handle -> io text
    
    -- | Put text to handle.
    hPutStr   :: (MonadIO io) => Handle -> text -> io ()
    
    -- | Put text line to handle.
    hPutStrLn :: (MonadIO io) => Handle -> text -> io ()
    hPutStrLn hdl text = do hPutStr hdl text; hPutChar hdl '\n'

--------------------------------------------------------------------------------

-- | Same as @hGetLine stdin@.
getLine :: (MonadIO io, IsTextFile text) => io text
getLine =  hGetLine stdin

-- | Same as @hPutStr stdout@.
putStr :: (MonadIO io, IsTextFile text) => text -> io ()
putStr =  hPutStr stdout

-- | Same as @hPutStrLn stdout@.
putStrLn :: (MonadIO io, IsTextFile text) => text -> io ()
putStrLn =  hPutStrLn stdout

-- | Short version of 'getLine'.
gets :: (MonadIO io, IsTextFile text) => io text
gets =  getLine

-- | Short version of 'putStrLn'.
puts :: (MonadIO io, IsTextFile text) => text -> io ()
puts =  putStrLn

--------------------------------------------------------------------------------

instance IsFilePath FilePath
  where
    hOpen = liftIO ... flip IO.openFile
    
    hOpenWith mode path buf bin = do
      h <- hOpen mode path
      hSetBuffering h buf
      hSetBinaryMode h bin
      return h
    
    hOpenTemp (dir :/ name) = liftIO $ first (dir :/) <$> IO.openTempFile dir name

instance IsFile String
  where
    hGetContents = liftIO  .  IO.hGetContents
    hPutContents = liftIO ... IO.hPutStr

instance IsTextFile String
  where
    hPutStrLn = liftIO ... IO.hPutStrLn
    hGetLine  = liftIO  .  IO.hGetLine
    hPutStr   = liftIO ... IO.hPutStr

--------------------------------------------------------------------------------

-- | @'fileContents' = 'sfield' 'readFile' 'writeFile'@.
#if MIN_VERSION_fmr(0,3,0)
fileContents :: (MonadIO io, IsFile file) => FieldT io '[GetA, SetA, ModifyA, ModifyMA] FilePath file
fileContents =  def `Field` SomeProp (def `Prop` Identity modifierM)
                    `Field` SomeProp (def `Prop` Identity modifier)
                    `Field` SomeProp (def `Prop` Identity setter)
                    `Field` SomeProp (def `Prop` Identity getter)
  where
    getter = AccessGet readFile
    setter = AccessSet writeFile
    
    modifier = AccessModify $ \ hdl f -> liftIO $ do
      size <- f <$> readFile hdl
      size <$ writeFile hdl size
    
    modifierM = AccessModifyM $ \ hdl go -> do
      size <- go =<< liftIO (readFile hdl)
      size <$ liftIO (writeFile hdl size)
#else
fileContents :: (MonadIO io, IsFile file) => Field io FilePath file
fileContents =  sfield readFile writeFile
#endif


