{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

{- |
    Module      :  System.IO.Classes
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @System.IO.Classes@ provides generalized path and file classes.
-}
module System.IO.Classes
(
  -- * Export
  module System.IO.Handle,
  module System.IO.Error,
  
  -- * Generalized path
  IsFilePath (..),
  
  -- * Generalized file
  IsFile (..),
  
  getContents, putContents,
  
  withFile, readFile, writeFile, appendFile,
  
  -- ** Text IO
  IsTextFile (..),
  
  getLine, putStr, putStrLn,
  
  gets, puts
)
where

import Prelude ()
import SDP.SafePrelude

import System.IO.Handle
import qualified System.IO as IO

import Data.FilePath

import Control.Exception
import System.IO.Error

default ()

--------------------------------------------------------------------------------

-- | 'IsFilePath' is type class of file path (data destination) representations.
class IsFilePath path
  where
    {- |
      @hOpen path mode@ open new text handle with block buffering and given
      @mode@ using @path@ identifier.
      
      This operation may fail with:
      
      * @isAlreadyInUseError@ if the file is already open and cannot be reopened
      * @isDoesNotExistError@ if the file does not exist
      * @isPermissionError@ if the user doesn't have permission to open the file
    -}
    hOpen :: IOMode -> path -> IO Handle
    hOpen mode path = hOpenWith mode path (BlockBuffering Nothing) False
    
    {- |
      @hOpenWith path mode buf bin@ open new handle with buffering mode @buf@,
      binary mode @bin@ and 'IOMode' @mode@ using @path@ identifier.
      
      This operation may fail with:

      * 'isAlreadyInUseError' if the file is already open and cannot be reopened
      * 'isDoesNotExistError' if the file does not exist
      * 'isPermissionError' if the user doesn't have permission to open the file
    -}
    hOpenWith :: IOMode -> path -> BufferMode -> Bool -> IO Handle
    
    -- | Open new temp file 'Handle' by given source identifier.
    hOpenTemp :: path -> IO (path, Handle)

--------------------------------------------------------------------------------

{- |
  'IsFile' is a type class that represents the contents of a file.
  
  'IsFile' provides only basic read and write operations. It doesn't allow, for
  example, changing the 'IOMode' or handle the concatenation of the file
  contents with the appendable information.
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
      
      * isEOFError if the end of file has been reached.
    -}
    hGetContents :: Handle -> IO file
    
    {- |
      @hPutContents hdl file@ writes the @file@ contents to @hdl@.
      
      This operation may fail with:
      
      * 'isFullError' if the device is full; or
      * 'isPermissionError' if another system resource limit would be exceeded.
      
      If 'hPutContents' changes the recording mode (buffering, binary/text),
      it should return the original Handle settings.
    -}
    hPutContents :: Handle -> file -> IO ()

-- | Just 'hGetContents' 'stdin'.
getContents :: (IsFile file) => IO file
getContents =  hGetContents stdin

-- | Just 'hPutContents' 'stdout'.
putContents :: (IsFile file) => file -> IO ()
putContents =  hPutContents stdout

--------------------------------------------------------------------------------

{- |
  @withFile path mode act@ opens a file using 'hOpen' and passes the resulting
  handle to the computation @act@. The handle will be closed on exit from
  @withFile@, whether by normal termination or by raising an exception.
  
  If closing the handle raises an exception, then this exception will be raised
  by withFile rather than any exception raised by act.
-}
withFile :: (IsFilePath path) => path -> IOMode -> (Handle -> IO a) -> IO a
withFile path mode = bracket (hOpen mode path) hClose

{- |
  The 'readFile' function reads a file and returns its contents.
  
  The specifics of reading a file (laziness/strictness, possible exceptions)
  depend on the type of resource and the 'hGetContents' implementation.
-}
readFile :: (IsFilePath p, IsFile f) => p -> IO f
readFile =  hOpen ReadMode >=> hGetContents

-- | @writeFile path file@ function writes the @file@ value, to the @path@.
writeFile :: (IsFilePath p, IsFile f) => p -> f -> IO ()
writeFile path file = withFile path WriteMode (`hPutContents` file)

-- | @appendFile path file@ appends the @file@ value, to the @path@.
appendFile :: (IsFilePath p, IsFile f) => p -> f -> IO ()
appendFile path file = withFile path AppendMode (`hPutContents` file)

--------------------------------------------------------------------------------

-- | 'IsTextFile' is a type class of text file representations.
class (IsFile text) => IsTextFile text
  where
    -- | Read one text line from handle.
    hGetLine :: Handle -> IO text
    
    -- | Put text to handle.
    hPutStr   :: Handle -> text -> IO ()
    
    -- | Put text line to handle.
    hPutStrLn :: Handle -> text -> IO ()
    hPutStrLn hdl text = do hPutStr hdl text; hPutChar hdl '\n'

--------------------------------------------------------------------------------

-- | Same as @hGetLine stdin@.
getLine :: (IsTextFile text) => IO text
getLine =  hGetLine stdin

-- | Same as @hPutStr stdout@.
putStr :: (IsTextFile text) => text -> IO ()
putStr =  hPutStr stdout

-- | Same as @hPutStrLn stdout@.
putStrLn :: (IsTextFile text) => text -> IO ()
putStrLn =  hPutStrLn stdout

-- | Short version of 'getLine'
gets :: (IsTextFile text) => IO text
gets =  getLine

-- | Short version of 'purStrLn'
puts :: (IsTextFile text) => text -> IO ()
puts =  putStrLn

--------------------------------------------------------------------------------

instance IsFilePath FilePath
  where
    hOpen = flip IO.openFile
    
    hOpenWith mode path buf bin = do
      h <- hOpen mode path
      hSetBuffering h buf
      hSetBinaryMode h bin
      return h
    
    hOpenTemp (dir :/ name) = do
      (temp, hdl) <- IO.openTempFile dir name
      
      return (dir :/ temp, hdl)

instance IsFile String
  where
    hGetContents = IO.hGetContents
    hPutContents = IO.hPutStr

instance IsTextFile String
  where
    hGetLine  = IO.hGetLine
    hPutStr   = IO.hPutStr
    hPutStrLn = IO.hPutStrLn

