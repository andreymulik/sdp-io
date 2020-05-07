{- |
    Module      :  System.IO.Handle
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @System.IO.Handle@ is safe import of "System.IO".
-}
module System.IO.Handle
(
  -- * Handles
  Handle, hClose,
  
  -- ** Standard handles
  stdin, stdout, stderr,
  
  -- ** IO mode
  IOMode (..),
  
  -- ** File size
  hFileSize, hSetFileSize,
  
  -- ** Detecting the end of input
  isEOF, hIsEOF,
  
  -- ** Buffering
  BufferMode (..), hSetBuffering, hGetBuffering, hFlush,
  
  -- ** Repositioning
  HandlePosn, hGetPosn, hSetPosn,
  
  SeekMode (..), hSeek, hTell,
  
  -- ** Properties
  hIsOpen, hIsClosed, hIsReadable, hIsWritable, hIsSeekable,
  
  -- * Terminal operations (not portable: GHC only)
  hIsTerminalDevice, hSetEcho, hGetEcho,
  
  -- * Text IO
  hWaitForInput, hReady, hLookAhead,
  
  hGetChar, hPutChar, putChar, getChar,
  
  -- * Binary IO
  hSetBinaryMode, withBinaryFile, openBinaryFile,
  
  hGetBuf, hGetBufSome,
  
  hPutBuf, hPutBufNonBlocking, hGetBufNonBlocking,
  
  -- * Temporary files
  openTempFile,
  openBinaryTempFile,
  openTempFileWithDefaultPermissions,
  openBinaryTempFileWithDefaultPermissions,
  
  -- * Encoding
  hSetEncoding, hGetEncoding,
  
  TextEncoding, mkTextEncoding, localeEncoding, char8, latin1,
  
  utf8, utf8_bom, utf16, utf16le, utf16be, utf32, utf32le, utf32be,
  
  -- * Newline conversion
  Newline(..), NewlineMode(..), nativeNewline, hSetNewlineMode,
  
  noNewlineTranslation, universalNewlineMode, nativeNewlineMode
)
where

import System.IO


