{-# LANGUAGE Safe, CPP #-}

{- |
    Module      :  System.Handle
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
  hGetFileSize, hSetFileSize, fileSize,
  
  -- ** Detecting the end of input
  isEOF, hIsEOF,
  
  -- ** Buffering
  BufferMode (..), hFlush, hSetBuffering, hGetBuffering, hBuffering,
  
  -- ** Repositioning
  HandlePosn, hGetPosn, hSetPosn,
  
  SeekMode (..), hSeek, hTell,
  
  -- ** Properties
  hIsOpen, hIsClosed, hIsReadable, hIsWritable, hIsSeekable,
  
  -- * Terminal operations (not portable: GHC only)
  hIsTerminalDevice, hSetEcho, hGetEcho, echo,
  
  -- * Text IO
  hWaitForInput, hReady, hLookAhead,
  
  hGetChar, hPutChar, getChar, putChar,
  
  -- * Binary IO
  hSetBinaryMode, withBinaryFile, openBinaryFile,
  
  hGetBuf, hGetBufSome, hPutBuf, hPutBufNonBlocking, hGetBufNonBlocking,
  
  -- * Temporary files
  openTempFile, openBinaryTempFile, openTempFileWith', openBinaryTempFile',
  
  -- * Encoding
  hSetEncoding, hGetEncoding, encoding,
  
  TextEncoding, mkTextEncoding, localeEncoding, char8, latin1, utf8, utf8_bom,
  utf16, utf16le, utf16be, utf32, utf32le, utf32be,
  
  -- * Newline conversion
  Newline (..), NewlineMode (..), nativeNewline, hSetNewlineMode,
  
  noNewlineTranslation, universalNewlineMode, nativeNewlineMode
)
where

import Prelude ()
import SDP.SafePrelude

import qualified System.IO as IO
import System.IO
  (
    TextEncoding, Newline (..), NewlineMode (..), localeEncoding, char8, latin1,
    
    Handle, HandlePosn, IOMode (..), BufferMode (..), SeekMode (..),
    
    noNewlineTranslation, universalNewlineMode, nativeNewlineMode, nativeNewline,
    
    utf8, utf8_bom, utf16, utf16le, utf16be, utf32, utf32le, utf32be,
    
    stdin, stdout, stderr
  )

import Foreign.Ptr ( Ptr )

import Data.Field

default ()

--------------------------------------------------------------------------------

{- |
  Computation @'hClose' hdl@ makes handle @hdl@ closed. Before the computation
  finishes, if @hdl@ is writable its buffer is flushed as for 'hFlush'.
  Performing 'hClose' on a handle that has already been closed has no effect;
  doing so is not an error. All other operations on a closed handle will fail.
  If 'hClose' fails for any reason, any further operations (apart from 'hClose')
  on the handle will still fail as if @hdl@ had been successfully closed.
-}
hClose :: (MonadIO io) => Handle -> io ()
hClose =  liftIO . IO.hClose

--------------------------------------------------------------------------------

{- |
  For a handle @hdl@ which attached to a physical file, @'fileSize' hdl@
  returns the size of that file in 8-bit bytes.
-}
hGetFileSize :: (MonadIO io) => Handle -> io Integer
hGetFileSize =  liftIO . IO.hFileSize

{- |
  @'hSetFileSize' hdl size@ truncates the physical file with handle @hdl@ to
  @size@ bytes.
-}
hSetFileSize :: (MonadIO io) => Handle -> Integer -> io ()
hSetFileSize =  liftIO ... IO.hSetFileSize

-- | File size 'Field'.
fileSize :: (MonadIO io) => Field io Handle Integer
fileSize =  Field hGetFileSize hSetFileSize
  (\ record f -> do val <- f <$> hGetFileSize record; hSetFileSize record val; return val)
#if MIN_VERSION_fmr(0,2,0)
  (\ record f -> do val <- f =<< hGetFileSize record; hSetFileSize record val; return val)
#endif

--------------------------------------------------------------------------------

-- | Computation 'isEOF' is equal to @'hIsEOF' 'stdin'@
isEOF :: (MonadIO io) => io Bool
isEOF =  liftIO IO.isEOF

{- |
  For a readable handle @hdl@, @'hIsEOF' hdl@ returns 'True' if no further input
  can be taken from @hdl@ or for a physical file, if the current I/O position is
  equal to the length of the file. Otherwise, it returns 'False'.
  
  NOTE: 'hIsEOF' may block, because it has to attempt to read from the stream to
  determine whether there is any more data to be read.
-}
hIsEOF :: (MonadIO io) => Handle -> io Bool
hIsEOF =  liftIO . IO.hIsEOF

--------------------------------------------------------------------------------

{- |
  The action @'hFlush' hdl@ causes any items buffered for output in handle @hdl@
  to be sent immediately to the operating system.
  
  This operation may fail with:
  
  * 'System.IO.Error.isFullError' if the device is full;
  * 'System.IO.Error.isPermissionError' if a system resource limit would be
  exceeded. It is unspecified whether the characters in the buffer are discarded
  or retained under these circumstances.
-}
hFlush :: (MonadIO io) => Handle -> io ()
hFlush =  liftIO . IO.hFlush

{- |
  @'hSetBuffering' hdl mode@ sets the mode of buffering for handle @hdl@ on
  subsequent reads and writes.
  
  If the buffer mode is changed from 'BlockBuffering' or 'LineBuffering' to
  'NoBuffering', then
  
  * if @hdl@ is writable, the buffer is flushed as for 'hFlush';
  * if @hdl@ is not writable, the contents of the buffer is discarded.
  
  This operation may fail with:
  
  * 'System.IO.Error.isPermissionError' if the handle has already been used
  for reading or writing and the implementation does not allow the buffering
  mode to be changed.
-}
hSetBuffering :: (MonadIO io) => Handle -> BufferMode -> io ()
hSetBuffering =  liftIO ... IO.hSetBuffering

-- | @'hGetBuffering' hdl@ returns the current buffering mode for @hdl@.
hGetBuffering :: (MonadIO io) => Handle -> io BufferMode
hGetBuffering =  liftIO . IO.hGetBuffering

-- | 'Handle' buffering 'Field'.
hBuffering :: (MonadIO io) => Field io Handle BufferMode
hBuffering =  Field hGetBuffering hSetBuffering
  (\ record f -> do val <- f <$> hGetBuffering record; hSetBuffering record val; return val)
#if MIN_VERSION_fmr(0,2,0)
  (\ record f -> do val <- f =<< hGetBuffering record; hSetBuffering record val; return val)
#endif

--------------------------------------------------------------------------------

{- |
  @'hGetPosn' hdl@ returns the current I/O position of @hdl@ as a value of the
  abstract type @HandlePosn@.
-}
hGetPosn :: (MonadIO io) => Handle -> io HandlePosn
hGetPosn =  liftIO . IO.hGetPosn

{- |
  If a call to @'hGetPosn' hdl@ returns a position @p@, then @'hSetPosn' p@ sets
  the position of @hdl@ to the position it held at the time of the call to
  'hGetPosn'.
  
  This operation may fail with:
  
  * 'System.IO.Error.isPermissionError' if a system resource limit would be
  exceeded.
-}
hSetPosn :: (MonadIO io) => HandlePosn -> io ()
hSetPosn =  liftIO . IO.hSetPosn

--------------------------------------------------------------------------------

{- |
  @'hSeek' hdl mode i@ sets the position of handle @hdl@ depending on @mode@.
  The offset @i@ is given in terms of 8-bit bytes.
  
  If @hdl@ is block- or line-buffered, then seeking to a position which isn't in
  the current buffer will first cause any items in the output buffer to be
  written to the device, and then cause the input buffer to be discarded. Some
  handles may not be seekable (see 'hIsSeekable'), or only support a subset of
  the possible positioning operations (for instance, it may only be possible to
  seek to the end of a tape, or to a positive offset from the beginning or
  current position). It isn't possible to set a negative I/O position, or for a
  physical file, an I/O position beyond the current end-of-file.
  
  This operation may fail with:
  
  * 'System.IO.Error.isIllegalOperationError' if the 'Handle' isn't seekable, or
  doesn't support the requested seek mode.
  * 'System.IO.Error.isPermissionError' if a system resource limit would be
  exceeded.
-}
hSeek :: (MonadIO io) => Handle -> IO.SeekMode -> Integer -> io ()
hSeek hdl = liftIO ... IO.hSeek hdl

{- |
  @'hTell' hdl@ returns the current position of the handle @hdl@, as the number
  of bytes from the beginning of the file. The value returned may be
  subsequently passed to 'hSeek' to reposition the handle to the current
  position.
  
  This operation may fail with:
  
  * 'System.IO.Error.isIllegalOperationError' if the Handle isn't seekable.
-}
hTell :: (MonadIO io) => Handle -> io Integer
hTell =  liftIO . IO.hTell

--------------------------------------------------------------------------------

-- | Same as 'IO.hIsOpen'.
hIsOpen :: (MonadIO io) => Handle -> io Bool
hIsOpen =  liftIO . IO.hIsOpen

-- | Same as 'IO.hIsClosed'.
hIsClosed :: (MonadIO io) => Handle -> io Bool
hIsClosed =  liftIO . IO.hIsClosed

-- | Same as 'IO.hIsReadable'.
hIsReadable :: (MonadIO io) => Handle -> io Bool
hIsReadable =  liftIO . IO.hIsReadable

-- | Same as 'IO.hIsWritable'.
hIsWritable :: (MonadIO io) => Handle -> io Bool
hIsWritable =  liftIO . IO.hIsWritable

-- | Same as 'IO.hIsSeekable'.
hIsSeekable :: (MonadIO io) => Handle -> io Bool
hIsSeekable =  liftIO . IO.hIsSeekable

--------------------------------------------------------------------------------

-- | Is the handle connected to a terminal?
hIsTerminalDevice :: (MonadIO io) => Handle -> io Bool
hIsTerminalDevice =  liftIO . IO.hIsTerminalDevice

-- | Set the echoing status of a handle connected to a terminal.
hSetEcho :: (MonadIO io) => Handle -> Bool -> io ()
hSetEcho =  liftIO ... IO.hSetEcho

-- | Get the echoing status of a handle connected to a terminal.
hGetEcho :: (MonadIO io) => Handle -> io Bool
hGetEcho =  liftIO . IO.hGetEcho

-- | Echo 'Field'.
echo :: (MonadIO io) => Field io Handle Bool
echo =  Field hGetEcho hSetEcho
  (\ record f -> do val <- f <$> hGetEcho record; hSetEcho record val; return val)
#if MIN_VERSION_fmr(0,2,0)
  (\ record f -> do val <- f =<< hGetEcho record; hSetEcho record val; return val)
#endif

--------------------------------------------------------------------------------

{- |
  @'hWaitForInput' hdl t@ waits until input is available on handle @hdl@.
  It returns 'True' as soon as input is available on @hdl@, or 'False' if no
  input is available within @t@ milliseconds. Note that @hWaitForInput@ waits
  until one or more full characters are available, which means that it needs to
  do decoding, and hence may fail with a decoding error.
  
  If @t@ is less than zero, then 'hWaitForInput' waits indefinitely.
  
  This operation may fail with:
  
  * 'System.IO.Error.isEOFError' if the end of file has been reached.
  * a decoding error, if the input begins with an invalid byte sequence in this
  Handle's encoding.
  
  NOTE for GHC users: unless you use the -threaded flag,
  @'hWaitForInput' hdl t where t >= 0@ will block all other Haskell threads for
  the duration of the call. It behaves like a safe foreign call in this respect.
-}
hWaitForInput :: (MonadIO io) => Handle -> Int -> io Bool
hWaitForInput =  liftIO ... IO.hWaitForInput

{- |
  @'hReady' hdl@ indicates whether at least one item is available for input from
  handle @hdl@.
    
  This operation may fail with:
  
  * 'System.IO.Error.isEOFError' if the end of file has been reached.
-}
hReady :: (MonadIO io) => Handle -> io Bool
hReady =  liftIO . IO.hReady

{- |
  Computation @'hGetChar' hdl@ reads a character from the file or channel
  managed by @hdl@, blocking until a character is available.
  
  This operation may fail with:
  
  * 'System.IO.Error.isEOFError' if the end of file has been reached.
-}
hLookAhead :: (MonadIO io) => Handle -> io Char
hLookAhead =  liftIO . IO.hLookAhead

--------------------------------------------------------------------------------

{- |
  @'hGetChar' hdl@ reads a character from the file or channel managed by @hdl@,
  blocking until a character is available.
  
  This operation may fail with:
  
  * 'System.IO.Error.isEOFError' if the end of file has been reached.
-}
hGetChar :: (MonadIO io) => Handle -> io Char
hGetChar =  liftIO . IO.hGetChar

{- |
  Computation @'hPutChar' hdl ch@ writes the character @ch@ to the file or
  channel managed by @hdl@. Characters may be buffered if buffering is enabled
  for @hdl@.
  
  This operation may fail with:
  
  * 'System.IO.Error.isPermissionError' if another system resource limit would
  be exceeded
  * 'System.IO.Error.isFullError' if the device is full
-}
hPutChar :: (MonadIO io) => Handle -> Char -> io ()
hPutChar =  liftIO ... IO.hPutChar

-- | Read a character from the standard input device, @'hGetChar' 'stdin'@.
getChar :: (MonadIO io) => io Char
getChar =  liftIO IO.getChar

-- | Write a character to the standard output device @'hPutChar' 'stdout'@.
putChar :: (MonadIO io) => Char -> io ()
putChar =  liftIO . IO.putChar

--------------------------------------------------------------------------------

{- |
  Select binary mode ('True') or text mode ('False') on a open handle.
  
  This has the same effect as calling 'hSetEncoding' with 'char8', together with
  'hSetNewlineMode' with 'noNewlineTranslation'.
-}
hSetBinaryMode :: (MonadIO io) => Handle -> Bool -> io ()
hSetBinaryMode =  liftIO ... IO.hSetBinaryMode

{- |
  @'withBinaryFile' name mode act@ opens a file using 'openBinaryFile' and
  passes the resulting handle to the computation @act@. The handle will be
  closed on exit from 'withBinaryFile', whether by normal termination or by
  raising an exception.
-}
withBinaryFile :: (MonadIO io) => FilePath -> IOMode -> (Handle -> IO r) -> io r
withBinaryFile path = liftIO ... IO.withBinaryFile path

{- |
  Like 'IO.openFile', but open the file in binary mode. On Windows, reading a
  file in text mode (which is the default) will translate CRLF to LF, and
  writing will translate LF to CRLF. This is usually what you want with text
  files. With binary files this is undesirable; also, as usual under MS OSes,
  text mode treats control-Z as EOF. Binary mode turns off all special treatment
  of end-of-line and end-of-file characters.
-}
openBinaryFile :: (MonadIO io) => FilePath -> IOMode -> io Handle
openBinaryFile =  liftIO ... IO.openBinaryFile

--------------------------------------------------------------------------------

{- |
  @'hGetBuf' hdl buf count@ reads data from the handle @hdl@ into the buffer
  @buf@ until either EOF is reached or count 8-bit bytes have been read. It
  returns the number of bytes actually read. This may be zero if EOF was reached
  before any data was read (or if count is zero).
  
  'hGetBuf' never raises an EOF exception, instead it returns a value smaller
  than count.
  
  If the handle is a pipe or socket, and the writing end is closed, 'hGetBuf'
  will behave as if EOF was reached.
  
  'hGetBuf' ignores the prevailing 'TextEncoding' and 'NewlineMode' on the
  'Handle', and reads bytes directly.
-}
hGetBuf :: (MonadIO io) => Handle -> Ptr a -> Int -> io Int
hGetBuf hdl = liftIO ... IO.hGetBuf hdl

{- |
  @'hGetBufSome' hdl buf count@ reads data from the handle @hdl@ into the buffer
  @buf@. If there is any data available to read, then 'hGetBufSome' returns it
  immediately; it only blocks if there is no data to be read.
  
  It returns the number of bytes actually read. This may be zero if EOF was
  reached before any data was read (or if count is zero).
  
  'hGetBufSome' never raises an EOF exception, instead it returns a value
  smaller than count.
  
  If the handle is a pipe or socket, and the writing end is closed,
  'hGetBufSome' will behave as if EOF was reached.
  
  'hGetBufSome' ignores the prevailing 'TextEncoding' and 'NewlineMode' on the
  'Handle', and reads bytes directly.
-}
hGetBufSome :: (MonadIO io) => Handle -> Ptr a -> Int -> io Int
hGetBufSome hdl = liftIO ... IO.hGetBufSome hdl

{- |
  @'hPutBuf' hdl buf count@ writes count 8-bit bytes from the buffer @buf@ to
  the handle @hdl@.
  
  'hPutBuf' ignores any text encoding that applies to the 'Handle', writing the
  bytes directly to the underlying file or device.
  
  This operation may fail with:
  
  * 'GHC.IO.Exception.ResourceVanished' if the handle is a pipe or socket, and
  the reading end is closed. (If this is a POSIX system, and the program has not
  asked to ignore SIGPIPE, then a SIGPIPE may be delivered instead, whose
  default action is to terminate the program).
-}
hPutBuf :: (MonadIO io) => Handle -> Ptr a -> Int -> io ()
hPutBuf hdl = liftIO ... IO.hPutBuf hdl

{- |
  @'hGetBufNonBlocking' hdl buf count@ reads data from the handle @hdl@ into the
  buffer @buf@ until either EOF is reached, or count 8-bit bytes have been read,
  or there is no more data available to read immediately.
  
  'hGetBufNonBlocking' is identical to 'hGetBuf', except that it will never
  block waiting for data to become available, instead it returns only whatever
  data is available. To wait for data to arrive before calling
  'hGetBufNonBlocking', use 'hWaitForInput'.
  
  If the handle is a pipe or socket, and the writing end is closed,
  'hGetBufNonBlocking' will behave as if EOF was reached.
  
  'hGetBufNonBlocking' ignores the prevailing 'TextEncoding' and 'NewlineMode'
  on the 'Handle', and reads bytes directly.
  
  NOTE: on Windows, this function doesn't work correctly; it behaves
  identically to 'hGetBuf'.
-}
hPutBufNonBlocking :: (MonadIO io) => Handle -> Ptr a -> Int -> io Int
hPutBufNonBlocking hdl = liftIO ... IO.hPutBufNonBlocking hdl

{- |
  @'hGetBufNonBlocking' hdl buf count@ reads data from the handle @hdl@ into the
  buffer @buf@ until either EOF is reached, or count 8-bit bytes have been read,
  or there is no more data available to read immediately.
  
  'hGetBufNonBlocking' is identical to 'hGetBuf', except that it will never
  block waiting for data to become available, instead it returns only whatever
  data is available. To wait for data to arrive before calling
  'hGetBufNonBlocking', use 'hWaitForInput'.
  
  If the handle is a pipe or socket, and the writing end is closed,
  'hGetBufNonBlocking' will behave as if EOF was reached.
  
  'hGetBufNonBlocking' ignores the prevailing 'TextEncoding' and 'NewlineMode'
  on the 'Handle', and reads bytes directly.
  
  NOTE: on Windows, this function doesn't work correctly; it behaves identically
  to 'hGetBuf'.
-}
hGetBufNonBlocking :: (MonadIO io) => Handle -> Ptr a -> Int -> io Int
hGetBufNonBlocking hdl = liftIO ... IO.hGetBufNonBlocking hdl

--------------------------------------------------------------------------------

{- |
  Set the 'NewlineMode' on the specified Handle. All buffered data is flushed
  first.
-}
hSetNewlineMode :: (MonadIO io) => Handle -> NewlineMode -> io ()
hSetNewlineMode =  liftIO ... IO.hSetNewlineMode

{- |
  Look up the named Unicode encoding. May fail with
  
  * isDoesNotExistError if the encoding is unknown
  
  The set of known encodings is system-dependent, but includes at least:
  
  * UTF-8
  * UTF-16, UTF-16BE, UTF-16LE
  * UTF-32, UTF-32BE, UTF-32LE
  
  There is additional notation (borrowed from GNU iconv) for specifying how
  illegal characters are handled:
  
  * a suffix of //IGNORE, e.g. UTF-8//IGNORE, will cause all illegal sequences
  on input to be ignored, and on output will drop all code points that have no
  representation in the target encoding.
  * a suffix of //TRANSLIT will choose a replacement character for illegal
  sequences or code points.
  * a suffix of //ROUNDTRIP will use a PEP383-style escape mechanism to
  represent any invalid bytes in the input as Unicode codepoints (specifically,
  as lone surrogates, which are normally invalid in UTF-32). Upon output, these
  special codepoints are detected and turned back into the corresponding
  original byte.
  
  In theory, this mechanism allows arbitrary data to be roundtripped via a
  String with no loss of data. In practice, there are two limitations to be
  aware of:
  
  * This only stands a chance of working for an encoding which is an ASCII
  superset, as for security reasons we refuse to escape any bytes smaller than
  128. Many encodings of interest are ASCII supersets (in particular, you can
  assume that the locale encoding is an ASCII superset) but many (such as
  UTF-16) are not.
  * If the underlying encoding is not itself roundtrippable, this mechanism can
  fail. Roundtrippable encodings are those which have an injective mapping into
  Unicode. Almost all encodings meet this criteria, but some do not. Notably,
  Shift-JIS (CP932) and Big5 contain several different encodings of the same
  Unicode codepoint.
  
  On Windows, you can access supported code pages with the prefix CP; for
  example, "CP1250".
-}
mkTextEncoding :: (MonadIO io) => String -> io TextEncoding
mkTextEncoding =  liftIO . IO.mkTextEncoding

{- |
  @'hSetEncoding' hdl@ encoding changes the text encoding for the handle @hdl@
  to encoding. The default encoding when a 'Handle' is created is
  'localeEncoding', namely the default encoding for the current locale.
  
  To create a 'Handle' with no encoding at all, use 'openBinaryFile'. To stop
  further encoding or decoding on an existing 'Handle', use 'hSetBinaryMode'.
  
  'hSetEncoding' may need to flush buffered data in order to change the encoding
-}
hSetEncoding :: (MonadIO io) => Handle -> TextEncoding -> io ()
hSetEncoding =  liftIO ... IO.hSetEncoding

{- |
  Return the current 'TextEncoding' for the specified 'Handle', or 'Nothing' if
  the 'Handle' is in binary mode.
  
  Note that the 'TextEncoding' remembers nothing about the state of the
  encoder/decoder in use on this 'Handle'. For example, if the encoding in use
  is @UTF-16@, then using 'hGetEncoding' and 'hSetEncoding' to save and restore
  the encoding may result in an extra byte-order-mark being written to the file.
-}
hGetEncoding :: (MonadIO io) => Handle -> io (Maybe TextEncoding)
hGetEncoding =  liftIO . IO.hGetEncoding

-- | Encoding 'Field', @set hdl [encoding := Nothing] = hSetBinaryMode hdl True@
encoding :: (MonadIO io) => Field io Handle (Maybe TextEncoding)
encoding =  Field hGetEncoding hSetEncoding'
  (\ record f -> do val <- f <$> hGetEncoding record; hSetEncoding' record val; return val)
#if MIN_VERSION_fmr(0,2,0)
  (\ record f -> do val <- f =<< hGetEncoding record; hSetEncoding' record val; return val)
#endif
  where
    hSetEncoding' record = hSetBinaryMode record True `maybe` hSetEncoding record

--------------------------------------------------------------------------------

{- |
  The function creates a temporary file in 'ReadWriteMode'. The created file
  isn't deleted automatically, so you need to delete it manually. The file is
  created with permissions such that only the current user can read/write it.
  
  With some exceptions (see below), the file will be created securely in the
  sense that an attacker should not be able to cause 'openTempFile' to overwrite
  another file on the filesystem using your credentials, by putting symbolic
  links (on Unix) in the place where the temporary file is to be created. On
  Unix the O_CREAT and O_EXCL flags are used to prevent this attack, but note
  that O_EXCL is sometimes not supported on NFS filesystems, so if you rely on
  this behaviour it is best to use local filesystems only.
-}
openTempFile :: (MonadIO io) => FilePath -> String -> io (FilePath, Handle)
openTempFile =  liftIO ... IO.openTempFile

-- | Like 'openTempFile', but opens the file in binary mode.
openBinaryTempFile :: (MonadIO io) => FilePath -> String -> io (FilePath, Handle)
openBinaryTempFile =  liftIO ... IO.openBinaryTempFile

-- | Like 'openTempFile', but uses the default file permissions.
openTempFileWith' :: (MonadIO io) => FilePath -> String -> io (FilePath, Handle)
openTempFileWith' =  liftIO ... IO.openTempFileWithDefaultPermissions

-- | Like 'openBinaryTempFile', but uses the default file permissions.
openBinaryTempFile' :: (MonadIO io) => FilePath -> String -> io (FilePath, Handle)
openBinaryTempFile' =  liftIO ... IO.openBinaryTempFileWithDefaultPermissions


