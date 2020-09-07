{-# LANGUAGE CPP, ViewPatterns, PatternSynonyms #-}

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
#else
#define IS_POSIX
#endif

{- |
    Module      :  Data.FilePath
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @Data.FilePath@ provides pattern synonyms similar to @System.FilePath@
    functions. So you don't need the @filepath@ package, when use @sdp-io@.
-}
module Data.FilePath
(
  -- * Separator predicates
  FilePath, isPathSep, pattern PathSep,
  
  -- * @$PATH@
  getPath,
  
  -- * Drive
  pattern (:\\),
  
  -- * Extension functions
  pattern (:.), pattern (:..),
  
  -- * Filename\/directory functions
  pattern (:/), pattern (://), pattern Path, pattern Dirs
)
where

import Prelude ()
import SDP.SafePrelude hiding ( many )

import SDP.Linear

import Data.Bifunctor

#ifndef IS_POSIX
import Data.Maybe
import Data.Char

import Text.ParserCombinators.ReadPrec ( lift )
import Text.ParserCombinators.ReadP

import Text.Read.SDP ( readMaybeBy )
#endif

import System.Environment

default ()

infixr 7 :., :.. -- like <.>
infixr 5 :/, :// -- like </>

--------------------------------------------------------------------------------

-- Internal separator check.
isPathSep :: Char -> Bool
#ifdef IS_POSIX
isPathSep =  (== '/')
#else
isPathSep =  (\ c -> c == '/' || c == '\\')
#endif

pattern PathSep :: Char
#ifdef IS_POSIX
pattern PathSep =  '/'
#else
pattern PathSep <- ((\ c -> c == '/' || c == '\\') -> True) where PathSep = '\\'
#endif

--------------------------------------------------------------------------------

{- PATH parse. -}

-- | Get a list of 'FilePath's in the $PATH.
getPath :: IO [FilePath]
getPath =
#ifdef IS_POSIX
    map (\ dir -> null dir ? "." $ dir) . splitsBy (== ':') <$> getEnv "PATH"
#else
    select (null ?- literal) . splitsBy (== ';') <$> getEnv "PATH"
  where
    literal ('"' : (path :< '"')) = path
    literal         path          = path
#endif

--------------------------------------------------------------------------------

{- Path/extension split and join. -}

{-# COMPLETE (:.) #-}

{- |
  Add an extension.
  
  > "/directory/path" :. "ext" == "/directory/path.ext"
  > "file.txt" :. "tar" == "file.txt.tar"
  > "file." :. ".tar" == "file..tar"
  > "file" :. ".xml" == "file.xml"
  > "/" :. "d" == "/.d"
  
  Windows:
  
  > "\\\\share" :. ".txt" == "\\\\share\\.txt"
  
  Split on the extension. Note that points are discarded.
  
  > ("file" :. "") <- "file"
  > ("out" :. "txt") <- "out.txt"
  > ("/etc/pam.d/" :. "") <- "/etc/pam.d/"
  > ("dir.d/fnya" :. "") <- "dir.d/fnya"
  > ("dir.d/data" :. "bak") <- "dir.d/data.bak"
  > ("/directory/path" :. "ext") <- "/directory/path.ext"
  > ("file/path.txt.alice" :. "bob") <- "file/path.txt.alice.bob"
  
  Note that filenames starting with a @.@ are handled correctly:
  
  > (".bashrc" :. "") <- ".bashrc"
-}
pattern (:.) :: FilePath -> String -> FilePath
pattern path :. ext <- (splitExt -> (path, ext)) where (:.) = addExt

splitExt :: FilePath -> (String, String)
splitExt path = null name ? (path, "") $ (dir ++ name, ext)
  where
    (name, ext) = divideBy (== '.') file
    (dir, file) = dirName path

addExt :: FilePath -> String -> FilePath
addExt file "" = file
addExt (drive :\\ path) ext@('.' : _) = drive :\\ (path ++ ext)
addExt (drive :\\ path) ext = drive :\\ (path ++ '.' : ext)

{-# COMPLETE (:..) #-}

{- |
  Add an extensions.
  
  > x :.. [] = x -- forall x
  > path :.. [ext] = path :. ext -- forall path ext
  
  > "dir/file" :.. ["fb2", "zip"] == "dir/file.fb2.zip"
  > "dir/file" :.. ["fb2", ".zip"] == "dir/file.fb2.zip"
  > "pacman" :.. ["pkg", "tar", "xz"] == "pacman.pkg.tar.xz"
  
  Split on the extensions. Note that points are discarded.
  
  > ("file" :.. []) <- "file"
  > ("out" :.. ["txt"]) <- "out.txt"
  > ("/etc/pam.d/" :.. []) <- "/etc/pam.d/"
  > ("dir.d/fnya" :.. []) <- "dir.d/fnya"
  > ("dir.d/data" :.. ["bak"]) <- "dir.d/data.bak"
  > ("/directory/path" :.. ["ext"]) <- "/directory/path.ext"
  > ("file/path." :.. ["txt", "alice", "bob"]) <- "file/path.txt.alice.bob"
  
  This function separates the extensions and also considers the case when the
  file name begins with a period.
  
  > splitExtensions "file/.path.txt.alice.bob" == ("file/", ".path.txt.alice.bob")
  > ("file/.path" :.. [txt, alice, bob]) <- "file/.path.txt.alice.bob"
  > splitExtensions ".bashrc" == ("", ".bashrc")
  > (".bashrc" :.. []) <- ".bashrc"
-}
pattern (:..) :: FilePath -> [String] -> FilePath
pattern path :.. exts <- (splitExts -> (path, exts)) where (:..) = foldl (:.)

splitExts :: FilePath -> (FilePath, [String])
splitExts path = case splitsBy (== '.') file' of
    (name : exts) -> (dir ++ pt ++ name, exts)
    _             -> (path, [])
  where
    (pt, file') = spanl (== '.') file
    (dir, file) = dirName path

--------------------------------------------------------------------------------

{- Directory/file split and join. -}

{-# COMPLETE (:/) #-}

{- |
  Split a filename into directory and file. The first component will often end
  with a trailing slash.
  
  > "/directory/" :/ "file.ext" <- "/directory/file.ext"
  > "file/" :/ "bob.txt" <- "file/bob.txt"
  > "./" :/ "bob" <- "bob"
  
  Posix:
  
  > "/" :/ "" <- "/"
  
  Windows:
  
  > "c:" :/ "" <- "c:"
  
  Combine two paths with a path separator.
  
  If the second path looks like an absolute path or a drive, then it returns the
  second.
  
  > "directory" :/ "/file.ext" == "/file.ext"
  
  Posix:
  
  > "/directory" :/ "file.ext" == "/directory/file.ext"
  > "/" :/ "tmp" == "/tmp"
  > "x:" :/ "foo" == "x:/foo"
  > "home" :/ "/user" == "/user"
  > "home" :/ "user" == "home/user"
  
  Windows:
  
  > "/directory" :/ "file.ext" == "/directory\\file.ext"
  > "C:\\foo" :/ "bar" == "C:\\foo\\bar"
  > "home" :/ "C:\\bob" == "C:\\bob"
  > "home" :/ "bob" == "home\\bob"
  
  > "C:\\home" :/ "\\bob" == "\\bob"
  > "home" :/ "\\bob" == "\\bob"
  > "home" :/ "/bob" == "/bob"
  
  > "D:\\foo" :/ "C:bar" == "C:bar"
  > "C:\\foo" :/ "C:bar" == "C:bar"
-}
pattern (:/) :: FilePath -> FilePath -> FilePath
pattern dir :/ file <- (splitName -> (dir, file))
  where
    a :/ b@(drive :\\ _) = headIs isPathSep b || drive /= "" ? b $ combine a b

splitName :: FilePath -> (FilePath, FilePath)
splitName =  first (\ dir -> null dir ? "./" $ dir) . dirName

dirName :: FilePath -> (FilePath, FilePath)
dirName (drive :\\ path) = (drive ++) `first` breakr isPathSep path

--------------------------------------------------------------------------------

{-# COMPLETE Path #-}

-- | Separates filepath into a search path (list of ancestors and file name).
pattern Path :: [FilePath] -> FilePath
pattern Path path <- (splitPath -> path) where Path = foldr (:/) []

splitPath :: FilePath -> [FilePath]
splitPath (drive :\\ path) = null drive ? f path $ drive : f path
  where
    f "" = []
    f y  = (a ++ c) : f d
      where
        (a, b) = breakl isPathSep y
        (c, d) = spanl  isPathSep b

{-# COMPLETE Dirs #-}

-- | Same as 'Path', but removes trailing path separators.
pattern Dirs :: [FilePath] -> FilePath
pattern Dirs dirs <- (splitDirs -> dirs) where Dirs = foldr (:/) []

splitDirs :: FilePath -> [FilePath]
splitDirs =  map stripSlash . splitPath
  where
    stripSlash (drive :\\ "") = drive
    stripSlash path = not (lastIs isPathSep path) ? path $
      case dropEnd isPathSep path of {"" -> [last path]; dir -> dir}

{-# COMPLETE (://) #-}

-- | Splits/joins directories and a file name.
pattern (://) :: [FilePath] -> FilePath -> FilePath
pattern dirs :// file <- (splitDirs -> dirs :< file) where (://) = flip $ foldr (:/)

--------------------------------------------------------------------------------

{- Drive split and join. -}

{-# COMPLETE (:\\) #-}

{- |
  Windows:
  
  > "" :\\ "file" <- "file"
  > "c:/" :\\ "file" <- "c:/file"
  > "c:\\" :\\ "file" <- "c:\\file"
  > "\\\\shared\\" :\\ "test" <- "\\\\shared\\test"
  > "\\\\shared" :\\ "" <- "\\\\shared"
  > "\\\\?\\UNC\\shared\\" :\\ "file" <- "\\\\?\\UNC\\shared\\file"
  > "\\\\?\\" :\\ "UNCshared\\file" <- "\\\\?\\UNCshared\\file"
  > "\\\\?\\d:\\" :\\ "file" <- "\\\\?\\d:\\file"
  > "" :\\ "/d" <- "/d"
  
  Posix:
  
  > "/" :\\ "test" <- "/test"
  > "//" :\\ "test" <- "//test"
  > "" :\\ "test/file" <- "test/file"
  > "" :\\ "file" <- "file"
-}
pattern (:\\) :: FilePath -> FilePath -> FilePath
pattern drive :\\ path <- (splitDrive -> (drive, path)) where (:\\) = combine

combine :: FilePath -> FilePath -> FilePath
combine a b
  | null a = b
  | null b = a
  | isPathSep (last a) = a ++ b
#ifdef IS_POSIX
  | True = a ++ '/' : b
#else
  | c : ":" <- a, isLetter' c = a ++ b
  | True = a ++ '\\' : b
#endif

splitDrive :: FilePath -> (FilePath, FilePath)
#ifdef IS_POSIX
splitDrive = spanl (== '/')
#else
splitDrive path =
  let drive = lift (letter <++ unc <++ share)
  in  ("", path) `fromMaybe` readMaybeBy drive path
#endif

--------------------------------------------------------------------------------

{- Windefs. -}

#ifndef IS_POSIX

unc :: ReadP (String, String)
unc =  do sep; sep; void (char '?'); sep; long <++ short
  where
    long  = do ci "UNC"; sep; first ("\\\\?\\UNC\\" ++) <$> shareName
    ci    = mapM_ $ \ c -> char (toLower c) <++ char (toUpper c)
    short = first ("\\\\?\\" ++) <$> letter

share :: ReadP (String, String)
share =  do sep; sep; first ("\\\\" ++) <$> shareName

shareName :: ReadP (String, String)
shareName =  (do x <- manyTill get sep; y <- end; return (x :< '\\', y)) <++
             (do x <- end; return (x, ""))

letter :: ReadP (String, String)
letter =  do c <- satisfy isLetter'; void (char ':'); slash [c, ':'] <$> end

slash :: String -> String -> (String, String)
slash a =  first (a ++) . span isPathSep

end :: ReadP String
end =  manyTill get eof

sep :: ReadP ()
sep =  void (satisfy isPathSep)

isLetter' :: Char -> Bool
isLetter' x = isAsciiLower x || isAsciiUpper x

#endif

--------------------------------------------------------------------------------

headIs :: (Char -> Bool) -> String -> Bool
headIs f es = not (null es) && f (head es)

lastIs :: (Char -> Bool) -> String -> Bool
lastIs f es = not (null es) && f (last es)


