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
  -- * FilePath
  FilePath, isPathSep, isValid, isRelative, isAbsolute,
  
  makeValid, normalise, equalFilePath, makeRelative,
  
  -- * @$PATH@
  getPath,
  
  -- * Patterns
  pattern PathSep,
  
  -- ** Drive
  pattern (:\\),
  
  -- ** Extensions
  pattern (:.), pattern (:..),
  
  -- ** Split path
  pattern (:/), pattern (://), pattern Path, pattern Dirs
)
where

import Prelude ()
import SDP.SafePrelude hiding ( many )
import SDP.Linear

import Data.Bifunctor
import Data.Function
import Data.Char

#ifndef IS_POSIX
import Data.Maybe

import Text.ParserCombinators.ReadPrec ( lift )
import Text.ParserCombinators.ReadP

import Text.Read.SDP ( readMaybeBy )
#endif

import System.Environment

default ()

infixr 7 :., :.. -- like <.>
infixr 5 :/, :// -- like </>

--------------------------------------------------------------------------------

-- | Separator check.
isPathSep :: Char -> Bool
#ifdef IS_POSIX
isPathSep =  (== '/')
#else
isPathSep =  (\ c -> c == '/' || c == '\\')
#endif

-- | Default path separator.
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
    a :/ b@(drive :\\ _) = headIs isPathSep b || drive /= "" ? b $ a :\\ b

splitName :: FilePath -> (FilePath, FilePath)
splitName =  first (\ dir -> null dir ? "./" $ dir) . dirName

dirName :: FilePath -> (FilePath, FilePath)
dirName (drive :\\ path) = (drive ++) `first` breakr isPathSep path

--------------------------------------------------------------------------------

{-# COMPLETE Path #-}

{- |
  Separates filepath into a search path - list of ancestors with trailing
  separators and file name (if any):
  
  Posix:
  > Path ["/", "home/", "user/", ".ghci"] <- "/home/user/.ghci"
  > Path ["/", "home/", "user/"] <- "/home/user/"
  > Path ["/", "home/", "user"] <- "/home/user"
  
  Windows:
  > Path ["C:\\", "home\\", "user\\"] <- "C:\\home\\user\\"
  > Path ["C:\\", "home\\", "user"] <- "C:\\home\\user"
  
  'Path' concatenates the file path regardless of a trailing separator.
-}
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

{- |
  Separates filepath into a search path - list of ancestors without trailing
  separators and file name (if any):
  
  Posix:
  > Dirs ["/", "home", "user", ".ghci"] <- "/home/user/.ghci"
  > Dirs ["/", "home", "user"] <- "/home/user/"
  > Dirs ["/", "home", "user"] <- "/home/user"
  
  Windows:
  > Dirs ["C:\\", "home", "user"] <- "C:\\home\\user\\"
  > Dirs ["C:\\","home","user"] <- "C:\\home\\user"
  
  'Dirs' concatenates the file path regardless of a trailing separator.
-}
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
pattern drive :\\ path <- (splitDrive -> (drive, path))
  where
    a :\\ b
      | null a = b
      | null b = a
      | isPathSep (last a) = a ++ b
#ifndef IS_POSIX
      | c : ":" <- a, isLetter' c = a ++ b
#endif
      | True = a ++ PathSep : b

splitDrive :: FilePath -> (FilePath, FilePath)
#ifdef IS_POSIX
splitDrive = spanl (== '/')
#else
splitDrive path =
  let drive = lift (letter <++ unc <++ share)
  in  ("", path) `fromMaybe` readMaybeBy drive path
#endif

--------------------------------------------------------------------------------

{- Validity. -}

{- |
  Is a 'FilePath' valid? This function checks for invalid names, invalid
  characters, but doesn't check if length limits are exceeded, as these are
  typically filesystem dependent.
  
  > isValid "" == False
  > isValid "\0" == False
  
  Posix:
  > isValid "/random_ path:*" == True
  > isValid x => not (null x)
  
  Windows:
  > isValid "c:\\test" == True
  > isValid "c:\\test:of_test" == False
  > isValid "test*" == False
  > isValid "c:\\test\\nul" == False
  > isValid "c:\\test\\prn.txt" == False
  > isValid "c:\\nul\\file" == False
  > isValid "\\\\" == False
  > isValid "\\\\\\foo" == False
  > isValid "\\\\?\\D:file" == False
  > isValid "foo\tbar" == False
  > isValid "nul .txt" == False
  > isValid " nul.txt" == True
-}
isValid :: FilePath -> Bool
isValid  ""  = False
#ifdef IS_POSIX
isValid path = not ('\0' `elem` path)
#else
isValid (drive :\\ path@(Dirs dirs)) = not $ or
  [
    any isBadChar path,
    any (\ (name :.. _) -> isBadElem name) dirs,
    
    drive .>= 2 && all isPathSep drive,
    isDriveUNC drive && not (lastIs isPathSep drive)
  ]
#endif

{- |
  Take a FilePath and make it vali, doesn't change already valid FilePaths:
  
  > isValid (makeValid x) == True
  > isValid x => makeValid x == x
  > makeValid "" == "_"
  > makeValid "file\0name" == "file_name"
  
  Windows:
  > makeValid "c:\\already\\/valid" == "c:\\already\\/valid"
  > makeValid "c:\\test:of_test" == "c:\\test_of_test"
  > makeValid "test*" == "test_"
  > makeValid "c:\\test\\nul" == "c:\\test\\nul_"
  > makeValid "c:\\test\\prn.txt" == "c:\\test\\prn_.txt"
  > makeValid "c:\\test/prn.txt" == "c:\\test/prn_.txt"
  > makeValid "c:\\nul\\file" == "c:\\nul_\\file"
  > makeValid "\\\\\\foo" == "\\\\drive"
  > makeValid "\\\\?\\D:file" == "\\\\?\\D:\\file"
  > makeValid "nul .txt" == "nul _.txt"
-}
makeValid :: FilePath -> FilePath
makeValid "" = "_"
#ifdef IS_POSIX
makeValid path = repl (== '\0') '_' path
#else
makeValid (drive :\\ path@(Path paths))
    | drive .>= 2 && all isPathSep drive = take 2 drive ++ "drive"
    | isDriveUNC drive && lastIs isPathSep drive =
      makeValid (drive ++ "\\" ++ path)
    | True = drive :\\ Path (f <$> paths)
  where
    f = uncurry ((++) . g) . break isPathSep . repl isBadChar '_'
    g = \ x@(a :.. b) -> isBadElem a ? a ++ "_" :.. b $ x
#endif

--------------------------------------------------------------------------------

{- Absolute and relative paths. -}

{- |
  Is a path relative, or is it fixed to the root?
  
  Posix:
  > isRelative "test/path" == True
  > isRelative "/test" == False
  > isRelative "/" == False
  
  Windows:
  
  > isRelative "path\\test" == True
  > isRelative "c:\\test" == False
  > isRelative "c:test" == True
  > isRelative "c:\\" == False
  > isRelative "c:/" == False
  > isRelative "c:" == True
  > isRelative "\\\\foo" == False
  > isRelative "\\\\?\\foo" == False
  > isRelative "\\\\?\\UNC\\foo" == False
  > isRelative "/foo" == True
  > isRelative "\\foo" == True
  
  * "A UNC name of any format [is never relative]."
  * "You can't use the "\\?\" prefix with a relative path."
-}
isRelative :: FilePath -> Bool
#ifdef IS_POSIX
isRelative (dr :\\ _) = null dr
#else
isRelative (dr :\\ _) = case dr of
  (c : ':' : x : _) -> isLetter' c && not (isPathSep x)
  [c, ':']          -> isLetter' c
  xs                -> null xs
#endif

-- | Same as @not . 'isRelative'@.
isAbsolute :: FilePath -> Bool
isAbsolute =  not . isRelative

--------------------------------------------------------------------------------

{- |
  Normalise a file name:
  * \/\/ outside of the drive can be made blank
  * \/ -> 'PathSep'
  * .\/ -> \"\"
  
  > normalise "." == "."
  
  Posix:
  > normalise "/file/\\test////" == "/file/\\test/"
  > normalise "/file/./test" == "/file/test"
  > normalise "/test/file/../bob/fred/" == "/test/file/../bob/fred/"
  > normalise "../bob/fred/" == "../bob/fred/"
  > normalise "./bob/fred/" == "bob/fred/"
  
  > normalise "./" == "./"
  > normalise "./." == "./"
  > normalise "/./" == "/"
  > normalise "/" == "/"
  > normalise "bob/fred/." == "bob/fred/"
  > normalise "//home" == "/home"
  
  Windows:
  > normalise "c:\\file/bob\\" == "C:\\file\\bob\\"
  > normalise "c:\\" == "C:\\"
  > normalise "C:.\\" == "C:"
  > normalise "\\\\server\\test" == "\\\\server\\test"
  > normalise "//server/test" == "\\\\server\\test"
  > normalise "c:/file" == "C:\\file"
  > normalise "/file" == "\\file"
  > normalise "\\" == "\\"
  > normalise "/./" == "\\"
-}
normalise :: FilePath -> FilePath
normalise (drive :\\ path@(Dirs dirs)) = addPathSep ? res :< PathSep $ res
  where
    res = join' $ normDrive drive :\\ Dirs (f dirs)
    
    join' x = null x ? "." $ x
    
    addPathSep = isDirPath path
                 && not (lastIs isPathSep res)
#ifndef IS_POSIX
                 && not (isRelativeDrive drive)
#endif
    
    isDirPath xs = lastIs isPathSep xs ||
                   lastIs (== '.') xs && lastIs isPathSep (init xs)
    
    f (x : xs) = except ("." ==) $ all isPathSep x ? [PathSep] : xs $ x : xs
    f    []    = []

normDrive :: FilePath -> FilePath
#ifdef IS_POSIX
normDrive dr = null dr ? "" $ [PathSep]
#else
normDrive "" = ""
normDrive dr = isDriveLetter dosDrive ? map toUpper dosDrive $ dosDrive
  where
    dosDrive = repl (== '/') '\\' dr
#endif

{- |
  Equality of two 'FilePath's. If you call @System.Directory.canonicalizePath@
  first this has a much better chance of working. Note that this doesn't follow
  symlinks or DOSNAMEs.
  
  > x == y ==> equalFilePath x y
  > normalise x == normalise y ==> equalFilePath x y
  
  > equalFilePath "foo" "foo/"
  > not (equalFilePath "foo" "/foo")
  
  Posix:
  > not (equalFilePath "foo" "FOO")
  
  Windows:
  > equalFilePath "foo" "FOO"
  > not (equalFilePath "C:" "C:/")
-}
equalFilePath :: FilePath -> FilePath -> Bool
equalFilePath =  on (==) $
  dropSep .
#ifndef IS_POSIX
  map toLower .
#endif
  normalise

dropSep :: FilePath -> FilePath
dropSep xs = lastIs isPathSep xs && notDrive xs ? (null xs' ? [x] $ xs') $ xs
  where
    notDrive path@(_ :\\ rel) = null path || not (null rel)
    
    xs' = dropEnd isPathSep xs
    x   = last xs

--------------------------------------------------------------------------------

{- |
  Contract a filename, based on a relative path. Note that the resulting path
  will never introduce @..@ paths, as the presence of symlinks means @..\/b@ may
  not reach @a\/b@ if it starts from @a\/c@. For a worked example see
  <http://neilmitchell.blogspot.co.uk/2015/10/filepaths-are-subtle-symlinks-are-hard.html this blog post>.
  
  The corresponding @makeAbsolute@ function can be found in @System.Directory@.
  
  > makeRelative "/directory" "/directory/file.ext" == "file.ext"
  > Valid x => makeRelative (takeDirectory x) x `equalFilePath` takeFileName x
  > makeRelative x x == "."
  > Valid x y => equalFilePath x y || (isRelative x && makeRelative y x == x) || equalFilePath (y </> makeRelative y x) x
  
  Posix:
  > makeRelative "/Home" "/home/bob" == "/home/bob"
  > makeRelative "/home/" "/home/bob/foo/bar" == "bob/foo/bar"
  > makeRelative "/fred" "bob" == "bob"
  > makeRelative "/file/test" "/file/test/fred" == "fred"
  > makeRelative "/file/test" "/file/test/fred/" == "fred/"
  > makeRelative "some/path" "some/path/a/b/c" == "a/b/c"
  
  Windows:
  > makeRelative "C:\\Home" "c:\\home\\bob" == "bob"
  > makeRelative "C:\\Home" "c:/home/bob" == "bob"
  > makeRelative "C:\\Home" "D:\\Home\\Bob" == "D:\\Home\\Bob"
  > makeRelative "C:\\Home" "C:Home\\Bob" == "C:Home\\Bob"
  > makeRelative "/Home" "/home/bob" == "bob"
  > makeRelative "/" "//" == "//"
-}
makeRelative :: FilePath -> FilePath -> FilePath
makeRelative root path
    |   equalFilePath root path    = "."
    | takeAbs root /= takeAbs path = path
    |             True             = dropAbs root `f` dropAbs path
  where
    f "" y = dropWhile isPathSep y
    f x  y = equalFilePath x1 y1 ? f x2 y2 $ path
      where
        (x1, x2) = g x
        (y1, y2) = g y
    
    g = double bimap (dropWhile isPathSep) . break isPathSep . dropWhile isPathSep
    
    dropAbs pth@(drv :\\ rel) = headIs isPathSep pth && null drv ? tail pth $ rel
    takeAbs pth@(drv :\\   _) = headIs isPathSep pth && null drv ? [PathSep] $ map (\ y -> isPathSep y ? PathSep $ toLower y) drv

--------------------------------------------------------------------------------

{- Windefs. -}

#ifndef IS_POSIX
isDriveLetter :: String -> Bool
isDriveLetter x2 =  isJust (readMaybeBy (lift letter) x2)

isDriveUNC :: String -> Bool
isDriveUNC =  isJust . readMaybeBy (lift unc)

isRelativeDrive :: String -> Bool
isRelativeDrive =
  maybe False (not . lastIs isPathSep . fst) . readMaybeBy (lift letter)

isBadElem :: FilePath -> Bool
isBadElem =  (`elem` badElems) . fmap toUpper . dropEnd (== ' ')

badElems :: [FilePath]
badElems =
  [
    "COM1", "COM2", "COM3", "COM4", "COM5", "COM6", "COM7", "COM8", "COM9",
    "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6", "LPT7", "LPT8", "LPT9",
    "CON",  "PRN",  "AUX",  "NUL",  "CLOCK$"
  ]

isBadChar :: Char -> Bool
isBadChar x = x >= '\0' && x <= '\31' || x `elem` ":*?><|\""

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

repl :: (a -> Bool) -> a -> ([a] -> [a])
repl =  \ f n -> map $ \ c -> f c ? n $ c

double :: (a -> a -> b) -> a -> b
double =  \ f x -> f x x

