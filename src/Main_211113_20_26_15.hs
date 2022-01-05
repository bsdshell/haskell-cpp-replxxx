-- {{{ begin_fold
-- script
-- #!/usr/bin/env runhaskell -i/Users/cat/myfile/bitbucket/haskelllib
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DuplicateRecordFields #-} 
-- import Turtle
-- echo "turtle"

{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE QuasiQuotes       #-} -- support raw string [r|<p>dog</p> |]
import Text.RawString.QQ       -- Need QuasiQuotes too 


-- import Data.Set   -- collide with Data.List 
import Control.Monad
import Data.Char
import Data.Typeable (typeOf) -- runtime type checker, typeOf "k"
import qualified Data.List as L
import Data.List.Split
import Data.Time
import Data.Time.Clock.POSIX
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Posix.Files
import System.Posix.Unistd
import System.Process
import Text.Read
import Text.Regex
import Text.Regex.Base
import Text.Regex.Base.RegexLike
import Text.Regex.Posix
import Data.IORef 
import Control.Monad (unless, when)
import Control.Concurrent 

import qualified Text.Regex.TDFA as TD

--import Data.Array

-- import Graphics.Rendering.OpenGL as GL 
-- import Graphics.Rendering.OpenGL.GLU.Matrix as GM  
-- import qualified Graphics.UI.GLFW as G
-- import Data.Set(Set) 
-- import qualified Data.Set as S 

--if (length argList) == 2 
--then case head argList of 
--    "svg" -> run cmd >> run "ls" >>= \x -> pp x 
--            where 
--                cmd = "pwd" 
--    "png" ->run cmd >> run ("ls " ++ fn) >>= \x -> pp x  
--            where 
--                cmd = "pwd" 
--    _     -> print "more arg" 
--else print "Need more arguments" 

--    takeFileName gives "file.ext"
--    takeDirectory gives "/directory"
--    takeExtension gives ".ext"
--    dropExtension gives "/directory/file"
--    takeBaseName gives "file"
--    "/directory" </> "file.ext".
--    "/directory/file" <.> "ext".
--    "/directory/file.txt" -<.> "ext".
-- |  end_fold ,}}}

-- shell command template:
-- 
--        argList <- getArgs
--        if len argList == 2 then do
--            let n = stringToInt $ head argList
--            let s = last argList
--            putStr $ drop (fromIntegral n) s
--        else print "drop 2 'abcd'"


import AronModule
import AronAlias

p1 = "/Users/cat/myfile/bitbucket/testfile/test.tex"

-- zo - open
-- za - close

{-|
    @
    s = " dog cat|fox"
    t = " dog catxxx|fox"
             |

    take n s  => " dog cat|fox"
         ↑                ↑
         |                |→  8 '|'
         |                    ↑
         |— - - - - - - — - - |

    drop n s  => " dog cat|fox" => |for"
         ↑                ↑
         |- - - - – - - - |



    let k1 = " dog cat|fox"
                      ↑
                      |→ index 8

    let k2 = " dog catxxx|fox"
                         ↑
                         |→ index 11


    print $ padLeft 8 11 '-' k1
                          ↑  ↑
                          |  |→ Input String
                          | 
                          |→ padding Char '-'


             " dog cat---|fox"
                         ↑
        k2 = " dog catxxx|fox"

    @
-}
padLeft::Int -> Int -> Char -> String -> String
padLeft inx m c s = s1 + rp + s2
  where
    (+) = (++)
    s1 = take inx s
    s2 = drop inx s
    rp = replicate (m - inx) c

{-|

  @
  --------------------------------------Input-------------------------------------
  [ "          else printBox 4 ["Need two arguments","
  , "                           "printBox 4 'dog'$'\\n''cat'","
  , "                           "         ↑                  ","
  , "                           "         |→ 4 tabs         ","
  , "                           "                           ","
  , "                           "                           ","
  , "                           "Three arguments","
  , "                           "printBox 4 'dog'$'\\n''cat' 200  ","
  , "                           "         ↑                   ↑    ","
  , "                           "         |→ 4 tabs           |    ","
  , "                           "                             |→ (0-255) color code ","
  , "                           "","
  , "                           "printBox 4 'dog\\ncat' => DOES NOT WORK""
  , "                          ]"
  ]
  -------------------------------alignmentStr-------------------------------------
  [ "          else printBox 4 ["Need two arguments                                 ","
  , "                           "printBox 4 'dog'$'\\n''cat'                        ","
  , "                           "         ↑                                         ","
  , "                           "         |→ 4 tabs                                 ","
  , "                           "                                                   ","
  , "                           "                                                   ","
  , "                           "Three arguments                                    ","
  , "                           "printBox 4 'dog'$'\\n''cat' 200                    ","
  , "                           "         ↑                   ↑                     ","
  , "                           "         |→ 4 tabs           |                     ","
  , "                           "                             |→ (0-255) color code ","
  , "                           "                                                   ","
  , "                           "printBox 4 'dog\\ncat' => DOES NOT WORK            ""
  ]
  @
-}
alignmentStr::Char -> Char -> [String] -> [String]
alignmentStr c pc ls = rets
  where
    ss = map (\s -> zipWith(\a b -> a == c ? (1, b, s) $ (0, b, s)) s [0..]) ls
    rr = map (\s -> filter(\(a, b, _) -> a == 1) s ) ss
    tt = map last $ map (takeEnd 2) $ filter(\x -> len x > 1) rr
    qq = map t2 tt
    m = foldl (\acc x -> max acc x) 0 qq
    rets = map (\(a, b, c) -> padLeft b m pc c ) tt



(⊖) (a1, b1) (a2, b2) = (a1 - a2, b1 - b2)
main = do 
        home <- getEnv "HOME"
        let a = (1, 2)
        let b = (4, 3)
        let c = (⊖) a b
        -- ls <- rfl "/tmp/f.x"
        ustr <- readFileUtf8 "/tmp/f.x"
        let ls = map toStr $ linesSText ustr
        let ss = map (\s -> zipWith(\a b -> a == '"' ? (1, b, s) $ (0, b, s)) s [0..]) ls
        let rr = map (\s -> filter(\(a, b, _) -> a == 1) s ) ss
        let tt = map last $ map (takeEnd 2) $ filter(\x -> len x > 1) rr
        let qq = map t2 tt
        let m = foldl (\acc x -> max acc x) 0 qq

        -- a   = b   kk jjj
        -- dog = cat kk    
        let k1 = " dog cat|fox"
        let k2 = " dog catxxx|fox"
        print $ zipWith (\x b -> x == '|' ? (1, b) $ (0, b)) k1 [0..]
        print $ zipWith (\x b -> x == '|' ? (1, b) $ (0, b)) k2 [0..] 
        print $ padLeft 8 11 '-' k1
        pre $ map (\(a, b, c) -> padLeft b m '-' c ) tt
        print "done!"
        fw "alignmentStr"
        let rets = alignmentStr '"' ' ' ls
        writeFileList "/tmp/y" rets
        fw "Input"
        pre ls
        fw "alignmentStr"
        pre rets
