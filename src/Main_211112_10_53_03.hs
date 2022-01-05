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
import Numeric

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


-- zo - open
-- za - close

-- KEY: circle points, gnu plot, gnu-plot
 

{-|

    === escape code sequence

    http://localhost/html/indexTerminalColorandEscapeCodeSequence.html

    @
      Use 256 Colors in terminal

      Set foreground color: \x1b[38;5; {color code}m \x1b[0m
      Set background color: \x1b[48;5; {color code}m \x1b[0m
                               |           |            |
                               |           |            |-> reset color
                               |           |-> RGB color (0-255)
                               |
                               |->  38 => foreground
                               |->  48 => background


      24 bit RGB (Truecolor)
      {r}    {g}       {b}
      8bits  8bits     8bits = 24bits

      32 bit RGBA
      {r}    {g}       {b}    {a}
      8bits  8bits     8bits  8bits

      2^10 = 1024
      2^5 = 32 x 32 = 940 + 64 = 1024
      2^24 = (2^10)^2 * 2^2 = 1024 x 1024 = 1M*2^2 = 4M

      Set foreground color: \x1b[38;2;{r};{g};{b}m\x1b[0m
      Set background color: \x1b[48;2;{r};{g};{b}m\x1b[0m
                               |           |            |
                               |           |            |-> reset color
                               |           |-> RGB color (0-255)
                               |
                               |->  38 => foreground
                               |->  48 => background
    

    @
-}
  
printColor::Int -> String -> String
printColor n s = fg ++ color ++ s ++ reset
       where
         fg = "\x1b[38;5;"
         color = (show n) ++ "m"
         reset = "\x1b[0m"
  
printbgColor::Int -> String -> String
printbgColor n s = bg ++ color ++ s ++ reset
       where
         bg = "\x1b[48;5;"
         color = (show n) ++ "m"
         reset = "\x1b[0m"
  
main = do 
        home <- getEnv "HOME"
        -- writeFileList "/tmp/x.data" ln
        -- putStr "\x1b[38;5;223mHello\x1b[0m"
        let s1 = "\x1b[38;5;" ++ (show 223) ++ "m"
        let s2 = "\x1b[0m"
        putStrLn $ s1 ++ "abstra cad abstra" ++ s2
        putStrLn $ printColor 0 "Barista"
        putStrLn $ printColor 255 "Barista"
        pp "done!"
