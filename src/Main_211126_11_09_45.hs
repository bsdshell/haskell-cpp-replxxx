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

p1 = "/Users/cat/myfile/bitbucket/testfile/test.tex"

-- zo - open
-- za - close
sw = show
  
{-|
    Mandelbrot set:

    z₀ = 0
    z_{n+1} = z_{n}² + c

    |z_{n}| ≤ 2

    z₀ = 0
    z₁ = c = 1j1
    z₂ = z₁*z₁ + c = (1 + 1i)*(1 + 1i) = 1 + (-1) + 2i + 1 + i = 1 + 3i

-}
mandelbrot::Integer -> Integer -> C -> C -> C
mandelbrot n max z0 c = if n < max then (if norm z0 <= 2 then mandelbrot (n+1) max z1 c else C{x=9,y=9}) else z0
  where
    z1 = z0 * z0 + c
  
man::Integer -> Integer -> C -> C -> [C]
man n max z0 c = manSet
  where
    s = map(\x -> 3*x) $ map (1/100 *) [-100..100]
    mat = out (\a b -> C{x = a, y = b}) s s
    lm = (map . map) (\x -> (x, mandelbrot 0 30 z0 x)) mat
    pairSet = (map . filter) (\(_, x) -> norm x <= 2) lm
    manSet = map fst $ join pairSet
    -- lss = map(\c -> (sw $ x c) ++ " " ++ (sw $ y c)) manSet
     
main = do 
        print "Hello World"
        let c1 = C{x=1, y=1}
        let c2 = C{x=3, y=4}
        let c3 = c1 + c2
        pre c3
        let c4 = c1*c2
        pre c4
        let n = norm c1
        fw "norm" 
        pre n
        fw "mandelbrot"
        let n = 0
        let z0 = C{x=0, y=0}
        let c = C{x=1, y=1}
        let m0 = mandelbrot 0 0 z0 c
        pre m0
        let k = 1
        let m1 = mandelbrot 0 1 z0 c
        pre m1
        let m2 = mandelbrot 0 2 z0 c
        pre m2
        let m3 = mandelbrot 0 3 z0 c
        
        let s = map(\x -> 10*x) $ map (1/1000 *) [-1000..1000]
        let mat = out (\a b -> C{x = a, y = b}) s s
        pre m3
        fw "mat"
        -- prel mat
        let lm = (map . map) (\x -> (x, mandelbrot 0 30 z0 x)) mat
        -- pre lm
        let pairSet = (map . filter) (\(_, x) -> norm x <= 2) lm
        let manSet = map fst $ join pairSet
        fw "pairSet"
        -- pre pairSet
        -- let ln = map norm pairSet
        -- fw "ln"
        -- pre ln
        pp $ "len=" ++ (show $ len $ join $ pairSet)
        pre $ take 10 $ join $ pairSet
        fw $ "len = " ++ (sw $ len manSet)
        -- pre manSet
        let lss = map(\c -> (sw $ x c) ++ " " ++ (sw $ y c)) manSet
        fw "lss"
        pp $ "len=" ++ (sw $ len lss)
        pre $ take 10 lss
        writeFileList "/tmp/mandelbrot.x" lss
        pp "done!"
