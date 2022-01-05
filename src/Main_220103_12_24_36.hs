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
import qualified System.Console.Pretty as SCP
import qualified Data.Array as DR

import qualified Text.Regex.TDFA
import Text.RE.TDFA.String

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


import AronModule  hiding (re)

p1 = "/Users/cat/myfile/bitbucket/testfile/test.tex"

-- zo - open
-- za - close
--

-- vim regex
--   ^\s*[*a-zA-Z0-9_-]\+\(::\)\{0,1}[a-zA-Z0-9_-]\+\([<][^<>]+[>]\)\{0,1}[[:space:]]\+[a-zA-z0-9_-]\+[(][^()]*[)][[:space:]]*[{]\{0,1}$


data MyType = MyType{isInside::Bool} deriving (Show)
data Type2 = Type2{x_::Int, y_::Int, isIn::Int -> Int -> Bool} 

fun::MyType
fun = MyType{isInside = False}

fun2::Type2
fun2 = Type2{x_ = 1, y_ = 2}

data Shape w = Shape{getXX::Int, setX :: Int -> Shape w} 

shape x y z = Shape{setX = \x' -> shape x' y z}


{-|
    === Extract AronModule.hs functions out

    gx file:///Users/aaa/myfile/bitbucket/stackproject/jupyterlab/readAronModule.html

    <file:///Users/aaa/myfile/bitbucket/stackproject/jupyterlab/readAronModule.html Example>

    @
    [(
        [ "AronModule.f"
        , "AronModule.fu"
        , "AronModule.fun"
        , "AronModule.funx"
        , "AronModule.n"
        , "AronModule.nx"
        , "AronModule.u"
        , "AronModule.un"
        , "AronModule.unx"
        , "AronModule.x"
        ]
    , 30003
    , [ "funx::(Integral a, Num b) => a -> b" ]
    )]
    @
-}
redisExtractAronModuleX::String -> [String] -> [([String], Integer, [String])]
redisExtractAronModuleX package [] = []
redisExtractAronModuleX package cx = pMap
    where
       rstr = "(^[a-z]+[a-zA-Z0-9_]*)[[:space:]]*::"
       list = filter(\e -> matchTest (mkRegex rstr) e) cx
       lss = map(\e -> (matchAllText (mkRegex rstr) $ e, e)) list
       ln = map(\x -> (DR.elems $ (fst x) !! 0, snd x)) lss
       lns = map(\x -> (fst $ head $ tail $ fst x, snd x)) ln
       -- rMap = zipWith(\n x -> (prefix $ fst x, n, [snd x])) [30000..] lns
       -- rMap = zipWith(\n x -> (prefixSuffix $ fst x, n, [snd x])) [30000..] lns
       ixBound = redisBound Haskell
       rMap = zipWith(\n x -> (substr $ fst x, n, [snd x])) [30000..] lns

       -- => [([AronModule.k0, AronModule.k1..], Integer, [String])]
       pMap = map (\ls -> (map(\x -> package ++ x) $ t1 ls, t2 ls, t3 ls)) rMap
       --                                         ↑           ↑       ↑
       --                                     ([String]     Integer [String])
       --
       -- package = "AronModule." -- append to each keys
       substr s = unique $ join $ allSubstr s

redisExtractCppAronLib::String -> [String] -> [([String], Integer, [String])]
redisExtractCppAronLib package cx = retMap 
    where
       lt = captureCppFun cx
       rMap = zipWith(\n x -> (substr $ snd x, n, [fst x])) [40000..] lt 
       retMap = map (\ls -> (map(\x -> package ++ x) $ t1 ls, t2 ls, t3 ls)) rMap
       substr s = unique $ join $ allSubstr s

{-|
     === KEY: capture cpp function

     @
        aronPath <- getEnv "cpplib" >>= \pa -> return $ pa </> "AronLib.h"
        namels <- captureCppFun aronPath
        pre namels
        pp $ "len namels=" ++ (show $ len namels)
     @

     TODO: Does work in the following cases
     @
        std::string fun(){
        }

        vector<std::string> splitStrRegex(const string& s, string rgxStr = "\\s+") {
     @
 -}
captureCppFun::[String] -> [(String, String)]
captureCppFun ls = filter (\(a, _) -> (len . trim) a > 0) $ map captureFun funls 
    where
       rexStr = "^[[:space:]]*[*<>a-zA-Z0-9_-]+[[:space:]]+([a-zA-Z0-9_-]+)[[:space:]]*[(][^()]*[)][[:space:]]*[{]{0,1}[[:space:]]*$"
       may = map (\s -> let may = matchAnyRegex (mkRegex rexStr) s
                            str = case may of 
                                   Just tup -> takeIndexBetweenInc tup s  
                                   Nothing  -> ""
                        in str) ls
       funls = filter (\x -> (len . trim) x > 0) may 


captureFun::String -> (String, String)
captureFun s =  if matchTest rex s then read tups :: (String, String) else ("", "")
    where
      rexStr = "^[[:space:]]*([*<>a-zA-Z0-9_-]|(::))+[*<>a-zA-Z0-9_-]+[[:space:]]+([a-zA-Z0-9_-]+)[[:space:]]*[(][^()]*[)][[:space:]]*[{]{0,1}[[:space:]]*$"
      rex = mkRegex rexStr
      tups = subRegex rex s "(\"\\0\",\"\\3\")"

main = do 
        aronPath <- getEnv "cpplib" >>= \pa -> return $ pa </> "AronLib.h"
        ls <- readFileList aronPath 
        -- let ls = [" mat m(2,2) ;", " mat m(3, 3) "]
        -- let s = "vector<int> fun(int n, int k, vector<int> v)"
        -- let regexStr = "^[[:space:]]*[<>a-zA-Z0-9_-]+[[:space:]]+[a-zA-Z0-9_-]+[[:space:]]*[(][^()]*[)][[:space:]]*[^;][[:space:]]*$"
        let rexStr = "^[[:space:]]*[*<>a-zA-Z0-9_-]+[[:space:]]+([a-zA-Z0-9_-]+)[[:space:]]*[(][^()]*[)][[:space:]]*[{]{0,1}[[:space:]]*$"
        let may = map (\s -> let may = matchAnyRegex (mkRegex rexStr) s
                                 str = case may of 
                                         Just tup -> takeIndexBetweenInc tup s  
                                         Nothing  -> ""
                             in str) ls
        let funls = filter (\x -> (len . trim) x > 0) may 
        pre funls
        pre $ len funls
        writeFileList "/tmp/x.x" funls
        let nameTuple = filter (\(a, _) -> (len . trim) a > 0) $ map captureFun funls 
        let s1 = " ab 123"
        let s2 = ";"

--   ^\s*[*a-zA-Z0-9_-]\+\(::\)\{0,1}[a-zA-Z0-9_-]\+\([<][^<>]+[>]\)\{0,1}[[:space:]]\+[a-zA-z0-9_-]\+[(][^()]*[)][[:space:]]*[{]\{0,1}$
--
--    ^[a-z]\+\(::\)\{0,1}[a-z]\+\(<[^<>]*>\)\{0,1}

        let regexStr2 = "^[[:space:]]*[*<>a-zA-Z_-]+[[:space:]]+([a-zA-Z0-9_-]+)[[:space:]]*[(][^()]*[)][[:space:]]*[{]{0,1}[[:space:]]*$"
        let m = mkRegex regexStr2 
        pre $ subRegex m "void " "(\"\\0\",\"\\1\")"
        fw "rs"
        pp "done!"
        pre $ captureFun "int adf {"
        fw "nameTuple"
        pre nameTuple
        writeFileList "/tmp/x2.x" $ map show nameTuple 
        pp $ "len nameTuple=" ++ (show $ len nameTuple)
        pp $ "len funls =" ++ (show $ len funls)
        fl
        let namels = captureCppFun ls 
        pre namels
        pp $ "len namels=" ++ (show $ len namels)

        lt <- readFileList  "/tmp/x.h"
        let lts = redisExtractAronModuleX "package." lt
        pre lts
        cppls <- readFileList "/tmp/cpp.h"

        let lt = captureCppFun cppls 
        let cpp = redisExtractCppAronLib "AronLib." cppls
        fw "BEG CPP"
        pre cpp
        fw "END CPP"
        fw "BEG lt"
        pre lt
        fw "END lt"
        fl
        ls <- readTagsFile "./TAGS"
--        ls <- readFileList "./TAGS"
--        let lss = map (\s -> splitWhen(\x -> x == '\x7f' || x == '\x01') s) ls
--        let lstup = map (\s -> if len s == 3 then (head s, (head . tail) s) else ("", "")) lss
--        let lsfi = filter (\(a, _) -> len a > 0) lstup
--        writeFileList "/tmp/x1.x" $ map unlines lss
--        pre $ take 20 ls
--        pre $ take 20 lss
--        pp $ "len lss=" ++ (show $ len lss)
--        pre $ take 20 lstup
--        pp $ "len lstup=" ++ (show $ len lstup)
--        pp $ "len lsfi=" ++ (show $ len lsfi)
        pre $ take 20 ls
        pp $ "len ls=" ++ (show $ len ls)
        


readTagsFile::FilePath -> IO [(String, String)]
readTagsFile fp = do 
        ls <- readFileList fp 
        let lss = map (\s -> splitWhen(\x -> x == '\x7f' || x == '\x01') s) ls
        let lstup = map (\s -> if len s == 3 then (head s, (head . tail) s) else ("", "")) lss
        return $ filter (\(a, _) -> len a > 0) lstup



