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
import qualified Data.Text                 as TS  -- Strict Text
import qualified System.Console.Pretty as SCP

import qualified Text.Regex.TDFA as TD

import System.Console.Haskeline
import qualified System.Console.ANSI as AN
import AronModule
import AronToken

p1 = "/Users/cat/myfile/bitbucket/testfile/test.tex"

-- zo - open
-- za - close


{-|
main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "% "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do outputStrLn $ "Input was: " ++ input
                                return $ writeFileListAppend "/tmp/xx2.x" [input]
                                outputStrLn $ "my input => " ++ input
                                loop
-}

mycmd = [
      ":h      => help",
      ":del    => delete code and create new source file",
      ":dr n   => delete nth line and replace with current code",
      ":ls     => list source file",
      ":run    => run source file",
      ":rep    => insert snippet to source file",
      ":sw 2 3 => swap line 2 and 3",
      ":hsc    =>",
      ":cmd    =>",
      ":cr     => clear screen"
    ]


headStr = [r|
#include<iostream>
#include "AronLib.h"
// #include "catch.hpp"

#include <string>
#include <fstream>
#include <sstream>
#include <iostream>
#include <vector>

using namespace std;
// using namespace AronPrint;  // pp()
// using namespace Algorithm;
// using namespace Utility;

int main() {
    // std::cout<<"cool"<<std::endl;
       |]

libSimple = [r|
#include<iostream>
#include <string>
#include <fstream>
#include <sstream>
#include <iostream>
#include <vector>

              |]
libAronLib = [r|
#include<iostream>
#include <string>
#include <fstream>
#include <sstream>
#include <iostream>
#include <vector>
#include "AronLib.h"
using namespace std;
using namespace AronPrint;  // pp()
using namespace Algorithm;
using namespace Utility;
using namespace MatrixVector;

              |]

mainStr = [r|
int main() {

           |]
  
  
tailStr = [r|
}
         |]
  
getTemplate::String -> String -> IO ([String], [String])
getTemplate cx cy = do
                    let ls = trimList $ lines cx
                    let lt = trimList $ lines cy
                    return (ls, lt)
  
-- cppFile = "./cpp.cpp"
-- cppEditedFile = "./cpp.code"

repelPath :: IO String
repelPath = getCurrentDirectory >>= \x -> return $ x </> "src" </> "code"
  
getCppFile :: IO String
getCppFile = do
             fn <- (</> "cpp.cpp") <$> repelPath
             b <- fileExistA fn
             when (not b) $ do
               createFile fn
             return fn
             
  
getEditedFile :: IO String
getEditedFile = do
                fn <- (</> "cpp.code") <$> repelPath
                b <- fileExistA fn
                when (not b) $ do
                 createFile fn
                return fn
  
conStr = containStr

runCode:: [String ] -> [String]-> IO()
runCode cx cy = do
      clear
      -- out <- run "cat /tmp/x4.x"
      rootDir <- getpwd
      old <- timeNowSecond
      fw "repelPath"
      repelPath >>= putStrLn
      cppEditedFile <- getEditedFile
      putStrLn $ "cppEditedFile=" ++ cppEditedFile
  
      cppFile <- getCppFile
      ls <- readFileStrict cppEditedFile >>= return . lines
         
      writeFileList cppFile $ cx ++ ls ++ cy
      getpwd >>= putStrLn
      -- (ext, stout, sterr) <- runSh $ toSText $ "haskell-cpp-compile " ++ cppFile
      cd "src/code"
      (ext, stout, sterr) <- runSh $ toSText $ "cmake --build build -- -j3"
      cd rootDir
      if ext == ExitSuccess then do 
        pp "ExitSuccess" 
        -- putStrLn $ toStr stout
        clear
        out <- runCmd "./src/code/bin/cpp"
        mapM_ putStrLn out
        -- clear
      else do
        pp "ERROR:"
        putStrLn $ toStr sterr
      new <- timeNowSecond
      putStrLn ""
      let diff = new - old
      putStrLn $ "Run seconds =" ++ show diff

{--
repFile::[String] -> IO()
repFile cx = do
      replaceFileWithStr "// replaceStr00" ((unlines . reverse) cx) cppFile
--}

repFile::[String] -> [String] -> [String] -> IO()
repFile left cx right = do
      cppFile <- getCppFile
      cppEditedFile <- getEditedFile
      let ls = left ++ cx ++ right
      -- replaceFileWithStr "// replaceStr00" ((unlines . reverse) cx) cppFile
      writeFileList cppFile ls
      writeFileList cppEditedFile cx
  
delCode::IO ()
delCode = do
      cppFile <- getCppFile
      ls <- readFileStrict cppFile >>= return . lines
      let lss = splitListWhen (\x -> (conStr "BEG_" x) || (conStr "END_" x)) ls
      let h = head lss
      let t = last lss
      writeFileList cppFile $ h ++ ["// BEG_rep", "// replaceStr00", "// END_rep"] ++ t
      str <- readFileStrict cppFile 
      let zls = zipWith(\n s -> (show n) ++ " " ++ s) [1..] $ lines str
      clear
      mapM_ (\x -> putStrLn $ "\t" ++ x) zls 

clearCode::IO ()
clearCode = do
      clear

cmdRun:: String -> IO ()
cmdRun s = do
    clear
    let cmd = drop 5 s
    out <- run $ cmd 
    setCursorPos 10  4 
    mapM_ putStrLn out

cppSnippet:: String -> IO ()
cppSnippet s = do
       clear
       let cmd = trim $ drop 4 s
       stdout <- run $ "hsc " ++ cmd
       setCursorPos 20  4 
       mapM_ putStrLn stdout 

showCode :: FilePath -> IO()
showCode fn = do
              str <- readFileStrict fn
              let ls = map (concat . colorToken) $ map (tokenize) $ lines str
              let zls = zipWith(\n s -> (show n) ++ " " ++ s) [0..] $ ls
              clear
              mapM_ (\x -> putStrLn $ "\t" ++ x) zls
  
lsCode:: IO ()
lsCode = do
       cppEditedFile <- getEditedFile
       showCode cppEditedFile
       {--
       str <- readFileStrict cppEditedFile 
       let zls = zipWith(\n s -> (show n) ++ " " ++ s) [0..] $ lines str
       clear
       mapM_ (\x -> putStrLn $ "\t" ++ x) zls
       --}


helpMe::IO ()
helpMe = do
    clear        
    setCursorPos 20  4 
    printBox 2 mycmd

main::IO()
main = do
  let x = 3

  let upLine = 20
  ioRef <- newIORef $ lines libSimple
  clear
  curr <- getCurrentDirectory
  putStrLn $ "curr=" ++ curr
  (leftL, rightL) <- getTemplate headStr tailStr
  
  let loop ioRef rightL n cx = do
        leftL <- readIORef ioRef >>= \x -> return $ x ++ (lines mainStr)
        setCursorPos (20 + n)  4 
        AN.clearFromCursorToLineEnd
        -- AN.cursorForward 4
        -- s <- getLineFlush >>= return . trim
        s <- getLineFlush
        -- AN.cursorUp upLine 
        -- putStrLn $ "\t" ++ s
        if | hasPrefix ":run" s -> do
             runCode leftL rightL
             loop ioRef rightL 0 []
           | hasPrefix ":rep" s -> do
             repFile leftL cx rightL
             loop ioRef rightL 0 []
  
           | hasPrefix ":del" s -> do
             delCode             
             loop ioRef rightL 0 []
           | hasPrefix ":cr" s -> do
             clearCode             
             loop ioRef rightL 0 []
           | hasPrefix ":cmd" s -> do
             cmdRun s
             loop ioRef rightL 0 []
           | hasPrefix ":hsc" s -> do
             cppSnippet s 
             loop ioRef rightL 0 []
           | hasPrefix ":ls" s -> do
             lsCode 
             loop ioRef rightL 0 []
  
           | hasPrefix ":lib" s -> do
             let ss = trim $ drop (len ":lib") s
             if ss == "s" then do
                 putStrLn ss
                 pp "simple"
               else do
                 writeIORef ioRef (lines libAronLib)
  
             loop ioRef rightL 0 []

           | hasPrefix ":dr" s -> do
             let ns = drop 3 s
             let n = read (trim ns) :: Int
             cppEditedFile <- getEditedFile
             str <- readFileStrict cppEditedFile
             let ls = lines str
             let left = take n ls
             let right = drop (n+1) ls
             let lt = left ++ cx ++ right
             writeFileList cppEditedFile lt
             loop ioRef rightL 0 []
  
           | hasPrefix ":empty" s -> do
             cppEditedFile <- getEditedFile
             writeFileList cppEditedFile []
             lsCode
             loop ioRef rightL 0 []
  
           | hasPrefix ":add" s -> do
             cppEditedFile <- getEditedFile
             ls <- readFileStrict cppEditedFile >>= \x -> return $ lines x
             let ns = trim $ drop (len ":add") s
             let n = read ns ::Int
             let left = take (n+1) ls 
             let right = drop (n+1) ls
             let lt = left ++ cx ++ right
             writeFileList cppEditedFile lt

             loop ioRef rightL 0 []

           | hasPrefix ":dl" s -> do
             cppEditedFile <- getEditedFile
             ls <- readFileStrict cppEditedFile >>= \x -> return $ lines x
             let ns = trim $ drop (len ":dl") s
             let n = read ns ::Int
             let left = take (n) ls 
             let right = drop (n+1) ls
             let lt = left ++ right
             writeFileList cppEditedFile lt
             lsCode

             loop ioRef rightL 0 []
           | hasPrefix ":sw" s -> do
             cppEditedFile <- getEditedFile
             ls <- readFileStrict cppEditedFile >>= \x -> return $ lines x
             let two = trimList $ splitSPC $ trim $ drop (len ":sw") s
             if len two == 2 then do
               let m = let x = head two in read x ::Int
               let n = let x = last two in read x ::Int
               -- sw 2 4
               --  0 1 2 3 4 5
               --      2   4
               --  s1 = 0 1
               --  mx = 2
               --  s2 = 
               let s1 = take m ls
               let mx = take 1 $ drop m ls
               let s2 = take (n - m - 1) $ drop (m + 1) ls
               let nx = take 1 $ drop n ls
               let s3 = drop (n + 1) ls
               let lt = s1 ++ nx ++ s2 ++ mx ++ s3
               writeFileList cppEditedFile lt
               lsCode
             else do
               print "Invalid Input"
             loop ioRef rightL 0 []

           | hasPrefix ":app" s -> do
             cppFile <- getCppFile
             cppEditedFile <- getEditedFile
             ls <- readFileStrict cppEditedFile >>= \s -> return $ trimList $ lines s
             let lt = ls ++ (reverse cx)
             writeFileList cppEditedFile lt
             showCode cppEditedFile

             loop ioRef rightL 0 []

           | hasPrefix ":pre" s -> do
             cppFile <- getCppFile
             cppEditedFile <- getEditedFile
             ls <- readFileStrict cppEditedFile >>= \s -> return $ trimList $ lines s
             let lt = (reverse cx) ++ ls
             writeFileList cppEditedFile lt
             showCode cppEditedFile

             loop ioRef rightL 0 []

           | hasPrefix ":h" s -> do
             helpMe 
             loop ioRef rightL 0 []

           | hasPrefix ":q" s -> do
             pp "done"
           | otherwise -> do 
             loop ioRef rightL (n + 1) (s:cx)

  loop ioRef rightL 0 []
  print "done"
