-- {{{ begin_fold
-- script
-- #!/usr/bin/env runhaskell -i/Users/cat/myfile/bitbucket/haskelllib
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DuplicateRecordFields #-} 
-- import Turtle
-- echo "turtle"
{-# language NumDecimals #-}
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
import System.Process.Streaming


import qualified Text.Regex.TDFA as TD
import Control.Concurrent
import Pipes
import qualified Data.ByteString.Char8 as Bytes
import qualified Data.ByteString as BS
  
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

periodic :: Producer Bytes.ByteString IO ()
periodic = go 0
    where
        s = ["dog", "cat", "dog 10", "cat 10"]
        go n = do
            d <- liftIO (pure (Bytes.pack (show s))) -- put your IO action here
            Pipes.yield d
            liftIO (threadDelay 1e4)
            go (succ n)
  
output  :: Producer Bytes.ByteString IO ()
output  = go
    where
        s = ["dog", "cat", "dog 10", "cat 10"]
        ss = unlines $ map show s
        go = do
            d <- liftIO (pure (Bytes.pack (ss))) -- put your IO action here
            Pipes.yield d
            liftIO (threadDelay 1e4)
            
  
writeFileRaw::FilePath -> String -> IO()
writeFileRaw fp s = do
  fileHandle <- openFile fp WriteMode
  hPutStr fileHandle s
  hClose fileHandle

{-|

     "a b "
  "k"   " d  "
     "    "
     "e   "


     a = b
     c = d

-}

  
-- (insert (concat "\n" (runSh "ghci-client -c  \"take 50 $ repeat '-'\"  ") ))
-- (insert (concat "\n" (runSh "ghci-client -c  \" getEnv \"HOME\" >>= print  \"  ") ))

main :: IO ()
main = do
    {-|
             (ghci ":!hsn apl")

                        +--------------------------<-pipe-------<--+---stdout-------+
                        |                                          |                |
                        |                                          |                |
                        + - - +                                    |                |
                              ↓                                    ↑                ↑
    -}
    -- executeInteractive (shell "alignmentStr -p kk"){ std_in = CreatePipe } (feedProducer output)
    -- execute            (shell "alignmentStr -p kk"){ std_in = CreatePipe } (feedProducer output)
    {-|
                       | _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ |         ↑
                                  ↑                         ↑                      |
                                  + → CreateProcess         |                      |
                                                            + -> Record            |
                                                                                   + → Stream Void a


    feedProducer :: Producer BytesString IO() -> Stream e ()

    https://hackage.haskell.org/package/process-streaming-0.9.3.0/docs/System-Process-Streaming.html
    -}
  
    -- System.Process createPipe :: IO (Handle, Handle)
    (h_read, h_write) <- createPipe
    let msg = "233"
    let ls = ["\"12\"", "\"34  \"", "\"5 6     \""]

    -- let ss = unlines ls
    let ss = unlines ls
    
    BS.hPut h_write $ toSBS ss
    hFlush h_write
    hClose h_write
    -- hSetBuffering h_read NoBuffering 
    -- BS.hGet h_read ln >>= print
  
  
    (Nothing, Just hout, Nothing, ph) <- createProcess (proc "alignmentStr" ["-p", "kk"]){ std_in = UseHandle h_read
                                                                       , std_out = CreatePipe
                                                                       }
    ec <- waitForProcess ph
    if ec == ExitSuccess
    then do
         -- hSetBuffering hout NoBuffering
         hSetBuffering hout NoBuffering
         hGetContents hout >>= \x -> length x `seq` putStr x
    else do
         print $ "ERROR:" ++ show ec
         exitFailure

    -- let s = "dog\ncat\nfox\n"
    -- let ls = ["dog", "what"]
    -- let ss = unlines ls
    -- writeFileRaw "/tmp/c" ss
    -- pp "ok"
     
    
    
    
    
  
  
