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
-- import Text.Read
import Text.Regex
import Text.Regex.Base
import Text.Regex.Base.RegexLike
import Text.Regex.Posix
import Data.IORef
import Control.Monad (unless, when)
import Control.Concurrent
import qualified System.Console.Pretty as SCP

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

  
-- DATE: Saturday, 04 December 2021 15:41 PST  
-- KEY: haskell state monad, state monad example
-- URL: https://hugopeters.me/posts/13/
  
sw = show

-- getNextNr::Int -> (Int, String)
-- getNextNr n = (n + 1, sw n)

getNextNr::State Int String
getNextNr = do
  current <- get
  put (current + 1)
  return (show current)

newtype State a b = State (a -> (a, b))

runState::State a b -> a -> (a, b)
runState (State f) a' = f a'


-- instance Functor Maybe where
  -- fmap f Nothing = Nothing
  -- fmap f (Just x) = Just (f x)
-- fmap (+1) Just 1 => Just 2

{-|
  State a   ⇒ Maybe
  State a b ⇒ Maybe a

  instance Functor (Maybe)     where
                      ↕
  instance Functor (State a) where

  (Maybe)  ↔   (State a)
     ↑            ↑
     +------------+
  (Mabye) b  ↔ (State a) b
          ↑              ↑
          +--------------+

  f   a -> (a, b)
      ↑     ↕
      |     a' -> (a', f b)
      + - - ↑
-}
instance Functor (State a) where
  fmap f state = State $ \a' -> let (n, res) = runState state a' in (n, f res)

{-|
   Just (+1) <*> Just 2 => Just 3
  (+) <$> Just 1 => Just (+1)
               |→ <*> Just 2 => Just 3

  (+) <$> Just 1 <*> Just 2  => Just 3


   class Functor => Applicative f where
      (<*>) :: f (a -> b) -> f a -> f b

   runState :: State a b -> a -> (a, b)
                       ↑             ↑
                       +--- (a -> b) +
                          f (a -> b)

   runState :: State a (a -> b) -> a -> (a, (a -> b))
                          ↑                    ↑
                          b                    b


   fs = f (a -> b)
   ft = f a

-}
instance Applicative (State a) where
    fs <*> ft = State $ \a' -> let (n1, f) = runState fs a'  -- f = (a -> b)
                                   (n2, a2) = runState ft n1

                               in (n2, f a2)
    pure x = State $ \a -> (a, x)


data ThreeStrings = ThreeStrings String String String

{-|
   State Int ThreeStrings
   runState :: State a b -> a -> (a, b)
                       ↑
                 ThreeStrings

   runState :: State Int ThreeStrings -> Int -> (Int ThreeStrings)
-}
tr :: State Int ThreeStrings
tr = ThreeStrings <$> getNextNr <*> getNextNr <*> getNextNr

{-|


   instance Monad Maybe where
        Nothing >>= f = Nothing
      Just x >= f = Just (f x)

   sfunc = a -> (a, b)

  

-}
instance Monad (State a) where
  (State sfunc) >>= f = State $ \state -> let (state', b1) = sfunc state in runState (f b1) state'
   --               ↑                                                                   ↑
   --              a -> m a                                                          State a b
   return = pure


get::State a a
get = State $ \state -> (state, state)

put::a -> State a ()
put x = State $ \_ -> (x, ())

execState::State a b -> a -> b
execState (State f) = snd . f

    
main :: IO()
main = let
      init::Int
      init = 0

      -- function :: [Int -> (Int, String)]
      function :: [State Int String]
      function = replicate 5 getNextNr

      -- chain :: [Int -> (Int, String)] -> Int -> (Int, [String])
      -- chain [] = error "No empty list plz"
      -- chain [f] = \i -> let (ni, v) = f i in (ni, [v])
      -- chain (f:fs) = \i -> let (ni, v) = f i
                               -- (nj, vs) = chain fs ni
                           -- in (nj, v:vs)

      chain::State Int [String]
      chain = sequence function
      
      
      -- (_, numbers) = chain function init
      -- in pre numbers
      numbers = execState chain init
      in print numbers
