{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE QuasiQuotes       #-} -- support raw string [r|<p>dog</p> |]
import Text.RawString.QQ       -- Need QuasiQuotes too 
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
import CMark
import qualified Data.Text.Lazy            as TL  -- Lazy Text
import qualified Data.Text                 as TS  -- Strict Text
import qualified Data.Text.IO              as TSO
import qualified Text.Regex.TDFA as TD
import AronModule 

p1 = "/Users/cat/myfile/bitbucket/testfile/test.tex"

-- zo - open
-- za - close



shiftR::(Num a) => Int -> [a] -> [a]
shiftR n cx = (take n $ repeat 0) ++ cx

shiftL::(Num a) => Int -> [a] -> [a]
shiftL n cx = (take n $ repeat 0) ++ cx


-- shiftRight::(Num a)=> [[a]] -> [[a]]
-- shiftRight cx = map (\x ->  let n = (len ls) - 1 in nzero (n - x) ++ [1, 2, 3] ++ nzero x) ls

{-|
   21 34
    (4,5) (2, 1) (3, 4)
                 (3, 4 + c) c = 0
                 (3 + (4 + c)/10, 4 + c % 10)
                (a, b)
         (c2 = 2 + c1, a1 = 1 + a, c1 = a1 % 10)
    (4 + c3, a2 = 5 + c2, c3 = a2 % 10)
    

    (4, 5) (2, 1) (3, 4) c = 0
                  (3 + (4 + c / 10), 4 + c % 10)
                  c = 3 + (4+c)/10
           (2 + (1 + c)/10, 1 + c % 10)
           c = 2 + (1+c)/10
    (4 + (5 + c)/10, 5 + c % 10)
    c = 4 + (5+4)/10
-}
carrying::Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
carrying c [] = [(div c 10, mod c  10)]
carrying c  ((a, b):cx) = let n = c + b
                              -- c1 = div n 10
                              (q1, r1) = divMod n 10
                              (q, r) = divMod (a + q1) 10
                          in (r, r1) : carrying q cx


spiralMatrix::[[a]] -> [a]
spiralMatrix cx = []



m9 = [[1, 2], [3, 4]]
rev = reverse
main = do
        let (+) = (++)
        argList <- getArgs
        let ls = [0, 1, 2]
        pre $ shiftR 3 [1]
        pp $ (>>>) 3 0 [2]
        pp $ (>>>) (-3) 0 [2]
        prel $ map (\x ->  let n = (len ls) - 1 in nzero (n - x) + [1, 2, 3] + nzero x) ls
        let lss = zipWith(\x b -> let n = len ls - 1 in (nzero $ n - x) + b + (nzero x)) [0, 1, 2] [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        fw "zipWith"
        prel lss
        mat <- geneRandMat (4, 4)
        fw "mat"
        prel mat
        fw "shiftLeftPad"
        let shiftMat = shiftMatrixLeft 4 mat
        prel shiftMat
        let mat' = tran shiftMat
        let ss = map (\x -> [sum x]) mat'
        let tm = tran ss
        fw "tm"
        pre tm
        fw "carrying"
        pre $ carrying 0 [(1, 1)]
        pre $ carrying 0 [(2, 4)]
        pre $ carrying 0 $ reverse [(23, 5), (12, 4)]
        fw "p"
        let p = out (\x y -> x * y) (rev [3, 9]) [4, 3]
        prel p
        let sp = shiftMatrixLeft 2 p
        fw "sp"
        prel sp
        pre $ tran sp
        let sumList = tran $ map (\x -> [sum x]) $ tran sp
        let sls = head sumList
        pre sls
        let mls = map (\x -> divMod x 10) sls
        pre mls
        let mulArr = carrying 0 $ reverse mls
        fw "mulArr"
        pre mulArr
        fw $ "39 x 43 = " ++ (show $ 39*43)
        -- let sumColumn = tran $ map sum $ tran sp
        -- fw "sumColumn"
        -- pre sumColumn
        fw "pad"
        pre $ pad 3 0 [1, 2, 3]
        pre $ pad (-3) 0 [1, 2, 3]
        let m = [[1, 2, 3]
                ,[3, 4, 5]
                ,[6, 7, 8]]
        let shiftMat = shiftMatrixLeft 3 m
        prel shiftMat
        fw "spiralMatrix"
        prel m
        pre $ head m
        let m1 = tran $ drop 1 m
        pre $ head m1
        let m2 = tran $ drop 1 m1
        pre $ head m2
        pp "ok"
