{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Lecture08.Time where

import Lecture08
import Control.Monad
import System.IO()
import System.Random
import System.CPUTime

-- Случайно генерируем список заданного размера, состоящий чисел от 0 до 20 включительно
genList :: Int -> Int -> [Int]
genList n len = take n $ randomRs (0,len) $ mkStdGen 100
  -- 100 - "волшебная константа"  :)
      
-------------------------------------------------------------------------------------------
-- Головная функция
computeTime :: Int -> Int -> [ (String,[Int] -> [Int]) ] -> IO ()
computeTime n len funcs = do
  let !lst = genList n len
    
  forM_ funcs (\(fName,func) -> do
    start <- getCPUTime      
    let !_ = func lst
    finish <- getCPUTime      
             
    putStrLn $ fName ++ ": " ++ (show $ fromIntegral (finish - start) / 1e12)
    )

example :: IO ()
example = computeTime 20 10 [("[Int]", countingSort @[Int])]