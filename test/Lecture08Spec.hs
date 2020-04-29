{-# LANGUAGE TypeApplications #-}

module Lecture08Spec where

import Test.Hspec

import Lecture08
import Data.Array

emptyStack :: Stack Int
emptyStack = createStack

toListByPop :: Stack a -> [a]
toListByPop (Stack xs) = xs

one :: Int
one = 1

spec :: Spec
spec = do
  describe "Stack" $ do
    it "createStack ~> emptyStack" $
     (createStack :: Stack Int) `shouldBe` emptyStack
    it "push emptyStack 1 ~> Stack [1]" $
      push emptyStack one `shouldBe` Stack ([1] :: [Int])
    it "push (Stack ['s', 'f']) 't' ~> Stack ['t', 's', 'f']" $
      push (Stack ['s', 'f']) 't' `shouldBe` Stack ['t', 's', 'f']
    it "pop emptyStack ~> Nothing" $
      pop (createStack :: Stack Int) `shouldBe` (Nothing :: Maybe (Stack Int))
    it "pop (Stack [3, 2, 1]) ~> Just (Stack [2, 1])" $
      pop (Stack ([3, 2, 1] :: [Int])) `shouldBe` Just (Stack [2, 1])
    it "peek emptyStack ~> Nothing" $
      peek emptyStack `shouldBe` (Nothing :: Maybe Int)
    it "peek (Stack ['g', 'a']) ~> Just 'g'" $
      peek (Stack ['g', 'a']) `shouldBe` Just 'g'
    it "toListByPop (push (push (push createStack 1) 2) 3) ~> [3, 2, 1]" $
      toListByPop (push (push (push createStack one) 2) 3) `shouldBe` [3, 2, 1]

  describe "Queue" $ do
    it "isEmpty emptyQueue ~> True" $
      isEmpty createQueue `shouldBe` True
    it "isEmpty (Queue [] [1, 2, 3]) ~> False" $
      isEmpty (Queue [] ([1, 2, 3] :: [Int])) `shouldBe` False
    it "isEmpty (Queue [1] []) ~> False" $
      isEmpty (Queue [1] ([] :: [Int])) `shouldBe` False
    it "enqueue (enqueue (enqueue emptyQueue 1) 2) 3 ~> Queue [3, 2, 1] []" $
      enqueue (enqueue (enqueue createQueue one) 2) 3 `shouldBe` Queue [3, 2, 1] []
    it "dequeue (enqueue (enqueue (enqueue emptyQueue 1) 2) 3) ~> (1, Queue [] [2, 3])" $
      dequeue (enqueue (enqueue (enqueue createQueue one) 2) 3) `shouldBe` (1, Queue [] [2, 3])
    it "dequeue $ snd $ dequeue (enqueue (enqueue (enqueue createQueue 1) 2) 3) ~> (2, Queue [] [3])" $
      (dequeue $ snd $ dequeue (enqueue (enqueue (enqueue createQueue one) 2) 3)) `shouldBe` (2, Queue [] [3])

  describe "countingSort @[Int]" $ do
    it "countingSort  @[Int] [] ~> []" $
      countingSort @[Int] [] `shouldBe` []
    it "countingSort @[Int] [0..10] ~> [0..10]" $
      countingSort @[Int] [0..10] `shouldBe` [0..10]
    it "countingSort @[Int] [10..1] ~> [1..10]" $
      countingSort @[Int] (reverse [1..10]) `shouldBe` [1..10]
    it "countingSort [6,5,6,0,1,6,7,2] ~> [0,1,2,5,6,6,6,7]" $
      countingSort @[Int] [6,5,6,0,1,6,7,2] `shouldBe` [0,1,2,5,6,6,6,7]

  describe "countingSort @(Array Int Int)" $ do
    it "countingSort @(Array Int Int) [] ~> []" $
      countingSort @(Array Int Int) [] `shouldBe` []
    it "countingSort @(Array Int Int) [0..10] ~> [0..10]" $
      countingSort @(Array Int Int) [0..10] `shouldBe` [0..10]
    it "countingSort @(Array Int Int) [10..1] ~> [1..10]" $
      countingSort @(Array Int Int) (reverse [1..10]) `shouldBe` [1..10]
    it "countingSort [6,5,6,0,1,6,7,2] ~> [0,1,2,5,6,6,6,7]" $
      countingSort @(Array Int Int) [6,5,6,0,1,6,7,2] `shouldBe` [0,1,2,5,6,6,6,7]