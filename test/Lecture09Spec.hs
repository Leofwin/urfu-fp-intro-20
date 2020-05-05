{-# LANGUAGE DuplicateRecordFields #-}

module Lecture09Spec where

import Test.Hspec

import Lecture09

import Data.List
import System.Directory

shift :: String
shift = "\n    "

testName :: [String] -> String
testName names = intercalate shift names ++ shift 

folder :: String
folder = "Lecture09TestTodotdqwerqwer"

createDefaultTodoList :: IO TodoList
createDefaultTodoList = createTodoList folder

addDefaultTodo :: TodoList -> String -> IO Id
addDefaultTodo tdList dl = addTodo tdList (Title "buy milk") (Content "1 carton") dline
  where
    dline = Deadline (if dl == "" then "2020-04-30" else dl)

defaultTodo :: Todo
defaultTodo = Todo (Id "") (Title "buy milk") (Content "1 carton") (Deadline "2020-04-30") False

removeTodoFolder :: IO ()
removeTodoFolder = do
  doestExist <- doesDirectoryExist folder
  if doestExist then removeDirectoryRecursive folder else return ()

clearId :: Todo -> Todo
clearId td = td {todoId = Id ""}

clearIds :: [Todo] -> [Todo]
clearIds = map clearId

selectIds :: [Todo] -> [Id]
selectIds = map todoId

spec :: Spec
spec = after_ removeTodoFolder $ do
  describe "TodoList" $ do
    it "createTodoList folderPath ~> folderPath exists" $ do
      todoList <- createDefaultTodoList
      doesExist <- doesDirectoryExist folder
      doesExist && todoList == TodoList folder `shouldBe` True

    it (testName [
        "newTodoId <- addTodo todoList \"buy milk\" \"2020-04-30\" \"1 carton\"",
        "readTodo newTodoId"]) $ do
      todoList <- createDefaultTodoList
      newTodoId <- addDefaultTodo todoList ""
      actualTodo <- readTodo todoList newTodoId
      clearId actualTodo `shouldBe` defaultTodo

    it (testName [
        "newTodoId   <- addTodo todoList \"buy milk\" \"2020-03-02\" \"1 carton\"",
        "newTodoId'  <- addTodo todoList \"buy milk\" \"2020-04-30\" \"1 carton\"",
        "newTodoId'' <- addTodo todoList \"buy milk\" \"2020-04-29\" \"1 carton\"",
        "readAllTodo todoList  ~>  todos in order of [\"2020-03-02\", \"2020-04-29\", \"2020-04-30\"]"
        ]) $ do
      let
        expectedTodos = [
          defaultTodo {deadline = Deadline "2020-03-02"} :: Todo,
          defaultTodo {deadline = Deadline "2020-04-29"} :: Todo,
          defaultTodo]
      todoList <- createDefaultTodoList
      _ <- addDefaultTodo todoList "2020-03-02"
      _ <- addDefaultTodo todoList ""
      _ <- addDefaultTodo todoList "2020-04-29"
      allTodos <- readAllTodo todoList
      clearIds allTodos `shouldBe` expectedTodos

    it (testName [
        "firstId  <- addTodo todoList \"buy milk\" \"2020-02-20\" \"1 carton\"",
        "secondId <- addTodo todoList \"buy milk\" \"2020-04-30\" \"1 carton\"",
        "removeTodo todoList firstId",
        "readAllTodo todoList  ~>  [ Todo todoId = secondId ... False ]"
        ]) $ do
      todoList <- createDefaultTodoList
      firstId <- addDefaultTodo todoList "2020-02-20"
      secondId <- addDefaultTodo todoList ""
      removeTodo todoList firstId
      allTodos <- readAllTodo todoList
      selectIds allTodos `shouldBe` [secondId]

    it (testName [
        "firstId  <- addTodo todoList \"buy milk\" \"2020-02-20\" \"1 carton\"",
        "secondId <- addTodo todoList \"buy milk\" \"2020-04-30\" \"1 carton\"",
        "setTodoAsDone todoList secondId",
        "readUnfinishedTodo todoList  ~>  [ Todo todoId = firstId ... False ]"
        ]) $ do
      todoList <- createDefaultTodoList
      firstId <- addDefaultTodo todoList "2020-02-20"
      secondId <- addDefaultTodo todoList ""
      setTodoAsDone todoList secondId
      allTodos <- readUnfinishedTodo todoList
      selectIds allTodos `shouldBe` [firstId]

    it (testName [
        "tdId <- addTodo todoList \"buy milk\" \"2020-04-30\" \"1 carton\"",
        "setTodoAsDone todoList tdId",
        "readTodo todoList tdId  ~>  { ... True }"
        ]) $ do
      todoList <- createDefaultTodoList
      tdId <- addDefaultTodo todoList ""
      setTodoAsDone todoList tdId
      todo <- readTodo todoList tdId
      todo `shouldBe` (defaultTodo {todoId = tdId, isDone = True} :: Todo)

    it (testName [
       "tdId <- addTodo todoList \"buy milk\" \"2020-04-30\" \"1 carton\"",
       "editTodo todoList tdId TodoEdit (Title \"buy water\") ...",
       "readTodo todoList tdId  ~>  { ... \"buy water\" ... }"
       ]) $ do
     todoList <- createDefaultTodoList
     tdId <- addDefaultTodo todoList ""
     editTodo todoList tdId (TodoEdit (Title "buy water") (Content "1 carton") (Deadline "2020-04-30"))
     todo <- readTodo todoList tdId
     todo `shouldBe` (defaultTodo {todoId = tdId, title = Title "buy water"} :: Todo)

    it "showTodo todoList ~> doesn't throw" $ do
      todoList <- createDefaultTodoList
      tdId <- addDefaultTodo todoList ""
      showTodo todoList tdId
      todoList `shouldBe` TodoList folder

    it "showAllTodo todoList ~> doesn't throw" $ do
      todoList <- createDefaultTodoList
      showAllTodo todoList
      todoList `shouldBe` TodoList folder

    it "showUnfinishedTodo todoList ~> doesn't throw" $ do
      todoList <- createDefaultTodoList
      showUnfinishedTodo todoList
      todoList `shouldBe` TodoList folder
