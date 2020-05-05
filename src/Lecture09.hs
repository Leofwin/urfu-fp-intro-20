{-# LANGUAGE DuplicateRecordFields, DeriveGeneric #-}

module Lecture09 where

import System.FilePath
import System.Random
import Data.List
import System.Directory
import Data.UUID

{-
  09: Монады IO и Random

  - overview and motivation
  - Pure computations a -> b
    - Lazy: very hard to do i/o => pure
    - effects and side-effects
      - java checked exceptions
        - throwable Exception
  - not a toy => I/O (before monads, haskell 1.0)
    - streams
    - continuations
  - Effects
    - a -> IO b -- i/o
    - a -> (b -> r) -> r -- continuations
    - a -> b + Ex -- exceptions
    - a -> [b]
    - a -> Maybe b
    - T = PR _, IO _, (_ -> r) -> r, _ + Ex, [_], Maybe _
      - T-effectfull computation a -> b is a pure computation a -> T b
      - why we want (show examples, continuations figure 4, arguments passing)
        - 1. want to embed pure data into effectful world
          a -> T a
        - 2. composition of effectful computation
          a -> T b
          b -> T c
          ---------
          a -> T c

          bind :: T b -> (b -> T c) -> T c
    - class T m where
        (>>=)  :: m a -> (  a -> m b) -> m b
        return ::   a                 -> m a
  - Monad
    - set of types: Int, Float, Maybe a, IO a, ...
    - laws
      Left identity: 	return a >>= f ≡ f a
      Right identity: m >>= return ≡ m
      Associativity: 	(m >>= f) >>= g	≡ m >>= (\x -> f x >>= g)
  - Higher-kinded polymorphism + type classes + monads
  - monadic I/O (haskell 1.3 1996)
    - standard functions
  - random
    - standard functions
  - algebraic effects
  - Functor => Applicative => Monad
    - semantics (behaviour)

  Подробнее:
  - Chapter 7
    https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf
  - https://www.seas.upenn.edu/~cis194/fall16/lectures/06-io-and-monads.html
-}

-- <Задачи для самостоятельного решения>

{-
  TODO list

  Напишите программу для работы со списком задач.
  Хранить задачи нужно в виде файлов в определённой папке.
  Вы можете сами выбрать формат имени и содержимого файлов.

  Вам понадобятся
  - System.Directory
    https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html
  - Работа с файлами в IO
    http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html
-}

newtype TodoList = TodoList FilePath deriving (Eq, Show)

newtype Id = Id String deriving (Eq, Show)

newtype Title = Title String deriving (Eq, Show)

newtype Deadline = Deadline String deriving (Eq, Show)

newtype Content = Content String deriving (Eq, Show)

-- Тип для чтения Todo
data Todo = Todo
  { todoId :: Id
  , title :: Title
  , content :: Content
  , deadline :: Deadline
  , isDone :: Bool
  } deriving (Eq, Show)

instance Ord Deadline where
  compare (Deadline first) (Deadline second) = compare first second

instance Ord Todo where
  compare Todo{deadline=first} Todo{deadline=second} = compare first second

-- Тип для редактирования Todo
data TodoEdit = TodoEdit
  { title :: Title
  , content :: Content
  , deadline :: Deadline
  } deriving (Eq, Show)

generateRandomId :: IO Id
generateRandomId = do
  g <- newStdGen
  let (uuid, _) = random g
  return $ Id $ toString uuid

getFilename :: Id -> FilePath
getFilename (Id id) = id ++ ".todo"

toString' :: Todo -> String
toString' (Todo (Id id) (Title title) (Content content) (Deadline deadline) isDone) = 
   intercalate "\n" [id, title, content, deadline, if isDone then "true" else "false"]

fromString' :: String -> Todo
fromString' text = Todo (Id id) (Title title) (Content content) (Deadline deadline) isDone
  where
    isDone = case isDoneStr of
      "true" -> True
      "false" -> False
      otherwise -> error "failed to deserialize isDone parameter"
    [id, title, content, deadline, isDoneStr] = lines text

createTodoList :: FilePath -> IO TodoList
createTodoList rootFolder = do
  createDirectoryIfMissing True rootFolder
  return $ TodoList rootFolder

writeTodo :: TodoList -> Todo -> IO Id
writeTodo (TodoList rootFolder) todo@(Todo {todoId=id}) = do
  writeFile (rootFolder </> getFilename id) $ toString' todo
  return $ id

addTodo :: TodoList -> Title -> Content -> Deadline -> IO Id
addTodo todoList title text deadline = do
  id <- generateRandomId
  todo <- return $ Todo id title text deadline False
  writeTodo todoList todo 

readTodoFromFile :: FilePath -> IO Todo
readTodoFromFile filepath = do
  fileContent <- readFile filepath
  return $ fromString' fileContent

readTodo :: TodoList -> Id -> IO Todo
readTodo (TodoList rootFolder) id = readTodoFromFile $ rootFolder </> (getFilename id)

showTodo :: TodoList -> Id -> IO ()
showTodo todoList id = do
  (Todo (Id id) (Title title) (Content content) (Deadline deadline) isDone) <- readTodo todoList id
  putStrLn $ "Id: " ++ id
  putStrLn $ "Title: " ++ title
  putStrLn $ "Content: " ++ content
  putStrLn $ "Deadline: " ++ deadline
  putStrLn $ "Done" ++ if isDone then "true" else "false"

removeTodo :: TodoList -> Id -> IO ()
removeTodo (TodoList rootFolder) id = do
  filepath <- return $ rootFolder </> getFilename id
  fileExists <- doesFileExist filepath
  case fileExists of
    True -> removeFile filepath
    False -> return ()

editTodo :: TodoList -> Id -> TodoEdit -> IO ()
editTodo todoList id (TodoEdit updatedTitle updatedContent updatedDeadline) = do
  Todo {todoId=id, isDone=isDone} <- readTodo todoList id
  updated <- return $ Todo id updatedTitle updatedContent updatedDeadline isDone
  _ <- writeTodo todoList updated
  return ()

setTodoAsDone :: TodoList -> Id -> IO ()
setTodoAsDone todoList id = do
  Todo id title content deadline _ <- readTodo todoList id
  updated <- return $ Todo id title content deadline True
  _ <- writeTodo todoList updated
  return ()

-- Todo должны быть упорядочены по возрастанию deadline'а
readAllTodo :: TodoList -> IO [Todo]
readAllTodo todoList@(TodoList rootFolder) = do
  filenames <- listDirectory rootFolder
  filepaths <- return $ map (\filename -> rootFolder </> filename) filenames
  todos <- mapM readTodoFromFile filepaths
  return $ sort todos

readUnfinishedTodo :: TodoList -> IO [Todo]
readUnfinishedTodo todoList = do
  todos <- readAllTodo todoList
  return $ filter (\(Todo{isDone=isDone}) -> not isDone) todos

showTodos :: TodoList -> [Todo] -> IO ()
showTodos todoList todos = do
  _ <- return $ map (\(Todo{todoId=id}) -> showTodo todoList id) todos
  return ()

showAllTodo :: TodoList -> IO ()
showAllTodo todoList = do
  todos <- readAllTodo todoList
  showTodos todoList todos

showUnfinishedTodo :: TodoList -> IO ()
showUnfinishedTodo todoList = do
  todos <- readUnfinishedTodo todoList
  showTodos todoList todos


{-
  Напишите игру для угадывания случайного числа.

  При старте, необходимо случайным образом выбрать число в
  отрезке [0..100] и предоставить пользователю возможность его угадать.

  Пример игровой сессии:

  > playGuessGame
  Your number: 50
  > Too big
  Your number: 25
  > Too small
  Your number: 37  
  > Yep, that's the number!
-}

guessNumber :: Integer -> IO ()
guessNumber expected = do
  putStr "Your number: "
  actual <- readLn :: IO Integer 
  case compare actual expected of
    LT -> do 
      putStrLn "Too small!"
      guessNumber expected
    GT -> do
      putStrLn "Too big!"
      guessNumber expected
    EQ -> putStrLn "Yep, that's the number!"

playGuessGame :: IO ()
playGuessGame = do
  expected <- randomRIO (0, 100) :: IO Integer
  guessNumber expected



-- </Задачи для самостоятельного решения>