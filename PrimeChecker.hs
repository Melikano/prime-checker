module PrimeChecker where

import Control.Concurrent
import Control.Exception (AsyncException (ThreadKilled))

findFactor :: Integer -> Integer -> Integer -> Integer -> Maybe Integer
findFactor value start end interval
  | start > end = Nothing
  | value `mod` left == 0 = Just left
  | value `mod` right == 0 = Just right
  | otherwise = findFactor value (start + interval) end interval
  where
    left = start - 1
    right = start + 1

solver :: MVar (Maybe Integer, Integer) -> Integer -> Integer -> Integer -> Integer -> IO ()
solver mvar value start end interval = do
  (_, i) <- readMVar mvar
  let maybeFactor = findFactor value start end interval
  takeMVar mvar
  putMVar mvar (maybeFactor, i + 1)

killThreads :: [ThreadId] -> Int -> IO ()
killThreads _ 0 = return ()
killThreads (t : ts) i = do
  throwTo t ThreadKilled
  print $ "killed thread " ++ show t
  killThreads ts (i - 1)

manageThreads :: Integer -> MVar (Maybe Integer, Integer) -> [ThreadId] -> IO ()
manageThreads threshold mvar threads = do
  mvarVal <- readMVar mvar
  case mvarVal of
    -- if non of the threads find a factor then the number is prime else keep reading...
    (Nothing, i) -> if i == threshold then print "Number is Prime" else manageThreads threshold mvar threads
    (Just factor, _) -> do
      -- if factor found, then kill other threads since we know that the number prime
      killThreads threads (length threads)
      print $ "Number is Not Prime! factor is " ++ show factor

precheck :: Integer -> Maybe String
precheck num
  | num == 1 = Just "Too small! Not prime."
  | even num = Just "Not Prime, it is devidable by 2"
  | num `mod` 3 == 0 = Just "Not Prime, it is devidable by 3"
  | otherwise = Nothing

main :: IO ()
main = do
  putStrLn "Enter an Intger"
  inpStr <- getLine
  let value = read inpStr :: Integer
  case precheck value of
    Just result -> print result
    Nothing -> do
      -- number of threads = log10 of the number
      let numberOfThreads = floor (logBase 10 (fromInteger value))
      print $ "number of threads: " ++ show numberOfThreads
      result <- newMVar (Nothing, 0)
      -- create threads and keep them because we need their id to be able to kill them if we found a factor
      threadIds <- mapM (\i -> forkIO (solver result value (6 + i * 6) (floor (sqrt (fromInteger value))) 6)) [0 .. numberOfThreads - 1]
      print threadIds
      manageThreads numberOfThreads result threadIds