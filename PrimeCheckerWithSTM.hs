import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (AsyncException (ThreadKilled))
import Control.Monad

findFactor :: Integer -> Integer -> Integer -> Integer -> Maybe Integer
findFactor value start end interval
  | start > end = Nothing
  | value `mod` left == 0 = Just left
  | value `mod` right == 0 = Just right
  | otherwise = findFactor value (start + interval) end interval
  where
    left = start - 1
    right = start + 1

solver :: TVar (Maybe Integer, Integer) -> Integer -> Integer -> Integer -> Integer -> STM ()
solver tvar value start end interval = do
  (_, i) <- readTVar tvar
  let maybeFactor = findFactor value start end interval
  writeTVar tvar (maybeFactor, i + 1)

killThreads :: [ThreadId] -> Int -> STM ()
killThreads _ 0 = return ()
killThreads (t : ts) i = do
  let io = throwTo t ThreadKilled
  killThreads ts (i - 1)

manageThreads :: Integer -> TVar (Maybe Integer, Integer) -> [ThreadId] -> STM Integer
manageThreads threshold tvar threads = do
  tvarVal <- readTVar tvar
  case tvarVal of
    -- if non of the threads find a factor then the number is prime else keep reading...
    (Nothing, i) -> if i == threshold then return 0 else retry
    (Just factor, _) -> do
      -- if factor found, then kill other threads since we know that the number prime
      killThreads threads (length threads)
      return factor

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
      result <- newTVarIO (Nothing, 0)
      -- create threads and keep them because we need their id to be able to kill them if we found a factor
      threadIds <- mapM (\i -> forkIO $ atomically (solver result value (6 + i * 6) (floor (sqrt (fromInteger value))) 6)) [0 .. numberOfThreads - 1]
      print threadIds
      factor <- atomically $ manageThreads numberOfThreads result threadIds
      putStrLn $ if factor == 0 then "Number is prime." else "Number is not prime, factor: " ++ show factor