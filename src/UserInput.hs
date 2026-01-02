module UserInput (ensureInput, chooseAmongst) where

import System.IO
import Text.Read
import Control.Monad

ensureInput :: IO (Either String b) -> IO b
ensureInput action = do
    badVal <- action
    case badVal of
      Left err -> do
          print err
          print "Try again:"
          ensureInput action
      Right v -> return v

(!?) :: [a] -> Int -> Either String a
(!?) [] _ = Left "Error: Index out of bound"
(h:t) !? 0 = Right h
(h:t) !? n = t !? (n - 1)

chooseAmongst :: Show a => [a] -> IO a
chooseAmongst options = ensureInput $ do
    let verboses = [" " ++ show i ++ ") " ++ show act | (i, act) <- zip [1..] options]
    mapM_ print verboses
    putStr "Your choice? "
    hFlush stdout
    (readEither >=> (options !?)) <$> getLine

