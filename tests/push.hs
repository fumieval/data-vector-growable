{-# LANGUAGE LambdaCase #-}
import Data.Vector.Growable
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent (yield, threadDelay)
import Control.Monad (unless)

main :: IO ()
main = do
  vec <- new :: IO (GrowableIOVector Int)
  let step i = atomicPush vec i
  let popper count = atomicPop vec >>= \case
        Nothing -> yield >> popper count
        Just (-1) -> do
            putStrLn $ "Count: " <> show count
            unless (count == 5608) $ fail "this is not expected"
        Just _ -> popper $! count + 1
  mapConcurrently_ id
    $ popper (0 :: Int)
    : [ mapM_ step [0..i * 200] | i <- [0..7] ]
    ++ [ threadDelay 1000000 >> atomicPush vec (-1)]
