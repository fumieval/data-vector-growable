{-# LANGUAGE LambdaCase #-}
import Data.Vector.Growable
import qualified Data.Vector as BV
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent

main :: IO ()
main = do
  vec <- new :: IO (GrowableIOVector Int)
  let step i = atomicPush vec i
  let popper count = atomicPop vec >>= \case
        Nothing -> yield >> popper count
        Just (-1) -> print count
        Just _ -> popper $! count + 1
  mapConcurrently_ id
    $ popper (0 :: Int)
    : [ mapM_ step [0..i * 200] | i <- [0..7] ]
    ++ [ threadDelay 1000000 >> atomicPush vec (-1)]