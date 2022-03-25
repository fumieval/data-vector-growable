import Test.Tasty.Bench
import Data.Vector.Growable

main :: IO ()
main = defaultMain
  [ bench "push/10000" $ whnfIO $ do
      vec <- new :: IO (GrowableIOVector Int)
      mapM_ (push vec) [0 :: Int .. 9999]
  ]

{-
Running 1 benchmarks...
Benchmark bench: RUNNING...
All
  push/10000: OK (1.44s)
    85.6 μs ± 6.8 μs

All 1 tests passed (1.44s)
Benchmark bench: FINISH
-}