import Data.Vector.Growable
import qualified Data.Vector as BV
import Control.Concurrent.Async (mapConcurrently_)

main :: IO ()
main = do
  vec <- new :: IO (GrowableIOVector Int)
  let step i = atomicPush vec i
  mapConcurrently_ (\i -> mapM_ step [0..i * 200]) [0 :: Int ..7]
  final <- freeze vec 
  print $ BV.length final -- 5608