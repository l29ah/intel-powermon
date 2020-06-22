import Control.Concurrent
import qualified Streaming.Prelude as S
import System.IO

toJoules :: String -> Double
toJoules = (/ 1e6) . (read :: String -> Double)

main = do
	hSetBuffering stdout LineBuffering
	S.print $ S.drop 2 $ S.scan (\(prev, _) new -> (new, new - prev)) (0, 0) snd $ S.map toJoules $ S.repeatM (threadDelay 1000000 >> readFile "/sys/class/powercap/intel-rapl:0/energy_uj")
