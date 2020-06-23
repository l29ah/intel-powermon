import Control.Concurrent
import Foreign.C.Types
import qualified Streaming.Prelude as S
import System.IO
import System.Posix.Clock
import System.Posix.Signals
import System.Posix.Timer

toJoules :: String -> Double
toJoules = (/ 1e6) . (read :: String -> Double)

every :: CTime -> IO () -> IO ()
every seconds action = do
	timer <- createTimer monotonicClock $ Just (realTimeAlarm, 0)
	installHandler realTimeAlarm (Catch action) Nothing
	configureTimer timer False (mkTimeSpec seconds 0) (mkTimeSpec seconds 0)
	pure ()

main = do
	hSetBuffering stdout LineBuffering
	ujstrs <- newChan
	every 1 $ readFile "/sys/class/powercap/intel-rapl:0/energy_uj" >>= writeChan ujstrs
	S.print $ S.drop 2 $ S.scan (\(prev, _) new -> (new, new - prev)) (0, 0) snd $ S.map toJoules $ S.repeatM $ readChan ujstrs
