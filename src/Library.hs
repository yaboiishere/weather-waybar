module Library where

import Data.Aeson (encode)
import RIO
import qualified RIO.ByteString.Lazy as BL
import System.IO (print)
import WeatherWaybar (buildWaybarJson, getWeather)

runMain :: IO ()
runMain = do
  weather <- getWeather
  either print (buildWaybarJson >>> encode >>> BL.putStr) weather
