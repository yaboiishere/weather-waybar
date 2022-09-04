module Library where

import Data.Aeson (FromJSON, defaultOptions, eitherDecode, genericParseJSON, parseJSON, withObject, (.:))
import Data.Aeson.Types (Parser, Value)
import Data.List (head)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import RIO
import Prelude (print)

runMain :: IO ()
runMain = do
  weather <- getWeather
  either print (buildWaybarJson >>> print) weather

data CurrentCondition = CurrentCondition
  { feelsLikeC :: String,
    humidity :: String,
    cloudCover :: String,
    weatherCode :: String
  }
  deriving (Eq, Show, Generic)

parseCurrentCondition :: Value -> Parser CurrentCondition
parseCurrentCondition = withObject "CurrentCondition" $ \obj -> do
  feelsLikeC <- obj .: "FeelsLikeC"
  humidity <- obj .: "humidity"
  cloudCover <- obj .: "cloudcover"
  weatherCode <- obj .: "weatherCode"
  pure (CurrentCondition feelsLikeC humidity cloudCover weatherCode)

instance FromJSON CurrentCondition where
  parseJSON = parseCurrentCondition

newtype Region = Region String
  deriving (Eq, Show, Generic)

parseRegion :: Value -> Parser Region
parseRegion = withObject "Region" $ \obj -> do
  region <- obj .: "value"
  pure (Region region)

instance FromJSON Region where
  parseJSON = parseRegion

newtype NearestArea = NearestArea
  { region :: [Region]
  }
  deriving (Eq, Show, Generic)

instance FromJSON NearestArea where
  parseJSON = genericParseJSON defaultOptions

data WeatherResp = WeatherResp
  { currentCondition :: [CurrentCondition],
    nearestArea :: [NearestArea]
  }
  deriving (Eq, Show, Generic)

parseWeatherResp :: Value -> Parser WeatherResp
parseWeatherResp = withObject "WeatherResp" $ \obj -> do
  currentCondition <- obj .: "current_condition"
  nearestArea <- obj .: "nearest_area"
  pure (WeatherResp currentCondition nearestArea)

instance FromJSON WeatherResp where
  parseJSON = parseWeatherResp

getWeather :: IO (Either String WeatherResp)
getWeather = do
  let url = "https://wttr.in/?format=j1"
  resp <- httpLbs (parseRequest_ url) =<< newManager tlsManagerSettings
  return $ eitherDecode $ responseBody resp

data WaybarJson = WaybarJson
  { text :: String,
    tooltip :: String
  }
  deriving (Eq, Show, Generic)

buildWaybarJson :: WeatherResp -> WaybarJson
buildWaybarJson weather =
  WaybarJson
    { tooltip =
        mconcat
          [ "Feels like ",
            feelsLikeC $ head $ currentCondition weather,
            "°C, ",
            humidity $ head $ currentCondition weather,
            "% humidity, ",
            cloudCover $ head $ currentCondition weather,
            "% cloud cover",
            "Weather code: ",
            weatherCode $ head $ currentCondition weather,
            ", region: ",
            show $ region $ head $ nearestArea weather
          ],
      text = feelsLikeC $ head $ currentCondition weather
    }
