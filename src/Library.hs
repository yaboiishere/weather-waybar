module Library where

import Data.Aeson (FromJSON, ToJSON (toJSON), defaultOptions, eitherDecode, encode, genericParseJSON, genericToJSON, parseJSON, withObject, (.:))
import Data.Aeson.Types (Parser, Value)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import RIO
import qualified RIO.ByteString.Lazy as BL
import RIO.List.Partial (head)
import System.IO (print)

runMain :: IO ()
runMain = do
  weather <- getWeather
  either print (buildWaybarJson >>> encode >>> BL.putStr) weather

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

instance ToJSON CurrentCondition where
  toJSON = genericToJSON defaultOptions

newtype Region = Region String
  deriving (Eq, Show, Generic)

parseRegion :: Value -> Parser Region
parseRegion = withObject "Region" $ \obj -> do
  region <- obj .: "value"
  pure (Region region)

instance FromJSON Region where
  parseJSON = parseRegion

instance ToJSON Region where
  toJSON = genericToJSON defaultOptions

newtype NearestArea = NearestArea
  { region :: [Region]
  }
  deriving (Eq, Show, Generic)

instance FromJSON NearestArea where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON NearestArea where
  toJSON = genericToJSON defaultOptions

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

instance ToJSON WeatherResp where
  toJSON = genericToJSON defaultOptions

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

instance FromJSON WaybarJson where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON WaybarJson where
  toJSON = genericToJSON defaultOptions

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
