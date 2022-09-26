module WeatherWaybar where

import Data.Aeson (FromJSON, ToJSON (toJSON), defaultOptions, eitherDecode, encode, genericParseJSON, genericToJSON, parseJSON, withObject, (.:))
import Data.Aeson.Types (Parser, Value)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import RIO
import qualified RIO.ByteString.Lazy as BL
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

newtype Region = Region {unRegion :: String}
  deriving (Eq, Show, Generic, IsString)

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
buildWaybarJson
  WeatherResp
    { currentCondition =
        [CurrentCondition {feelsLikeC, humidity, weatherCode, cloudCover}],
      nearestArea =
        [NearestArea {region = [Region {unRegion = region}]}]
    } = do
    let weatherEmoji = maybe "" unEmoji $ getEmojiByWeatherCode constWeatherCodes $ WeatherCode weatherCode
    WaybarJson
      { tooltip =
          mconcat
            [ "Feels like ",
              feelsLikeC,
              "°C\n",
              humidity,
              "% humidity\n",
              cloudCover,
              "% cloud cover\n",
              "Weather code: ",
              weatherCode,
              " ",
              weatherEmoji,
              "\nRegion: ",
              region
            ],
        text =
          mconcat
            [ weatherEmoji,
              feelsLikeC,
              "°C "
            ]
      }
buildWaybarJson err = error $ "Unexpected response from wttr.in" ++ show err

getEmojiByWeatherCode :: [(WeatherCode, Emoji)] -> WeatherCode -> Maybe Emoji
getEmojiByWeatherCode weatherCodes weatherCode =
  lookup weatherCode weatherCodes

newtype Emoji = Emoji {unEmoji :: String}
  deriving (Eq, Show, Generic, IsString)

newtype WeatherCode = WeatherCode {unWeatherCode :: String}
  deriving (Eq, Show, Generic, IsString)

constWeatherCodes :: [(WeatherCode, Emoji)]
constWeatherCodes =
  [ ("113", "☀️"),
    ("116", "⛅️"),
    ("119", "☁️"),
    ("122", "☁️"),
    ("143", "🌫"),
    ("176", "🌦"),
    ("179", "🌧"),
    ("182", "🌧"),
    ("185", "🌧"),
    ("200", "⛈"),
    ("227", "🌨"),
    ("230", "❄️"),
    ("248", "🌫"),
    ("260", "🌫"),
    ("263", "🌦"),
    ("266", "🌦"),
    ("281", "🌧"),
    ("284", "🌧"),
    ("293", "🌦"),
    ("296", "🌦"),
    ("299", "🌧"),
    ("302", "🌧"),
    ("305", "🌧"),
    ("308", "🌧"),
    ("311", "🌧"),
    ("314", "🌧"),
    ("317", "🌧"),
    ("320", "🌨"),
    ("323", "🌨"),
    ("326", "🌨"),
    ("329", "❄️"),
    ("332", "❄️"),
    ("335", "❄️"),
    ("338", "❄️"),
    ("350", "🌧"),
    ("353", "🌦"),
    ("356", "🌧"),
    ("359", "🌧"),
    ("362", "🌧"),
    ("365", "🌧"),
    ("368", "🌨"),
    ("371", "❄️"),
    ("374", "🌧"),
    ("377", "🌧"),
    ("386", "⛈"),
    ("389", "🌩"),
    ("392", "⛈"),
    ("395", "❄️")
  ]
