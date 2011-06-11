{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson as Aeson
import Data.Attoparsec (parse, maybeResult)
--import qualified Data.ByteString as BS
--import qualified Data.ByteString.Lazy as LBS
import Data.List
import qualified Data.Map (fromList, lookup)
import Data.Maybe
import Data.String
import Data.Text (pack, unpack)
import Network (withSocketsDo)
import Network.HTTP

fileNameTitle :: String -> String
fileNameTitle = fst . break (=='.')

defAeson :: Aeson.Value
defAeson = Aeson.Object $ Data.Map.fromList [(pack "Title", Aeson.String $ pack "Error"),
                                    (pack "Year", Aeson.String $ pack "Error"),
                                    (pack "Rated", Aeson.String $ pack "Error"),
                                    (pack "Plot", Aeson.String $ pack "Error"),
                                    (pack "Runtime", Aeson.String $ pack "Error"),
                                    (pack "Rating", Aeson.String $ pack "Error"),
                                    (pack "ID", Aeson.String $ pack "Error")]

parseJson :: String -> Aeson.Value
parseJson x = fromMaybe defAeson (maybeResult $ parse Aeson.json $ fromString x)

data Movie = Movie { movieTitle :: String,
                     movieYear :: String,
                     movieRated :: String,
                     moviePlot :: String,
                     movieRuntime :: String,
                     movieRating :: String,
                     movieID :: String }

aesonToMovie :: Aeson.Value -> Movie
aesonToMovie a = case a of
                  Aeson.Object o -> fromJust $ do
                    mtitle <- get "Title"
                    myear <- get "Year"
                    mrated <- get "Rated"
                    mplot <- get "Plot"
                    mruntime <- get "Runtime"
                    mrating <- get "Rating"
                    mid <- get "ID"
                    Just Movie { movieTitle = mtitle,
                            movieYear = myear,
                            movieRated = mrated,
                            moviePlot = mplot,
                            movieRuntime = mruntime,
                            movieRating = mrating,
                            movieID = mid }
                    where get :: String -> Maybe String
                          get s = case (Data.Map.lookup (pack s) o) of
                            Just (Aeson.String s) -> Just (unpack s)
                            _ -> Nothing
                  _ -> undefined
main :: IO ()
main = withSocketsDo $ simpleHTTP (getRequest $ "http://www.imdbapi.com/?i=&t=" ++ (urlEncode $ fileNameTitle "Gone in sixty seconds.avi")) >>= getResponseBody >>= \x-> return (parseJson x) >>= \x -> putStrLn $ moviePlot $ aesonToMovie x


