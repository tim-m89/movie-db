{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, GADTs, DoAndIfThenElse, FlexibleContexts #-}

module Main where

import qualified Control.Monad.Parallel as P
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.Aeson as Aeson
import Data.Attoparsec (parse, maybeResult)
import Data.List
import qualified Data.HashMap.Strict (fromList, lookup)
import Data.Maybe
import Data.String
import Data.Text (pack, unpack)
import Network (withSocketsDo)
import Network.HTTP
import qualified Network.HTTP.HandleStream as S
import qualified System.IO.UTF8 as UTF8

import Data.Tuple 

import Database.Persist
import Database.Persist.Sqlite as Sqlite
import Database.Persist.TH

fileNameTitle :: String -> String
fileNameTitle = fst . break (=='.')

defAeson :: Aeson.Value
defAeson = Aeson.Object $ Data.HashMap.Strict.fromList [(pack "Title", Aeson.String $ pack "Error"),
                                    (pack "Year", Aeson.String $ pack "Error"),
                                    (pack "Rated", Aeson.String $ pack "Error"),
                                    (pack "Plot", Aeson.String $ pack "Error"),
                                    (pack "Poster", Aeson.String $ pack "Error"),
                                    (pack "Runtime", Aeson.String $ pack "Error"),
                                    (pack "Rating", Aeson.String $ pack "Error"),
                                    (pack "ID", Aeson.String $ pack "Error")]

parseJson :: String -> Aeson.Value
parseJson x = fromMaybe defAeson (maybeResult $ parse Aeson.json $ fromString x)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Movie
  title String
  year String
  rated String
  plot String
  poster String
  runtime String
  rating String
  imdb String
  MovieIMDB imdb
|]

aesonToMovie :: Aeson.Value -> Maybe Movie
aesonToMovie a = case a of
                  Aeson.Object o -> do
                    mtitle <- get "Title"
                    myear <- get "Year"
                    mrated <- get "Rated"
                    mplot <- get "Plot"
                    mposter <- get "Poster"
                    mruntime <- get "Runtime"
                    mrating <- get "Rating"
                    mid <- get "ID"
                    get "Response" >>= \r-> if r=="True" then (Just r) else Nothing
                    Just $ Movie mtitle myear mrated mplot mposter mruntime mrating mid
                    where get :: String -> Maybe String
                          get s = case (Data.HashMap.Strict.lookup (pack s) o) of
                            Just (Aeson.String s) -> Just (unpack s)
                            _ -> Nothing
                  _ -> Nothing

imdbGet :: String -> String -> IO (Maybe Movie)
imdbGet s requestType = withSocketsDo $ simpleHTTP (getRequest $ "http://www.imdbapi.com/?" ++ requestType ++ "=" ++ (urlEncode s)) >>= getResponseBody >>= \x-> return (parseJson x) >>= \x-> return (aesonToMovie x)

myMovies :: [String]
myMovies = [
  ""]

ratingClass :: Movie -> String
ratingClass m = case (movieRated m) of
  "R" -> "ratingR"
  "G" -> "ratingG"
  "PG" -> "ratingPG"
  "PG-13" -> "ratingPG13"
  otherwise -> "ratingNA"

movieHtml :: Movie -> String
movieHtml m = "<tr class=\"" ++ (ratingClass m) ++ "\"><td rowspan=\"2\"><img src=\"" ++ (moviePoster m) ++ "\" /></td><td><a href=\"http://www.imdb.com/title/" ++ (movieImdb m) ++ "/\">" ++
              (htmlEncode $ movieTitle m) ++ " (" ++ (movieYear m) ++ ") (" ++ (movieRated m) ++ ")</a></td></tr><tr class=\"" ++ (ratingClass m) ++ "\"><td><p>" ++
              (htmlEncode $ moviePlot m) ++ "<br /><br />Runtime: " ++ (movieRuntime m) ++ "<br /><br />Rated: " ++ (movieRating m) ++ "/10</p></td></tr>\n"

imdbGetReport :: String -> String -> IO (Maybe Movie)
imdbGetReport requestType t = imdbGet t requestType >>= \x -> if (isNothing x)
  then do
    putStrLn ("Error getting data for " ++ t)
    return x
  else do
    putStrLn $ "Done: " ++ t
    return x

htmlEncode :: String -> String
htmlEncode s = concatMap (\c-> case c of
                                '\"' -> "&quot;"
                                '&' -> "&amp;"
                                '\'' -> "&apos;"
                                '<' -> "&lt;"
                                '>' -> "&gt;"
                                _ -> [c]) s


loop :: IO ()
loop = do
  line <- getLine
  if line=="quit" then return ()
  else if line=="import" then do
    importList
    loop
  else if line=="html" then do
    genHtml
    loop
  else if line=="add" then do
    mid <- getLine
    mm <- imdbGetReport "i" mid
    maybe (return ()) addOne mm
    loop
  else do
    putStrLn "Unknown commad"
    loop

addOne :: Movie -> IO ()
addOne movie = withSqliteConn "movies.db" $ runSqlConn $ do
  Sqlite.insert movie
  commit
  return ()
  
main :: IO ()
main = withSqliteConn "movies.db" $ runSqlConn $ do
  runMigration migrateAll
  commit
  liftIO $ loop

importList :: IO ()
importList = withSqliteConn "movies.db" $ runSqlConn $ do
  mvs <- liftIO $ mapM (imdbGetReport "t") myMovies >>= return . catMaybes
  mapM (\m-> Sqlite.insert m) mvs
  commit
  return ()

genHtml :: IO ()
genHtml = withSqliteConn "movies.db" $ runSqlConn $ do
  liftIO $ putStr "Generating Movies.html .."
  movies <- selectList [] [Asc MovieTitle]
  liftIO $ UTF8.writeFile "./Movies.html" ("<html>\n<head>\n<style type=\"text/css\">\np {\n    width: 600px;\n    word-wrap: break-word;\n}\n\nimg {\n    width: 320px;\n    height: 480px;\n}\ntable {page-break-inside:auto }\n" ++
    "tr    { page-break-inside:avoid; page-break-after:auto } \n</style>\n" ++
    "<script type=\"text/javascript\" src=\"http://code.jquery.com/jquery-1.7.1.min.js\"></script>\n<script type=\"text/javascript\">\n  function getElementsByClassName(classname, node) {\n    if(!node) node = document.getElementsByTagName(\"body\")[0];\n" ++
    "    var a = [];\n    var re = new RegExp('\\b' + classname + '\\b');\n    var els = node.getElementsByTagName(\"*\");\n    for(var i=0,j=els.length; i<j; i++)\n      if(re.test(els[i].className))\n        " ++
    "a.push(els[i]);\n    return a;\n  }\n  function changeShow(ev, s) {\n    $(s).each(function(i,elem) {\n      if(ev.target.checked)\n        $(elem).show();\n      else\n        $(elem).hide();\n    });\n  }" ++
    "\n  $(document).ready(function() {\n    $(\"#checkG\").change(function (ev) { changeShow(ev, \".ratingG\") });\n    $(\"#checkPG\").change(function (ev) { changeShow(ev, \".ratingPG\") });" ++
    "\n    $(\"#checkPG13\").change(function (ev) { changeShow(ev, \".ratingPG13\") });\n    $(\"#checkR\").change(function (ev) { changeShow(ev, \".ratingR\") });\n    $(\"#checkNA\").change(function (ev) { changeShow(ev, \".ratingNA\") });" ++
    "\n  }); </script>\n</head>\n<body>\n<fieldset name=\"fields1\">\n<legend>Display movies with rating:</legend>\n" ++
    "<input id=\"checkG\" type=\"checkbox\" checked=\"true\" /><label for=\"checkG\">G</label>\n" ++
    "<input id=\"checkPG\" type=\"checkbox\" checked=\"true\" /><label for=\"checkPG\">PG</label>\n" ++
    "<input id=\"checkPG13\" type=\"checkbox\" checked=\"true\" /><label for=\"checkPG13\">PG-13</label>\n" ++
    "<input id=\"checkR\" type=\"checkbox\" checked=\"true\" /><label for=\"checkR\">R</label>\n" ++
    "<input id=\"checkNA\" type=\"checkbox\" checked=\"true\" /><label for=\"checkNA\">N/A</label>\n</fieldset>\n<table>\n" ++
    (concat $ map movieHtml (map snd movies)) ++ "</table>\n</body>\n</html>")
  liftIO $ putStrLn ".. Done"


