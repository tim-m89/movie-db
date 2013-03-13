{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, GADTs, DoAndIfThenElse, FlexibleContexts #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.Attoparsec (parse, maybeResult)
import qualified Data.HashMap.Strict (fromList, lookup)
import Data.Maybe
import Data.String (fromString)
import Data.Text as Text
import qualified Data.Text.IO as TextIO
import Network (withSocketsDo)
import Network.HTTP
import qualified Network.HTTP.HandleStream as S
import qualified System.IO.UTF8 as UTF8

import Database.Persist
import Database.Persist.Sqlite as Sqlite
import Database.Persist.TH

--fileNameTitle :: String -> String
--fileNameTitle = fst . break (=='.')

defAeson :: Aeson.Value
defAeson = Aeson.Object $ Data.HashMap.Strict.fromList [("Title", Aeson.String "Error"),
                                    ("Year", Aeson.String "Error"),
                                    ("Rated", Aeson.String "Error"),
                                    ("Plot", Aeson.String "Error"),
                                    ("Poster", Aeson.String "Error"),
                                    ("Runtime", Aeson.String "Error"),
                                    ("imdbRating", Aeson.String "Error"),
                                    ("imdbID", Aeson.String "Error")]

parseJson :: String -> Aeson.Value
parseJson x = fromMaybe defAeson (maybeResult $ parse Aeson.json $ fromString x)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Movie
  title Text
  year Text
  rated Text
  plot Text
  poster Text
  runtime Text
  rating Text
  imdb Text
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
                    mrating <- get "imdbRating"
                    mid <- get "imdbID"
                    get "Response" >>= \r-> if r=="True" then (Just r) else Nothing
                    Just $ Movie mtitle myear mrated mplot mposter mruntime mrating mid
                    where get :: Text -> Maybe Text
                          get s = case (Data.HashMap.Strict.lookup s o) of
                            Just (Aeson.String s) -> Just s
                            _ -> Nothing
                  _ -> Nothing

imdbGet :: String -> String -> IO (Maybe Movie)
imdbGet s requestType = withSocketsDo $ simpleHTTP (getRequest $ "http://www.imdbapi.com/?" ++ requestType ++ "=" ++ (urlEncode s)) >>= getResponseBody >>= \x-> return (parseJson x) >>= \x-> return (aesonToMovie x)

myMovies :: [String]
myMovies = [
  ""]

ratingClass :: Movie -> Text
ratingClass m = case (movieRated m) of
  "R" -> "ratingR"
  "G" -> "ratingG"
  "PG" -> "ratingPG"
  "PG-13" -> "ratingPG13"
  otherwise -> "ratingNA"

movieHtml :: Movie -> Text
movieHtml m = "<tr class=\"" `Text.append` (ratingClass m) `Text.append` "\"><td rowspan=\"2\"><img src=\"" `Text.append` (moviePoster m) `Text.append` "\" /></td><td>" `Text.append` "<p class=\"title\">" `Text.append`
              (htmlEncode $ movieTitle m) `Text.append` " (" `Text.append` (movieYear m) `Text.append` ") (" `Text.append` (movieRated m) `Text.append` ")</p>" `Text.append` "<a href=\"http://www.imdb.com/title/" `Text.append`
              (movieImdb m) `Text.append` "/\">" `Text.append` (htmlEncode $ movieTitle m) `Text.append` " (" `Text.append` (movieYear m) `Text.append` ") (" `Text.append` (movieRated m) `Text.append`
              ")</a></td></tr><tr class=\"" `Text.append` (ratingClass m) `Text.append` "\"><td><p>" `Text.append` (htmlEncode $ moviePlot m) `Text.append` "<br /><br />Runtime: " `Text.append`
              (movieRuntime m) `Text.append` "<br /><br />Rated: " `Text.append` (movieRating m) `Text.append` "/10</p></td></tr>\n"

imdbGetReport :: String -> String -> IO (Maybe Movie)
imdbGetReport requestType t = imdbGet t requestType >>= \x -> if (isNothing x)
  then do
    putStrLn ("Error adding to db: " ++ t)
    return x
  else do
    putStrLn $ "Done: " ++ t
    return x

htmlEncode :: Text -> Text
htmlEncode s = Text.concatMap (\c-> case c of
                                '\"' -> "&quot;"
                                '&' -> "&amp;"
                                '\'' -> "&apos;"
                                '<' -> "&lt;"
                                '>' -> "&gt;"
                                _ -> pack [c]) s


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
    putStrLn "Unknown command"
    loop

addOne :: Movie -> IO ()
addOne movie = runSqlite "movies.db" $ do
  Sqlite.insert movie
  commit
  return ()
  
main :: IO ()
main = runSqlite "movies.db" $ do
  runMigration migrateAll
  commit
  liftIO $ loop

importList :: IO ()
importList = runSqlite "movies.db" $ do
  mvs <- liftIO $ mapM (imdbGetReport "t") myMovies >>= return . catMaybes
  mapM (\m-> Sqlite.insert m) mvs
  commit
  return ()

genHtml :: IO ()
genHtml = runSqlite "movies.db" $ do
  liftIO $ putStr "Generating Movies.html .."
  movies <- selectList [] [Asc MovieTitle]
  liftIO $ TextIO.writeFile "./Movies.html" ("<html>\n<head>\n<style type=\"text/css\">\np {\n    width: 600px;\n    word-wrap: break-word;\n}\np.title {\n  display: none;\n}\nimg" `Text.append`
    " {\n    width: 320px;\n    height: 480px;\n}\ntable {page-break-inside:auto }\n" `Text.append`
    "tr    { page-break-inside:avoid; page-break-after:auto }\n</style>\n<style type=\"text/css\" media=\"print\">\np {\n  width: auto;\n  font-size: 32pt;\n}\nfieldset {\n  display: none;\n}\na {\n  display: none;\n}\np.title" `Text.append`
    " {\n  font-size: 32pt;\n  display: inline;\n}\n</style>\n" `Text.append`
    "<script type=\"text/javascript\" src=\"http://code.jquery.com/jquery-1.7.1.min.js\"></script>\n<script type=\"text/javascript\">\n  function getElementsByClassName(classname, node) {\n    if(!node) node = document.getElementsByTagName(\"body\")[0];\n" `Text.append`
    "    var a = [];\n    var re = new RegExp('\\b' + classname + '\\b');\n    var els = node.getElementsByTagName(\"*\");\n    for(var i=0,j=els.length; i<j; i++)\n      if(re.test(els[i].className))\n        " `Text.append`
    "a.push(els[i]);\n    return a;\n  }\n  function changeShow(ev, s) {\n    $(s).each(function(i,elem) {\n      if(ev.target.checked)\n        $(elem).show();\n      else\n        $(elem).hide();\n    });\n  }" `Text.append`
    "\n  $(document).ready(function() {\n    $(\"#checkG\").change(function (ev) { changeShow(ev, \".ratingG\") });\n    $(\"#checkPG\").change(function (ev) { changeShow(ev, \".ratingPG\") });" `Text.append`
    "\n    $(\"#checkPG13\").change(function (ev) { changeShow(ev, \".ratingPG13\") });\n    $(\"#checkR\").change(function (ev) { changeShow(ev, \".ratingR\") });\n    $(\"#checkNA\").change(function (ev) { changeShow(ev, \".ratingNA\") });" `Text.append`
    "\n  }); </script>\n</head>\n<body>\n<fieldset name=\"fields1\">\n<legend>Display movies with rating:</legend>\n" `Text.append`
    "<input id=\"checkG\" type=\"checkbox\" checked=\"true\" /><label for=\"checkG\">G</label>\n" `Text.append`
    "<input id=\"checkPG\" type=\"checkbox\" checked=\"true\" /><label for=\"checkPG\">PG</label>\n" `Text.append`
    "<input id=\"checkPG13\" type=\"checkbox\" checked=\"true\" /><label for=\"checkPG13\">PG-13</label>\n" `Text.append`
    "<input id=\"checkR\" type=\"checkbox\" checked=\"true\" /><label for=\"checkR\">R</label>\n" `Text.append`
    "<input id=\"checkNA\" type=\"checkbox\" checked=\"true\" /><label for=\"checkNA\">N/A</label>\n</fieldset>\n<table>\n" `Text.append`
    (Text.concat $ Prelude.map (movieHtml . entityVal) movies) `Text.append` "</table>\n</body>\n</html>")
  liftIO $ putStrLn ".. Done"


