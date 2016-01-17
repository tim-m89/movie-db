{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, GADTs, DoAndIfThenElse, FlexibleContexts, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Attoparsec (parse, maybeResult)
import qualified Data.ByteString.Lazy.Char8 as LCB
import qualified Data.HashMap.Strict (fromList, lookup)
import Data.Maybe
import Data.String (fromString)
import Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as TextIO
import Network (withSocketsDo)
import Network.HTTP
import System.IO

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Text.Hamlet
import Text.Lucius
import Text.Julius
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
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


instance FromJSON Movie where
  parseJSON (Object v) =
    Movie <$> v .: "Title"
          <*> v .: "Year"
          <*> v .: "Rated"
          <*> v .: "Plot"
          <*> v .: "Poster"
          <*> v .: "Runtime"
          <*> v .: "imdbRating"
          <*> v .: "imdbID"
  parseJSON _ = mzero

instance ToJSON Movie where
  toJSON (Movie mtitle myear mrated mplot mposter mruntime mrating mid) =
    object [ "Title"      .= mtitle
           , "Year"       .= myear
           , "Rated"      .= mrated
           , "Plot"       .= mplot
           , "Poster"     .= mposter
           , "Runtime"    .= mruntime
           , "imdbRating" .= mrating
           , "imdbID"     .= mid
           ]
          

imdbGet :: String -> String -> IO (Maybe Movie)
imdbGet s requestType = withSocketsDo $ simpleHTTP (getRequest $ "http://www.imdbapi.com/?" ++ requestType ++ "=" ++ (urlEncode s)) >>= getResponseBody >>= \x-> return $ decode $  LCB.pack x

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

movieHtml :: Movie -> Html
movieHtml m = [shamlet|
<tr class="#{ratingClass m}">
  <td rowspan="2">
    <img src="#{moviePoster m}">
  <td>
    <p class="title">#{htmlEncode $ movieTitle m} (#{movieYear m}) (#{movieRated m})>
    <a href="http://www.imdb.com/title/#{movieImdb m}">
      #{htmlEncode $ movieTitle m} (#{movieYear m}) (#{movieRated m})
<tr class="#{ratingClass m}">
  <td>
    <p>
      #{htmlEncode $ moviePlot m}
      <br>
      <br>
      Runtime: #{movieRuntime m}
      <br>
      <br>
      Rated: #{movieRating m}/10
|]

movieText :: Movie -> Text
movieText m = (movieTitle m) `Text.append` " (" `Text.append` (movieYear m) `Text.append` ")"

imdbGetReport :: String -> String -> IO (Maybe Movie)
imdbGetReport requestType t = imdbGet t requestType >>= \x -> if (isNothing x)
  then do
    putStrLn ("Error adding to db: " ++ t)
    return x
  else return x

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
  else if line=="text" then do
    genText
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
  m <- insertUnique movie
  case m of
    Just k -> do
      Just mv <- get k
      liftIO $ TextIO.putStrLn ("Done: \"" `Text.append` (movieTitle mv) `Text.append` "\" (" `Text.append` (movieYear mv) `Text.append` ")")
    Nothing -> liftIO $ putStrLn "Movie already exists"
  
main :: IO ()
main = runSqlite "movies.db" (runMigration migrateAll) >> loop

importList :: IO ()
importList = runSqlite "movies.db" $ do
  mvs <- liftIO $ mapM (imdbGetReport "t") myMovies >>= return . catMaybes
  mapM (\m-> insertUnique m) mvs
  return ()



luc1 = renderCss $ [lucius|
p {
    width: 600px;
    word-wrap: break-word;
}
p.title {
  display: none;
}
img {
    width: 320px;
    height: 480px;
}
table {
  page-break-inside: auto;
}
tr {
  page-break-inside: avoid;
  page-break-after: auto;
}
|] undefined



luc2 = renderCss $ [lucius|
p {
  width: auto;
  font-size: 32pt;
}
fieldset {
  display: none;
}
a {
  display: none;
}
p.title {
  font-size: 32pt;
  display: inline;
}
|] undefined


jul1 = renderJavascript $ [julius|
function getElementsByClassName(classname, node) {
    if(!node)
      node = document.getElementsByTagName("body")[0];
    var a = [];
    var re = new RegExp('\\b' + classname + '\\b');
    var els = node.getElementsByTagName("*");
    for(var i=0,j=els.length; i<j; i++)
      if(re.test(els[i].className))        
        a.push(els[i]);
    return a;
  }
  function changeShow(ev, s) {
    $(s).each(function(i,elem) {
      if(ev.target.checked)
        $(elem).show();
      else
        $(elem).hide();
    });
  }
  $(document).ready(function() {
    $("#checkG").change(function (ev) { changeShow(ev, ".ratingG") });
    $("#checkPG").change(function (ev) { changeShow(ev, ".ratingPG") });
    $("#checkPG13").change(function (ev) { changeShow(ev, ".ratingPG13") });
    $("#checkR").change(function (ev) { changeShow(ev, ".ratingR") });
    $("#checkNA").change(function (ev) { changeShow(ev, ".ratingNA") });
  });
|] undefined

genText :: IO ()
genText = runSqlite "movies.db" $ do
  liftIO $ putStr "Generating Movies.txt .." >> hFlush stdout
  movies <- selectList [] [Asc MovieTitle]
  let movies' = Text.unlines $ Prelude.map (movieText . entityVal) movies
  liftIO $ TextIO.writeFile "./Movies.txt" movies' 
  liftIO $ putStrLn ".. Done"

genHtml :: IO ()
genHtml = runSqlite "movies.db" $ do
  liftIO $ putStr "Generating Movies.html .." >> hFlush stdout
  movies <- selectList [] [Asc MovieTitle]
  let movies' = Prelude.map (movieHtml . entityVal) movies
  liftIO $ TextIO.writeFile "./Movies.html" $ renderHtml [shamlet|
<html>
  <head>
    <style type="text/css">#{preEscapedToHtml luc1}
    <style type="text/css" media="print">#{preEscapedToHtml luc2}
    <script type="text/javascript" src="http://code.jquery.com/jquery-1.7.1.min.js">
    <script type="text/javascript">#{preEscapedToHtml jul1}
  <body>
    <fieldset name="fields1">
      <legend>Display movies with rating:
      <input id="checkG" type="checkbox" checked="true">
      <label for="checkG">G
      <input id="checkPG" type="checkbox" checked="true">
      <label for="checkPG">PG
      <input id="checkPG13" type="checkbox" checked="true">
      <label for="checkPG13">PG-13
      <input id="checkR" type="checkbox" checked="true">
      <label for="checkR">R
      <input id="checkNA" type="checkbox" checked="true">
      <label for="checkNA">N/A
    <table>
      $forall movie <- movies'
        ^{movie}
|]
  liftIO $ putStrLn ".. Done"


