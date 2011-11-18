{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Monad.Parallel as P
import qualified Data.Aeson as Aeson
import Data.Attoparsec (parse, maybeResult)
import Data.List
import qualified Data.Map (fromList, lookup)
import Data.Maybe
import Data.String
import Data.Text (pack, unpack)
import Network (withSocketsDo)
import Network.HTTP
import qualified System.IO.UTF8 as UTF8

fileNameTitle :: String -> String
fileNameTitle = fst . break (=='.')

defAeson :: Aeson.Value
defAeson = Aeson.Object $ Data.Map.fromList [(pack "Title", Aeson.String $ pack "Error"),
                                    (pack "Year", Aeson.String $ pack "Error"),
                                    (pack "Rated", Aeson.String $ pack "Error"),
                                    (pack "Plot", Aeson.String $ pack "Error"),
                                    (pack "Poster", Aeson.String $ pack "Error"),
                                    (pack "Runtime", Aeson.String $ pack "Error"),
                                    (pack "Rating", Aeson.String $ pack "Error"),
                                    (pack "ID", Aeson.String $ pack "Error")]

parseJson :: String -> Aeson.Value
parseJson x = fromMaybe defAeson (maybeResult $ parse Aeson.json $ fromString x)

data Movie = Movie { movieTitle :: String,
                     movieYear :: String,
                     movieRated :: String,
                     moviePlot :: String,
                     moviePoster :: String,
                     movieRuntime :: String,
                     movieRating :: String,
                     movieID :: String }

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
                    Just Movie { movieTitle = mtitle,
                            movieYear = myear,
                            movieRated = mrated,
                            moviePlot = mplot,
                            moviePoster = mposter,
                            movieRuntime = mruntime,
                            movieRating = mrating,
                            movieID = mid }
                    where get :: String -> Maybe String
                          get s = case (Data.Map.lookup (pack s) o) of
                            Just (Aeson.String s) -> Just (unpack s)
                            _ -> Nothing
                  _ -> Nothing

imdbTitle :: String -> IO (Maybe Movie)
imdbTitle s = withSocketsDo $ simpleHTTP (getRequest $ "http://www.imdbapi.com/?i=&t=" ++ (urlEncode s)) >>= getResponseBody >>= \x-> return (parseJson x) >>= \x-> return (aesonToMovie x)

myMovies :: [String]
myMovies = [
  "127 Hours",
  "13 (2010)",
  "300",
  "5 Days of War",
  "8 Mile",
  "A Beautiful Mind",
  "Accidents Happen",
  "Across the Universe",
  "Age of Heroes",
  "Alice in Wonderland",
  "Alvin and the Chipmunks: The Squeekquel",
  "American Gangster",
  "An Inconvenient Truth",
  "Australia",
  "Avatar",
  "Babel",
  "Batman Begins",
  "Battle Los Angeles",
  "Battle of the Bulge",
  "Bedtime Stories",
  "Blitz",
  "Blood Out",
  "Body Of Lies",
  "Boy",
  "Cats & Dogs (2001)",
  "Cats and dogs 2",
  "Changeling",
  "Charlie's Angels: Full Throttle",
  "Children Of Men",
  "Click",
  "Cloudy With A Chance Of Meatballs",
  "Cloverfield",
  "Collateral",
  "Cop Out",
  "Coraline",
  "Crank",
  "Crank 2",
  "Crossroads (1986)",
  "Daybreakers",
  "Death Race",
  "Death Race 2",
  "Defiance",
  "Deja Vu",
  "Despicable Me",
  "Dinner For Schmucks",
  "Dirty Dancing 1987 20th Anniversary Edition",
  "District 9",
  "Enemy At The Gates",
  "Fast and Furious",
  "Fast and Furious 5",
  "Fiddler On The Roof",
  "Fight Club",
  "Four Brothers",
  "Funny People",
  "G-Force",
  "G.I. Joe The Rise Of Cobra",
  "Gangs of New York",
  "Get Him to the Greek",
  "Ghost Rider (2007)",
  "Gladiator",
  "Gnomeo and Juliet",
  "Gone in 60 Seconds",
  "Gran Torino",
  "Green Lantern",
  "Green Zone",
  "Hall Pass (2011)",
  "Harry Potter and the Philosopher's Stone",
  "Harry Potter and the Chamber of Secrets",
  "Harry Potter and the Prisoner of Azkaban",
  "Harry Potter and the Goblet of Fire",
  "Harry Potter and the Order of the Phoenix",
  "Harry Potter and the Half-Blood Prince",
  "Hobo With A Shotgun",
  "Home Alone",
  "Home Alone 2",
  "Home Alone 3",
  "Home Alone 4",
  "Hop",
  "Horrible Bosses",
  "Hotel for dogs",
  "How to train your dragon",
  "I Am Legend",
  "I Know You Know (2008)",
  "I think i love my wife",
  "I,Robot",
  "Ice Age 3 Dawn Of The Dinosaurs",
  "Immortal (2004)",
  "Inception",
  "Inside Job",
  "Inside Man",
  "Iron Man 2",
  "It's Complicated",
  "Jackass",
  "Jackass 2",
  "Jackass 2.5",
  "Jackass 3",
  "Jackass 3.5",
  "Julie and Julia",
  "Jumper",
  "Just Go With It",
  "Kick-Ass",
  "Killer Elite",
  "Knight And Day",
  "Law Abiding Citizen",
  "Leon [The Professional]",
  "Lock stock and two smoking barrels",
  "Machete",
  "Madagascar - Escape 2 Africa",
  "Man On Fire",
  "Marley and Me",
  "Marmaduke",
  "Mean Girls",
  "Mean Girls 2",
  "Megamind",
  "Michael Jackson This Is It",
  "My Sisters's Keeper",
  "Never back down",
  "Night At The Museum",
  "Night At The Museum 2 - Battle Of The Smithsonian",
  "Nine to Five",
  "No Strings Attached",
  "Not Easily Broken",
  "One Missed Call",
  "Paul",
  "PS I Love You",
  "Percy Jackson & the Olympians_ The Lightning Thief",
  "Pirates of the Caribbean: On Stranger Tides",
  "Planet 51",
  "Public Enemies",
  "Rango",
  "Red State",
  "Resident Evil Extinction",
  "Resident Evil Afterlife",
  "Rio",
  "Rock n Rolla",
  "Salt",
  "Scream 4",
  "Setup (2011)",
  "Seven Pounds",
  "Sex and the City (2008)",
  "Sex and the City 2",
  "Shaun of the Dead",
  "Sherlock Holmes",
  "Shooter",
  "Slumdog Millionaire",
  "Snatch",
  "Source Code",
  "Star Wars Episode I The Phantom Menace",
  "Star Wars Episode II Attack Of The Clones",
  "Star Wars Episode III Revenge Of The Sith",
  "Star Wars Episode IV A New Hope",
  "Star Wars Episode V The Empire Strikes Back",
  "Star Wars Episode VI Return Of The Jedi",
  "Step Up",
  "Step Up 2: The Streets",
  "Step Up 3",
  "Stone (2010)",
  "Street Kings",
  "Sucker Punch",
  "Surrogates",
  "TRON Legacy",
  "Taking Of Pelham 123",
  "Tekken",
  "Tenacious D in The Pick of Destiny",
  "Terminator Salvation",
  "The A-Team",
  "The Adjustment Bureau",
  "The Bank Job",
  "The Book of Eli",
  "The Bourne Identity",
  "The Bourne Supremacy",
  "The Bourne Ultimatum",
  "The Bucket List",
  "The Curious Case of Benjamin Button",
  "The Da Vinci Code",
  "The Dark Knight (2008)",
  "The Departed",
  "The Expendables",
  "The Fighter",
  "The Godfather",
  "The Godfather: Part II",
  "The Godfather: Part III",
  "The Green Hornet",
  "The Hangover",
  "The Hangover 2",
  "The Italian Job",
  "The Karate Kid (1984)",
  "The Karate Kid, Part II",
  "The Karate Kid, Part III",
  "The Next Karate Kid",
  "The Karate Kid (2010)",
  "The King's Speech",
  "The Lovely Bones",
  "The Mechanic",
  "The Next Three Days",
  "The Notebook",
  "The Pursuit Of Happyness",
  "The Simpsons Movie",
  "The Social Network",
  "The Tourist",
  "The Town",
  "The Wolfman",
  "Thor",
  "Training Day",
  "Transformers 3 Dark of the moon",
  "Transporter 3",
  "Unknown (2011)",
  "Unstoppable",
  "Unthinkable",
  "Untraceable",
  "Up In The Air",
  "Up",
  "Valkyrie",
  "Wall Street Money Never Sleeps",
  "Wall-E",
  "Wanted",
  "Watchmen"]

movieHtml :: Movie -> String
movieHtml m = "<tr><td rowspan=\"2\"><img src=\"" ++ (moviePoster m) ++ "\" /></td><td><a href=\"http://www.imdb.com/title/" ++ (movieID m) ++ "/\">" ++
              (htmlEncode $ movieTitle m) ++ " (" ++ (movieYear m) ++ ") (" ++ (movieRated m) ++ ")</a></td></tr><tr><td><p>" ++
              (htmlEncode $ moviePlot m) ++ "<br /><br />Runtime: " ++ (movieRuntime m) ++ "<br /><br />Rated: " ++ (movieRating m) ++ "/10</p></td></tr>\n"

titleHtml :: String -> IO String
titleHtml t = imdbTitle t >>= \x-> case x of
                                      Just m -> putStrLn t >> return (movieHtml m)
                                      _ -> putStrLn ("Error getting data for " ++ t) >> return ("<tr><td>" ++ t ++ "</td></tr>\n")

htmlEncode :: String -> String
htmlEncode s = concatMap (\c-> case c of
                                '\"' -> "&quot;"
                                '&' -> "&amp;"
                                '\'' -> "&apos;"
                                '<' -> "&lt;"
                                '>' -> "&gt;"
                                _ -> [c]) s


main :: IO ()
main = P.mapM titleHtml myMovies >>= \s-> UTF8.writeFile "./Movies.html" ("<html><head><link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\" /></head><table>" ++  (concat s) ++ "</table></html>")


