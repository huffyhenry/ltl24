module F24File where

import Prelude hiding (min)
import qualified Data.ByteString as BS
import Text.XML.Light.Input
import Text.XML.Light.Types
import Control.Exception

import F24
import Errors

loadGameFromFile :: String -> IO Game
loadGameFromFile filepath = do
    xml <- BS.readFile filepath
    let result = parseXMLDoc xml
    case result of
        Just root -> return (makeGame (head (getChildren (\q -> True) root)))
        Nothing -> throw ParserFailure

attrLookup :: Element -> (String -> a) -> String -> Maybe a
attrLookup el cast key = let keys = map (qName . attrKey) (elAttribs el)
                             vals = map attrVal (elAttribs el)
                             val = lookup key (zip keys vals)
                         in maybe Nothing (Just . cast) val

attrLookupStrict :: Element -> (String -> a) -> String -> a
attrLookupStrict el cast key = let val = (attrLookup el cast key)
                               in maybe (throw MissingData) id val

getChildren :: (Element -> Bool) -> Element -> [Element]
getChildren cond el = let getElems :: [Content] -> [Element]
                          getElems [] = []
                          getElems ((Elem e):rest) = e:(getElems rest)
                          getElems ((Text _):rest) = getElems rest
                          getElems ((CRef _):rest) = getElems rest
                      in filter cond (getElems (elContent el))

makeQ :: Element -> Q
makeQ el = Q { qid = attrLookupStrict el read "id",
               qualifier_id = attrLookupStrict el read "qualifier_id",
               value = attrLookup el read "value" }

makeEvent :: Element -> Event
makeEvent el = Event { eid = attrLookupStrict el read "id",
                       event_id = attrLookupStrict el read "event_id",
                       type_id = attrLookupStrict el read "type_id",
                       period_id = attrLookupStrict el read "period_id",
                       min = attrLookupStrict el read "min",
                       sec = attrLookupStrict el read "sec",
                       player_id = attrLookup el read "player_id",
                       team_id = attrLookupStrict el read "team_id",
                       outcome = attrLookup el read "outcome",
                       x = attrLookup el read "x",
                       y = attrLookup el read "y",
                       timestamp = attrLookupStrict el read "timestamp",
                       last_modified = attrLookupStrict el read "last_modified",
                       qs = let condQ = (\e -> qName (elName e) == "Q")
                            in map makeQ (getChildren condQ el) }

makeGame :: Element -> Game
makeGame el = Game { gid = attrLookupStrict el read "id",
                     away_team_id = attrLookupStrict el read "away_team_id",
                     away_team_name = attrLookupStrict el id "away_team_name",
                     competition_id = attrLookupStrict el read "competition_id",
                     competition_name = attrLookupStrict el id "competition_name",
                     game_date = attrLookupStrict el read "game_date",
                     home_team_id = attrLookupStrict el read "home_team_id",
                     home_team_name = attrLookupStrict el id "home_team_name",
                     matchday = attrLookupStrict el read "matchday",
                     period_1_start = attrLookupStrict el read "period_1_start",
                     period_2_start = attrLookupStrict el read "period_2_start",
                     season_id = attrLookupStrict el read "season_id",
                     season_name = attrLookupStrict el id "season_name",
                     events = let condE = (\e -> qName (elName e) == "Event")
                              in map makeEvent (getChildren condE el) }
