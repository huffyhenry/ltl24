module Literal where

import Prelude hiding (min)
import F24

data Literal = PlayerId
             | TeamId
             | TypeId
             | X
             | Y
             | Time
             | PeriodId
             | QValue Int
             | Constant String

instance Show Literal where
    show PlayerId = "player_id"
    show TeamId = "team_id"
    show TypeId = "type_id"
    show X = "x"
    show Y = "y"
    show PeriodId = "period_id"
    show Time = "time"
    show (QValue i) = "qval(" ++ (show i) ++ ")"
    show (Constant s) = "\"" ++ s ++ "\""


liteval :: Literal -> Event -> Maybe String
liteval PlayerId e = maybe Nothing (Just . show) (player_id e)
liteval TeamId e = (Just . show . team_id) e
liteval TypeId e = (Just . show . type_id) e
liteval X e = maybe Nothing (Just . show) (x e)
liteval Y e = maybe Nothing (Just . show) (y e)
liteval Time e = let mins = fromIntegral (min e) :: Double
                     secs = fromIntegral (sec e) :: Double
                 in Just (show $ mins + secs/60.0)
liteval PeriodId e = (Just . show . period_id) e
liteval (Constant c) _ = Just c
liteval (QValue i) e = qval i e
