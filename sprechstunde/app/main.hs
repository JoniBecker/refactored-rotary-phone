import Prelude
import Application (appMain)

main :: IO ()
main = appMain

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday 
    deriving Show

data Rhythm = Every | Odd | Even
    deriving Show

data Time = Time Int Int
instance Show Time where
    show (Time h m) = (show h) ++ ":" ++ (show m)

data Date = Date Weekday Time Time Rhythm
    deriving Show

standardDate :: Weekday -> Time -> Date
standardDate day time = Date day time (addMinutes time 60) Every

normalisedTime :: Time -> Time
normalisedTime (Time h m) = Time (h + quot m 60) (m `mod` 60)

addMinutes :: Time -> Int -> Time
addMinutes (Time h m) minutes = normalisedTime $Time h $m+minutes


