module VehicleRegister
  ( Model, Year, RegNo
  , Status(..)
  , Vehicle(..)
  , showVehicle
  , trim
  , lower
  , parseStatus
  , findVehicle
  , wins -- exported for completeness (if needed later)
  ) where

import Data.Char (toLower)

type Model = String
type Year  = Int
type RegNo = String

data Status = Registered | Unregistered deriving (Show, Eq)

data Vehicle = Vehicle
  { vModel  :: Model
  , vYear   :: Year
  , vRegNo  :: RegNo
  , vStatus :: Status
  } deriving (Show, Eq)

-- Pretty print
showVehicle :: Vehicle -> String
showVehicle (Vehicle m y r s) =
  "Vehicle { model = " ++ m ++ ", year = " ++ show y
  ++ ", regNo = " ++ r ++ ", status = " ++ show s ++ " }"

-- Helpers
trim :: String -> String
trim = f . f where f = reverse . dropWhile (== ' ')

lower :: String -> String
lower = map toLower

parseStatus :: String -> Maybe Status
parseStatus s =
  case lower (trim s) of
    "registered"   -> Just Registered
    "unregistered" -> Just Unregistered
    _              -> Nothing

-- Search by RegNo (exact match)
findVehicle :: RegNo -> [Vehicle] -> Maybe Vehicle
findVehicle regNo = go
  where
    go (v:vs) | vRegNo v == regNo = Just v
              | otherwise         = go vs
    go [] = Nothing

-- Potential future use: winning patterns placeholder (kept to show export pattern evolution)
wins :: [[Int]]
wins = []
