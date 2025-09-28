module Main where

import System.IO (hFlush, stdout)
import VehicleRegister
  ( Model, Year, RegNo
  , Status(..)
  , Vehicle(..)
  , showVehicle
  , trim
  , lower
  , parseStatus
  , findVehicle
  )

prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine

promptMaybe :: String -> IO (Maybe String)
promptMaybe msg = do
  s <- prompt msg
  let t = trim s
  pure $ if null t then Nothing else Just t

readYear :: IO Year
readYear = do
  s <- prompt "Enter year (e.g., 2019): "
  case reads (trim s) :: [(Int, String)] of
    [(n,"")] -> pure n
    _        -> putStrLn "Invalid year. Try again." >> readYear

readStatus :: IO Status
readStatus = do
  s <- prompt "Enter status (Registered/Unregistered): "
  case parseStatus s of
    Just st -> pure st
    Nothing -> putStrLn "Invalid status. Try again." >> readStatus

-- Optional readers (blank keeps existing)
readYearMaybe :: IO (Maybe Year)
readYearMaybe = do
  s <- prompt "Enter year (leave blank to keep): "
  let t = trim s
  if null t
    then pure Nothing
    else case reads t :: [(Int,String)] of
           [(n,"")] -> pure (Just n)
           _        -> putStrLn "Invalid year. Try again." >> readYearMaybe

readStatusMaybe :: IO (Maybe Status)
readStatusMaybe = do
  s <- prompt "Enter status (Registered/Unregistered, blank to keep): "
  let t = trim s
  if null t
    then pure Nothing
    else case parseStatus t of
           Just st -> pure (Just st)
           Nothing -> putStrLn "Invalid status. Try again." >> readStatusMaybe

-- List all vehicles
listVehicles :: [Vehicle] -> IO ()
listVehicles [] = putStrLn "(No vehicles in register.)"
listVehicles vs = mapM_ (putStrLn . ("  " ++) . showVehicle) (reverse vs)

-- Add a vehicle (with duplicate RegNo check)
addVehicle :: [Vehicle] -> IO [Vehicle]
addVehicle reg = do
  putStrLn "\n--- Add Vehicle ---"
  model <- prompt "Enter model: "
  year  <- readYear
  regNo <- prompt "Enter registration number (unique): "
  case findVehicle regNo reg of
    Just _  -> putStrLn "A vehicle with that RegNo already exists. Cancelled." >> pure reg
    Nothing -> do
      status <- readStatus
      let v = Vehicle { vModel = model, vYear = year, vRegNo = regNo, vStatus = status }
      putStrLn "Vehicle added:"
      putStrLn ("  " ++ showVehicle v)
      pure (v : reg)

-- Search flow
searchFlow :: [Vehicle] -> IO ()
searchFlow reg = do
  putStrLn "\n--- Search Vehicle ---"
  q <- prompt "Enter registration number: "
  case findVehicle q reg of
    Just v  -> putStrLn ("Found:\n  " ++ showVehicle v)
    Nothing -> putStrLn "No vehicle found."

-- Update vehicle
updateVehicle :: [Vehicle] -> IO [Vehicle]
updateVehicle reg = do
  putStrLn "\n--- Update Vehicle ---"
  key <- prompt "Enter existing registration number to update: "
  let (left, rest) = break (\v -> vRegNo v == key) reg
  case rest of
    [] -> putStrLn "No vehicle with that RegNo." >> pure reg
    (oldV:right) -> do
      putStrLn $ "Current: " ++ showVehicle oldV
      mModel <- promptMaybe "Enter model (blank to keep): "
      mYear  <- readYearMaybe
      mRegNo <- promptMaybe "Enter new registration number (blank to keep): "
      mStat  <- readStatusMaybe

      -- Compute new values with keep-on-blank
      let newModel = maybe (vModel oldV) id mModel
          newYear  = maybe (vYear  oldV) id mYear
          newRegNo = maybe (vRegNo oldV) id mRegNo
          newStat  = maybe (vStatus oldV) id mStat

      -- If RegNo changes, ensure unique against all others
      let others = left ++ right
      if newRegNo /= vRegNo oldV && any (\v -> vRegNo v == newRegNo) others
        then putStrLn "Another vehicle already uses that RegNo. Update cancelled." >> pure reg
        else do
          let newV   = Vehicle newModel newYear newRegNo newStat
              newReg = left ++ (newV : right)
          putStrLn "Updated:"
          putStrLn ("  " ++ showVehicle newV)
          pure newReg

-- Delete vehicle
deleteVehicle :: [Vehicle] -> IO [Vehicle]
deleteVehicle reg = do
  putStrLn "\n--- Delete Vehicle ---"
  key <- prompt "Enter registration number to delete: "
  let (left, rest) = break (\v -> vRegNo v == key) reg
  case rest of
    [] -> putStrLn "No vehicle with that RegNo." >> pure reg
    (v:right) -> do
      putStrLn $ "About to delete:\n  " ++ showVehicle v
      c <- prompt "Confirm delete? (y/n): "
      if lower (trim c) `elem` ["y","yes"]
        then putStrLn "Deleted." >> pure (left ++ right)
        else putStrLn "Cancelled." >> pure reg

-- Menu loop
menu :: [Vehicle] -> IO ()
menu reg = do
  putStrLn "\n====== Vehicle Register ======"
  putStrLn "1) List vehicles"
  putStrLn "2) Search by RegNo"
  putStrLn "3) Add vehicle"
  putStrLn "4) Update vehicle"
  putStrLn "5) Delete vehicle"
  putStrLn "6) Quit"
  choice <- prompt "Choose an option (1-6): "
  case choice of
    "1" -> listVehicles reg      >> menu reg
    "2" -> searchFlow reg        >> menu reg
    "3" -> addVehicle reg        >>= menu
    "4" -> updateVehicle reg     >>= menu
    "5" -> deleteVehicle reg     >>= menu
    "6" -> putStrLn "Goodbye!"
    _   -> putStrLn "Invalid choice." >> menu reg

-- Seed data
seed :: [Vehicle]
seed =
  [ Vehicle "Toyota Corolla"   2015 "ABC123GP" Registered
  , Vehicle "Ford Ranger"      2020 "XYZ789MP" Unregistered
  , Vehicle "Volkswagen Polo"  2018 "VWP456FS" Registered
  , Vehicle "BMW X5"           2021 "BMW999GP" Registered
  , Vehicle "Mercedes C200"    2017 "MER777WC" Unregistered
  ]

main :: IO ()
main = do
  putStrLn "Loaded seed register with 5 vehicles."
  menu seed
