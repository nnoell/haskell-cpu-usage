---------------------------------------------------------------------------------------------
--- Author      : Julian Bouzas <nnoell3[at]gmail.com>                                     --
--- License     : BSD3-style (see LICENSE)                                                 --
--- Stability   : Stable                                                                   --
--- Desc        : Simple haskell CPU usage monitor                                         --
--- Build       : $ ghc haskell-cpu-usage.hs -o haskell-cpu-usage.out                      --
--- Usage       : haskell-cpu-usage.out <num-cores> <regresh-rate> <output-file>           --
---               Example: ./haskell-cpu-usage.out 5 1 "/tmp/haskell-cpu-usage.txt"        --
---------------------------------------------------------------------------------------------

module Main where

-- Useful imports
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)


-- | Default number of cores
defNumCores :: Int
defNumCores = 5

-- | Refresh rate (in seconds)
defRefreshRate :: Int
defRefreshRate = 1

-- | Where to write cpu percent
defOutputFile :: String
defOutputFile = "/tmp/haskell-cpu-usage.txt"


--------------------------------------------------------------------------------------------

data Settings = Settings { settingsNumCores :: Int
                         , settingsRefreshRate :: Int
                         , settingsOutputFile :: String
                         }

parseSettings :: [String] -> Settings
parseSettings [] = Settings defNumCores defRefreshRate defOutputFile
parseSettings [x] = Settings (read x::Int) defRefreshRate defOutputFile
parseSettings [x,y] = Settings (read x::Int) (read y::Int) defOutputFile
parseSettings [x,y,z] = Settings (read x::Int) (read y::Int) z

-- | Get /proc/stat values of a specific cpu
cpuInfo :: Int -- ^ number of cores
        -> IO [[Integer]]
cpuInfo ncpus = do
  c <- readFile "/proc/stat"
  let getCoresInfo = map (dropWhile (/= ' ')) $ take ncpus $ lines c
  return $ map (map read . words) getCoresInfo


-- | Get cpuPerc with its respective idle and total amount of time: [idle,total,cpuPerc]
getPerc :: [Integer] -- ^ specific cpu values of /proc/stat
        -> Integer   -- ^ previous idle value
        -> Integer   -- ^ previous total value
        -> [Integer]
getPerc cpu pidle ptotal = [idle, total, diffUsage]
  where
    idle = cpu !! 3
    total = sum cpu
    diffIdle  = idle - pidle
    diffTotal = total - ptotal
    diffUsage = 100 * (diffTotal - diffIdle) `div` diffTotal

-- | Transforms a list of Integers to one line String separating values with spaces
percToStr :: [Integer] -- ^ List of Integers
          -> String
percToStr xs = (concat [ show x ++ " " | x <- xs ]) ++ "\n"

-- | Recursive cpu usage calculator
recWritePerc :: Settings  -- ^ settings
             -> [Integer] -- ^ previous idle values of all cores
             -> [Integer] -- ^ previous total values of all cores
             -> IO ()
recWritePerc settings pis pts = do
  cpus <- cpuInfo $ settingsNumCores settings
  let results = zipWith (!!) [ zipWith (getPerc cpu) pis pts | cpu <- cpus ] [0..]
  let resultsStr = percToStr $ map (!!2) results
  if null $ settingsOutputFile settings then
    putStr resultsStr
  else
    writeFile (settingsOutputFile settings) resultsStr
  threadDelay $ settingsRefreshRate settings * 1000000
  recWritePerc settings (map (!!0) results) (map (!!1) results)

-- | Main
main :: IO ()
main = do
  args <- getArgs
  let settings = parseSettings args
  recWritePerc settings [0,0..0] [0,0..0]
