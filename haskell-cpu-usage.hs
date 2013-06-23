--------------------------------------------------------------------------------------------
-- Author      : Nnoell <nnoell3[at]gmail.com>                                            --
-- Stability   : Unstable                                                                 --
-- Portability : Unportable                                                               --
-- Desc        : Simple haskell CPU usage monitor                                         --
-- Usage       : haskell-cpu-usage.out <number-of-cores>                                  --
--               1 -> [cpu]                                                               --
--               2 -> [cpu, cpu1]                                                         --
--               3 -> [cpu, cpu1, cpu2]                                                   --
--               N -> [cpu, cpu1, cpu2, ..., cpuN-1]                                      --
--------------------------------------------------------------------------------------------

-- Useful imports
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)


-- | Default number of cores
defNumCores :: Int
defNumCores = 5

-- | Refresh rate (in seconds)
delay :: Int
delay = 1

-- | Where to write cpu percent
cpuPercFile :: String
cpuPercFile = "/tmp/haskell-cpu-usage.txt"


--------------------------------------------------------------------------------------------

-- | Get /proc/stat values of a specific cpu
cpuInfo :: Int -- ^ number of cores
		-> IO [[Integer]]
cpuInfo ncpus = do
	c <- readFile "/proc/stat"
	let getCoresInfo = map (dropWhile (/= ' ')) $ take ncpus $ lines c
	return $ map (map read) $ map words $ getCoresInfo


-- | Get cpuPerc with its respective idle and total amount of time: [idle,total,cpuPerc]
getPerc :: [Integer] -- ^ specific cpu values of /proc/stat
		-> Integer   -- ^ previous idle value
		-> Integer   -- ^ previous total value
		-> [Integer]
getPerc cpu pidle ptotal = idle:total:diffUsage:[]
	where
		idle = cpu !! 3
		total = sum cpu
		diffIdle  = idle - pidle
		diffTotal = total - ptotal
		diffUsage = (100 * (diffTotal - diffIdle) `div` diffTotal)

-- | Transforms a list of Integers to one line String separating values by Whitespaces
percToStr :: [Integer] -- ^ List of Integers
		  -> String
percToStr xs = (concat [ (show x) ++ " " | x <- xs ]) ++ "\n"

-- | Recursive cpu usage calculator
recWritePerc :: Int        -- ^ number of cores
			 -> [Integer] -- ^ previous idle values of all cores
			 -> [Integer] -- ^ previous total values of all cores
			 -> IO ()
recWritePerc ncpus pis pts = do
	cpus <- cpuInfo ncpus
	let results = zipWith (!!) [ zipWith (getPerc cpu) pis pts | cpu <- cpus ] [0..]
	writeFile cpuPercFile $ percToStr $ map (!!2) results
	threadDelay $ delay * 1000000
	recWritePerc ncpus (map (!!0) results) (map (!!1) results)

-- | Main
main :: IO ()
main = do
	args <- getArgs
	let argValue = if null args then defNumCores else (read (head args)::Int)
	recWritePerc argValue [0,0..0] [0,0..0]
