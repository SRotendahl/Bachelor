import System.Process
import System.IO.Silently
import Text.Regex
import Text.Regex.Base
import Data.List.Split
import Data.Maybe

getBenchOutput =
  capture $ callCommand "futhark bench --backend=opencl LocVolCalib.fut --tuning=tune --skip-compilation --exclude-case=notune"

splitNameTime :: [String] -> [(String,Float)]
splitNameTime str = 
  let regex = mkRegex "dataset +([^:]+)\\:\\ +([0-9]+\\.[0-9]+)"
      list  = catMaybes $ map (matchRegex regex) str
  in  map (\x -> (x!!0, read $ x!!1)) list


main = do  
  out <- getBenchOutput
  let test = fst out
  let mkLines = lines test 
  let match = splitNameTime mkLines
  mapM_ print $ match
