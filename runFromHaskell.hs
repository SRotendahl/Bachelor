import System.Process
import Text.Regex.Posix
pat = "(foo[a-z]*bar|quux)"
ting = "foodiequux,foodiebar" =~ pat :: String
--main = "foodiebar, fooquuxbar" =~ pat :: String

print_sizes = do
  sizes <- readProcess "./LocVolCalib" ["--print-sizes"] ""
  let arr = lines sizes
  return arr
