import System.Environment
import Parschema

main = do
  args <- getArgs
  if ((length args) < 1) then
    print "Error: Input a file name to parse"
  else do
    showParschemaFile (args !! 0)
