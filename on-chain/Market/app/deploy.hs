module Main where
------------------------------------------------------------------------------------------
-- External Imports
------------------------------------------------------------------------------------------
import qualified Control.Exception  as ControlException (throwIO)
import qualified Data.List as DataList
import qualified System.Environment  as SystemEnvironment (getArgs)
import qualified Prelude as P

------------------------------------------------------------------------------------------
-- Internal Imports
------------------------------------------------------------------------------------------
import qualified Deploy 
------------------------------------------------------------------------------------------
-- Module
------------------------------------------------------------------------------------------

main :: P.IO ()
main = deploy

deploy :: P.IO ()
deploy = do
  ------------------------------
  args <- SystemEnvironment.getArgs
  ------------------------------
  case args of
    [baseFolder] -> Deploy.runDeploy baseFolder
    [] -> Deploy.runDeploy ""
    _ -> P.putStrLn "Error: Expected 1 argument: baseFolder"

------------------------------