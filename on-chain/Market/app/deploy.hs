module Main where
------------------------------------------------------------------------------------------
-- External Imports
------------------------------------------------------------------------------------------
import qualified Control.Exception  as ControlException (throwIO)
import qualified Data.List as DataList
import qualified System.Environment  as SystemEnvironment (getArgs)
------------------------------------------------------------------------------------------
-- Internal Imports
------------------------------------------------------------------------------------------
import qualified Deploy as Deploy
------------------------------------------------------------------------------------------
-- Module
------------------------------------------------------------------------------------------
main :: IO ()
main = do
  [ pathStr] <- SystemEnvironment.getArgs
  _ <- Deploy.runDeploy pathStr 
  return ()
