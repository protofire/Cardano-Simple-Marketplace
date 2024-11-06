{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Deploy where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2

import qualified Control.Exception as ControlException (throwIO)
import qualified Control.Monad.IO.Class as MonadIOClass (MonadIO (..))
import qualified Data.Aeson as DataAeson (FromJSON, ToJSON)
import qualified Data.List as DataList
import qualified Data.OpenApi.Schema as DataOpenApiSchema (ToSchema)
import qualified Data.String as DataString
import qualified Data.Time as DataTime (defaultTimeLocale, formatTime, getCurrentTime)
import qualified GHC.Generics as GHCGenerics (Generic)
import qualified Ledger
import qualified Ledger.Value as LedgerValue
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx
import qualified PlutusTx.Builtins.Class as TxBuiltinsClass
import PlutusTx.Prelude hiding (unless)
import qualified Schema
import qualified System.Directory as SystemDirectory
import qualified System.Environment as SystemEnvironment (getArgs, getProgName)
import qualified System.FilePath as SystemFilePath
import qualified System.FilePath.Posix as SystemFilePathPosix
import qualified System.Process as SystemProcess
import qualified Prelude as P

--------------------------------------------------------------------------------2
-- Internal Imports
--------------------------------------------------------------------------------2

import qualified Helpers.CLI as CLIHelpers
import qualified Helpers.Deploy as DeployHelpers
import qualified Helpers.OffChain as OffChainHelpers

import Policys.PolicyID as PolicyID (policy_ID, policyIdCode)
import Validators.MarketValidator as MarketValidator (marketValidator)

--------------------------------------------------------------------------------2

main :: P.IO ()
main = deploy

deploy :: P.IO ()
deploy = do
  ------------------------------
  args <- SystemEnvironment.getArgs
  ------------------------------
  case args of
    [baseFolder] -> runDeploy baseFolder
    [] -> runDeploy ""
    _ -> P.putStrLn "Error: Expected 1 argument: baseFolder"

------------------------------

runDeploy :: P.String -> P.IO ()
runDeploy baseFolder = do
    ------------------------------
    progName <- SystemEnvironment.getProgName
    ------------------------------
    let stripSuffix suffix str =
          if suffix `DataList.isSuffixOf` str
            then P.take (P.length str P.- P.length suffix) str
            else str
        projectName = stripSuffix "Deploy" progName
    ------------------------------
    let path = baseFolder SystemFilePathPosix.</> "export" SystemFilePathPosix.</> projectName
    ------------------------------
    currentTime <- DataTime.getCurrentTime
    let defaultName = DataTime.formatTime DataTime.defaultTimeLocale "%Y-%m-%d-%H-%M" currentTime
    ------------------------------
    P.putStrLn $ "Folder Name (default=" ++ defaultName ++ "):"
    folderName <- CLIHelpers.getStrWithDefault defaultName
    ------------------------------
    SystemDirectory.removePathForcibly (path SystemFilePathPosix.</> folderName)
    SystemDirectory.createDirectoryIfMissing True (path SystemFilePathPosix.</> folderName)
    ------------------------------
    P.putStrLn "Generating Files..."
    ------------------------------
    P.putStrLn $ "Path: " ++ path SystemFilePathPosix.</> folderName
    ------------------------------
    do
      P.putStrLn "Generating 'marketValidator' Script..."
      let validator = MarketValidator.marketValidator
          validator_Hash = OffChainHelpers.hashValidator validator
          validator_Address = OffChainHelpers.addressValidator validator_Hash
      _ <- DeployHelpers.deployValidator (path SystemFilePathPosix.</> folderName) "marketValidator" validator
      _ <- DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> folderName) "marketValidator" validator_Hash
      DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> folderName) "marketValidator" validator_Address
      ------------------------------
      P.putStrLn "Generating 'paramCheckSignaturePolicy' Script..."
      ------------------------------
      let policy = PolicyID.policy_ID validator_Hash
          policy_CS = OffChainHelpers.getCurSymbolOfPolicy policy
      DeployHelpers.deployMintingPolicy (path SystemFilePathPosix.</> folderName) "PolicyID" policy policy_CS
  ------------------------------
    return ()
