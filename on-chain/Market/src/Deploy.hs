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

import qualified Data.List as DataList
import qualified Data.Time as DataTime (defaultTimeLocale, formatTime, getCurrentTime)
import PlutusTx.Prelude hiding (unless)
import qualified System.Directory as SystemDirectory
import qualified System.Environment as SystemEnvironment (getArgs, getProgName)
import qualified System.FilePath.Posix as SystemFilePathPosix
import qualified Prelude as P

--------------------------------------------------------------------------------2
-- Internal Imports
--------------------------------------------------------------------------------2

import qualified Helpers.CLI as CLIHelpers
import qualified Helpers.Deploy as DeployHelpers
import qualified Helpers.OffChain as OffChainHelpers

import Policys.PolicyID as PolicyID (policy_ID)
import Policys.PolicyNFT as PolicyNFT (nftPolicyCode)
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
      P.putStrLn "Generating PolicyID' Script..."
      ------------------------------
      let policy = PolicyID.policy_ID validator_Hash
          policy_CS = OffChainHelpers.getCurSymbolOfPolicy policy
      DeployHelpers.deployMintingPolicy (path SystemFilePathPosix.</> folderName) "PolicyID" policy policy_CS
  ------------------------------
      P.putStrLn "Generating 'PolicyNFT' Script..."
      ------------------------------
      let policyCode = PolicyNFT.nftPolicyCode
      DeployHelpers.writeCompiledCodeToJsonFile (path SystemFilePathPosix.</> folderName SystemFilePathPosix.</> "NftPolicy.plutus") policyCode
    return ()
