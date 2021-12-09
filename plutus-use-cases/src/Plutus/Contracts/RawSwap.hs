{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Plutus.Contracts.RawSwap where

import           Cardano.Api
import           Cardano.Api.Shelley
import           Codec.Serialise
import           Control.Monad                     (when)
import qualified Data.Aeson                        as Aeson
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Base16            as B16
import qualified Data.ByteString.Lazy              as BSL
import           Data.Maybe
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import           GHC.Generics
import           Ledger
import           Ledger.Scripts
import           Ledger.Value
import           Plutus.Contracts.Uniswap.OffChain
import           Plutus.Contracts.Uniswap.Types
import           PlutusCore.Data
import           PlutusTx
import           PlutusTx.Builtins.Internal
import           Safe
import           System.Directory
import           System.Process

data RecentPoolBalance = RecentPoolBalance
  { pcA :: Integer
  , pcB :: Integer
  } deriving (Show, Generic)

instance FromJSON RecentPoolBalance
instance ToJSON RecentPoolBalance

main
  :: CurrencySymbol
  -> TokenName
  -> Integer
  -> CurrencySymbol
  -> TokenName
  -> Integer
  -> FilePath  -- file with pool state datum
  -> FilePath  -- exe path to cardano-cli
  -> FilePath  -- path to cardano-node node.socket
  -> String    -- uniswap smart contract address
  -> String    -- testnet magic number
  -> IO ()
main coinACurrencySymbol tokenNameA amountA coinBCurrencySymbol tokenNameB amountB poolStateDatumHash cardanoCliExe cardanoNodeSocket addr magic = do
  let coinA = mkCoin coinACurrencySymbol tokenNameA
      coinB = mkCoin coinBCurrencySymbol tokenNameB
      swapParams = SwapParams {
          spCoinA = coinA
        , spCoinB = coinB
        , spAmountA = Amount amountA
        , spAmountB = Amount amountB
        }
  if not (spAmountA swapParams > 0 && spAmountB swapParams == 0 || spAmountA swapParams == 0 && spAmountB swapParams > 0)
    then print ("exactly one amount must be positive" :: String)
    else do
      rawJsonScriptData <- BSL.readFile poolStateDatumHash
      let eJsonScriptData :: Either String Aeson.Value = Aeson.eitherDecode rawJsonScriptData
      case eJsonScriptData of
        Left err -> do
          print $ "pool datum not found: " ++ err
          return ()
        Right jsonScriptData -> do
          let eitherScriptData = scriptDataFromJson ScriptDataJsonDetailedSchema jsonScriptData
          case eitherScriptData of
            Left err -> do
              print err
              return ()
            Right sData -> do
              let poolDatum :: Maybe UniswapDatum = fromBuiltinData $ dataToBuiltinData $ toPlutusData sData
              case poolDatum of
                Nothing -> print ("fromBuiltinData conversion failed" :: String)
                Just uniswapDatum -> case uniswapDatum of
                  Factory _ -> print ("factory found instead of LiquidityPool" :: String)
                  Pool lp la -> do
                    pwd <- getCurrentDirectory
                    print $ "pwd: " ++ (show pwd)
                    -- Read stdout to get the balance of each tokens that makes of the liquidity pool
                    (_, cliOut, cliErr) <- readCreateProcessWithExitCode
                      (CreateProcess
                        {
                          cmdspec = RawCommand
                            cardanoCliExe
                            ["query", "utxo", "--address", addr, "--testnet-magic", magic]
                        , cwd = Nothing
                        , env = Just $ [(("CARDANO_NODE_SOCKET_PATH" :: String)
                                     ,cardanoNodeSocket)]
                        , std_in = Inherit
                        , std_out = Inherit
                        , std_err = Inherit
                        , close_fds = False
                        , create_group = False
                        , delegate_ctlc = False
                        , detach_console = False
                        , create_new_console = False
                        , new_session = False
                        , child_group = Nothing
                        , child_user = Nothing
                        , use_process_jobs = False
                        }) []
                    let tkA = if tokenNameA == "" then "lovelace" else do
                          let (BuiltinByteString bsA) = unTokenName tokenNameA
                          T.decodeUtf8 bsA
                        tkB = if tokenNameB == "" then "lovelace" else do
                          let (BuiltinByteString bsB) = unTokenName tokenNameB
                          T.decodeUtf8 bsB
                        mNodeQueryResults = headMay $ filter (\str -> T.isInfixOf tkA str && T.isInfixOf tkB str) $ map T.pack $ lines cliOut
                        -- drop leading txHash and txIndex, keep balances of what is being held at the utxo handle
                    print $ show cliOut
                    print $ show cliErr
                    print $ show tkA
                    print $ show tkB
                    print $ show mNodeQueryResults
                    case mNodeQueryResults of
                      Nothing -> print ("No utxo found for token pool" :: String)
                      Just nodeQueryResults -> do
                        let utxoBalances = map T.strip $ T.splitOn "+" $ T.intercalate " " $ drop 2 $ T.words $ nodeQueryResults
                            balanceTokenA = fromMaybe 0 $ maybe Nothing (\str -> readMay (T.unpack str) :: Maybe Integer)
                              $ maybe Nothing (\str -> headMay $ T.words str)
                              $ headMay $ filter (\str -> T.isInfixOf tkA str) utxoBalances
                            balanceTokenB = fromMaybe 0 $ maybe Nothing (\str -> readMay (T.unpack str) :: Maybe Integer)
                              $ maybe Nothing (\str -> headMay $ T.words str)
                              $ headMay $ filter (\str -> T.isInfixOf tkB str) utxoBalances
                        print $ balanceTokenA
                        print $ balanceTokenB
                        print $ show lp
                        print $ show la
                        let oldA = Amount balanceTokenA
                            oldB = Amount balanceTokenB
                        (newA, newB) <- if (spAmountA swapParams) > 0 then do
                            let outB = Amount $ findSwapA oldA oldB $ spAmountA swapParams
                            when (outB == 0) $ print "no payout" -- TODO: should throw
                            return (oldA + (spAmountA swapParams), oldB - outB)
                                                         else do
                            let outA = Amount $ findSwapB oldA oldB $ spAmountB swapParams
                            when (outA == 0) $ print "no payout" -- TODO: should throw
                            return (oldA - outA, oldB + (spAmountB swapParams))
                        print $ "Estimated amount to returned after swap: " ++ (show $ oldA - newA)
                        print $ "Estimated amount of newB is : " ++ (show newB)
                        print $ "Estimated amount to returned after inverse swap: " ++ (show $ oldB - newB)
                        print $ "New liquidity balance for " ++ (show tokenNameA) ++ ": " ++ (show newA)
                        print $ "New liquidity balance for " ++ (show tokenNameB) ++ ": " ++ (show newB)

                        createDirectoryIfMissing False "./rawSwap"

                        let redeemerUniswapAction :: UniswapAction
                            redeemerUniswapAction = Swap
                        let redeemerPlutusData = serialise $ builtinDataToData $ toBuiltinData redeemerUniswapAction
                            redeemerCoder = fromPlutusData $ builtinDataToData $ toBuiltinData redeemerUniswapAction
                            redeemerJson = scriptDataToJson ScriptDataJsonDetailedSchema redeemerCoder
                            recentPoolBalance = RecentPoolBalance (unAmount newA) (unAmount newB)
                        print $ "redeemerCoder " ++ (show redeemerCoder)
                        BSL.writeFile "./rawSwap/rawSwap-redeemer-test" $ BSL.fromStrict $ B16.encode $ BSL.toStrict redeemerPlutusData
                        BSL.writeFile "./rawSwap/rawSwap-redeemer" $ Aeson.encode redeemerJson
                        BSL.writeFile "./rawSwap/recentPoolBalance.json" $ Aeson.encode recentPoolBalance

                        let poolScriptDataFromDatum = fromPlutusData $ builtinDataToData $ toBuiltinData uniswapDatum
                            poolScriptDataJson = scriptDataToJson ScriptDataJsonDetailedSchema poolScriptDataFromDatum
                        BSL.writeFile "./rawSwap/poolDatum.plutus" $ Aeson.encode poolScriptDataJson
                        return ()
              return ()
