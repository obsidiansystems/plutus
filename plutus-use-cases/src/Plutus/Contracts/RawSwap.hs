{-# LANGUAGE OverloadedStrings #-}

module Plutus.Contracts.RawSwap where

import           Cardano.Api
import           Cardano.Api.Shelley
import           Control.Monad                     (when)
import qualified Data.Aeson                        as Aeson
import qualified Data.ByteString.Lazy              as BSL
import           Data.Maybe
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import           Ledger
import           Ledger.Value
import           Plutus.Contracts.Uniswap.OffChain
import           Plutus.Contracts.Uniswap.Types
import           PlutusTx
import           PlutusTx.Builtins.Internal
import           Safe
import           System.Directory
import           System.Process

main :: (CurrencySymbol,TokenName) -> Integer -> (CurrencySymbol, TokenName) -> Integer -> FilePath -> IO ()
main (coinACurrencySymbol, tokenNameA) amountA (coinBCurrencySymbol, tokenNameB) amountB poolStateDatumHash = do
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
      -- (_, (oref, o, lp, liquidity)) <- findUniswapFactoryAndPool us (spCoinA swapParams) (spCoinB swapParams)
      -- let poolScriptDataFromDatum = fromBuiltinData $ dataToBuiltinData $ toPlutusData poolStateDatumHash
      -- mOutVal <- datumFromHash poolStateDatumHash
        -- TODO: How to turn Datum to a Ledger.Value?
      -- let outVal = view ciTxOutValue o
      -- TODO: Should not read pool state from a file, it should be read from the node. Use datumFromHash and add helper functions to Plutus.Contracts.Uniswap... to read from node
      -- and calculate swap results.
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
                    -- Read stdout to get the balance of each tokens that makes of the liquidity pool
                    (_, cliOut, _) <- readCreateProcessWithExitCode
                      (CreateProcess
                        {
                          cmdspec = RawCommand
                            "/home/zigpolymath/cardano-cli"
                            ["query", "utxo", "--address", "addr_test1wpr9r6r6q0wgydckagwh52kvw2fxwq3mc3mpcym7l6l9lzqgvz4f8", "--testnet-magic", "8"]
                        , cwd = Nothing
                        , env = Just $ [(("CARDANO_NODE_SOCKET_PATH" :: String)
                                     ,("/home/zigpolymath/node.socket" :: String))]
                        , std_in = Inherit
                        , std_out = Inherit
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
                        print $ "New liquidity balance for " ++ (show tokenNameA) ++ ": " ++ (show newA)
                        print $ "New liquidity balance for " ++ (show tokenNameB) ++ ": " ++ (show newB)

                        createDirectoryIfMissing False "./rawSwap"

                        let redeemerUniswapAction :: UniswapAction
                            redeemerUniswapAction = Swap
                        let redeemerCoder = fromPlutusData $ builtinDataToData $ toBuiltinData redeemerUniswapAction
                            redeemerJson = scriptDataToJson ScriptDataJsonDetailedSchema redeemerCoder
                        BSL.writeFile "./rawSwap/rawSwap-redeemer" $ Aeson.encode redeemerJson

                        let poolScriptDataFromDatum = fromPlutusData $ builtinDataToData $ toBuiltinData uniswapDatum
                            poolScriptDataJson = scriptDataToJson ScriptDataJsonDetailedSchema poolScriptDataFromDatum
                        BSL.writeFile "./rawSwap/poolDatum.plutus" $ Aeson.encode poolScriptDataJson
                        return ()
              return ()
