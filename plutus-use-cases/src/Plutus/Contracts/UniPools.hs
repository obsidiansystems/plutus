{-# LANGUAGE OverloadedStrings #-}

module Plutus.Contracts.UniPools where

import           Cardano.Api
import           Cardano.Api.Shelley
import qualified Data.Aeson                        as Aeson
import qualified Data.ByteString.Lazy              as LBS
import           Ledger
import           Plutus.Contracts.Currency
import           Plutus.Contracts.Uniswap.OffChain
import           Plutus.Contracts.Uniswap.Pool
import           Plutus.Contracts.Uniswap.Types
import           PlutusTx
import           System.Directory

-- After running main, Uniswap scripts will be compiled and files generated that will be passed into cardano-cli
-- when submitting scripts to create a uniswap pool between two coins.

main :: Integer -> Integer -> CurrencySymbol -> (CurrencySymbol, TokenName) -> (CurrencySymbol, TokenName) -> IO ()
main amountA amountB uniswapCurrencySymbol (coinACurrencySymbol, coinAName) (coinBCurrencySymbol, coinBName) = do
  let adaCoin = mkCoin coinACurrencySymbol coinAName
      pikaCoin = mkCoin coinBCurrencySymbol coinBName
      us = Uniswap $ mkCoin uniswapCurrencySymbol "Uniswap"
  let cp = CreateParams {
              cpCoinA = adaCoin
            , cpCoinB = pikaCoin
            , cpAmountA = (Amount amountA)
            , cpAmountB = (Amount amountB)
            }
  let lps = []
  -- (oref, o, lps) <- findUniswapFactory us
  let liquidity = calculateInitialLiquidity (cpAmountA cp) (cpAmountB cp)
      lp        = LiquidityPool {lpCoinA = adaCoin, lpCoinB = pikaCoin}
  let usInst   = uniswapInstance us
      usScript = uniswapScript us
      usDat1   = Factory $ lp:lps
      usDat2   = Pool lp liquidity
      psC      = poolStateCoin us
      lPolicy    = liquidityPolicy us
      lPolicyScript = toCardanoApiScript $ getMintingPolicy lPolicy
      lC       = mkCoin (liquidityCurrency us) $ lpTicker lp
      usVal    = unitValue $ usCoin us
      lpVal    = valueOf (cpCoinA cp) (cpAmountA cp)
        <> valueOf (cpCoinB cp) (cpAmountB cp)
        <> unitValue psC

  createDirectoryIfMissing False "./unipool"

  let uniswapPlutusScript = toCardanoApiScript $ getValidator $ uniswapScript us
  _ <- writeFileTextEnvelope "unipool/recover-uniswapPlutusScript.plutus" Nothing uniswapPlutusScript

  print $ "Liquidity Pool is "
  print lp
  writeFile "unipool/liquidityPool" $ show lp

  print $ "IHS-DEBUG: value of useDat1 is " ++ (show usDat1) ++ " written to factoryDatum.plutus"
  writeFile "unipool/factoryDatum.hash" $ show $ DatumHash $ dataHash $ toBuiltinData $ usDat1
  let factoryScriptDataFromDatum = fromPlutusData $ builtinDataToData $ toBuiltinData usDat1
      factoryScriptDataJson = scriptDataToJson ScriptDataJsonDetailedSchema factoryScriptDataFromDatum
  LBS.writeFile "./unipool/factoryDatum.plutus" $ Aeson.encode factoryScriptDataJson

  print $ "IHS-DEBUG: value of useDat2 is" ++ (show usDat2) ++ " written to poolDatum.plutus"
  _ <- writeFile "unipool/poolDatum.hash" $ show $ DatumHash $ dataHash $ toBuiltinData $ usDat2
  let poolScriptDataFromDatum = fromPlutusData $ builtinDataToData $ toBuiltinData usDat2
      poolScriptDataJson = scriptDataToJson ScriptDataJsonDetailedSchema poolScriptDataFromDatum
  LBS.writeFile "./unipool/poolDatum.plutus" $ Aeson.encode poolScriptDataJson

  let poolScriptDataFromDatum' = fromPlutusData $ builtinDataToData $ toBuiltinData $ Factory []
      poolScriptDataJson' = scriptDataToJson ScriptDataJsonDetailedSchema poolScriptDataFromDatum'
  LBS.writeFile "./unipool/poolDatum.empty.plutus" $ Aeson.encode poolScriptDataJson'

  print $ "The value of pool state coin is " ++ (show psC)
  writeFile "unipool/poolStateCoin" $ show psC

  print $ "The liquidity policy script is " ++ (show lPolicyScript)
  _ <- writeFileTextEnvelope "unipool/liquidityCurrencyPolicy.plutus" Nothing lPolicyScript

  let redeemerUniswapAction :: UniswapAction
      redeemerUniswapAction = Create lp
  let redeemerCoder = fromPlutusData $ builtinDataToData $ toBuiltinData redeemerUniswapAction
      redeemerJson = scriptDataToJson ScriptDataJsonDetailedSchema redeemerCoder
  LBS.writeFile "./unipool/unipool-redeemer" $ Aeson.encode redeemerJson

  print $ "The value of uniswap instance is " ++ (show usInst)
  print $ "The value of uniswap script is " ++ (show usScript)
  print $ "The value of uniswap value is " ++ (show usVal)
  print $ "The value of liquidity pool val is " ++ (show lpVal)
  print $ "The value of cpAmountA is " ++ (show $ cpAmountA cp)
  print $ "The value of cpAmountB is " ++ (show $ cpAmountB cp)
  print $ "The value of liquidity is " ++ (show liquidity)
  print $ "Liquidity Coin is " ++ (show lC)
  print $ "Uniswap is " ++ (show us)
