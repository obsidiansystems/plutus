{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Read             as T
import qualified Plutus.Contracts.RawSwap   as PCR
import           System.Environment

import           Ledger
import           Ledger.Value
import           PlutusTx.Builtins.Internal


main :: IO ()
main = do
  coinACurrencySymbol:tokenNameA:amountA:coinBCurrencySymbol:tokenNameB:amountB:poolStateDatumHash:cardanoCliExe:cardanoNodeSocket:uniswapAddress:testnetMagic:proposalPath:_ <- getArgs
  let coinACurrencySymbol' :: CurrencySymbol = CurrencySymbol $ BuiltinByteString $ T.encodeUtf8 $ T.pack coinACurrencySymbol
      coinBCurrencySymbol' :: CurrencySymbol = CurrencySymbol $ BuiltinByteString $ T.encodeUtf8 $ T.pack coinBCurrencySymbol
      tokenNameA' :: TokenName = TokenName $ BuiltinByteString $ T.encodeUtf8 $ T.pack tokenNameA
      tokenNameB' :: TokenName = TokenName $ BuiltinByteString $ T.encodeUtf8 $ T.pack tokenNameB
      amountA' :: Integer = either (\_ -> 0) Prelude.fst $ T.decimal $ T.pack amountA
      amountB' :: Integer = either (\_ -> 0) Prelude.fst $ T.decimal $ T.pack amountB
  PCR.main coinACurrencySymbol' tokenNameA' amountA' coinBCurrencySymbol' tokenNameB' amountB' poolStateDatumHash cardanoCliExe cardanoNodeSocket uniswapAddress testnetMagic proposalPath

