{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE MultiParamTypeClasses      #-} 

module UnitTests where 

-- import qualified Plutus.V2.Ledger.Api                    as LedgerApiV2
import qualified PlutusLedgerApi.V2 as LedgerApiV2
import qualified Data.ByteString.Char8                   as B
-- import qualified Ledger
import qualified Prelude                                as P
import qualified PlutusTx
import PlutusTx.Prelude 
-- import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as Scripts
-- import qualified Plutus.Script.Utils.Typed as Scripts
-- import qualified Plutus.Script.Utils.V1.Typed.Scripts.Validators as Scripts




beneficiaryPkh :: LedgerApiV2.PubKeyHash
beneficiaryPkh = "0e97aa033ceee762c25285cdcc94287178f01b45a585fd0d4da8387a"
dCurrencySymbol :: LedgerApiV2.CurrencySymbol
dCurrencySymbol = "404c7e7ab0e42a8b1a247c2eba252698cad8a19c50c0cf5cfb1c2283" -- last one
dToken    :: LedgerApiV2.TokenName
dToken = "MyCrowdFund"

targetAmount :: P.Integer
targetAmount = 300000000   -- 300 Ada

-- 1st contributor is Collateral wallet
contributor1 :: B.ByteString
contributor1 = "0d29d2f72ba11f3381783dda5501139f397d81e83244fce13e7a711a"  -- Collateral wallet

-- 2nd contributor is Beneficiary wallet address (not to confuse with Crowd Fund Beneficiary)
contributor2 :: B.ByteString
contributor2 = "0e97aa033ceee762c25285cdcc94287178f01b45a585fd0d4da8387a"  -- Beneficiary wallet

crowdDeadline :: LedgerApiV2.POSIXTime
crowdDeadline = 1676641405000    -- Friday, February 17, 2023 5:43:25 AM GMT-08:00
-- Epoch timestamp: 1676641405
-- Timestamp in milliseconds: 1676641405000
-- Date and time (GMT): Friday, February 17, 2023 1:43:25 PM
-- Date and time (your time zone): Friday, February 17, 2023 5:43:25 AM GMT-08:00



-- data Dat = Dat 
--     {
--         beneficiary :: LedgerApiV2.PubKeyHash
--         , deadline :: LedgerApiV2.POSIXTime
--         , aCurrency :: LedgerApiV2.CurrencySymbol
--         , aToken    :: LedgerApiV2.TokenName
--         , targetAmount :: P.Integer
--         , actualtargetAmountsoFar :: P.Integer
--         , contributorsMap :: [(LedgerApiV2.PubKeyHash, P.Integer)] 
--     } 
--     deriving P.Show
-- PlutusTx.unstableMakeIsData ''Dat



-- data Redeem = Contribute 
--     {
--         contribution :: (LedgerApiV2.PubKeyHash,Integer)
--     } 
--               | Close 
--     deriving P.Show

-- PlutusTx.unstableMakeIsData ''Redeem

-- data Crowd
-- instance Scripts.ValidatorTypes Crowd where
--     type instance RedeemerType Crowd = Redeem
--     type instance DatumType Crowd = Dat

-- datumCrowd :: Dat
-- datumCrowd = Dat { 
--                              beneficiary = beneficiaryPkh
--                            , deadline =crowdDeadline
--     -- https://preview.cardanoscan.io/token/d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be4d7943726f776446756e64?tab=transactions
--                            -- , OnChain.aCurrency = "d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be"
--                            ,    aCurrency = dCurrencySymbol
--                            , aToken = dToken
--                            , targetAmount = targetAmount
--                            , actualtargetAmountsoFar = 2000000
--                            , contributorsMap = []}
                          
-- datumCrowdBi = LedgerApiV2.toData datumCrowd



-- datumBuilt = PlutusTx.toBuiltinData datumCrowd
-- writeCrowdDatum :: P.IO ()
-- writeCrowdDatum = 
--     let crowd = datumCrowd
--         d = PlutusTx.toBuiltinData crowd
--     in writeJSON "src/CrowdFunding/Deploy/crowdFunding-datum.json" d


-- instance PlutusTx.ToData Dat where
--   toBuiltinData dat =
--     let beneficiaryData = PlutusTx.toBuiltinData (beneficiary dat)
--         deadlineData = PlutusTx.toBuiltinData (deadline dat)
--         currencyData = PlutusTx.toBuiltinData (aCurrency dat)
--         tokenData = PlutusTx.toBuiltinData (aToken dat)
--         targetAmountData = PlutusTx.toBuiltinData (targetAmount dat)
--         targetAmountSoFarData = PlutusTx.toBuiltinData (actualtargetAmountsoFar dat)
--         contributorsMapData = PlutusTx.toBuiltinData (contributorsMap dat)
--     in PlutusTx.toBuiltinData $
--         PlutusTx.Tuple7
--             beneficiaryData
--             deadlineData
--             currencyData
--             tokenData
--             targetAmountData
--             targetAmountSoFarData
--             contributorsMapData