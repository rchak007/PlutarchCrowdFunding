{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}              -- use @ symbol

-- 2. imports external/imports

module CrowdFundingOnChain (pcrowdValidatorW) where

-- from DAOValidator
import Plutarch.Api.V2 
-- import Plutarch.Api.V1 (PCredential (PPubKeyCredential, PScriptCredential))
-- import Plutarch.Api.V1 
import Plutarch.DataRepr
import Plutarch.Api.V1.Value 
import Plutarch.Bool
import Plutarch.Prelude

import Plutarch.Extra.ScriptContext (ptryFromInlineDatum, pfromPDatum)
-- import Utils (ppositiveSymbolValueOf, (#>), (#>=))
import Utils
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont 
import Plutarch.Builtin as BI
import Plutarch.Lift
import PlutusCore (DefaultFun(..))
import Plutarch.Unsafe
import Control.Monad
import qualified PlutusTx
import qualified Plutarch as PL
import Plutarch.Evaluate (evalScript, evalScriptHuge, evalTerm)
import Data.Text (Text, pack)
import PlutusLedgerApi.V1 (Data, ExBudget)
import Plutarch.Script (Script (Script, unScript), serialiseScript)
import Data.Default (def)
import Control.Lens.Combinators (over)
import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program, progTerm)
import PlutusCore.MkPlc (mkIterApp, mkConstant) 
import Data.Bifunctor (first)
import Data.ByteString.Short (ShortByteString)

import PlutusLedgerApi.V2
import qualified PlutusLedgerApi.V2 as LedgerApiV2
-- import qualified Ledger
import qualified Data.ByteString.Char8                   as B
import PlutusLedgerApi.V1.Interval
-- import PlutusLedgerApi.V1
import qualified Plutarch.Monadic as P
import qualified Data.Map
import PlutusTx.AssocMap
-- import Plutus.Script.Utils.V2.Typed.Scripts.Validators
-- import Plutus.Script.Utils.V2.Scripts
-- import qualified Ledger.Ada                                      as Ada
-- import Plutus.Script.Utils.Ada

import Plutarch.Trace
import Plutarch.Context (
  BaseBuilder,
  Builder,
  address,
  buildMinting,
  buildMinting',
  buildSpending,
  buildTxInfo,
  buildTxOuts,
  checkNormalized,
  input,
  mint,
  mkNormalized,
  normalizeValue,
  output,
  pubKey,
  runChecker,
  script,
  withDatum,
  withMinting,
  withRefIndex,
  withRefTxId,
  withSpendingUTXO,
  withStakingCredential,
  withValue,
  withdrawal,
 )


import Test.Tasty (TestTree, defaultMain, testGroup)
import Plutarch.Test.Precompiled (
  Expectation (Failure, Success),
  testEqualityCase,
  testEvalCase,
  tryFromPTerm,
  withApplied,
  (@!>),
  (@&),
  (@>),
 )
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch (
  Config (Config, tracingMode),
  TracingMode (NoTracing),
  compile,
 )
-- import qualified Prelude                                as P
-- Unit testing
-- import UnitTests

-- import Plutarch.Docs.Run (applyArguments, evalT, evalSerialize, evalWithArgsT, evalWithArgsT') 
-- import Plutarch.Run
-- import qualified Plutarch.Monadic as P
-- import Plutarch.Docs.Run (evalWithArgsT)

-- Reference
-- data PDaoDatum (s :: S) = 
--    PDaoDatum (Term s (PDataRecord '["approvedSignatories" ':= PBuiltinList (PAsData PPubKeyHash), "requiredNoOfSigs" ':= PInteger]))
                    -- I think PDataRecord is a list 
--  deriving stock (Generic)
--  deriving anyclass (PlutusType, PIsData, PDataFields)

data PDat (s :: S) = 
    PDat 
      ( Term 
          s 
          ( PDataRecord 
          '["beneficiary" ':= (PPubKeyHash)
          -- '["beneficiary" ':= (PAsData PPubKeyHash)
          , "deadline" ':= PPOSIXTime
          , "aCurrency" ':= PCurrencySymbol
          , "aToken" ':= PTokenName
          , "targetAmount" ':= PInteger
          , "actualtargetAmountsoFar" ':= PInteger
          , "contributorsMap" ':= PBuiltinList (PAsData (PTuple (PAsData PPubKeyHash) PInteger)) ] ))
 deriving stock (Generic)
 deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PDat where
  type DPTStrat _ = PlutusTypeData 

instance PTryFrom PData (PAsData PDat)
instance PTryFrom PData PDat 
  
-- from -- https://github.com/Plutonomicon/plutarch-plutus/blob/master/plutarch-docs/Types/PDataSum%20and%20PDataRecord.md
-- newtype Foo (s :: S) = Foo (Term s (PDataRecord '["fooField" ':= PInteger]))  

------- Orig Haskell type
-- Haskell datum
-- data Dat = Dat 
--     {
--         beneficiary :: Ledger.PaymentPubKeyHash
--         , deadline :: LedgerApiV2.POSIXTime
--         , aCurrency :: LedgerApiV2.CurrencySymbol
--         , aToken    :: LedgerApiV2.TokenName
--         , targetAmount :: Integer
--         , actualtargetAmountsoFar :: Integer
--         , contributorsMap :: [(Ledger.PaymentPubKeyHash,Integer)] 
--     } deriving P.Show
-- PlutusTx.unstableMakeIsData ''Dat


-- instance DerivePlutusType PDat where
--   type DPTStrat _ = PlutusTypeEnumData 

-- instance PTryFrom PData (PAsData PDat)

-- PlutusTx.unstableMakeIsData ''Dat

        -- Keep hash of whole MAP - for now we will do with Map. 
        -- Later need to optimize since if we have 1000s ofcontributors its storage issue
            -- maybe we instead mint an NFT to them instead of storing them here etc. 


-- data Contribution 

-- data PRedeem (s :: S) = PContribute 
--     {
--         contribution ::  Term s PInteger   -- (Ledger.PaymentPubKeyHash,Integer)
--     } 
--               | PClose 
--  deriving stock (Generic)
--  deriving anyclass (PlutusType, PEq, PShow)

data PRedeem (s :: S) = 
      PContribute 
        ( Term 
          s 
          ( PDataRecord 
          '["contribution" ':= PInteger])) 
      | PClose (Term s (PDataRecord '[]))
 deriving stock (Generic)
 deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PRedeem where
  type DPTStrat _ = PlutusTypeData 

instance PTryFrom PData (PAsData PRedeem)
instance PTryFrom PData PRedeem 

-- instance DerivePlutusType PRedeem where
--   type DPTStrat _ = PlutusTypeData 

-- instance PTryFrom PData (PAsData PRedeem)
-- instance PTryFrom PData PRedeem 

-- data PDaoAction (s :: S) = 
--     Add (Term s (PDataRecord '["pkh" ':= PPubKeyHash]))
--    | Remove (Term s (PDataRecord '["pkh" ':= PPubKeyHash]))
--    | Approve (Term s (PDataRecord '[]))
--  deriving stock (Generic)
--  deriving anyclass (PlutusType, PIsData, PEq, PShow)


-- instance DerivePlutusType PRedeem where
--   type DPTStrat _ = PlutusTypeEnumData 

-- instance PTryFrom PData (PAsData PRedeem)

-- PlutusTx.unstableMakeIsData ''Redeem

-- data Crowd
-- instance Scripts.ValidatorTypes Crowd where
--     type instance RedeemerType Crowd = Redeem
--     type instance DatumType Crowd = Dat

-- data Dt1 = Dt1 { 
--                    tAmount :: Integer 
--       } deriving P.Show


-- Define a function using plam
-- increment :: Term s pInteger -> Term s pInteger
-- increment = plam (\x -> x + 1)

-- -- 3. onchain code


-- pcrowdValidatorTest :: Term s  (PDat :--> PRedeem :--> PScriptContext :--> PUnit)
pcrowdValidatorTest :: Term s  (PAsData PPubKeyHash :--> PDat :--> PRedeem :--> PScriptContext :--> PUnit)
-- pcrowdValidatorTest :: Term s  (PPubKeyHash PAsData PDat :--> PAsData PRedeem :--> PAsData PScriptContext :--> PUnit)

-- pcrowdValidator :: Term s PValidator 
pcrowdValidatorTest = phoistAcyclic $ plam $ \ph dat redeemer ctx -> unTermCont $ do 
-- pcrowdValidatorTest = phoistAcyclic $ plam $ \dat redeemer ctx -> unTermCont $  do 
  ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
  PSpending ((pfield @"_0" #) -> ownRef) <- pmatchC ctxF.purpose
    -- here if a Txn is submitted with more than 1 script UTXO then this validator will be called mutiple times actually each time with 1 script UTXO to spend it.
  datF <- pletFieldsC @'["beneficiary", "deadline", "aCurrency", "aToken", "targetAmount", "actualtargetAmountsoFar", "contributorsMap" ] dat
  let signatories = pfield @"signatories" # ctxF.txInfo
  infoF <- pletFieldsC @'["inputs", "outputs", "signatories", "data"] ctxF.txInfo
  sigs :: Term _ (PBuiltinList (PAsData PPubKeyHash)) <- pletC infoF.signatories 
  -- allInputs <- pletC infoF.inputs
  let ownInput = ptryOwnInput # infoF.inputs # ownRef      -- so this actually implicitly is only 1 possible. -- cause each SCript UTXO calls validator each time separately
  -- ownInput - PTxInInfo (Term s (PDataRecord '["outRef" := PTxOutRef, "resolved" := PTxOut]))
  --     Also ownInput should only be 1 and that should have the datum. 
  --     it can have other UTXO not own which are Beneficiary's for collateral eg.
  ownInputF <- pletFieldsC @'["value", "address", "datum"] ownInput 
  let outDatum = pfromPDatum @PDat # (ptryFromInlineDatum # ownInputF.datum)
  datumOnUtxo <- pletFieldsC @'["beneficiary", "deadline", "aCurrency", "aToken", "targetAmount", "actualtargetAmountsoFar", "contributorsMap"] outDatum 
     -- we need to check that the value has Datum
  pure $
      -- pif
        -- (pvalueOf # ownInputF.value # datumOnUtxo.aCurrency # datumOnUtxo.aToken #== 1)
        (pmatch redeemer $ \case
          PClose _ -> 
            pif 
              -- validation#15 
              (pelem # (datF.beneficiary) #  sigs ) -- signatories ) -- pfromData signatories)
              -- (pvalueOf # ownInputF.value # datumOnUtxo.aCurrency # datumOnUtxo.aToken #== 1)
              ( pif 
                  -- validation#14 
                  -- datumOnUtxo checks the datum on UTXO and compares the TargetAmount and what ActualTargetAmountSoFar
                  -- datF is what's passed to validator in Datum
                  (pfromData datF.actualtargetAmountsoFar #>= ( pfromData datF.targetAmount) #&&
                    pfromData datumOnUtxo.actualtargetAmountsoFar #>= ( pfromData datumOnUtxo.targetAmount) #&& 
                       pfromData datumOnUtxo.actualtargetAmountsoFar #>= ( pfromData datumOnUtxo.actualtargetAmountsoFar) )
                  ( pif 
-- --                  validation#13
--                   --  traceIfFalse "UTXO being spend values are not matching based on Datum" correctInputValueClose
--                   --          Need to get the Datum's actual contribution and match it to the value also.
                      ( (pvalueOf # ownInputF.value # datumOnUtxo.aCurrency # datumOnUtxo.aToken #== 1)   #&&
                        (plovelaceValueOf # ownInputF.value  #== datumOnUtxo.actualtargetAmountsoFar) )    -- Curr Symbol and Token qty is only 1
                      (pconstant ())
                      (ptraceError "Input UTXO values are not equal to Datum actual target amount so far")
                  )
                  (ptraceError "targetAmount not reached on the Datums")
              )
              (ptraceError "Beneficiary signature not correct")
          PContribute pc -> 
            perror
        )
        -- perror
            -- pif 
            --   -- validation#15  
            --   (pelem # (datF.beneficiary) #  sigs ) -- signatories ) -- pfromData signatories)
            -- --   -- (pelem # ph # sigs)
            -- --   -- ((plength # sigs) #== 0)   -- fail
            -- --   -- ((plength # (pfromData signatories)) #== 1)   -- succeeded finally!
            -- --   -- ((phead # sig) #== ph)
            --   ( pif 
            --     -- validation#14 
            --     -- datumOnUtxo checks the datum on UTXO and compares the TargetAmount and what ActualTargetAmountSoFar
            --     -- datF is what's passed to validator in Datum
                -- (pfromData datF.actualtargetAmountsoFar #>= ( pfromData datF.targetAmount) #&&
                --     pfromData datumOnUtxo.actualtargetAmountsoFar #>= ( pfromData datumOnUtxo.targetAmount) #&& 
                --        pfromData datumOnUtxo.actualtargetAmountsoFar #>= ( pfromData datumOnUtxo.actualtargetAmountsoFar) )
        
-- --                validation#13
--                   -- traceIfFalse "UTXO being spend values are not matching based on Datum" correctInputValueClose
--                   --          Need to get the Datum's actual contribution and match it to the value also.
--                 ( pif
--                     ( (pvalueOf # ownInputF.value # datumOnUtxo.aCurrency # datumOnUtxo.aToken #== 1)   #&&
--                         (plovelaceValueOf # ownInputF.value  #== datumOnUtxo.actualtargetAmountsoFar) )    -- Curr Symbol and Token qty is only 1
        
--                     -- Validation 16 -- make sure only Ada and oue CrowdFund token is there and nothing else. 
--                     --    we dont want 1000 other tokens to be deposited etc
--                     ( pif
--                       (pvalueOf # ownInputF.value  #== 1)
--                       (pconstant ())
--                       (ptraceError "Currency Symb and Token qty not equal to 1") 
--                     )
--                     (ptraceError "Currency Symb and Token qty not equal to 1") -- perror -- (ptraceError "x shouldn't be 10")
--                 )
--                 -- 
--                 (ptraceError "targetAmount not reached on the Datums") -- perror -- (ptraceError "x shouldn't be 10")
--               )
--               -- (pconstant ())
--               perror -- (pconstant ()) -- perror --  -- perror




-- -- {-# INLINABLE crowdValidator #-}
-- pcrowdValidator :: Term s  (PDat :--> PRedeem :--> PScriptContext :--> PUnit)
-- -- pcrowdValidator :: Term s PValidator 
-- pcrowdValidator = phoistAcyclic $ plam $ \dat redeemer ctx -> unTermCont $ do 
--   ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
--   PSpending ((pfield @"_0" #) -> ownRef) <- pmatchC ctxF.purpose
--   infoF <- pletFieldsC @'["inputs", "outputs", "signatories", "data"] ctxF.txInfo
--   -- inputsAll <- pconstant infoF.inputs
--   let ownInput = ptryOwnInput # infoF.inputs # ownRef 
--   ownInputF <- pletFieldsC @'["value", "address"] ownInput   
--   -- sigs :: Term _ (PBuiltinList (PAsData PPubKeyHash)) <- pletC infoF.signatories
--   let signatories = pfield @"signatories" # ctxF.txInfo
--   datF <- pletFieldsC @'["beneficiary", "deadline", "aCurrency", "aToken", "targetAmount", "actualtargetAmountsoFar", "contributorsMap" ] dat
--   -- in popaque $ psignedByBeneficiary # ourDatum # ourRedeemer # ctx 
   
--   pure $
--     pif
--         (pmatch redeemer $ \case
--           PClose _ -> 
--             -- get the beneficiary signatory in signatories list
--             pif 
--               (pelem # datF.beneficiary # (pfromData signatories))
--               -- (pconstant ())
--               (pconstant True)
--               (pconstant True) -- perror
--           PContribute _ ->

-- -- --    validation#1     - First haskell in comments
-- --       traceIfFalse "wrong input value" ( correctInputValue d )       -- NEED TO ADD Policy id cannot be blank.
--       -- correctInputValue :: Dat -> Bool
--       -- correctInputValue dt = checkInputFound getAllValuesTxIns (tokenValue <> Ada.lovelaceValueOf  (( amountInDatum (contributorsMap dt) )) <> minAda) (totalValueDatumTxin )  
-- --          This validates 3 parameters to be equal 
-- --             1st parameter - from actual Tx-ins Values , validates that the UTXO with NFT in its Values - bypasses other Tx-in w/o NFT like Fees Tx-in
-- --             2nd parameter - Values constructed based on Datum passed to the Validator
-- --             3rd parametr - Values constructed from Datum at the UTXO. 
--             -- (pfilter # plam (\x -> pelem # x # datF.approvedSignatories)  


--             (pconstant True) -- perror
--         )
--     -- pif 
--     --   (pmatch redeemer $ \case
--     --       Close _ -> 
--     --         pconstant True
--     --       Contribute _ ->
--     --         perror
--     --   )
--         (pconstant () )
--         (pconstant () ) -- perror


-- Haskell 
--          && traceIfFalse "Not signed by beneficiary" signedByBeneficiary
      -- signedByBeneficiary :: Bool
      -- signedByBeneficiary = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (beneficiary d)



-- pcrowdValidatorW :: ClosedTerm (PValidator)
-- pcrowdValidatorW = plam $ \datum redeemer ctx -> unTermCont $ do 
--   -- (dat, _) <- ptryFromC @PDat datum 
--   -- dat <- ptryFromC @PDat datum
--   (redmr, _) <- ptryFromC @PRedeem redeemer 
--   lctx <- pletFields @["txInfo", "purpose"] ctx
--   datF <- pletFieldsC @'["beneficiary", "deadline", "aCurrency", "aToken", "targetAmount", "actualtargetAmountsoFar", "contributorsMap" ] datum
--   -- PSpending _ <- pmatch ctx.purpose
--   let signatories = pfield @"signatories" # lctx.txInfo
--   pif
--     (pelem # datF.signatories # pfromData signatories)
--     -- Success!
--     (pconstant ())
--     -- Signature not present.
--     perror
--   -- pure $ popaque $ pcrowdValidator # dat # redmr # ctx

pcrowdValidatorW :: ClosedTerm (PAsData PPubKeyHash :--> PValidator)
-- pcrowdValidatorW :: ClosedTerm (PValidator)
-- pcrowdValidatorW = plam $ \datum redeemer ctx' -> unTermCont $ do 
pcrowdValidatorW = plam $ \ph datum redeemer ctx' -> unTermCont $ do 
  (dat, _) <- ptryFromC @PDat datum 
  (redmr, _) <- ptryFromC @PRedeem redeemer 
  -- pure $ popaque $ pcrowdValidator # dat # redmr # ctx'
  pure $ popaque $ pcrowdValidatorTest # ph # dat # redmr # ctx'



-- getAllInputs :: Term s (PList PTxInInfo :-->  PList PTxOut)
-- -- pluList2 = pmap # (plam $ \x -> x + x3) # (pluList)
-- getAllInputs = phoistAcyclic $
--     plam $ \inp -> 
--       pmap # (\self x -> pletFields @'["resolved"] x) # inp
-- getAllInputs = phoistAcyclic $
--   plam $ \inputs -> pmap # (\res -> res.resolved)  # (pmap # \inp -> pletFields @'["outRef", "resolved"] inp # inputs)
-- --     precList (\self x xs -> pletFields @'["outRef", "resolved"] x $ \txInFields -> pif (ownRef #== txInFields.outRef) txInFields.resolved (self # xs)) (const perror) # inputs



--------------------------------------------------------------------------------------------------------------

--------------------------------------------------------------- contextBuilding for Unit testing----



ptryOwnInput :: (PIsListLike list PTxInInfo) => Term s (list PTxInInfo :--> PTxOutRef :--> PTxOut)
ptryOwnInput = phoistAcyclic $
  plam $ \inputs ownRef ->
    precList (\self x xs -> pletFields @'["outRef", "resolved"] x $ \txInFields -> pif (ownRef #== txInFields.outRef) txInFields.resolved (self # xs)) (const perror) # inputs
-- since only 1 Script UTXO can be calling Validator each time -- here at most we only get 1 match. 

-- precList :: PIsListLike list a => (Term s (list a :--> r) -> Term s a -> Term s (list a) -> Term s r) -> (Term s (list a :--> r) -> Term s r) -> Term s (list a :--> r)
-- Like pelimList, but with a fixpoint recursion hatch.
-- preclist is travserses list recursively and for each element you can so whatever - here we do If stement to get TxOut for the ownRef. 

-- preclist breakDown - 
-- Takes "inputs" list, and recursivley strips first element and rest  (\self x xs)
--    So each element in inputs list is TxOutRef and TxOut     (PTxInInfo (Term s (PDataRecord '["outRef" := PTxOutRef, "resolved" := PTxOut])))
--    we are interested in only TxOutRef
--    So from the `x` we get the "outRef" and "resolved" with pletFields and pass that already evaluated fields into Lambda variable `txInFields`
--    Then we take only txInFields.outRef since we want to match that for what is in the ScriptPurpose ownRef that was passed to this function.
--    we recursively try to find - if we find it, then we take the txInFields.resolved which has the UTXO info of Value, datum, address.
--    If we dont find anything we return (const perror)


-- pAsDatadatumCrowd = PlutusTx.toData datumCrowd


-- correctPkh :: PubKeyHash
-- correctPkh = "0e97aa033ceee762c25285cdcc94287178f01b45a585fd0d4da8387a"

-- sampleCtx :: ScriptContext 
-- sampleCtx = ScriptContext 
--   (TxInfo [] [] [] mempty mempty [] Map.empty (interval (POSIXTime 1) (POSIXTime 2)) [correctPkh] Map.empty Map.empty "")
--   $ Spending (TxOutRef "" 0)




data Dat = Dat 
    {
        beneficiary :: LedgerApiV2.PubKeyHash
        , deadline :: LedgerApiV2.POSIXTime
        , aCurrency :: LedgerApiV2.CurrencySymbol
        , aToken    :: LedgerApiV2.TokenName
        , targetAmount :: Integer
        , actualtargetAmountsoFar :: Integer
        , contributorsMap :: [(LedgerApiV2.PubKeyHash, Integer)] 
    } 
    deriving Show
PlutusTx.unstableMakeIsData ''Dat

--------------------------------------------------------------------
-- Some Helper functions
-- --------------------------------------------------------------------
-- decodeHex :: B.ByteString -> PlutusPrelude.BuiltinByteString
-- decodeHex hexBS =
--     case getTx of
--         Right decHex -> do
--             PlutusPrelude.toBuiltin(decHex)  
--         Left _ -> do
--             PlutusPrelude.emptyByteString 
--     where
--         getTx :: Either String B.ByteString 
--         getTx = B16.decode hexBS

-- convertToPubKeyHash :: B.ByteString -> LedgerApiV2.PaymentPubKeyHash
-- convertToPubKeyHash b = LedgerApiV2.PaymentPubKeyHash (Ledger.PubKeyHash $ decodeHex b)

targetAmount :: Integer
targetAmount = 50000000   -- 50 Ada

beneficiaryHash :: LedgerApiV2.PubKeyHash    --- B.ByteString
beneficiaryHash = "0e97aa033ceee762c25285cdcc94287178f01b45a585fd0d4da8387a"  -- Beneficiary wallet


pubKeyHashTest1 :: Term s PPubKeyHash
pubKeyHashTest1 = pconstant beneficiaryHash

-- pubKeyHashAsData :: Term s (PAsData PPubKeyHash)
-- pubKeyHash = pdata pconstant beneficiaryHash

contributor1 :: LedgerApiV2.PubKeyHash
contributor1 = "0d29d2f72ba11f3381783dda5501139f397d81e83244fce13e7a711a" 

crowdDeadline :: LedgerApiV2.POSIXTime
crowdDeadline = 1671159023000    -- Thursday, December 15, 2022 6:50:23 PM

dCurrencySymbol :: LedgerApiV2.CurrencySymbol
-- dCurrencySymbol = "88d4e1abfbcd08ace98f41a1a514e84239703c0ab5e5feb61f029eed"
dCurrencySymbol = "b7047182a00354f8c4cd7b01c2faab230e01d2f33a6dcfd0c781f7ec"
dToken    :: LedgerApiV2.TokenName
dToken = "MyCrowdFund"

datumCrowd :: Dat
datumCrowd = Dat { 
                             beneficiary =  beneficiaryHash
                           , deadline = crowdDeadline
    -- https://preview.cardanoscan.io/token/d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be4d7943726f776446756e64?tab=transactions
                           -- , OnChain.aCurrency = "d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be"
                           , aCurrency = dCurrencySymbol
                           , aToken = dToken
                           , targetAmount = targetAmount
                           , actualtargetAmountsoFar = 20000000
                           , contributorsMap = []}


-- validation 13 a - Success
datumCrowdVal13aClose :: Dat
datumCrowdVal13aClose = Dat { 
                             beneficiary =  beneficiaryHash
                           , deadline = crowdDeadline
    -- https://preview.cardanoscan.io/token/d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be4d7943726f776446756e64?tab=transactions
                           -- , OnChain.aCurrency = "d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be"
                           , aCurrency = dCurrencySymbol
                           , aToken = dToken
                           , targetAmount = targetAmount
                           , actualtargetAmountsoFar = 72000000      -- 72 Ada so target reached.
                           , contributorsMap = []}



-- validation 14 a - Success
datumCrowdVal14aClose :: Dat
datumCrowdVal14aClose = Dat { 
                             beneficiary =  beneficiaryHash
                           , deadline = crowdDeadline
    -- https://preview.cardanoscan.io/token/d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be4d7943726f776446756e64?tab=transactions
                           -- , OnChain.aCurrency = "d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be"
                           , aCurrency = dCurrencySymbol
                           , aToken = dToken
                           , targetAmount = targetAmount
                           , actualtargetAmountsoFar = 70000000      -- 70 Ada so target reached.
                           , contributorsMap = []}

-- validation 14 a - Failure
datumCrowdVal14aCloseFail :: Dat
datumCrowdVal14aCloseFail = Dat { 
                             beneficiary =  beneficiaryHash
                           , deadline = crowdDeadline
    -- https://preview.cardanoscan.io/token/d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be4d7943726f776446756e64?tab=transactions
                           -- , OnChain.aCurrency = "d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be"
                           , aCurrency = dCurrencySymbol
                           , aToken = dToken
                           , targetAmount = targetAmount
                           , actualtargetAmountsoFar = 10000000      -- 10 Ada so target NOT reached.
                           , contributorsMap = []}

-- validation 15 a - Failure
datumCrowdVal15aCloseFail :: Dat
datumCrowdVal15aCloseFail = Dat { 
                             beneficiary =  beneficiaryHash
                           , deadline = crowdDeadline
    -- https://preview.cardanoscan.io/token/d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be4d7943726f776446756e64?tab=transactions
                           -- , OnChain.aCurrency = "d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be"
                           , aCurrency = dCurrencySymbol
                           , aToken = dToken
                           , targetAmount = targetAmount
                           , actualtargetAmountsoFar = 70000000      -- 70 Ada so target reached.
                           , contributorsMap = []}

datumCrowdBuiltin = PlutusTx.toBuiltinData datumCrowd
datumCrowdVal14aCloseBuiltin = PlutusTx.toBuiltinData datumCrowdVal14aClose


datumCrowdVal13aCloseBuiltin = PlutusTx.toBuiltinData datumCrowdVal13aClose

datumCrowdToData = PlutusTx.toData datumCrowd

-- datumCrowdToDataHash = datumHash datumCrowdToData

-- datum1 ::  BuiltinData -> Maybe Datum
-- datum1 b = case  (fromBuiltinData b) of
--     Just dt -> Just dt
--     Nothing -> Nothing

-- myMap :: Map DatumHash (Maybe Datum)
-- myMap = case (fromBuiltinData datumCrowdBuiltin) of
--   Just datum -> Map.singleton (getDatumHash datum) datum
--   Nothing -> Map.empty
-- -- Just datum1 = fromBuiltinData datumCrowdBuiltin

data Redeem = Contribute 
    {
        contribution :: (LedgerApiV2.PubKeyHash,Integer)
    } 
              | Close 
    deriving Show

PlutusTx.unstableMakeIsData ''Redeem

contributorAmount :: Integer
contributorAmount = 30000000   -- 30 Ada contribution

redeemCrowdContribute :: Redeem
redeemCrowdContribute = Contribute {  
                             contribution = (contributor1, contributorAmount)
                            }

redeemCrowdContributeBuiltin = PlutusTx.toBuiltinData redeemCrowdContribute

redeemCrowdClose :: Redeem
redeemCrowdClose = Close

redeemCrowdCloseBuiltin = PlutusTx.toBuiltinData redeemCrowdClose


myDatum1Map :: Map DatumHash Datum
-- -- myEmptyMap = Data.Map.empty
myDatum1Map = fromList [(DatumHash "datumCrowd", Datum datumCrowdBuiltin)]

datumCrowdVal14aMap :: Map DatumHash Datum
datumCrowdVal14aMap = fromList [(DatumHash "datumCrowd", Datum datumCrowdVal14aCloseBuiltin)]

datumCrowdVal13aMap :: Map DatumHash Datum
datumCrowdVal13aMap = fromList [(DatumHash "datumCrowd", Datum datumCrowdVal13aCloseBuiltin)]


myRedeemer1Map :: Map ScriptPurpose Redeemer
myRedeemer1Map = fromList [((Spending (TxOutRef "" 1), Redeemer redeemCrowdCloseBuiltin))]

myStake1Map :: Map StakingCredential Integer
myStake1Map = fromList [ ( StakingPtr 0 0 0 , 1)]



-- Our Curr Symbol and Token
mapValueCsTk :: Map CurrencySymbol (Map TokenName Integer)
mapValueCsTk = fromList [(dCurrencySymbol, tokenMap)]

tokenMap :: Map TokenName Integer
tokenMap = fromList [(dToken, 1)]

pint1 :: Term s PInteger
pint1 = pconstant 1

-- -- valueCsTk1 :: PValue
-- valueCsTk1 :: 
--   forall 
--     (keys :: KeyGuarantees)
--     (amounts :: AmountGuarantees)
--     (s :: S). 
--   Term s ( PValue keys amounts )

-- valueCsTk1 = PValue crowdCurrSymb crowdTokenName pint1

-- valueCsTk1 = PValue SmapValueCsTk


scripAddr1 :: BuiltinByteString
--addr1 = "43726f776446756e6441646472657373"  -- hex of "CrowdFundAddress"
scripAddr1 = "addr_test1wp02taqyn6rp38g4wqn7h5sxccgwkdzex9cegexxsny4qlczfn2al"    -- CrowdFund Address


payAddr1 :: BuiltinByteString
payAddr1 = "addr_test1vq8f02sr8nhwwckz22zumny59pch3uqmgkjctlgdfk5rs7sx52ldh"    -- Beneficiary Address


addrStkCred :: Maybe StakingCredential
addrStkCred = Nothing

-- scripValHash :: ValidatorHash
-- scripValHash = ValidatorHash scripAddr1

payValHash :: PubKeyHash
payValHash = PubKeyHash payAddr1
-- 0e97aa033ceee762c25285cdcc94287178f01b45a585fd0d4da8387a

scrCred :: LedgerApiV2.Credential
-- scrCred = LedgerApiV2.ScriptCredential scripValHash
scrCred = LedgerApiV2.ScriptCredential "xyzScript"

pubKeyCred :: LedgerApiV2.Credential
pubKeyCred = LedgerApiV2.PubKeyCredential payValHash

crAddress1 :: LedgerApiV2.Address
crAddress1 = LedgerApiV2.Address { 
    LedgerApiV2.addressCredential = scrCred,
    LedgerApiV2.addressStakingCredential = Nothing
}

pyAddress1 :: LedgerApiV2.Address
pyAddress1 = LedgerApiV2.Address { 
    LedgerApiV2.addressCredential = pubKeyCred,
    LedgerApiV2.addressStakingCredential = Nothing
}

minAda :: LedgerApiV2.Value
-- minAda = toValue 2000000  
-- minAda = lovelaceValueOf 20000000
minAda = PlutusLedgerApi.V2.singleton adaSymbol adaToken 2000000


-- mapValueMinAda :: Map CurrencySymbol (Map TokenName Integer)
-- mapValueMinAda = fromList [("", pMinAda)]

-- pMinAda :: Map TokenName Integer
-- pMinAda = fromList [("ADA", 2000000)]

-- pValueMinAda = PValue mapValueMinAda
-- pMinAda :: Pvalue
-- pMinAda = PlutusLedgerApi.V2.singleton adaSymbol adaToken 2000000

-- minAdaPint = pvalueOf # mapValueMinAda # "" # "ADA"

ada30 :: LedgerApiV2.Value
-- ada30 = toValue 30000000
-- ada30 = lovelaceValueOf 30000000
ada30 = PlutusLedgerApi.V2.singleton adaSymbol adaToken 30000000


ada70 :: LedgerApiV2.Value
ada70 = PlutusLedgerApi.V2.singleton adaSymbol adaToken 70000000


v2 =  ada70 <> minAda 

valueCsTk :: LedgerApiV2.Value
-- ada30 = toValue 30000000
-- ada30 = lovelaceValueOf 30000000
valueCsTk = PlutusLedgerApi.V2.singleton dCurrencySymbol dToken 1

-- -- Validation#14A -
-- -- this test will have tx-in 2 ada, output Datum 32 Ada - so 1 contributor 
crTxOutVal4Test1OutForInput :: LedgerApiV2.TxOut
crTxOutVal4Test1OutForInput = LedgerApiV2.TxOut { LedgerApiV2.txOutAddress = crAddress1,
             LedgerApiV2.txOutDatum  = (rawDatToOutputDatum datumCrowdVal14aClose), 
             LedgerApiV2.txOutReferenceScript = Nothing,
             LedgerApiV2.txOutValue = valueCsTk <> ada30 <> minAda   

}



crTxOutVal13aTest1OutForInput :: LedgerApiV2.TxOut
crTxOutVal13aTest1OutForInput = LedgerApiV2.TxOut { LedgerApiV2.txOutAddress = crAddress1,
             LedgerApiV2.txOutDatum  = (rawDatToOutputDatum datumCrowdVal13aClose), 
             LedgerApiV2.txOutReferenceScript = Nothing,
             LedgerApiV2.txOutValue =  ada70 <> minAda <> valueCsTk 

}


txInVal13aScriptUtxo :: LedgerApiV2.TxInInfo
txInVal13aScriptUtxo = LedgerApiV2.TxInInfo {
           LedgerApiV2.txInInfoOutRef = lTxOutRef,
           LedgerApiV2.txInInfoResolved = crTxOutVal13aTest1OutForInput
}

txInVal14aScriptUtxo :: LedgerApiV2.TxInInfo
txInVal14aScriptUtxo = LedgerApiV2.TxInInfo {
           LedgerApiV2.txInInfoOutRef = lTxOutRef,
           LedgerApiV2.txInInfoResolved = crTxOutVal4Test1OutForInput
}

lTxOutRef :: LedgerApiV2.TxOutRef
lTxOutRef = LedgerApiV2.TxOutRef {
    LedgerApiV2.txOutRefId = lId,
      LedgerApiV2.txOutRefIdx = (1 :: Integer)
}

lId :: LedgerApiV2.TxId 
lId = LedgerApiV2.TxId {
    LedgerApiV2.getTxId = lGetTxId
}

lGetTxId ::  BuiltinByteString
lGetTxId = "sampleTestTxId"



-- -- Validation#4
-- -- this test will have tx-in 2 ada, output Datum 32 Ada - so 1 contributor 
-- crTxOutVal4Test1Out :: V2LedgerTx.TxOut
-- crTxOutVal4Test1Out = V2LedgerTx.TxOut { V2LedgerTx.txOutAddress = crAddress1,
--              V2LedgerTx.txOutDatum  = (rawDatToOutputDatum cFDatumRawVal4TxOut), 
--              V2LedgerTx.txOutReferenceScript = Nothing,
--              V2LedgerTx.txOutValue = singleTonCF <> ada30 <> minAda   --- valueCsTk

-- }




cFdatum1 :: BuiltinData -> LedgerApiV2.Datum
cFdatum1 bd = LedgerApiV2.Datum { LedgerApiV2.getDatum = bd}

cfDatumOutputDatum :: LedgerApiV2.Datum -> LedgerApiV2.OutputDatum
cfDatumOutputDatum dt = LedgerApiV2.OutputDatum  dt

cFDatum1BuiltInData :: Dat -> BuiltinData
cFDatum1BuiltInData rd = (PlutusTx.toBuiltinData rd)

-- Function - takes Raw Dat and returns OutputDatum
rawDatToOutputDatum :: Dat -> LedgerApiV2.OutputDatum
rawDatToOutputDatum rd = cfDatumOutputDatum (cFdatum1 (cFDatum1BuiltInData rd))



-- Validation 13 - Success - 
mockCtxVal13a :: ScriptContext
mockCtxVal13a =
  ScriptContext
    (TxInfo
      [txInVal13aScriptUtxo]                              -- input
      mempty                                             -- referenceInputs
      mempty                                             -- outputs
      mempty                                             -- fee
      mempty                                             -- mint
      mempty                                             -- dcert
      myStake1Map                                        -- wdrl   
      (interval (POSIXTime 1) (POSIXTime 2))             -- validRange
      [beneficiaryHash]                                  -- signatories - txInfoSignatories
      myRedeemer1Map                                     -- redeemers --- txInfoRedeemers
      datumCrowdVal13aMap                                        -- datums
      ""                                                 -- id 
    )
    -- (Spending (TxOutRef "" 1))
    (Spending ( lTxOutRef))





-- Validation 14 - Failure - target amount not reached.
mockCtxVal14a :: ScriptContext
mockCtxVal14a =
  ScriptContext
    (TxInfo
      [txInVal14aScriptUtxo]                              -- input
      mempty                                             -- referenceInputs
      mempty                                             -- outputs
      mempty                                             -- fee
      mempty                                             -- mint
      mempty                                             -- dcert
      myStake1Map                                        -- wdrl   
      (interval (POSIXTime 1) (POSIXTime 2))             -- validRange
      [beneficiaryHash]                                  -- signatories - txInfoSignatories
      myRedeemer1Map                                     -- redeemers --- txInfoRedeemers
      datumCrowdVal14aMap                                        -- datums
      ""                                                 -- id 
    )
    -- (Spending (TxOutRef "" 1))
    (Spending ( lTxOutRef))








--- Changing to V2
mockCtxV2 :: ScriptContext
mockCtxV2 =
  ScriptContext
    (TxInfo
      -- mempty                                             -- inputs 
      [txInVal14aScriptUtxo]                              -- input
      mempty                                             -- referenceInputs
      mempty                                             -- outputs
      mempty                                             -- fee
      mempty                                             -- mint
      mempty                                             -- dcert
      myStake1Map                                        -- wdrl   
      (interval (POSIXTime 1) (POSIXTime 2))             -- validRange
      [beneficiaryHash]                                  -- signatories - txInfoSignatories
      myRedeemer1Map                                     -- redeemers --- txInfoRedeemers
      myDatum1Map                                        -- datums
      ""                                                 -- id 
    )
    -- (Spending (TxOutRef " " 1))
    (Spending ( lTxOutRef))

-- THis was V1
-- mockCtx :: ScriptContext
-- mockCtx =
--   ScriptContext
--     (TxInfo
--       mempty                                                                                        -- txInfoInputs 
--       mempty                                                                                        -- txInfoOutputs txInfoReferenceInputs  (not on V1)
--       mempty                                                                                        -- txInfoFee
--       mempty                                                                                        -- txInfoMint
--       mempty                                                                                        -- txInfoDCert
--       mempty                                                                                        -- txInfoWdrl
--       -- mempty                                                                                        --    -- I added this
--       (interval (POSIXTime 1) (POSIXTime 2))                                                        -- txInfoValidRange
--     --   [fromString hashStr, "f013", "ab45"]
--     --   ["abce0f123e", "f013", "ab45"]
--     --   ["0e97aa033ceee762c25285cdcc94287178f01b45a585fd0d4da8387a", "f013", "ab45"]
--       -- [beneficiaryHash, "f013", "ab45"]    --- this works too. So did not need `fromString`
--       [beneficiaryHash]    --- this works too. So did not need `fromString`                         -- signatories - txInfoSignatories
--       mempty                                                                                        -- datums --- txInfoRedeemers
--       ""                                                                                            -- id 
--     )
--     (Spending (TxOutRef "" 1))


-- evalWithArgsT (pcrowdValidatorW) [PlutusTx.toData datumCrowdBuiltin, PlutusTx.toData redeemCrowdContributeBuiltin, PlutusTx.toData mockCtx]
-- ghci> evalWithArgsT (pcrowdValidatorW) [PlutusTx.toData datumCrowdBuiltin, PlutusTx.toData redeemCrowdContributeBuiltin, PlutusTx.toData mockCtx]
-- Left "An error has occurred:  User error:\nThe machine terminated because of an error, either from a built-in function or from an explicit use of 'error'.\nCaused by: 
-- (unIData #d8799f581c0d29d2f72ba11f3381783dda5501139f397d81e83244fce13e7a711a1a01c9c380ff)"


-- evalWithArgsT (pcrowdValidatorW) [PlutusTx.toData datumCrowdBuiltin, PlutusTx.toData redeemCrowdCloseBuiltin, PlutusTx.toData mockCtx]

-- evalWithArgsT (pcrowdValidatorTest # pubKeyHash) [PlutusTx.toData datumCrowdBuiltin, PlutusTx.toData redeemCrowdCloseBuiltin, PlutusTx.toData mockCtxV2]




-- evalWithArgsT (pcrowdValidatorW # (pdata pubKeyHashTest1)) [PlutusTx.toData datumCrowdBuiltin, PlutusTx.toData redeemCrowdCloseBuiltin, PlutusTx.toData mockCtx]

-- V2
-- evalWithArgsT (pcrowdValidatorW # (pdata pubKeyHashTest1)) [PlutusTx.toData datumCrowdBuiltin, PlutusTx.toData redeemCrowdCloseBuiltin, PlutusTx.toData mockCtxV2]

--------------------------------------------------------------------------------------------------------------

--------------------------------------------------------------- simple TESTING plutarch while learning--------------------------------------
--------------------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------------------


-- https://plutonomicon.github.io/plutarch-plutus/Typeclasses/PIsDataRepr%20and%20PDataFields#implementing-pisdatarepr-and-friends
currSym :: Term s PCurrencySymbol
currSym = pconstant $ LedgerApiV2.CurrencySymbol "foo"

fields :: Term _ (PDataRecord '[ "_0" ':= PCurrencySymbol ])
fields = pdcons # (pdata currSym) # pdnil


x3 :: Term s PInteger
x3 = pconstant 3

-- x3ToData = toData x3

liftedX3 :: Integer
liftedX3 = plift x3

doubleH :: Integer -> Integer
doubleH i = i * 2

pluDoubleH :: Term s ( PInteger :--> PInteger )
pluDoubleH =  phoistAcyclic $ 
  plam $ \i -> 
    i * 2


doubleX3 =  doubleH (liftedX3)
-- ghci> doubleX3
-- 6

-- doubleX32 :: Term s PInteger -> Integer
-- doubleX32 x =  doubleH (plift x)

-- ghci> doubleH (liftedX3)
-- 6

-- funcPinToInt :: Term s PInteger -> Integer
-- funcPinToInt pi = (plift pi)

pluList :: Term s (PList PInteger)
pluList = pcons # 2 #$ pcons # 6 #$ pcons # 12 #$ pcons # x3 # pnil


func123 :: Term s  ( PInteger :--> ((PList PInteger)) :--> (PInteger))
func123  = phoistAcyclic $ 
  pfix #$ plam (\self pfi pl ->
    pmatch pl $ \case
      PSCons i pls -> 
        pif (i #== pfi) 
          pfi 
          (self # pfi # pls)
      PSNil -> 0
    )


-- func234 :: (Term s PInteger) :--> (Term s (PList PInteger)) :--> PInteger
-- func234  = phoistAcyclic $ 
--   pfix #$ plam (\self pfi pl ->
--     pmatch pl $ \case
--       PSCons i pls -> 
--         pif (i #== pfi) 
--           pfi 
--           (self # pfi # pls)
--       PSNil -> 0
--     )


-- pbool1 = pelimList # func123 # x3 # pluList

pluNullCheck = pnull # pluList
-- ghci> plift pluNullCheck 
-- False
pluLen = plength # pluList
-- ghci> plift pluLen
-- 3

hpList :: [Term s PInteger]
hpList = [x3, x3, x3]

-- pluToHList ::[ Term s PInteger] -> [Integer]
-- pluToHList [] = []
-- pluToHList [x] = [(plift x)]
-- pluToHList (x:xs) = (plift x) : pluToHList xs

headHpList = head hpList

-- pluToHlist = fmap (plift) hpList

hList :: [Integer]
hList = [1, 2, 3]

hToHList :: [Integer] -> [Integer]
hToHList [] = []
hToHList [x] = [x * x]
hToHList (x:xs) = (x* x) : hToHList xs



-- ghci> hToHList hList
-- [1,4,9]


-- pluToHaskList :: Term s (PList PInteger) :: [Term s PInteger]
-- pluToHaskList = pfix #$ plam f
--   where
--     f :: Term s (PInteger :--> PInteger) -> Term s PInteger -> Term s PInteger
--     f self n = pif (n #== 1) n $ n * (self #$ n - 1)




-- hList :: Term s (PList PInteger) :--> [Integer]
-- hList = phoistAcyclic $ plam $ \plist2612 ->
--   pmatch plist2612 $ \case
--     precList
--       (\self x xs) ->
--           pmatch x \case
--             PCons pi -> lift pi
--             PNil -> 0
--             (self # xs)
--       )
--       (const perror)
--       # pto  plist2612

-- psingletonTokenNameWithCS = phoistAcyclic $ plam $ \policyId val ->
--   pmatch val $ \(PValue val') ->
--     precList 
--       (\self x xs ->
--         pif 
--           (pfstBuiltin # x #== policyId)
--           ( pmatch (pfromData (psndBuiltin # x)) $ \(PMap tokens) ->
--               plet (phead # tokens) $ \fstTk ->
--                 (pif 
--                   (pnull # (ptail # tokens) #&& pfromData (psndBuiltin # fstTk) #== 1)
--                   (pfromData $ pfstBuiltin # fstTk)
--                   perror 
--                 )
--           )
--           (self # xs)
--         )
--       (const perror)
--       # pto val'




list4 :: [Integer]
list4 = [1,2,3,4]

-- plenList4 :: [Integer] :--> PInteger
-- plenList4 = plength list4

-- someListFunc :: Terms s (PList PInterger) -> [Integer]
-- someListFunc li = 




pluHead = phead # pluList
x = plift pluHead
-- ghci> plift pluHead
-- 2


-- funcAdd :: Integer -> Integer 
-- funcadd i = i + 5

pluList2 :: Term s (PList PInteger)
pluList2 = pmap # (plam $ \x -> x + x3) # (pluList)
-- ghci> plift (phead # pluList2)
-- 5

-- lenP = pmap # (\x -> pdiv # x # 2) # (pluList)


pFeList :: forall s. Term s (PList PByteString)
pFeList = pcon $ PSCons (phexByteStr "fe") $ pcon PSNil
pFeListHead = phead # pFeList


l1 :: [Integer]
l1 = [1,2,3,4]
l2 = fmap (\y -> y * 3) (fmap (\x -> x + 10) l1)




-- psignedByBeneficiary :: Term s (POurDatum :--> POurRedeemer :--> PScriptContext :--> PUnit)
-- psignedByBeneficiary = phoistAcyclic $ plam $ \datum redeemer _ctx -> unTermCont $ do 

      -- signedByBeneficiary :: Bool
      -- signedByBeneficiary = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (beneficiary d)

alwaysSucceeds :: Term s (PAsData PDat :--> PAsData PRedeem :--> PAsData PScriptContext :--> PUnit)
alwaysSucceeds = plam $ \_datm _redm _ctx -> pconstant ()
-- pcrowdValidator = phoistAcyclic $ plam $ \datm redm ctx ->
--   (pconstant ())

-- pvalidateSmallChecks :: Term s (POurDatum :--> POurRedeemer :--> PScriptContext :--> PUnit)
-- pvalidateSmallChecks = phoistAcyclic $ plam $ \datum redeemer _ctx -> unTermCont $ do 
--     datumF <- pletFieldsC @'["password"] datum 
--     redeemF <- pletFieldsC @'["password"] redeemer 
--     pure $
--       pif ( (redeemF.password) #== datumF.password )
--       (pconstant ())
--       perror

-- emurgoValidator :: Term s (PCurrencySymbol :--> PDaoDatum :--> PDaoAction :--> PScriptContext :--> PUnit)
-- emurgoValidator = phoistAcyclic $ plam $ \stateCS dat redeemer ctx -> unTermCont $ do
--     ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx


-- alwaysSucceeds :: Term s (PAsData PDatum :--> PAsData PRedeemer :--> PAsData PScriptContext :--> PUnit)
-- alwaysSucceeds = plam $ \datm redm ctx -> pconstant ()



-- foo :: Maybe PInteger -> PInteger
-- foo mb@(Maybe PInteger) = mb (\x -> x + 42) 0


-- exampleFunction :: Term s (PInteger :--> PInteger)
-- exampleFunction = plam $ \x -> unTermCont $ do
--   x' <- plet $ x + 1 + 2 + 3
--   x' + x'

x' :: Term s PInteger
x' = pconstant 3

x'' :: Term s PInteger
x'' = pconstant 5

x''' = x'' + x'


-- scr1 :: Term s PScriptContext
-- ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx





-- crowdCurrSymbBuilt = pdata crowdCurrSymb


-- crowdName :: (Term s PByteString)
-- crowdName = pconstant "CrowdFundingToken"
-- crowdNameBuilt = pdata crowdName
-- ghci> plift crowdName
-- "CrowdFundingToken"



-- crowdTokenNameBuilt = pdata crowdTokenName

crowdNTokens :: Term s PInteger
crowdNTokens = pconstant 5
crowdNTokensBuilt = pdata crowdNTokens
-- keys1 :: KeyGuarantees
-- keys1 = Sorted

-- -- crowdPair = (PBuiltinPair crowdTokenName crowdNTokens)
-- crowd1 = PMap crowdTokenName crowdNTokens
-- crowd1 = ppairDataBuiltin # ( crowdTokenName)  # (pdata crowdNTokensBuilt)
-- crowd1 = ppairData # ( crowdTokenName)  # (pdata crowdNTokens)


-- pluList2 :: Term s (PList PInteger)
-- pluList2 = pmap # (plam $ \x -> x + x3) # (pluList)

-- built1 :: Term s (PBuiltinPair PInteger PInteger)
-- built1 =  (PBuiltinPair crowdNTokens crowdNTokens)

-- data PInteger s

-- pluList :: Term s (PList PInteger)
-- pluList = pcons # 2 #$ pcons # 6 #$ pcons # 12 # pnil





int2 :: Term s PInteger
int2 = pconstant 5

int3 = pconstantData @PInteger 3
int4 = pconstantData @PInteger 4

int5 = pconstantData @PInteger 5
int6 = pconstantData @PInteger 6

int7 = pconstantData @PInteger 7
int8 = pconstantData @PInteger 8


int9 = pconstantData @PInteger 9
int10 = pconstantData @PInteger 10

-- data PBuiltinPair (a :: PType) (b :: PType) (s :: S)
-- built2 :: Term s ( PInteger , PInteger)
-- built2 =  PBuiltinPair # int1 # int2

int1ToPas = pdata int1
built3 :: Term s (PBuiltinPair (PAsData PInteger) (PAsData PInteger))
built3 = ppairDataBuiltin # int3 # int4
fst3 = pfstBuiltin # built3
snd3 = psndBuiltin # built3


built56 :: Term s (PBuiltinPair (PAsData PInteger) (PAsData PInteger))
built56 = ppairDataBuiltin # int5 # int6


built78 :: Term s (PBuiltinPair (PAsData PInteger) (PAsData PInteger))
built78 = ppairDataBuiltin # int7 # int8

built910 :: Term s (PBuiltinPair (PAsData PInteger) (PAsData PInteger))
built910 = ppairDataBuiltin # int9 # int10

builtPairList :: forall s. Term s (PBuiltinList (PBuiltinPair (PAsData PInteger) (PAsData PInteger)))
builtPairList = pcon $ PCons built3 $ pcon PNil

-- join BuiltinPairs into a PList
pluListPair :: Term s (PList (PBuiltinPair (PAsData PInteger) (PAsData PInteger)))
pluListPair = pcons # built3 #$ pcons # built56 #$ pcons # built78 # pnil

-- join BuiltinPairs into a PBuiltinList
pluBuiltListPair :: Term s (PBuiltinList (PBuiltinPair (PAsData PInteger) (PAsData PInteger)))
pluBuiltListPair = pcons # built3 #$ pcons # built56 #$ pcons # built78 # pnil
-- ghci> evalT pluBuiltListPair
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf (DefaultUniApply DefaultUniProtoList 
-- (DefaultUniApply (DefaultUniApply DefaultUniProtoPair DefaultUniData) DefaultUniData)) [(I 3,I 4),(I 5,I 6),(I 7,I 8)]))}},
-- ExBudget {exBudgetCPU = ExCPU 1093112, exBudgetMemory = ExMemory 3192},[])


-- PMap
-- newtype PMap (keysort :: KeyGuarantees) (k :: PType) (v :: PType) (s :: S)
-- contructor - 
-- PMap (Term s (PBuiltinList (PBuiltinPair (PAsData k)  (PAsData v))))

-- kSort :: KeyGuarantees
-- kSort = Sorted

-- kSortPAs = pdata kSort

-- -- pmap1 :: PMap (keysort :: KeyGuarantees) (k :: PType) (v :: PType) (s :: S)





-- this will take Pbuiltinlist of Pair and extract lets says First part only puts it in PBuiltinList
extractPBuiltInList :: Term s ( (PBuiltinList (PBuiltinPair (PAsData PInteger) (PAsData PInteger))) :--> PBuiltinList (PAsData PInteger))
extractPBuiltInList = phoistAcyclic $
  pfix #$ plam (\self n ->
    pmatch n $ \case
      PCons x xs ->
        (pcons # (pfstBuiltin # x) # (self # xs))
      PNil -> pnil 
    -- pif (n #== PCons x xs ) (pcons # x) $ (self xs)
    --   pif (n #== PNil) (pnil) $
        
  )

-- this will take Pbuiltinlist of Pair and extract lets says First part only puts it in PBuiltinList
extractPBuiltInListGT5 :: Term s ( (PBuiltinList (PBuiltinPair (PAsData PInteger) (PAsData PInteger))) :--> PBuiltinList (PAsData PInteger))
extractPBuiltInListGT5 = phoistAcyclic $
  pfix #$ plam (\self n ->
    pmatch n $ \case
      PCons x xs -> 
        pif ( (pfromData (pfstBuiltin # x)) #> (pfromData int5))
          (pcons # (pfstBuiltin # x) # (self # xs))
          (self # xs)
      PNil -> pnil 
    -- pif (n #== PCons x xs ) (pcons # x) $ (self xs)
    --   pif (n #== PNil) (pnil) $
        
  )


fstPluBuiltListPair = extractPBuiltInList # pluBuiltListPair
-- ghci> evalT fstPluBuiltListPair 
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () 
-- (Some (ValueOf (DefaultUniApply DefaultUniProtoList DefaultUniData) [I 3,I 5,I 7]))}},
-- ExBudget {exBudgetCPU = ExCPU 5475608, exBudgetMemory = ExMemory 16704},[])


-- extract values in first element of pair greater than 5
fstPluBuiltListPairGT5 = extractPBuiltInListGT5 # pluBuiltListPair
-- ghci> evalT fstPluBuiltListPairGT5 
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () 
-- (Some (ValueOf (DefaultUniApply DefaultUniProtoList DefaultUniData) [I 7]))}},
-- ExBudget {exBudgetCPU = ExCPU 7771338, exBudgetMemory = ExMemory 22002},[])

-- pconsBuiltin :: Term s (a -> PBuiltinList a :--> PBuiltinList a)
-- pconsBuiltin = phoistAcyclic $ pforce $ punsafeBuiltin MkCons

-- add an element to a List
pluBuiltListPairAddOn = pcons # built910 # pluBuiltListPair

-- fPIntToInt :: Term s PInteger -> Integer
-- fPIntToInt pi = plift pi


emptyPairList :: forall s. Term s (PBuiltinList (PBuiltinPair (PAsData PInteger) (PAsData PInteger)))
emptyPairList = pcon PNil

-- pmatch using the full list for testing
matchOnPairList :: forall {s :: S}. Term s PInteger
matchOnPairList = pmatch (pcon $ PCons built3 $ pcon PNil) $ \case
  PNil -> perror
  PCons pb _ -> (pfromData (pfstBuiltin # pb))
-- ghci> plift matchOnPairList 
-- 3

-- pmatch on variable instead of writing it out
matchOnPairList1 = pmatch (builtPairList) $ \case
  PNil -> perror
  PCons pb _ -> (pfromData (pfstBuiltin # pb))
-- ghci> plift matchOnPairList1
-- 3

-- pmatch but do from data after you get result and then plist to print it.
matchOnPairList2 = pmatch (builtPairList) $ \case
  PNil -> perror
  PCons pb _ -> (pfstBuiltin # pb)
-- ghci> plift (pfromData matchOnPairList2)
-- 3


matchOnList :: forall s. Term s PString
matchOnList = pmatch (pcon $ PCons (phexByteStr "fe") $ pcon PNil) $ \case
  PNil -> "hey hey there's nothing here!"
  PCons _ _ -> "oooo fancy!"
-- ghci> plift matchOnList 
-- "oooo fancy!"
-- pairList = pfromData builtPairList

threePdata :: Term s (PAsData PInteger)
threePdata = ( pdata 3 ) 

builtInt34pdata :: forall s. Term s (PBuiltinList (PAsData PInteger))
builtInt34pdata = pcon $ PCons (pdata 3) $ pcon PNil

builtInt34 :: forall s. Term s (PBuiltinList (PAsData PInteger))
builtInt34 = pcon $ PCons int3 $ pcon PNil

listOfBs :: forall s. Term s (PBuiltinList (PAsData PByteString))
listOfBs = pcon $ PCons (pdata $ phexByteStr "fe") $ pcon PNil

-- builtList :: forall s. Term s (PBuiltinList (PBuiltinPair (PAsData PInteger) (PAsData PInteger)))
-- builtList = BI.PCons # BI.PCons built3 
-- builtListHead = phead # builtList
-- pluList :: Term s (PList PInteger)
-- pluList = pcons # 2 #$ pcons # 6 #$ pcons # 12 # pnil


int3FromPas = pfromData fst3
-- ghci> plift int3FromPas 
-- 3

--  Below wont work since PBuiltinPair expects PAsData thats the main reason you struggled a lot - you kept using regular plutarch type.
-- built4 :: forall {s :: S}.
--      Term s (PBuiltinPair (PInteger) (PInteger))
-- built4 = ppairDataBuiltin # int1 # int2
-- src/CrowdFundingOnChain.hs:389:29: error:
--     • Couldn't match type ‘PInteger’ with ‘PAsData a0’
--       Expected: Term s (PAsData a0)
--         Actual: Term s PInteger
--     • In the second argument of ‘(#)’, namely ‘int1’
--       In the first argument of ‘(#)’, namely ‘ppairDataBuiltin # int1’
--       In the expression: ppairDataBuiltin # int1 # int2
--     |
-- 389 | built4 = ppairDataBuiltin # int1 # int2



-- ghci> :t built3
-- built3
--   :: forall {s :: S}.
--      Term s (PBuiltinPair (PAsData PInteger) (PAsData PInteger))

myCrowdCurrTokenValue = Plutarch.Api.V1.Value.psingleton # (pcrowdCurrSymb) # ( pcrowdTokenName) # pint1ghci> evalT myCrowdCurrTokenValue
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf (DefaultUniApply DefaultUniProtoList (DefaultUniApply (DefaultUniApply DefaultUniProtoPair DefaultUniData) DefaultUniData)) 
-- [(B "\219\186\180|\246\DLE\146\GS\184\226f\195t|\211\147\219o\157K~\184\227H\221\235\&9q",Map [(B "CrowdFundingToken",I 1)])]))}},
--ExBudget {exBudgetCPU = ExCPU 2297429, exBudgetMemory = ExMemory 7558},[])


pcrowdTokenName :: (Term s PTokenName)
pcrowdTokenName = pconstant "CrowdFundingToken"
pcrowdCurrSymb :: (Term s PCurrencySymbol)
pcrowdCurrSymb = pconstant "dbbab47cf610921db8e266c3747cd393db6f9d4b7eb8e348ddeb3971"


crowdAddress :: (Term s PByteString)
crowdAddress = pconstant "CrowdFundingSomeAddress"
crowdAddressBuilt = pdata crowdAddress

crowdCurrSymb :: forall {s :: S}. PCurrencySymbol s
crowdCurrSymb = PCurrencySymbol crowdName


-- crowdTokenPair = ppairDataBuiltin # ( crowdNameBuilt) # (pdata pint1)

-- crowdTokenPairList :: Term s (PBuiltinList (PBuiltinPair (PAsData PByteString) (PAsData PInteger)))
-- crowdTokenPairList = pcons # crowdTokenPair # pnil
-- pmap1 = PMap crowdTokenPairList


-- crowdCurrTokenPair = ppairDataBuiltin # ( crowdAddress) # (crowdTokenPairList)

crowdName :: (Term s PByteString)
crowdName = pconstant "CrowdFundingToken"
crowdNameBuilt = pdata crowdName

crowdTokenName :: forall {s :: S}. PTokenName s
crowdTokenName = PTokenName crowdName


-- adaName :: (Term s PByteString)
-- adaName = pconstant "ADA"

-- adaTokenName :: forall {s :: S}. PTokenName s
-- adaTokenName = PTokenName adaName




int1 :: Term s PInteger
int1 = pconstant 12
-- ghci> plift (fib # int1)
-- 144


-- my1Val :: forall 
--     (keys :: KeyGuarantees)
--     (amounts :: AmountGuarantees)
--     (s :: S). PValue keys amounts s
-- my1Val = PValue ((PMap crowdCurrSymb  (PMap crowdTokenName int1)))	

-- PValue constructor - 
-- PValue (Term s (PMap keys PCurrencySymbol (PMap keys PTokenName PInteger)))

-- x1Map = (PMap (int1 int1)

------ My own breakdown below for preclist - 
psingletonTokenNameWithCS :: 
  forall 
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S). 
  Term s (PAsData PCurrencySymbol :--> PValue keys amounts :--> PTokenName)
psingletonTokenNameWithCS = phoistAcyclic $ plam $ \policyId val ->
  pmatch val $ \(PValue val') ->
    precList 
      (\self x xs ->                                                                     -- this one takes the PValue which is map of CurrSymb and then map of token name qty.
        pif                                                                              -- pif -
          (pfstBuiltin # x #== policyId)                                                 --       next line is the condition
          ( pmatch (pfromData (psndBuiltin # x)) $ \(PMap tokens) ->                     --       next line is the Then when condition is true, Meaning we found policy id
              plet (phead # tokens) $ \fstTk ->                       --  since policy id matched now we look at tokens and qty. we do a let for variables. We set head 1st element
                (pif                                                                     -- Next if stmt
                  (pnull # (ptail # tokens) #&& pfromData (psndBuiltin # fstTk) #== 1)   -- since for this CurrSYm/Policy we expect only 1 (not more than 1) token , tail needs to be Null.
                  (pfromData $ pfstBuiltin # fstTk)                                      -- above condition is true we return Token Name since its 1st part (2nd part is qty)
                  perror                                                                 -- ELSE error since either qty >1 or Has some other tokens etc.. 
                )
          )
          (self # xs)                                                                     -- ELSE for 1st If. Policy id did not match so looks for remaining list. 
        )
      (const perror)
      # pto val'



-- https://plutonomicon.github.io/plutarch-plutus/Usage/Recursion

pfac :: Term s (PInteger :--> PInteger)
pfac = pfix #$ plam f
  where
    f :: Term s (PInteger :--> PInteger) -> Term s PInteger -> Term s PInteger
    f self n = pif (n #== 1) n $ n * (self #$ n - 1)

--- my own understanding - 
--- pfac 5
--- 5 * (pfac 4)
--- 5 * ( 4 * (pfac 3))
--- 5 * (4 * (3 * (pfac 2)))
--- 5 * (4 * (3 * ( 2* (pfac 1))))
--- 5 * (4 * (3 * ( 2* ( 1))))
--- 120


fib :: Term s (PInteger :--> PInteger)
fib = phoistAcyclic $ 
  pfix #$ plam 
    (\self n ->
     pif (n #== 0) 0 $ 
       pif (n #== 1) 1 $
         (self # (n - 1) + self # (n-2))
    )
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () 
-- (Some (ValueOf DefaultUniInteger 144))}},
-- ExBudget {exBudgetCPU = ExCPU 811771341, exBudgetMemory = ExMemory 1849274},[])


-- pasConstr :: Term s (PData :--> PBuiltinPair PInteger (PBuiltinList PData))
-- pasConstr = punsafeBuiltin UnConstrData

constructorIdOf :: Term s (PData :--> PInteger)
constructorIdOf = plam $ \x -> pfstBuiltin #$ pasConstr # x

fieldsOf :: Term s (PData :--> PBuiltinList PData)
fieldsOf = plam $ \x -> psndBuiltin #$ pasConstr # x









-- https://gist.github.com/MangoIV/3bbe1f029805f0d829248f00c2cfc4a2
-- c1 = constructorIdOf evalWithArgsT [toData (Nothing :: Maybe ())]

-- https://plutonomicon.github.io/plutarch-plutus/Run 
-- Below functions were copied from above link
---- UTIL 
-- evalWithArgsT :: ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
-- evalWithArgsT x args = do
--   cmp <- PL.compile def x
--   let (escr, budg, trc) = evalScript $ applyArguments cmp args
--   scr <- first (pack . show) escr
--   pure (scr, budg, trc)

-- applyArguments :: Script -> [Data] -> Script
-- applyArguments (Script p) args =
--     let termArgs = mkConstant () <$> args
--         applied t = mkIterApp () t termArgs
--     in Script $ over progTerm applied p

-- evalSerialize :: ClosedTerm a -> Either Text ShortByteString
-- evalSerialize x = serialiseScript . (\(a, _, _) -> a) <$> evalT x

-- evalT :: ClosedTerm a -> Either Text (Script, ExBudget, [Text])
-- evalT x = evalWithArgsT x []

-- evalWithArgsT' :: ClosedTerm a -> [Data] -> Either Text (Program DeBruijn DefaultUni DefaultFun (), ExBudget, [Text])
-- evalWithArgsT' x args =
--   (\(res, budg, trcs) -> (unScript res, budg, trcs))
--     <$> evalWithArgsT x args



--------------------------------------------------------------------------------------------------------------
---- ----------------------------------------------------------  Running Unit testing


-- sampleValidatorTest :: TestTree
-- sampleValidatorTest = tryFromPTerm "sample validator" sampleValidator $ do
--   [PlutusTx.toData (), PlutusTx.toData (1 :: Integer), PlutusTx.toData ()] @> "It should succeed when given 1"

pcrowdValidatorWTest :: TestTree
pcrowdValidatorWTest = tryFromPTerm "sample validator" (pcrowdValidatorW # (pdata pubKeyHashTest1)) $ do
  [PlutusTx.toData datumCrowdVal14aClose, PlutusTx.toData redeemCrowdCloseBuiltin, PlutusTx.toData mockCtxV2] @> "It should succeed when given 1"

-- pcrowdValidatorWTest = tryFromPTerm "sample validator" (pcrowdValidatorW # (pdata pubKeyHashTest1)) $ do
--   [PlutusTx.toData datumCrowdBuiltin, PlutusTx.toData redeemCrowdCloseBuiltin, PlutusTx.toData mockCtxV2] @> "It should succeed when given 1"

  -- [PlutusTx.toData ()] @& do
  --   testEvalCase
  --     "(Sharing first argument) It should succeed when given 1"
  --     Success
  --     [PlutusTx.toData (1 :: Integer), PlutusTx.toData ()]

  -- withApplied [PlutusTx.toData ()] $ do
  --   testEvalCase
  --     "(Sharing first argument) It should fail when given 10"
  --     Failure
  --     [PlutusTx.toData (10 :: Integer), PlutusTx.toData ()]



pcrowdValidatorWTest13a :: TestTree
pcrowdValidatorWTest13a = tryFromPTerm "sample validator" (pcrowdValidatorW # (pdata pubKeyHashTest1)) $ do
  [PlutusTx.toData datumCrowdVal13aCloseBuiltin, PlutusTx.toData redeemCrowdCloseBuiltin, PlutusTx.toData mockCtxVal13a] @> "It should fail when Target no reached"
       

pcrowdValidatorWTest14a :: TestTree
pcrowdValidatorWTest14a = tryFromPTerm "sample validator" (pcrowdValidatorW # (pdata pubKeyHashTest1)) $ do
  [PlutusTx.toData datumCrowdBuiltin, PlutusTx.toData redeemCrowdCloseBuiltin, PlutusTx.toData mockCtxVal14a] @> "It should fail when Target no reached"
        

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $
    testGroup
      "test suite"
      [ -- pcrowdValidatorWTest ,
        -- pcrowdValidatorWTest
        -- pcrowdValidatorWTest14a
        pcrowdValidatorWTest13a

      -- , sampleFunctionTest
      ]

-- -- Success Case 
-- -- ghci> main
-- -- test suite
-- --   sample validator
-- --     It should succeed when given 1: OK

-- -- All 1 tests passed (0.01s)
-- -- *** Exception: ExitSuccess


-- -- Failure case
-- -- ghci> main
-- -- test suite
-- --   sample validator
-- --     It should succeed when given 1: FAIL (0.02s)
-- --       Expected a successful run, but failed instead.
-- --       Error
-- --                                                          An error has occurred:  User error:
-- --       The machine terminated because of an error, either from a built-in function or from an explicit use of 'error'.Logs
-- --                 "targetAmount"

-- -- 1 out of 1 tests failed (0.02s)
-- -- *** Exception: ExitFailure 1