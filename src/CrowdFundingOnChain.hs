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
import Plutarch.Api.V1 (PCredential (PPubKeyCredential, PScriptCredential))
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
import Plutarch.Api.V1.Tuple

import Plutarch.Extra.Time
import Plutarch.Extra.Applicative (PApply (pliftA2))

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

import Plutarch.Extra.Maybe (pjust, pmaybe, pnothing)
import Plutarch.Extra.Field (pletAll, pletAllC)
import Plutarch.Trace
import Plutarch.Context
-- import Plutarch.Context (
--   BaseBuilder,
--   Builder,
--   address,
--   buildMinting,
--   buildMinting',
--   buildSpending,
--   buildTxInfo,
--   buildTxOuts,
--   checkNormalized,
--   input,
--   mint,
--   mkNormalized,
--   normalizeValue,
--   output,
--   pubKey,
--   runChecker,
--   script,
--   withDatum,
--   withMinting,
--   withRefIndex,
--   withRefTxId,
--   withSpendingUTXO,
--   withStakingCredential,
--   withValue,
--   withdrawal,
--  )


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
          , "contributorsMap" ':= PBuiltinList (PAsData (PTuple (PPubKeyHash) (PInteger) )) ] ))
          -- , "contributorsMap" ':= PBuiltinList  ( PBuiltinPair (PAsData PPubKeyHash) (PAsData PInteger)) ] ))
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

-- type PProposalTime = PCurrentTime
--     PInterval iv' <- pmatchC iv
--     ivf <- pletAllC iv'
--     PLowerBound lb <- pmatchC ivf.from
--     PUpperBound ub <- pmatchC ivf.to

--     let lowerBound = pletAll lb $ \f ->
--           pif
--             f._1
--             ( pmatch f._0 $ \case
--                 PFinite (pfromData . (pfield @"_0" #) -> d) -> pjust # d
--                 _ -> ptrace "currentProposalTime: time range should be bounded" pnothing
--             )
-- -- 3. onchain code


-- pcrowdValidatorTest :: Term s  (PAsData PPubKeyHash :--> PDat :--> PRedeem :--> PScriptContext :--> PUnit)
-- pcrowdValidatorTest :: Term s  (PPubKeyHash PAsData PDat :--> PAsData PRedeem :--> PAsData PScriptContext :--> PUnit)

-- pcrowdValidator :: Term s PValidator 
-- pcrowdValidatorTest = phoistAcyclic $ plam $ \ph dat redeemer ctx -> unTermCont $ do 


pcrowdValidator :: Term s  (PDat :--> PRedeem :--> PScriptContext :--> PUnit)
pcrowdValidator = phoistAcyclic $ plam $ \dat redeemer ctx -> unTermCont $  do 
  ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
  PSpending ((pfield @"_0" #) -> ownRef) <- pmatchC ctxF.purpose

  PSpending pspnd <- pmatchC ctxF.purpose
  pSp0 <- pletFieldsC @'["_0"] pspnd
  -- PTxOutRef pTxoutref0 <- pmatchC pSp0._0
  -- id0 <- pletFieldsC @'["id"] pTxoutref0  
  -- PTxId ((pfield @"_0" #) -> pTxId) <- pmatchC id0.id

    -- here if a Txn is submitted with more than 1 script UTXO then this validator will be called mutiple times actually each time with 1 script UTXO to spend it.
  datF <- pletFieldsC @'["beneficiary", "deadline", "aCurrency", "aToken", "targetAmount", "actualtargetAmountsoFar", "contributorsMap" ] dat

  

  let signatories = pfield @"signatories" # ctxF.txInfo
  infoF <- pletFieldsC @'["inputs", "outputs", "signatories", "validRange"] ctxF.txInfo
-- below lines practice for subsequent lines to indicate similarity even though done bit differently
  -- vrF <- pletFieldsC @'["from", "to"] infoF.validRange
  -- PLowerBound lb0 <- pmatchC vrF.from
  -- ppos0 <- pletFieldsC @'["_0"] lb0
  -- PFinite ((pfield @"_0" #) -> pf0) <- pmatchC ppos0._0
  -----  finalPpos0 <- pletFieldsC @'["_0"] pf0  - this does not work
  PInterval iv' <- pmatchC infoF.validRange
  ivf <- pletAllC iv'
  PLowerBound lb <- pmatchC ivf.from
  PUpperBound ub <- pmatchC ivf.to
  lowerBound <- pletAllC lb
  -- PFinite pf <- pmatchC lowerBound._0
  PFinite ((pfield @"_0" #) -> pf) <- pmatchC lowerBound._0

  validR <- pletFieldsC @'["from", "to"] infoF.validRange
  -- upperB <- pletFieldsC @'["_0", "_1"] validR.to
  -- PFinite ((pfield @"_0" #) -> currTime) <- pmatch upperB
  -- PSpending ((pfield @"_0" #) -> ownRef)
  -- pFinite <- pletFieldsC @'["_0"] upperB._0
  -- let currTimeApprox = pupperBoundCurrentTimeApproximation # infoF.validRange
  -- let currTimeApprox = plowerBoundCurrentTimeApproximation # infoF.validRange
  sigs :: Term _ (PBuiltinList (PAsData PPubKeyHash)) <- pletC infoF.signatories 
  -- allInputs <- pletC infoF.inputs
  let ownInput = ptryOwnInput # infoF.inputs # ownRef      -- so this actually implicitly is only 1 possible. -- cause each SCript UTXO calls validator each time separately
  -- let ownInput = ptryOwnInput # infoF.inputs # (pfromData pSp0) 
  -- ownInput - PTxInInfo (Term s (PDataRecord '["outRef" := PTxOutRef, "resolved" := PTxOut]))
  --     Also ownInput should only be 1 and that should have the datum. 
  --     it can have other UTXO not own which are Beneficiary's for collateral eg.
  ownInputF <- pletFieldsC @'["value", "address", "datum"] ownInput 
  let inDatum = pfromPDatum @PDat # (ptryFromInlineDatum # ownInputF.datum)
  inDatumOnUtxo <- pletFieldsC @'["beneficiary", "deadline", "aCurrency", "aToken", "targetAmount", "actualtargetAmountsoFar", "contributorsMap"] inDatum 
  

  -- we construct the Value from Datum - from actualtargetAmountsoFar and our Token
  -- then we get the input Value on UTXO. the idea is these 2 has to be same. 
  -- That way no malicious actor can deposit Million tokens and make our script unusable etc.
  --    We check this on Contribution when we dont let them write back to script with extra tokens
  --    We also check on the Close. So if its something is malicious then at least later 
  --          Contributors can take back their funds.
  --     datumAdaVal is ada value of so far collected funds from the datum at UTXO.
  let datumAdaVal = Plutarch.Api.V1.Value.psingleton # padaSymbol # padaToken # inDatumOnUtxo.actualtargetAmountsoFar
  --     datumCurrSymb is out State token value constructed to compare (so this is expected as we know)
  let datumCurrSymb = Plutarch.Api.V1.Value.psingleton # (inDatumOnUtxo.aCurrency) # (inDatumOnUtxo.aToken) # (pconstant 1)
  --     datumTotalVal - is constructed value of both Collected so far and our State CrowdFund token
  --        now we can compare the input Value of UTXO and need to be same.
  let datumTotalVal = datumAdaVal <> datumCurrSymb
  --     due to technical types of Input value and Contructed datum value 
  --            we convert the values to same type to compare
  let forgetPositiveUTXOInputVal = Plutarch.Api.V1.Value.pforgetPositive ownInputF.value
  let forgetSortedDatumTotalVal = Plutarch.Api.V1.Value.pforgetSorted datumTotalVal

-- below we also do the same Value construction from Datum passed to validator - 
--    Technically this should be same as UTXO datum as Cardano network Phase 1 validation should give error
  let datumAdaVal2 = Plutarch.Api.V1.Value.psingleton # padaSymbol # padaToken # inDatumOnUtxo.actualtargetAmountsoFar
  let datumCurrSymb2 = Plutarch.Api.V1.Value.psingleton # (inDatumOnUtxo.aCurrency) # (inDatumOnUtxo.aToken) # (pconstant 1)
  let datumTotalVal2 = datumAdaVal2 <> datumCurrSymb2
  let forgetSortedDatumTotalVal2 = Plutarch.Api.V1.Value.pforgetSorted datumTotalVal2
  


-- Store the Validaton 1, 2, 13 and 16 as Bool for later as this is common between 
--     Redeem actions Contribute and Close 
  let pval_1_2_13_16 =  
            -- make sure out CrowdFund Symb and token is present and only 1 qty
            ( (pvalueOf # ownInputF.value # inDatumOnUtxo.aCurrency # inDatumOnUtxo.aToken #== 1) #&&
            -- validate the Input value is same as in Datum which is our state
              plovelaceValueOf # ownInputF.value  #== inDatumOnUtxo.actualtargetAmountsoFar   #&&  

            -- Below checks that Value constructed from Datum is equal to what is on UTXO value
                -- This way we make sure no one has deposited lot of other tokens not relevant.
                -- the bigger validation of this is when we Contribute so it does not end up on our Script  
            (forgetPositiveUTXOInputVal #== forgetSortedDatumTotalVal) #&& 
            ((forgetPositiveUTXOInputVal #== forgetSortedDatumTotalVal2))
            ) 

  PScriptCredential ((pfield @"_0" #) -> ownValHash) <- pmatchC $ pfield @"credential" # ownInputF.address
  let ownOutput = pheadSingleton #$ pfilter # (paysToCredential # ownValHash) # infoF.outputs 
  ownOutputF <- pletFieldsC @'["value", "datum"] ownOutput
  let outputDatum = pfromPDatum @PDat # (ptryFromInlineDatum # ownOutputF.datum)
  datumOnOutUtxoF <- pletFieldsC @'["beneficiary", "deadline", "aCurrency", "aToken", "targetAmount", "actualtargetAmountsoFar", "contributorsMap"] outputDatum 

  -- for the Datum on UTXO at script being spent get the total contribution as per datum on inputs UTXO
  -- let inContribDatumIntValue = pExtractContribPBuiltInList # (Plutarch.Api.V1.Tuple.pbuiltinPairFromTuple # inDatumOnUtxo.contributorsMap)
  let inContributionMap = pExtractContribPBuiltInListTuple # inDatumOnUtxo.contributorsMap
  let inContributionMapValue = Plutarch.Api.V1.Value.psingleton # padaSymbol # padaToken # inContributionMap

  -- get contribution amount from redeemer
  let redeemContrib = getContributionRedeemer # redeemer
  let redeemContribValue = Plutarch.Api.V1.Value.psingleton # padaSymbol # padaToken # redeemContrib

-- Add the contribution map from existing UTXO from input to Contribution being made in currenet Txn.
  let inDatumPlusRedeemVal = inContributionMapValue <> redeemContribValue

-- get the contribution Map constructed on the UTXO datum being written back to script on Outputs
  let outContributionMap = pExtractContribPBuiltInListTuple # datumOnOutUtxoF.contributorsMap
  let outContributionMapValue = Plutarch.Api.V1.Value.psingleton # padaSymbol # padaToken # outContributionMap

  -- Store Validation 6 for later 
  let pval_4 = 
            -- output Datum back to script CrowdFund token should be there only 1 qty
            (pvalueOf # ownOutputF.value # datumOnOutUtxoF.aCurrency # datumOnOutUtxoF.aToken #== 1)  #&&
            -- total contributions (map) on datum in output written back to script should be equal total contributions on datum on the UTXO being spent + Contribution now in Redeem
              (outContributionMapValue #== inDatumPlusRedeemVal )
                 
            -- #&& plovelaceValueOf # ownOutputF.value  #== datumOnUtxo.actualtargetAmountsoFar 

  pure $
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
                    pfromData inDatumOnUtxo.actualtargetAmountsoFar #>= ( pfromData inDatumOnUtxo.targetAmount) #&& 
                       pfromData inDatumOnUtxo.actualtargetAmountsoFar #>= ( pfromData inDatumOnUtxo.actualtargetAmountsoFar) )
                  ( pif 
                       pval_1_2_13_16

                      ( pif 
--                          validation#17                      
                            ((inDatumOnUtxo.deadline) #< (pfromData pf))     -- Datum is less than Slot txn - so deadline passed
                            -- (datumOnUtxo.deadline #< (pfromData pf0))
                      --     LedgerIntervalV1.contains (LedgerIntervalV1.from (deadline d)) (Contexts.txInfoValidRange txinfo)
                            -- plet redeemContrib = getContributionRedeemer # redeemer
                            (pconstant ())
                            (ptraceError "Deadline has not passed")
                      )
                            

                      --        validation#17
                                -- && traceIfFalse "Deadline not yet reached" deadlinepassed                      
                      (ptraceError "Validation 13, 16 and 1 - Input UTXO values are not equal to Datum actual target amount so far")
                  )
                  (ptraceError "targetAmount not reached on the Datums")
              )
              (ptraceError "Beneficiary signature not correct")
          PContribute pc -> 
            -- plet ( getContributionRedeemer # redeemer)  $ \redeemContrib ->
            --   plet (Plutarch.Api.V1.Value.psingleton # padaSymbol # padaToken # redeemContrib) $ \redeemContribValue ->
              ( pif 
          --   Validation # 1      This validates 3 parameters to be equal 
--               1st parameter - from actual Tx-ins Values , validates that the UTXO with NFT in its Values - 
--                       bypasses other Tx-in w/o NFT like Fees Tx-in
--               2nd parameter - Values calculated based on Datum passed to the Validator
--               3rd parametr - Values calculated from Datum at the UTXO.
--                      Technically the 2 and 3 should be ensured by Network.
--              Validation # 2 - check
--                 --Only 1 tx-in with datum allowed- other can be payment address fee etc. which dont have datum
                -- this below handles both validation 1, 2, 13 and 16
                pval_1_2_13_16
                -- plet (pfield @"contribute" # pc) $ \cr ->
                --       --  (pconstant ())
                -- (pconstant ())
                ( pif
--                  validation#4
-- --               Validates expected Values based on Datum of tx-out and tx-in - tx-in Value  + redeemer value = tx-out Value
                    pval_4 
                    (pconstant ())
                    (ptraceError "Validation 4- Input Datum contributions + Redeemer Contribution is NOT equal to Output Datum contribution")
                )
  
--             validation#6
-- --          tx-out - Datum collected amount should be updated with Tx-in amount + contributed amount
              --  && traceIfFalse "the ContributedSoFar amount has a descrepancy" correctTargetAmountSoFarDatum 
  
--                   FOr this validation the Target Amount so far has to increase in the tx-out by contribution amount. So we add that from Redeemer.
--                   This is only Datum validation. Not value. Thats done with "correctOutputValue"
                (ptraceError "Validation 13, 16 and 1 - Input UTXO values are not equal to Datum actual target amount so far")
              )

        )





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

-- pcrowdValidatorW :: ClosedTerm (PAsData PPubKeyHash :--> PValidator)
pcrowdValidatorW :: ClosedTerm (PValidator)
pcrowdValidatorW = plam $ \datum redeemer ctx' -> unTermCont $ do 
-- pcrowdValidatorW = plam $ \ph datum redeemer ctx' -> unTermCont $ do 
  (dat, _) <- ptryFromC @PDat datum 
  (redmr, _) <- ptryFromC @PRedeem redeemer 
  pure $ popaque $ pcrowdValidator # dat # redmr # ctx'
  -- pure $ popaque $ pcrowdValidatorTest # ph # dat # redmr # ctx'



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


-- currentProposalTime :: forall (s :: S). Term s (PPOSIXTimeRange :--> PMaybe PPOSIXTimeRange)
-- currentProposalTime = phoistAcyclic $
--   plam $ \iv -> unTermCont $ do
--     PInterval iv' <- pmatchC iv
--     ivf <- pletAllC iv'
--     PLowerBound lb <- pmatchC ivf.from
--     PUpperBound ub <- pmatchC ivf.to

--     let lowerBound = pletAll lb $ \f ->
--           pif
--             f._1
--             ( pmatch f._0 $ \case
--                 PFinite (pfromData . (pfield @"_0" #) -> d) -> pjust # d
--                 _ -> ptrace "currentProposalTime: time range should be bounded" pnothing
--             )
--             (ptrace "currentProposalTime: lower bound of the time range should be inclusive" pnothing)

--         upperBound = pletAll ub $ \f ->
--           pmatch f._0 $ \case
--             PFinite (pfromData . (pfield @"_0" #) -> d) -> pjust # d
--             _ -> ptrace "currentProposalTime: time range should be bounded" pnothing

--         mkTime = phoistAcyclic $ plam $ pcon .* PCurrentTime
--     pure $ pliftA2 # mkTime # lowerBound # upperBound

-- "contributorsMap" ':= PBuiltinList (PAsData (PTuple (PAsData PPubKeyHash) PInteger)) ] ))
-- pGetTotalContribution :: Term s ((PBuiltinList (PAsData (PTuple (PAsData PPubKeyHash) PInteger))) :--> PInteger)
-- pGetTotalContribution = phoistAcyclic $
--   plam $ \contr ->
--     precList (\self x xs -> x $ \ptup -> 
--       pmatch ptup $ \case 
--         (psndBuiltin (pbuiltinPairFromTuple # ptup))) # contr



paysToCredential :: Term s (PScriptHash :--> PTxOut :--> PBool)
paysToCredential = phoistAcyclic $
  plam $ \valHash txOut -> unTermCont $ do
    let txOutCred = pfield @"credential" # (pfield @"address" # txOut)
    pure $
      pmatch txOutCred $ \case
        PScriptCredential txOutValHash -> (pfield @"_0" # txOutValHash) #== valHash
        PPubKeyCredential _ -> (pcon PFalse)


pheadSingleton :: (PListLike list, PElemConstraint list a) => Term s (list a :--> a)
pheadSingleton = phoistAcyclic $
  plam $ \xs ->
    pelimList (\x xs -> (pelimList (\_ _ -> perror) x xs)) perror xs 

ptryOwnInput :: (PIsListLike list PTxInInfo) => Term s (list PTxInInfo :--> PTxOutRef :--> PTxOut)
ptryOwnInput = phoistAcyclic $
  plam $ \inputs ownRef ->
    precList (\self x xs -> pletFields @'["outRef", "resolved"] x $ \txInFields -> pif (ownRef #== txInFields.outRef) txInFields.resolved (self # xs)) (const perror) # inputs
-- since only 1 Script UTXO can be calling Validator each time -- here at most we only get 1 match. 

-- this will take Pbuiltinlist of Pair and extract lets says First part only puts it in PBuiltinList
pExtractContribPBuiltInList :: Term s ( (PBuiltinList (PBuiltinPair (PAsData PPubKeyHash) (PAsData PInteger))) :--> PInteger)  -- PBuiltinList (PAsData PInteger))
pExtractContribPBuiltInList = phoistAcyclic $
  pfix #$ plam (\self n ->
    pmatch n $ \case
      PCons x xs ->
        ((pfromData (psndBuiltin # x)) + (self # xs))
      PNil -> (pconstant 0)
    -- pif (n #== PCons x xs ) (pcons # x) $ (self xs)
    --   pif (n #== PNil) (pnil) $
        
  )


pExtractContribPBuiltInListTuple :: Term s ( (PBuiltinList (PAsData (PTuple (PPubKeyHash) (PInteger) ) )) :--> PInteger )  -- PBuiltinList (PAsData PInteger))
pExtractContribPBuiltInListTuple = phoistAcyclic $
  pfix #$ plam (\self n ->
    pmatch n $ \case
      PCons x xs ->
           (  pfromData ((psndBuiltin # (pfromData (Plutarch.Api.V1.Tuple.pbuiltinPairFromTuple  x)) )  )     +     (self # xs) )
      PNil -> (pconstant 0)
    -- pif (n #== PCons x xs ) (pcons # x) $ (self xs)
    --   pif (n #== PNil) (pnil) $
        
  )

getContributionRedeemer :: Term s  (PRedeem :--> PInteger)
getContributionRedeemer = phoistAcyclic $
  plam $ \red -> (pmatch red $ \case
                    PContribute pc ->
                      plet (pfield @"contribution" # pc) $ \cont -> cont
                    PClose _ -> (pconstant 0)
                 )

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
-- scrCred = LedgerApiV2.ScriptCredential "xyzScript"
scrCred = LedgerApiV2.ScriptCredential  "d8038c3146fe364d9b2ebd196fd4aef8d6df35f92a969ddf2febdde0adbbb531"
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
             LedgerApiV2.txOutValue =  ada70  <> minAda <> valueCsTk 

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
      -- mempty                                             -- outputs
      [crTxOutVal13aTest1OutForInput]                      -- outputs
      mempty                                             -- fee
      mempty                                             -- mint
      mempty                                             -- dcert
      myStake1Map                                        -- wdrl   
      (interval (POSIXTime 1671159025000) (POSIXTime 1671159025200))   -- validRange  Datum 1671159023000  -- Success case - as datum deadline LESS than slot - so we passed deadline
      -- (interval (POSIXTime 1671159021000) (POSIXTime 1671159021200))      -- validRange    Datum 1671159023000  -- Fail case - Datum > Slot
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

--  "contributorsMap" ':= PBuiltinList (PAsData (PTuple (PAsData PPubKeyHash) PInteger)) ] ))

-- ptupleFromBuiltin :: Term s (PAsData (PBuiltinPair (PAsData a) (PAsData b))) -> Term s (PAsData (PTuple a b))
built3TuplePAs :: Term s (PAsData (PTuple PInteger PInteger))
built3TuplePAs = Plutarch.Api.V1.Tuple.ptupleFromBuiltin (built3Pas)
-- built3Tuple
built3Pas = pdata built3
-- built3 :: Term s (PBuiltinPair (PAsData PInteger) (PAsData PInteger))
-- built3 = ppairDataBuiltin # int3 # int4

tupleToBuilt3 :: Term s (PAsData (PBuiltinPair (PAsData PInteger) (PAsData PInteger)))
tupleToBuilt3 = Plutarch.Api.V1.Tuple.pbuiltinPairFromTuple built3TuplePAs

tupleToBuilt3a :: Term s (PBuiltinPair (PAsData PInteger) (PAsData PInteger))
tupleToBuilt3a = pfromData tupleToBuilt3

tupleToBuilt3b :: Term s (PAsData PInteger)
tupleToBuilt3b = psndBuiltin # tupleToBuilt3a

tupleToBuilt3c :: Term s ( PInteger)
tupleToBuilt3c = pfromData tupleToBuilt3b

listTuple3d :: Term s (PBuiltinList ( PAsData ( PTuple (PInteger) (PInteger) ) ))
listTuple3d = pcons # (built3TuplePAs) #$ pcons # ( built3TuplePAs) # pnil

headlistTuple3d :: Term s (PAsData (PTuple PInteger PInteger))
headlistTuple3d = phead # listTuple3d

headlistTuple3d1 :: Term s (PTuple PInteger PInteger)
headlistTuple3d1 = pfromData headlistTuple3d

headlistTuple3d2 = Plutarch.Api.V1.Tuple.pbuiltinPairFromTuple headlistTuple3d

contribPair1 :: Term s (PBuiltinPair (PAsData PPubKeyHash) (PAsData PInteger))
contribPair1 = ppairDataBuiltin # (pdata pubKeyHashTest1 ) # int4

contribPair1Tuple = Plutarch.Api.V1.Tuple.ptupleFromBuiltin (pdata contribPair1)

ptuple1 = ptuple # (pdata pubKeyHashTest1 ) # int4

contribPair2 :: Term s (PBuiltinPair (PAsData PPubKeyHash) (PAsData PInteger))
contribPair2 = ppairDataBuiltin # (pdata pubKeyHashTest1 ) # int3

ptuple2 = ptuple # (pdata pubKeyHashTest1 ) # int3


contribPair2Tuple = Plutarch.Api.V1.Tuple.ptupleFromBuiltin (pdata contribPair2)

-- join BuiltinPairs into a PList
listContribPair1 :: Term s (PBuiltinList (PBuiltinPair (PAsData PPubKeyHash) (PAsData PInteger)))
listContribPair1 = pcons # contribPair1 #$ pcons # contribPair2 # pnil

listContribPair1Tuple :: Term s (PBuiltinList ( PAsData ( PTuple (PPubKeyHash) (PInteger) ) ))
listContribPair1Tuple = pcons # (pdata ptuple1) #$ pcons # ( pdata ptuple2) # pnil

tupleInt1 :: Term s PInteger
tupleInt1 = pExtractContribPBuiltInListTuple # listContribPair1Tuple
-- ghci> evalT tupleInt1 
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () 
-- (Some (ValueOf DefaultUniInteger 7))}},
-- ExBudget {exBudgetCPU = ExCPU 7300430, exBudgetMemory = ExMemory 21660},[])

intFromContribMap = pExtractContribPBuiltInList # listContribPair1
-- ghci> evalT intFromContribMap 
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () 
-- (Some (ValueOf DefaultUniInteger 7))}},
-- ExBudget {exBudgetCPU = ExCPU 4550562, exBudgetMemory = ExMemory 13248},[])


-- ptryOwnInput :: (PIsListLike list PTxInInfo) => Term s (list PTxInInfo :--> PTxOutRef :--> PTxOut)
-- ptryOwnInput = phoistAcyclic $
--   plam $ \inputs ownRef ->

-- this will take Pbuiltinlist of Pair and extract lets says First part only puts it in PBuiltinList
-- , "contributorsMap" ':= PBuiltinList (PAsData (PTuple (PAsData PPubKeyHash) PInteger)) ] ))
-- --- n/a pExtractContribPBuiltInListTuple :: Term s ( (PBuiltinList (PBuiltinPair (PAsData PPubKeyHash) (PAsData PInteger))) :--> PInteger)  -- PBuiltinList (PAsData PInteger))
-- pExtractContribPBuiltInListTuple :: Term s ( (PBuiltinList (PAsData (PTuple (PPubKeyHash) (PInteger) ) )) :--> PInteger )  -- PBuiltinList (PAsData PInteger))
-- pExtractContribPBuiltInListTuple = phoistAcyclic $
--   pfix #$ plam (\self n ->
--     pmatch n $ \case
--       PCons x xs ->
--            (  pfromData ((psndBuiltin # (pfromData (Plutarch.Api.V1.Tuple.pbuiltinPairFromTuple  x)) )  )     +     (self # xs) )
--       PNil -> (pconstant 0)
--     -- pif (n #== PCons x xs ) (pcons # x) $ (self xs)
--     --   pif (n #== PNil) (pnil) $
        
--   )


-- pbuiltinPairFromTuple :: Term s (PAsData (PTuple a b)) -> Term s (PAsData (PBuiltinPair (PAsData a) (PAsData b)))


int1ToPas = pdata int1
built3 :: Term s (PBuiltinPair (PAsData PInteger) (PAsData PInteger))
built3 = ppairDataBuiltin # int3 # int4
-- ghci> evalT built3
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf (DefaultUniApply (DefaultUniApply DefaultUniProtoPair DefaultUniData) DefaultUniData)
--  (I 3,I 4)))}},
-- ExBudget {exBudgetCPU = ExCPU 191611, exBudgetMemory = ExMemory 632},[])
fst3 = pfstBuiltin # built3
snd3 = psndBuiltin # built3

-- data PBuiltinPair (a :: PType) (b :: PType) (s :: S)

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

--  "contributorsMap" ':= PBuiltinList (PAsData (PTuple (PAsData PPubKeyHash) PInteger)) ] ))
-- type PTuple a b = PDataSum '['["_0" := a, "_1" := b]]

-- newtype PDataSum defs s
-- A sum of PDataRecords. The underlying representation is the Constr constructor, 
-- where the integer is the index of the variant and the list is the record.
-- Constructors
-- PDataSum (NS (Compose (Term s) PDataRecord) defs)


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

myCrowdCurrTokenValue = Plutarch.Api.V1.Value.psingleton # (pcrowdCurrSymb) # ( pcrowdTokenName) # (pconstant 1)
-- ghci> evalT myCrowdCurrTokenValue 
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf (DefaultUniApply DefaultUniProtoList (DefaultUniApply (DefaultUniApply DefaultUniProtoPair DefaultUniData) DefaultUniData)) 
-- [(B "\219\186\180|\246\DLE\146\GS\184\226f\195t|\211\147\219o\157K~\184\227H\221\235\&9q",Map [(B "CrowdFundingToken",I 1)])]))}},
--ExBudget {exBudgetCPU = ExCPU 2297429, exBudgetMemory = ExMemory 7558},[])

someAdaTokenValue = Plutarch.Api.V1.Value.psingleton # (padaSymbol) # ( padaToken) # int1

-- ghci> evalT int1
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () 
-- (Some (ValueOf DefaultUniInteger 12))}},ExBudget {exBudgetCPU = ExCPU 23100, exBudgetMemory = ExMemory 200},[])
-- ghci> evalT someAdaTokenValue 
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf (DefaultUniApply DefaultUniProtoList (DefaultUniApply (DefaultUniApply DefaultUniProtoPair DefaultUniData) DefaultUniData)) 
-- [(B "",Map [(B "",I 12)])]))}},
-- ExBudget {exBudgetCPU = ExCPU 2228429, exBudgetMemory = ExMemory 7258},[])

-- adaPlusCrowdUnion = Plutarch.Api.V1.Value.pleftBiasedCurrencyUnion # myCrowdCurrTokenValue # someAdaTokenValue

adaPlusCrowdUnion = someAdaTokenValue <> myCrowdCurrTokenValue 


myCrowdCurrTokenValue2 = Plutarch.Api.V1.Value.psingleton # (pcrowdCurrSymb) # ( pcrowdTokenName) # (pconstant 1)
someAdaTokenValue2 = Plutarch.Api.V1.Value.psingleton # (padaSymbol) # ( padaToken) # int1
adaPlusCrowdUnion2 = someAdaTokenValue <> myCrowdCurrTokenValue 

pbool2Values = someAdaTokenValue2 #== adaPlusCrowdUnion2
-- ghci> evalT pbool2Values 
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () 
-- (Some (ValueOf DefaultUniBool False))}},
-- ExBudget {exBudgetCPU = ExCPU 31974502, exBudgetMemory = ExMemory 90441},[])

-- ghci> evalT adaPlusCrowdUnion 
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () (Some (ValueOf (DefaultUniApply DefaultUniProtoList (DefaultUniApply (DefaultUniApply DefaultUniProtoPair DefaultUniData) DefaultUniData)) 
-- [(B "",Map [(B "",I 12)]),(B "\219\186\180|\246\DLE\146\GS\184\226f\195t|\211\147\219o\157K~\184\227H\221\235\&9q",
-- Map [(B "CrowdFundingToken",I 1)])]))}},
-- ExBudget {exBudgetCPU = ExCPU 27944664, exBudgetMemory = ExMemory 81818},[])


-- ghci> :t myCrowdCurrTokenValue
-- myCrowdCurrTokenValue
--   :: forall {s :: S}. Term s (PValue 'Sorted 'NonZero)
-- PTxOut (Term s (PDataRecord '["address" := PAddress, "value" := PValue 'Sorted 'Positive, "datum" := POutputDatum, "referenceScript" := PMaybeData PScriptHash]))
myCrowdCurrTokenValue3 :: forall {s :: S}. Term s (PValue 'Sorted 'Positive)
myCrowdCurrTokenValue3 = Plutarch.Api.V1.Value.pconstantPositiveSingleton  (pcrowdCurrSymb)  ( pcrowdTokenName)  (pconstant 1)


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
pcrowdValidatorWTest = tryFromPTerm "sample validator" (pcrowdValidatorW ) $ do
-- pcrowdValidatorWTest = tryFromPTerm "sample validator" (pcrowdValidatorW # (pdata pubKeyHashTest1)) $ do
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
pcrowdValidatorWTest13a = tryFromPTerm "sample validator" (pcrowdValidatorW) $ do
-- pcrowdValidatorWTest13a = tryFromPTerm "sample validator" (pcrowdValidatorW # (pdata pubKeyHashTest1)) $ do
  [PlutusTx.toData datumCrowdVal13aCloseBuiltin, PlutusTx.toData redeemCrowdCloseBuiltin, PlutusTx.toData mockCtxVal13a] @> "All CrowdFunding validations need to pass"
       

pcrowdValidatorWTest14a :: TestTree
pcrowdValidatorWTest14a = tryFromPTerm "sample validator" (pcrowdValidatorW) $ do
-- pcrowdValidatorWTest14a = tryFromPTerm "sample validator" (pcrowdValidatorW # (pdata pubKeyHashTest1)) $ do
  [PlutusTx.toData datumCrowdBuiltin, PlutusTx.toData redeemCrowdCloseBuiltin, PlutusTx.toData mockCtxVal14a] @> "All CrowdFunding validations need to pass"
        

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