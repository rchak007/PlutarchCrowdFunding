# Plutarch CrowdFunding
Optimized Crowdfunding compared to earlier one done on plutus apps.



We revisit the Plutus Apps CrowdFunding project developed earlier but now use Plutarch to develop the same one.

[Plutus apps Crowdfunding](https://github.com/rchak007/CrowdFundingCardanoPlutus)





## Plutarch On Chain code

### Overview

Plutarch is an eDSL in Haskell for writing on-chain scripts for Cardano. With some caveats, Plutarch is a [simply-typed lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus) (or STLC). Writing a script in Plutarch allows us to leverage the language features provided by Haskell while retaining the ability to compile to compact Untyped Plutus Core (or UPLC, which is an untyped lambda calculus).

When we talk about “Plutarch scripts,” we are referring to values of type `Term (s :: S) (a :: PType)`. `Term` is a `newtype` wrapper around a more complex type, the details of which Plutarch end-users can ignore. A `Term` is a typed lambda term; it can be thought of as representing a computation that, if successfully evaluated, will return a value of type `a`.



### Why Plutarch?

Plutarch written validators are often significantly more efficient than Plutus Tx written validators. With Plutarch, you have much more fine gained control of the Plutus Core you generate, without giving up any type information.

To put things into perspective, one validator script from a large production contract was rewritten in Plutarch, changed from Plutus Tx. Here's the comparison between the Plutarch script's execution cost compared to the Plutus Tx script's execution cost. These numbers were gathered by simulating the whole contract flow on a testnet:

| Version            | CPU         | Memory  | Script Size |
| ------------------ | ----------- | ------- | ----------- |
| PlutusTx (current) | 198,505,651 | 465,358 | 2013        |
| Plutarch           | 51,475,605  | 99,992  | 489         |



[Reference]: https://github.com/Plutonomicon/plutarch-plutus#why-plutarch	"Why Plutarch"







#### Plutarch validator

This is a simple validator where it matches the existing Datum number with Redeemer number, and if its equal then it unlocks the funds. Again this is a trivial example as we inline the Datum so its actually visible but ideally it should be hashed.



```haskell
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
    -- (pconstant ())
    pif 
      -- validation#15  
      (pelem # (datF.beneficiary) #  sigs ) -- signatories ) -- pfromData signatories)
    --   -- (pelem # ph # sigs)
    --   -- ((plength # sigs) #== 0)   -- fail
    --   -- ((plength # (pfromData signatories)) #== 1)   -- succeeded finally!
    --   -- ((phead # sig) #== ph)
      ( pif 
        -- validation#14
        -- datumOnUtxo checks the datum on UTXO and compares the TargetAmount and what ActualTargetAmountSoFar
        -- datF is what's passed to validator in Datum
        (pfromData datF.actualtargetAmountsoFar #>= ( pfromData datF.targetAmount) #&&
            pfromData datumOnUtxo.actualtargetAmountsoFar #>= ( pfromData datumOnUtxo.targetAmount) #&& 
               pfromData datumOnUtxo.actualtargetAmountsoFar #>= ( pfromData datumOnUtxo.actualtargetAmountsoFar) )
--        validation#13
          -- traceIfFalse "UTXO being spend values are not matching based on Datum" correctInputValueClose
          --          Need to get the Datum's actual contribution and match it to the value also.
        
        ( pif
            ( (pvalueOf # ownInputF.value # datumOnUtxo.aCurrency # datumOnUtxo.aToken #== 1)   #&&
                (plovelaceValueOf # ownInputF.value  #== datumOnUtxo.actualtargetAmountsoFar) )    -- Curr Symbol and Token qty is only 1
            (pconstant ()) -- (pconstant True) -- 
            (ptraceError "Currency Symb and Token qty not equal to 1") -- perror -- (ptraceError "x shouldn't be 10")
        )
        -- 
        (ptraceError "targetAmount not reached on the Datums") -- perror -- (ptraceError "x shouldn't be 10")
      )
      -- (pconstant ())
      perror -- (pconstant ()) -- perror --  -- perror




pcrowdValidatorW :: ClosedTerm (PAsData PPubKeyHash :--> PValidator)
-- pcrowdValidatorW :: ClosedTerm (PValidator)
-- pcrowdValidatorW = plam $ \datum redeemer ctx' -> unTermCont $ do 
pcrowdValidatorW = plam $ \ph datum redeemer ctx' -> unTermCont $ do 
  (dat, _) <- ptryFromC @PDat datum 
  (redmr, _) <- ptryFromC @PRedeem redeemer 
  -- pure $ popaque $ pcrowdValidator # dat # redmr # ctx'
  pure $ popaque $ pcrowdValidatorTest # ph # dat # redmr # ctx'
```













## Unit Testing - Tasty

**Tasty** is a modern testing framework for Haskell.

It lets you combine your unit tests, golden tests, QuickCheck/SmallCheck properties, and any other types of tests into a single test suite.

Features:

- Run tests in parallel but report results in a deterministic order
- Filter the tests to be run using patterns specified on the command line
- Hierarchical, colored display of test results
- Reporting of test statistics
- Acquire and release resources (sockets, temporary files etc.) that can be shared among several tests
- Extensibility: add your own test providers and ingredients (runners) above and beyond those provided



Here is snippet of Unit testing:

```haskell
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
```



### Develop Script Contexts



```haskell
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
```



```haskell

-- TXInInfo
txInVal13aScriptUtxo :: LedgerApiV2.TxInInfo
txInVal13aScriptUtxo = LedgerApiV2.TxInInfo {
           LedgerApiV2.txInInfoOutRef = lTxOutRef,
           LedgerApiV2.txInInfoResolved = crTxOutVal13aTest1OutForInput
}

-- TxOut
crTxOutVal13aTest1OutForInput :: LedgerApiV2.TxOut
crTxOutVal13aTest1OutForInput = LedgerApiV2.TxOut { LedgerApiV2.txOutAddress = crAddress1,
             LedgerApiV2.txOutDatum  = (rawDatToOutputDatum datumCrowdVal13aClose), 
             LedgerApiV2.txOutReferenceScript = Nothing,
             LedgerApiV2.txOutValue =  ada70 <> minAda <> valueCsTk 

}

-- Datum
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
 
datumCrowdVal13aCloseBuiltin = PlutusTx.toBuiltinData datumCrowdVal13aClose

datumCrowdVal13aMap :: Map DatumHash Datum
datumCrowdVal13aMap = fromList [(DatumHash "datumCrowd", Datum datumCrowdVal13aCloseBuiltin)]
                           
                           
                           
redeemCrowdClose :: Redeem
redeemCrowdClose = Close

redeemCrowdCloseBuiltin = PlutusTx.toBuiltinData redeemCrowdClose



```

