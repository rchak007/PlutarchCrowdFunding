# Plutarch CrowdFunding
Optimized CrowdFunding smart contract on Plutarch



![image-20230616205650516](Images/image-20230616205650516.png)



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







#### Plutarch validator - Crowd Funding Smart contract 



This smart contact will help anyone who wants to raise money through Crowd Funding on public block-chain Cardano. Its generic Smart contract so anyone can use it to raise Funds. So it can be used to run as many Crowd Funding ventures as needed by anyone.  

The methodology used is an unique one and only NFT will always be present on UTXO that will gather all the crowd funded Ada. The very first time the Beneficiary (or anyone can too specifying a Beneficiary who will collect the funds) starts off with depositing an NFT at the script to kick off the Crowd Funding venture. This is the initial UTXO with NFT. This NFT is our thread token that will track this particular Crowd Funding venture.

When a contributor wants to contribute they will spend this unique UTXO with NFT and write it back to script with this NFT with any Ada already present and additionally the amount they are contributing. This is serialized in this way to keep track of Target amount. 

In the end once target is met and deadline is passed the Beneficiary can collect the funds.

The OnChain code will do all the validations necessary so this execution happens  and provide necessary safe guards and no other malicious actors can hijack the contract and drain money etc. 





![image-20230222112832909](Images/image-20230222112832909.png)



#### On-Chain 

The UTXO at the script will carry this information on a Datum as a State of the crowd funding venture.



[`CrowdFundingOnChain.hs`](https://github.com/rchak007/PlutarchCrowdFunding/blob/main/src/CrowdFundingOnChain.hs)

##### Crowd Funding State (Datum)

1. Beneficiary 
   1. Beneficiary is the person who will get the funds raised by this contract. This is represented by public hash key of the Beneficiary wallet address. Only this address can get the funds.
   2. Only if the target is reached the Beneficiary can collect the amount and also past the Deadline
2. Deadline - will allow us to set a deadline for the Fund contribution and also collection. 
3. A unique one and only NFT will  manage this contract. The NFT will always sit at the UTXO that has all the Ada being collected at the script.
4. Contributors 
   1. Contributors can contribute to this Smart contract until the deadline is reached
   2. Smart contract will also keep track of who is contributing through the Contributor's public key hash and also amount they contribute. 
5. Target amount - a target amount is first specified by the beneficiary as the goal of the Crowd Funding contract. If this is not reached the Beneficiary cannot withdraw amounts.
6. Actual Target Amount so far - this represents how much was collected so far



This Crowd Funding smart contract is developed on Cardano blockchain using `Plutarch`.

This same contract can be used for multiple Crowd Funding ventures by anyone.

```haskell
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
```

We represent this above Crowd Funding State as a UTxO. The UTxO sits at the script address of the Crowd Fund smart contract, and its datum field it carries the current state of the crowd funding data.

Since validation only happens when you want to consume something from a script address, not when you produce an output at a script address. This means that we can’t prevent anybody producing arbitrary outputs at the script address.

Somehow we need to distinguish the true Crowd Fund output from other outputs that may be sitting at the same script address. And the way we do this is to put an NFT on the output. Because an NFT can only exist once, there can only be one UTxO at the script address that holds the NFT.



Now, the Crowd Funding validator has to check several things.

1. Is the NFT present in the consumed input?
2. Is there an output from the transaction at the same address containing the same NFT?
3. Is the value in the output UTxO the same as the input value?
4. Is the fee present?

Now we can complete the transaction.



##### Crowd Funding Redeem Actions

Redeem actions are Contribute and Close.







```haskell
data PRedeem (s :: S) = 
      PContribute 
        ( Term 
          s 
          ( PDataRecord 
          '["contribution" ':= PInteger])) 
      | PClose (Term s (PDataRecord '[]))
 deriving stock (Generic)
 deriving anyclass (PlutusType, PIsData, PEq, PShow)
```



###### Contribute 

With this redeem action Contribtute a contributor will provide their pubKeyHash and amount they are contributing.

Also since we write the script UTXO we consume back the script we need to provide the new Datum too and this Datum's fields need to reflect the correct Value being added. So the new Datum, Redeem and actual value at script UTXO (NFT + already held Value + new contribution) should all be correctly formed. otherwise there will be error.

Note- deadline reached was commented out to make it easier for testing. But in real contracts we will have this turned on.



###### Close

Beneficiary can collect all the Ada from Crowd Fund contract when Target Amount is met and deadline is passed.





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

