module CheckSignatory (checkSignatory) where


-- {-# LANGUAGE OverloadedStrings #-}

import Plutarch.Prelude
import Plutarch.Api.V1 (PDatum, PRedeemer, PScriptContext)
import Plutarch.Api.V1.Crypto (PPubKeyHash)
import Plutarch.Api.V1.Contexts (PScriptPurpose(PSpending))
-- import Plutarch.Docs.Run (evalWithArgsT)
import Plutarch.Script (Script)
import qualified PlutusTx
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)
import qualified Plutarch.Monadic as P
import Data.Text (Text)

-- import Data.Integer.Internal

import Utils

-- import Plutus.V1.Ledger.Api
import PlutusLedgerApi.V1
import PlutusLedgerApi.V1.Interval
-- import Plutus.V2.Ledger.Interval
-- import qualified PlutusTx
-- import PlutusLedgerApi.V1.Bytes

-- ------------------------- from -- SmallValidator
import Plutarch
import Plutarch.DataRepr



--- from Spending Builder

-- import qualified PlutusLedgerApi.V2 (
--   ScriptContext (scriptContextPurpose),
--   ScriptPurpose (Spending),
--   TxOutRef (..),
--   singleton,
--  ) as PLAV2

import qualified PlutusLedgerApi.V2 as PLAV2

import Data.Functor.Contravariant (Contravariant (contramap))
import Optics (view)
import Plutarch.Context (
  CheckerPos (AtInput),
  SpendingBuilder,
  checkAt,
  checkFoldable,
  checkValidatorRedeemer,
  input,
  mint,
  output,
  pubKey,
  script,
  tryBuildSpending,
  unpack,
  withDatum,
  withRef,
  withRefIndex,
  withRefTxId,
  withSpendingOutRef,
  withSpendingOutRefId,
  withSpendingOutRefIdx,
  withValue,
 )



data POurDatum (s :: S) = POurDatum (Term s (PDataRecord '[ "password" ':= PInteger ]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType POurDatum where 
    type DPTStrat _ = PlutusTypeData 

instance PTryFrom PData POurDatum 




-- ------------------------- end----  SmallValidator




checkSignatory :: Term s (PPubKeyHash :--> PAsData PDatum :--> PAsData PRedeemer :--> PAsData PScriptContext :--> PUnit)
checkSignatory = plam $ \ph _ _ ctx' -> P.do
  ctx <- pletFields @["txInfo", "purpose"] ctx'
  PSpending _ <- pmatch ctx.purpose
  let signatories = pfield @"signatories" # ctx.txInfo
  pif
    (pelem # pdata ph # pfromData signatories)
    -- Success!
    (pconstant ())
    -- Signature not present.
    perror


hashStr :: PubKeyHash
hashStr = "0e97aa033ceee762c25285cdcc94287178f01b45a585fd0d4da8387a"



pubKeyHash :: Term s PPubKeyHash
pubKeyHash = pconstant hashStr

mockCtx :: ScriptContext
mockCtx =
  ScriptContext
    (TxInfo
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      (interval (POSIXTime 1) (POSIXTime 2))
    --   [fromString hashStr, "f013", "ab45"]
    --   ["abce0f123e", "f013", "ab45"]
    --   ["0e97aa033ceee762c25285cdcc94287178f01b45a585fd0d4da8387a", "f013", "ab45"]
      [hashStr, "f013", "ab45"]    --- this works too. So did not need `fromString`
      mempty
      ""
    )
    (Spending (TxOutRef "" 1))

-- ghci> evalWithArgsT (checkSignatory # pubKeyHash) [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData mockCtx]
-- Right (Script {unScript = Program {_progAnn = (), _progVer = Version () 1 0 0, _progTerm = Constant () 
-- (Some (ValueOf DefaultUniUnit ()))}},
-- ExBudget {exBudgetCPU = ExCPU 6393126, exBudgetMemory = ExMemory 16609},[])


-- evalWithArgsT (checkSignatory # pubKeyHash) [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData mockCtx]
-- evalWithArgsT (checkSignatory # ( "f013")) [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData mockCtx]


mockCtx2 :: ScriptContext
mockCtx2 =
  ScriptContext
    (TxInfo
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      (interval (POSIXTime 1) (POSIXTime 2))
    --   [fromString hashStr, "f013", "ab45"]
    --   ["abce0f123e", "f013", "ab45"]
    --   ["0e97aa033ceee762c25285cdcc94287178f01b45a585fd0d4da8387a", "f013", "ab45"]
      [hashStr, "f013", "ab45"]    --- this works too. So did not need `fromString`
      mempty
      ""
    )
    (Minting (CurrencySymbol "abc"))

-- ghci> evalWithArgsT (checkSignatory # pubKeyHash) [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData mockCtx2]
-- Left "An error has occurred:  User error:\nThe machine terminated because of an error, either from a built-in function or from an explicit use of 'error'."


xx = PlutusTx.toData mockCtx2


int1 :: Term s PInteger
int1 = pconstant 10
datum1 = POurDatum (pdcons # (pdata int1) # pdnil)     -- this is already in PAsData PIntger
-- datum2 = POurDatum (pdcons # ( int1) # pdnil)

-- datum1ToDo = PlutusTx.toData datum1



someOutRef :: TxOutRef
someOutRef = TxOutRef "abcdee" 71

sample :: SpendingBuilder
sample =
  mconcat
    [ mint $ PLAV2.singleton "aaaa" "hello" 333
    , input $
        pubKey "ffaacc"
          <> withValue (PLAV2.singleton "cc" "hello" 123)
          <> withRefIndex 19
    , input $
        pubKey "aaccdd"
          <> withValue (PLAV2.singleton "cc" "hello" 123)
          <> withRef someOutRef
    , input $
        pubKey "aabb"
          <> withValue (PLAV2.singleton "cc" "hello" 123)
          <> withRefIndex 1121
          <> withRefTxId "abababcc"
    , input $
        pubKey "eeffdd"
          <> withValue (PLAV2.singleton "cc" "hello" 123)
          <> withRefTxId "eeddaa"
    , input $
        pubKey "eeee"
          <> withValue (PLAV2.singleton "cc" "hello" 123)
          <> withDatum (123 :: Integer)
    , output $
        script "cccc"
          <> withValue (PLAV2.singleton "dd" "world" 123)
    ]


-- evalWithArgsT (checkSignatory # pubKeyHash) [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData sample]