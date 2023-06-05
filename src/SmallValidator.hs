{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module SmallValidator where

import Plutarch
import Plutarch.Prelude
import Plutarch.Api.V1 (
  PCredential (PPubKeyCredential, PScriptCredential),
 )
import Plutarch.Api.V2 
import Plutarch.DataRepr
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
  pmatchC,
  ptryFromC
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusTx qualified
import Utils

-- from CheckSignatories
import PlutusLedgerApi.V1
import PlutusLedgerApi.V1.Interval


--data OurDatum = OurDatum {password :: BuiltinByteString}

data POurDatum (s :: S) = POurDatum (Term s (PDataRecord '[ "password" ':= PInteger ]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType POurDatum where 
    type DPTStrat _ = PlutusTypeData 

instance PTryFrom PData POurDatum 

data POurRedeemer (s :: S) = POurRedeemer (Term s (PDataRecord '[ "password" ':= PInteger ]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType POurRedeemer where 
    type DPTStrat _ = PlutusTypeData

instance PTryFrom PData POurRedeemer  

-- validateSmallChecks :: OurDatum -> OurRedeemer -> ScriptContext -> () 
pvalidateSmallChecks :: Term s (POurDatum :--> POurRedeemer :--> PScriptContext :--> PUnit)
pvalidateSmallChecks = phoistAcyclic $ plam $ \datum redeemer _ctx -> unTermCont $ do 
    -- ctxF <- pletFieldsC @'["txInfo"] ctx 
    -- infoF <- pletFieldsC @'["signatories"] ctxF.txInfo 
    datumF <- pletFieldsC @'["password"] datum 
    redeemF <- pletFieldsC @'["password"] redeemer 
    pure $
      pif ( (redeemF.password) #== datumF.password )
      (pconstant ())
      perror

pvalidateSmallChecksW :: Term s PValidator 
pvalidateSmallChecksW = phoistAcyclic $ plam $ \datum redeemer ctx ->
    let ourDatum :: Term _ POurDatum 
        ourDatum = punsafeCoerce datum 
        ourRedeemer :: Term _ POurRedeemer 
        ourRedeemer = punsafeCoerce redeemer
     in popaque $ pvalidateSmallChecks # ourDatum # ourRedeemer # ctx 



int1 :: Term s PInteger
int1 = pconstant 10

fields :: Term _ (PDataRecord '[ "_0" ':= PInteger ])
fields = pdcons # (pdata int1) # pdnil



datum1 = POurDatum (pdcons # (pdata int1) # pdnil)
datum2 = POurRedeemer (pdcons # (pdata int1) # pdnil)


hashStr :: PubKeyHash
hashStr = "0e97aa033ceee762c25285cdcc94287178f01b45a585fd0d4da8387a"

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

-- result1 = pvalidateSmallChecksW # datum1 # datum2 # 


-- createDatum :: Terms s (PInteger :--> POurDatum)
-- createDatum phoistAcyclic $ plam $ \i -> unTermCont $ do 
--   ctxF <- pletFieldsC @'["txInfo", "purpose"] context 

-- dataRec1 = pdcons # int1 # pdnil

-- fields = pdcons # currSymDat # pdnil
-- evalWithArgsT (checkSignatory # pubKeyHash) [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData mockCtx]



