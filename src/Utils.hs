{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Utils where 

import Plutarch.Api.V2
import Plutarch.Prelude
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont 
import "liqwid-plutarch-extra" Plutarch.Extra.List (plookupAssoc)

-- evalT related imports
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

pexpectJust :: Term s r -> Term s (PMaybe a) -> TermCont @r s (Term s a)
pexpectJust escape ma = tcont $ \f -> pmatch ma $ \case
  PJust v -> f v 
  PNothing -> escape 

psymbolValueOfHelper ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term
    s
    ( (PInteger :--> PBool)
        :--> PCurrencySymbol
        :--> ( PValue keys amounts
                :--> PInteger
             )
    )
psymbolValueOfHelper =
  phoistAcyclic $
    plam $ \cond sym value'' -> unTermCont $ do
      PValue value' <- pmatchC value''
      PMap value <- pmatchC value'
      m' <-
        pexpectJust
          0
          ( plookupAssoc
              # pfstBuiltin
              # psndBuiltin
              # pdata sym
              # value
          )
      PMap m <- pmatchC (pfromData m')
      pure $
        pfoldr
          # plam
            ( \x v ->
                plet (pfromData $ psndBuiltin # x) $ \q ->
                  pif
                    (cond # q)
                    (q + v)
                    v
            )
          # 0
          # m

-- | @since 1.0.0
ppositiveSymbolValueOf ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PCurrencySymbol :--> (PValue keys amounts :--> PInteger))
ppositiveSymbolValueOf = phoistAcyclic $ psymbolValueOfHelper #$ plam (0 #<)

-- | @since 1.0.0
pnegativeSymbolValueOf ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PCurrencySymbol :--> (PValue keys amounts :--> PInteger))
pnegativeSymbolValueOf = phoistAcyclic $ psymbolValueOfHelper #$ plam (#< 0)

(#>) :: PPartialOrd t => Term s t -> Term s t -> Term s PBool
a #> b = b #< a
infix 4 #>

(#>=) :: (PPartialOrd t) => Term s t -> Term s t -> Term s PBool
a #>= b = b #<= a

infix 4 #>=



evalWithArgsT :: ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT x args = do
  cmp <- PL.compile def x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

applyArguments :: Script -> [Data] -> Script
applyArguments (Script p) args =
    let termArgs = mkConstant () <$> args
        applied t = mkIterApp () t termArgs
    in Script $ over progTerm applied p

evalSerialize :: ClosedTerm a -> Either Text ShortByteString
evalSerialize x = serialiseScript . (\(a, _, _) -> a) <$> evalT x

evalT :: ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT x = evalWithArgsT x []

evalWithArgsT' :: ClosedTerm a -> [Data] -> Either Text (Program DeBruijn DefaultUni DefaultFun (), ExBudget, [Text])
evalWithArgsT' x args =
  (\(res, budg, trcs) -> (unScript res, budg, trcs))
    <$> evalWithArgsT x args