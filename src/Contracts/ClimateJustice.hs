{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Contracts.ClimateJustice where

import Jambhala.Plutus
    ( makeLift,
      ScriptContext(scriptContextTxInfo),
      CurrencySymbol,
      AssetClass,
      PubKeyHash,
      POSIXTime, fromBuiltinData,
      unstableMakeIsData, OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash), Datum (..), Validator, CompiledCode,
      compile, TxOut, txOutDatum, getContinuingOutputs, adaSymbol, txOutValue, singleton, adaToken, txSignedBy, contains, to, txInfoValidRange, applyCode, liftCode, unsafeFromBuiltinData, TxInfo, txInfoInputs, txInfoOutputs
      )
import Jambhala.Utils
import Jambhala.CLI.Types ( ValidatorContract )

import qualified PlutusTx.Prelude as P
import           PlutusTx          ( compile, applyCode, unstableMakeIsData, FromData(fromBuiltinData), makeLift, liftCode, CompiledCode )
import           Plutus.V1.Ledger.Value       ( geq, AssetClass (AssetClass), gt, singleton, adaToken, adaSymbol, symbols)
import           Plutus.V1.Ledger.Interval (contains)
import           Plutus.V2.Ledger.Contexts as V2
import           Plutus.V2.Ledger.Api         ( mkValidatorScript, to, Validator, PubKeyHash, Validator,
                                                POSIXTime, UnsafeFromData (unsafeFromBuiltinData), OutputDatum(OutputDatumHash, NoOutputDatum, OutputDatum),
                                                Datum(Datum), CurrencySymbol)
import Utilities.Serialise

data ProjectType = Approved | PoolProject deriving Show
makeLift ''ProjectType
unstableMakeIsData ''ProjectType

data Goal = SDG1 | SDG2 | SDG3 | SDG4 | SDG5 | SDG6 | SDG7 | SDG8 | SDG9 | SDG10 | SDG11 | SDG12 | SDG13 | SDG14 | SDG15 | SDG16 deriving Show
unstableMakeIsData ''Goal

type Impact = [Goal]

data Milestone = Milestone
    { title          :: BuiltinByteString
    , amountRequired :: Integer
    , timeRange      :: Integer
    , proposedImpact :: [Impact]
    , dependencies   :: [Milestone]
    } deriving Show
unstableMakeIsData ''Milestone

type Schedule = [Milestone]

data ProjectParams = ProjectParams
    { projectFundingTarget :: Integer
    , projectFundingDeadline :: POSIXTime
    , projectCreator  :: PubKeyHash
    , creatorUniqueToken :: AssetClass
    } deriving Show
makeLift ''ProjectParams

data ProjectDatum = ProjectDatum
    { spendingMintingPolicyId      :: CurrencySymbol
    , fundingAckTokMintingPolicyId :: CurrencySymbol
    , proposalTokMintingPolicyId   :: CurrencySymbol
    , projectOwnerTokMintPolicyId  :: CurrencySymbol
    , projectFunders               :: [PubKeyHash]
    , projectOwners                :: [PubKeyHash]
    , fundingAmount                :: Integer
    , fundingAckAmount             :: Integer
    , currentProposalAmount        :: Integer
    } deriving Show
unstableMakeIsData ''ProjectDatum

type ProposalTitle = BuiltinByteString
type ReportDocument = BuiltinByteString

data ProjectAction = Fund PubKeyHash
                   | MoveFundsProposal PubKeyHash
                   | MoveFunds (Integer, PubKeyHash)
                   | SubmitReport ReportDocument
unstableMakeIsData ''ProjectAction

{-# INLINABLE mkProjectsValidator #-}
mkProjectsValidator :: ProjectParams -> ProjectDatum -> ProjectAction -> ScriptContext -> Bool
mkProjectsValidator project dat red ctx =
    case red of
        Fund pkh                        ->   traceIfFalse "Amount is not greater than five" checkContributionAmount                                   &&
                                             traceIfFalse "Signed By contributor" (signedByContributor pkh)                                           &&
                                             traceIfFalse "Datum has not updated" (checkDatumUpdate pkh)                                              &&
                                             traceIfFalse "Funding token not minted and transferred" (checkFundingTokenTransfer pkh)                  &&
                                             traceIfFalse "Deadline has passed" checkDeadlinePassed                                                   &&
                                             traceIfFalse "Target has reached" checkFundingTargetReached
        
        MoveFundsProposal pkh           ->   traceIfFalse "Not signed by project funder" (signedByProjectFunder pkh)                                  &&
                                             traceIfFalse "Signed by project creator" (not (signedByProjectCreator (projectCreator project)))         &&
                                             traceIfFalse "Proposal Token not minted and transferred" checkProposalTokenTransfer
        
        MoveFunds (_, _)                ->   traceIfFalse "Expenditure token not in input" (checkTokenInInputs $ spendingMintingPolicyId dat)         &&
                                             traceIfFalse "Proposal token not in input" (checkTokenInInputs $ proposalTokMintingPolicyId dat)         &&
                                             traceIfFalse "Datum has not updated" (checkDatumUpdate (projectCreator project))                         &&
                                             traceIfFalse "Funds exceeding proposal amount" checkExpenditureFundsAmount                               &&
                                             traceIfFalse "Transaction not signed by authorized entity" (signedByProjectCreator (projectCreator project))
        
        SubmitReport _                  ->   False
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        datumInOutput :: Maybe ProjectDatum
        datumInOutput = case txInfoOutputs info of
            []   -> Nothing
            outs -> Just =<<
              parseProjectDatum (txOutDatum (P.head $ P.take 1 outs)) info

        checkContributionAmount :: Bool
        checkContributionAmount = case getContinuingOutputs ctx of
          [] -> traceError "No outputs paying to the script"
          os -> let payingOutVal = P.mconcat $ P.filter (\outV -> adaSymbol `P.elem` symbols outV) $ map txOutValue os
                in payingOutVal `gt` singleton adaSymbol adaToken 5_000_000

        signedByContributor :: PubKeyHash -> Bool
        signedByContributor contributorPkh = txSignedBy info contributorPkh

        checkDatumUpdate :: PubKeyHash -> Bool
        checkDatumUpdate contPkh = case txInfoOutputs info of
          [] -> traceError "No outputs in the transaction"
          tos -> P.any hasFunderPkh tos && P.any hasIncreasedAmount tos
            where
                hasFunderPkh, hasIncreasedAmount:: TxOut -> Bool
                hasFunderPkh o = case parseProjectDatum (txOutDatum o) info of
                                        Nothing -> False
                                        Just pd -> P.elem contPkh $ projectFunders pd
                hasIncreasedAmount o = case parseProjectDatum (txOutDatum o) info of
                                        Nothing -> False
                                        Just pd -> valueProduced info `geq` singleton adaSymbol adaToken (fundingAmount pd)

        checkFundingTokenTransfer :: PubKeyHash -> Bool
        checkFundingTokenTransfer contrPkh = fundingAckTokMintingPolicyId dat `P.elem` symbols (valuePaidTo info contrPkh)

        checkDeadlinePassed :: Bool
        checkDeadlinePassed = not (to (projectFundingDeadline project) `contains` txInfoValidRange info)

        checkFundingTargetReached :: Bool
        checkFundingTargetReached = case txInfoOutputs info of
                                            [] -> traceError "No transaction outputs found"
                                            tout : _ -> case parseProjectDatum (txOutDatum tout) info of
                                                                Nothing -> traceError "datum not found"
                                                                Just pd -> fundingAmount pd P.<= projectFundingTarget project

        checkProposalTokenTransfer :: Bool
        checkProposalTokenTransfer = P.any (\funder -> proposalTokMintingPolicyId dat `P.elem` symbols (valuePaidTo info funder)) $ projectFunders dat

        signedByProjectFunder :: PubKeyHash -> Bool
        signedByProjectFunder projFunder = txSignedBy info projFunder 

        signedByProjectCreator :: PubKeyHash -> Bool
        signedByProjectCreator creatorPkh = txSignedBy info creatorPkh

        checkExpenditureFundsAmount :: Bool
        checkExpenditureFundsAmount = case txInfoOutputs info of
                                                [] -> traceError "Transaction doesn't have any outputs"
                                                tos -> let adaValues = P.filter (\outV -> adaSymbol `P.elem` symbols outV) $ map txOutValue tos 
                                                       in not (P.mconcat adaValues `geq` singleton adaSymbol adaToken (currentProposalAmount dat))

        checkTokenInInputs :: CurrencySymbol -> Bool
        checkTokenInInputs cs = case getContinuingOutputs ctx of
                                            [] -> traceError "No inputs in transaction"
                                            txIns -> P.any (\txIn -> cs `P.elem` symbols (txOutValue txIn)) txIns

{-# INLINABLE parseProjectDatum #-}
parseProjectDatum :: OutputDatum -> TxInfo -> Maybe ProjectDatum
parseProjectDatum o info = case o of
    NoOutputDatum -> Nothing
    OutputDatum (Datum d) -> fromBuiltinData d
    OutputDatumHash dh -> do
                        Datum d <- findDatum dh info
                        fromBuiltinData d

{-# INLINABLE mkWrappedProjectsValidator #-}
mkWrappedProjectsValidator :: ProjectParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedProjectsValidator = wrapValidator . mkProjectsValidator

typedProjectsValidator :: ProjectParams -> Validator
typedProjectsValidator pp = mkValidatorScript
    ($$(compile [|| mkWrappedProjectsValidator ||])
    `applyCode`
    liftCode pp)

{-# INLINABLE mkWrappedProjectValidatorLucid #-}
mkWrappedProjectValidatorLucid :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedProjectValidatorLucid pft pfd pc ctokcs ctoktn = wrapValidator $ mkProjectsValidator pp
    where
        pp = ProjectParams
            { projectFundingTarget = unsafeFromBuiltinData pft
            , projectFundingDeadline = unsafeFromBuiltinData pfd
            , projectCreator = unsafeFromBuiltinData pc
            , creatorUniqueToken = AssetClass (unsafeFromBuiltinData ctokcs, unsafeFromBuiltinData ctoktn)
            }

projectsValidatorCodeLucid :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
projectsValidatorCodeLucid = $$(compile [|| mkWrappedProjectValidatorLucid ||])


