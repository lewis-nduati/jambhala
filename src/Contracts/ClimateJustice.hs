{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant where" #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# HLINT ignore "Use fewer imports" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE StrictData #-}
module Contracts.ClimateJustice where

import Jambhala.Plutus
    ( makeLift,
      ScriptContext(scriptContextTxInfo),
      CurrencySymbol,
      AssetClass,
      PubKeyHash,
      POSIXTime,
      TxInfo(txInfoOutputs),
      unstableMakeIsData, OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash), Datum (..), Validator, CompiledCode,
      compile
      )
import Jambhala.CLI.Types ( ValidatorContract )
import Options.Applicative ()
import Prelude hiding (not, Bool, (.), ($), (&&))
import GHC.Base ( Bool, (&&), not )


data ProjectType = Approved | PoolProject deriving Show

Jambhala.Plutus.makeLift ''ProjectType

unstableMakeIsData ''ProjectType

data Goal = SDG1 | SDG2 | SDG3 | SDG4 | SDG5 | SDG6 | SDG7 | SDG8 | SDG9 | SDG10 | SDG11 | SDG12 | SDG13 | SDG14 | SDG15 | SDG16 deriving Show

unstableMakeIsData ''Goal

type Impact =[Goal]

data Milestone where
  Milestone :: {title :: BuiltinByteString,
                amountRequired :: Integer,
                timeRange :: Integer,
                proposedImpact :: [Impact],
                dependencies :: [Milestone]}
               -> Milestone


unstableMakeIsData ''Milestone

type Schedule = [Milestone]

data ProjectParams = ProjectParams
    { projectFundingTarget  :: Integer
    , projectFundingDeadline :: POSIXTime
    , projectCreator         :: PubKeyHash
    , creatorUniqueToken     :: AssetClass
    } deriving (Generic, FromJSON, ToJSON)

makeLift ''ProjectParams

data ProjectDatum = ProjectDatum
    { spendingMintingPolicyId      :: CurrencySymbol
    , fundingAckToMintingPolicyId  :: CurrencySymbol
    , proposalTokMintingPolicyId   :: CurrencySymbol
    , projectOwnerToMintPolicyId   :: CurrencySymbol
    , projectFunders               :: [PubKeyHash]
    , projectOwners                :: [PubKeyHash]
    , fundingAmount                :: Integer
    , fundingAckAmount             :: Integer
    } deriving Show

unstableMakeIsData ''ProjectDatum

type ProposalTitle = BuiltinByteString

type ReportDocument = BuiltinByteString

data ProjectAction
    = Fund PubKeyHash
    | MoveFundsProposal PubKeyHash
    | MoveFunds (Integer, PubKeyHash)
    | SubmitReport ReportDocument

unstableMakeIsData ''ProjectAction

{-# INLINABLE mkProjectsValidator #-}
mkProjectsValidator :: ProjectParams -> ProjectDatum -> ProjectAction -> ScriptContext -> Bool
mkProjectsValidator project dat red ctx = case red of
        Fund pkh -> traceIfFalse "Amount is not greater than five" checkContributionAmount &&
                    traceIfFalse "Signed By contributor" (signedByContributor pkh) &&
                    traceIfFalse "Datum not updated" (checkDatumUpdate pkh) &&
                    traceIfFalse "Funding token not minted and transferred" (checkFundingTokenTransfer pkh) &&
                    traceIfFalse "Deadline has not passed" checkDeadlinePassed &&
                    traceIfFalse "Target has not reached" checkFundingTargetReached

        MoveFundsProposal pkh -> signedBy pkh &&
                                  not (signedBy (projectCreator project)) &&
                                  checkProposalTokenTransfer

        MoveFunds (_, _) -> checkTokenInInputs (spendingMintingPolicyId dat) &&
                            checkTokenInInputs (proposalTokMintingPolicyId dat) &&
                            checkDatumUpdate (projectCreator project) &&
                            checkExpenditureFundsAmount &&
                            signedBy (projectCreator project)

        SubmitReport _ -> False


checkDeadlinePassed :: Bool
checkDeadlinePassed = _

checkContributionAmount :: Bool
checkContributionAmount = _

checkFundingTargetReached :: Bool
checkFundingTargetReached = _

checkFundingTokenTransfer :: PubKeyHash -> Bool
checkFundingTokenTransfer = _

checkDatumUpdate :: PubKeyHash -> Bool
checkDatumUpdate = _

checkExpenditureFundsAmount :: Bool
checkExpenditureFundsAmount = _

checkProposalTokenTransfer :: Bool
checkProposalTokenTransfer = _

checkTokenInInputs :: CurrencySymbol -> Bool
checkTokenInInputs = _

datumInOutput :: Maybe ProjectDatum
datumInOutput = _

signedByProjectFunder :: PubKeyHash -> Bool
signedByProjectFunder = _

parseProjectDatum :: OutputDatum -> TxInfo -> Maybe ProjectDatum
parseProjectDatum o info = case o of
        NoOutputDatum -> Nothing
        OutputDatum (Datum d) -> fromBuiltinData d
        OutputDatumHash dh -> do
                            Datum d <- findDatum dh info
                            fromBuiltinData d
