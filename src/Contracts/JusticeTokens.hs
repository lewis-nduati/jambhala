{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Contracts.JusticeTokens
    ( saveProjectOwnerTokenCode
    , saveProjectOwnerTokenPolicy
    , projectOwnerTokenCurrencySymbol
    -- * Expenditure Proposal
    , saveExpenditurePropCode
    -- * Funding Acknowledge
    , saveFundingAcknowledgePolicyCode
    -- * Expenditure Spending
    , saveExpenditureSpendingPolicyCode
    ) where

import Jambhala.Utils
import Jambhala.Plutus
import Jambhala.CLI
import Jambhala.CLI.Emulator.Types
import Contracts.ClimateJustice
import PlutusTx.Prelude hiding ((==))
import qualified Plutus.V1.Ledger.Api as PlutusTx
import qualified PlutusTx
import Prelude hiding ((>), (<), any)

---------------------------------------------------------------------------------------------------
------------------------------------- PROJECT OWNER MINT --------------------------------------------

{-# INLINABLE mkProjectOwnerTokPolicy #-}
mkProjectOwnerTokPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkProjectOwnerTokPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"    hasUTxO      &&
                                         traceIfFalse "Wrong amount minted" checkMintedAmount
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        hasUTxO :: Bool
        hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

        checkMintedAmount :: Bool
        checkMintedAmount = case flattenValue (txInfoMint info) of
                                [(_, tn', amt)] -> tn' == tn && amt == 1
                                _                -> False

{-# INLINABLE mkWrappedProjectOwnerTokPolicy #-}
mkWrappedProjectOwnerTokPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedProjectOwnerTokPolicy tid ix tn = wrapPolicy (mkProjectOwnerTokPolicy oref tn)
    where
        oref :: TxOutRef
        oref = TxOutRef
            (TxId $ PlutusTx.unsafeFromBuiltinData tid)
            (PlutusTx.unsafeFromBuiltinData ix)

        tn :: TokenName
        tn = PlutusTx.unsafeFromBuiltinData tn

projectOwnerTokenCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
projectOwnerTokenCode = $$(PlutusTx.compile [|| mkWrappedProjectOwnerTokPolicy ||])

projectOwnerTokenPolicy :: TxOutRef -> TokenName -> MintingPolicy
projectOwnerTokenPolicy oref tn = mkMintingPolicyScript $
    projectOwnerTokenCode
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ getTxId $ txOutRefId oref)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ txOutRefIdx oref)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData tn)

---------------------------------------------------------------------------------------------------
------------------------------------- FUNDING ACKNOWLEDGE TOKEN ------------------------------------

data FundingAckParams = FundingAckParams
        { projectValidatorHash    :: ValidatorHash
        , initialFundingAckSupply :: Integer
        , fundingAckTokenName     :: TokenName
        } deriving Show
PlutusTx.makeLift ''FundingAckParams

data FundingAckMintRedeemer = InitialMint | Mint | Burn
PlutusTx.unstableMakeIsData ''FundingAckMintRedeemer

{-# INLINABLE mkFundingAcknowledgePolicy #-}
mkFundingAcknowledgePolicy :: FundingAckParams -> FundingAckMintRedeemer -> ScriptContext -> Bool
mkFundingAcknowledgePolicy fundAckParams red ctx = case red of
    InitialMint -> traceIfFalse "minted amount must be positive" checkMintPositive && 
                   traceIfFalse "invalid datum at project output" checkDatum       &&
                   traceIfFalse "Invalid mint amount" checkInitialMintValue
    Mint   -> traceIfFalse "minted amount must be positive" checkMintPositive  
    Burn   -> traceIfFalse "Minting instead of burning!" checkBurnNegative
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        mintedAmount :: Integer
        mintedAmount = assetClassValueOf (txInfoMint info) (AssetClass (ownCurrencySymbol ctx, fundingAckTokenName fundAckParams))

        checkMintPositive :: Bool
        checkMintPositive = mintedAmount > 0

        checkBurnNegative :: Bool
        checkBurnNegative = mintedAmount < 0

        checkInitialMintValue :: Bool 
        checkInitialMintValue = mintedAmount == initialFundingAckSupply fundAckParams

        projectOutput :: (OutputDatum , Value)
        projectOutput = case scriptOutputsAt (projectValidatorHash fundAckParams) info of
                        outs -> head outs
                        []   -> traceError "expected a project output"

        projectOutputDatum :: Maybe ProjectDatum
        projectOutputDatum = parseProjectDatum d info
            where
                (d,_) = projectOutput

        checkDatum :: Bool
        checkDatum = case projectOutputDatum of
            Nothing -> False
            Just d  -> fundingAckAmount d PlutusTx.Prelude.>= mintedAmount &&
                       any (txSignedBy info) (projectFunders d)

{-# INLINABLE mkWrappedFundingAcknowledgePolicy #-}
mkWrappedFundingAcknowledgePolicy :: FundingAckParams -> BuiltinData -> BuiltinData -> ()
mkWrappedFundingAcknowledgePolicy = wrapPolicy . mkFundingAcknowledgePolicy

fundingAcknowledgePolicy :: FundingAckParams -> MintingPolicy
fundingAcknowledgePolicy fAckParam = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkWrappedFundingAcknowledgePolicy ||])
        `PlutusTx.applyCode`
        PlutusTx.liftCode fAckParam

{-# INLINABLE mkWrappedFundingAcknowledgePolicyLucid #-}
mkWrappedFundingAcknowledgePolicyLucid :: BuiltinData ->  BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedFundingAcknowledgePolicyLucid pValHash initSup tn = wrapPolicy $ mkFundingAcknowledgePolicy fAckParam
    where
        fAckParam = FundingAckParams
            { projectValidatorHash    = unsafeFromBuiltinData pValHash
            , initialFundingAckSupply = unsafeFromBuiltinData initSup
            , fundingAckTokenName     = unsafeFromBuiltinData tn
            }

fundingAcknowledgePolicyCodeLucid :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
fundingAcknowledgePolicyCodeLucid = $$(PlutusTx.compile [|| mkWrappedFundingAcknowledgePolicyLucid ||])

---------------------------------------------------------------------------------------------------
------------------------------------- EXPENDITURE PROPOSAL MINT -----------------------------------

type Proposal = BuiltinByteString 

data ExpenditureProposalParams = ExpenditureProposalParams
        { projectValidator             :: ValidatorHash
        , expenditureProposalTokenName :: TokenName  
        } deriving Show
PlutusTx.makeLift ''ExpenditureProposalParams        

{-# INLINABLE mkExpenditurePropPolicy #-}
mkExpenditurePropPolicy :: ExpenditureProposalParams -> () -> ScriptContext -> Bool
mkExpenditurePropPolicy expPropParams () ctx = traceIfFalse "Project has no datum"    hasDatum      &&
                                               traceIfFalse "Amount not positive" checkMintPositive
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        mintedAmount :: Integer
        mintedAmount = assetClassValueOf (txInfoMint info) (AssetClass (ownCurrencySymbol ctx, expenditureProposalTokenName expPropParams))

        checkMintPositive :: Bool
        checkMintPositive = mintedAmount > 0

        projectOutput :: (OutputDatum , Value)
        projectOutput = case scriptOutputsAt (projectValidator expPropParams) info of
                        []   -> traceError "expected a project output"
                        outs -> head outs

        projectOutputDatum :: Maybe ProjectDatum
        projectOutputDatum = parseProjectDatum d info
            where
                (d,_) = projectOutput

        hasDatum :: Bool
        hasDatum = case projectOutputDatum of            
                            Nothing -> False
                            Just _  -> True

        checkMintedAmount :: Bool
        checkMintedAmount = case flattenValue (txInfoMint info) of
                                [(_, tn', amt)] -> tn' == expenditureProposalTokenName expPropParams && amt == 1
                                _                -> False

{-# INLINABLE mkWrappedExpenditurePropPolicy #-}
mkWrappedExpenditurePropPolicy :: ExpenditureProposalParams -> BuiltinData -> BuiltinData -> ()
mkWrappedExpenditurePropPolicy = wrapPolicy . mkExpenditurePropPolicy

expenditureProposalTokenPolicy :: ExpenditureProposalParams -> MintingPolicy
expenditureProposalTokenPolicy expPropParams = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkWrappedExpenditurePropPolicy ||])
        `PlutusTx.applyCode`
        PlutusTx.liftCode expPropParams

{-# INLINABLE mkWrappedExpenditureProposalPolicyLucid #-}
mkWrappedExpenditureProposalPolicyLucid :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedExpenditureProposalPolicyLucid pValHash propTn = wrapPolicy $ mkExpenditurePropPolicy expPropParam
    where
        expPropParam = ExpenditureProposalParams
            { projectValidator             = unsafeFromBuiltinData pValHash
            , expenditureProposalTokenName = unsafeFromBuiltinData propTn
            }

expenditureProposalTokenPolicyCodeLucid :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
expenditureProposalTokenPolicyCodeLucid = $$(PlutusTx.compile [|| mkWrappedExpenditureProposalPolicyLucid ||])

---------------------------------------------------------------------------------------------------
------------------------------------- EXPENDITURE SPENDING ----------------------------------------

data ExpenditureSpendingParams = ExpenditureSpendingParams
        { projectValidator' :: ValidatorHash
        , expenditureTokenName' :: TokenName  
        } deriving Show
PlutusTx.makeLift ''ExpenditureSpendingParams        

{-# INLINABLE mkExpenditureSpendingPolicy #-}
mkExpenditureSpendingPolicy :: ExpenditureSpendingParams -> () -> ScriptContext -> Bool
mkExpenditureSpendingPolicy expSpendParams () ctx = traceIfFalse "Project has no datum"    hasDatum      &&
                                                    traceIfFalse "Project has no redeemer" hasRedeemer  &&
                                                    traceIfFalse "Amount not positive" checkMintPositive
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        mintedAmount :: Integer
        mintedAmount = assetClassValueOf (txInfoMint info) (AssetClass (ownCurrencySymbol ctx, expenditureTokenName' expSpendParams))

        checkMintPositive :: Bool
        checkMintPositive = mintedAmount > 0

        projectInput :: (OutputDatum , Value)
        projectInput = case scriptInputsAt (projectValidator' expSpendParams) info of
                        []   -> traceError "expected a project input"
                        ins -> head ins

        projectInputDatum :: Maybe ProjectDatum
        projectInputDatum = parseProjectDatum d info
            where
                (d,_) = projectInput

        hasDatum :: Bool
        hasDatum = case projectInputDatum of            
                            Nothing -> False
                            Just _  -> True

        projectInputRedeemer :: Maybe ProjectRedeemer
        projectInputRedeemer = parseProjectRedeemer r info
            where
                (r,_) = projectInput

        hasRedeemer :: Bool
        hasRedeemer = case projectInputRedeemer of            
                            Nothing -> False
                            Just _  -> True

{-# INLINABLE mkWrappedExpenditureSpendingPolicy #-}
mkWrappedExpenditureSpendingPolicy :: ExpenditureSpendingParams -> BuiltinData -> BuiltinData -> ()
mkWrappedExpenditureSpendingPolicy = wrapPolicy . mkExpenditureSpendingPolicy

expenditureSpendingTokenPolicy :: ExpenditureSpendingParams -> MintingPolicy
expenditureSpendingTokenPolicy expSpendParams = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkWrappedExpenditureSpendingPolicy ||])
        `PlutusTx.applyCode`
        PlutusTx.liftCode expSpendParams

{-# INLINABLE mkWrappedExpenditureSpendingPolicyLucid #-}
mkWrappedExpenditureSpendingPolicyLucid :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedExpenditureSpendingPolicyLucid pValHash expTn = wrapPolicy $ mkExpenditureSpendingPolicy expSpendParam
    where
        expSpendParam = ExpenditureSpendingParams
            { projectValidator'    = unsafeFromBuiltinData pValHash
            , expenditureTokenName' = unsafeFromBuiltinData expTn
            }

expenditureSpendingTokenPolicyCodeLucid :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
expenditureSpendingTokenPolicyCodeLucid = $$(PlutusTx.compile [|| mkWrappedExpenditureSpendingPolicyLucid ||])

---------------------------------------------------------------------------------------------------
------------------------------------- SAVE FUNCTIONS ----------------------------------------------

saveProjectOwnerTokenCode :: IO ()
saveProjectOwnerTokenCode = writePolicyToFile "assets/projectOwnerToken.plutus" projectOwnerTokenPolicy

saveExpenditurePropCode :: IO ()
saveExpenditurePropCode = writePolicyToFile "assets/expenditurePropToken.plutus" expenditureProposalTokenPolicy

saveFundingAcknowledgePolicyCode :: IO ()
saveFundingAcknowledgePolicyCode = writePolicyToFile "assets/fundingAcknowledgeToken.plutus" fundingAcknowledgePolicy

saveExpenditureSpendingPolicyCode :: IO ()
saveExpenditureSpendingPolicyCode = writePolicyToFile "assets/expenditureSpendingToken.plutus" expenditureSpendingTokenPolicy
