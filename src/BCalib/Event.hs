{-# LANGUAGE DeriveGeneric #-}

module BCalib.Event
    ( module X
    , Event(..)
    , runNumber, eventNumber, mu
    , leptons, jets, met, eventWeight
    ) where

import Control.Lens

import GHC.Generics (Generic)
import GHC.Float (float2Double)

import Data.HEP.LorentzVector as X
import Data.TTree

import BCalib.Jet as X
import BCalib.Lepton as X


data LFlavor = Electron | Muon
    deriving (Generic, Show)
data LCharge = Plus | Minus
    deriving (Generic, Show)

data Lepton =
    Lepton
        { _lflavor :: LFlavor
        , _charge :: LCharge
        , _lfourmom :: PtEtaPhiE
        } deriving (Generic, Show)

data Event =
    Event
        { _runNumber :: Int
        , _eventNumber :: Int
        , _mu :: Double
        , _leptons :: (Lepton, Lepton)
        , _jets :: [Jet]
        , _met :: PtEtaPhiE
        , _eventWeight :: Double
        } deriving (Generic, Show)


-- TH doesn't work with c compilation (e.g. ttree)
runNumber :: Lens' Event Int
runNumber = lens _runNumber $ \e x -> e { _runNumber = x }

eventNumber :: Lens' Event Int
eventNumber = lens _eventNumber $ \e x -> e { _eventNumber = x }

mu :: Lens' Event Double
mu = lens _mu $ \e x -> e { _mu = x }

leptons :: Lens' Event (Lepton, Lepton)
leptons = lens _leptons $ \e x -> e { _leptons = x }

jets :: Lens' Event [Jet]
jets = lens _jets $ \e x -> e { _jets = x }

met :: Lens' Event PtEtaPhiE
met = lens _met $ \e x -> e { _met = x }

eventWeight :: Lens' Event Double
eventWeight = lens _eventWeight $ \e x -> e { _eventWeight = x }


readMET :: MonadIO m => String -> String -> TR m PtEtaPhiE
readMET m p = do
    et <- float2Double <$> readBranch m
    phi <- float2Double <$> readBranch p
    return $ PtEtaPhiE et 0 phi et

ci2i :: CInt -> Int
ci2i = fromEnum

readLepton = error "readLepton not implemented"


weight :: MonadIO m => TR m Double
weight = float2Double . product
    <$> sequence
        [ readBranch "eventWeight"
        , readBranch "MCEventWeight"
        , readBranch "PileupWeight"
        , readBranch "leptonSF"
        , readBranch "trigSF"
        ]


instance FromTTree Event where
    fromTTree =
        Event
            <$> fmap ci2i (readBranch "runNumber")
            <*> fmap ci2i (readBranch "eventNumber")
            <*> fmap float2Double (readBranch "mu")
            <*> ((,) <$> readLepton "Lep" <*> readLepton "SecLep")
            <*> readJets
            <*> readMET "MET" "METphi"
            <*> weight
