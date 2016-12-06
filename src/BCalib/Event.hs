{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module BCalib.Event
    ( module X
    , Event(..)
    , runNumber, eventNumber, mu
    , leptons, jets, met, eventWeight
    , eventHs
    , withWeight
    , lepFlavorChannels
    , lepChargeChannels
    , nJetChannels
    , overlapRemoval
    ) where

import Control.Lens
import Control.Arrow ((&&&))
import qualified Control.Foldl as F

import qualified Data.Map.Strict as M

import GHC.Generics (Generic)
import GHC.Float (float2Double)

import Data.HEP.LorentzVector as X
import Data.TTree

import BCalib.Histograms
import BCalib.Jet as X
import BCalib.Lepton as X

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

overlapRemoval :: Event -> Event
overlapRemoval evt = over jets (filter filt) evt
    where
        leps = view leptons evt
        filt = not . any (< 0.2) . traverse lvDREta leps

jetsHs :: Fill Event
jetsHs = (M.unions <$> sequenceA [ allHs, jet0Hs, jet1Hs ]) <$$= jets

    where
        allHs :: F.Fold (Double, [Jet]) YodaFolder
        allHs = (M.union <$> nH 10 <*> (F.handles traverse jetHs <$= sequenceA))
                <&> (M.mapKeysMonotonic ("/jets" <>) . over (traverse.xlabel) ("jet " <>))

        jet0Hs :: F.Fold (Double, [Jet]) YodaFolder
        jet0Hs = F.handles (ix 0) jetHs <$= sequenceA
                <&> (M.mapKeysMonotonic ("/jet0" <>) . over (traverse.xlabel) ("leading jet " <>))

        jet1Hs :: F.Fold (Double, [Jet]) YodaFolder
        jet1Hs = F.handles (ix 1) jetHs <$= sequenceA
                <&> (M.mapKeysMonotonic ("/jet1" <>) . over (traverse.xlabel) ("subleading jet " <>))


lepsHs :: Fill Event
lepsHs =
    -- TODO
    -- must be better way to write this...
    F.handles traverse lepHs <$= (\(w, e) -> let (l1, l2) = view leptons e in [(w, l1), (w, l2)])
    <&> (M.mapKeysMonotonic ("/leps" <>) . over (traverse.xlabel) ("lep " <>))

muH :: Fill Event
muH = fillH1L mu "/mu" $
    yodaHist 50 0 50 "$ <\\mu> $" (dsigdXpbY "<\\mu>" "1")


eventHs :: Fill Event
eventHs = mconcat [ lepsHs, jetsHs, muH ]


readMET :: MonadIO m => String -> String -> TR m PtEtaPhiE
readMET m p = do
    et <- float2Double <$> readBranch m
    phi <- float2Double <$> readBranch p
    return $ PtEtaPhiE et 0 phi et

weight :: MonadIO m => TR m Double
weight = float2Double . product
    <$> sequence
        [ readBranch "eventWeight"
        -- TODO
        -- TODO
        -- some of these are NaNs.
        -- , readBranch "leptonSF"
        -- , readBranch "trigSF"
        ]


instance FromTTree Event where
    fromTTree = do
        isData <- (== (0 :: CInt)) <$> readBranch "sampleID" 
        Event
            <$> fmap ci2i (readBranch "runNumber")
            <*> fmap ci2i (readBranch "eventNumber")
            <*> fmap (convMu isData . float2Double) (readBranch "mu")
            <*> readLeptons
            <*> readJets isData
            <*> readMET "MET" "METphi"
            <*> weight

        where
            ci2i :: CInt -> Int
            ci2i = fromEnum

            convMu False = id
            convMu True = (/1.09)

withWeight :: Event -> (Double, Event)
withWeight = view eventWeight &&& id


leptonFlavors :: (LFlavor, LFlavor) -> Event -> Bool
leptonFlavors flvs e = flvs == (view leptons e & over both (view lepFlavor))

leptonCharges :: (LCharge, LCharge) -> Event -> Bool
leptonCharges chgs e = chgs == (view leptons e & over both (view lepCharge))


lepChargeChannels :: Fill Event -> Fill Event
lepChargeChannels =
    channels
        [ ("/os", (||) <$> leptonCharges (Plus, Minus) <*> leptonCharges (Minus, Plus))
        -- , ("/ss", (||) <$> leptonCharges (Plus, Plus) <*> leptonCharges (Minus, Minus))
        -- , ("/allLepCharge", const True)
        ]

lepFlavorChannels :: Fill Event -> Fill Event
lepFlavorChannels =
    channels 
        [ ("/elmu", (||) <$> leptonFlavors (Electron, Muon) <*> leptonFlavors (Muon, Electron))
        -- , ("/mumu", leptonFlavors (Muon, Muon))
        -- , ("/elel", leptonFlavors (Electron, Electron))
        -- , ("/allLepFlav", const True)
        ]

nJetChannels :: Fill Event -> Fill Event
nJetChannels =
    channels 
        [ ("/2pjet", (>= 2) . views jets length)
        , ("/2jet", (== 2) . views jets length)
        , ("/3jet", (== 3) . views jets length)
        , ("/4pjet", (>= 4) . views jets length)
        ]
