{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module BCalib.Event
    ( module X
    , Event(..)
    , runNumber, eventNumber, mu
    , leptons, jets, met, eventWeight
    , eventHs
    ) where

import Control.Lens
import Control.Applicative (getZipList)
import qualified Control.Foldl as F

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


jetsHs :: Fills Event
jetsHs = (F.handles traverse jetHs <$= sequenceA)
                <> sequenceA (ZipList [nH 10]) 
            <$$= jets
            <&> fmap (over path ("/jets" <>) . over xlabel ("jet " <>))

lepsHs :: Fills Event
lepsHs =
    -- TODO
    -- must be better way to write this...
    F.handles traverse lepHs <$= (\(w, e) -> let (l1, l2) = view leptons e in [(w, l1), (w, l2)])
    <&> fmap (over path ("/leps" <>) . over xlabel ("lep " <>))

muH :: Fill Event
muH = fillH1L mu $
    yodaHist 50 0 50 "/mu" "$ <\\mu> $" (dsigdXpbY "<\\mu>" "1")


eventHs :: Fills Event
eventHs = lepsHs <> jetsHs <> sequenceA (ZipList [muH])


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
            <*> fmap getZipList (readJets isData)
            <*> readMET "MET" "METphi"
            <*> weight

        where
            ci2i :: CInt -> Int
            ci2i = fromEnum

            convMu True = id
            convMu False = (*1.09)
