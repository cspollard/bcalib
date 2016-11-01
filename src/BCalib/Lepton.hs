{-# LANGUAGE DeriveGeneric #-}

module BCalib.Lepton
    ( module X
    , Lepton(Lepton)
    , lepFlavor, lepCharge, readLeptons
    ) where

import Control.Lens

import Foreign.C.Types (CInt)
import GHC.Generics
import GHC.Float (float2Double)

import Data.HEP.LorentzVector as X
import Data.TTree


data LFlavor = Electron | Muon
    deriving (Generic, Show)

data LCharge = Plus | Minus
    deriving (Generic, Show)

data Lepton =
    Lepton
        { _flavor :: LFlavor
        , _charge :: LCharge
        , _lfourmom :: PtEtaPhiE
        } deriving (Generic, Show)

instance HasLorentzVector Lepton where
    toPtEtaPhiE = lens _lfourmom $ \l x -> l { _lfourmom = x }

lepFlavor :: Lens' Lepton LFlavor
lepFlavor = lens _flavor $ \l x -> l { _flavor = x }

lepCharge :: Lens' Lepton LCharge
lepCharge = lens _charge $ \l x -> l { _charge = x }

readLeptons :: MonadIO m => TR m (Lepton, Lepton)
readLeptons = do
    pt1 <- float2Double <$> readBranch "ptLep"
    eta1 <- float2Double <$> readBranch "etaLep"
    phi1 <- float2Double <$> readBranch "phiLep"
    let p1 = PtEtaPhiE pt1 eta1 phi1 $ pt1 * cosh eta1
    lc1 <- readBranch "signLep"
    let lcharge1 = case (lc1 :: CInt) of
                    (-1) -> Minus
                    ( 1) -> Plus
                    x    -> error $ "invalid signLep: " ++ show x

    pt2 <- float2Double <$> readBranch "ptSecLep"
    eta2 <- float2Double <$> readBranch "etaSecLep"
    phi2 <- float2Double <$> readBranch "phiSecLep"
    let p2 = PtEtaPhiE pt2 eta2 phi2 $ pt2 * cosh eta2
    lc2 <- readBranch "signSecLep"
    let lcharge2 = case (lc2 :: CInt) of
                    (-1) -> Minus
                    ( 1) -> Plus
                    x    -> error $ "invalid signSecLep: " ++ show x

    lchan <- readBranch "leptonChannel"
    if (lchan :: CInt) == 00 || lchan == 10
        then
            let elec = Lepton Electron lcharge1 p1
                muon = Lepton Muon lcharge2 p2
            in if pt1 > pt2
                then return (elec, muon)
                else return (muon, elec)
    else if lchan == 1 || lchan == 11
        then return
            ( Lepton Electron lcharge1 p1
            , Lepton Electron lcharge2 p2
            )
    else if lchan == 2 || lchan == 22
        then return
            ( Lepton Muon lcharge1 p1
            , Lepton Muon lcharge2 p2
            )
    else error $ "invalid leptonChannel: " ++ show lchan