{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BCalib.Lepton
    ( Lepton(Lepton)
    , lepFlavor, lepCharge, readLeptons
    , LFlavor(..), LCharge(..)
    , lepHs
    ) where

import Control.Lens

import Foreign.C.Types (CInt)
import GHC.Generics
import GHC.Float (float2Double)

import Data.HEP.LorentzVector
import Data.TTree
import BCalib.Histograms


data LFlavor = Electron | Muon
    deriving (Generic, Show, Ord, Eq)

data LCharge = Plus | Minus
    deriving (Generic, Show, Ord, Eq)

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

lepHs :: Fill Lepton
lepHs = lvHs

readLeptons :: MonadIO m => TR m (Lepton, Lepton)
readLeptons = do
    pt1 <- float2Double . (/ 1e3) <$> readBranch "ptLep"
    eta1 <- float2Double <$> readBranch "etaLep"
    phi1 <- float2Double <$> readBranch "phiLep"
    let p1 = PtEtaPhiE pt1 eta1 phi1 $ pt1 * cosh eta1
    (lc1 :: CInt) <- readBranch "signLep"
    let lcharge1 = if lc1 > 0
                    then Plus
                    else Minus

    pt2 <- float2Double <$> readBranch "ptSecLep"
    eta2 <- float2Double <$> readBranch "etaSecLep"
    phi2 <- float2Double <$> readBranch "phiSecLep"
    let p2 = PtEtaPhiE pt2 eta2 phi2 $ pt2 * cosh eta2
    (lc2 :: CInt) <- readBranch "signSecLep"
    let lcharge2 = if lc2 > 0
                    then Plus
                    else Minus

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
    else if lchan == 2 || lchan == 12
        then return
            ( Lepton Muon lcharge1 p1
            , Lepton Muon lcharge2 p2
            )
    else error $ "invalid leptonChannel: " ++ show lchan
