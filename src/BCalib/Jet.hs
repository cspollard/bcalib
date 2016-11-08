{-# LANGUAGE DeriveGeneric #-}

module BCalib.Jet
    ( module X
    , Jet(Jet)
    , JetFlavor(..)
    , mv2c00, mv2c10, mv2c20, mv2c100, mv2cl100
    , ip2dLLR, ip3dLLR, sv1LLR, jfLLR
    , truthFlavor
    , readJets
    ) where

import Control.Lens
import Control.Applicative (ZipList(..))

import Foreign.C.Types (CInt)
import GHC.Generics
import GHC.Float (float2Double)

import Data.HEP.LorentzVector as X
import Data.TTree


data JetFlavor = L | C | B
    deriving (Generic, Show, Eq, Ord)

flavFromCInt :: CInt -> JetFlavor
flavFromCInt x = case x of
                    5 -> B
                    4 -> C
                    0 -> L
                    _ -> error $ "bad jet flavor label: " ++ show x

data Jet =
    Jet
        { _jfourmom :: PtEtaPhiE
        , _mv2c00 :: Double
        , _mv2c10 :: Double
        , _mv2c20 :: Double
        , _mv2c100 :: Double
        , _mv2cl100 :: Double
        , _ip2dLLR :: Double
        , _ip3dLLR :: Double
        , _sv1LLR :: Double
        , _jfLLR :: Double
        , _jTruthFlavor :: Maybe JetFlavor
        } deriving (Generic, Show)

instance HasLorentzVector Jet where
    toPtEtaPhiE = lens _jfourmom $ \j x -> j { _jfourmom = x }

mv2c00, mv2c10, mv2c20, mv2c100, mv2cl100
    :: Lens' Jet Double
mv2c00 = lens _mv2c00 $ \j x -> j { _mv2c00 = x }
mv2c10 = lens _mv2c10 $ \j x -> j { _mv2c10 = x }
mv2c20 = lens _mv2c20 $ \j x -> j { _mv2c20 = x }
mv2c100 = lens _mv2c100 $ \j x -> j { _mv2c100 = x }
mv2cl100 = lens _mv2cl100 $ \j x -> j { _mv2cl100 = x }

ip2dLLR, ip3dLLR, sv1LLR, jfLLR
    :: Lens' Jet Double
ip2dLLR = lens _ip2dLLR $ \j x -> j { _ip2dLLR = x }
ip3dLLR = lens _ip3dLLR $ \j x -> j { _ip3dLLR = x }
sv1LLR = lens _sv1LLR $ \j x -> j { _sv1LLR = x }
jfLLR = lens _jfLLR $ \j x -> j { _jfLLR = x }

truthFlavor :: Lens' Jet (Maybe JetFlavor)
truthFlavor = lens _jTruthFlavor $ \j x -> j { _jTruthFlavor = x }

lvsFromTTree :: MonadIO m => String -> String -> String -> TR m (ZipList PtEtaPhiE)
lvsFromTTree ptn etan phin = do
    pts <- fmap float2Double <$> readBranch ptn
    etas <- fmap float2Double <$> readBranch etan
    phis <- fmap float2Double <$> readBranch phin

    let es = (\pt eta -> pt * cosh eta) <$> pts <*> etas

    return $ PtEtaPhiE <$> pts <*> etas <*> phis <*> es


readJets :: MonadIO m => TR m (ZipList Jet)
readJets = do
    fourmoms <- lvsFromTTree "jetsMomPt" "jetsMomEta" "jetsMomPhi"
    mv2c00s <- fmap float2Double <$> readBranch "jetsMV2c00"
    mv2c10s <- fmap float2Double <$> readBranch "jetsMV2c10"
    mv2c20s <- fmap float2Double <$> readBranch "jetsMV2c20"
    mv2c100s <- fmap float2Double <$> readBranch "jetsMV2c100"
    mv2cl100s <- fmap float2Double <$> readBranch "jetsMV2cl100"
    ip2dLLRs <- fmap float2Double <$> readBranch "jetsIP2D_loglikelihoodratio"
    ip3dLLRs <- fmap float2Double <$> readBranch "jetsIP3D_loglikelihoodratio"
    sv1LLRs <- fmap float2Double <$> readBranch "jetsSV1_loglikelihoodratio"
    sfLLRs <- fmap float2Double <$> readBranch "jetsJetFitter_loglikelihoodratio"

    sid <- readBranch "sampleID"
    flvs <- if (sid :: CInt) == 0
                then return $ ZipList (repeat Nothing)
                else fmap (Just . flavFromCInt) <$> readBranch "jetsTrueFlavor"

    return $ Jet
            <$> fourmoms
            <*> mv2c00s
            <*> mv2c10s
            <*> mv2c20s
            <*> mv2c100s
            <*> mv2cl100s
            <*> ip2dLLRs
            <*> ip3dLLRs
            <*> sv1LLRs
            <*> sfLLRs
            <*> flvs
