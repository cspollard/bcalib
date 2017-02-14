{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module BCalib.IP2D where

import GHC.Float
import GHC.Generics hiding (to)

import Data.Map.Strict as M

import BCalib.Histograms


data IP2DInfo =
    IP2DInfo
        { _ip2dNTrk :: Int
        , _ip2dLLR :: Double
        , _ip2dPu :: Double
        , _ip2dPc :: Double
        , _ip2dPb :: Double
        } deriving (Generic, Show)


ip2dHs :: Fill IP2DInfo
ip2dHs = M.unions <$> sequenceA
    [ fillH1L (ip2dNTrk.integralL) "/ip2dntrk" $ yodaHist 20 0 20 "IP2D track multiplicity" (dsigdXpbY "n" "1")
    , fillH1L ip2dLLR "/ip2dllr" $ yodaHist 50 (-20) 30 "IP2D LLR" (dsigdXpbY "\\mathrm{LLR}" "1")
    -- , fillH1L ip2dPu "/ip2dpu" $ yodaHist 50 0 1 "IP2D P(light)" (dsigdXpbY "P" "1")
    -- , fillH1L ip2dPc "/ip2dpc" $ yodaHist 50 0 1 "IP2D P(charm)" (dsigdXpbY "P" "1")
    -- , fillH1L ip2dPb "/ip2dpb" $ yodaHist 50 0 1 "IP2D P(bottom)" (dsigdXpbY "P" "1")
    ]

    where
        integralL :: (Num a, Integral s, Profunctor p, Contravariant f) => Optic' p f s a
        integralL = to fromIntegral


readIP2Ds :: MonadIO m => TR m (ZipList IP2DInfo)
readIP2Ds = do
    ntrk <- readI "jetsIP2D_ntrk"
    llr <- readD "jetsIP2D_loglikelihoodratio"
    pu <- readD "jetsIP2D_pu"
    pc <- readD "jetsIP2D_pc"
    pb <- readD "jetsIP2D_pb"

    return $ IP2DInfo
        <$> ntrk
        <*> llr
        <*> pu
        <*> pc
        <*> pb

    where
        readD n = fmap float2Double <$> readBranch n
        readI n = fmap (fromEnum :: CInt -> Int) <$> readBranch n


-- TODO
-- can't get TH working with external libs (e.g. root)

ip2dLLR :: Lens' IP2DInfo Double
ip2dLLR f_ajeI (IP2DInfo x1_ajeJ x2_ajeK x3_ajeL x4_ajeM x5_ajeN)
  = fmap
      (\ y1_ajeO -> IP2DInfo x1_ajeJ y1_ajeO x3_ajeL x4_ajeM x5_ajeN)
      (f_ajeI x2_ajeK)
{-# INLINE ip2dLLR #-}
ip2dNTrk :: Lens' IP2DInfo Int
ip2dNTrk f_ajeP (IP2DInfo x1_ajeQ x2_ajeR x3_ajeS x4_ajeT x5_ajeU)
  = fmap
      (\ y1_ajeV -> IP2DInfo y1_ajeV x2_ajeR x3_ajeS x4_ajeT x5_ajeU)
      (f_ajeP x1_ajeQ)
{-# INLINE ip2dNTrk #-}
ip2dPb :: Lens' IP2DInfo Double
ip2dPb f_ajeW (IP2DInfo x1_ajeX x2_ajeY x3_ajeZ x4_ajf0 x5_ajf1)
  = fmap
      (\ y1_ajf2 -> IP2DInfo x1_ajeX x2_ajeY x3_ajeZ x4_ajf0 y1_ajf2)
      (f_ajeW x5_ajf1)
{-# INLINE ip2dPb #-}
ip2dPc :: Lens' IP2DInfo Double
ip2dPc f_ajf3 (IP2DInfo x1_ajf4 x2_ajf5 x3_ajf6 x4_ajf7 x5_ajf8)
  = fmap
      (\ y1_ajf9 -> IP2DInfo x1_ajf4 x2_ajf5 x3_ajf6 y1_ajf9 x5_ajf8)
      (f_ajf3 x4_ajf7)
{-# INLINE ip2dPc #-}
ip2dPu :: Lens' IP2DInfo Double
ip2dPu f_ajfa (IP2DInfo x1_ajfb x2_ajfc x3_ajfd x4_ajfe x5_ajff)
  = fmap
      (\ y1_ajfg -> IP2DInfo x1_ajfb x2_ajfc y1_ajfg x4_ajfe x5_ajff)
      (f_ajfa x3_ajfd)
{-# INLINE ip2dPu #-}
