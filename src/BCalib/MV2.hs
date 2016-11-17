{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module BCalib.MV2 where

import GHC.Float
import GHC.Generics hiding (to)

import Data.Map.Strict as M

import BCalib.Histograms


data MV2Info =
    MV2Info
        { _mv2c00 :: Double
        , _mv2c10 :: Double
        , _mv2c20 :: Double
        , _mv2c100 :: Double
        , _mv2cl100 :: Double
        } deriving (Generic, Show)

mv2Hs :: Fill MV2Info
mv2Hs = M.unions <$> sequenceA 
    [ fillH1L mv2c00 "/mv2c00" $ yodaHist 50 (-1) 1 "MV2c00" (dsigdXpbY "MV2" "1")
    , fillH1L mv2c10 "/mv2c10" $ yodaHist 50 (-1) 1 "MV2c10" (dsigdXpbY "MV2" "1")
    , fillH1L mv2c20 "/mv2c20" $ yodaHist 50 (-1) 1 "MV2c20" (dsigdXpbY "MV2" "1")
    , fillH1L mv2c100 "/mv2c100" $ yodaHist 50 (-1) 1 "MV2c100" (dsigdXpbY "MV2" "1")
    , fillH1L mv2cl100 "/mv2cl100" $ yodaHist 50 (-1) 1 "MV2cl100" (dsigdXpbY "MV2" "1")
    ]


readMV2s :: MonadIO m => TR m (ZipList MV2Info)
readMV2s = do
    c00 <- readD "jetsMV2c00"
    c10 <- readD "jetsMV2c10"
    c20 <- readD "jetsMV2c20"
    c100 <- readD "jetsMV2c100"
    cl100 <- readD "jetsMV2cl100"

    return $ MV2Info
        <$> c00
        <*> c10
        <*> c20
        <*> c100
        <*> cl100

    where
        readD n = fmap float2Double <$> readBranch n


-- TODO
-- can't get TH working with external libs (e.g. root)

mv2c00 :: Lens' MV2Info Double
mv2c00 f_ajeI (MV2Info x1_ajeJ x2_ajeK x3_ajeL x4_ajeM x5_ajeN)
  = fmap
      (\ y1_ajeO -> MV2Info y1_ajeO x2_ajeK x3_ajeL x4_ajeM x5_ajeN)
      (f_ajeI x1_ajeJ)
{-# INLINE mv2c00 #-}
mv2c10 :: Lens' MV2Info Double
mv2c10 f_ajeP (MV2Info x1_ajeQ x2_ajeR x3_ajeS x4_ajeT x5_ajeU)
  = fmap
      (\ y1_ajeV -> MV2Info x1_ajeQ y1_ajeV x3_ajeS x4_ajeT x5_ajeU)
      (f_ajeP x2_ajeR)
{-# INLINE mv2c10 #-}
mv2c100 :: Lens' MV2Info Double
mv2c100 f_ajeW (MV2Info x1_ajeX x2_ajeY x3_ajeZ x4_ajf0 x5_ajf1)
  = fmap
      (\ y1_ajf2 -> MV2Info x1_ajeX x2_ajeY x3_ajeZ y1_ajf2 x5_ajf1)
      (f_ajeW x4_ajf0)
{-# INLINE mv2c100 #-}
mv2c20 :: Lens' MV2Info Double
mv2c20 f_ajf3 (MV2Info x1_ajf4 x2_ajf5 x3_ajf6 x4_ajf7 x5_ajf8)
  = fmap
      (\ y1_ajf9 -> MV2Info x1_ajf4 x2_ajf5 y1_ajf9 x4_ajf7 x5_ajf8)
      (f_ajf3 x3_ajf6)
{-# INLINE mv2c20 #-}
mv2cl100 :: Lens' MV2Info Double
mv2cl100 f_ajfa (MV2Info x1_ajfb x2_ajfc x3_ajfd x4_ajfe x5_ajff)
  = fmap
      (\ y1_ajfg -> MV2Info x1_ajfb x2_ajfc x3_ajfd x4_ajfe y1_ajfg)
      (f_ajfa x5_ajff)
{-# INLINE mv2cl100 #-}
