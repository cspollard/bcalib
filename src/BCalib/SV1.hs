{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module BCalib.SV1 where

import GHC.Float
import GHC.Generics hiding (to)

import Data.Map.Strict as M

import BCalib.Histograms


data SV1Info =
    SV1Info
        { _sv1MSV :: Double
        , _sv1NGTJet :: Int
        , _sv1NGTSV :: Int
        , _sv1Efrac :: Double
        , _sv1LLR :: Double
        , _sv1Pu :: Double
        , _sv1Pc :: Double
        , _sv1Pb :: Double
        } deriving (Generic, Show)


-- TODO
-- profiles

sv1Hs :: Fill SV1Info
sv1Hs = M.unions <$> sequenceA
    [ fillH1L sv1MSV "/sv1msv" $ yodaHist 50 0 10000 "SV1 SV mass [MeV]" (dsigdXpbY "m" "MeV")
    , fillH1L (sv1NGTJet.integralL) "/sv1ngtj" $ yodaHist 10 0 10 "SV1 Jet NGT" (dsigdXpbY "n" "1")
    , fillH1L (sv1NGTSV.integralL) "/sv1ngtsv" $ yodaHist 10 0 10 "SV1 SV NGT" (dsigdXpbY "n" "1")
    , fillH1L sv1Efrac "/sv1efrac" $ yodaHist 50 0 1 "SV1 energy fraction" (dsigdXpbY "fraction" "1")
    , fillH1L sv1LLR "/sv1llr" $ yodaHist 50 (-20) 30 "SV1 LLR" (dsigdXpbY "LLR" "1")
    -- , fillH1L sv1Pu "/sv1pu" $ yodaHist 50 0 1 "SV1 P(light)" (dsigdXpbY "P" "1")
    -- , fillH1L sv1Pc "/sv1pc" $ yodaHist 50 0 1 "SV1 P(charm)" (dsigdXpbY "P" "1")
    -- , fillH1L sv1Pb "/sv1pb" $ yodaHist 50 0 1 "SV1 P(bottom)" (dsigdXpbY "P" "1")
    ]

    where
        integralL :: Num a => Getter Int a
        integralL = to fromIntegral


readSV1s :: MonadIO m => TR m (ZipList SV1Info)
readSV1s = do
    msv <- readD "jetsSV1_masssvx"
    ngtjet <- readI "jetsSV1_NGTinJet"
    ngtsv <- readI "jetsSV1_NGTinSvx"
    efrac <- readD "jetsSV1_efracsvx"
    llr <- readD "jetsSV1_loglikelihoodratio"
    pu <- readD "jetsSV1_pu"
    pc <- readD "jetsSV1_pc"
    pb <- readD "jetsSV1_pb"

    return $ SV1Info
        <$> msv
        <*> ngtjet
        <*> ngtsv
        <*> efrac
        <*> llr
        <*> pu
        <*> pc
        <*> pb

    where
        readD n = fmap float2Double <$> readBranch n
        readI n = fmap (fromEnum :: CInt -> Int) <$> readBranch n


-- TODO
-- can't get TH working with external libs (e.g. root)

sv1Efrac :: Lens' SV1Info Double
sv1Efrac
  f_ajgB
  (SV1Info x1_ajgC
           x2_ajgD
           x3_ajgE
           x4_ajgF
           x5_ajgG
           x6_ajgH
           x7_ajgI
           x8_ajgJ)
  = fmap
      (\ y1_ajgK
         -> SV1Info
              x1_ajgC x2_ajgD x3_ajgE y1_ajgK x5_ajgG x6_ajgH x7_ajgI x8_ajgJ)
      (f_ajgB x4_ajgF)
{-# INLINE sv1Efrac #-}
sv1LLR :: Lens' SV1Info Double
sv1LLR
  f_ajgL
  (SV1Info x1_ajgM
           x2_ajgN
           x3_ajgO
           x4_ajgP
           x5_ajgQ
           x6_ajgR
           x7_ajgS
           x8_ajgT)
  = fmap
      (\ y1_ajgU
         -> SV1Info
              x1_ajgM x2_ajgN x3_ajgO x4_ajgP y1_ajgU x6_ajgR x7_ajgS x8_ajgT)
      (f_ajgL x5_ajgQ)
{-# INLINE sv1LLR #-}
sv1MSV :: Lens' SV1Info Double
sv1MSV
  f_ajgV
  (SV1Info x1_ajgW
           x2_ajgX
           x3_ajgY
           x4_ajgZ
           x5_ajh0
           x6_ajh1
           x7_ajh2
           x8_ajh3)
  = fmap
      (\ y1_ajh4
         -> SV1Info
              y1_ajh4 x2_ajgX x3_ajgY x4_ajgZ x5_ajh0 x6_ajh1 x7_ajh2 x8_ajh3)
      (f_ajgV x1_ajgW)
{-# INLINE sv1MSV #-}
sv1NGTJet :: Lens' SV1Info Int
sv1NGTJet
  f_ajh5
  (SV1Info x1_ajh6
           x2_ajh7
           x3_ajh8
           x4_ajh9
           x5_ajha
           x6_ajhb
           x7_ajhc
           x8_ajhd)
  = fmap
      (\ y1_ajhe
         -> SV1Info
              x1_ajh6 y1_ajhe x3_ajh8 x4_ajh9 x5_ajha x6_ajhb x7_ajhc x8_ajhd)
      (f_ajh5 x2_ajh7)
{-# INLINE sv1NGTJet #-}
sv1NGTSV :: Lens' SV1Info Int
sv1NGTSV
  f_ajhf
  (SV1Info x1_ajhg
           x2_ajhh
           x3_ajhi
           x4_ajhj
           x5_ajhk
           x6_ajhl
           x7_ajhm
           x8_ajhn)
  = fmap
      (\ y1_ajho
         -> SV1Info
              x1_ajhg x2_ajhh y1_ajho x4_ajhj x5_ajhk x6_ajhl x7_ajhm x8_ajhn)
      (f_ajhf x3_ajhi)
{-# INLINE sv1NGTSV #-}
sv1Pb :: Lens' SV1Info Double
sv1Pb
  f_ajhp
  (SV1Info x1_ajhq
           x2_ajhr
           x3_ajhs
           x4_ajht
           x5_ajhu
           x6_ajhv
           x7_ajhw
           x8_ajhx)
  = fmap
      (\ y1_ajhy
         -> SV1Info
              x1_ajhq x2_ajhr x3_ajhs x4_ajht x5_ajhu x6_ajhv x7_ajhw y1_ajhy)
      (f_ajhp x8_ajhx)
{-# INLINE sv1Pb #-}
sv1Pc :: Lens' SV1Info Double
sv1Pc
  f_ajhz
  (SV1Info x1_ajhA
           x2_ajhB
           x3_ajhC
           x4_ajhD
           x5_ajhE
           x6_ajhF
           x7_ajhG
           x8_ajhH)
  = fmap
      (\ y1_ajhI
         -> SV1Info
              x1_ajhA x2_ajhB x3_ajhC x4_ajhD x5_ajhE x6_ajhF y1_ajhI x8_ajhH)
      (f_ajhz x7_ajhG)
{-# INLINE sv1Pc #-}
sv1Pu :: Lens' SV1Info Double
sv1Pu
  f_ajhJ
  (SV1Info x1_ajhK
           x2_ajhL
           x3_ajhM
           x4_ajhN
           x5_ajhO
           x6_ajhP
           x7_ajhQ
           x8_ajhR)
  = fmap
      (\ y1_ajhS
         -> SV1Info
              x1_ajhK x2_ajhL x3_ajhM x4_ajhN x5_ajhO y1_ajhS x7_ajhQ x8_ajhR)
      (f_ajhJ x6_ajhP)
{-# INLINE sv1Pu #-}
