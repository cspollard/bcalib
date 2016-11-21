{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module BCalib.JF where

import GHC.Float
import GHC.Generics hiding (to)

import Data.Map.Strict as M

import BCalib.Histograms


data JFInfo =
    JFInfo
        { _jfNVtx :: Int
        , _jfMass :: Double
        , _jfNSingleTrks :: Int
        , _jfNTrksAtVtx :: Int
        , _jfEfrac :: Double
        , _jfN2TPair :: Int
        , _jfLLR :: Double
        , _jfPu :: Double
        , _jfPc :: Double
        , _jfPb :: Double
        } deriving (Generic, Show)

jfHs :: Fill JFInfo
jfHs = M.unions <$> sequenceA
    [ fillH1L (jfNVtx.integralL) "/jfnvtx" $ yodaHist 5 0 5 "JF vertex multiplicity" (dsigdXpbY "n" "1")
    , fillH1L jfMass "/jfmass" $ yodaHist 50 0 10000 "JF mass [GeV]" (dsigdXpbY "m" "GeV")
    , fillH1L (jfNSingleTrks.integralL) "/jfnsingtrks" $ yodaHist 10 0 10 "JF single track multiplicity" (dsigdXpbY "n" "1")
    , fillH1L (jfNTrksAtVtx.integralL) "/jfntrksatvtx" $ yodaHist 10 0 10 "JF vertex track multiplicity" (dsigdXpbY "n" "1")
    , fillH1L jfEfrac "/jfefrac" $ yodaHist 50 0 1 "JF energy fraction" (dsigdXpbY "fraction" "1")
    , fillH1L (jfN2TPair.integralL) "/jfn2tpair" $ yodaHist 20 0 20 "JF n2tpair" (dsigdXpbY "n" "1")
    , fillH1L jfLLR "/jfllr" $ yodaHist 50 (-20) 30 "JF LLR" (dsigdXpbY "LLR" "1")
    -- , fillH1L jfPu "/jfpu" $ yodaHist 50 0 1 "JF P(light)" (dsigdXpbY "P" "1")
    -- , fillH1L jfPc "/jfpc" $ yodaHist 50 0 1 "JF P(charm)" (dsigdXpbY "P" "1")
    -- , fillH1L jfPb "/jfpb" $ yodaHist 50 0 1 "JF P(bottom)" (dsigdXpbY "P" "1")
    ]

    where
        integralL :: Num a => Getter Int a
        integralL = to fromIntegral


readJFs :: MonadIO m => TR m (ZipList JFInfo)
readJFs = do
    nvtx <- readI "jetsJetFitter_nVTX"
    mass <- fmap (/ 1e3) <$> readD "jetsJetFitter_mass"
    nsts <- readI "jetsJetFitter_nSingleTracks"
    ntsav <- readI "jetsJetFitter_nTracksAtVtx"
    efrac <- readD "jetsJetFitter_energyFraction"
    n2tpair <- readI "jetsJetFitter_N2Tpair"
    llr <- readD "jetsJetFitter_loglikelihoodratio"
    pu <- readD "jetsJetFitter_pu"
    pc <- readD "jetsJetFitter_pc"
    pb <- readD "jetsJetFitter_pb"

    return $ JFInfo
        <$> nvtx
        <*> mass
        <*> nsts
        <*> ntsav
        <*> efrac
        <*> n2tpair
        <*> llr
        <*> pu
        <*> pc
        <*> pb

    where
        readD n = fmap float2Double <$> readBranch n
        readI n = fmap (fromEnum :: CInt -> Int) <$> readBranch n


-- TODO
-- can't get TH working with external libs (e.g. root)


jfEfrac :: Lens' JFInfo Double
jfEfrac
  f_ajhR
  (JFInfo x1_ajhS
          x2_ajhT
          x3_ajhU
          x4_ajhV
          x5_ajhW
          x6_ajhX
          x7_ajhY
          x8_ajhZ
          x9_aji0
          x10_aji1)
  = fmap
      (\ y1_aji2
         -> JFInfo
              x1_ajhS
              x2_ajhT
              x3_ajhU
              x4_ajhV
              y1_aji2
              x6_ajhX
              x7_ajhY
              x8_ajhZ
              x9_aji0
              x10_aji1)
      (f_ajhR x5_ajhW)
{-# INLINE jfEfrac #-}
jfLLR :: Lens' JFInfo Double
jfLLR
  f_aji3
  (JFInfo x1_aji4
          x2_aji5
          x3_aji6
          x4_aji7
          x5_aji8
          x6_aji9
          x7_ajia
          x8_ajib
          x9_ajic
          x10_ajid)
  = fmap
      (\ y1_ajie
         -> JFInfo
              x1_aji4
              x2_aji5
              x3_aji6
              x4_aji7
              x5_aji8
              x6_aji9
              y1_ajie
              x8_ajib
              x9_ajic
              x10_ajid)
      (f_aji3 x7_ajia)
{-# INLINE jfLLR #-}
jfMass :: Lens' JFInfo Double
jfMass
  f_ajif
  (JFInfo x1_ajig
          x2_ajih
          x3_ajii
          x4_ajij
          x5_ajik
          x6_ajil
          x7_ajim
          x8_ajin
          x9_ajio
          x10_ajip)
  = fmap
      (\ y1_ajiq
         -> JFInfo
              x1_ajig
              y1_ajiq
              x3_ajii
              x4_ajij
              x5_ajik
              x6_ajil
              x7_ajim
              x8_ajin
              x9_ajio
              x10_ajip)
      (f_ajif x2_ajih)
{-# INLINE jfMass #-}
jfN2TPair :: Lens' JFInfo Int
jfN2TPair
  f_ajir
  (JFInfo x1_ajis
          x2_ajit
          x3_ajiu
          x4_ajiv
          x5_ajiw
          x6_ajix
          x7_ajiy
          x8_ajiz
          x9_ajiA
          x10_ajiB)
  = fmap
      (\ y1_ajiC
         -> JFInfo
              x1_ajis
              x2_ajit
              x3_ajiu
              x4_ajiv
              x5_ajiw
              y1_ajiC
              x7_ajiy
              x8_ajiz
              x9_ajiA
              x10_ajiB)
      (f_ajir x6_ajix)
{-# INLINE jfN2TPair #-}
jfNTrksAtVtx :: Lens' JFInfo Int
jfNTrksAtVtx
  f_ajiD
  (JFInfo x1_ajiE
          x2_ajiF
          x3_ajiG
          x4_ajiH
          x5_ajiI
          x6_ajiJ
          x7_ajiK
          x8_ajiL
          x9_ajiM
          x10_ajiN)
  = fmap
      (\ y1_ajiO
         -> JFInfo
              x1_ajiE
              x2_ajiF
              x3_ajiG
              y1_ajiO
              x5_ajiI
              x6_ajiJ
              x7_ajiK
              x8_ajiL
              x9_ajiM
              x10_ajiN)
      (f_ajiD x4_ajiH)
{-# INLINE jfNTrksAtVtx #-}
jfNSingleTrks :: Lens' JFInfo Int
jfNSingleTrks
  f_ajiP
  (JFInfo x1_ajiQ
          x2_ajiR
          x3_ajiS
          x4_ajiT
          x5_ajiU
          x6_ajiV
          x7_ajiW
          x8_ajiX
          x9_ajiY
          x10_ajiZ)
  = fmap
      (\ y1_ajj0
         -> JFInfo
              x1_ajiQ
              x2_ajiR
              y1_ajj0
              x4_ajiT
              x5_ajiU
              x6_ajiV
              x7_ajiW
              x8_ajiX
              x9_ajiY
              x10_ajiZ)
      (f_ajiP x3_ajiS)
{-# INLINE jfNSingleTrks #-}
jfNVtx :: Lens' JFInfo Int
jfNVtx
  f_ajj1
  (JFInfo x1_ajj2
          x2_ajj3
          x3_ajj4
          x4_ajj5
          x5_ajj6
          x6_ajj7
          x7_ajj8
          x8_ajj9
          x9_ajja
          x10_ajjb)
  = fmap
      (\ y1_ajjc
         -> JFInfo
              y1_ajjc
              x2_ajj3
              x3_ajj4
              x4_ajj5
              x5_ajj6
              x6_ajj7
              x7_ajj8
              x8_ajj9
              x9_ajja
              x10_ajjb)
      (f_ajj1 x1_ajj2)
{-# INLINE jfNVtx #-}
jfPb :: Lens' JFInfo Double
jfPb
  f_ajjd
  (JFInfo x1_ajje
          x2_ajjf
          x3_ajjg
          x4_ajjh
          x5_ajji
          x6_ajjj
          x7_ajjk
          x8_ajjl
          x9_ajjm
          x10_ajjn)
  = fmap
      (\ y1_ajjo
         -> JFInfo
              x1_ajje
              x2_ajjf
              x3_ajjg
              x4_ajjh
              x5_ajji
              x6_ajjj
              x7_ajjk
              x8_ajjl
              x9_ajjm
              y1_ajjo)
      (f_ajjd x10_ajjn)
{-# INLINE jfPb #-}
jfPc :: Lens' JFInfo Double
jfPc
  f_ajjp
  (JFInfo x1_ajjq
          x2_ajjr
          x3_ajjs
          x4_ajjt
          x5_ajju
          x6_ajjv
          x7_ajjw
          x8_ajjx
          x9_ajjy
          x10_ajjz)
  = fmap
      (\ y1_ajjA
         -> JFInfo
              x1_ajjq
              x2_ajjr
              x3_ajjs
              x4_ajjt
              x5_ajju
              x6_ajjv
              x7_ajjw
              x8_ajjx
              y1_ajjA
              x10_ajjz)
      (f_ajjp x9_ajjy)
{-# INLINE jfPc #-}
jfPu :: Lens' JFInfo Double
jfPu
  f_ajjB
  (JFInfo x1_ajjC
          x2_ajjD
          x3_ajjE
          x4_ajjF
          x5_ajjG
          x6_ajjH
          x7_ajjI
          x8_ajjJ
          x9_ajjK
          x10_ajjL)
  = fmap
      (\ y1_ajjM
         -> JFInfo
              x1_ajjC
              x2_ajjD
              x3_ajjE
              x4_ajjF
              x5_ajjG
              x6_ajjH
              x7_ajjI
              y1_ajjM
              x9_ajjK
              x10_ajjL)
      (f_ajjB x8_ajjJ)
{-# INLINE jfPu #-}
