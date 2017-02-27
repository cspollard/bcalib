{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module BCalib.IP3D where

import           Control.Applicative (ZipList (..))
import           Data.Map.Strict     as M
import           GHC.Float
import           GHC.Generics        hiding (to)

import           BCalib.Imports


data IP3DInfo =
    IP3DInfo
        { _ip3dNTrk :: Int
        , _ip3dLLR  :: Double
        , _ip3dPu   :: Double
        , _ip3dPc   :: Double
        , _ip3dPb   :: Double
        } deriving (Generic, Show)


ip3dHs :: Fill IP3DInfo
ip3dHs = M.unions <$> sequenceA
  [ hist1DDef (binD 0 20 20) "IP3D track multiplicity" (dsigdXpbY "n" "1") "/ip3dntrk"
    <$$= (ip3dNTrk.integralL)
  , hist1DDef (binD (-20) 50 30) "IP3D LLR" (dsigdXpbY "\\mathrm{LLR}" "1") "/ip3dllr"
    <$$= ip3dLLR
  ]

  where
    integralL :: (Num a, Integral s, Profunctor p, Contravariant f) => Optic' p f s a
    integralL = to fromIntegral


readIP3Ds :: MonadIO m => TR m (ZipList IP3DInfo)
readIP3Ds = do
  ntrk <- readI "jetsIP3D_ntrk"
  llr <- readD "jetsIP3D_loglikelihoodratio"
  pu <- readD "jetsIP3D_pu"
  pc <- readD "jetsIP3D_pc"
  pb <- readD "jetsIP3D_pb"

  return $ IP3DInfo
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

ip3dLLR :: Lens' IP3DInfo Double
ip3dLLR f_ajeI (IP3DInfo x1_ajeJ x2_ajeK x3_ajeL x4_ajeM x5_ajeN)
  = fmap
      (\ y1_ajeO -> IP3DInfo x1_ajeJ y1_ajeO x3_ajeL x4_ajeM x5_ajeN)
      (f_ajeI x2_ajeK)
{-# INLINE ip3dLLR #-}
ip3dNTrk :: Lens' IP3DInfo Int
ip3dNTrk f_ajeP (IP3DInfo x1_ajeQ x2_ajeR x3_ajeS x4_ajeT x5_ajeU)
  = fmap
      (\ y1_ajeV -> IP3DInfo y1_ajeV x2_ajeR x3_ajeS x4_ajeT x5_ajeU)
      (f_ajeP x1_ajeQ)
{-# INLINE ip3dNTrk #-}
ip3dPb :: Lens' IP3DInfo Double
ip3dPb f_ajeW (IP3DInfo x1_ajeX x2_ajeY x3_ajeZ x4_ajf0 x5_ajf1)
  = fmap
      (\ y1_ajf2 -> IP3DInfo x1_ajeX x2_ajeY x3_ajeZ x4_ajf0 y1_ajf2)
      (f_ajeW x5_ajf1)
{-# INLINE ip3dPb #-}
ip3dPc :: Lens' IP3DInfo Double
ip3dPc f_ajf3 (IP3DInfo x1_ajf4 x2_ajf5 x3_ajf6 x4_ajf7 x5_ajf8)
  = fmap
      (\ y1_ajf9 -> IP3DInfo x1_ajf4 x2_ajf5 x3_ajf6 y1_ajf9 x5_ajf8)
      (f_ajf3 x4_ajf7)
{-# INLINE ip3dPc #-}
ip3dPu :: Lens' IP3DInfo Double
ip3dPu f_ajfa (IP3DInfo x1_ajfb x2_ajfc x3_ajfd x4_ajfe x5_ajff)
  = fmap
      (\ y1_ajfg -> IP3DInfo x1_ajfb x2_ajfc y1_ajfg x4_ajfe x5_ajff)
      (f_ajfa x3_ajfd)
{-# INLINE ip3dPu #-}
