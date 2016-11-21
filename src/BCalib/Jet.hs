{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module BCalib.Jet
    ( module X
    , Jet(Jet)
    , JetFlavor(..)
    , mv2info, ip2dinfo, ip3dinfo, sv1info, jfinfo, truthFlavor
    , jetHs
    , readJets
    ) where

import Control.Lens

import Foreign.C.Types (CInt)
import GHC.Generics hiding (to)
import GHC.Float (float2Double)

import Data.Text as T

import BCalib.Histograms

import Data.HEP.LorentzVector as X
import BCalib.IP2D as X
import BCalib.IP3D as X
import BCalib.JF as X
import BCalib.MV2 as X
import BCalib.SV1 as X

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
        , _mv2info :: MV2Info
        , _ip2dinfo :: IP2DInfo
        , _ip3dinfo :: IP3DInfo
        , _sv1info :: SV1Info
        , _jfinfo :: JFInfo
        , _truthFlavor :: Maybe JetFlavor
        } deriving (Generic, Show)

instance HasLorentzVector Jet where
    toPtEtaPhiE = jfourmom


jetHs :: Fill Jet
jetHs =
    channels
        [ ("/allJetFlavs", const True)
        , ("/light", views truthFlavor (== Just L))
        , ("/charm", views truthFlavor (== Just C))
        , ("/bottom", views truthFlavor (== Just B))
        ] $
    channels
        ( ("/inclusive", const True)
        : ("/pt_gt500", (> 500000) . view lvPt)
        : bins "/pt" (view lvPt) [20, 30, 50, 75, 100, 150, 250, 500]
        ++ bins "/eta" (view lvEta) [-2.5, -1.5, -0.5, 0.5, 1.5, 2.5]
        )
        $
    mconcat
        [ lvHs
        , sv1Hs <$$= sv1info
        , jfHs <$$= jfinfo
        , ip2dHs <$$= ip2dinfo
        , ip3dHs <$$= ip3dinfo
        , mv2Hs <$$= mv2info
        ]

    where
        bins :: T.Text -> (Jet -> Double) -> [Double] -> [(T.Text, Jet -> Bool)]

        bins lab f (b0:b1:bs) =
            ( fixT $ lab <> "_" <> T.pack (show b0) <> "_" <> T.pack (show b1)
            , \j -> let x = f j in b0 < x && x < b1
            ) : bins lab f (b1:bs)

        bins _ _ _ = []

        fixT = T.replace "-" "m"


lvsFromTTree :: MonadIO m => String -> String -> String -> TR m (ZipList PtEtaPhiE)
lvsFromTTree ptn etan phin = do
    pts <- fmap (float2Double . (/ 1e3)) <$> readBranch ptn
    etas <- fmap float2Double <$> readBranch etan
    phis <- fmap float2Double <$> readBranch phin

    let es = (\pt eta -> pt * cosh eta) <$> pts <*> etas

    return $ PtEtaPhiE <$> pts <*> etas <*> phis <*> es


readJets :: MonadIO m => Bool -> TR m (ZipList Jet)
readJets isData = do
    fourmoms <- lvsFromTTree "jetsMomPt" "jetsMomEta" "jetsMomPhi"

    mv2s <- readMV2s
    ip2ds <- readIP2Ds
    ip3ds <- readIP3Ds
    sv1s <- readSV1s
    jfs <- readJFs

    flvs <- if isData
                then return $ ZipList (repeat Nothing)
                else fmap (Just . flavFromCInt) <$> readBranch "jetsTrueFlavor"

    return $ Jet
            <$> fourmoms
            <*> mv2s
            <*> ip2ds
            <*> ip3ds
            <*> sv1s
            <*> jfs
            <*> flvs



-- TODO
-- can't get TH working with external libs (e.g. root)


ip2dinfo :: Lens' Jet IP2DInfo
ip2dinfo
  f_ai3K
  (Jet x1_ai3L x2_ai3M x3_ai3N x4_ai3O x5_ai3P x6_ai3Q x7_ai3R)
  = fmap
      (\ y1_ai3S
         -> Jet x1_ai3L x2_ai3M y1_ai3S x4_ai3O x5_ai3P x6_ai3Q x7_ai3R)
      (f_ai3K x3_ai3N)
{-# INLINE ip2dinfo #-}
ip3dinfo :: Lens' Jet IP3DInfo
ip3dinfo
  f_ai3T
  (Jet x1_ai3U x2_ai3V x3_ai3W x4_ai3X x5_ai3Y x6_ai3Z x7_ai40)
  = fmap
      (\ y1_ai41
         -> Jet x1_ai3U x2_ai3V x3_ai3W y1_ai41 x5_ai3Y x6_ai3Z x7_ai40)
      (f_ai3T x4_ai3X)
{-# INLINE ip3dinfo #-}
truthFlavor :: Lens' Jet (Maybe JetFlavor)
truthFlavor
  f_ai42
  (Jet x1_ai43 x2_ai44 x3_ai45 x4_ai46 x5_ai47 x6_ai48 x7_ai49)
  = fmap
      (\ y1_ai4a
         -> Jet x1_ai43 x2_ai44 x3_ai45 x4_ai46 x5_ai47 x6_ai48 y1_ai4a)
      (f_ai42 x7_ai49)
{-# INLINE truthFlavor #-}
jfinfo :: Lens' Jet JFInfo
jfinfo
  f_ai4b
  (Jet x1_ai4c x2_ai4d x3_ai4e x4_ai4f x5_ai4g x6_ai4h x7_ai4i)
  = fmap
      (\ y1_ai4j
         -> Jet x1_ai4c x2_ai4d x3_ai4e x4_ai4f x5_ai4g y1_ai4j x7_ai4i)
      (f_ai4b x6_ai4h)
{-# INLINE jfinfo #-}
jfourmom :: Lens' Jet PtEtaPhiE
jfourmom
  f_ai4k
  (Jet x1_ai4l x2_ai4m x3_ai4n x4_ai4o x5_ai4p x6_ai4q x7_ai4r)
  = fmap
      (\ y1_ai4s
         -> Jet y1_ai4s x2_ai4m x3_ai4n x4_ai4o x5_ai4p x6_ai4q x7_ai4r)
      (f_ai4k x1_ai4l)
{-# INLINE jfourmom #-}
mv2info :: Lens' Jet MV2Info
mv2info
  f_ai4t
  (Jet x1_ai4u x2_ai4v x3_ai4w x4_ai4x x5_ai4y x6_ai4z x7_ai4A)
  = fmap
      (\ y1_ai4B
         -> Jet x1_ai4u y1_ai4B x3_ai4w x4_ai4x x5_ai4y x6_ai4z x7_ai4A)
      (f_ai4t x2_ai4v)
{-# INLINE mv2info #-}
sv1info :: Lens' Jet SV1Info
sv1info
  f_ai4C
  (Jet x1_ai4D x2_ai4E x3_ai4F x4_ai4G x5_ai4H x6_ai4I x7_ai4J)
  = fmap
      (\ y1_ai4K
         -> Jet x1_ai4D x2_ai4E x3_ai4F x4_ai4G y1_ai4K x6_ai4I x7_ai4J)
      (f_ai4C x5_ai4H)
{-# INLINE sv1info #-}
