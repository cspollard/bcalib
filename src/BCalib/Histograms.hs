{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module BCalib.Histograms
    ( module X
    , fillH1L, fillP1L
    , Fill, Fills, (<$$=)
    , channel, channels
    , lvHs, nH
    ) where
{-
    , eventHs
    , withWeight
    , lepFlavorChannels
    , lepChargeChannels
    , nJetChannels
    ) where
-}

import Control.Lens
import Control.Applicative as X (ZipList(..), liftA2)

import qualified Control.Foldl as F

import Data.Semigroup as X
import qualified Data.Text as T

import Data.HEP.LorentzVector
import Data.YODA.Obj as X
import Data.TTree as X
import Data.Atlas.Histogramming as X

instance Semigroup (ZipList a) where
    ZipList xs <> ZipList ys = ZipList $ xs <> ys

instance Monoid (ZipList a) where
    mempty = ZipList []
    mappend = (<>)

instance Semigroup b => Semigroup (F.Fold a b) where
    (<>) = liftA2 (<>)


type Fill a = F.Fold (Double, a) YodaObj
type Fills a = F.Fold (Double, a) (ZipList YodaObj)

fill :: Fillable h => (a -> FillVec h) -> (Weight h, a) -> h -> h
fill l (w, x) = filling w (l x)

fillH1L :: Getter a Double -> YodaObj -> Fill a
fillH1L l yh = F.Fold (\yh' xs -> over (noted._H1DD) (fill (view l) xs) yh') yh id

fillP1L :: Getter a (Double, Double) -> YodaObj -> Fill a
fillP1L l yp = F.Fold (\yp' xs -> over (noted._P1DD) (fill (view l) xs) yp') yp id


selector :: (a -> Bool) -> Prism' (Double, a) (Double, a)
selector f = prism' id $ \wx@(_, x) -> if f x then Just wx else Nothing

channel :: T.Text -> (a -> Bool) -> Fills a -> Fills a
channel n f fills = fmap (over path (n <>)) <$> F.handles (selector f) fills


channels :: [(T.Text, a -> Bool)] -> Fills a -> Fills a
channels fns fills = fmap mconcat . sequenceA $ uncurry channel <$> fns <*> pure fills


nH :: Foldable f => Int -> Fill (f a)
nH mx = fillH1L (to $ fromIntegral.length) $
    yodaHist mx 0 (fromIntegral mx) "/n" "$n$" (dsigdXpbY "n" "1")

ptH :: HasLorentzVector a => Fill a
ptH = fillH1L lvPt $
    yodaHist 25 0 250000 "/pt" "$p_{\\mathrm T}$ [MeV]" (dsigdXpbY pt mev)

etaH :: HasLorentzVector a => Fill a
etaH = fillH1L lvEta $
    yodaHist 30 (-3) 3 "/eta" "$\\eta$" (dsigdXpbY "\\eta" "{\\mathrm rad}")

infixl 2 <$$=
(<$$=) :: Fills b -> Getter a b -> Fills a
fs <$$= l = F.premap (fmap (view l)) fs


-- generic histograms for a lorentz vector
lvHs :: HasLorentzVector a => Fills a
lvHs = sequenceA (ZipList [ptH, etaH])

{-
fillH1 :: Getter a Double -> (Double, a) -> YodaObj -> YodaObj
fillH1 l (w, x) = over (noted._H1DD) (fill (view l) (w, x))

fillP1 :: Getter a (Double, Double) -> (Double, a) -> YodaObj -> YodaObj
fillP1 l (w, x) = over (noted._P1DD) (fill (view l) (w, x))



ftagHs :: Fills Jet
ftagHs = sequenceA . ZipList $
    [ ftagH mv2c00 "mv2c00" "MV2c00" (-1) 1
    , ftagH mv2c10 "mv2c10" "MV2c10"  (-1) 1
    , ftagH mv2c20 "mv2c20" "MV2c20"  (-1) 1
    , ftagH mv2c100 "mv2c100" "MV2c100"  (-1) 1
    , ftagH mv2cl100 "mv2cl100" "MV2cl100"  (-1) 1

    , ftagH ip2dNTrk "ip2dntrk" "IP2D track multiplicity" 0 10
    , ftagH ip2dLLR "ip2dllr" "IP2D LLR" (-20) 30
    , ftagH ip2dPu "ip2dpu" "IP2D P(light)" 0 1
    , ftagH ip2dPc "ip2dpc" "IP2D P(charm)" 0 1
    , ftagH ip2dPb "ip2dpb" "IP2D P(bottom)" 0 1

    , ftagH ip3dNTrk "ip3dntrk" "IP3D track multiplicity" 0 10
    , ftagH ip3dLLR "ip3dllr" "IP3D LLR" (-20) 30
    , ftagH ip3dPu "ip3dpu" "IP3D P(light)" 0 1
    , ftagH ip3dPc "ip3dpc" "IP3D P(charm)" 0 1
    , ftagH ip3dPb "ip3dpb" "IP3D P(bottom)" 0 1

    , ftagH jfNVtx "jfnvtx" "JetFitter vertex multiplicity" 0 5
    , ftagH jfMass "jfmass" "JetFitter mass" 0 10000
    , ftagH jfNSingleTrks "jfnsingtrks" "JetFitter number of single tracks" 0 10
    , ftagH jfNTrksAtVtx "jfntrksatvtx" "JetFitter number of tracks at vertex" 0 10
    , ftagH jfEfrac "jfefrac" "JetFitter energy fraction" 0 1
    , ftagH jfN2TPair "jfn2tpair" "JetFitter n2tpair" 0 10
    , ftagH jfLLR "jfllr" "JetFitter LLR" (-20) 30
    , ftagH jfPu "jfpu" "JetFitter P(light)" 0 1
    , ftagH jfPc "jfpc" "JetFitter P(charm)" 0 1
    , ftagH jfPb "jfpb" "JetFitter P(bottom)" 0 1
    ]

    where
        ftagH :: Lens' Jet Double -> T.Text -> Double -> Double -> Fill Jet
        ftagH l n t mn mx =
            let hist = yodaHist 50 mn mx n t (dsigdXpbY n "1")
            in Fold (flip $ fillH1 l) hist id



withWeight :: Event -> (Double, Event)
withWeight = view eventWeight &&& id


leptonFlavors :: (LFlavor, LFlavor) -> Event -> Bool
leptonFlavors flvs e = flvs == (view leptons e & over both (view lepFlavor))

leptonCharges :: (LCharge, LCharge) -> Event -> Bool
leptonCharges chgs e = chgs == (view leptons e & over both (view lepCharge))


lepChargeChannels :: Fills Event -> Fills Event
lepChargeChannels =
    channels
        [ ("/allLepCharge", const True)
        , ("/os", (||) <$> leptonCharges (Plus, Minus) <*> leptonCharges (Minus, Plus))
        , ("/ss", (||) <$> leptonCharges (Plus, Plus) <*> leptonCharges (Minus, Minus))
        ]

lepFlavorChannels :: Fills Event -> Fills Event
lepFlavorChannels =
    channels 
        [ ("/allLepFlav", const True)
        , ("/elmu", (||) <$> leptonFlavors (Electron, Muon) <*> leptonFlavors (Muon, Electron))
        , ("/mumu", leptonFlavors (Muon, Muon))
        , ("/elel", leptonFlavors (Electron, Electron))
        ]

nJetChannels :: Fills Event -> Fills Event
nJetChannels =
    channels 
        [ ("/allNjets", const True)
        , ("/2jet", (== 2) . views jets length)
        , ("/3jet", (== 3) . views jets length)
        , ("/4pjet", (>= 4) . views jets length)
        ]
-}
