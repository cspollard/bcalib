{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module BCalib.Histograms
    ( module X
    , eventHs
    , withWeight
    , lepFlavorChannels
    , lepChargeChannels
    , nJetChannels
    ) where

import Control.Lens hiding (Fold)
import Control.Arrow ((&&&))
import Control.Applicative

import Control.Foldl (Fold(..))
import qualified Control.Foldl as F

import Data.Semigroup
import qualified Data.Text as T

import Data.YODA.Obj as X
import Data.TTree as X
import Data.Atlas.Histogramming as X
import BCalib.Event

instance Semigroup (ZipList a) where
    ZipList xs <> ZipList ys = ZipList $ xs <> ys

instance Monoid (ZipList a) where
    mempty = ZipList []
    mappend = (<>)

instance Semigroup b => Semigroup (Fold a b) where
    (<>) = liftA2 (<>)


type Fill a = Fold (Double, a) YodaObj
type Fills a = Fold (Double, a) (ZipList YodaObj)

fill :: Fillable h => (a -> FillVec h) -> (Weight h, a) -> h -> h
fill l (w, x) = filling w (l x)

fillH1 :: Getter a Double -> (Double, a) -> YodaObj -> YodaObj
fillH1 l (w, x) = over (noted._H1DD) (fill (view l) (w, x))

fillP1 :: Getter a (Double, Double) -> (Double, a) -> YodaObj -> YodaObj
fillP1 l (w, x) = over (noted._P1DD) (fill (view l) (w, x))


nH :: Foldable f => Int -> Fill (f a)
nH mx = Fold (flip $ fillH1 (to $ fromIntegral . length)) hist id
    where
        hist = yodaHist mx 0 (fromIntegral mx) "/n" "$n$" ""

muH :: Fill Event
muH = Fold (flip $ fillH1 mu) hist id
    where
        hist = yodaHist 50 0 50 "/mu" "$ <\\mu> $" ""

ptH :: HasLorentzVector a => Fill a
ptH = Fold (flip $ fillH1 lvPt) hist id
    where
        hist = yodaHist 25 0 250000 "/pt" "$p_{\\mathrm T}$ [MeV]" ""

etaH :: HasLorentzVector a => Fill a
etaH = Fold (flip $ fillH1 lvEta) hist id
    where
        hist = yodaHist 30 (-3) 3 "/eta" "$\\eta$" ""


-- generic histograms for a lorentz vector
lvHs :: HasLorentzVector a => Fills a
lvHs = sequenceA (ZipList [ptH, etaH])

ftagHs :: Fills Jet
ftagHs = sequenceA . ZipList $
    [ ftagH mv2c00 "mv2c00"
    , ftagH mv2c10 "mv2c10"
    , ftagH mv2c20 "mv2c20"
    , ftagH mv2c100 "mv2c100"
    , ftagH mv2cl100 "mv2cl100"
    , ftagH ip2dLLR "ip2dLLR"
    , ftagH ip3dLLR "ip3dLLR"
    , ftagH sv1LLR "sv1LLR"
    , ftagH jfLLR "jfLLR"
    ]
    where
        ftagH :: Lens' Jet Double -> T.Text -> Fill Jet
        ftagH l n =
            let hist = yodaHist 50 (-1) 1 ("/" <> n) n ""
            in Fold (flip $ fillH1 l) hist id


jetsHs :: Fills Event
jetsHs =
    (F.handles traverse (lvHs <> ftagHs) <$= sequenceA)
        <> sequenceA (ZipList [nH 10])
    <$= fmap (view jets)
    <&> fmap (over path ("/jets" <>) . over xlabel ("jet " <>))

lepsHs :: Fills Event
lepsHs =
    -- TODO
    -- must be better way to write this...
    F.handles traverse lvHs <$= (\(w, e) -> let (l1, l2) = view leptons e in [(w, l1), (w, l2)])
    <&> fmap (over path ("/leps" <>) . over xlabel ("lep " <>))


withWeight :: Event -> (Double, Event)
withWeight = view eventWeight &&& id

eventHs :: Fills Event
eventHs = lepsHs <> jetsHs <> sequenceA (ZipList [muH])

selector :: (a -> Bool) -> Prism' (Double, a) (Double, a)
selector f = prism' id $ \wx@(_, x) -> if f x then Just wx else Nothing

channel :: T.Text -> (a -> Bool) -> Fills a -> Fills a
channel n f fills = fmap (over path (n <>)) <$> F.handles (selector f) fills


channels :: [(T.Text, a -> Bool)] -> Fills a -> Fills a
channels fns fills = fmap mconcat . sequenceA $ uncurry channel <$> fns <*> pure fills


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
