{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module BCalib.Histograms
    ( module X
    , eventHs
    , withWeight
    , lepFlavorChannels
    , lepChargeChannels
    , zlseq
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

zlseq :: ZipList a -> ZipList a
zlseq zl = case zl of
            ZipList xs -> ZipList $! lseq xs

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

jetsHs :: Fills Event
jetsHs =
    (F.handles traverse lvHs <$= sequenceA)
        <> sequenceA (ZipList [nH 10])
    <$= fmap (view jets)
    <&> fmap (over path ("/jets" <>) . over xlabel ("jet " <>))

lepsHs :: Fills Event
lepsHs =
    -- TODO
    -- must be better way to write this...
    F.handles traverse lvHs <$= (\(w, e) -> let (l1, l2) = view leptons e in [(w, l1), (w, l2)])
    <&> fmap (over path ("/leps" <>) . over xlabel ("lep " <>))


lepChargeChannels :: Fills Event -> Fills Event
lepChargeChannels fills =
    mappend
        <$> channel "/allLepCharge" fills
        <*> exclChannels
            [ ("/os", (||) <$> leptonCharges (Plus, Minus) <*> leptonCharges (Minus, Plus))
            , ("/ss", (||) <$> leptonCharges (Plus, Plus) <*> leptonCharges (Minus, Minus))
            ] fills

lepFlavorChannels :: Fills Event -> Fills Event
lepFlavorChannels fills =
    mappend
        <$> channel "/allLepFlav" fills
        <*> exclChannels
            [ ("/elmu", (||) <$> leptonFlavors (Electron, Muon) <*> leptonFlavors (Muon, Electron))
            , ("/mumu", leptonFlavors (Muon, Muon))
            , ("/elel", leptonFlavors (Electron, Electron))
            ] fills

withWeight :: Event -> (Double, Event)
withWeight = view eventWeight &&& id

eventHs :: Fills Event
eventHs = lepsHs <> jetsHs <> sequenceA (ZipList [muH])

channel :: T.Text -> Fills a -> Fills a
channel n fs = fmap (over path (n <>)) <$> fs

-- TODO
-- SO MUCH DUPLICATION

inclChannels :: [(T.Text, a -> Bool)] -> Fills a -> Fills a
inclChannels cuts fills = mconcat <$> Fold f fills' g
    where
        fills' = (\(n, isGood) fs -> (isGood, channel n fs)) <$> cuts <*> pure fills

        f o x = over (traverse . h (snd x) . _2) (`feed` x) o

        h x = prism' id $ \(c, fs) -> if c x then Just (c, fs) else Nothing

        g = lseq . fmap ((\(Fold _ x w) -> w x) . snd)

exclChannels :: [(T.Text, a -> Bool)] -> Fills a -> Fills a
exclChannels cuts fills = mconcat <$> Fold f fills' g
    where
        fills' = (\(n, isGood) fs -> (isGood, channel n fs)) <$> cuts <*> pure fills

        f o x = over (taking 1 $ traverse . h (snd x) . _2) (`feed` x) o

        h x = prism' id $ \(c, fs) -> if c x then Just (c, fs) else Nothing

        g = lseq . fmap ((\(Fold _ x w) -> w x) . snd)


leptonFlavors :: (LFlavor, LFlavor) -> Event -> Bool
leptonFlavors flvs e = flvs == (view leptons e & over both (view lepFlavor))

leptonCharges :: (LCharge, LCharge) -> Event -> Bool
leptonCharges chgs e = chgs == (view leptons e & over both (view lepCharge))
