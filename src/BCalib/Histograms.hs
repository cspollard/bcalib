{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module BCalib.Histograms
    ( module X
    , eventHs
    , withWeight
    , lepChannels
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

jetsHs :: Fills Event
jetsHs =
    (foldAll (sequenceA (ZipList [ptH, etaH])) <$= sequenceA)
    <> sequenceA (ZipList [nH 10])
    <$= fmap (view jets)
    <&> fmap (over path ("/jet" <>) . over xlabel ("jet " <>))


lepChannels :: Fills Event -> Fills Event
lepChannels =
    fillFirst
        [ ("elmu", oppLepCharge)
        ]

withWeight :: Event -> (Double, Event)
withWeight = view eventWeight &&& id

eventHs :: Fills Event
eventHs = jetsHs <> sequenceA (ZipList [muH])

channel :: T.Text -> Fills a -> Fills a
channel n fs = fmap (over path (n <>)) <$> fs

-- TODO
-- SO MUCH DUPLICATION

fillAll :: [(T.Text, a -> Bool)] -> Fills a -> Fills a
fillAll cuts fills = mconcat <$> Fold f fills' g
    where
        fills' = (\(n, isGood) fs -> (isGood, channel n fs)) <$> cuts <*> pure fills


        f o x = over (traverse . h (snd x) . _2) (`feed` x) o

        h x = prism' id $ \(c, fs) -> if c x then Just (c, fs) else Nothing

        g = fmap ((\(Fold _ x w) -> w x) . snd)

fillFirst :: [(T.Text, a -> Bool)] -> Fills a -> Fills a
fillFirst cuts fills = mconcat <$> Fold f fills' g
    where
        fills' = (\(n, isGood) fs -> (isGood, channel n fs)) <$> cuts <*> pure fills

        f o x = over (taking 1 $ traverse . h (snd x) . _2) (`feed` x) o

        h x = prism' id $ \(c, fs) -> if c x then Just (c, fs) else Nothing

        g = fmap ((\(Fold _ x w) -> w x) . snd)

oppLepFlav :: Event -> Bool
-- TODO
-- why does view (leptons.both.lepFlavor) e not work?
oppLepFlav e =
    case view leptons e & over both (view lepFlavor) of
        (Electron, Muon) -> True
        (Muon, Electron) -> True
        _                -> False

-- TODO
-- why does view (leptons.both.lepCharge) e not work?
oppLepCharge :: Event -> Bool
oppLepCharge e =
    case view leptons e & over both (view lepCharge) of
        (Plus, Minus) -> True
        (Minus, Plus) -> True
        _             -> False
