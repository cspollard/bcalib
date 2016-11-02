{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module BCalib.Histograms
    ( module X
    , eventHs
    ) where

import Control.Lens hiding (Fold)
import Control.Arrow ((&&&))
import Control.Applicative

import Control.Foldl

import Data.Semigroup

import Data.YODA.Obj as X
import Data.TTree as X
import Data.Atlas.Histogramming as X
import BCalib.Event

instance Semigroup (ZipList a) where
    ZipList xs <> ZipList ys = ZipList $ xs <> ys

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


muH :: Fill Event
muH = Fold (flip $ fillH1 mu) hist id
    where
        hist = yodaHist 50 0 50 "/mu" "$ <\\mu> $" ""

ptH :: HasLorentzVector a => Fill a
ptH = Fold (flip $ fillH1 lvPt) hist id
    where
        hist = yodaHist 25 0 100000 "/pt" "$p_{\\mathrm T}$ [MeV]" ""

etaH :: HasLorentzVector a => Fill a
etaH = Fold (flip $ fillH1 lvEta) hist id
    where
        hist = yodaHist 30 (-3) 3 "/eta" "$\\eta$" ""

jetHs :: Fills Jet
jetHs = sequenceA (ZipList [ptH, etaH])
    <&> fmap (over path ("/jet" <>) . over xlabel ("jet " <>))

eventHs :: Fold Event (ZipList YodaObj)
eventHs = premap (view eventWeight &&& id) $
    sequenceA (ZipList [muH])
    <> (foldAll jetHs <$= traverse (view jets))
