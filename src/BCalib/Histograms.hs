{-# LANGUAGE OverloadedStrings #-}

module BCalib.Histograms
    ( module X
    , runFiller
    , bcalibHists
    ) where

import Control.Lens
import Control.Applicative

import Data.Semigroup
import Data.Vector (Vector)

import GHC.Float

import Data.YODA.Obj as X
import Data.TTree as X
import Data.Atlas.Histogramming as X

newtype Filler m = Filler (ZipList (YodaObj -> TR m YodaObj), ZipList YodaObj)


singleton :: (YodaObj -> TR m YodaObj) -> YodaObj -> Filler m
singleton f h = Filler (ZipList [f], ZipList [h])

fillH :: (MonadIO m, Foldable f) => TR m (f Double) -> TR m Double -> YodaObj -> Filler m
fillH obs wgt = singleton f
    where f h = do
            xs <- obs
            w <- wgt
            return $ foldl (\h' x -> over (noted._H1DD) (filling w x) h') h xs

fillP :: (MonadIO m, Foldable f) => TR m (f (Double, Double)) -> TR m Double -> YodaObj -> Filler m
fillP obs wgt = singleton f
    where f h = do
            xs <- obs
            w <- wgt
            return $ foldl (\h' x -> over (noted._P1DD) (filling w x) h') h xs



fillWH :: (MonadIO m, Foldable f) => TR m (f Double) -> YodaObj -> Filler m
fillWH tr = fillH tr weight

fillWP :: (MonadIO m, Foldable f) => TR m (f (Double, Double)) -> YodaObj -> Filler m
fillWP tr = fillP tr weight


instance Semigroup (Filler m) where
    (<>) = addF

instance Monoid (Filler m) where
    mempty = Filler (ZipList [], ZipList [])
    mappend = (<>)


addF :: Filler m -> Filler m -> Filler m
addF (Filler (x, y)) (Filler (x', y')) = Filler (addZL x x', addZL y y')
    where addZL (ZipList xs) (ZipList ys) = ZipList $ xs ++ ys


runFiller :: MonadIO m => Filler m -> TTree -> m (ZipList YodaObj)
runFiller (Filler (fs, hs)) = runTTree (ff fs) hs
    where
        ff fs' hs' = sequence $ fs' <*> hs'


bcalibHists :: MonadIO m => Filler m
bcalibHists =
    mconcat
        [ muH
        , jetPtH
        , jetEtaH
        ]


muH :: MonadIO m => Filler m
muH = fillWH tr h
    where
        tr = Just . float2Double <$> readBranch "mu"
        h = yodaHist 100 0 100 "/mu" "$ <\\mu> $" ""

jetPtH :: MonadIO m => Filler m
jetPtH = fillWH tr h
    where
        tr :: MonadIO m => TR m (Vector Double)
        tr = fmap float2Double <$> readBranch "jetsMomPt"
        h = yodaHist 100 0 100000 "/jetpt" "jet $p_{\\mathrm T}$ [MeV]" ""

jetEtaH :: MonadIO m => Filler m
jetEtaH = fillWH tr h
    where
        tr :: MonadIO m => TR m (Vector Double)
        tr = fmap float2Double <$> readBranch "jetsMomEta"
        h = yodaHist 100 (-3.0) 3.0 "/jeteta" "jet $\\eta$ [MeV]" ""
