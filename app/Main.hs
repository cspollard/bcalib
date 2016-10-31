{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Lens
import Control.Monad (forM)
import Control.Applicative

import Data.Semigroup
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)

import qualified Data.ByteString.Lazy as BS
import Data.Serialize (encodeLazy)
import Data.Serialize.ZipList ()
import Codec.Compression.GZip (compress)

import GHC.Float

import Data.YODA.Obj
import Data.TTree
import Data.Atlas.Histogramming
import Data.Atlas.CrossSections

import Options.Generic


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


weight :: MonadIO m => TR m Double
weight = float2Double . product
    <$> sequence
        [ readBranch "eventWeight"
        , readBranch "MCEventWeight"
        , readBranch "PileupWeight"
        , readBranch "leptonSF"
        , readBranch "trigSF"
        ]


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

muH :: MonadIO m => Filler m
muH = fillWH tr h
    where
        tr = Just . float2Double <$> readBranch "mu"
        h = yodaHist 100 0 100 "/mu" "$ <\\mu> $" ""

jetEtaH :: MonadIO m => Filler m
jetEtaH = fillWH tr h
    where
        tr :: MonadIO m => TR m (Vector Double)
        tr = fmap float2Double <$> readBranch "jetsMomEta"
        h = yodaHist 100 (-3.0) 3.0 "/jeteta" "jet $\\eta$ [GeV]" ""

jetPtH :: MonadIO m => Filler m
jetPtH = fillWH tr h
    where
        tr :: MonadIO m => TR m (Vector Double)
        tr = fmap float2Double <$> readBranch "jetsMomPt"
        h = yodaHist 100 0 100000 "/jetpt" "jet $p_{\\mathrm T}$ [GeV]" ""

hists :: MonadIO m => Filler m
hists = mconcat
    [ muH
    , jetPtH
    , jetEtaH
    ]

data Args = Args { outfile :: String
                 , infiles :: String
                 , xsecfile :: String
                 } deriving (Show, Generic)

instance ParseRecord Args where

main :: IO ()
main = do
    args <- getRecord "run-hs" :: IO Args
    xsecs <- fromMaybe (error "failed to parse xsec file.")
                <$> readXSecFile (xsecfile args)

    fs <- filter (not . null) . lines <$> readFile (infiles args)

    ts <- mapM (ttree "FlavourTagging_Nominal") fs

    hs <- forM ts $ runFiller hists

    BS.writeFile (outfile args) (compress $ encodeLazy hs)
