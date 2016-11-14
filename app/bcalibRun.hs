{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
import Control.Monad (forM)
import Data.Semigroup ((<>))
import Data.List (isInfixOf)

import qualified List.Transformer as L
import qualified Control.Foldl as F

import qualified Data.IntMap as IM
import qualified Data.Text as T

import qualified Data.ByteString.Lazy as BS
import Data.Serialize (encodeLazy)
import Data.Serialize.ZipList ()
import Codec.Compression.GZip (compress)

import Data.TFile
import Data.TH1

import Options.Generic
import System.IO (hFlush, stdout)

import BCalib.Event
import BCalib.Histograms


data Args = Args { outfile :: String
                 , infiles :: String
                 } deriving (Show, Generic)

instance ParseRecord Args where

main :: IO ()
main = do
    args <- getRecord "run-hs" :: IO Args

    fns <- filter (not . null) . lines <$> readFile (infiles args)

    -- TODO
    -- this folding doesn't need to store histograms from each file
    -- in memory...
    -- TODO
    -- TODO
    -- turn this into a fold
    -- that looks to see if we've already filled hists for this dsid
    -- if so: continue filling the histograms
    -- if not: use new fold.
    let collapse = IM.fromListWith (\(n, ms) (n', ms') -> (n+n', mergeYO <$> ms <*> ms'))
    hs <- fmap collapse . forM fns $ \fn -> do
        putStrLn ("analyzing file " ++ fn) >> hFlush stdout

        -- check whether or not this is a data file
        let (dsid :: Int) =
                if "data15_13TeV" `isInfixOf` fn || "data16_13TeV" `isInfixOf` fn
                    then 0
                    else fn
                        & read . T.unpack . (!! 3)
                        . T.split (== '.') . (!! 1)
                        . reverse . T.split (== '/') . T.pack

        f <- tfileOpen fn
        h <- tfileGet f "MetaData_EventCount"
        ninitial <- entryd h 4

        t <- ttree f "FlavourTagging_Nominal"
        nt <- isNullTree t

        let hs = lepFlavorChannels . lepChargeChannels . nJetChannels $ eventHs

        (dsid,) . (ninitial,)
            <$> F.purely L.fold hs (if nt then L.empty else withWeight <$> project t)
            <* tfileClose f

    putStrLn $ "writing to file " ++ outfile args
    hFlush stdout

    BS.writeFile (outfile args) (compress . encodeLazy . over (traverse._2.traverse.path) ("/bcalib" <>) $ hs)
