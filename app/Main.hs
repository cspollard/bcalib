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

import BCalib.Histograms


data Args = Args { outfile :: String
                 , infiles :: String
                 } deriving (Show, Generic)

instance ParseRecord Args where

main :: IO ()
main = do
    args <- getRecord "run-hs" :: IO Args

    fns <- filter (not . null) . lines <$> readFile (infiles args)

    let collapse = IM.fromListWith (\(n, ms) (n', ms') -> (n+n', mergeYO <$> ms <*> ms'))


    BS.writeFile (outfile args) (compress . encodeLazy . over (traverse._2.traverse.path) ("/bcalib" <>) $ hs)

    where
        addFile fn im = do
            putStrLn ("analyzing file " ++ fn) >> hFlush stdout

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

            t <- do
                t' <- ttree f "FlavourTagging_Nominal"
                if isNullTree t'
                    then L.empty
                    else withWeight <$> project t'

            hs' <- (fromEnum dsid,) . (ninitial,)
                <$> F.purely L.fold (lepFlavorChannels . lepChargeChannels . nJetChannels $ eventHs) t

            tfileClose f
            return hs'
