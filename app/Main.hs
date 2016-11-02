{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
import Control.Monad (forM)
import Data.Semigroup ((<>))

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
    hs <- forM fns $ \fn -> do
        putStrLn $ "analyzing file " ++ fn

        let (dsid :: Int) = fn 
                & read . T.unpack . (!! 2)
                . T.split (== '.') . (!! 1)
                . reverse . T.split (== '/') . T.pack

        let dsid' = if dsid < 100000 then 0 else dsid

        f <- tfileOpen fn
        h <- tfileGet f "MetaData_EventCount"
        ninitial <- entryd h 4

        t <- ttree f "FlavourTagging_Nominal"
        (fromEnum dsid',) . (ninitial,)
            <$> F.purely L.fold eventHs (project t)
            <* tfileClose f

    let hs' = IM.fromListWith (\(n, ms) (n', ms') -> (n+n', mergeYO <$> ms <*> ms')) hs

    BS.writeFile (outfile args) (compress . encodeLazy . over (traverse._2.traverse.path) ("/lljj" <>) $ hs')
