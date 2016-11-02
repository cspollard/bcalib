{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
import Control.Monad (forM)
import Data.Semigroup ((<>))

import qualified Data.IntMap as IM

import qualified List.Transformer as L
import qualified Control.Foldl as F

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
        f <- tfileOpen fn
        h <- tfileGet f "MetaData_EventCount"
        ninitial <- entryd h 4
        t <- ttree f "FlavourTagging_Nominal"
        (L.Cons dsid _) <- L.next $ runTTreeL (readBranch "sampleID") t :: IO (L.Step IO CInt)
        (fromEnum dsid,) . (ninitial,) <$> F.purely L.fold eventHs (project t) <* tfileClose f

    let hs' = IM.fromListWith (\(n, ms) (n', ms') -> (n+n', mergeYO <$> ms <*> ms')) hs

    BS.writeFile (outfile args) (compress . encodeLazy . over (traverse._2.traverse.path) ("/lljj" <>) $ hs')
