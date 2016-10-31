{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Lens
import Control.Monad (forM)
import Control.Applicative

import Foreign.C.String

import Data.Maybe (fromMaybe)
import qualified Data.IntMap as IM

import qualified List.Transformer as L

import qualified Data.ByteString.Lazy as BS
import Data.Serialize (encodeLazy)
import Data.Serialize.ZipList ()
import Codec.Compression.GZip (compress)

import Data.TH1
import Data.Atlas.CrossSections

import Options.Generic

import BCalib.Histograms


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

    -- TODO
    -- this folding doesn't need to store histograms from each file
    -- in memory...
    hs <- forM fs $ \f -> do
        putStrLn $ "analyzing file " ++ f
        h <- withCString "MetaData_EventCount" $ \hn -> withCString f (th1d hn)
        ninitial <- entryd h 4
        t <- ttree "FlavourTagging_Nominal" f
        (L.Cons dsid _) <- L.next $ runTTreeL (readBranch "sampleID") t :: IO (L.Step IO CInt)
        (fromEnum dsid,) . (ninitial,) <$> runFiller bcalibHists t

    let hs' = IM.fromListWith (\(n, ms) (n', ms') -> (n+n', liftA2 mergeYO ms ms')) hs

    print hs'
    BS.writeFile (outfile args) (compress $ encodeLazy hs')
