{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
import Control.Comonad (Comonad(..))
import Control.Monad (when)
import Data.Semigroup ((<>))
import Data.List (isInfixOf)

import qualified List.Transformer as L
import qualified Control.Foldl as F

import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM
import qualified Data.Text as T

import qualified Data.ByteString.Lazy as BS
import Data.Serialize (encodeLazy)
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
    let fnl = L.select fns :: L.ListT IO String

    let f = F.FoldM fillFile (return Nothing) return

    (imf :: Maybe (Int, Double, Fill Event)) <- F.impurely L.foldM f fnl
    let imh = over (_Just._3') extract imf

    putStrLn ("writing to file " ++ outfile args) >> hFlush stdout

    BS.writeFile (outfile args) (compress . encodeLazy . over (_Just._3') (M.mapKeysMonotonic ("/bcalib" <>)) $ imh)


fillFile :: Maybe (Int, Double, Fill Event) -> String -> IO (Maybe (Int, Double, Fill Event))
fillFile m fn = do
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

    -- deal with possible missing trees
    nt <- isNullTree t
    let l = if nt then (L.empty :: L.ListT IO (Double, Event)) else withWeight . overlapRemoval <$> project t

    case m of
        Nothing -> do
            hs <- F.purely L.fold (duplicate defHs) l <* tfileClose f
            hs `seq` return (Just (dsid, ninitial, hs))

        Just (dsid', n, hs') -> do
            when (dsid /= dsid') $ error "attempting to analyse different dsids in one run!!!"

            hs <- F.purely L.fold (duplicate hs') l <* tfileClose f
            hs `seq` return (Just (dsid, n+ninitial, hs))


    where
        defHs :: Fill Event
        defHs = lepFlavorChannels . lepChargeChannels . nJetChannels $ eventHs