{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Codec.Compression.GZip (compress)
import           Control.Comonad        (Comonad (..))
import qualified Control.Foldl          as F
import           Control.Lens
import           Control.Monad          (when)
import qualified Data.ByteString.Lazy   as BS
import           Data.List              (isInfixOf)
import qualified Data.Map.Strict        as M
import           Data.Serialize         (encodeLazy)
import qualified Data.Text              as T
import qualified List.Transformer       as L
import           Options.Generic
import           System.IO              (hFlush, stdout)

import           BCalib.Event
import           BCalib.Histograms
import           BCalib.Systematics
import           Data.TFile
import           Data.TH1
import           Data.TTree


data Args =
  Args
    { outfile :: String
    , infiles :: String
    } deriving (Show, Generic)

instance ParseRecord Args where

main :: IO ()
main = do
  args <- getRecord "run-hs" :: IO Args

  fns <- filter (not . null) . lines <$> readFile (infiles args)
  let fnl = L.select fns :: L.ListT IO String
      f tn readws = F.FoldM (fillFile tn readws) (return Nothing) return

  (imf :: Maybe (Int, Double, F.Fold (Event, SystMap Double) (SystMap YodaFolder))) <-
    F.impurely L.foldM (f "FlavourTagging_Nominal" weightSysts) fnl

  -- let imf' = ifoldlOf traversed (\s m m' -> maybe m (M.union m') )
  let imh = over (_Just._3') extract imf

  putStrLn ("writing to file " ++ outfile args) >> hFlush stdout

  BS.writeFile (outfile args) (compress . encodeLazy $ imh)


fillFile
  :: String
  -> TR IO (M.Map SystName Double)
  -> Maybe (Int, Double, F.Fold (Event, SystMap Double) (SystMap YodaFolder))
  -> String
  -> IO (Maybe (Int, Double, F.Fold (Event, SystMap Double) (SystMap YodaFolder)))
fillFile tn readws m fn = do
  putStrLn ("analyzing file " ++ fn) >> hFlush stdout

  -- check whether or not this is a data file
  let (dsid :: Int) =
          if "data15_13TeV" `isInfixOf` fn || "data16_13TeV" `isInfixOf` fn
            then 0
            -- this works on files of the form
            -- /blah/blah/blah/blah.blah.blah.DSID.blah/blah
            else
              fn
                & read . T.unpack . (!! 3)
                  . T.split (== '.') . (!! 1)
                  . reverse . T.split (== '/') . T.pack

  f <- tfileOpen fn
  h <- tfileGet f "MetaData_EventCount"
  ninitial <- entryd h 4

  t <- ttree f tn

  -- deal with possible missing trees
  nt <- isNullTree t
  let l =
        if nt
          then (L.empty :: L.ListT IO (Event, SystMap Double))
          else runTTreeL tmp t
      tmp = do
        evt <- overlapRemoval <$> fromTTree
        if dsid == 0
          then return (evt, M.singleton "data" 1)
          else do
            ws <- readws
            return (evt, ws)

  case m of
    Nothing -> do
      hs <- F.purely L.fold (duplicate defHs) l <* tfileClose f
      return (Just (dsid, ninitial, hs))

    Just (dsid', n, hs') -> do
      when (dsid /= dsid') $ error "attempting to analyze different dsids in one run!!!"

      hs <- F.purely L.fold (duplicate hs') l <* tfileClose f
      return (Just (dsid, n+ninitial, hs))


  where
    defHs :: F.Fold (Event, SystMap Double) (SystMap YodaFolder)
    defHs = withWeights . lepFlavorChannels . lepChargeChannels . nJetChannels $ eventHs
