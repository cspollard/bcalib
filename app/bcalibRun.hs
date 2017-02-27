{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Codec.Compression.GZip (compress)
import qualified Control.Foldl          as F
import           Control.Monad          (forM, when)
import qualified Data.ByteString.Lazy   as BS
import           Data.List              (isInfixOf)
import qualified Data.Map.Strict        as M
import           Data.Serialize         (encodeLazy)
import qualified Data.Text              as T
import qualified List.Transformer       as L
import           Options.Generic
import           System.IO              (hFlush, stdout)

import           BCalib.Event
import           BCalib.Imports
import           BCalib.Systematics
import           Data.TFile
import           Data.TH1


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
      f = F.FoldM (fillFile allSysts) (return Nothing) return

  imh <- F.impurely L.foldM f fnl

  putStrLn ("writing to file " ++ outfile args) >> hFlush stdout

  BS.writeFile (outfile args) (compress . encodeLazy $ imh)


fillFile
  :: [(TreeName, TR IO (M.Map SystName Double))]
  -> Maybe (Int, Double, SystMap YodaFolder)
  -> String
  -> IO (Maybe (Int, Double, SystMap YodaFolder))
fillFile systs m fn = do
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

  systHs <- fmap M.unions . forM systs $ \(tn, readws) -> do
    t <- ttree f tn
    putStrLn $ "looping over tree " <> tn

    -- deal with possible missing trees
    nt <- isNullTree t
    when nt $ do
      putStrLn $ "missing tree " <> tn <> " in file " <> fn <> "."
      putStrLn "continuing."

    let l =
          if nt
            then L.empty
            else runTTreeL tmp t
        tmp = do
          evt <- overlapRemoval <$> fromTTree
          liftIO $ print evt
          if dsid == 0
            then return (evt, M.singleton "data" 1)
            else do
              ws <- readws
              liftIO $ print ws
              return (evt, ws)

    F.purely L.fold defHs l

  tfileClose f

  case m of
      Nothing -> return $ Just (dsid, ninitial, systHs)
      Just (dsid', n, hs') -> do
        when (dsid /= dsid') $ error "attempting to analyze different dsids in one run!!!"
        return $ Just (dsid, n+ninitial, M.unionWith mergeYF systHs hs')

  where
    defHs :: F.Fold (Event, SystMap Double) (SystMap YodaFolder)
    defHs = withWeights . lepFlavorChannels . lepChargeChannels . nJetChannels $ eventHs
