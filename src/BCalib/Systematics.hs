{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module BCalib.Systematics where

import           Data.Map.Strict as M
import           Data.Semigroup
import qualified Data.Text       as T
import           Debug.Trace
import           GHC.Float

import           Data.TTree

type TreeName = String
type SystName = T.Text
type WeightName = String

type SystMap = M.Map SystName

nominalWeight :: MonadIO m => TR m Double
nominalWeight = float2Double <$> readBranch "eventWeight"


weightSysts :: MonadIO m => TR m (Map SystName Double)
weightSysts = sequence
  [ ("nominal", nominalWeight)
  , ("pileup_up", puwup)
  ]

  where
    puwup = do
      w <- readBranch "eventWeight"
      puw <- readBranch "PileupWeight"
      wup <- head <$> readBranch "PileupWeightSys"
      return . float2Double $ w * wup / puw


treeSysts :: MonadIO m => [(TreeName, TR m (Map SystName Double))]
treeSysts =
  f <$>
    [ "JET_EffectiveNP_1__1up"
    , "JET_EffectiveNP_2__1up"
    , "JET_EffectiveNP_3__1up"
    , "JET_EffectiveNP_4__1up"
    , "JET_EffectiveNP_5__1up"
    , "JET_EffectiveNP_6restTerm__1up"
    , "EL_EFF_ID_TotalCorrUncertainty__1up"
    ]

  where
    f s =
      ( "FlavourTagging_" <> s
      , sequence $ M.singleton (T.pack s) (traceShow s . traceShowId <$> nominalWeight)
      )


allSysts :: MonadIO m => [(TreeName, TR m (Map SystName Double))]
allSysts = ("FlavourTagging_Nominal", weightSysts) : treeSysts
