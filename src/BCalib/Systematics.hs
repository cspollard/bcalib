{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module BCalib.Systematics where

import           Data.Map.Strict as M
import           Data.Semigroup
import qualified Data.Text       as T
import           GHC.Float

import           Data.TTree

type TreeName = T.Text
type SystName = T.Text
type WeightName = String

type SystMap = M.Map SystName

data Systematic =
  Systematic
    { systName     :: SystName
    , systTreeName :: TreeName
    , systWeights  :: [WeightName]
    } deriving (Show, Eq, Ord)

weightSysts :: MonadIO m => TR m (Map SystName Double)
weightSysts = sequence
  [ ("nominal", float2Double <$> readBranch "eventWeight")
  , ("pileup_up", puwup)
  ]

  where
    puwup = do
      w <- readBranch "eventWeight"
      puw <- readBranch "PileupWeight"
      wup <- head <$> readBranch "PileupWeightSys"
      return . float2Double $ w * wup / puw

treeSysts :: Map SystName TreeName
treeSysts =
  fromList
    $ f <$>
      [ "JET_EffectiveNP_1__1up" ]

  where f s = (s, "FlavourTagging_" <> s)
