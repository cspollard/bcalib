{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module BCalib.Systematics where

import           Data.Map.Strict as M
import           Data.Semigroup
import           Data.Text       as T
import           GHC.Float

import           BCalib.Event
import           Data.TTree

type TreeName = Text
type SystName = Text
type WeightName = String

data Systematic =
  Systematic
    { systName     :: SystName
    , systTreeName :: TreeName
    , systWeights  :: [WeightName]
    } deriving Show

readWeight :: MonadIO m => [WeightName] -> TR m Double
readWeight wns = float2Double . product <$> traverse readBranch wns

nominalWeight :: Map SystName [WeightName]
nominalWeight = M.singleton "nominal" ["eventWeight"]

weightSysts :: Map SystName [WeightName]
weightSysts = nominalWeight

treeSysts :: Map SystName TreeName
treeSysts =
  fromList
    $ f <$>
      [ "JET_EffectiveNP_1__1up" ]

  where f s = (s, "FlavourTagging_" <> s)

readWeights :: MonadIO m => Map SystName [WeightName] -> TR m (Map SystName Double)
readWeights = traverse readWeight

systReads :: MonadIO m => Map SystName [WeightName] -> TR m (Map SystName (Event, Double))
systReads ws = do
  weights <- readWeights ws
  evt <- fromTTree
  return $ fmap (evt,) weights


allSysts :: Map String (String, [String])
allSysts = M.singleton "nominal" ("FlavourTagging_Nominal", ["eventWeight"])
