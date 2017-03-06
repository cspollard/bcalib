{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

module BCalib.Systematics where

import           Control.Monad.Trans.Class
import           Data.Map.Strict           as M
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text                 as T
import           GHC.Float
import           GHC.TypeLits

import           Data.Atlas.Variation
import           Data.TTree

type TreeName = String
type SystName = T.Text
type WeightName = String

type SystMap = M.Map SystName

nominalWeight :: MonadIO m => VariationT "nominal" (TR m) Double
nominalWeight = float2Double <$> lift (readBranch "eventWeight")

systName :: forall s m a. KnownSymbol s => VariationT s m a -> T.Text
systName _ = T.pack $ symbolVal p
  where p = Proxy :: Proxy s

tupV :: KnownSymbol s => VariationT s f t -> (T.Text, f t)
tupV vsa = (systName vsa, runVariationT vsa)

weightSysts :: MonadIO m => TR m (Map SystName Double)
weightSysts = sequence
  [ tupV nominalWeight
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
      , sequence $ M.singleton (T.pack s) (float2Double <$> readBranch "eventWeight")
      )


allSysts :: MonadIO m => [(TreeName, TR m (Map SystName Double))]
allSysts = ("FlavourTagging_Nominal", weightSysts) : treeSysts
