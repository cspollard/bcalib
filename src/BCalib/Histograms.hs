{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module BCalib.Histograms
    ( module X
    , hist1DDef, prof1DDef
    , Fill
    , channel, channels
    , selector
    , lvHs, nH
    , (<$$=)
    ) where

import qualified Control.Foldl            as F
import           Control.Lens             as X
import           Data.Bifunctor
import qualified Data.Histogram.Generic   as G
import qualified Data.Map.Strict          as M
import           Data.Semigroup           as X
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Vector              as V

import           Data.Atlas.Histogramming as X
import           Data.HEP.LorentzVector   as X
import           Data.YODA.Obj            as X

type Fill a = F.Fold (a, Double) YodaFolder

channel :: T.Text -> (a -> Bool) -> Fill a -> Fill a
channel n f fills = M.mapKeysMonotonic (n <>) <$> F.handles (selector (f.fst)) fills


channels :: [(T.Text, a -> Bool)] -> Fill a -> Fill a
channels fns fills = mconcat $ uncurry channel <$> fns <*> pure fills


hEmpty :: (Bin bin, Monoid a) => bin -> Histogram V.Vector bin a
hEmpty b =
  let v = V.replicate (nBins b) mempty
      uo = Just (mempty, mempty)
  in G.histogramUO b uo v

hist1DDef
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> Text -> Text -> Text -> F.Fold (Double, Double) YodaFolder
hist1DDef b xt yt pa =
    M.singleton pa
      . Annotated [("XTitle", xt), ("YTitle", yt)]
      . H1DD
      . over bins toArbBin
      <$> hist1DFill (hEmpty b)

prof1DDef
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> Text -> Text -> Text -> F.Fold ((Double, Double), Double) YodaFolder
prof1DDef b xt yt pa =
    M.singleton pa
      . Annotated [("XTitle", xt), ("YTitle", yt)]
      . P1DD
      . over bins toArbBin
      <$> prof1DFill (hEmpty b)


nH :: Foldable f => Int -> Fill (f a)
nH n =
  F.premap (first $ fromIntegral . length)
    $ hist1DDef (binD 0 n (fromIntegral n)) "$n$" (dsigdXpbY "n" "1") "/n"

ptH :: HasLorentzVector a => Fill a
ptH =
  F.premap (first $ view lvPt)
    $ hist1DDef
      (binD 0 50 500)
      "$p_{\\mathrm T}$ [GeV]"
      (dsigdXpbY pt gev)
      "/pt"

etaH :: HasLorentzVector a => Fill a
etaH =
  F.premap (first $ view lvEta)
    $ hist1DDef
      (binD (-3) 39 3)
      "$\\eta$"
      (dsigdXpbY "\\eta" "{\\mathrm rad}")
      "/etat"


-- generic histograms for a lorentz vector
lvHs :: HasLorentzVector a => Fill a
lvHs = mappend <$> ptH <*> etaH

selector :: (a -> Bool) -> Prism' a a
selector f = prism' id $ \x -> if f x then Just x else Nothing

infixl 2 <$$=
(<$$=) :: Fill b -> Getter a b -> Fill a
fs <$$= l = F.premap (first (view l)) fs
