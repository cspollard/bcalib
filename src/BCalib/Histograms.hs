{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module BCalib.Histograms where
    -- ( module X
    -- , fillH1L, fillP1L
    -- , Fill, (<$$=)
    -- , channel, channels
    -- , selector
    -- , lvHs, nH
    -- ) where

import           Control.Applicative      as X (ZipList (..), liftA2)
import qualified Control.Foldl            as F
import           Control.Lens             as X
import qualified Data.Histogram.Generic   as G
import qualified Data.Map.Strict          as M
import           Data.Proxy
import           Data.Semigroup           as X
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Vector              as V
import           GHC.TypeLits
import           Linear.V

import           Data.Atlas.Histogramming as X
import           Data.HEP.LorentzVector   as X
import           Data.TTree               as X
import           Data.YODA.Obj            as X

channel :: T.Text -> (a -> Bool) -> F.Fold a YodaFolder -> F.Fold a YodaFolder
channel n f fills = M.mapKeysMonotonic (n <>) <$> F.handles (selector f) fills


channels :: [(T.Text, a -> Bool)] -> F.Fold a YodaFolder -> F.Fold a YodaFolder
channels fns fills = mconcat $ uncurry channel <$> fns <*> pure fills


-- TODO
-- conversion from binning to ArbBin at the end of the day

hEmpty :: (Bin bin, Monoid a) => bin -> Histogram V.Vector bin a
hEmpty b =
  let v = V.replicate (nBins b) mempty
      uo = Just (mempty, mempty)
  in G.histogramUO b uo v

hist1DDefault
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> Text -> Text -> Text -> F.Fold (Double, Double) YodaFolder
hist1DDefault b xt yt pa =
    M.singleton pa
      . Annotated [("XTitle", xt), ("YTitle", yt)]
      . H1DD
      . over bins toArbBin
      <$> hist1DFill (hEmpty b)

prof1DDefault
  :: (BinValue b ~ Double, IntervalBin b)
  => b -> Text -> Text -> Text -> F.Fold ((Double, Double), Double) YodaFolder
prof1DDefault b xt yt pa =
    M.singleton pa
      . Annotated [("XTitle", xt), ("YTitle", yt)]
      . P1DD
      . over bins toArbBin
      <$> prof1DFill (hEmpty b)

nH
  :: Foldable f
  -- => Int -> Hist1DFill (ArbBin Double) (f a, Double)
  => Int -> F.Fold (f a, Double) YodaFolder
nH n =
  F.premap (over _1 (fromIntegral . length))
    $ hist1DDefault (binD 0 n (fromIntegral n)) "$n$" (dsigdXpbY "n" "1") "/n"

-- fillH1L (to $ fromIntegral.length) "/n" $
--     yodaHist mx 0 (fromIntegral mx) "$n$"

selector :: (a -> Bool) -> Prism' a a
selector f = prism' id $ \x -> if f x then Just x else Nothing

{-
ptH :: HasLorentzVector a => Fill a
ptH = fillH1L lvPt "/pt" $
    yodaHist 50 0 500 "$p_{\\mathrm T}$ [GeV]" (dsigdXpbY pt gev)

etaH :: HasLorentzVector a => Fill a
etaH = fillH1L lvEta "/eta" $
    yodaHist 30 (-3) 3 "$\\eta$" (dsigdXpbY "\\eta" "{\\mathrm rad}")

infixl 2 <$$=
(<$$=) :: Fill b -> Getter a b -> Fill a
fs <$$= l = F.premap (fmap (view l)) fs


-- generic histograms for a lorentz vector
lvHs :: HasLorentzVector a => Fill a
lvHs = mappend <$> ptH <*> etaH
-}
