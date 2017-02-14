{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module BCalib.Histograms
    ( module X
    , fillH1L, fillP1L
    , Fill, (<$$=)
    , channel, channels
    , selector
    , lvHs, nH
    ) where
{-
    ) where
-}

import Control.Lens as X
import Control.Applicative as X (ZipList(..), liftA2)

import qualified Control.Foldl as F
import qualified Data.Map.Strict as M

import Data.Semigroup as X
import qualified Data.Text as T

import Data.HEP.LorentzVector as X
import Data.YODA.Obj as X
import Data.TTree as X
import Data.Atlas.Histogramming as X

type Fill a = F.Fold (Double, a) YodaFolder

fill :: Fillable h => (a -> FillVec h) -> (Weight h, a) -> h -> h
fill l (w, x) = filling w (l x)

fillH1L :: Getter a Double -> T.Text -> YodaObj -> Fill a
fillH1L l n yh = M.singleton n <$> F.Fold (\yh' xs -> over (noted._H1DD) (fill (view l) xs) yh') yh id

fillP1L :: Getter a (Double, Double) -> T.Text -> YodaObj -> Fill a
fillP1L l n yp = M.singleton n <$> F.Fold (\yp' xs -> over (noted._P1DD) (fill (view l) xs) yp') yp id


selector :: (a -> Bool) -> Prism' a a
selector f = prism' id $ \x -> if f x then Just x else Nothing

channel :: T.Text -> (a -> Bool) -> Fill a -> Fill a
channel n f fills = M.mapKeysMonotonic (n <>) <$> F.handles (selector (f.snd)) fills


channels :: [(T.Text, a -> Bool)] -> Fill a -> Fill a
channels fns fills = mconcat $ uncurry channel <$> fns <*> pure fills


nH :: Foldable f => Int -> Fill (f a)
nH mx = fillH1L (to $ fromIntegral.length) "/n" $
    yodaHist mx 0 (fromIntegral mx) "$n$" (dsigdXpbY "n" "1")

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
