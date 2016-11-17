{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module BCalib.Histograms
    ( module X
    , fillH1L, fillP1L
    , Fill, Fills, (<$$=)
    , channel, channels
    , lvHs, nH
    ) where
{-
    ) where
-}

import Control.Lens as X
import Control.Applicative as X (ZipList(..), liftA2)

import qualified Control.Foldl as F

import Data.Semigroup as X
import qualified Data.Text as T

import Data.HEP.LorentzVector as X
import Data.YODA.Obj as X
import Data.TTree as X
import Data.Atlas.Histogramming as X

instance Semigroup (ZipList a) where
    ZipList xs <> ZipList ys = ZipList $ xs <> ys

instance Monoid (ZipList a) where
    mempty = ZipList []
    mappend = (<>)

instance Semigroup b => Semigroup (F.Fold a b) where
    (<>) = liftA2 (<>)


type Fill a = F.Fold (Double, a) YodaObj
type Fills a = F.Fold (Double, a) (ZipList YodaObj)

fill :: Fillable h => (a -> FillVec h) -> (Weight h, a) -> h -> h
fill l (w, x) = filling w (l x)

fillH1L :: Getter a Double -> YodaObj -> Fill a
fillH1L l yh = F.Fold (\yh' xs -> over (noted._H1DD) (fill (view l) xs) yh') yh id

fillP1L :: Getter a (Double, Double) -> YodaObj -> Fill a
fillP1L l yp = F.Fold (\yp' xs -> over (noted._P1DD) (fill (view l) xs) yp') yp id


selector :: (a -> Bool) -> Prism' (Double, a) (Double, a)
selector f = prism' id $ \wx@(_, x) -> if f x then Just wx else Nothing

channel :: T.Text -> (a -> Bool) -> Fills a -> Fills a
channel n f fills = fmap (over path (n <>)) <$> F.handles (selector f) fills


channels :: [(T.Text, a -> Bool)] -> Fills a -> Fills a
channels fns fills = fmap mconcat . sequenceA $ uncurry channel <$> fns <*> pure fills


nH :: Foldable f => Int -> Fill (f a)
nH mx = fillH1L (to $ fromIntegral.length) $
    yodaHist mx 0 (fromIntegral mx) "/n" "$n$" (dsigdXpbY "n" "1")

ptH :: HasLorentzVector a => Fill a
ptH = fillH1L lvPt $
    yodaHist 25 0 250000 "/pt" "$p_{\\mathrm T}$ [MeV]" (dsigdXpbY pt mev)

etaH :: HasLorentzVector a => Fill a
etaH = fillH1L lvEta $
    yodaHist 30 (-3) 3 "/eta" "$\\eta$" (dsigdXpbY "\\eta" "{\\mathrm rad}")

infixl 2 <$$=
(<$$=) :: Fills b -> Getter a b -> Fills a
fs <$$= l = F.premap (fmap (view l)) fs


-- generic histograms for a lorentz vector
lvHs :: HasLorentzVector a => Fills a
lvHs = sequenceA (ZipList [ptH, etaH])
