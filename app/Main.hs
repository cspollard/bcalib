{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.Float

import Control.Lens
import Control.Applicative

import System.Environment (getArgs)
import Control.Monad (forM)

import Data.YODA.Obj
import Data.TTree
import Data.Atlas.Histogramming

fillH :: MonadIO m => TR m Double -> TR m Double -> YodaObj -> TR m YodaObj
fillH obs wgt h = do
    x <- obs
    w <- wgt
    return $ over (noted._H1DD) (filling w x) h

fillP :: MonadIO m => TR m (Double, Double) -> TR m Double -> YodaObj -> TR m YodaObj
fillP obs wgt h = do
    xs <- obs
    w <- wgt
    return $ over (noted._P1DD) (filling w xs) h


weight :: MonadIO m => TR m Double
weight = return 1.0

ff :: Monad m => [YodaObj -> TR m YodaObj] -> [YodaObj] -> TR m [YodaObj]
ff fs hs = fmap getZipList . sequence $ ZipList fs <*> ZipList hs

muH :: YodaObj
muH = yodaHist 100 0 100 "/mu" "<\\mu>" ""

main :: IO ()
main = do (tn:fns) <- getArgs
          ts <- mapM (ttree tn) fns

          ns <- forM ts $
                    \t -> runTTree (fillH (float2Double <$> readBranch "mu") weight) t muH

          print ns
