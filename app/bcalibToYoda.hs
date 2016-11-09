{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens hiding (Fold)
import Control.Applicative (ZipList(..), liftA2)

import qualified Control.Foldl as F
import qualified List.Transformer as L

import qualified Data.ByteString.Lazy as BS
import Data.Serialize (decodeLazy)
import Data.Serialize.ZipList ()
import Codec.Compression.GZip (decompress)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.IntMap as IM
import Data.Maybe (fromMaybe)

import Data.YODA.Obj
import Data.Atlas.CrossSections
import Data.Atlas.ProcessInfo

import Options.Applicative
import System.IO (hFlush, stdout)

data InArgs =
    InArgs
        { outfolder :: String
        , xsecfile :: String
        , lumi :: Double
        , infiles :: [String]
        }

inArgs :: Parser InArgs
inArgs = InArgs
    <$> strOption
        ( long "outfolder"
        <> metavar "OUTFOLDER" )
    <*> strOption
        ( long "xsecfile"
        <> metavar "XSECFILE" )
    <*> option auto
        ( long "lumi"
        <> metavar "LUMI" )
    <*> some (strArgument (metavar "INFILES"))

opts :: ParserInfo InArgs
opts = info (helper <*> inArgs) fullDesc


main :: IO ()
main = do
    args <- execParser opts

    xsecs <- fromMaybe (error "failed to parse xsec file.")
                <$> (fmap.fmap.fmap) fst (readXSecFile (xsecfile args))

    let fole = F.FoldM (\x s -> IM.unionWith mergeRuns x <$> decodeFile s) (return IM.empty) return
    im <- F.impurely L.foldM fole (L.select (infiles args) :: L.ListT IO String)

    let im' = flip IM.mapMaybeWithKey im $
                \ds (n, hs) ->
                    if ds == 0
                        then Just $ hs
                                & traverse.annots.at "LineStyle" ?~ "none"
                                & traverse.annots.at "LineColor" ?~ "Black"
                                & traverse.annots.at "DotSize" ?~ "0.1"
                                & traverse.annots.at "ErrorBars" ?~ "1"
                                & traverse.annots.at "PolyMarker" ?~ "*"
                        else if processTitle ds == "other"
                                then Nothing
                                else Just $
                                        over (traverse.noted._H1DD) (scaling $ lumi args * (xsecs IM.! ds) / n) hs

    let im'' = let f k = if k /= 0 && (k < 410000 || k > 410004)
                            then dsidOTHER
                            else k
               in IM.mapKeysWith (liftA2 mergeYO) f im'

    let im''' = case dsidOTHER `IM.lookup` im'' of
                    Nothing -> im''
                    Just hs -> im''
                                & ix 410000 %~ liftA2 mergeYO hs
                                & ix 410001 %~ liftA2 mergeYO hs
                                & ix 410002 %~ liftA2 mergeYO hs
                                & ix 410003 %~ liftA2 mergeYO hs
                                & ix 410004 %~ liftA2 mergeYO hs

    iforM_ im''' $
        \ds hs -> T.writeFile (outfolder args ++ '/' : show ds ++ ".yoda")
                    (T.unlines $ hs ^.. traverse.to printYObj)

    where
        dsidOTHER = 999999

mergeRuns :: (Double, ZipList YodaObj) -> (Double, ZipList YodaObj) -> (Double, ZipList YodaObj)
mergeRuns (sumwgt, hs) (sumwgt', hs') = (sumwgt+sumwgt', liftA2 mergeYO hs hs')

decodeFile :: String -> IO (IM.IntMap (Double, ZipList YodaObj))
decodeFile f = do
    putStrLn ("decoding file " ++ f) >> hFlush stdout
    eim <- decodeLazy . decompress <$> BS.readFile f ::
                IO (Either String (IM.IntMap (Double, ZipList YodaObj)))

    case eim of
         Left _ -> error $ "failed to decode file " ++ f
         Right im -> return im
