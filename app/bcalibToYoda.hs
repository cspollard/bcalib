{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens hiding (Fold)
import Control.Applicative (ZipList(..))
import Control.Monad (when)

import qualified Control.Foldl as F
import qualified List.Transformer as L

import qualified Data.ByteString.Lazy as BS
import Data.Serialize (decodeLazy)
import Data.Serialize.ZipList ()
import Codec.Compression.GZip (decompress)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
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

    let im' = flip IM.mapWithKey im $
                \ds (n, hs) ->
                    if ds == 0
                        then hs & traverse.annots.at "LineStyle" ?~ "none"
                                & traverse.annots.at "LineColor" ?~ "Black"
                                & traverse.annots.at "DotSize" ?~ "0.1"
                                & traverse.annots.at "ErrorBars" ?~ "1"
                                & traverse.annots.at "PolyMarker" ?~ "*"
                                & traverse.annots.at "Title" ?~ "data"

                        else hs & traverse.noted._H1DD %~ scaling (lumi args * (xsecs IM.! ds) / n)
                                & traverse.annots.at "Title" ?~ processTitle ds

    -- lump together non-ttbar processes
    let im'' = let f k = if k /= 0 && (k < 410000 || k > 410004)
                            then dsidOTHER
                            else k
               in IM.mapKeysWith (M.unionWith mergeYO) f im'

    let im''' = case dsidOTHER `IM.lookup` im'' of
                    Nothing -> im''
                    Just hs -> im''
                                & ix 410000 %~ M.unionWith (flip mergeYO) hs
                                & ix 410001 %~ M.unionWith (flip mergeYO) hs
                                & ix 410002 %~ M.unionWith (flip mergeYO) hs
                                & ix 410003 %~ M.unionWith (flip mergeYO) hs
                                & ix 410004 %~ M.unionWith (flip mergeYO) hs

    iforM_ im''' $
        \ds hs -> T.writeFile (outfolder args ++ '/' : show ds ++ ".yoda")
                    (T.unlines $ hs ^.. traverse.to printYObj)


    {-
    -- TODO
    -- this could be optimized a lot.
    let immc = im''' `sans` 0 `sans` dsidOTHER
    let mdata = im''' IM.! 0
    let imhf = flip (over traverse) immc . M.filterWithKey $
                    \k _ -> "/bottom/" `T.isInfixOf` k

    -- TODO
    -- this won't work
    -- need to merge charm and light first.
    let imlf = flip (over traverse) immc . M.filterWithKey $
                    \k _ -> "/light/" `T.isInfixOf` k || "/charm/" `T.isInfixOf` k

    let imlf' = flip (over traverse) imlf $
                    M.mapKeys (T.replace "light/" "" . T.replace "charm/" "")

    let imhf' = flip (over traverse) imhf $
                    M.mapKeys (T.replace "bottom/" "")

    return ()
    -}

    where
        dsidOTHER = 999999


type YodaFolder = M.Map T.Text YodaObj

-- this needs to be strict or else...!!!
mergeRuns :: (Double, YodaFolder) -> (Double, YodaFolder) -> (Double, YodaFolder)
mergeRuns (sumwgt, hs) (sumwgt', hs') = ((,) $! sumwgt+sumwgt') $! M.unionWith mergeYO hs hs'

decodeFile :: String -> IO (IM.IntMap (Double, YodaFolder))
decodeFile f = do
    putStrLn ("decoding file " ++ f) >> hFlush stdout
    eim <- decodeLazy . decompress <$> BS.readFile f ::
                IO (Either String (IM.IntMap (Double, ZipList YodaObj)))

    case eim of
         Left _ -> error $ "failed to decode file " ++ f

         -- throw out samples we don't need immediately.
         Right im -> return . flip IM.mapMaybeWithKey im $
                        \ds hs -> if processTitle ds == "other"
                                    then Nothing
                                    else Just $ over _2 toMap hs

    where
        toMap :: ZipList YodaObj -> YodaFolder
        toMap hs = M.fromList . getZipList $ fmap (\h -> let n = view path h in (n, h)) hs
