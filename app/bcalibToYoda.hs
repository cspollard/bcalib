{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Function (on)

import qualified Control.Foldl as F
import qualified List.Transformer as L

import Text.Regex.Posix.String
import Text.Regex.Base.RegexLike

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

import BCalib.Histograms hiding (option, (<>))

data InArgs =
    InArgs
        { outfolder :: String
        , xsecfile :: String
        , lumi :: Double
        , regex :: String
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
    <*> strOption
        ( long "regex"
        <> metavar "REGEX=*"
        <> value "*" )
    <*> some (strArgument (metavar "INFILES"))

opts :: ParserInfo InArgs
opts = info (helper <*> inArgs) fullDesc


main :: IO ()
main = do
    args <- execParser opts

    xsecs <- fromMaybe (error "failed to parse xsec file.")
                <$> (fmap.fmap.fmap) fst (readXSecFile (xsecfile args))

    let fole = F.FoldM (\x s -> IM.unionWith mergeRuns x <$> decodeFile (regex args) s) (return IM.empty) return
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


    -- TODO
    -- this could be optimized a lot.
    let immc = sans 0 . sans dsidOTHER $ im'''

    -- light flavor
    let iml = flip (over traverse) immc . M.filterWithKey $
                    \k _ -> "/light/" `T.isInfixOf` k
    let iml' = flip (over (traverse.traverse)) iml $
                    (path %~ T.replace "/light/" "/allJetFlavs/")
                    . (annots.at "Title" ?~ "light")
    iforM_ iml' $
        \ds hs -> T.writeFile (outfolder args ++ '/' : show ds ++ "_light.yoda")
                    (T.unlines $ hs ^.. traverse.to printYObj)

    -- charm
    -- need to add light contribution for stack plots.
    let imc = flip (over traverse) immc . M.filterWithKey $
                    \k _ -> "/charm/" `T.isInfixOf` k
    let imc' = flip (over (traverse.traverse)) imc $
                    (path %~ T.replace "/charm/" "/allJetFlavs/")
                    . (annots.at "Title" ?~ "charm")
    let imc'' = IM.intersectionWith (liftA2 mergeYO `on` (ZipList . M.elems)) imc' iml'
    iforM_ imc'' $
        \ds hs -> T.writeFile (outfolder args ++ '/' : show ds ++ "_charm.yoda")
                    (T.unlines $ hs ^.. traverse.to printYObj)

    -- bottom
    -- use allJetFlavs since we're stacking on top of light and charm.
    let imb = flip (over traverse) immc . M.filterWithKey $
                    \k _ -> "/allJetFlavs/" `T.isInfixOf` k
    let imb' = flip (set (traverse.traverse.annots.at "Title")) imb $
                    Just "charm"
    iforM_ imb' $
        \ds hs -> T.writeFile (outfolder args ++ '/' : show ds ++ "_bottom.yoda")
                    (T.unlines $ hs ^.. traverse.to printYObj)


    where
        dsidOTHER = 999999


type YodaFolder = M.Map T.Text YodaObj

-- this needs to be strict or else...!!!
mergeRuns :: (Double, YodaFolder) -> (Double, YodaFolder) -> (Double, YodaFolder)
mergeRuns (sumwgt, hs) (sumwgt', hs') = ((,) $! sumwgt+sumwgt') $! M.unionWith mergeYO hs hs'

decodeFile :: String -> String -> IO (IM.IntMap (Double, YodaFolder))
decodeFile rxp f = do
    putStrLn ("decoding file " ++ f) >> hFlush stdout
    eim <- decodeLazy . decompress <$> BS.readFile f ::
                IO (Either String (IM.IntMap (Double, ZipList YodaObj)))

    case eim of
         Left _ -> error $ "failed to decode file " ++ f

         -- immediately throw out samples we don't need
         Right im -> return . flip IM.mapMaybeWithKey im $
                        \ds hs -> if processTitle ds == "other"
                                    then Nothing
                                    else Just $! over _2' (toMap . filt) hs

    where
        toMap :: ZipList YodaObj -> YodaFolder
        toMap hs = M.fromList . getZipList $ fmap (\h -> let n = view path h in (n, h)) hs

        -- TODO
        -- lens filter...
        filt :: ZipList YodaObj -> ZipList YodaObj
        filt (ZipList yhs) = ZipList . filter (matchTest (makeRegex rxp :: Regex) . T.unpack . view path) $ yhs
