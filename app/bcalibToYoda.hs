{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Control.Foldl as F
import qualified List.Transformer as L

import Text.Regex.Posix.String
import Text.Regex.Base.RegexLike

import qualified Data.ByteString.Lazy as BS
import Data.Serialize (decodeLazy)
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

import Data.Histogram (bmap)
import Data.Hist (hist)

import BCalib.Histograms hiding (option, (<>))

data InArgs =
    InArgs
        { outfolder :: String
        , xsecfile :: String
        , lumi :: Double
        , regex :: Maybe String
        , reweight :: Bool
        , infiles :: [String]
        }

inArgs :: Parser InArgs
inArgs = InArgs
    <$> strOption
        ( long "outfolder"
        <> short 'o'
        <> metavar "OUTFOLDER" )
    <*> strOption
        ( long "xsecfile"
        <> metavar "XSECFILE" )
    <*> option auto
        ( long "lumi"
        <> metavar "LUMI" )
    <*> optional
            (strOption (long "regex" <> metavar "REGEX=\".*\""))
    <*> option auto
        ( long "reweight"
        <> metavar "REWEIGHT=False"
        <> value False
        )
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
                                & traverse.noted._H1DD %~ scaling (1.0 / lumi args)

                        else hs & traverse.noted._H1DD %~ scaling ((xsecs IM.! ds) / n)
                                & traverse.annots.at "Title" ?~ ("\"" <> processTitle ds <> "\"")

    -- lump together non-ttbar processes
    let im'' = let f k = if k /= 0 && (k < 410000 || k > 410004)
                            then dsidOTHER
                            else k
               in IM.mapKeysWith mergeYF f im'

    let im''' = case dsidOTHER `IM.lookup` im'' of
                    Nothing -> im''
                    Just hs -> im''
                                & ix 410000 %~ flip mergeYF hs
                                & ix 410001 %~ flip mergeYF hs
                                & ix 410002 %~ flip mergeYF hs
                                & ix 410003 %~ flip mergeYF hs
                                & ix 410004 %~ flip mergeYF hs

    -- TODO
    -- this could be optimized a lot.
    let im'''' = if reweight args
                    then over (traverse.itraversed.indices ((&&) <$> T.isInfixOf "/light/" <*> T.isSuffixOf "mv2c10")) reweightLF im'''
                    else im'''

    iforM_ im'''' $
        \ds hs -> T.writeFile (outfolder args ++ '/' : show ds ++ ".yoda")
                    (ifoldMap printYObj hs)


    let immc = sans 0 . sans dsidOTHER $ im''''

    -- light flavor
    let iml = flip (over traverse) immc . M.filterWithKey $
                    \k _ -> "/light/" `T.isInfixOf` k
    let iml' = flip (over traverse) iml $
                    M.mapKeysMonotonic (T.replace "/light/" "/allJetFlavs/")
                    . (traverse.annots.at "Title" ?~ "light")

    iforM_ iml' $
        \ds hs -> T.writeFile (outfolder args ++ '/' : show ds ++ "_light.yoda")
                    (ifoldMap printYObj hs)

    -- charm
    -- need to add light contribution for stack plots.
    let imc = flip (over traverse) immc . M.filterWithKey $
                    \k _ -> "/charm/" `T.isInfixOf` k
    let imc' = flip (over traverse) imc $
                    M.mapKeysMonotonic (T.replace "/charm/" "/allJetFlavs/")
                    . (traverse.annots.at "Title" ?~ "charm")
    let imc'' = IM.intersectionWith mergeYF imc' iml'
    iforM_ imc'' $
        \ds hs -> T.writeFile (outfolder args ++ '/' : show ds ++ "_charm.yoda")
                    (ifoldMap printYObj hs)

    -- bottom
    -- use allJetFlavs since we're stacking on top of light and charm.
    let imb = flip (over traverse) immc . M.filterWithKey $
                    \k _ -> "/bottom/" `T.isInfixOf` k
    let imb' = flip (over traverse) imb $
                    M.mapKeysMonotonic (T.replace "/bottom/" "/allJetFlavs/")
                    . (traverse.annots.at "Title" ?~ "bottom")
    let imb'' = IM.intersectionWith mergeYF imb' imc''
    iforM_ imb'' $
        \ds hs -> T.writeFile (outfolder args ++ '/' : show ds ++ "_bottom.yoda")
                    (ifoldMap printYObj hs)


    where
        dsidOTHER = 999999


-- this needs to be strict or else...!!!
mergeRuns :: (Double, YodaFolder) -> (Double, YodaFolder) -> (Double, YodaFolder)
mergeRuns (sumwgt, hs) (sumwgt', hs') = ((,) $! sumwgt+sumwgt') $! M.unionWith mergeYO hs hs'

decodeFile :: Maybe String -> String -> IO (IM.IntMap (Double, YodaFolder))
decodeFile rxp f = do
    putStrLn ("decoding file " ++ f) >> hFlush stdout
    eim <- decodeLazy . decompress <$> BS.readFile f ::
                IO (Either String (IM.IntMap (Double, YodaFolder)))

    case eim of
         Left _ -> error $ "failed to decode file " ++ f

         -- immediately throw out samples we don't need
         Right im -> let im' = IM.mapMaybeWithKey filtProc im
                     in im' `seq` return im'

    where
        filt :: YodaFolder -> YodaFolder
        filt = case rxp of
                Nothing -> id
                Just s -> M.filterWithKey $ \k _ -> matchTest (makeRegex s :: Regex) . T.unpack $ k

        filtProc ds hs = if processTitle ds == "other"
                            then Nothing
                            else Just $! over _2' filt hs


reweightLF :: YodaObj -> YodaObj
reweightLF = over (noted._H1DD.hist) (bmap f)
    where
        -- polynomial fit to fix from Fede
        f x = scaling $
            1.18874 
            - 0.0386554 * x
            - 0.122412 * x^2
            + 0.423323 * x^3
            - 0.0498479 * x^4
            - 0.217662 * x^5
