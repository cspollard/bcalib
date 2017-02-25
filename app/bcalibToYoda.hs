{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Codec.Compression.GZip    (decompress)
import qualified Control.Foldl             as F
import           Control.Lens
import qualified Data.ByteString.Lazy      as BS
import           Data.Histogram.Generic    (bmap)
import qualified Data.IntMap.Strict        as IM
import qualified Data.Map.Strict           as M
import           Data.Maybe                (fromMaybe)
import           Data.Serialize            (decodeLazy)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import qualified List.Transformer          as L
import           Options.Applicative
import           System.IO                 (hFlush, stdout)
import           Text.Regex.Base.RegexLike
import           Text.Regex.Posix.String

import           BCalib.Histograms         hiding (option, (<>))
import           BCalib.Systematics
import           Data.Atlas.CrossSections
import           Data.Atlas.ProcessInfo

data InArgs =
  InArgs
    { outfolder :: String
    , xsecfile  :: String
    , lumi      :: Double
    , regex     :: Maybe String
    , reweight  :: Bool
    , infiles   :: [String]
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
    ( strOption (long "regex" <> metavar "REGEX=\".*\"") )
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

  xsecs <-
    fromMaybe (error "failed to parse xsec file.")
      <$> (fmap.fmap.fmap) fst (readXSecFile (xsecfile args))

  let f =
        F.FoldM
          (\x s -> maybe x (\(k, h) -> IM.insertWith mergeYF k h x) <$> decodeFile xsecs (lumi args) (regex args) s)
          (return IM.empty)
          return

  im <- F.impurely L.foldM f (L.select (infiles args) :: L.ListT IO String)

  let im' = flip IM.mapWithKey im $
              \ds hs ->
                  if ds == 0
                      then hs & traverse.annots.at "LineStyle" ?~ "none"
                              & traverse.annots.at "LineColor" ?~ "Black"
                              & traverse.annots.at "DotSize" ?~ "0.1"
                              & traverse.annots.at "ErrorBars" ?~ "1"
                              & traverse.annots.at "PolyMarker" ?~ "*"
                              & traverse.annots.at "Title" ?~ "\"data\""
                      else hs & traverse.annots.at "Title" ?~ ("\"" <> processTitle ds <> "\"")

  -- the "flip"s below are required so we don't clobber the
  -- annotations of the histogram we're keeping.
  let im'' = case dsidOTHER `IM.lookup` im' of
                  Nothing -> im'
                  Just hs -> im'
                              & ix 410000 %~ flip mergeYF hs
                              & ix 410001 %~ flip mergeYF hs
                              & ix 410002 %~ flip mergeYF hs
                              & ix 410003 %~ flip mergeYF hs
                              & ix 410004 %~ flip mergeYF hs

  let im''' = if reweight args
                  then over (traverse.itraversed.indices ((&&) <$> T.isInfixOf "/light/" <*> T.isSuffixOf "mv2c10")) reweightLF im''
                  else im''

  iforM_ im''' $
      \ds hs -> T.writeFile (outfolder args ++ '/' : show ds ++ ".yoda")
                  (ifoldMap printYodaObj hs)

  let immc = sans 0 . sans dsidOTHER $ im'''

  -- light flavor
  let iml =
        flip (over traverse) immc . M.filterWithKey
          $ \k _ -> "/light/" `T.isInfixOf` k
      iml' =
        flip (over traverse) iml
          $ M.mapKeysMonotonic (T.replace "/light/" "/allJetFlavs/")
            . (traverse.annots.at "Title" ?~ "light")

  iforM_ iml'
    $ \ds hs ->
      T.writeFile
        (outfolder args ++ '/' : show ds ++ "_light.yoda")
        (ifoldMap printYodaObj hs)

  -- charm
  -- need to add light contribution for stack plots.
  let imc =
        flip (over traverse) immc . M.filterWithKey
          $ \k _ -> "/charm/" `T.isInfixOf` k
      imc' =
        flip (over traverse) imc
          $ M.mapKeysMonotonic (T.replace "/charm/" "/allJetFlavs/")
            . (traverse.annots.at "Title" ?~ "charm")
      imc'' = IM.intersectionWith mergeYF imc' iml'

  iforM_ imc''
    $ \ds hs ->
        T.writeFile
          (outfolder args ++ '/' : show ds ++ "_charm.yoda")
          (ifoldMap printYodaObj hs)

  -- bottom
  -- use allJetFlavs since we're stacking on top of light and charm.
  let imb =
        flip (over traverse) immc . M.filterWithKey
          $ \k _ -> "/bottom/" `T.isInfixOf` k
      imb' =
        flip (over traverse) imb
          $ M.mapKeysMonotonic (T.replace "/bottom/" "/allJetFlavs/")
            . (traverse.annots.at "Title" ?~ "bottom")
      imb'' = IM.intersectionWith mergeYF imb' imc''

  iforM_ imb''
    $ \ds hs ->
      T.writeFile
      (outfolder args ++ '/' : show ds ++ "_bottom.yoda")
      (ifoldMap printYodaObj hs)


dsidOTHER :: Int
dsidOTHER = 999999


decodeFile :: IM.IntMap Double -> Double -> Maybe String -> String -> IO (Maybe (Int, YodaFolder))
decodeFile xsecs lu rxp f = do
  putStrLn ("decoding file " ++ f) >> hFlush stdout
  e <- decodeLazy . decompress <$> BS.readFile f ::
    IO (Either String (Maybe (Int, Double, SystMap YodaFolder)))

  case e of
    Left _ -> error $ "failed to decode file " ++ f

    Right Nothing -> return Nothing
    Right (Just (dsid, sumwgt, hs)) -> do
      let hs' = collapseSysts . fmap filt $ hs
      return
        $ if processTitle dsid == "other"
          then Nothing
          else Just $
            if dsid == 0
              then (0, over (noted._H1DD) (scaling $ 1.0/lu) <$> hs')
              else if dsid < 410000 || dsid > 410004
                then (dsidOTHER, over (noted._H1DD) (scaling $ xsecs IM.! dsid/sumwgt) <$> hs')
                else (dsid, over (noted._H1DD) (scaling $ xsecs IM.! dsid/sumwgt) <$> hs')

  where
    filt :: YodaFolder -> YodaFolder
    filt =
      case rxp of
        Nothing -> id
        Just s -> M.filterWithKey $ \k _ -> matchTest (makeRegex s :: Regex) . T.unpack $ k

    collapseSysts :: SystMap YodaFolder -> YodaFolder
    collapseSysts = M.foldrWithKey (\k yf yf' -> yf' <> M.mapKeysMonotonic (T.cons '/' k <>) yf) M.empty


-- NB: we lose overflow info here
-- since we don't have a bin center
-- for the overflows.
reweightLF :: YodaObj -> YodaObj
reweightLF = over (noted._H1DD) (bmap f)
  where
    -- polynomial fit to fix from Fede
    f x = scaling $
      1.18874
      - 0.0386554 * x
      - 0.122412 * x^2
      + 0.423323 * x^3
      - 0.0498479 * x^4
      - 0.217662 * x^5
