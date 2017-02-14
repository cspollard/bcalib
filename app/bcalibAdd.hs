
module Main where

import qualified Control.Foldl as F
import qualified List.Transformer as L

import Options.Applicative
import qualified Data.Map.Strict as M

import qualified Data.ByteString.Lazy as BS
import Data.Serialize (decodeLazy, encodeLazy)
import Codec.Compression.GZip (decompress, compress)

import Data.YODA.Obj

data InArgs =
    InArgs
        { outfile :: String
        , infiles :: [String]
        }

inArgs :: Parser InArgs
inArgs = InArgs
    <$> strOption
        ( long "outfile"
        <> short 'o'
        <> metavar "OUTFILE" )
    <*> some (strArgument (metavar "INFILES"))

main :: IO ()
main = do
    args <- execParser $ info (helper <*> inArgs) fullDesc
    let f = F.FoldM (\x s -> merge x . toMaybe <$> decodeFile s) (return Nothing) return
    out <- F.impurely L.foldM f
            (L.select (infiles args) :: L.ListT IO String)

    BS.writeFile (outfile args) (compress . encodeLazy $ out)


    where
        merge x Nothing = x
        merge Nothing y = y
        merge (Just (dsid, sumwgt, hs)) (Just (dsid', sumwgt', hs')) =
            let x = if dsid == dsid'
                        then dsid
                        else error "attempt to add histograms with different dsids!"

                y = sumwgt + sumwgt'
                z = M.unionWith mergeYO hs hs'
            in  x `seq` y `seq` z `seq` Just (x, y, z)

        toMaybe (Left _) = Nothing
        toMaybe (Right x) = x


decodeFile :: String -> IO (Either String (Maybe (Int, Double, YodaFolder)))
decodeFile f = do
    putStrLn ("decoding file " ++ f)
    decodeLazy . decompress <$> BS.readFile f
