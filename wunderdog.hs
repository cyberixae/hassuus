import System.Environment
import Control.Exception
import System.IO.Error
import System.IO.Strict (readFile)
import Prelude hiding (readFile,)
import qualified Data.Set as Set
import Data.List
import Data.List.Split
import Debug.Trace
import Data.Char (ord)
import Text.Regex (splitRegex, mkRegex)

data Ehdokas = Ehdokas { otaSana :: String
                       , otaPisteet :: Int
                       } deriving (Show)

instance Eq Ehdokas where
    Ehdokas sanaA pisteetA == Ehdokas sanaB pisteetB =
        pisteetA == pisteetB

instance Ord Ehdokas where
    Ehdokas sanaA pisteetA `compare` Ehdokas sanaB pisteetB =
        pisteetA `compare` pisteetB


-- Tuloksen selvittäminen

erotinKuvaus = mkRegex "\\ |'|\\:|\\.|\r|\n|\\,|\\?|\\;|\\!|\\(|\\)|\\\"|\\_|\\/"

sanoiksi teksti = splitRegex erotinKuvaus teksti

ketjusta pituus =
    pituus * (2 ^ pituus)

konsonanttiKuvaus = mkRegex "q|w|r|t|p|s|d|f|g|h|j|k|l|z|x|c|v|b|n|m|Q|W|R|T|P|S|D|F|G|H|J|K|L|Z|X|C|V|B|N|M|0|1|2|3|4|5|6|7|8|9|-"
pisteytä sana =
    let vokaaliKetjut = splitRegex konsonanttiKuvaus sana
        ketjuPituudet = map length vokaaliKetjut
        ketjuPisteet = map ketjusta ketjuPituudet
        in sum ketjuPisteet

ehdokkaaksi sana =
    let pisteet = pisteytä sana
        in Ehdokas sana pisteet

päivitäJohto johto uusiEhdokas =
    case compare uusiEhdokas $ head johto of
        LT -> johto
        EQ -> uusiEhdokas : johto
        GT -> [uusiEhdokas]

parhaat ehdokkaat@(ehdokas:loput) =
    if length ehdokkaat < 1
        then []
        else foldl päivitäJohto [ehdokas] loput

hassuimmat teksti =
    let sanat = sanoiksi teksti
        ehdokkaat = map ehdokkaaksi sanat
        voittajat = parhaat ehdokkaat
        in voittajat


-- Pääohjelma

main = do
    putStrLn "Hassuusselvitin, Toni Ruottu 2015\n"
    tiedosto <- selvitäTiedosto
    aineisto <- lueAineisto tiedosto
    putStrLn $ muotoileVastaus $ hassuimmat aineisto


-- Tulosteen muotoilu

muotoileVastaus tulos = 
    let voittajaSanat = map otaSana tulos
        voittajaLista = intercalate ", " $ sort $ voittajaSanat
        voittajaLause = "Hassuimmat sanat olivat: " ++ voittajaLista
        eräsVoittaja = head tulos
        voittopisteet = otaPisteet eräsVoittaja
        pisteLause = "Nämä saivat " ++ (show voittopisteet) ++ " pistettä."
        lauseet = [voittajaLause, pisteLause]
        vastaus = intercalate "\n" lauseet
        in vastaus


-- Parametrien luku ja validointi

selvitäTiedosto = do
    komentoriviParametrit <- getArgs
    if length komentoriviParametrit > 0
        then return $ head komentoriviParametrit
        else parametriVirhe


-- Tiedoston luku ja validointi

lueAineisto tiedosto = do
    aineisto <- catch (readFile tiedosto) lukuVirhe
    return aineisto


-- Virheen raportointi

virhe tyyppi kuvaus yksityiskohdat =
    let otsikko = tyyppi ++ "virhe!"
        rivit = otsikko : kuvaus : yksityiskohdat
        in error $ intercalate "\n" rivit

parametriVirhe =
    let tyyppi = "Parametri"
        kuvaus = "Anna aineistotiedoston nimi ensimmäisenä parametrina."
        yksityiskohdat = []
        in virhe tyyppi kuvaus yksityiskohdat 

lukuVirhe :: IOError -> IO String
lukuVirhe poikkeus = 
    let tyyppi = "Aineiston luku"
        kuvaus = "Aineiston lukeminen tiedostosta epäonnistui!"
        yksityiskohdat = []
        in virhe tyyppi kuvaus yksityiskohdat 
