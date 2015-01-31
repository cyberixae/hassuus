import System.Environment
import Control.Exception
import System.IO.Error
import System.IO.UTF8 hiding (putStrLn,)
import Prelude hiding (readFile,)
import qualified Data.Set as Set
import Data.List
import Data.List.Split
import Debug.Trace
import Data.Char (ord)


-- Merkkien luokittelu

erottimet = " ':.\r\n,?;!()\"_/"
vokaalit = "qeyuioåaöäüQEYUIOÅAÖÄÜ"
muut = "wrtpsdfghjklzxcvbnmWRTPSDFGHJKLZXCVBNM0123456789-"

erotinJoukko = Set.fromList erottimet
erotinko kirjain = Set.member kirjain erotinJoukko


-- Tuloksen selvittäminen

sanoiksi teksti =
    let alkuunSiivottu = dropWhile erotinko teksti
        (sana, loput) = break erotinko alkuunSiivottu
        lopuksiSiivottu = dropWhile erotinko loput
        in if length teksti < 1
            then []
            else sana : sanoiksi lopuksiSiivottu

pisteytä sana =
    let ketjut = splitOneOf muut sana
        ketjuPituudet = map length ketjut
        ketjuPisteet = map (\n -> n * (2 ^ n)) ketjuPituudet
        in sum ketjuPisteet

hassuimmat teksti =
    let sanat = sanoiksi teksti
        yksilölliset = nub sanat
        pisteet = map pisteytä yksilölliset
        tulokset = zip pisteet yksilölliset
        voittoPisteet = maximum pisteet
        voittajat = [sana | (pisteet, sana) <- tulokset, pisteet == voittoPisteet]
        in (voittoPisteet, voittajat)


-- Pääohjelma

main = do
    putStrLn "Hassuusselvitin, Toni Ruottu 2015\n"
    tiedosto <- selvitäTiedosto
    aineisto <- lueAineisto tiedosto
    putStrLn $ muotoileVastaus $ hassuimmat aineisto


-- Tulosteen muotoilu

muotoileVastaus (voittopisteet, voittajat) = 
    let voittajaLista = intercalate ", " $ sort voittajat
        voittajaLause = "Hassuimmat sanat olivat: " ++ voittajaLista
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

validoi teksti =
    let merkit = nub teksti
        tunnetut = erottimet ++ vokaalit ++ muut
        tuntemattomat = merkit \\ tunnetut
        in if length tuntemattomat > 0
            then merkkiVirhe tuntemattomat
            else ()

lueAineisto tiedosto = do
    aineisto <- catch (readFile tiedosto) lukuVirhe
    seq (validoi aineisto) $ return aineisto


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

ongelmaMerkinKuvaus (järjestysNumero, merkki) =
    let kuvaus = (show järjestysNumero) ++ ". tuntematon merkki:"
        esimerkki = "'" ++ [merkki] ++ "'"
        merkkiKoodi = show $ ord merkki
        rivit = kuvaus : esimerkki : merkkiKoodi : []
        in intercalate " " rivit

merkkiVirhe tuntemattomat =
    let tyyppi = "Aineistomuoto"
        kuvaus = "Tiedosto sisältää tuntemattomia merkkejä."
        ongelmaMerkit = zip [1..] tuntemattomat
        yksityiskohdat = map ongelmaMerkinKuvaus ongelmaMerkit
        in virhe tyyppi kuvaus yksityiskohdat 
