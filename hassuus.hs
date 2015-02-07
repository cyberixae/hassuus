
-- Hassuusselvitin, Toni Ruottu 2015
--
-- Vastaus Wunderdog Oy:n "hassuimmat sanat" koodauspähkinään.
-- Tehtävänanto: http://wunderdog.fi/koodaus-hassuimmat-sanat/
--
-- Kääntöohje:
-- ghc -O2 -with-rtsopts="-K100m" hassuus.hs

import System.Environment (getArgs)
import Data.List (intercalate, sort)
import Text.Regex (splitRegex, mkRegex)


-- Merkkien luokittelu

erotinKuvaus = mkRegex "\\ |'|\\:|\\.|\r|\n|\\,|\\?|\\;|\\!|\\(|\\)|\\\"|\\_|\\/"
konsonanttiKuvaus = mkRegex "q|w|r|t|p|s|d|f|g|h|j|k|l|z|x|c|v|b|n|m|Q|W|R|T|P|S|D|F|G|H|J|K|L|Z|X|C|V|B|N|M|0|1|2|3|4|5|6|7|8|9|-"


-- Tietotyyppien määrittelyt

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

sanoiksi teksti = splitRegex erotinKuvaus teksti

ketjusta pituus =
    pituus * (2 ^ pituus)

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


-- Pääohjelma

main = do
    putStrLn "Hassuusselvitin, Toni Ruottu 2015\n"
    komentoriviParametrit <- getArgs
    aineisto <- readFile (head komentoriviParametrit)
    putStrLn $ muotoileVastaus $ hassuimmat aineisto
