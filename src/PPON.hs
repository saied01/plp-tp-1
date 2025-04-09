module PPON where

import Documento
import GHC.IO.Encoding (TextEncoding(textEncodingName))

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

pponAtomico :: PPON -> Bool
pponAtomico pp = case pp of
  TextoPP x -> True
  IntPP x -> True
  _ -> False

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple pp = case pp of
  TextoPP _ -> False
  IntPP _ -> False
  ObjetoPP pp -> foldr pponAtomicoAux b pp
  where b = False
        pponAtomicoAux (_,x) b = pponAtomico x || b

intercalar :: Doc -> [Doc] -> Doc
intercalar _ [] = texto ""
intercalar separador listaDoc = foldl1 (\acc x -> acc <+> separador <+> x) listaDoc

entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds =
  texto "{"
    <+> indentar
      2
      ( linea
          <+> intercalar (texto "," <+> linea) ds
      )
    <+> linea
    <+> texto "}"

aplanar :: Doc -> Doc
aplanar doc = intercalar (texto " ") (foldDoc [] textoRec lineaRec doc)
  where
    textoRec str listaDoc = texto str :listaDoc
    lineaRec _ listaDoc   = listaDoc

pponADoc :: PPON -> Doc
pponADoc ppon = case ppon of
  TextoPP s -> texto (['"'] ++ s ++ ['"'])
  IntPP i -> texto (show i)
  ObjetoPP o -> if pponObjetoSimple (ObjetoPP o) then entreLlavesSinSaltos (map formatoPPON o) else entreLlaves (map formatoPPON o)
    where formatoPPON (s,pp) = texto (show s ++ ": ") <+> pponADoc pp

entreLlavesSinSaltos :: [Doc] -> Doc
entreLlavesSinSaltos ds = texto "{ " <+> intercalar (texto ", ") ds <+> texto " }"