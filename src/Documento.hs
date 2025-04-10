{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Documento
  ( Doc,
    vacio,
    linea,
    texto,
    foldDoc,
    (<+>),
    indentar,
    mostrar,
    imprimir,
  )
where
--import Distribution.Compat.Lens (_1)

data Doc
  = Vacio
  | Texto String Doc
  | Linea Int Doc
  deriving (Eq, Show)

vacio :: Doc
vacio = Vacio

linea :: Doc
linea = Linea 0 Vacio

texto :: String -> Doc
texto t | '\n' `elem` t = error "El texto no debe contener saltos de línea"
texto [] = Vacio
texto t = Texto t Vacio


foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
foldDoc fv ft fl doc = case doc of
                        Vacio -> fv
                        Texto s d -> ft s (rec d)
                        Linea i d -> fl i (rec d)
              where rec = foldDoc fv ft fl

-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>

(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc d2 textoRec lineaRec d1
  where
    textoRec s x = case x of
      Texto s2 d  -> Texto (s ++ s2) d
      _           -> Texto s x
    lineaRec i d = Linea i d

--Asumo que d1 y d2 son documentos validos, es decir, cumplen con el invariante.
 --La unica forma en la que el resultado sea un documento que no es valido, es que d1 tenga como
 --ultimo elemento un texto y d2 tenga como primer elemento un texto, por lo tanto el resultado
 --tendria un texto seguido de un texto, lo cual no es valido. Para evitar eso, tengo el caso
 --"Texto s2 d  -> Texto (s ++ s2) d" (Linea 50), que se encarga de concatenar los strings
 --y dejar el resto del documento como estaba.

indentar :: Int -> Doc -> Doc
indentar i d = foldDoc Vacio textoIgual agregarInd d
  where
    agregarInd int doc = Linea (int+i) doc 
    textoIgual str doc = Texto str doc

-- El invariante de Doc dice que la cantidad de lineas luego de un salto debe ser mayor o igual a cero.
-- Asumiendo que d es un documento valido, y asumiendo tambien que dado "indentar i d" con i >= 0, entonces se garantiza que se mantiene el invariante.

mostrar :: Doc -> String
mostrar = foldDoc ("") (\s dacc -> s ++ dacc) (\i dacc -> "\n" ++ (replicate i ' ') ++ dacc)



-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def


imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)