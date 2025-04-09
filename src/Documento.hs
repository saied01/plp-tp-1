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

-- foldDoc :: ... PENDIENTE: Ejercicio 1 ...
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
    lineaRec = Linea


indentar :: Int -> Doc -> Doc
indentar i d = foldDoc Vacio textoIgual agregarInd d
  where
    agregarInd int doc = Linea (int+i) doc 
    textoIgual str doc = Texto str doc

mostrar :: Doc -> String
mostrar = foldDoc ("") (\s dacc -> s ++ dacc) (\i dacc -> "\n" ++ (replicate i ' ') ++ dacc)

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def


imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)