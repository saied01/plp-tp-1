module Main (main) where

import Documento
import PPON
import Test.HUnit

main :: IO ()
main = runTestTTAndExit allTests

allTests :: Test
allTests =
  test
    [ "Ejercicio 2" ~: testsEj2,
      "Ejercicio 3" ~: testsEj3,
      "Ejercicio 4" ~: testsEj4,
      "Ejercicio 6" ~: testsEj6,
      "Ejercicio 7" ~: testsEj7,
      "Ejercicio 8" ~: testsEj8,
      "Ejercicio 9" ~: testsEj9
    ]

testsEj2 :: Test
testsEj2 =
  test
    [ vacio <+> vacio ~?= vacio,
      texto "a" <+> texto "b" ~?= texto "ab",
      (texto "a" <+> linea) <+> texto "b" ~?= texto "a" <+> (linea <+> texto "b"),

      -- Concatenaciones con vacio
      vacio <+> texto "hola" ~?= texto "hola",
      texto "hola" <+> vacio ~?= texto "hola",

      -- Otros casos
      (texto "a" <+> texto "b") <+> texto "c" ~?= texto "abc",
      texto "a" <+> (texto "b" <+> texto "c") ~?= texto "abc",
      (linea <+> vacio) ~?= linea,
      (vacio <+> linea) ~?= linea,
      (texto "X" <+> vacio <+> texto "Y") ~?= texto "XY",
      (vacio <+> linea <+> vacio <+> texto "fin") ~?= linea <+> texto "fin"
    ]

testsEj3 :: Test
testsEj3 =
  test
    [ indentar 2 vacio ~?= vacio,
      indentar 2 (texto "a") ~?= texto "a",
      indentar 2 (texto "a" <+> linea <+> texto "b") ~?= texto "a" <+> indentar 2 (linea <+> texto "b"),
      indentar 2 (linea <+> texto "a") ~?= indentar 1 (indentar 1 (linea <+> texto "a")),
      indentar 3 (texto "hello") ~?= texto "hello"
    ]

testsEj4 :: Test
testsEj4 =
  test
    [ mostrar vacio ~?= "",
      mostrar linea ~?= "\n",
      mostrar (indentar 2 (texto "a" <+> linea <+> texto "b")) ~?= "a\n  b",
      mostrar (texto "a" <+> linea <+> texto "b") ~?= "a\nb",
      mostrar (texto "abc") ~?= "abc",
      mostrar (texto "a" <+> texto "b") ~?= "ab",
      mostrar (indentar 2 (texto "abc" <+> linea <+> texto "def")) ~?= "abc\n  def",
      mostrar (indentar 2 (texto "a" <+> linea <+> texto "b" <+> linea <+> texto "c"))
        ~?= "a\n  b\n  c"
    ]

pericles, merlina, addams, familias :: PPON
pericles = ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]
merlina = ObjetoPP [("nombre", TextoPP "Merlina"), ("edad", IntPP 24)]
addams = ObjetoPP [("0", pericles), ("1", merlina)]
familias = ObjetoPP [("Addams", addams)]

testsEj6 :: Test
testsEj6 =
  test
    [ pponObjetoSimple pericles ~?= True,
      pponObjetoSimple addams ~?= False,
      pponObjetoSimple addams ~?= False,
      
      pponObjetoSimple (ObjetoPP []) ~?= False,

      pponObjetoSimple (ObjetoPP [("nombre", TextoPP "Cosa"), ("otro", pericles)]) ~?= True,
      pponObjetoSimple (ObjetoPP [("nombre", TextoPP "Cosa"), ("sub", addams)]) ~?= True,
      pponObjetoSimple (ObjetoPP [("sub", addams), ("nombre", TextoPP "Cosa")]) ~?= True
    ]

a, b, c :: Doc
a = texto "a"
b = texto "b"
c = texto "c"

testsEj7 :: Test
testsEj7 =
  test
    [ mostrar (intercalar (texto ", ") []) ~?= "",
      mostrar (intercalar (texto ", ") [a, b, c]) ~?= "a, b, c",
      mostrar (entreLlaves []) ~?= "{ }",
      mostrar (entreLlaves [a, b, c]) ~?= "{\n  a,\n  b,\n  c\n}",

      -- un solo elemento en `intercalar`
      mostrar (intercalar (texto ", ") [a]) ~?= "a",

      -- un solo elemento en `entreLlaves`
      mostrar (entreLlaves [a]) ~?= "{\n  a\n}",

      -- diferente separador en `intercalar`
      mostrar (intercalar (texto " | ") [a, b, c]) ~?= "a | b | c",

      -- dos elementos en `entreLlaves`
      mostrar (entreLlaves [b, c]) ~?= "{\n  b,\n  c\n}"
    ]

testsEj8 :: Test
testsEj8 =
  test
    [ mostrar (aplanar (a <+> linea <+> b <+> linea <+> c)) ~?= "a b c",

      -- un solo elemento (sin líneas para aplanar)
      mostrar (aplanar a) ~?= "a",

      -- una línea e indentación
      mostrar (aplanar (a <+> indentar 2 (linea <+> b))) ~?= "a b",

      -- múltiples niveles de indentación
      mostrar (aplanar (a <+> indentar 4 (linea <+> b <+> indentar 2 (linea <+> c)))) ~?= "a b c",

      -- documentos vacíos
      mostrar (aplanar (texto "" <+> linea <+> texto "")) ~?= "",

      -- varios elementos separados por líneas
      mostrar (aplanar (texto "x" <+> linea <+> texto "y" <+> linea <+> texto "z")) ~?= "x y z"
    ]

testsEj9 :: Test
testsEj9 =
  test
    [ mostrar (pponADoc pericles) ~?= "{ \"nombre\": \"Pericles\", \"edad\": 30 }",
      mostrar (pponADoc addams) ~?= "{\n  \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n  \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n}",
      mostrar (pponADoc familias) ~?= "{\n  \"Addams\": {\n    \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n    \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n  }\n}",
            -- objeto vacío
      mostrar (pponADoc (ObjetoPP [])) ~?= "{ }",

      -- claves y valores más complejos
      mostrar (pponADoc (ObjetoPP [("clave1", TextoPP "valor1"), ("clave2", IntPP 42)])) ~?= "{\n  \"clave1\": \"valor1\",\n  \"clave2\": 42\n}",

      -- PPON anidado
      mostrar (pponADoc (ObjetoPP [("anidado", ObjetoPP [("subclave", TextoPP "subvalor")])])) ~?=  "{\n  \"anidado\": { \"subclave\": \"subvalor\" }\n}"
    ]
