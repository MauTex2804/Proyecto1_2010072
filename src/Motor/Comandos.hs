module Motor.Comandos where

import Motor.Tipos
import Data.Char (toLower, isSpace)
import Data.List (isPrefixOf)

leerComando :: String -> Maybe Comando
leerComando input =
  case words (map toLower input) of
    [] -> Nothing
    ("ir":dir:_)       -> fmap Ir (leerDireccion dir)
    ["norte"]          -> Just (Ir Norte)
    ["sur"]            -> Just (Ir Sur)
    ["este"]           -> Just (Ir Este)
    ["oeste"]          -> Just (Ir Oeste)
    ["arriba"]         -> Just (Ir Arriba)
    ["abajo"]          -> Just (Ir Abajo)
    ["mirar"]          -> Just Mirar
    ["mirar", _]       -> Just Mirar
    ("tomar":obj:_)    -> Just (Tomar obj)
    ("coger":obj:_)    -> Just (Tomar obj)
    ("soltar":obj:_)   -> Just (Soltar obj)
    ("examinar":obj:_) -> Just (Examinar obj)
    ["inventario"]     -> Just Inventario
    ["inv"]            -> Just Inventario
    ("guardar":file:_) -> Just (Guardar file)
    ("cargar":file:_)  -> Just (Cargar file)
    ["ayuda"]          -> Just Ayuda
    ["salir"]          -> Just Salir
    _                  -> Nothing

leerDireccion :: String -> Maybe Direccion
leerDireccion "norte"  = Just Norte
leerDireccion "sur"    = Just Sur
leerDireccion "este"   = Just Este
leerDireccion "oeste"  = Just Oeste
leerDireccion "arriba" = Just Arriba
leerDireccion "abajo"  = Just Abajo
leerDireccion _        = Nothing
