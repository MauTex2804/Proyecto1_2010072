module Motor.Reconocedor where

import Data.Char (isSpace, isAlpha, isDigit, toLower)
import Data.List (stripPrefix)

type Reconocedor a = String -> [(a, String)]

exito :: a -> Reconocedor a
exito x s = [(x, s)]

fallo :: Reconocedor a
fallo _ = []

satisface :: (Char -> Bool) -> Reconocedor Char
satisface p (c:cs)
  | p c = [(c, cs)]
satisface _ _ = []

caracter :: Char -> Reconocedor Char
caracter c = satisface (== c)

cadena :: String -> Reconocedor String
cadena cs s = case stripPrefix cs s of
  Just resto -> [(cs, resto)]
  Nothing    -> []

espacio :: Reconocedor ()
espacio = () <$ satisface isSpace

espacios :: Reconocedor ()
espacios = muchos espacio >> exito ()

muchos :: Reconocedor a -> Reconocedor [a]
muchos p s = case p s of
  [] -> [([], s)]
  resultados -> [ (x:xs, r2) | (x, r1) <- resultados, (xs, r2) <- muchos p r1 ]

algunos :: Reconocedor a -> Reconocedor [a]
algunos p = do
  x <- p
  xs <- muchos p
  return (x:xs)

opcional :: Reconocedor a -> Reconocedor (Maybe a)
opcional p s = case p s of
  [] -> [(Nothing, s)]
  resultados -> [ (Just x, r) | (x, r) <- resultados ]

alternativa :: Reconocedor a -> Reconocedor a -> Reconocedor a
alternativa p q s = p s ++ q s

palabra :: Reconocedor String
palabra = algunos (satisface (\c -> isAlpha c || isDigit c || c == '_'))

hastaFinDeLinea :: Reconocedor String
hastaFinDeLinea = algunos (satisface (/= '\n'))

nuevaLinea :: Reconocedor Char
nuevaLinea = caracter '\n'

instance Functor Reconocedor where
  fmap f p s = [ (f x, r) | (x, r) <- p s ]

instance Applicative Reconocedor where
  pure = exito
  pf <*> px s = [ (f x, r2) | (f, r1) <- pf s, (x, r2) <- px r1 ]

instance Monad Reconocedor where
  p >>= f s = [ (y, r2) | (x, r1) <- p s, (y, r2) <- f x r1 ]
