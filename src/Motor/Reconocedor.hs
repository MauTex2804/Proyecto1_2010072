module Motor.Reconocedor where

import Data.Char (isSpace, isAlpha, isDigit)
import Data.List (stripPrefix)

newtype Reconocedor a = Reconocedor { runReconocedor :: String -> [(a, String)] }

exito :: a -> Reconocedor a
exito x = Reconocedor $ \s -> [(x, s)]

fallo :: Reconocedor a
fallo = Reconocedor $ \_ -> []

satisface :: (Char -> Bool) -> Reconocedor Char
satisface p = Reconocedor $ \s -> case s of
  (c:cs) | p c -> [(c, cs)]
  _            -> []

caracter :: Char -> Reconocedor Char
caracter c = satisface (== c)

cadena :: String -> Reconocedor String
cadena cs = Reconocedor $ \s -> case stripPrefix cs s of
  Just resto -> [(cs, resto)]
  Nothing    -> []

espacio :: Reconocedor ()
espacio = () <$ satisface isSpace

espacios :: Reconocedor ()
espacios = muchos espacio >> exito ()

muchos :: Reconocedor a -> Reconocedor [a]
muchos p = Reconocedor $ \s -> case runReconocedor p s of
  [] -> [([], s)]
  resultados -> [ (x:xs, r2) | (x, r1) <- resultados, (xs, r2) <- runReconocedor (muchos p) r1 ]

algunos :: Reconocedor a -> Reconocedor [a]
algunos p = do
  x <- p
  xs <- muchos p
  return (x:xs)

opcional :: Reconocedor a -> Reconocedor (Maybe a)
opcional p = Reconocedor $ \s -> case runReconocedor p s of
  [] -> [(Nothing, s)]
  resultados -> [ (Just x, r) | (x, r) <- resultados ]

alternativa :: Reconocedor a -> Reconocedor a -> Reconocedor a
alternativa p q = Reconocedor $ \s -> runReconocedor p s ++ runReconocedor q s

palabra :: Reconocedor String
palabra = algunos (satisface (\c -> isAlpha c || isDigit c || c == '_'))

hastaFinDeLinea :: Reconocedor String
hastaFinDeLinea = algunos (satisface (/= '\n'))

nuevaLinea :: Reconocedor Char
nuevaLinea = caracter '\n'

instance Functor Reconocedor where
  fmap f p = Reconocedor $ \s -> [ (f x, r) | (x, r) <- runReconocedor p s ]

instance Applicative Reconocedor where
  pure = exito
  (<*>) pf px = Reconocedor $ \s -> [ (f x, r2) | (f, r1) <- runReconocedor pf s, (x, r2) <- runReconocedor px r1 ]

instance Monad Reconocedor where
  (>>=) p f = Reconocedor $ \s -> [ (y, r2) | (x, r1) <- runReconocedor p s, (y, r2) <- runReconocedor (f x) r1 ]
