module Motor.Persistencia where

import Motor.Tipos
import Motor.Mundo (leerMundo)
import System.Directory (doesFileExist)
import Text.Read (readMaybe)
import qualified Data.Map as Map

cargarMundo :: FilePath -> IO (Either [ErrorMundo] Mundo)
cargarMundo path = do
  existe <- doesFileExist path
  if not existe
    then return $ Left [ErrorMundo Nothing ("No existe el archivo: " ++ path)]
    else do
      contenido <- readFile path
      return $ leerMundo contenido

guardarPartida :: FilePath -> Estado -> IO ()
guardarPartida path estado = do
  let contenido = show estado
  writeFile path contenido

cargarPartida :: FilePath -> Mundo -> IO (Either String Estado)
cargarPartida path mundo = do
  existe <- doesFileExist path
  if not existe
    then return $ Left ("No existe el archivo de partida: " ++ path)
    else do
      contenido <- readFile path
      case readMaybe contenido of
        Nothing -> return $ Left "El archivo de partida esta corrupto."
        Just estado ->
          if Map.member (estSalaActual estado) (mundoSalas mundo)
            then return $ Right estado
            else return $ Left "La partida pertenece a otro mundo."
