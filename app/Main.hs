module Main where

import Motor.Tipos
import Motor.Mundo (leerMundo)
import Motor.Persistencia (cargarMundo, guardarPartida, cargarPartida)
import Motor.Comandos (leerComando)
import Motor.Nucleo (estadoInicial, ejecutar)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitWith, ExitCode(..))
import System.IO (hFlush, stdout, isEOF)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-validar", path] -> validarMundo path
    [path] -> jugar path
    _ -> do
      putStrLn "Uso: aventura MUNDO  |  aventura -validar MUNDO"
      exitWith (ExitFailure 1)

validarMundo :: FilePath -> IO ()
validarMundo path = do
  resultado <- cargarMundo path
  case resultado of
    Left errores -> do
      mapM_ printError errores
      exitWith (ExitFailure 1)
    Right _ -> do
      putStrLn "Mundo valido."
      exitSuccess

printError :: ErrorMundo -> IO ()
printError (ErrorMundo _ msg) = putStrLn ("Error: " ++ msg)

jugar :: FilePath -> IO ()
jugar path = do
  resultado <- cargarMundo path
  case resultado of
    Left errores -> do
      mapM_ printError errores
      exitWith (ExitFailure 1)
    Right mundo -> do
      putStrLn ("Bienvenido a: " ++ mundoTitulo mundo)
      let estadoInicial' = estadoInicial mundo
          (msgs, estadoFinal) = ejecutar Mirar estadoInicial'
      mapM_ putStrLn msgs
      bucleJuego estadoFinal

bucleJuego :: Estado -> IO ()
bucleJuego estado = do
  if estGanado estado
    then do
      putStrLn "¡Has ganado!"
      exitSuccess
    else do
      putStr "> "
      hFlush stdout
      eof <- isEOF
      if eof
        then return ()
        else do
          linea <- getLine
          case leerComando linea of
            Nothing -> do
              putStrLn "Comando no reconocido"
              bucleJuego estado
            Just cmd ->
              case cmd of
                Salir -> do
                  putStrLn "Saliendo"
                  return ()
                Guardar archivo -> do
                  guardarPartida archivo estado
                  putStrLn ("Partida guardada en: " ++ archivo)
                  bucleJuego estado
                Cargar archivo -> do
                  res <- cargarPartida archivo (estMundo estado)
                  case res of
                    Left err -> do
                      putStrLn ("Error al cargar: " ++ err)
                      bucleJuego estado
                    Right estado' -> do
                      putStrLn "Partida cargada."
                      bucleJuego estado'
                _ -> do
                  let (msgs, estado') = ejecutar cmd estado
                  mapM_ putStrLn msgs
                  bucleJuego estado'
