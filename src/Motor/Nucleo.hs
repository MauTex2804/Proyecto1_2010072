module Motor.Nucleo where

import Motor.Tipos
import qualified Data.Map as Map
import Data.List (intercalate, nub)

estadoInicial :: Mundo -> Estado
estadoInicial mundo = Estado
  { estMundo = mundo
  , estSalaActual = mundoInicio mundo
  , estInventario = []
  , estGanado = False
  }

ejecutar :: Comando -> Estado -> ([String], Estado)
ejecutar cmd estado =
  case cmd of
    Ir dir -> mover dir estado
    Mirar -> (describirSalaCompleta estado, estado)
    Tomar obj -> tomar obj estado
    Soltar obj -> soltar obj estado
    Examinar obj -> examinar obj estado
    Inventario -> (["Llevas: " ++ intercalate ", " (estInventario estado)], estado)
    Guardar _ -> (["Comando guardar pendiente (se hara en persistencia)."], estado)
    Cargar _ -> (["Comando cargar pendiente (se hara en persistencia)."], estado)
    Ayuda -> (["Comandos: ir <direccion>, mirar, tomar <objeto>, soltar <objeto>, examinar <objeto>, inventario, guardar <archivo>, cargar <archivo>, ayuda, salir"], estado)
    Salir -> (["Saliendo del juego..."], estado)

mover :: Direccion -> Estado -> ([String], Estado)
mover dir estado =
  case Map.lookup (estSalaActual estado) (mundoSalas (estMundo estado)) of
    Nothing -> (["Error interno: la sala actual no existe."], estado)
    Just sala ->
      case buscarSalida dir (salaSalidas sala) of
        Nothing -> (["No hay salida hacia el " ++ mostrarDireccion dir ++ "."], estado)
        Just salida ->
          case salRequiere salida of
            Nothing -> entrarSala salida estado
            Just obj ->
              if obj `elem` estInventario estado
                then entrarSala salida estado
                else (["La salida esta bloqueada. Necesitas: " ++ obj ++ "."], estado)

entrarSala :: Salida -> Estado -> ([String], Estado)
entrarSala salida estado =
  let nuevoEstado = estado { estSalaActual = salDestino salida }
      msg = "Te mueves hacia el " ++ mostrarDireccion (salDir salida) ++ "."
      mensajes = msg : describirSalaCompleta nuevoEstado
      estadoFinal = verificarMeta nuevoEstado
  in (mensajes, estadoFinal)

buscarSalida :: Direccion -> [Salida] -> Maybe Salida
buscarSalida _ [] = Nothing
buscarSalida d (s:ss)
  | salDir s == d = Just s
  | otherwise     = buscarSalida d ss

tomar :: ObjetoId -> Estado -> ([String], Estado)
tomar obj estado =
  case Map.lookup (estSalaActual estado) (mundoSalas (estMundo estado)) of
    Nothing -> (["Error interno."], estado)
    Just sala ->
      if obj `elem` salaObjetos sala
        then
          let sala' = sala { salaObjetos = filter (/= obj) (salaObjetos sala) }
              mundo' = (estMundo estado) { mundoSalas = Map.insert (estSalaActual estado) sala' (mundoSalas (estMundo estado)) }
              estado' = estado { estMundo = mundo', estInventario = obj : estInventario estado }
          in (["Tomaste: " ++ obj ++ "."], verificarMeta estado')
        else (["No hay " ++ obj ++ " en esta sala."], estado)

soltar :: ObjetoId -> Estado -> ([String], Estado)
soltar obj estado =
  if obj `elem` estInventario estado
    then
      case Map.lookup (estSalaActual estado) (mundoSalas (estMundo estado)) of
        Nothing -> (["Error interno."], estado)
        Just sala ->
          let sala' = sala { salaObjetos = obj : salaObjetos sala }
              mundo' = (estMundo estado) { mundoSalas = Map.insert (estSalaActual estado) sala' (mundoSalas (estMundo estado)) }
              estado' = estado { estMundo = mundo', estInventario = filter (/= obj) (estInventario estado) }
          in (["Soltaste: " ++ obj ++ "."], estado')
    else (["No llevas " ++ obj ++ " en el inventario."], estado)

examinar :: ObjetoId -> Estado -> ([String], Estado)
examinar obj estado =
  case Map.lookup obj (mundoObjetos (estMundo estado)) of
    Just objDef -> (["Examinas " ++ obj ++ ": " ++ objDesc objDef], estado)
    Nothing -> (["No existe el objeto " ++ obj ++ "."], estado)

describirSalaCompleta :: Estado -> [String]
describirSalaCompleta estado =
  case Map.lookup (estSalaActual estado) (mundoSalas (estMundo estado)) of
    Nothing -> ["Error interno: sala no encontrada."]
    Just sala ->
      [ salaDesc sala
      , "Objetos: " ++ (if null (salaObjetos sala) then "ninguno" else intercalate ", " (salaObjetos sala))
      , "Salidas: " ++ (if null (salaSalidas sala) then "ninguna" else intercalate ", " (map (mostrarDireccion . salDir) (salaSalidas sala)))
      ]

verificarMeta :: Estado -> Estado
verificarMeta estado =
  case mundoMeta (estMundo estado) of
    LlegarA sala ->
      if estSalaActual estado == sala
        then estado { estGanado = True }
        else estado
    LlevarA obj sala ->
      if estSalaActual estado == sala && obj `elem` estInventario estado
        then estado { estGanado = True }
        else estado

mostrarDireccion :: Direccion -> String
mostrarDireccion Norte  = "norte"
mostrarDireccion Sur    = "sur"
mostrarDireccion Este   = "este"
mostrarDireccion Oeste  = "oeste"
mostrarDireccion Arriba = "arriba"
mostrarDireccion Abajo  = "abajo"
