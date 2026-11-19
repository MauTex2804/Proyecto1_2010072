module Motor.Tipos where

import qualified Data.Map as Map

type SalaId = String
type ObjetoId = String

data Direccion = Norte | Sur | Este | Oeste | Arriba | Abajo
  deriving (Show, Eq, Ord)

data Meta
  = LlegarA SalaId
  | LlevarA ObjetoId SalaId
  deriving (Show, Eq)

data Objeto = Objeto
  { objId :: ObjetoId
  , objDesc :: String
  , objLuz :: Bool
  } deriving (Show, Eq)

data Salida = Salida
  { salDir :: Direccion
  , salDestino :: SalaId
  , salRequiere :: Maybe ObjetoId
  } deriving (Show, Eq)

data Sala = Sala
  { salaId :: SalaId
  , salaDesc :: String
  , salaSalidas :: [Salida]
  , salaObjetos :: [ObjetoId]
  , salaOscura :: Bool
  } deriving (Show, Eq)

data Mundo = Mundo
  { mundoTitulo :: String
  , mundoInicio :: SalaId
  , mundoMeta :: Meta
  , mundoSalas :: Map.Map SalaId Sala
  , mundoObjetos :: Map.Map ObjetoId Objeto
  } deriving (Show, Eq)

data Estado = Estado
  { estMundo :: Mundo
  , estSalaActual :: SalaId
  , estInventario :: [ObjetoId]
  , estGanado :: Bool
  } deriving (Show, Eq)

data Comando
  = Ir Direccion
  | Mirar
  | Tomar ObjetoId
  | Soltar ObjetoId
  | Examinar ObjetoId
  | Inventario
  | Guardar String
  | Cargar String
  | Ayuda
  | Salir
  deriving (Show, Eq)

data ErrorMundo = ErrorMundo
  { errLinea :: Maybe Int
  , errMensaje :: String
  } deriving (Show, Eq)
