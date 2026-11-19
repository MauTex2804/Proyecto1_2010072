module Motor.Mundo where

import Motor.Tipos
import Motor.Reconocedor
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, nub, group, sort)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Linea = (Int, String)

limpiarLinea :: String -> String
limpiarLinea = dropWhile isSpace . reverse . dropWhile isSpace . reverse

esComentario :: String -> Bool
esComentario ('#':_) = True
esComentario _       = False

esSeparador :: String -> Bool
esSeparador "---" = True
esSeparador _     = False

lineasUtiles :: String -> [Linea]
lineasUtiles texto = 
  [ (n, l) 
  | (n, l) <- zip [1..] (lines texto)
  , let l' = limpiarLinea l
  , not (null l')
  , not (esComentario l')
  ]

elevarError :: Either ErrorMundo a -> Either [ErrorMundo] a
elevarError (Left err)  = Left [err]
elevarError (Right val) = Right val

parsearLinea :: Linea -> Either ErrorMundo (String, String)
parsearLinea (n, l) =
  case break (== ':') l of
    (clave, ':':resto) ->
      let clave' = limpiarLinea clave
          valor  = limpiarLinea resto
      in if all (\c -> isAlphaNum c || c == '_') clave' && not (null clave')
         then Right (clave', valor)
         else Left (ErrorMundo (Just n) "clave desconocida o mal formada")
    _ -> Left (ErrorMundo (Just n) "linea sin la forma CLAVE: valor")

agruparBloques :: [Linea] -> [[Linea]]
agruparBloques [] = []
agruparBloques (l:ls) =
  if esSeparador (snd l)
  then agruparBloques ls
  else let (bloque, resto) = break (esSeparador . snd) ls
       in (l : bloque) : agruparBloques (drop 1 resto)

leerDireccion :: String -> Maybe Direccion
leerDireccion "norte"  = Just Norte
leerDireccion "sur"    = Just Sur
leerDireccion "este"   = Just Este
leerDireccion "oeste"  = Just Oeste
leerDireccion "arriba" = Just Arriba
leerDireccion "abajo"  = Just Abajo
leerDireccion _        = Nothing

parsearSalida :: Linea -> Either ErrorMundo (Int, Salida)
parsearSalida (n, l) =
  case words l of
    ["SALIDA:", dir, "->", destino] ->
      case leerDireccion dir of
        Just d -> Right (n, Salida d destino Nothing)
        Nothing -> Left (ErrorMundo (Just n) "direccion inexistente")
    ["SALIDA:", dir, "->", destino, "requiere", obj] ->
      case leerDireccion dir of
        Just d -> Right (n, Salida d destino (Just obj))
        Nothing -> Left (ErrorMundo (Just n) "direccion inexistente")
    _ -> Left (ErrorMundo (Just n) "SALIDA mal formada")

parsearMeta :: Linea -> Either ErrorMundo Meta
parsearMeta (n, l) =
  case words l of
    ["META:", "llegar", "a", sala] -> Right (LlegarA sala)
    ["META:", "llevar", obj, "a", sala] -> Right (LlevarA obj sala)
    _ -> Left (ErrorMundo (Just n) "META mal formada")

leerMundo :: String -> Either [ErrorMundo] Mundo
leerMundo texto = do
  let bloques = agruparBloques (lineasUtiles texto)
  (cabecera, restoBloques) <- case bloques of
    (b:bs) -> Right (b, bs)
    []     -> Left [ErrorMundo Nothing "el archivo esta vacio"]

  cabeceraPares <- mapM (elevarError . parsearLinea) cabecera
  (titulo, inicio, meta) <- case (lookup "MUNDO" cabeceraPares, lookup "INICIO" cabeceraPares, lookup "META" cabeceraPares) of
    (Just t, Just i, Just m) -> 
      case parsearMeta (0, "META: " ++ m) of
        Right metaVal -> Right (t, i, metaVal)
        Left err -> Left [err]
    _ -> Left [ErrorMundo Nothing "cabecera mal formada (falta MUNDO, INICIO o META)"]

  let (bloquesObj, bloquesSala) = partitionBloques restoBloques
  objetosPares <- mapM parsearBloqueObjeto bloquesObj
  salasPares <- mapM parsearBloqueSala bloquesSala

  let objetos = Map.fromList objetosPares
      salas = Map.fromList salasPares
      errores = validarMundo inicio meta salas objetos

  if not (null errores)
    then Left errores
    else Right (Mundo titulo inicio meta salas objetos)

partitionBloques :: [[Linea]] -> ([[Linea]], [[Linea]])
partitionBloques = foldr clasificar ([], [])
  where
    clasificar b (objs, salas) =
      case b of
        ((_, primera):_) | "OBJETO" `isPrefixOf` primera -> (b:objs, salas)
        ((_, primera):_) | "SALA" `isPrefixOf` primera   -> (objs, b:salas)
        _ -> (objs, salas)

parsearBloqueObjeto :: [Linea] -> Either [ErrorMundo] (ObjetoId, Objeto)
parsearBloqueObjeto bloque = do
  pares <- mapM (elevarError . parsearLinea) bloque
  case pares of
    [("OBJETO", oid), ("DESC", desc)] -> Right (oid, Objeto oid desc False)
    _ -> Left [ErrorMundo Nothing "bloque OBJETO mal formado"]

parsearBloqueSala :: [Linea] -> Either [ErrorMundo] (SalaId, Sala)
parsearBloqueSala bloque = do
  pares <- mapM (elevarError . parsearLinea) bloque
  let esSalida (k, _) = k == "SALIDA"
      esObjeto (k, _) = k == "OBJETO"
      salidasPares = zipWith (\n (_, v) -> (n, "SALIDA: " ++ v)) [1..] (filter esSalida pares)
      objetosIds = [ v | (k, v) <- pares, esObjeto (k, v) ]
  
  salidas <- mapM (elevarError . parsearSalida) salidasPares
  case filter (\(k, _) -> k `notElem` ["SALA", "DESC", "SALIDA", "OBJETO", "OSCURA", "LUZ"]) pares of
    ((_, _):_) -> Left [ErrorMundo Nothing "clave desconocida en bloque SALA"]
    [] -> do
      case [ v | (k, v) <- pares, k == "SALA" ] of
        [sid] -> do
          case [ v | (k, v) <- pares, k == "DESC" ] of
            [desc] -> Right (sid, Sala sid desc (map snd salidas) objetosIds False)
            _ -> Left [ErrorMundo Nothing "falta DESC o esta repetido en SALA"]
        _ -> Left [ErrorMundo Nothing "falta SALA o esta repetido"]

validarMundo :: SalaId -> Meta -> Map.Map SalaId Sala -> Map.Map ObjetoId Objeto -> [ErrorMundo]
validarMundo inicio meta salas objetos = 
  let errs = concat
        [ validarV2 salas objetos
        , validarV3 salas
        , validarV4 salas objetos
        , validarV5 salas
        , validarV6 salas objetos
        , validarV7 salas
        , validarV8 inicio meta salas objetos
        , validarV9 inicio salas
        , validarV10 meta salas objetos
        ]
  in errs

validarV2 :: Map.Map SalaId Sala -> Map.Map ObjetoId Objeto -> [ErrorMundo]
validarV2 _ _ = []

validarV3 :: Map.Map SalaId Sala -> [ErrorMundo]
validarV3 salas = 
  [ ErrorMundo Nothing ("salida hacia sala inexistente: " ++ salDestino s)
  | sala <- Map.elems salas
  , s <- salaSalidas sala
  , not (Map.member (salDestino s) salas)
  ]

validarV4 :: Map.Map SalaId Sala -> Map.Map ObjetoId Objeto -> [ErrorMundo]
validarV4 salas objetos =
  [ ErrorMundo Nothing ("salida requiere objeto inexistente: " ++ obj)
  | sala <- Map.elems salas
  , s <- salaSalidas sala
  , Just obj <- [salRequiere s]
  , not (Map.member obj objetos)
  ]

validarV5 :: Map.Map SalaId Sala -> [ErrorMundo]
validarV5 salas =
  [ ErrorMundo Nothing ("sala con dos salidas en la misma direccion: " ++ salaId sala)
  | sala <- Map.elems salas
  , let dirs = map salDir (salaSalidas sala)
  , length dirs /= length (nub dirs)
  ]

validarV6 :: Map.Map SalaId Sala -> Map.Map ObjetoId Objeto -> [ErrorMundo]
validarV6 salas objetos =
  [ ErrorMundo Nothing ("sala contiene objeto no definido: " ++ obj)
  | sala <- Map.elems salas
  , obj <- salaObjetos sala
  , not (Map.member obj objetos)
  ]

validarV7 :: Map.Map SalaId Sala -> [ErrorMundo]
validarV7 salas =
  let todosLosObjetos = concatMap salaObjetos (Map.elems salas)
      repetidos = [ x | (x:_:_) <- group (sort todosLosObjetos) ]
  in [ ErrorMundo Nothing ("objeto colocado en mas de una sala: " ++ obj) | obj <- repetidos ]

validarV8 :: SalaId -> Meta -> Map.Map SalaId Sala -> Map.Map ObjetoId Objeto -> [ErrorMundo]
validarV8 inicio meta salas objetos =
  let errInicio = if Map.member inicio salas then [] else [ErrorMundo Nothing "INICIO con sala inexistente"]
      errMeta = case meta of
        LlegarA s -> if Map.member s salas then [] else [ErrorMundo Nothing "META con sala inexistente"]
        LlevarA o s -> 
          (if Map.member s salas then [] else [ErrorMundo Nothing "META con sala inexistente"]) ++
          (if Map.member o objetos then [] else [ErrorMundo Nothing "META con objeto inexistente"])
  in errInicio ++ errMeta

validarV9 :: SalaId -> Map.Map SalaId Sala -> [ErrorMundo]
validarV9 inicio salas =
  let alcanzables = alcanzablesDesde inicio salas
      todas = Map.keysSet salas
      inalcanzables = Set.toList (todas `Set.difference` alcanzables)
  in [ ErrorMundo Nothing ("sala no alcanzable: " ++ s) | s <- inalcanzables ]

alcanzablesDesde :: SalaId -> Map.Map SalaId Sala -> Set.Set SalaId
alcanzablesDesde inicio salas = go Set.empty [inicio]
  where
    go visitados [] = visitados
    go visitados (s:resto)
      | s `Set.member` visitados = go visitados resto
      | otherwise =
        case Map.lookup s salas of
          Nothing -> go (Set.insert s visitados) resto
          Just sala -> go (Set.insert s visitados) (map salDestino (salaSalidas sala) ++ resto)

validarV10 :: Meta -> Map.Map SalaId Sala -> Map.Map ObjetoId Objeto -> [ErrorMundo]
validarV10 meta salas objetos =
  let objetosEnSalas = Set.fromList (concatMap salaObjetos (Map.elems salas))
      requeridos = case meta of
        LlegarA _ -> []
        LlevarA o _ -> [o]
      faltantes = [ o | o <- requeridos, not (o `Set.member` objetosEnSalas) ]
  in [ ErrorMundo Nothing ("objeto necesario pero no colocado: " ++ o) | o <- faltantes ]
