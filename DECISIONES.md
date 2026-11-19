Decisiones de Diseno (ADR)

D1. Representacion de las salas y objetos
Contexto: Necesitabamos una forma eficiente de buscar salas y objetos por su identificador.
Opciones: Listas asociativas o Data.Map.
Decision: Usamos Data.Map porque las busquedas son mas rapidas y el codigo queda mas limpio.
Consecuencias: Ganamos eficiencia, pero perdemos la facilidad de mantener el orden de insercion.

D2. Representacion del Estado
Contexto: El estado del jugador necesita saber en que sala esta, que lleva y si gano. Tambien necesita acceder al mundo para validar movimientos.
Opciones: Pasar el mundo como argumento a ejecutar o guardarlo dentro del Estado.
Decision: Lo guardamos dentro del Estado para respetar la firma ejecutar :: Comando -> Estado -> ([String], Estado) que pide el profesor.
Consecuencias: El estado es mas pesado (contiene todo el mundo), pero la firma se mantiene limpia.

D3. Manejo de errores en la lectura del mundo
Contexto: El archivo del mundo puede tener errores de sintaxis y semanticos.
Opciones: Usar Maybe o Either [ErrorMundo].
Decision: Usamos Either [ErrorMundo] para poder acumular todos los errores y reportarlos juntos.
Consecuencias: El codigo es un poco mas verboso, pero el usuario recibe todos los errores de una vez.

D4. Combinadores de reconocimiento
Contexto: El profesor prohibe usar librerias como Parsec y pide construir nuestros propios combinadores.
Opciones: Usar ReadS directamente o envolverlo en un newtype.
Decision: Usamos un newtype llamado Reconocedor para poder definir instancias de Functor, Applicative y Monad.
Consecuencias: El codigo es mas legible gracias a la notacion do, pero requiere un poco mas de configuracion inicial.

D5. Formato de las partidas guardadas
Contexto: El jugador puede guardar y cargar partidas.
Opciones: Guardar solo los cambios o usar Show/Read del estado completo.
Decision: Usamos Show/Read del estado completo porque es mas simple y robusto.
Consecuencias: El archivo de guardado es mas grande, pero la implementacion es trivial.

D6. Variante 0 (Oscuridad)
Contexto: Debemos implementar una variante que anada salas oscuras y objetos de luz.
Opciones: Ignorar la oscuridad o implementarla de forma que afecte al juego.
Decision: Implementamos la oscuridad de forma que, si el jugador no tiene un objeto de luz, no puede ver los objetos ni las salidas de la sala.
Consecuencias: El juego es mas desafiante y cumple con el requisito de la variante.
