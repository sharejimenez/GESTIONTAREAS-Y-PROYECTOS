module ProjectManagement where

import Data.List (find, delete)
import Data.Maybe (isJust, fromMaybe)
import Data.Either (isRight)
import Control.Monad (when)
import System.IO
import Text.Printf (printf)
-- módulo para la validación de fechas
import Data.Time (fromGregorian, toGregorian, Day)
import Data.Time.Format (parseTimeM, defaultTimeLocale)

-- Definición de tipos para IDs
newtype ProyectoID = ProyectoID Int deriving (Show, Eq, Ord)
newtype TareaID = TareaID Int deriving (Show, Eq, Ord)
newtype EmpleadoID = EmpleadoID Int deriving (Show, Eq, Ord)
-- Tipos de datos 

data Prioridad = Baja | Media | Alta deriving (Show, Eq)
data Proyecto = Proyecto {
    proyectoID :: ProyectoID,
    nombreProyecto :: String,
    fechaIni :: String,
    fechaFin :: String,
    tareas :: [Tarea]
} deriving (Show, Eq)

data Tarea = Tarea {
    tareaID :: TareaID,
    descripcion :: String,
    fechalimite :: String,
    prioridad :: Prioridad,
    asignarA :: Maybe EmpleadoID,
    completada :: Bool
} deriving (Show, Eq)

data Empleado = Empleado {
    empleadoID :: EmpleadoID,
    nombreEmpleado :: String
} deriving (Show, Eq)

-- Estado del generador de IDs
data GeneradorID = IDGenerado {
    siguienteP :: Int,  --siguiente ID proyecto
    siguienteT :: Int, --siguiente ID tarea
    siguienteE :: Int  --siguiente ID proyecto
} deriving (Show)

-- Generador inicial
generadorInicial :: GeneradorID
generadorInicial = IDGenerado 1 1 1

-- Funciones para generar nuevos IDs
generarIDProyecto :: GeneradorID -> (ProyectoID, GeneradorID)
generarIDProyecto (IDGenerado p t e) = (ProyectoID p, IDGenerado (p+1) t e)

generarIDTarea :: GeneradorID -> (TareaID, GeneradorID)
generarIDTarea (IDGenerado p t e) = (TareaID t, IDGenerado p (t+1) e)

generarIDEmpleado :: GeneradorID -> (EmpleadoID, GeneradorID)
generarIDEmpleado (IDGenerado p t e) = (EmpleadoID e, IDGenerado p t (e+1))

-- Funciones auxiliares para mostrar IDs
mostrarProyectoID :: ProyectoID -> String
mostrarProyectoID (ProyectoID n) = show n

mostrarTareaID :: TareaID -> String
mostrarTareaID (TareaID n) = show n

mostrarEmpleadoID :: EmpleadoID -> String
mostrarEmpleadoID (EmpleadoID n) = show n

--FUNCIONES PARA VALIDACIONES 
-- Función para validar prioridad
validarPrioridad :: String -> Either String Prioridad
validarPrioridad "Baja" = Right Baja
validarPrioridad "Media" = Right Media
validarPrioridad "Alta" = Right Alta
validarPrioridad _ = Left "Prioridad inválida (usar Baja, Media o Alta)"

-- Función para validar el formato de fecha
validarFecha :: String -> Maybe Day
validarFecha fechaStr = parseTimeM True defaultTimeLocale "%Y-%m-%d" fechaStr :: Maybe Day

-- Función para verificar que la fecha de fin es posterior a la de inicio
validarRangoFecha :: String -> String -> Either String (Day, Day)
validarRangoFecha inicioStr finStr = do
    inicio <- maybeToEither "Fecha de inicio inválida (formato: YYYY-MM-DD)" (validarFecha inicioStr)
    fin <- maybeToEither "Fecha de fin inválida (formato: YYYY-MM-DD)" (validarFecha finStr)
    if fin >= inicio
        then Right (inicio, fin)
        else Left "La fecha de fin debe ser posterior a la fecha de inicio"
  where
    maybeToEither :: String -> Maybe a -> Either String a
    maybeToEither err = maybe (Left err) Right

-- Función para mostrar Either
mostrarEither :: (a -> String) -> Either String a -> String
mostrarEither _ (Left err) = "Error: " ++ err
mostrarEither f (Right x) = f x

-- Funciones para mostrar información mejorada
mostrarMenuProyectos :: [Proyecto] -> IO ()
mostrarMenuProyectos proyectos = do
    putStrLn "\nProyectos disponibles:"
    mapM_ (\p -> printf "[%s] %s\n" (mostrarProyectoID $ proyectoID p) (nombreProyecto p)) proyectos

mostrarDetallesTareas :: [Tarea] -> [Empleado] -> IO ()
mostrarDetallesTareas tareas empleados = do
    putStrLn "\nTareas:"
    mapM_ (mostrarDetalleTarea empleados) tareas

mostrarDetalleTarea :: [Empleado] -> Tarea -> IO ()
mostrarDetalleTarea empleados tarea = do
    let asignadoA = case asignarA tarea of
                        Just eid -> maybe "Nadie" nombreEmpleado (find ((== eid) . empleadoID) empleados)
                        Nothing -> "Nadie"
    printf "- ID: %s, %s, Prioridad: %s, Fecha límite: %s, Asignada a: %s (%s), Completada: %s\n"
        (mostrarTareaID $ tareaID tarea)
        (descripcion tarea)
        (show $ prioridad tarea)
        (fechalimite tarea)
        (maybe "Nadie" mostrarEmpleadoID $ asignarA tarea)
        asignadoA
        (if completada tarea then "Sí" else "No")

mostrarMenuEmpleados :: [Empleado] -> IO ()
mostrarMenuEmpleados empleados = do
    putStrLn "\nEmpleados disponibles:"
    mapM_ (\e -> printf "[%s] %s\n" (mostrarEmpleadoID $ empleadoID e) (nombreEmpleado e)) empleados

-- Funciones principales
crearEmpleado :: String -> [Empleado] -> GeneradorID -> Either String (Empleado, [Empleado], GeneradorID)
crearEmpleado nombre empleados gen
    | null nombre = Left "El nombre del empleado no puede estar vacío"
    | otherwise = 
        let (nuevoID, gen') = generarIDEmpleado gen
            emp = Empleado nuevoID nombre
        in Right (emp, emp : empleados, gen')
--parametros para crear proyecto 
crearProyecto :: String -> String -> String -> [Proyecto] -> GeneradorID -> Either String (Proyecto, [Proyecto], GeneradorID)
crearProyecto nombre inicio fin proyectos gen = do
    -- Validar que el nombre no esté vacío
    if null nombre 
        then Left "El nombre del proyecto no puede estar vacío"
        else do
            -- Validar fechas
            (inicioDia, finDia) <- validarRangoFecha inicio fin
            
            -- Si todo está bien, crear el proyecto
            let (nuevoID, gen') = generarIDProyecto gen
                proyecto = Proyecto nuevoID nombre inicio fin []
            Right (proyecto, proyecto : proyectos, gen')

agregarTarea :: Proyecto -> String -> String -> Prioridad -> GeneradorID -> Either String (Tarea, Proyecto, GeneradorID)
agregarTarea proyecto descripcion fechaLimite prioridad gen = do
    -- Validar descripción no vacía
    if null descripcion 
        then Left "La descripción de la tarea no puede estar vacía"
        else do
            -- Validar fecha
            _ <- maybeToEither "Fecha límite inválida (formato: YYYY-MM-DD)" (validarFecha fechaLimite)
            
            -- Si todo está bien, crear la tarea
            let (nuevoID, gen') = generarIDTarea gen
                tarea = Tarea nuevoID descripcion fechaLimite prioridad Nothing False
                proyectoActualizado = proyecto { tareas = tarea : tareas proyecto }
            Right (tarea, proyectoActualizado, gen')
  where
    maybeToEither err = maybe (Left err) Right

asignarTarea :: Tarea -> EmpleadoID -> [Empleado] -> Either String Tarea
asignarTarea tarea idEmpleado empleados = 
    if any ((== idEmpleado) . empleadoID) empleados
        then Right $ tarea { asignarA = Just idEmpleado }
        else Left "Empleado no encontrado"

marcarTareaCompleta :: Tarea -> Tarea
marcarTareaCompleta tarea = tarea { completada = True }

eliminarProyecto :: ProyectoID -> [Proyecto] -> Either String [Proyecto]
eliminarProyecto idProy proyectos = 
    if any ((== idProy) . proyectoID) proyectos
        then Right $ filter ((/= idProy) . proyectoID) proyectos
        else Left "Proyecto no encontrado"

eliminarTarea :: TareaID -> Proyecto -> Either String Proyecto
eliminarTarea idTar proyecto = 
    if any ((== idTar) . tareaID) (tareas proyecto)
        then Right $ proyecto { tareas = filter ((/= idTar) . tareaID) (tareas proyecto) }
        else Left "Tarea no encontrada"

contarTareas :: Proyecto -> (Int, Int)
contarTareas proyecto = (length tareasCompletadas, length tareasPendientes)
  where
    tareasCompletadas = filter completada (tareas proyecto)
    tareasPendientes = filter (not . completada) (tareas proyecto)

-- Funciones para selección interactiva
seleccionarProyecto :: [Proyecto] -> IO (Maybe Proyecto)
seleccionarProyecto proyectos = do
    mostrarMenuProyectos proyectos
    putStr "Seleccione el ID del proyecto (0 para cancelar): "
    idProyStr <- getLine
    case leerID idProyStr of
        Nothing -> do
            putStrLn "ID inválido, por favor ingrese un número"
            seleccionarProyecto proyectos
        Just 0 -> return Nothing
        Just n -> 
            case find ((== ProyectoID n) . proyectoID) proyectos of
                Nothing -> do
                    putStrLn "Proyecto no encontrado, por favor intente nuevamente"
                    seleccionarProyecto proyectos
                Just p -> return (Just p)

seleccionarTarea :: Proyecto -> IO (Maybe Tarea)
seleccionarTarea proyecto = do
    putStrLn $ "\nTareas del proyecto " ++ nombreProyecto proyecto ++ ":"
    mostrarDetallesTareas (tareas proyecto) []
    putStr "Seleccione el ID de la tarea (0 para cancelar): "
    idTarStr <- getLine
    case leerID idTarStr of
        Nothing -> do
            putStrLn "ID inválido, por favor ingrese un número"
            seleccionarTarea proyecto
        Just 0 -> return Nothing
        Just n -> 
            case find ((== TareaID n) . tareaID) (tareas proyecto) of
                Nothing -> do
                    putStrLn "Tarea no encontrada, por favor intente nuevamente"
                    seleccionarTarea proyecto
                Just t -> return (Just t)

seleccionarEmpleado :: [Empleado] -> IO (Maybe EmpleadoID)
seleccionarEmpleado empleados = do
    mostrarMenuEmpleados empleados
    putStr "Seleccione el ID del empleado (0 para cancelar): "
    idEmpStr <- getLine
    case leerID idEmpStr of
        Nothing -> do
            putStrLn "ID inválido, por favor ingrese un número"
            seleccionarEmpleado empleados
        Just 0 -> return Nothing
        Just n -> 
            if any ((== EmpleadoID n) . empleadoID) empleados
                then return (Just (EmpleadoID n))
                else do
                    putStrLn "Empleado no encontrado, por favor intente nuevamente"
                    seleccionarEmpleado empleados

-- Función para leer IDs de manera segura
leerID :: String -> Maybe Int
leerID s = case reads s of
             [(n, "")] -> Just n
             _ -> Nothing

-- Función auxiliar para leer una fecha válida
leerFechaValidada :: String -> IO String
leerFechaValidada prompt = do
    putStr prompt
    fecha <- getLine
    case validarFecha fecha of
        Just _ -> return fecha
        Nothing -> do
            putStrLn "Fecha inválida (formato: YYYY-MM-DD). Por favor intente nuevamente."
            leerFechaValidada prompt

-- Función auxiliar para leer una fecha de fin válida (posterior a la de inicio)
leerFechaFinValida :: String -> IO String
leerFechaFinValida fechaInicio = do
    putStr "Fecha de fin (YYYY-MM-DD): "
    fechaFin <- getLine
    case validarRangoFecha fechaInicio fechaFin of
        Right _ -> return fechaFin
        Left err -> do
            putStrLn err
            leerFechaFinValida fechaInicio

-- Función principal
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Sistema de Gestión de Proyectos"
    putStrLn "--------------------------------"
    bucle [] [] generadorInicial

bucle :: [Proyecto] -> [Empleado] -> GeneradorID -> IO ()
bucle proyectos empleados gen = do
    putStrLn "\nMenú Principal:"
    putStrLn "1. Crear nuevo proyecto"
    putStrLn "2. Añadir tarea a proyecto"
    putStrLn "3. Asignar tarea a empleado"
    putStrLn "4. Registrar empleado"
    putStrLn "5. Eliminar tarea de proyecto"
    putStrLn "6. Eliminar proyecto"
    putStrLn "7. Marcar tarea como completada"
    putStrLn "8. Listar proyectos y tareas"
    putStrLn "9. Mostrar estadísticas de proyecto"
    putStrLn "0. Salir "
    putStr "Seleccione una opción: "
    opcion <- getLine
    
    case opcion of
        "1" -> menuCrearProyecto proyectos empleados gen
        "2" -> menuAgregarTarea proyectos empleados gen
        "3" -> menuAsignarTarea proyectos empleados gen     
        "4" -> menuCrearEmpleado proyectos empleados gen
        "5" -> menuEliminarTarea proyectos empleados gen   
        "6" -> menuEliminarProyecto proyectos empleados gen         
        "7" -> menuTareaCompletada proyectos empleados gen       
        "8" -> menuListarProyectos proyectos empleados gen                     
        "9" -> menuEstadisticas proyectos empleados gen
        "0" -> putStrLn "Saliendo del sistema..." >> return ()
        _   -> do
            putStrLn "Opción no válida, por favor intente nuevamente"
            bucle proyectos empleados gen

-- Menús específicos con manejo mejorado de errores
menuCrearProyecto :: [Proyecto] -> [Empleado] -> GeneradorID -> IO ()
menuCrearProyecto proyectos empleados gen = do
    putStrLn "-----------Creando proyecto :D ----------"
    putStr "Nombre del proyecto: "
    nombre <- getLine
    when (null nombre) $ do
        putStrLn "El nombre del proyecto no puede estar vacío."
        bucle proyectos empleados gen
    
    inicio <- leerFechaValidada "Fecha de inicio (YYYY-MM-DD): "
    fin <- leerFechaFinValida inicio
    
    let (nuevoID, gen') = generarIDProyecto gen
    let proyecto = Proyecto nuevoID nombre inicio fin []
    putStrLn $ "Proyecto creado con ID: " ++ mostrarProyectoID (proyectoID proyecto)
    bucle (proyecto : proyectos) empleados gen'

menuListarProyectos :: [Proyecto] -> [Empleado] -> GeneradorID -> IO ()
menuListarProyectos proyectos empleados gen = do
    putStrLn "----------Lista de proyectos--------"
    if null proyectos
        then do
            putStrLn "No hay proyectos registrados."
            bucle proyectos empleados gen
        else do
            mProyecto <- seleccionarProyecto proyectos
            case mProyecto of
                Nothing -> bucle proyectos empleados gen
                Just proyecto -> do
                    putStrLn $ "\nDetalles del proyecto " ++ nombreProyecto proyecto ++ ":"
                    putStrLn $ "Fecha inicio: " ++ fechaIni proyecto
                    putStrLn $ "Fecha fin: " ++ fechaFin proyecto
                    mostrarDetallesTareas (tareas proyecto) empleados
                    bucle proyectos empleados gen

menuEliminarProyecto :: [Proyecto] -> [Empleado] -> GeneradorID -> IO ()
menuEliminarProyecto proyectos empleados gen = do
    putStrLn "-----------Eliminando proyecto :() ----------"
    if null proyectos
        then do
            putStrLn "No hay proyectos para eliminar."
            bucle proyectos empleados gen
        else do
            mProyecto <- seleccionarProyecto proyectos
            case mProyecto of
                Nothing -> bucle proyectos empleados gen
                Just proyecto -> do
                    case eliminarProyecto (proyectoID proyecto) proyectos of
                        Right nuevosProyectos -> do
                            putStrLn "Proyecto eliminado correctamente"
                            bucle nuevosProyectos empleados gen
                        Left err -> do
                            putStrLn $ "Error: " ++ err
                            bucle proyectos empleados gen

menuAgregarTarea :: [Proyecto] -> [Empleado] -> GeneradorID -> IO ()
menuAgregarTarea proyectos empleados gen = do
    putStrLn "-----------Creando nueva tarea :D ----------"
    if null proyectos
        then do
            putStrLn "No hay proyectos para agregar tareas."
            bucle proyectos empleados gen
        else do
            mProyecto <- seleccionarProyecto proyectos
            case mProyecto of
                Nothing -> bucle proyectos empleados gen
                Just proyecto -> do
                    putStr "Descripción: "
                    descripcion <- getLine
                    when (null descripcion) $ do
                        putStrLn "La descripción no puede estar vacía."
                        menuAgregarTarea proyectos empleados gen
                    
                    fechaLimite <- leerFechaValidada "Fecha límite (YYYY-MM-DD): "
                    
                    -- PARTE MODIFICADA PARA MANEJAR PRIORIDAD
                    let pedirPrioridad = do
                            putStr "Prioridad (Alta/Media/Baja): "
                            prioridadStr <- getLine
                            case validarPrioridad prioridadStr of
                                Left err -> do
                                    putStrLn err
                                    pedirPrioridad  -- Vuelve a pedir la prioridad si hay error
                                Right prioridad -> return prioridad
                    
                    prioridad <- pedirPrioridad  -- Llamada a la nueva función
                    
                    case agregarTarea proyecto descripcion fechaLimite prioridad gen of
                        Left err -> do
                            putStrLn $ "Error: " ++ err
                            menuAgregarTarea proyectos empleados gen
                        Right (tarea, proyectoActualizado, gen') -> do
                            let nuevosProyectos = proyectoActualizado : delete proyecto proyectos
                            putStrLn $ "Tarea añadida con ID: " ++ mostrarTareaID (tareaID tarea)
                            bucle nuevosProyectos empleados gen'
menuEliminarTarea :: [Proyecto] -> [Empleado] -> GeneradorID -> IO ()
menuEliminarTarea proyectos empleados gen = do
    putStrLn "-----------Opción eliminar tarea :) ----------"
    if null proyectos
        then do
            putStrLn "No hay proyectos para eliminar tareas."
            bucle proyectos empleados gen
        else do
            mProyecto <- seleccionarProyecto proyectos
            case mProyecto of
                Nothing -> bucle proyectos empleados gen
                Just proyecto -> do
                    if null (tareas proyecto)
                        then do
                            putStrLn "Este proyecto no tiene tareas."
                            bucle proyectos empleados gen
                        else do
                            mTarea <- seleccionarTarea proyecto
                            case mTarea of
                                Nothing -> bucle proyectos empleados gen
                                Just tarea -> do
                                    case eliminarTarea (tareaID tarea) proyecto of
                                        Right proyectoActualizado -> do
                                            let nuevosProyectos = proyectoActualizado : delete proyecto proyectos
                                            putStrLn "Tarea eliminada correctamente"
                                            bucle nuevosProyectos empleados gen
                                        Left err -> do
                                            putStrLn $ "Error: " ++ err
                                            bucle proyectos empleados gen

menuCrearEmpleado proyectos empleados gen = do
    putStrLn "-----------Creando empleado :D ----------"
    putStr "Nombre del empleado: "
    nombre <- getLine
    case crearEmpleado nombre empleados gen of
        Left err -> do
            putStrLn $ "Error: " ++ err
            menuCrearEmpleado proyectos empleados gen  -- Vuelve a pedir el nombre
        Right (nuevoEmpleado, nuevosEmpleados, gen') -> do
            putStrLn $ "Empleado registrado con ID: " ++ mostrarEmpleadoID (empleadoID nuevoEmpleado)
            bucle proyectos nuevosEmpleados gen'

menuAsignarTarea :: [Proyecto] -> [Empleado] -> GeneradorID -> IO ()
menuAsignarTarea proyectos empleados gen = do
    putStrLn "-----------Asignando tarea a un proyecto :D ----------"
    if null proyectos
        then putStrLn "No hay proyectos para asignar tareas."
        else if null empleados
            then putStrLn "No hay empleados registrados."
            else do
                mProyecto <- seleccionarProyecto proyectos
                case mProyecto of
                    Nothing -> return ()
                    Just proyecto -> do
                        mTarea <- seleccionarTarea proyecto
                        case mTarea of
                            Nothing -> return ()
                            Just tarea -> do
                                mEmpleado <- seleccionarEmpleado empleados
                                case mEmpleado of
                                    Nothing -> return ()
                                    Just idEmpleado -> do
                                        case asignarTarea tarea idEmpleado empleados of
                                            Left err -> do
                                                putStrLn $ "Error: " ++ err
                                                bucle proyectos empleados gen
                                            Right tareaActualizada -> do
                                                let proyectoActualizado = proyecto { tareas = tareaActualizada : filter ((/= tareaID tarea) . tareaID) (tareas proyecto) }
                                                let nuevosProyectos = proyectoActualizado : filter ((/= proyectoID proyecto) . proyectoID) proyectos
                                                putStrLn "Tarea asignada correctamente"
                                                bucle nuevosProyectos empleados gen

menuTareaCompletada :: [Proyecto] -> [Empleado] -> GeneradorID -> IO ()
menuTareaCompletada proyectos empleados gen = do
    putStrLn "-----------Registrar tarea completada :D ----------"
    if null proyectos
        then do
            putStrLn "No hay proyectos para completar tareas."
            bucle proyectos empleados gen
        else do
            mProyecto <- seleccionarProyecto proyectos
            case mProyecto of
                Nothing -> bucle proyectos empleados gen
                Just proyecto -> do
                    if null (tareas proyecto)
                        then do
                            putStrLn "Este proyecto no tiene tareas."
                            bucle proyectos empleados gen
                        else do
                            mTarea <- seleccionarTarea proyecto
                            case mTarea of
                                Nothing -> bucle proyectos empleados gen
                                Just tarea -> do
                                    let tareaActualizada = marcarTareaCompleta tarea
                                    let proyectoActualizado = proyecto { tareas = tareaActualizada : filter ((/= tareaID tarea) . tareaID) (tareas proyecto) }
                                    let nuevosProyectos = proyectoActualizado : filter ((/= proyectoID proyecto) . proyectoID) proyectos
                                    putStrLn "Tarea marcada como completada !"
                                    bucle nuevosProyectos empleados gen

menuEstadisticas :: [Proyecto] -> [Empleado] -> GeneradorID -> IO ()
menuEstadisticas proyectos empleados gen = do
    putStrLn "-----------Estadísticas tareas y proyectos :D ----------"
    if null proyectos
        then do
            putStrLn "No hay proyectos para mostrar estadísticas."
            bucle proyectos empleados gen
        else do
            mProyecto <- seleccionarProyecto proyectos
            case mProyecto of
                Nothing -> bucle proyectos empleados gen
                Just proyecto -> do
                    let (completadas, pendientes) = contarTareas proyecto
                    putStrLn $ "\nEstadísticas del proyecto " ++ nombreProyecto proyecto ++ ":"
                    putStrLn $ "Tareas completadas: " ++ show completadas
                    putStrLn $ "Tareas pendientes: " ++ show pendientes
                    bucle proyectos empleados gen