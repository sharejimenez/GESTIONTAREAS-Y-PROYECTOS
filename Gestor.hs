module ProjectManagement where

import Data.Time (Day)
import Data.List (find)

-- Tipos algebraicos para representar el dominio
data Priority = Low | Medium | High
    deriving (Show, Eq)

data TaskStatus = Pending | Completed
    deriving (Show, Eq)

data Employee = Employee
    { employeeId :: Int
    , employeeName :: String
    }
    deriving (Show, Eq)

data Task = Task
    { taskId :: Int
    , description :: String
    , deadline :: Day
    , priority :: Priority
    , assignedTo :: Maybe Employee
    , status :: TaskStatus
    }
    deriving (Show, Eq)

data Project = Project
    { projectName :: String
    , startDate :: Day
    , endDate :: Day
    , tasks :: [Task]
    }
    deriving (Show, Eq)

type ProjectManagementSystem = [Project]

-- Crea un nuevo proyecto sin tareas
createProject :: String -> Day -> Day -> Project
createProject name start end = Project name start end []

-- Agrega un proyecto al sistema
addProject :: Project -> ProjectManagementSystem -> ProjectManagementSystem
addProject project system = project : system

-- Crea una nueva tarea
createTask :: Int -> String -> Day -> Priority -> Task
createTask id desc deadline priority = 
    Task id desc deadline priority Nothing Pending

-- Agrega una tarea a un proyecto
addTaskToProject :: Int -> Task -> Project -> Either String Project
addTaskToProject projectId task project =
    if projectId == taskId task
    then Left "El ID de la tarea no puede ser igual al ID del proyecto"
    else if any (\t -> taskId t == taskId task) (tasks project)
         then Left "Ya existe una tarea con ese ID en el proyecto"
         else Right $ project { tasks = task : tasks project }

-- Asigna un empleado a una tarea
assignEmployeeToTask :: Int -> Employee -> Project -> Either String Project
assignEmployeeToTask taskId employee project =
    case find (\t -> taskId == taskId t) (tasks project) of
        Nothing -> Left "Tarea no encontrada"
        Just task -> 
            let updatedTask = task { assignedTo = Just employee }
                updatedTasks = map (\t -> if taskId t == taskId then updatedTask else t) (tasks project)
            in Right $ project { tasks = updatedTasks }

-- Marca una tarea como completada
completeTask :: Int -> Project -> Either String Project
completeTask taskId project =
    case find (\t -> taskId == taskId t) (tasks project) of
        Nothing -> Left "Tarea no encontrada"
        Just task -> 
            let updatedTask = task { status = Completed }
                updatedTasks = map (\t -> if taskId t == taskId then updatedTask else t) (tasks project)
            in Right $ project { tasks = updatedTasks }

-- Lista todos los proyectos con sus tareas
listProjects :: ProjectManagementSystem -> [(Project, [(Task, TaskStatus)])]
listProjects system = 
    map (\p -> (p, map (\t -> (t, status t)) (tasks p))) system

-- Cuenta tareas completadas y pendientes de un proyecto
countTasksStatus :: Project -> (Int, Int)
countTasksStatus project =
    let completed = length $ filter (\t -> status t == Completed) (tasks project)
        pending = length $ filter (\t -> status t == Pending) (tasks project)
    in (completed, pending)

-- Elimina un proyecto del sistema
removeProject :: String -> ProjectManagementSystem -> ProjectManagementSystem
removeProject name = filter (\p -> projectName p /= name)

-- Elimina una tarea de un proyecto
removeTaskFromProject :: Int -> Project -> Either String Project
removeTaskFromProject taskId project =
    if not (any (\t -> taskId == taskId t) (tasks project))
    then Left "Tarea no encontrada"
    else Right $ project { tasks = filter (\t -> taskId /= taskId t) (tasks project) }

-- Encuentra un proyecto por nombre
findProject :: String -> ProjectManagementSystem -> Either String Project
findProject name system =
    case find (\p -> projectName p == name) system of
        Nothing -> Left $ "Proyecto '" ++ name ++ "' no encontrado"
        Just project -> Right project

-- Encuentra una tarea en un proyecto
findTaskInProject :: Int -> Project -> Either String Task
findTaskInProject taskId project =
    case find (\t -> taskId == taskId t) (tasks project) of
        Nothing -> Left "Tarea no encontrada en el proyecto"
        Just task -> Right task