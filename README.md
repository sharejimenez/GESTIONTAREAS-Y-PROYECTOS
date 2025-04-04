# Sistema de Gestión de Proyectos en Haskell

Este proyecto implementa un sistema de gestión de proyectos que permite crear proyectos, asignar tareas, registrar empleados y realizar seguimiento del progreso de las tareas.

## Estructura del Proyecto

El sistema está organizado en un único módulo Haskell (`ProjectManagement`) que contiene:

1. **Tipos de datos fundamentales**:
   - `Proyecto`, `Tarea` y `Empleado` con sus respectivos identificadores
   - `Prioridad` (Baja, Media, Alta)
   - `GeneradorID` para manejar la creación de IDs únicos

2. **Funciones principales**:
   - Creación y eliminación de proyectos y tareas
   - Asignación de tareas a empleados
   - Marcado de tareas como completadas
   - Generación de estadísticas

3. **Interfaz de usuario**:
   - Menú interactivo por consola
   - Validación de entradas
   - Visualización de datos

## Requisitos para Ejecutar el Código

- [GHC](https://www.haskell.org/ghc/) instalado
- Paquetes Haskell adicionales:
  - `time` (para manejo de fechas)
  - `text-printf` (para formateo de texto)

## Cómo Ejecutar el Sistema

1. **Compilación**:
  
   - En nuestro directorio debemos de acceder a la consola de Haskel ingresando ghci
   ```bash
    ghci
   ```
   - Posteriomente debemos gaurdar los cmabios del archivo con:

   ```bash
    :l Gestor.hs
   ```
2. **Ejecución**:
   - Para poder interactuar con cada una de las funciondes debemos acceder a la funion main simplemente escribiendo la palbra main en la consola
   

3. **Uso del sistema**:
   - Al iniciar, se mostrará un menú con las opciones disponibles
   - Seleccione una opción ingresando el número correspondiente
   - Siga las instrucciones para cada operación

## Funcionalidades Principales

### 1. Gestión de Proyectos
- **Crear proyecto**: Solicita nombre, fechas de inicio y fin
- **Eliminar proyecto**: Elimina un proyecto y todas sus tareas
- **Listar proyectos**: Muestra todos los proyectos con sus detalles

### 2. Gestión de Tareas
- **Añadir tarea**: Agrega una tarea a un proyecto existente
- **Eliminar tarea**: Remueve una tarea de un proyecto
- **Marcar como completada**: Registra una tarea como finalizada
- **Asignar a empleado**: Asocia una tarea con un empleado registrado

### 3. Gestión de Empleados
- **Registrar empleado**: Añade un nuevo empleado al sistema

### 4. Estadísticas
- **Mostrar estadísticas**: Proporciona conteo de tareas completadas y pendientes por proyecto

## Validaciones Implementadas

El sistema incluye validaciones para:
- Formatos de fecha (YYYY-MM-DD)
- Rango de fechas (fin posterior a inicio)
- Nombres no vacíos
- IDs válidos
- Prioridades válidas (Baja, Media, Alta)
- Existencia de proyectos, tareas y empleados al realizar operaciones

## Ejemplo de Flujo de Trabajo

1. Registrar empleados
2. Crear un proyecto
3. Añadir tareas al proyecto
4. Asignar tareas a empleados
5. Marcar tareas como completadas
6. Consultar estadísticas de progreso

## Consideraciones

- Los datos se mantienen en memoria durante la ejecución (no hay persistencia)
- Todos los IDs son generados automáticamente
- El sistema proporciona mensajes de error descriptivos para operaciones inválidas
