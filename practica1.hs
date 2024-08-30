import System.IO
import Control.Exception
import Control.Concurrent (threadDelay)


data Articulo = Articulo {
    nombre :: String,
    categoria :: String
} deriving (Show, Read)

type Inventario = [Articulo]

registrarArticulo :: String -> String -> Inventario -> Inventario
registrarArticulo nombreArticulo categoriaArticulo inventario =
    Articulo nombreArticulo categoriaArticulo : inventario

buscarPorCategoria :: String -> Inventario -> [Articulo]
buscarPorCategoria categoriaArticulo inventario =
    filter (\art -> categoria art == categoriaArticulo) inventario

contarPorCategoria :: String -> Inventario -> Int
contarPorCategoria categoriaArticulo inventario =
    length $ buscarPorCategoria categoriaArticulo inventario


guardarInventario :: Inventario -> IO ()
guardarInventario inventario = do
    resultado <- reintentar 5 $ withFile "Inventario.txt" WriteMode (\h -> hPutStr h (unlines (map mostrarArticulo inventario)))
    case resultado of
        Left ex -> putStrLn $ "Error guardando el inventario: " ++ show ex
        Right _ -> putStrLn "Inventario guardado en el archivo inventario.txt."

reintentar :: Int -> IO a -> IO (Either IOException a)
reintentar 0 accion = catch (Right <$> accion) manejarError
  where
    manejarError :: IOException -> IO (Either IOException a)
    manejarError ex = return (Left ex)
reintentar n accion = do
    resultado <- catch (Right <$> accion) manejarError
    case resultado of
        Left _ -> do
            threadDelay 1000000  
            reintentar (n - 1) accion
        Right val -> return (Right val)
  where
    manejarError :: IOException -> IO (Either IOException a)
    manejarError ex = return (Left ex)


cargarInventario :: IO Inventario
cargarInventario = do
    resultado <- try (readFile "inventario.txt") :: IO (Either IOException String)
    case resultado of
        Left ex -> do
            putStrLn $ "Error cargando el inventario: " ++ show ex
            return []
        Right contenido -> do
            let lineas = lines contenido
            let articulos = mapM (\linea -> case reads linea of
                                              [(art, "")] -> Right art
                                              _           -> Left $ "No se pudo analizar la línea: " ++ linea) lineas
            case articulos of
                Left err -> do
                    putStrLn err
                    return []
                Right artList -> return artList

mostrarArticulo :: Articulo -> String
mostrarArticulo articulo =
    "Articulo{ nombre = \"" ++ nombre articulo ++ "\", categoria = \"" ++ categoria articulo ++ "\" }"

main :: IO ()
main = do
    inventario <- cargarInventario
    putStrLn "¡Bienvenido al Sistema de Gestión de Inventario!"

    cicloPrincipal inventario

cicloPrincipal :: Inventario -> IO ()
cicloPrincipal inventario = do
    putStrLn "Seleccione una opción:"
    putStrLn "1. Registrar entrada de artículo"
    putStrLn "2. Buscar artículos por categoría"
    putStrLn "3. Listar todos los artículos"
    putStrLn "4. Mostrar cantidad de artículos por categoría"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el nombre del artículo:"
            nombreArticulo <- getLine
            putStrLn "Ingrese la categoría del artículo:"
            categoriaArticulo <- getLine
            let inventarioActualizado = registrarArticulo nombreArticulo categoriaArticulo inventario
            guardarInventario inventarioActualizado
            cicloPrincipal inventarioActualizado
        "2" -> do
            putStrLn "Ingrese la categoría a buscar:"
            categoriaArticulo <- getLine
            let articulosEncontrados = buscarPorCategoria categoriaArticulo inventario
            if null articulosEncontrados
                then putStrLn "No se encontraron artículos en esa categoría."
                else mapM_ (putStrLn . mostrarArticulo) articulosEncontrados
            cicloPrincipal inventario
        "3" -> do
            putStrLn "Mostrando todos los artículos en el inventario:"
            mapM_ (putStrLn . mostrarArticulo) inventario
            cicloPrincipal inventario
        "4" -> do
            putStrLn "Ingrese la categoría para contar artículos:"
            categoriaArticulo <- getLine
            let cantidad = contarPorCategoria categoriaArticulo inventario
            putStrLn $ "Hay " ++ show cantidad ++ " artículos en la categoría " ++ categoriaArticulo
            cicloPrincipal inventario
        "5" -> putStrLn "!Muchas gracias por su compra, que tenga una buena tarde¡"
        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal inventario
