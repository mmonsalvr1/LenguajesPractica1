import System.IO
import Control.Exception (catch, IOException)

-- Definimos el tipo de datos
data Producto = Producto { 
  nombre :: String, 
  categoria :: String 
  } deriving (Show, Read)

-- Funciones
registrarProducto :: Producto -> [Producto] -> [Producto]
registrarProducto nuevoProducto inventario = nuevoProducto : inventario


buscarPorCategoria :: String -> [Producto] -> [Producto]
buscarPorCategoria cat inventario = filter (\p -> categoria p == cat) inventario


listarInventario :: [Producto] -> [Producto]
listarInventario = id


contarPorCategoria :: String -> [Producto] -> Int
contarPorCategoria cat inventario = length $ buscarPorCategoria cat inventario


guardarInventario :: [Producto] -> IO ()
guardarInventario inventario = withFile "inventario.txt" WriteMode $ \h -> do
    hPutStrLn h (show inventario)


cargarInventario :: IO [Producto]
cargarInventario = do
    contenido <- catch (readFile "inventario.txt") manejarError
    return (read contenido :: [Producto])

-- revisa el archivo para no dar error
manejarError :: IOException -> IO String
manejarError _ = return "[]"

-- Menu
menu :: [Producto] -> IO ()
menu inventario = do
    putStrLn "\nMenú Principal:"
    putStrLn "1. Registrar nuevo producto"
    putStrLn "2. Buscar productos por categoría"
    putStrLn "3. Listar inventario"
    putStrLn "4. Contar productos por categoría"
    putStrLn "5. Salir"
    putStrLn "Selecciona una opción: "
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Nombre del producto:"
            nom <- getLine
            putStrLn "Categoría del producto:"
            cat <- getLine
            let nuevoProducto = Producto nom cat
            let nuevoInventario = registrarProducto nuevoProducto inventario
            guardarInventario nuevoInventario
            putStrLn "Producto registrado y guardado."
            menu nuevoInventario
        "2" -> do
            putStrLn "Ingrese la categoría a buscar:"
            cat <- getLine
            let resultados = buscarPorCategoria cat inventario
            putStrLn "Productos en la categoría:"
            print resultados
            menu inventario
        "3" -> do
            putStrLn "Inventario completo:"
            print (listarInventario inventario)
            menu inventario
        "4" -> do
            putStrLn "Ingrese la categoría a contar:"
            cat <- getLine
            let cantidad = contarPorCategoria cat inventario
            putStrLn ("Cantidad de productos en la categoría " ++ cat ++ ": " ++ show cantidad)
            menu inventario
        "5" -> do
            putStrLn "Saliendo..."
            return ()
        _ -> do
            putStrLn "Opción no válida, intenta de nuevo."
            menu inventario

-- main
main :: IO ()
main = do
    putStrLn "Cargando inventario desde 'inventario.txt'..."
    inventario <- cargarInventario
    menu inventario
