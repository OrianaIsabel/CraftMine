module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Personaje = UnPersonaje { 
    nombre :: String, 
    puntaje :: Number, 
    inventario :: [Material] 
    } deriving Show

tom :: Personaje
tom = UnPersonaje "Tom" 500 ["madera","tinte","hierro","tierra","sueter","harina","hielo","algodon"]

yorke :: Personaje
yorke = UnPersonaje "Yorke" 200 ["arena","chocolate","madera","harina","paraguas"]

type Material = String

data Receta = UnaReceta {
    producto :: String,
    componentes :: [Material],
    tiempo :: Number
    } deriving Show

cajon :: Receta
cajon = UnaReceta "cajon" ["hierro","madera"] 6

torta :: Receta
torta = UnaReceta "torta" ["harina","chocolate"] 2

cama :: Receta
cama = UnaReceta "cama" ["madera","algodon","tinte"] 50

iglu :: Receta
iglu = UnaReceta "barco" ["hielo"] 70

-- Punto 1

sumarProducto :: Material -> [Material] -> [Material]
sumarProducto material materiales = material : materiales

eliminarMateriales :: [Material] -> [Material] -> [Material]
eliminarMateriales materiales1 materiales2 = filter (noSeUsaEn materiales1) materiales2

noSeUsaEn :: [Material] -> Material -> Bool
noSeUsaEn materiales material
    | seUsaEn materiales material = False
    | otherwise = True

seUsaEn :: [Material] -> Material -> Bool
seUsaEn materiales material = elem material materiales

alterarInventario :: Receta -> Personaje -> Personaje
alterarInventario receta persona = persona { inventario = (sumarProducto (producto receta).eliminarMateriales (componentes receta)) (inventario persona) }

incrementarPuntaje :: Receta -> Personaje -> Personaje
incrementarPuntaje receta persona = persona { puntaje = (puntaje persona) + (tiempo receta) * 10 }

craftear :: Receta -> Personaje -> Personaje
craftear receta persona = alterarInventario receta (incrementarPuntaje receta persona)

loPuedeCraftear :: Receta -> Personaje -> Bool
loPuedeCraftear receta persona = all (loTiene (inventario persona)) (componentes receta)

loTiene :: [Material] -> Material -> Bool
loTiene materiales material = elem material materiales

intentarCraftear :: Receta -> Personaje -> Personaje
intentarCraftear receta persona
    | loPuedeCraftear receta persona = craftear receta persona
    | otherwise = persona { puntaje = puntaje persona - 100 }

-- Punto 2

cualPuedeCraftearYLeDuplica :: [Receta] -> Personaje -> [Receta]
cualPuedeCraftearYLeDuplica recetas persona = filter (flip puedeCraftearYLeDuplica persona) recetas

duplicaPuntaje :: Receta -> Personaje -> Bool
duplicaPuntaje receta persona
    | (puntaje.(craftear receta)) persona >= (2 * (puntaje persona)) = True
    | otherwise = False

puedeCraftearYLeDuplica :: Receta -> Personaje -> Bool
puedeCraftearYLeDuplica receta persona = loPuedeCraftear receta persona && duplicaPuntaje receta persona

craftearSucesivamente :: [Receta] -> Personaje -> Personaje
craftearSucesivamente recetas persona = foldr intentarCraftear persona (cualPuedeCraftearYLeDuplica recetas persona)

quedaConMasPuntos :: [Receta] -> Personaje -> Bool
quedaConMasPuntos recetas persona
    | puntaje (craftearSucesivamente recetas persona) > puntaje persona = True
    | otherwise = False

-- Punto 3

data Bioma = UnBioma {
    materiales :: [Material],
    requerido :: Material
} deriving Show

artico :: Bioma
artico = UnBioma ["hielo","iglu","lobos"] "sueter"

type Herramienta = [Material] -> Material

hacha :: Herramienta
hacha = last

espada :: Herramienta
espada = head

pico :: Number -> Herramienta
pico = flip (!!)

minar :: Herramienta -> Personaje -> Bioma -> Personaje
minar herramienta persona bioma
    | poseeLoRequerido persona bioma = minarMaterial herramienta persona bioma
    | otherwise = persona

poseeLoRequerido :: Personaje -> Bioma -> Bool
poseeLoRequerido persona bioma = loTiene (inventario persona) (requerido bioma)

minarMaterial :: Herramienta -> Personaje -> Bioma -> Personaje
minarMaterial herramienta persona bioma = persona { inventario = (inventario persona) ++ [herramienta (materiales bioma)]}