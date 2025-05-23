module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | PanIntegral | Panceta | Cheddar | Pollo | Curry | Papas | QuesoDeAlmendras | Pativegano | BaconDeTofu
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas = 10
precioIngrediente Pativegano = 10
precioIngrediente BaconDeTofu = 12
precioIngrediente PanIntegral = 3


data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

megaDibu = Hamburguesa 100 [Pan, Carne, Carne, Cheddar, Cheddar, Panceta, Panceta, Pan]
mcpollo = Hamburguesa 70 [Pan, Pollo, Cheddar, Panceta, Pan]
mixta = Hamburguesa 50 [Pan, Pollo, Carne, Cheddar, Pan]
cuartoDeLibra = Hamburguesa 20 [Pan, Carne, Cheddar, Pan]
veggie = Hamburguesa 40 [Pan, Pativegano, QuesoDeAlmendras, Pan]

-- PARTE 1 : HAMBURGUESA

-- AGRANDAR HAMBURGUESA

agrandar :: Hamburguesa -> Hamburguesa
agrandar = actualizarHamburgesaAgrandada

actualizarHamburgesaAgrandada :: Hamburguesa ->  Hamburguesa
actualizarHamburgesaAgrandada hamburguesa = 
    agregarIngrediente ((elIngredienteBaseEs . ingredientes) hamburguesa) hamburguesa

elIngredienteBaseEs :: [Ingrediente] -> Ingrediente
elIngredienteBaseEs (ingrediente:ingredientes)
    | ingrediente == Carne = Carne
    | ingrediente == Pollo = Pollo
    | ingrediente == Pativegano = Pativegano
    | otherwise = elIngredienteBaseEs ingredientes

-- AGREGAR INGREDIENTE

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamburguesa = 
    hamburguesa {ingredientes = ingrediente : ingredientes hamburguesa} 

-- APLICAR x % DE DESCUENTO A UNA HAMBURGUESA

descuento ::  Number -> Hamburguesa -> Hamburguesa
descuento  porcentaje hamburguesa = hamburguesa {
    precioBase = (calcularDescuento porcentaje . precioBase) hamburguesa }

calcularDescuento :: Number -> Number -> Number
calcularDescuento porcentaje precio = precio - (precio * porcentaje/100) 

precioIngredientes :: [Ingrediente] -> Number
precioIngredientes = sum . map precioIngrediente

precioFinal :: Hamburguesa -> Number
precioFinal hamburguesa = precioBase hamburguesa + precioIngredientes (ingredientes hamburguesa)

-- LA PDEP BURGER

pdepBurger :: Hamburguesa
pdepBurger = descuento 20 . agregarIngrediente Cheddar . agregarIngrediente Panceta . agrandar . agrandar $ cuartoDeLibra

-- PARTE 2 : ALGUNAS HAMBURGUESA MAS

dobleCuarto :: Hamburguesa
dobleCuarto = agregarIngrediente Carne . agregarIngrediente Cheddar $ cuartoDeLibra

bigPdep :: Hamburguesa 
bigPdep = agregarIngrediente Curry dobleCuarto 

delDia :: Hamburguesa -> Hamburguesa
delDia = descuento 30 . agregarIngrediente Papas

-- PARTE 3 : ALGUNOS CAMBIOS MAS

-- Hacer hamburguesa vegana 

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie hamburguesa = hamburguesa {ingredientes = map remplazar (ingredientes hamburguesa)}

remplazar :: Ingrediente -> Ingrediente
remplazar Carne = Pativegano
remplazar Pollo = Pativegano
remplazar Cheddar = QuesoDeAlmendras
remplazar Panceta = BaconDeTofu
remplazar ingrediente = ingrediente 

cambiarAPanIntegral :: Ingrediente -> Ingrediente
cambiarAPanIntegral  Pan = PanIntegral
cambiarAPanIntegral ingrediente = ingrediente

-- CAMBIAR PAN DE PATI

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati hamburguesa = hamburguesa {ingredientes = map cambiarAPanIntegral (ingredientes hamburguesa)}

-- Doble Cuarto Vegano

dobleCuartoVegano :: Hamburguesa
dobleCuartoVegano = cambiarPanDePati . hacerVeggie $ dobleCuarto 