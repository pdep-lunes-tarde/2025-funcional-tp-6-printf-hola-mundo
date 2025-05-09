module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "PARTE 1 - HAMBURGUESA" $ do
        describe "agrandar" $ do 
            it "Si la hamburguesa a agrandar tiene como ingrediente base carne, se agranda la hamburguesa con carne" $ do
                agrandar cuartoDeLibra `shouldBe` Hamburguesa 20 [Carne, Pan, Carne, Cheddar, Pan]

            it "Si la hamburguesa a agrandar tiene como ingrediente base pollo, se agranda la hamburguesa con pollo" $ do
                agrandar mcpollo `shouldBe` Hamburguesa 70 [Pollo, Pan, Pollo, Cheddar, Panceta, Pan]
                
            it "Si la hamburguesa a agrandar tiene como ingrediente base pati vegano, se agranda la hamburguesa con pati vegano" $ do
                agrandar veggie `shouldBe` Hamburguesa 40 [Pativegano, Pan, Pativegano, QuesoDeAlmendras, Pan]
                
            it "Si la hamburguesa a agrandar tiene varios ingredientes base, se agranda la hamburguesa segun que ingrediente base este primero en sus ingredientes  " $ do
                agrandar mixta `shouldBe` Hamburguesa 50 [Pollo, Pan, Pollo, Carne, Cheddar, Pan]

        describe "agregarIngrediente" $ do 
            it "Agregar cheddar a una hamburguesa mixta" $ do
                agregarIngrediente Cheddar mixta `shouldBe` Hamburguesa 50 [Cheddar, Pan, Pollo, Carne, Cheddar, Pan]

        describe "descuento" $ do
            it "Aplicar un descuento del 0% a la hamburguesa mixta" $ do
                descuento 0 mixta `shouldBe` Hamburguesa 50 [Pan, Pollo, Carne, Cheddar, Pan]
            it "Aplicarle un descuento del 30% a la hamburguesa mcpollo" $ do
                descuento 30 mcpollo `shouldBe` Hamburguesa 49 [Pan, Pollo, Cheddar, Panceta, Pan]

        describe "pdepBurger" $ do 
            it "El precio final de la PdepBurger deberia ser 110" $ do
                precioFinal pdepBurger `shouldBe` 110 

    describe "PARTE 2 - ALGUNAS HAMBURGUESAS MAS" $ do
        describe "dobleCuarto" $ do 
            it "El precio final de la hamburguesa doble cuarto deberia ser 84" $ do
                precioFinal dobleCuarto `shouldBe` 84

        describe "bigPdep" $ do 
            it "El precio final de la Big Pdep deberia ser 89" $ do
                precioFinal bigPdep `shouldBe` 89

        describe "delDia" $ do 
            it "Aplicar la promo 'Del Dia' a la hamburguesa mega dibu" $ do
                delDia megaDibu `shouldBe` Hamburguesa 70 [Papas, Pan, Carne, Carne, Cheddar, Cheddar, Panceta, Panceta, Pan]

    describe "PARTE 3 - ALGUNOS CAMBIOS MAS" $ do
        describe "hacerVeggie" $ do 
            it "Si le pasamos una hamburguesa, remplaza los ingredientes de origen animal por ingredientes veganos " $ do
                hacerVeggie cuartoDeLibra `shouldBe` Hamburguesa 20 [Pan, Pativegano, QuesoDeAlmendras, Pan]
                hacerVeggie mixta `shouldBe` Hamburguesa 50 [Pan, Pativegano, Pativegano, QuesoDeAlmendras, Pan]
                hacerVeggie mcpollo `shouldBe` Hamburguesa 70 [Pan, Pativegano, QuesoDeAlmendras, BaconDeTofu, Pan]

        describe "cambiarPanDePati" $ do 
            it "Cambiar los panes de la hamburguesa mixta" $ do
                cambiarPanDePati mixta `shouldBe` Hamburguesa 50 [PanIntegral, Pollo, Carne, Cheddar, PanIntegral] 

        describe "dobleCuartoVegano" $ do 
            it "Los ingredientes de la hamburguesa doble cuarto vegano " $ do
                ingredientes dobleCuartoVegano `shouldBe` [Pativegano,QuesoDeAlmendras,PanIntegral,Pativegano,QuesoDeAlmendras,PanIntegral]
