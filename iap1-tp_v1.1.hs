-- Modulo para Testing

module Solucion where

-- Completar con los datos del grupo
--
-- Nombre de Grupo: WhatTheHaskell
-- Integrante 1: Alison Yamila Herrera Aguilar, yaliherrera02@gmail.com, 814/23
-- Integrante 2: Camila Aylén Grassi, camigras04@gmail.com, 176/23
-- Integrante 3: Sofía Gerpe Lizárraga, sofiagerpel@gmail.com, 411/23
-- Integrante 4: Theo Vilardo, theovilardo@gmail.com, 743/22

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

-- describir qué hace la función: EJERCICIO 1 -> Devuelve los nombres de usuario de la red social
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios (a,b,c) = eliminaRepetidos (proyectarNombres (usuarios (a,b,c)))

-- Funciones Auxiliares

proyectarNombres :: [Usuario] -> [String]
proyectarNombres [] = []
proyectarNombres (x:xs) = snd x : proyectarNombres xs

eliminaRepetidos :: Eq a => [a] -> [a]
eliminaRepetidos [] = []
eliminaRepetidos (x:xs) = x : eliminaRepetidos (quita x xs)
  where quita _ [] = []
        quita y (z:zs) | y == z    = quita y zs
                       | otherwise = z : quita y zs

-- describir qué hace la función: EJERCICIO 2 -> Devuelve la lista de amigos que tiene el usuario ingresado (dentro de la red)
--Le falta función para eliminar los repetidos
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe rd u = listaDeAmigos u (relaciones rd)


listaDeAmigos :: Usuario -> [Relacion] -> [Usuario]
listaDeAmigos u [] = []
listaDeAmigos u (x:xs) | u == fst x = snd x : listaDeAmigos u xs
                       | u == snd x = fst x : listaDeAmigos u xs
                       | otherwise = listaDeAmigos u xs 

-- describir qué hace la función: EJERCICIO 3 -> Devuelve la cantidad de amigos que tiene el usuario ingresado (dentro de la red)
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos (us,rs,ps) (id, nombre) = contarAmigos (amigosDe (us,rs,ps) (id, nombre))

contarAmigos :: [Usuario] -> Int
contarAmigos []= 0
contarAmigos (x:xs) = 1+ contarAmigos xs

-- describir qué hace la función: EJERCICIO 4 -> Dada la red social, devuelve el usuario con mas amigos
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos (us,rs,ps) = cantidadDeAcu (us,rs,ps) (usuarios (us,rs,ps))

-- Funciones Auxiliares
cantidadDeAcu :: RedSocial -> [Usuario] -> Usuario
cantidadDeAcu rs (x:xs) | null (x:xs) = x
                        | cantidadDeAmigos (rs) x > cantidadDeAmigos (rs) (head xs) = x
                        | otherwise = cantidadDeAcu rs xs

-- Reemplazo de head por las dudas:
cabeza :: [a] -> a
cabeza [x] = x
cabeza (x:xs) = x

-- describir qué hace la función: EJERCICIO 5 -> Si dentro de la red hay un usuario con mas de un millon de amigos devuelve True, sino False
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos (us,rs,ps) | cantidadDeAmigos (us,rs,ps) (usuarioConMasAmigos (us,rs,ps)) > 1000000 = True
                             | otherwise = False

-- describir qué hace la función: EJERCICIO 6 -> devuelve las publicaciones de un usuario dentro de la red social
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (us,rs,ps) u = pubDe (publicaciones (us,rs,ps)) u

-- Funciones Auxiliares

pubDe :: [Publicacion] -> Usuario -> [Publicacion]
pubDe [] _ = []
pubDe (x:xs) u | null (x:xs) = [x]
               | usuarioDePublicacion x == u = x : pubDe xs u
               | otherwise = pubDe xs u

-- describir qué hace la función: EJERCICIO 7 -> Devuelve las publicaciones que le gustaron al usuario ingresado (dentro de la red)
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (us,rs,ps) u = eliminaRepetidos (tusLikes (publicaciones (us,rs,ps)) u) -- FALTA PROBAR LOS CASOS DE REPETICION

-- Funciones Auxiliares

tusLikes :: [Publicacion] -> Usuario -> [Publicacion]
tusLikes [] _ = []
tusLikes (x:xs) u | null (x:xs) = [x]
                  | pertenece u (likesDePublicacion x) = x : tusLikes xs u
                  | otherwise = tusLikes xs u

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece a [] = False
pertenece a (x:xs) | a == x = True
                   | otherwise = pertenece a xs

-- describir qué hace la función: EJERCICIO 8 -> Devuelve True si los usuarios ingresados le dieron me gusta a las mismas publicaciones #Actualizado para la version 2.1 del TP
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones (us,rs,ps) a b | mismaCantidad (publicacionesQueLeGustanA (us,rs,ps) a) (publicacionesQueLeGustanA (us,rs,ps) b) && mismosElementos (publicacionesQueLeGustanA (us,rs,ps) a) (publicacionesQueLeGustanA (us,rs,ps) b) = True
                                               | otherwise = False

-- Funciones Auxiliares

longitud :: Eq t => [t] -> Integer
longitud (x:xs) | null (x:xs) = 0
                | (x:xs) == [x] = 1
                | otherwise = 1 + longitud xs

mismaCantidad :: (Eq t) => [t] -> [t] -> Bool
mismaCantidad a b | a == b = True
                  | longitud a == longitud b = True
                  | otherwise = False

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos a b = listaEnLista a b && listaEnLista b a

listaEnLista :: (Eq t) => [t] -> [t] -> Bool
listaEnLista [] y = True
listaEnLista (x:xs) y = pertenece x y && listaEnLista xs y

-- describir qué hace la función: EJERCICIO 9 -> Devuelve True si existe un usuario que le haya dado like a todas las publicaciones hechas por el usuario ingresado
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel (us,rs,ps) u = likesPertenecen (us,rs,ps) (quitar u (usuarios (us,rs,ps))) (publicacionesDe (us,rs,ps) u)

-- Funciones Auxiliares
quitar :: (Eq t) => t -> [t] -> [t]
quitar a (x:xs) | null (x:xs) = []
                | not (pertenece a (x:xs)) = x:xs
                | a == x = xs 
                | otherwise = x : quitar a xs

likesPertenecen :: RedSocial -> [Usuario] -> [Publicacion] -> Bool
likesPertenecen (_,_,_) [] _ = False
likesPertenecen (us,rs,ps) _ [] = False
likesPertenecen (us,rs,ps) (x:xs) (y:ys) | perteneceLike (y:ys) (publicacionesQueLeGustanA (us,rs,ps) x) = True
                                         | otherwise = likesPertenecen (us,rs,ps) xs (y:ys)

perteneceLike :: (Eq t) => [t] -> [t] -> Bool
perteneceLike [] [] = False
perteneceLike _ [] = False
perteneceLike (y:ys) (x:xs) | (y:ys) == [y] && pertenece y (x:xs) = True
perteneceLike (y:ys) (x:xs) | pertenece y (x:xs) = perteneceLike ys (x:xs)
                            | otherwise = False

-- describir qué hace la función: EJERCICIO 10 -> Devuelve true si en la red existe una cadena de amistades
-- Explicacion:
-- La idea es crear una lista aparte que contiene a todos los amigos del u1 y a los amigos de sus amigos y a los amigos de los amigos de sus amigos...
-- Entonces, si u2 esta relacionado con el u1 a través de alguna de sus amistades, u2 tendría que aparecer en esa lista.

existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos rs u1 u2 | (amigosDe rs u1) == [] || (amigosDe rs u2) == [] = False
                                 | pertenece u1 (amigosDe rs u2) = True
                                 | pertenece u2 (listaDeAmigos2 rs u1 (amigosDe rs u1)) = True
                                 | otherwise = False
                                 

listaDeAmigos2 :: RedSocial -> Usuario -> [Usuario] -> [Usuario]
listaDeAmigos2 rs u1 [] = []
listaDeAmigos2 rs u1 (x:xs) = [x] ++ (amigosDeAmigos rs (amigosDe rs x) (u1:x:[])) ++ listaDeAmigos2 rs u1 xs 

-- amigosDeAmigos recibe 2 listas. La primera son los amigos de la cabeza de la lista que aparece en la función anterior.
-- La segunda es una lista aparte que contiene a todos los usuarios a los que yo ya me fijé quienes eran sus amigos, esto es para evitar un bucle infinito.
-- La función quitar del where es para quitar a todos los elementos de esa lista de la lista de amigos del usuario

amigosDeAmigos :: RedSocial -> [Usuario] -> [Usuario] -> [Usuario]
amigosDeAmigos rs [] ys = []
amigosDeAmigos rs (x:xs) ys = [x] ++ amigosDeAmigos rs (quitar ys(amigosDe rs x)) (x:ys) ++ amigosDeAmigos rs xs ys
 where quitar (y:ys) [] = []
       quitar [] (z:zs) = (z:zs)
       quitar (y:ys) (z:zs) | (y:ys) == [y] = quita y (z:zs)
                            | otherwise = quitar ys (quita y (z:zs))
        where quita _ [] = []
              quita y (z:zs) | y == z = quita y zs
                             | otherwise = z : quita y zs
