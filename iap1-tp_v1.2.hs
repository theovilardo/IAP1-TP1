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

-- EJERCICIOS


-- Funciones auxiliares que se utilizan en distintos ejercicios

eliminaRepetidos :: (Eq t) => [t] -> [t]
eliminaRepetidos [] = []
eliminaRepetidos (x:xs) = x : eliminaRepetidos (quitar x xs)

longitud :: (Eq t) => [t] -> Int 
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece a [] = False
pertenece a (x:xs) | a == x = True
                   | otherwise = pertenece a xs

quitar :: (Eq t) => t -> [t] -> [t]
quitar a [] = []
quitar a (x:xs) | not (pertenece a (x:xs)) = x:xs
                | a == x = xs 
                | otherwise = x : quitar a xs

compartenElementos :: (Eq t) => [t] -> [t] -> Bool
compartenElementos [] _ = False
compartenElementos _ [] = False
compartenElementos (x:xs) ys | pertenece x ys && (x:xs) == [x] = True
                             | not(pertenece x ys) = False
                             | otherwise = compartenElementos xs ys 


-- EJERCICIO 1 -> Devuelve un conjunto con los nombres de usuario de la red social
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios (us,rs,ps) = eliminaRepetidos (proyectarNombres (usuarios (us,rs,ps)))

-- Funciones Auxiliares

proyectarNombres :: [Usuario] -> [String]
proyectarNombres [] = []
proyectarNombres (x:xs) = nombreDeUsuario x : proyectarNombres xs


-- EJERCICIO 2 -> Devuelve un conjunto con los amigos que tiene el usuario ingresado dentro de la red
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (us,rs,ps) u = eliminaRepetidos (listaDeAmigosDe u (relaciones (us,rs,ps)))

-- Funciones Auxiliares

listaDeAmigosDe :: Usuario -> [Relacion] -> [Usuario]
listaDeAmigosDe u [] = []
listaDeAmigosDe u (x:xs) | u == fst x = snd x : listaDeAmigosDe u xs
                         | u == snd x = fst x : listaDeAmigosDe u xs
                         | otherwise = listaDeAmigosDe u xs 


-- EJERCICIO 3 -> Devuelve la cantidad de amigos que tiene el usuario ingresado dentro de la red
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos (us,rs,ps) u = longitud (amigosDe (us,rs,ps) u)


-- EJERCICIO 4 -> Devuelve el usuario con más amigos dentro de la red social
-- Si dos o más usuarios tienen la mayor cantidad de amigos devuelve cualquiera de ellos
-- Si la mayor cantidad de amigos es 0 devuelve cualquier usuario de la red
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos (us,rs,ps) = tieneMasAmigos (us,rs,ps) (usuarios (us,rs,ps))

-- Funciones Auxiliares

tieneMasAmigos :: RedSocial -> [Usuario] -> Usuario
tieneMasAmigos (us,[],ps) (x:xs) = x
tieneMasAmigos (us,rs,ps) (x:xs) | xs == [] = x
                                 | cantidadDeAmigos (us,rs,ps) x > cantidadDeAmigos (us,rs,ps) (head xs) = tieneMasAmigos (us,rs,ps) (x : (tail xs))
                                 | otherwise = tieneMasAmigos (us,rs,ps) xs


-- EJERCICIO 5 -> Devuelve True si dentro de la red hay un usuario con más de un diez amigos
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos (us,rs,ps) | cantidadDeAmigos (us,rs,ps) (usuarioConMasAmigos (us,rs,ps)) > 10 = True
                             | otherwise = False


-- EJERCICIO 6 -> Devuelve un conjunto con las publicaciones de un usuario dentro de la red social
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (us,rs,ps) u = eliminaRepetidos (listaDePublicacionesDe (publicaciones (us,rs,ps)) u)

-- Funciones Auxiliares

listaDePublicacionesDe :: [Publicacion] -> Usuario -> [Publicacion]
listaDePublicacionesDe [] _ = []
listaDePublicacionesDe (x:xs) u | null (x:xs) = [x]
                              | usuarioDePublicacion x == u = x : listaDePublicacionesDe xs u
                              | otherwise = listaDePublicacionesDe xs u


-- EJERCICIO 7 -> Devuelve un conjunto con las publicaciones que le gustaron al usuario ingresado (dentro de la red)
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (us,rs,ps) u = eliminaRepetidos (tusLikes (publicaciones (us,rs,ps)) u)

-- Funciones Auxiliares

tusLikes :: [Publicacion] -> Usuario -> [Publicacion]
tusLikes [] _ = []
tusLikes (x:xs) u | null (x:xs) = [x]
                  | pertenece u (likesDePublicacion x) = x : tusLikes xs u
                  | otherwise = tusLikes xs u


-- EJERCICIO 8 -> Devuelve True si los usuarios ingresados le dieron me gusta a las mismas publicaciones #Actualizado para la version 2.1 del TP
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones (us,rs,ps) u1 u2 | longitud (publicacionesQueLeGustanA (us,rs,ps) u1) == longitud (publicacionesQueLeGustanA (us,rs,ps) u2) && compartenElementos (publicacionesQueLeGustanA (us,rs,ps) u1) (publicacionesQueLeGustanA (us,rs,ps) u2) = True
                                                 | otherwise = False


-- EJERCICIO 9 -> Devuelve True si existe un usuario que le haya dado like a todas las publicaciones hechas por el usuario ingresado
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel (us,rs,ps) u = algunoDioLikeATodas (us,rs,ps) (quitar u (usuarios (us,rs,ps))) (publicacionesDe (us,rs,ps) u)

-- Funciones Auxiliares

algunoDioLikeATodas :: RedSocial -> [Usuario] -> [Publicacion] -> Bool
algunoDioLikeATodas (_,_,_) [] _ = False
algunoDioLikeATodas (us,rs,ps) _ [] = False
algunoDioLikeATodas (us,rs,ps) (x:xs) (y:ys) | compartenElementos (y:ys) (publicacionesQueLeGustanA (us,rs,ps) x) = True
                                             | otherwise = algunoDioLikeATodas (us,rs,ps) xs (y:ys)

 
-- EJERCICIO 10 -> Devuelve True si en la red existe una cadena de amistades
-- Para que exista una cadena de amistades se tiene que llegar de uno al otro a través de relaciones directas o indirectas
-- Relación directa: el usuario es amigo del usuario ingresado
-- Relación indirecta: el usuario no es amigo del usuario ingresado, pero tiene amigos en común o sus amigos tienen amigos en común con el usuario ingresado
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos (us,rs,ps) u1 u2 | (amigosDe (us,rs,ps) u1) == [] || (amigosDe (us,rs,ps) u2) == [] = False
                                         | pertenece u1 (amigosDe (us,rs,ps) u2) = True
                                         | pertenece u2 (amistadDirectaEIndirecta (us,rs,ps) u1 (amigosDe (us,rs,ps) u1)) = True
                                         | otherwise = False   
                              
-- Crea una lista que contiene todos los usuarios con los que se relaciona directa o indirectamente el usuario ingresado. 
amistadDirectaEIndirecta :: RedSocial -> Usuario -> [Usuario] -> [Usuario]
amistadDirectaEIndirecta (us,rs,ps) u1 [] = []
amistadDirectaEIndirecta (us,rs,ps) u1 (x:xs) = [x] ++ (amigosDeAmigos (us,rs,ps) (amigosDe (us,rs,ps) x) (u1:x:[])) ++ amistadDirectaEIndirecta (us,rs,ps) u1 xs 

--Crea una lista con los amigos de los amigos de los usuarios y en cada recursión quita de la primera lista los usuarios que ya fueron chequeados para evitar un bucle infinito
amigosDeAmigos :: RedSocial -> [Usuario] -> [Usuario] -> [Usuario]
amigosDeAmigos (us,rs,ps) [] ys = []
amigosDeAmigos (us,rs,ps) (x:xs) ys = [x] ++ (amigosDeAmigos (us,rs,ps) (quitarLista ys (amigosDe (us,rs,ps) x)) (x:ys)) ++ amigosDeAmigos (us,rs,ps) xs ys
 where quitarLista (y:ys) [] = []
       quitarLista [] (z:zs) = (z:zs)
       quitarLista (y:ys) (z:zs) | (y:ys) == [y] = quitar y (z:zs)
                                 | otherwise = quitarLista ys (quitar y (z:zs))


