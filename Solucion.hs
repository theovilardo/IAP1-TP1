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
eliminaRepetidos (x:xs) | pertenece x xs = eliminaRepetidos xs -- (elimina la primera aparicion de x)
                        | otherwise = x: eliminaRepetidos xs -- (si x ya no tiene repetidos, lo agrega y avanza con el siguiente x)

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

-- Verifica si todos los elementos de una lista están contenidos en otra
estaContenida :: (Eq t) => [t] -> [t] -> Bool
estaContenida [] _ = False
estaContenida _ [] = False
estaContenida (x:xs) ys | pertenece x ys && (x:xs) == [x] = True
                        | not(pertenece x ys) = False
                        | otherwise = estaContenida xs ys 


-- EJERCICIO 1 -> Devuelve una lista con los nombres de usuario de la red social ingresada sin repetir ninguno.
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios (us,rs,ps) = eliminaRepetidos (proyectarNombres (usuarios (us,rs,ps)))

-- Funciones Auxiliares

proyectarNombres :: [Usuario] -> [String] -- Concatena los nombres de los usuarios de la lista perteneciente a la red social ingresada y devuelve una lista con todos los nombres de usuario.
proyectarNombres [] = []
proyectarNombres (x:xs) = nombreDeUsuario x : proyectarNombres xs


-- EJERCICIO 2 -> Devuelve una lista con los amigos que tiene el usuario ingresado dentro de la red (sin repetir ninguno).
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (us,rs,ps) u = eliminaRepetidos (listaDeAmigosDe u (relaciones (us,rs,ps)))

-- Funciones Auxiliares

listaDeAmigosDe :: Usuario -> [Relacion] -> [Usuario] -- Devuelve una lista concatenando los usuarios que pertenezcan a las relaciones del usuario ingresado dentro de la red, para esto la funcion chequea si el usuario ingresado aparene en el primer o segundo lugar de la tupla (relacion).
listaDeAmigosDe u [] = []
listaDeAmigosDe u (x:xs) | u == fst x = snd x : listaDeAmigosDe u xs
                         | u == snd x = fst x : listaDeAmigosDe u xs
                         | otherwise = listaDeAmigosDe u xs 


-- EJERCICIO 3 -> Devuelve la cantidad de amigos que tiene el usuario ingresado dentro de la red social.
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos (us,rs,ps) u = longitud (amigosDe (us,rs,ps) u)


-- EJERCICIO 4 -> Devuelve el usuario con más amigos dentro de la red social
-- Si dos o más usuarios tienen la mayor cantidad de amigos devuelve cualquiera de ellos
-- Si la mayor cantidad de amigos es 0 devuelve cualquier usuario de la red
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos (us,rs,ps) = tieneMasAmigos (us,rs,ps) (usuarios (us,rs,ps))

-- Funciones Auxiliares

tieneMasAmigos :: RedSocial -> [Usuario] -> Usuario -- Compara la cantidad de amigos de todos los usuarios dentro de la red ingresada y devuelve al que tenga mayor cantidad.
tieneMasAmigos (us,[],ps) (x:xs) = x
tieneMasAmigos (us,rs,ps) (x:xs) | xs == [] = x
                                 | cantidadDeAmigos (us,rs,ps) x > cantidadDeAmigos (us,rs,ps) (head xs) = tieneMasAmigos (us,rs,ps) (x : (tail xs))
                                 | otherwise = tieneMasAmigos (us,rs,ps) xs


-- EJERCICIO 5 -> Devuelve True si dentro de la red hay un usuario con más de diez amigos, se utiliza la funcion "usuarioConMasAmigos" ya que si el que mas amigos tiene no alcanza esa cantidad, los que tienen menos tampoco podrán.
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos (us,rs,ps) | cantidadDeAmigos (us,rs,ps) (usuarioConMasAmigos (us,rs,ps)) > 10 = True
                             | otherwise = False


-- EJERCICIO 6 -> Devuelve una lista con todas las publicaciones del usuario ingresado dentro de la red social (sin repetir ninguna).
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (us,rs,ps) u = eliminaRepetidos (listaDePublicacionesDe (publicaciones (us,rs,ps)) u)

-- Funciones Auxiliares

listaDePublicacionesDe :: [Publicacion] -> Usuario -> [Publicacion] -- Chequea recursivamente que las publicaciones de la red social sean del usuario ingresado, de ser asi creara una lista de las publicaciones concatenando las que cumplan dicha condicion.
listaDePublicacionesDe [] _ = []
listaDePublicacionesDe (x:xs) u | null (x:xs) = [x]
                              | usuarioDePublicacion x == u = x : listaDePublicacionesDe xs u
                              | otherwise = listaDePublicacionesDe xs u


-- EJERCICIO 7 -> Devuelve una lista de las publicaciones que le gustaron al usuario ingresado (dentro de la red).
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (us,rs,ps) u = eliminaRepetidos (tusLikes (publicaciones (us,rs,ps)) u)

-- Funciones Auxiliares

tusLikes :: [Publicacion] -> Usuario -> [Publicacion] -- Chequea recursivamente la lista de publicaciones de la red ingreada, y cuando el usuario ingresado pertenezca a los likes de alguna publicacion, estas seran concatenadas creando asi una lista de publicaciones.
tusLikes [] _ = []
tusLikes (x:xs) u | null (x:xs) = [x]
                  | pertenece u (likesDePublicacion x) = x : tusLikes xs u
                  | otherwise = tusLikes xs u


-- EJERCICIO 8 -> Retorna True si ambos usuarios no le dieron like a ninguna publicación. En otros casos, compara la longitud de la lista de publicaciones que les gustan a ambos usuarios y verifica si todos los elementos de la primera lista están contenidos en la segunda. Si se cumplen ambas condiciones, retorna True; sino, retorna False.
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones (us,rs,ps) u1 u2 | longitud (publicacionesQueLeGustanA (us,rs,ps) u1) == 0 && longitud (publicacionesQueLeGustanA (us,rs,ps) u2) == 0 = True
                                                 | longitud (publicacionesQueLeGustanA (us,rs,ps) u1) == longitud (publicacionesQueLeGustanA (us,rs,ps) u2) && estaContenida (publicacionesQueLeGustanA (us,rs,ps) u1) (publicacionesQueLeGustanA (us,rs,ps) u2) = True
                                                 | otherwise = False


-- EJERCICIO 9 -> Evalúa "algunoDioLikeATodas" con los parámetros: la red, la lista de usuarios (sin el usuario ingresado) y las publicaciones del usuario ingresado. Retorna True si hay un usuario en la red que le dio like a todas sus publicaciones; sino, retorna False.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel (us,rs,ps) u = algunoDioLikeATodas (us,rs,ps) (quitar u (usuarios (us,rs,ps))) (publicacionesDe (us,rs,ps) u)

-- Funciones Auxiliares
-- Recorre la lista de usuarios y la lista de publicaciones y verifica si alguna de las publicaciones se encuentra dentro de la lista de publicaciones que le gustan a cada usuario de la lista. Devuelve "True" si encuentra alguna coincidencia, sino sigue verificando con todos los usuarios de la lista hasta llegar al final. Si no encuentra coincidencias, devuelve False.
algunoDioLikeATodas :: RedSocial -> [Usuario] -> [Publicacion] -> Bool
algunoDioLikeATodas (_,_,_) [] _ = False
algunoDioLikeATodas (us,rs,ps) _ [] = False
algunoDioLikeATodas (us,rs,ps) (x:xs) (y:ys) | estaContenida (y:ys) (publicacionesQueLeGustanA (us,rs,ps) x) = True
                                             | otherwise = algunoDioLikeATodas (us,rs,ps) xs (y:ys)

 
-- EJERCICIO 10 -> Devuelve True cuando el primer usuario pertenece a la lista de amigos del segundo o cuando el segundo usuario pertenece a la lista de relaciones directas e indirectas del primero (explicación más abajo), en otro caso, devuelve False
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos (us,rs,ps) u1 u2 | (amigosDe (us,rs,ps) u1) == [] || (amigosDe (us,rs,ps) u2) == [] = False
                                         | pertenece u1 (amigosDe (us,rs,ps) u2) = True
                                         | pertenece u2 (amistadDirectaEIndirecta (us,rs,ps) u1 (amigosDe (us,rs,ps) u1)) = True
                                         | otherwise = False   
 
-- Funciones Auxiliares

-- Devuelve una lista con los usuarios de la lista ingresada, con los amigos de esos usuarios y con los amigos de esos amigos, de ser necesario
amistadDirectaEIndirecta :: RedSocial -> Usuario -> [Usuario] -> [Usuario]
amistadDirectaEIndirecta (us,rs,ps) u1 [] = []
amistadDirectaEIndirecta (us,rs,ps) u1 (x:xs) = [x] ++ (amigosDeAmigos (us,rs,ps) (amigosDe (us,rs,ps) x) (u1:x:[])) ++ amistadDirectaEIndirecta (us,rs,ps) u1 xs 

-- Devuelve una lista con los amigos de los usuarios de la primera lista ingresada y con los amigos de esos amigos (similar a la función anterior)
-- Cuando hace recursión para agregar a los amigos de los usuarios, agrega a la segunda lista ingresada a todos los usuarios cuyos amigos ya fueron concatenados y los elimina de la primera lista para evitar un bucle infinito
amigosDeAmigos :: RedSocial -> [Usuario] -> [Usuario] -> [Usuario]
amigosDeAmigos (us,rs,ps) [] ys = []
amigosDeAmigos (us,rs,ps) (x:xs) ys = [x] ++ (amigosDeAmigos (us,rs,ps) (quitarLista ys (amigosDe (us,rs,ps) x)) (x:ys)) ++ amigosDeAmigos (us,rs,ps) xs ys
 where quitarLista (y:ys) [] = []
       quitarLista [] (z:zs) = (z:zs)
       quitarLista (y:ys) (z:zs) | (y:ys) == [y] = quitar y (z:zs)
                                 | otherwise = quitarLista ys (quitar y (z:zs))


