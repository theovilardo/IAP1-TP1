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

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

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
cabeza [] = []
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
tieneUnSeguidorFiel (us,rs,ps) u = likesPertenecen (us,rs,ps) (usuarios (us,rs,ps)) (publicacionesDe (us,rs,ps) u)

-- Funciones Auxiliares

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

-- describir qué hace la función: EJERCICIO 10 -> Que diablos es esto                 -- Devuelve true si en la red existe una cadena de amistades
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos (us,rs,ps) a b | amigosDe (us,rs,ps) a == amigosDe (us,rs,ps) b = True
                                       | amigosEnComun (us,rs,ps) (amigosDe (us,rs,ps) a) (amigosDe (us,rs,ps) b) = True
                                       | otherwise = False

-- Funciones Auxiliares

concatenaAmistades :: [Usuario] -> [Usuario] -> [Usuario]               -- Su funcion es unir las listas de amigos de cada usuario (sin repetir usuarios) 
concatenaAmistades (x:xs) (y:ys) = eliminaRepetidos ((x:xs) ++ (y:ys))


amigosEnComun :: RedSocial -> [Usuario] -> [Usuario] -> Bool            -- Chequea la cadena de amistades entre los amigos de los usuarios ingresados
amigosEnComun (_,_,_) [] _ = False
amigosEnComun (_,_,_) _ [] = False
amigosEnComun (us,rs,ps) (x:xs) (a:bc) | mismosElementos (x:xs) (a:bc) || mismosElementos (a:bc) (x:xs) = True
                                       | pertenece x (a:bc) = True
                                       | otherwise = (algunoEsAmigo (us,rs,ps) (z:zs)) || (algunoEsAmigoFix (us,rs,ps) z zs)
                                       where (z:zs) = concatenaAmistades (x:xs) (a:bc)


algunoEsAmigoFix :: RedSocial -> Usuario -> [Usuario] -> Bool                 -- version 2, esta debe contemplar todos los casos, en resumen hace que el primer valor de la lista de amistades (la que se genera en concatenaAmistades) se evalua con todos los valores que siguen dentro de la lista
algunoEsAmigoFix (_,_,_) _ [] = False
algunoEsAmigoFix (us,rs,ps) a (x:xs) | sonAmigos (us,rs,ps) a x = True
                                     | otherwise = algunoEsAmigoFix (us,rs,ps) x xs

algunoEsAmigo :: RedSocial -> [Usuario] -> Bool       -- Su funcion es chequear dentro de la lista de amistades de ambos usuarios (la que se crea con concatenaAmistades) si existe alguna relacion, si es asi, devolvera True
algunoEsAmigo (us,rs,ps) [] = False
algunoEsAmigo (us,rs,ps) (x:y:ys) | sonAmigos (us,rs,ps) x y = True
                                  | otherwise = algunoEsAmigo (us,rs,ps) (y:ys)

sonAmigos :: RedSocial -> Usuario -> Usuario -> Bool                                                    -- Chequea si dos usuarios dentro de la misma red son amigos
sonAmigos (us,rs,ps) a b = sonAmigosMutuos (us,rs,ps) (amigosDe (us,rs,ps) a) (amigosDe (us,rs,ps) b)

sonAmigosMutuos :: RedSocial -> [Usuario] -> [Usuario] -> Bool                              -- Mas que nada es una dependencia para sonAmigos, por ahi se puede simplificar
sonAmigosMutuos (_,_,_) [] [] = False
sonAmigosMutuos (_,_,_) [] _ = False
sonAmigosMutuos (_,_,_) _ [] = False
sonAmigosMutuos (us,rs,ps) (x:xs) (y:ys) | pertenece x (y:ys) = True
                                         | otherwise = sonAmigosMutuos (us,rs,ps) xs (y:ys)
