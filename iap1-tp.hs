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

-- describir qué hace la función: devuelve los nombres de usuario de la red social
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

-- describir qué hace la función: .....
--Le falta función para eliminar los repetidos
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe rd u = listaDeAmigos u (relaciones rd)


listaDeAmigos :: Usuario -> [Relacion] -> [Usuario]
listaDeAmigos u [] = []
listaDeAmigos u (x:xs) | u == fst x = snd x : listaDeAmigos u xs
                       | u == snd x = fst x : listaDeAmigos u xs
                       | otherwise = listaDeAmigos u xs 

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos (us,rs,ps) (id, nombre) = contarAmigos (amigosDe (us,rs,ps) (id, nombre))

contarAmigos :: [Usuario] -> Int
contarAmigos []= 0
contarAmigos (x:xs) = 1+ contarAmigos xs

-- describir qué hace la función: dada la red social, devuelve el usuario con mas amigos
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos (us,rs,ps) = cantidadDeAcu (us,rs,ps) (usuarios (us,rs,ps))

-- ???: DUDA CON LAS REPETICIONES EN CANTIDAD DE AMIGOS

-- Funciones Auxiliares
cantidadDeAcu :: RedSocial -> [Usuario] -> Usuario
cantidadDeAcu rs (x:xs) | null (x:xs) = x
                        | cantidadDeAmigos (rs) x > cantidadDeAmigos (rs) (head xs) = x
                        | otherwise = cantidadDeAcu rs xs

-- Reemplazo de head por las dudas:
cabeza :: [a] -> a
cabeza [] = []
cabeza (x:xs) = x

-- describir qué hace la función: si dentro de la red hay un usuario con mas de un millon de amigos devuelve True
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos (us,rs,ps) | cantidadDeAmigos (us,rs,ps) (usuarioConMasAmigos (us,rs,ps)) > 1000000 = True
                             | otherwise = False

-- describir qué hace la función: devuelve las publicaciones de un usuario dentro de la red social
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (us,rs,ps) u = pubDe (publicaciones (us,rs,ps)) u

-- Funciones Auxiliares

pubDe :: [Publicacion] -> Usuario -> [Publicacion]
pubDe [] _ = []
pubDe (x:xs) u | null (x:xs) = [x]
               | usuarioDePublicacion x == u = x : pubDe xs u
               | otherwise = pubDe xs u

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined
