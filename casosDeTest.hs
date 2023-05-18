{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module TestDeMisFunciones where

import Test.HUnit
import Solucion


runCatedra = runTestTT testcatedra

testcatedra = test [
    " nombresDeUsuarios 1" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],

    " amigosDe 1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4],

    " cantidadDeAmigos 1" ~: (cantidadDeAmigos redA usuario1) ~?= 2,

    " usuarioConMasAmigos 1" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],

    " estaRobertoCarlos 1" ~: (estaRobertoCarlos redA) ~?= False,

    " publicacionesDe 1" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],

    " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],

    " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,

    " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,

    " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True,

    " existeSecuenciaDeAmigos 2" ~: (existeSecuenciaDeAmigos redC usuario1 usuario8) ~?= True
    ]

run1= runTestTT testdeListadeNombresdeUsuarios
testdeListadeNombresdeUsuarios = test [   
    "Caso 1: Con un usuario" ~: (nombresDeUsuarios redD) ~?= ["Pedro"], 
    "Caso 2: Con más de un usuario" ~: (nombresDeUsuarios redC) ~?= ["Juan","Natalia","Pedro","Mariela","Ricardo"],
    "Caso 3: Sin usuarios" ~: (nombresDeUsuarios redVacia) ~?= []
    ]

run2= runTestTT testdeamigosDe
testdeamigosDe = test [
    
    "Caso 1: Usuario sin ningun amigo en una red social con amistades " ~: (amigosDe redB usuario5) ~?= [],
    "Caso 2: Usuario sin ningun amigo en una red social sin amistades" ~: (amigosDe redE usuario6) ~?= [], 
    "Caso 3: Usuario sin amigos en una red con solo un usuario" ~: (amigosDe redD usuario3 ) ~?= [],
    "Caso 4: Usuario con un amigo" ~: (amigosDe redB usuario3) ~?= [usuario2],
    "Caso 5: Usuario con más de un amigo" ~: (amigosDe redA usuario1) ~?= [usuario2,usuario4],
    "Caso 6: Usuario con amigo del mismo nombre" ~: (amigosDe redC usuario5) ~?= [usuario2, usuario4, usuario6],
    "Caso 7: Usuario con amigos del mismo nombre" ~: (amigosDe redF usuario5) ~?= [usuario9, usuario11, usuario2]
    ]

run3= runTestTT testcantidadDeAmigos
testcantidadDeAmigos = test [
    "Caso 1: Sin amigos en una red con usuarios" ~: (cantidadDeAmigos redE usuario6 ) ~?= 0,
    "Caso 2: Sin amigos en una red con solo un usuario" ~: (cantidadDeAmigos redD usuario3)  ~?= 0,
    "Caso 3: Con un amigo" ~: (cantidadDeAmigos redB usuario3) ~?= 1,
    "Caso 4: Con más de un amigo" ~: (cantidadDeAmigos redC usuario2) ~?= 4
    ]

run4= runTestTT testusuarioConMasAmigos 
testusuarioConMasAmigos = test [
    
    "Caso 1: Con la misma cantidad de amigos que otro usuario" ~: (usuarioConMasAmigos redC) ~?= usuario4,
    "Caso 2: Con la misma cantidad de amigos que otro usuario, y en la que su cantidad es 0" ~: (usuarioConMasAmigos redG) ~?= usuario7,
    "Caso 3: Usuario con la mayor cantidad de amigos" ~: (usuarioConMasAmigos redF) ~?= usuario9
    ]


run5=runTestTT testestaRobertoCarlos
testestaRobertoCarlos = test [
    "Caso 1: Usuario con menos de 10 amigos en una red social sin amistades"~: (estaRobertoCarlos redE ) ~?= False,
    "Caso 2: Usuario con menos de 10 amigos en una red social con amistades"  ~: (estaRobertoCarlos redB ) ~?= False,
    "Caso 3: Usuario con más de 10 amigos" ~: (estaRobertoCarlos redF ) ~?= True
    ]

run6=runTestTT testpublicacionesDe
testpublicacionesDe = test [
    "Caso 1: Usuario sin ninguna publicacion en una red sin publicaciones" ~: (publicacionesDe redG usuario12 ) ~?= [],
    "Caso 2: Usuario sin ninguna publicacion en una red social con publicaciones" ~: (publicacionesDe redB usuario2) ~?= [],
    "Caso 3: Usuario con una publicacion" ~: (publicacionesDe redE usuario4)~?= [(usuario4, "I am Alice. Not", [usuario1, usuario2])],
    "Caso 4: Usuario con más de una publicacion" ~: (publicacionesDe redB usuario1) ~?= [(usuario1,"Este es mi tercer post",[usuario2,usuario5]),(usuario1,"Este es mi cuarto post",[]),(usuario1,"Este es como mi quinto post",[usuario5])]
    ]

run7= runTestTT testPublicacionesQueLeGustanA
testPublicacionesQueLeGustanA= test[
    "Caso 1: Usuario que no le gusta ninguna publicacion en una red con publicaciones" ~: (publicacionesQueLeGustanA redA usuario3) ~?= [],
    "Caso 2: Usuario que no le gusta ninguna publicacion en una red sin publicaciones" ~: (publicacionesQueLeGustanA redD usuario7) ~?= [],
    "Caso 3: Usuario que le gusta una publicacion" ~: (publicacionesQueLeGustanA redF usuario5) ~?= [(usuario1, "Este es como mi quinto post", [usuario5])],
    "Caso 4: Usuario que le gusta más de una publicacion" ~:(publicacionesQueLeGustanA redA usuario4) ~?= [(usuario1, "Este es mi primer post", [usuario2, usuario4]),(usuario1, "Este es mi segundo post", [usuario4]),(usuario2, "Hello World", [usuario4]),(usuario2, "Good Bye World", [usuario1, usuario4])]
    ]
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

run8= runTestTT testlesGustanLasMismasPublicaciones
testlesGustanLasMismasPublicaciones = test[
    "Caso 1: Dos usuarios que no le dieron like a ninguna publicacion" ~: (lesGustanLasMismasPublicaciones redF (usuario3) (usuario7)) ~?= True,
    "Caso 2: Dos usuarios que le dieron like a las mismas publicaciones y a la misma cantidad" ~: (lesGustanLasMismasPublicaciones redH (usuario5) (usuario2)) ~?= True,
    "Caso 3: Dos usuarios que le dieron like a la misma cantidad de publicaciones pero comparten solo una" ~: (lesGustanLasMismasPublicaciones redB (usuario2) (usuario5)) ~?= False,
    "Caso 4: Un usuario le dio like a las mismas publicaciones que otro, pero a uno le gusto una publicacion mas" ~: (lesGustanLasMismasPublicaciones redH (usuario7) (usuario6)) ~?= False,
    "Caso 5: Se ingresa dos veces el mismo usuario" ~: (lesGustanLasMismasPublicaciones redH (usuario4) (usuario4)) ~?= True
    ]

--run9 = runTestTT testtieneUnSeguidorFiel
--testtieneUnSeguidorFiel = test [

--]

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Ricardo") -- añadido para el ej10
usuario7 = (7, "Armando") -- añadido para el ej10
usuario8 = (8, "Alberto") -- añadido para el ej10
usuario9 = (9, "Lucia")
usuario10 = (10, "Rocio")
usuario11= (11, "Natalia")
usuario12= (12, "Julian")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_9 = (usuario1, usuario9)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion2_5 = (usuario2, usuario5)
relacion2_9 = (usuario9, usuario2)
relacion3_4 = (usuario4, usuario3)
relacion4_5 = (usuario4, usuario5) -- agregada para probar transitividad
relacion4_2 = (usuario4, usuario2)
relacion5_6 = (usuario5, usuario6) -- agregada para probar transitividad x2
relacion5_9 = (usuario9, usuario5)
relacion6_7 = (usuario6, usuario7) -- agregada para probar transitividad x3
relacion6_9 = (usuario9, usuario6) 
relacion7_8 = (usuario7, usuario8) -- agregada para probar transitividad x4
relacion7_9 = (usuario9, usuario7)
relacion8_9 = (usuario9, usuario8)
relacion9_3 = (usuario9, usuario3)
relacion9_4 = (usuario4, usuario9)
relacion10_9 =(usuario10, usuario9)
relacion11_9 = (usuario11, usuario9)
relacion12_9 = (usuario9, usuario12)
relacion11_5 = (usuario5, usuario11)



publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])
publicacion2_3 = (usuario2, "Que onda", [usuario4, usuario1]) -- agregada recien

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])
publicacion3_4 = (usuario3, "Allahu Akbar", [usuario6, usuario7])
publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])
publicacion4_4 = (usuario2, "Francia Segundo", [usuario4, usuario1]) -- agregada recien
publicacion4_5 = (usuario4, "Quien engaño a Roger Rabbit", [usuario7]) -- agregada recien


publicacion6_1 = (usuario6, "Apruebenme", [usuario6, usuario7, usuario3]) -- agregada recien


-- redA
usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

-- redB
usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

--redC 
relacionesC = [relacion1_2, relacion2_3, relacion2_5, relacion3_4, relacion4_2, relacion4_5, relacion5_6, relacion6_7, relacion7_8,relacion9_4]
usuariosC = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9]
publicacionesC = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redC = (usuariosC, relacionesC, publicacionesC)

--redVacia
usuariosvacios = []
relacionesvacias = []
publicacionesvacias = []
redVacia = (usuariosvacios, relacionesvacias, publicacionesvacias)

--redD (Con solo un usuario)
usuariosConSoloUnUsuario = [usuario3]
relacionesDeUnUsuario= []
publicacionesDeUnUsuario = []
redD = (usuariosConSoloUnUsuario, relacionesDeUnUsuario, publicacionesDeUnUsuario )

--redE (Sin relaciones de amistad)
relacionesE = []
usuariosE = [usuario2, usuario4, usuario5, usuario6]
publicacionesE = [publicacion2_1,publicacion4_1]
redE= (usuariosE, relacionesE, publicacionesE)

--redF (Con muchas relaciones de amistad)
relacionesF = [relacion1_9,relacion2_9, relacion9_3, relacion9_4, relacion5_9, relacion6_9, relacion7_9, relacion8_9, relacion10_9, relacion11_9, relacion12_9, relacion11_5, relacion2_5]
usuariosF= [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
publicacionesF = [publicacion1_1, publicacion1_5]-- no olvidar agregar publis asi lo uso para el ejercicio de publis 6 o 7!!
redF = (usuariosF, relacionesF, publicacionesF)

--redG (Con solo dos usuarios)
usuariosG =[usuario7, usuario12]
relacionesG = []
publicacionesG = []--
redG = (usuariosG, relacionesG, publicacionesG)

-- redH ()
relacionesH = [relacion3_4]
usuariosH = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7]
publicacionesH = [publicacion2_3, publicacion3_4, publicacion4_4, publicacion4_5, publicacion6_1]
redH = (usuariosH, relacionesH, publicacionesH)
