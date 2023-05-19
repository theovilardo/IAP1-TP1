{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module TestDeMisFunciones where

import Test.HUnit
import Solucion

main = runTestTT runAll
runAll = test [testcatedra, testdeListadeNombresdeUsuarios, testdeamigosDe, testcantidadDeAmigos, testusuarioConMasAmigos, testestaRobertoCarlos, testpublicacionesDe, testPublicacionesQueLeGustanA, testlesGustanLasMismasPublicaciones, testtieneUnSeguidorFiel, testexisteSecuenciaDeAmigos]

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
    "Caso 2: Con más de un usuario" ~: (nombresDeUsuarios redC) ~?= ["Juan","Pedro","Mariela","Natalia","Ricardo","Armando","Alberto","Lucia"], --["Juan","Natalia","Pedro","Mariela","Ricardo"]
    "Caso 3: Sin usuarios" ~: (nombresDeUsuarios redVacia) ~?= [],
    "Caso 4: Con varios usuarios con el mismo nombre" ~: (nombresDeUsuarios redF) ~?= ["Juan","Pedro","Mariela","Armando","Alberto","Lucia","Natalia","Julian","Ricardo"] 
    ]

run2= runTestTT testdeamigosDe
testdeamigosDe = test [
    
    "Caso 1: Usuario sin ningun amigo en una red social con amistades " ~: (amigosDe redB (5, "Natalia")) ~?= [],
    "Caso 2: Usuario sin ningun amigo en una red social sin amistades" ~: (amigosDe redE (6, "Ricardo") ) ~?= [], 
    "Caso 3: Usuario sin amigos en una red con solo un usuario" ~: (amigosDe redD (3, "Pedro") ) ~?= [],
    "Caso 4: Usuario con un amigo" ~: (amigosDe redB (3, "Pedro")) ~?= [(2, "Natalia")],
    "Caso 5: Usuario con más de un amigo" ~: (amigosDe redA (1, "Juan")) ~?= [(2, "Natalia"),(4, "Mariela")],
    "Caso 6: Usuario con amigo del mismo nombre" ~: (amigosDe redC (5, "Natalia")) ~?= [(2,"Natalia"),(4,"Mariela"),(6,"Ricardo")],
    "Caso 7: Usuario con amigos del mismo nombre" ~: (amigosDe redF (5, "Natalia")) ~?= [(9,"Lucia"),(11,"Natalia"),(2,"Natalia")]
    ]

run3= runTestTT testcantidadDeAmigos
testcantidadDeAmigos = test [
    "Caso 1: Sin amigos en una red con usuarios" ~: (cantidadDeAmigos redE (6, "Ricardo") ) ~?= 0,
    "Caso 2: Sin amigos en una red con solo un usuario" ~: (cantidadDeAmigos redD (3, "Pedro"))  ~?= 0,
    "Caso 3: Con un amigo" ~: (cantidadDeAmigos redB (3, "Pedro")) ~?= 1,
    "Caso 4: Con más de un amigo" ~: (cantidadDeAmigos redC (2, "Natalia")) ~?= 4
    ]

run4= runTestTT testusuarioConMasAmigos 
testusuarioConMasAmigos = test [
    
    "Caso 1: Con la misma cantidad de amigos que otro usuario" ~: (usuarioConMasAmigos redC) ~?= (4, "Mariela"),
    "Caso 2: Con la misma cantidad de amigos que otro usuario, y en la que su cantidad es 0" ~: (usuarioConMasAmigos redG) ~?= (7, "Armando"),
    "Caso 3: Usuario con la mayor cantidad de amigos" ~: (usuarioConMasAmigos redF) ~?= (9,"Lucia")
    ]


run5=runTestTT testestaRobertoCarlos
testestaRobertoCarlos = test [
    "Caso 1: Usuario con menos de 10 amigos en una red social sin amistades"~: (estaRobertoCarlos redE ) ~?= False,
    "Caso 2: Usuario con menos de 10 amigos en una red social con amistades"  ~: (estaRobertoCarlos redB ) ~?= False,
    "Caso 3: Usuario con más de 10 amigos" ~: (estaRobertoCarlos redF ) ~?= True
    ]

run6=runTestTT testpublicacionesDe
testpublicacionesDe = test [
    "Caso 1: Usuario sin ninguna publicacion en una red sin publicaciones" ~: (publicacionesDe redG (12, "Julian") ) ~?= [],
    "Caso 2: Usuario sin ninguna publicacion en una red social con publicaciones" ~: (publicacionesDe redB (2, "Natalia")) ~?= [],
    "Caso 3: Usuario con una publicacion" ~: (publicacionesDe redE (4,"Mariela"))~?= [(usuario4, "I am Alice. Not", [usuario1, usuario2])],
    "Caso 4: Usuario con más de una publicacion" ~: (publicacionesDe redB (1, "Juan")) ~?= [((1,"Juan"),"Este es mi tercer post",[(2,"Natalia"),(5,"Natalia")]),((1,"Juan"),"Este es mi cuarto post",[]),((1,"Juan"),"Este es como mi quinto post",[(5,"Natalia")])]
    ]

run7= runTestTT testPublicacionesQueLeGustanA
testPublicacionesQueLeGustanA= test[
    "Caso 1: Usuario que no le gusta ninguna publicacion en una red con publicaciones" ~: (publicacionesQueLeGustanA redA (3,"Pedro")) ~?= [],
    "Caso 2: Usuario que no le gusta ninguna publicacion en una red sin publicaciones" ~: (publicacionesQueLeGustanA redD (7,"Armando")) ~?= [],
    "Caso 3: Usuario que le gusta una publicacion" ~: (publicacionesQueLeGustanA redX usuario11) ~?= [(usuario13, "bastaaaaaaaa", [usuario11])],
    "Caso 4: Usuario que le gusta más de una publicacion" ~:(publicacionesQueLeGustanA redA (4, "Mariela")) ~?= [(usuario1, "Este es mi primer post", [usuario2, usuario4]),(usuario1, "Este es mi segundo post", [usuario4]),(usuario2, "Hello World", [usuario4]),(usuario2, "Good Bye World", [usuario1, usuario4])]
    ]


run8= runTestTT testlesGustanLasMismasPublicaciones
testlesGustanLasMismasPublicaciones = test[
    "Caso 1: Dos usuarios que no le dieron like a ninguna publicacion" ~: (lesGustanLasMismasPublicaciones redF (usuario3) (usuario7)) ~?= True, 
    "Caso 2: Dos usuarios que le dieron like solo a una publicacion y es exactamente la misma " ~: (lesGustanLasMismasPublicaciones redB (usuario2) (usuario5)) ~?= False,--debería dar TRUE
    "Caso 3: Dos usuarios que le dieron like a varias publicaciones y son las mismas" ~: (lesGustanLasMismasPublicaciones redH (usuario5) (usuario2)) ~?= True,
    "Caso 4: Un usuario le dio like a las mismas publicaciones que otro, pero a uno le gusto una publicacion mas" ~: (lesGustanLasMismasPublicaciones redH (usuario7) (usuario6)) ~?= False,
    "Caso 5: Se ingresa dos veces el mismo usuario" ~: (lesGustanLasMismasPublicaciones redH (usuario4) (usuario4)) ~?= True
    ]

run9 = runTestTT testtieneUnSeguidorFiel
testtieneUnSeguidorFiel = test [
    "Caso 1: Usuario que solo tiene autolikes" ~: (tieneUnSeguidorFiel redF usuario10) ~?= False,
    "Caso 2: Algún usuario le dio like a todas las publicaciones del usuario ingresado" ~: (tieneUnSeguidorFiel redF usuario12) ~?= True,
    "Caso 3: Un usuario le dio like a algunas de las publicaciones del usuario ingresado" ~: (tieneUnSeguidorFiel redF usuario1) ~?= False,
    "Caso 4: El usuario ingresado no tiene likes en ninguna de sus publicaciones" ~: (tieneUnSeguidorFiel redF usuario11) ~?= False
    ]

run10 = runTestTT testexisteSecuenciaDeAmigos
testexisteSecuenciaDeAmigos = test [
    "Caso 1: Alguno de los usuarios ingresados no tiene amigos" ~: (existeSecuenciaDeAmigos redX usuario2 usuario1) ~?= False,
    "Caso 2: Los usuarios ingresados tienen un amigo en comun" ~: (existeSecuenciaDeAmigos redC usuario1 usuario3) ~?= True,
    "Caso 3: Existe una relacion indirecta (por transitividad) entre los usuarios ingresados" ~: (existeSecuenciaDeAmigos redC usuario1 usuario9) ~?= True,
    "Caso 4: No existe relacion entre los usuarios ingresados (no existe la cadena)" ~: (existeSecuenciaDeAmigos redX usuario3 usuario10) ~?= False,
    "Caso 5: No existen relaciones en la red social ingresada" ~: (existeSecuenciaDeAmigos redG usuario12 usuario7) ~?= False
    ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Ricardo") 
usuario7 = (7, "Armando") 
usuario8 = (8, "Alberto") 
usuario9 = (9, "Lucia")
usuario10 = (10, "Natalia")
usuario11 = (11, "Natalia")
usuario12 = (12, "Julian")
usuario13 = (13, "Ricardo")
usuario14 = (14, "Ricardo")
usuario15 = (15, "Paula")


relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_9 = (usuario1, usuario9)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion2_5 = (usuario2, usuario5)
relacion2_9 = (usuario9, usuario2)
relacion3_4 = (usuario4, usuario3)
relacion4_5 = (usuario4, usuario5) 
relacion4_2 = (usuario4, usuario2)
relacion5_6 = (usuario5, usuario6) 
relacion5_9 = (usuario9, usuario5)
relacion6_7 = (usuario6, usuario7)
relacion6_9 = (usuario9, usuario6) 
relacion7_8 = (usuario7, usuario8) 
relacion7_9 = (usuario9, usuario7)
relacion8_9 = (usuario9, usuario8)
relacion9_3 = (usuario9, usuario3)
relacion9_4 = (usuario4, usuario9)
relacion10_9 =(usuario10,usuario9)
relacion10_2 = (usuario10, usuario2)
relacion11_9 = (usuario11,usuario9)
relacion12_9 = (usuario9, usuario12)
relacion11_5 = (usuario5, usuario11)



publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])
publicacion2_3 = (usuario2, "Que onda", [usuario4, usuario1]) 

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])
publicacion3_4 = (usuario3, "Allahu Akbar", [usuario6, usuario7])
publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])
publicacion4_4 = (usuario2, "Francia Segundo", [usuario4, usuario1]) 
publicacion4_5 = (usuario4, "Quien engaño a Roger Rabbit", [usuario7]) 


publicacion6_1 = (usuario6, "Apruebenme", [usuario6, usuario7, usuario3])

publicacion10_1 = (usuario10, "A mi tambien!", [usuario10])
publicacion10_2 = (usuario10, "Me too!", [usuario10])
publicacion10_3 = (usuario10, "Me three!", [usuario10])

publicacion11_1 = (usuario11, "Estamos para el 10!", [])
publicacion11_2 = (usuario11, "Estamos para el 10?", [])
publicacion11_3 = (usuario11, "Estamos para el 4?", [])
publicacion11_4 = (usuario11, "Estamos?", [])

publicacion12_1 = (usuario12, "Me conformo con un 8", [usuario11])
publicacion12_2 = (usuario12, "Me conformo con un 6", [usuario11])
publicacion12_3 = (usuario12, "Me conformo con un 4", [usuario11])

publicacion13_1 = (usuario13, "bastaaaaaaaa", [usuario11])


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
usuariosF= [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12, usuario13, usuario14]
publicacionesF = [publicacion1_1, publicacion1_2, publicacion1_3, publicacion1_4, publicacion1_5, publicacion10_1, publicacion10_2, publicacion10_3, publicacion11_1, publicacion11_2, publicacion11_3, publicacion11_4, publicacion12_1, publicacion12_2, publicacion12_3]
redF = (usuariosF, relacionesF, publicacionesF)

--redG (Con solo dos usuarios)
usuariosG =[usuario7, usuario12]
relacionesG = []
publicacionesG = []
redG = (usuariosG, relacionesG, publicacionesG)

-- redH
relacionesH = [relacion3_4]
usuariosH = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7]
publicacionesH = [publicacion2_3, publicacion3_4, publicacion4_4, publicacion4_5, publicacion6_1]
redH = (usuariosH, relacionesH, publicacionesH)

-- redX
relacionesX = [relacion1_3, relacion3_4, relacion4_5, relacion5_6, relacion6_7, relacion7_8,relacion9_4, relacion10_2]
usuariosX = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario13]
publicacionesX = [publicacion2_3, publicacion3_4, publicacion4_4, publicacion4_5, publicacion6_1, publicacion13_1]
redX = (usuariosX, relacionesX, publicacionesX)
