import Data.List 
import System.IO 
import Data.Time.Calendar.OrdinalDate
import Data.Time

-- Contar las comas de un string

--import System.IO
--import qualified Data.Text    as Text
--import qualified Data.Text.IO as Text

-- Funciones auxiliares

enesimo (milista,x) = 
    if x == 0 then head milista 
    else enesimo( tail milista,x-1);

parseFile fp = do
  contents <- readFile fp
  let (s : ss) = lines contents
  return (s : ss)



largo :: [a] -> Int
largo [] = 0
largo (x: xs) = 1 + largo xs


separarPorComas2 (cadena, temp)
 | cadena == ""
 = [temp]
 | head cadena == head ","
 = temp : separarPorComas2 (tail cadena, "")
 | otherwise 
 = separarPorComas2 (tail cadena, temp ++ [head cadena])


functionToList (function, list) = [function x | x <- list]

separarPorComas lista = separarPorComas2 (lista,"")


separarPorSaltodeLine (cadena, temp)
 | cadena == ""
 = [temp]
 | head cadena == head "\n"
 = temp : separarPorSaltodeLine (tail cadena, "")
 | otherwise 
 = separarPorSaltodeLine (tail cadena, temp ++ [head cadena])

leerInfoHotel = do
    content <- readFile "InfoHotel.txt"
    let contenido = separarPorComas content
    --print contenido
    print "-=[ Informacion del hotel ]=-"
    print ("Nombre del hotel: " ++ enesimo(contenido,0))
    print ("Cedula juridica: " ++ enesimo(contenido,1))
    print ("Sitio Web: " ++ enesimo(contenido,2))
    print ("Telefono: " ++ enesimo(contenido,3))
    print ("Pais: " ++ enesimo(contenido,4))
    print ("Provincia: " ++ enesimo(contenido,5))

guardarInfoHotel = do
    print "Ingrese el nombre del hotel: "
    nombre <- getLine
    print "Ingrese la cedula juridica: "
    cedula <- getLine
    print "Ingrese la direccion del sitio web: "
    sitio <- getLine
    print "Ingrese el telefono del hotel: "
    tel <- getLine
    print "Ingrese el pais donde se encuentra el hotel: "
    pais <- getLine
    print "Ingrese la provincia donde se encuentra el hotel: "
    provincia <- getLine
    let cadena = nombre ++ "," ++ cedula ++ "," ++ sitio ++ "," ++ tel ++ "," ++ pais ++ "," ++ provincia
    writeFile "InfoHotel.txt" cadena
    print "Datos Ingresados con exito"


cargarTipoHabitaciones cantxTipo tarifas reserv fact = do
    print "Ingrese la ruta del archivo de habitaciones"
    ruta <- getLine
    content <- readFile ruta
    let contenido = separarPorSaltodeLine (content,"")
    let contenido2 =  functionToList (separarPorComas,contenido)
    print "Tipo de habitaciones cargado correctamente"
    admin contenido2 cantxTipo tarifas reserv fact



asignarHabitacionexTipo (hab,cantxTipo,tarifas ,reser, fact, i, respuesta )= do 
    let tipo = enesimo(enesimo(hab,i),0)
    let g = "Ingrese la cantidad para el tipo "  ++ tipo
    print g        
    cant <- getLine    
    if i == largo (hab) - 1 then 
        admin hab cantxTipo tarifas  reser fact
    else asignarHabitacionexTipo (hab,cantxTipo,tarifas,reser ,fact ,i+1, respuesta ++ [[tipo]++ [cant]])

cargaTarifas hab cantxTipo  reser fact = do 
    print "Ingrese la ruta del archivo de tarifas"
    ruta <- getLine
    content <- readFile ruta
    let contenido = separarPorSaltodeLine (content,"")
    let contenido2 =  functionToList (separarPorComas,contenido)
    print contenido2
    admin hab cantxTipo contenido2 reser fact    


-- Verifica si un elemento de la lista existe en el la lista indicada
-- Entradas: Lista y un elemento
-- Salida: Boolean
elemento (x:xs) a = if a == x then True else elemento xs a
elemento [] a = False

-- Validar si una habitacion existe
-- Entradas: Arreglo de habitaciones, Nombre de la habitación y Indice = 0
-- Salida: Boleean

validarHabitacionExiste (habArreglo, tipoHabitacion,i) = do
  if elemento(enesimo(enesimo(habArreglo,i),0)) tipoHabitacion then True
  else
    if i == largo(habArreglo) then False
    else
      validarHabitacionExiste (habArreglo, tipoHabitacion, i+1)


-- Validar si una cantidad es valida para determinado tipo de habitacion
-- Entradas: Arreglo de habitaciones, Nombre de la habitación, Cantidad a preguntar y Indice = 0
-- Salida: Boleean
validarCantidadTipoHabitacion (habArreglo, tipoHabitacion, cantidad, i) = do
  if cantidad <= enesimo(enesimo(habArreglo,i),2) && elemento(enesimo(enesimo(habArreglo,i),0)) tipoHabitacion then True
  else
    if i == largo(habArreglo) then False
      else
        validarCantidadTipoHabitacion (habArreglo, tipoHabitacion, cantidad, i+1)


calcularMonto (tarifaArreglo, tipoPersona, anno, mes, dia, cantidadPersonas, monto) = do
  let f = mondayStartWeek(fromGregorian anno mes dia)
  let fecha = snd f
  if tipoPersona == "Adulto" && (mes >= 4) && (mes <= 10) then do                 --TEMPORADA BAJA
      if (fecha == 5) || (fecha == 6) then do --Si es viernes o sabado
      let tarifa = read (enesimo(enesimo(tarifaArreglo,3-1),1)) :: Int
      let monto = tarifa * cantidadPersonas                                     -- 3: Verde fin de semana adulto: de viernes a sábado, de abril a octubre
      monto
      else do
        let tarifa = read(enesimo(enesimo(tarifaArreglo,1-1),1)) :: Int 
        let monto = tarifa * cantidadPersonas                                   -- 1: Verde entre semana adulto: de domingo a jueves, de abril a octubre
        monto
  else 
    if tipoPersona == "Adulto" && (mes >= 11) && (mes <= 3) then do                   --TEMPORADA ALTA
      if (fecha == 5) || (fecha == 6) then do  --Si es viernes o sabado
        let tarifa = read(enesimo(enesimo(tarifaArreglo,7-1),1)) :: Int 
        let monto = tarifa * cantidadPersonas                                   -- 7: Alta fin de semana adulto: de viernes a sábado, de noviembre a marzo
        monto
      else do
        let tarifa = read(enesimo(enesimo(tarifaArreglo,5-1),1)) :: Int 
        let monto = tarifa * cantidadPersonas                                   -- 5: Alta entre semana adulto: domingo a jueves, de noviembre a marzo
        monto
    else
      if tipoPersona == "Nino" && (mes >= 4) && (mes <= 10) then do                   --TEMPORADA BAJA
        if (fecha == 5) || (fecha == 6) then do --Si es viernes o sabado
           let tarifa = read(enesimo(enesimo(tarifaArreglo,4-1),1)) :: Int  
           let monto = tarifa * cantidadPersonas                                 -- 4: Verde fin de semana niño: de viernes a sábado, de abril a octubre
           monto
        else do
          let tarifa = read(enesimo(enesimo(tarifaArreglo,2-1),1)) 
          let monto = tarifa * cantidadPersonas                                  -- 2: Verde entre semana niño: de domingo a jueves, de abril a octubre
          monto
      else do                                                                   --TEMPORADA ALTA
        if (fecha == 5) || (fecha == 6) then do --Si es viernes o sabado
          let tarifa = read(enesimo(enesimo(tarifaArreglo,8-1),1)) :: Int  
          let monto = tarifa * cantidadPersonas                                 -- 8: Alta fin de semana niño: de viernes a sábado, de noviembre a marzo
          monto
        else do
          let tarifa = read(enesimo(enesimo(tarifaArreglo,6-1),1)) :: Int 
          let monto = tarifa * cantidadPersonas                                 -- 6: Alta entre semana niño: domingo a jueves, de noviembre a marzo
          monto

calcularTarifaDias
  (tarifaArreglo, monto,
   tipoPersona, anno, cant, diaInicial, diaFinal, mesInicial, mesFinal)
  | (diaInicial == diaFinal) && (mesInicial == mesFinal) = monto
  | diaInicial == 31
  = calcularTarifaDias
      (tarifaArreglo, monto +
       calcularMonto
         (tarifaArreglo, tipoPersona, anno, mesInicial, diaInicial, cant, 
          0), 
       tipoPersona, anno, cant, 1, diaFinal, mesInicial + 1, mesFinal)
  | otherwise
  = calcularTarifaDias
      (tarifaArreglo,monto + 
       calcularMonto
         (tarifaArreglo, tipoPersona, anno, mesInicial, diaInicial, cant, 
          0), tipoPersona, anno, cant, diaInicial + 1, diaFinal, mesInicial, mesFinal ) 



imprime l = do 
        print ""
        return l 

-- main        
main = 
    menu [] [] [] [] [] 


menu tipoHabitaciones cantidadXtipo tarifas reservaciones facturas  = do 
    putStr "Ingrese una opcion \n"
    putStr "1.Opciones Administrativas \n"
    putStr "2.Opciones generales \n"
    putStr "3.Salir \n"
    first <- getLine
    if first== [head "1"] then                             
        admin tipoHabitaciones cantidadXtipo tarifas reservaciones facturas
    else if  first== [head "2"] then                             
        generales tipoHabitaciones cantidadXtipo tarifas reservaciones facturas

    else menu tipoHabitaciones cantidadXtipo tarifas reservaciones facturas
generales tipoHabitaciones cantidadXtipo tarifas reservaciones facturas = do 
    putStr "Ingrese una opcion \n"
    putStr "1.Reservacion \n"
    putStr "2.Factura \n"
    first <- getLine
    if first== [head "1"] then                             
        reservar tipoHabitaciones cantidadXtipo tarifas reservaciones facturas
    else 
        generales tipoHabitaciones cantidadXtipo tarifas reservaciones facturas    






reservar tipoHabitaciones cantidadXtipo tarifas reservaciones facturas = do 
    putStr "Reservacion \n"
    putStr "Ingrese la fecha de entrada \n"
    entrada <- getLine
    putStr "Ingrese la fecha de salida \n"
    salida <- getLine
    putStr "Ingrese la cantidad de adultos \n"
    adultos <- getLine 
    putStr "Ingrese la cantidad de niños\n"
    ninnos <- getLine  
    putStr "Nombre de la persona que reserva: \n"
    nombre <- getLine
    putStr nombre
    -- let habitaciones = getHabitaciones cantidadXtipo




admin hab cantxTipo tarifas reser fact   = do 
                          putStr "Ingrese una opcion \n"
                          putStr "1.Información de hotel \n"
                          putStr "2.Cargar tipo de habitacion  \n" 
                          putStr "3.Asignar cantidad de habitaciones por tipo \n"
                          putStr "4.Carga de tarifas \n"
                          putStr "5.Consultar reservaciones \n"
                          putStr "6.Consulta de factura \n"
                          putStr "7.Estadísticas de ocupación \n"
                          putStr "3.Salir \n"
                          first <- getLine
                          if first== [head "2"] then                             
                            cargarTipoHabitaciones cantxTipo tarifas reser fact
                          else if first== [head "3"] then                             
                            asignarHabitacionexTipo(hab,cantxTipo,tarifas,reser,fact,0,[])
                          else if first== [head "4"] then                             
                            cargaTarifas hab cantxTipo reser fact
                          else 
                            admin hab cantxTipo tarifas  reser fact
