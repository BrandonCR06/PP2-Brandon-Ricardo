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



-- Verifica si un elemento de la lista existe en el la lista indicada
-- Entradas: Lista y un elemento
-- Salida: Boolean
elemento (x:xs) a = if a == x then True else elemento xs a
elemento [] a = False

-- Validar si una habitacion existe
-- Entradas: Arreglo de habitaciones, Nombre de la habitación y Indice = 0
-- Salida: Boleean

validarHabitacionExiste (habArreglo, tipoHabitacion,i) = do
    if i == largo(habArreglo) then False
    else if enesimo(enesimo(habArreglo,i),0)== tipoHabitacion then True  
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


cargarTipoHabitaciones cantxTipo tarifas reserv fact habitaciones = do
    print "Ingrese la ruta del archivo de habitaciones"
    ruta <- getLine
    content <- readFile ruta
    let contenido = separarPorSaltodeLine (content,"")
    let contenido2 =  functionToList (separarPorComas,contenido)
    print "Tipo de habitaciones cargado correctamente"
    print contenido2
    admin contenido2 cantxTipo tarifas reserv fact habitaciones

generarNHabitaciones( cant, idf, hab, tipo,i) = 
    if i== cant then hab 
    else do 
        let  ki = show idf 
        generarNHabitaciones (cant, idf+1, (hab++[[ki]++[tipo]++["noReservada"]]), tipo,i+1)        
-- [["Estandar","des","4"],["Suite","des","4"]]
asignarHabitacionexTipo (hab,cantxTipo,tarifas ,reser, fact, i, respuesta, habitaciones )= do     
    let tipo = enesimo(enesimo(hab,i),0)
    let g = "Ingrese la cantidad para el tipo "  ++ tipo
    print g        
    cant <- getLine    
    let cant1 = read cant :: Int
    let olId = largo habitaciones
    let habi = generarNHabitaciones (cant1, olId, [], tipo,0)    
    if i == largo (hab) -1 then do         
        let c = habitaciones++habi
        print c
        admin hab respuesta tarifas reser fact c
    else asignarHabitacionexTipo (hab,cantxTipo,tarifas,reser ,fact ,i+1, respuesta ++ [[tipo]++ [cant]],habitaciones++habi)

cargaTarifas hab cantxTipo  reser fact habitaciones = do 
    print "Ingrese la ruta del archivo de tarifas"
    ruta <- getLine
    content <- readFile ruta
    let contenido = separarPorSaltodeLine (content,"")
    let contenido2 =  functionToList (separarPorComas,contenido)
    admin hab cantxTipo contenido2 reser fact habitaciones


cargarTarifas hab cantxTipo  reser fact = do 
    print "Ingrese la ruta del archivo de tarifas"
    ruta <- getLine
    content <- readFile ruta
    let contenido = separarPorSaltodeLine (content,"")
    let contenido2 =  functionToList (separarPorComas,contenido)
    return contenido2

imprime l = do 
        print ""
        return l 

-- main        
menuTotal = 
    menu [] [] [] [] [] [] 


menu tipoHabitaciones cantidadXtipo tarifas reservaciones facturas habitaciones = do 
    putStr "Ingrese una opcion \n"
    putStr "1.Opciones Administrativas \n"
    putStr "2.Opciones generales \n"
    putStr "3.Salir \n"
    first <- getLine
    if first== [head "1"] then                             
        admin tipoHabitaciones cantidadXtipo tarifas reservaciones facturas habitaciones
    else if  first== [head "2"] then                             
        generales( tipoHabitaciones, cantidadXtipo, tarifas, reservaciones, facturas,habitaciones)
    else if  first== [head "3"] then   
        print ""
    else menu tipoHabitaciones cantidadXtipo tarifas reservaciones facturas habitaciones 

generales (tipoHabitaciones ,cantidadXtipo ,tarifas ,reservaciones ,facturas,habitaciones )= do 
    putStr "Ingrese una opcion \n"
    putStr "1.Reservacion \n"
    putStr "2.Factura \n"
    putStr "3.Volver \n"
    first <- getLine
    print reservaciones
    if first== [head "1"] then                             
        reservar (tipoHabitaciones, cantidadXtipo, tarifas, reservaciones, facturas,habitaciones, [],0,0,0)
    else if first== [head "3"] then                             
        menu tipoHabitaciones cantidadXtipo tarifas reservaciones facturas habitaciones

    else
        generales (tipoHabitaciones, cantidadXtipo, tarifas, reservaciones, facturas,habitaciones)    




getCurrResId reservaciones  =
    if reservaciones == [] then "0"
    else if largo(reservaciones)==1 then  
        head (head reservaciones) 
    else
        getCurrResId (tail reservaciones)

terminarHab tipoHabitaciones cantidadXtipo tarifas reservaciones facturas newRes i habitaciones cantNinos cantAdultos= do
    putStr "Nombre de la persona que reserva: \n"
    nombre <- getLine
    putStr "Ingrese la fecha de entrada \n"
    entrada <- getLine
    let diaIn = read([head entrada] ++ [head (tail entrada)]) :: Int
    let restante = tail(tail(tail(entrada)))
    let mesIn = read([head restante] ++ [head (tail restante)]) :: Int
    let restante2 = tail(tail(tail(restante)))
    let year = read(restante2) :: Integer

    putStr "Ingrese la fecha de salida \n"
    salida <- getLine
    let diaFin = read([head salida] ++ [head (tail salida)]) :: Int
    let restante3 = tail(tail(tail(salida)))
    let mesFin = read([head restante3] ++ [head (tail restante3)]) :: Int


    let x = show i 
    let id = read (getCurrResId reservaciones )::Int
    let idInt = id+1
    let idStr = show idInt
    let montoAds = calcularTarifaDias (tarifas,0,"Adulto",year,cantAdultos,diaIn,diaFin,mesIn,mesFin)
    let montoKids = calcularTarifaDias (tarifas,0,"Nino",year,cantNinos,diaIn,diaFin,mesIn,mesFin)
    let montoTotal = montoAds+montoKids
    let montoEnString = show montoTotal 

 
    let c  = reservaciones++[[idStr]++[nombre]++[entrada]++[salida]++[x]++["activa"]++["noFacturada"]++[montoEnString ]++newRes]
    let msg =  "Reserva realizadad con exito, id de la reserva:" ++ idStr ++ "\n"
    putStr msg 
    print c
    generales(tipoHabitaciones, cantidadXtipo, tarifas, c, facturas,habitaciones)

-- Formato de reserva: [[idRes,nombre, fechaEntrada,fechaSalida,cantidadHab,activa,noFacturada,montoEnString,idHabitacion,nombreTipoHab, cantNiños,cantAdultos],
-- [idRes,nombre, fechaEntrada,fechaSalida,cantidadHab,activa,noFacturada,nombreTipoHa, cantNiños,cantAdultos, 
-- nombreTipoHab,cantNiños,cantAdultos,nombreTipoHab,cantNiños,cantAdultos]]
-- [["10","nombre","fechaEntrada","fechaSalida",,"cantidadHab","activa","noFacturada","montoEnString","idHab","nombreTipoHa","cantNi\241os","cantAdultos","idHab","nombreTipoHab","cantNi\241os","cantAdultos","idHab","nombreTipoHab","cantNi\241os","cantAdultos"]]
cantidadResXTipo( reservaActual,  tipo, i, cantidadRes, cont )= 
    if cont == cantidadRes then 0
    else if enesimo(reservaActual,i)==tipo then 
       1+ cantidadResXTipo (reservaActual  ,tipo ,i+4 ,cantidadRes ,cont+1)
    else
    cantidadResXTipo( reservaActual,  tipo, i+4, cantidadRes, cont+1)

    

totalResPorTipo (reserv,  tipo) =
    if reserv==[] then 
        0
    else do
        let c =  read(enesimo(head reserv, 4)) :: Int
        (cantidadResXTipo (head reserv,  tipo, 9, c, 0)) + totalResPorTipo  (tail reserv, tipo)   -- se ubica en el indice 7 de cada subarreglo

buscarCantTipo(cantXtipo,tipo)= 
    if head(head cantXtipo) == tipo then head(tail(head(cantXtipo)))
    else buscarCantTipo( tail cantXtipo,tipo)

validarCantidadHabitacionXtipo(reser,cantXtipo,tipo)= do 
    let cant = read(buscarCantTipo(cantXtipo,tipo)):: Int
    (totalResPorTipo(reser, tipo)) +1 < cant
    




-- [["1","Rocardp\DEL Soto","10/10/2020/\DEL","10/10/2020","3","Estandar","3","2","Estandar","3","2","Suite","3","3"],["2","Rocardo Soto","10/20/1010","10/20/2020","1","Estandar","2","1"]]


buscarId( habitaciones, tipo) = 
    if habitaciones==[] then [] 
    else if enesimo(head habitaciones, 2)=="noReservada" && tipo==enesimo(head habitaciones, 1) then enesimo(head habitaciones, 0) -- retorna el id 

    else 
        buscarId (tail habitaciones, tipo) 
actualizarHabs(idHab, habitaciones) = 
    if habitaciones==[] then [] 
        else if idHab==enesimo(head habitaciones, 0) then 
            [[idHab]++[enesimo(head habitaciones, 1)]++["reservada"]] ++actualizarHabs( idHab,tail habitaciones)
    else 
        [head habitaciones ]++actualizarHabs(idHab,tail habitaciones)
-- reservar([["Estandar","des","30"],["Suite","des","40"]], [["Estandar","3"],["Suite","4"]],[["1","123"],["2","133"],["3","434"],["4","233"],["5","4343"],["6","4344"],["7","5345"],["8","6456"]],[],[],[["0","Estandar","noReservada"],["1","Estandar","noReservada"],["2","Estandar","noReservada"],["3","Suite","noReservada"],["4","Suite","noReservada"],["5","Suite","noReservada"],["6","Suite","noReservada"]],[],0,0,0)

-- [["1","Ricardo","10/10/2020","10/20/1010","2","activa","noFacturada","montoEnString","0","Estandar","2","2","3","Suite","1","3"],["2","Brandon","10/11/2020","14/11/2020","2","activa","noFacturada","montoEnString","4","Suite","0","4","1","Estandar","2","1"]]

reservar (tipoHabitaciones, cantidadXtipo, tarifas, reservaciones, facturas,habitaciones, newRes, i,cantNinos,cantAdultos ) = do 
    putStr  "\n"    
    putStr "Reservacion \n"    
    putStr "Ingrese la cantidad de adultos \n"
    putStr "Salir y terminar de reservar habitaciones(S)\n" 
    adultos <- getLine 
    if adultos == [head "S"] then 
        terminarHab tipoHabitaciones cantidadXtipo tarifas reservaciones facturas newRes i habitaciones cantNinos cantAdultos
    else do
    putStr "Ingrese la cantidad de niños\n"
    ninnos <- getLine  
    putStr "Ingrese el tipo de habitacion\n"
    tipo <- getLine         

    if  validarHabitacionExiste(tipoHabitaciones,tipo,0) then  do 
        if validarCantidadHabitacionXtipo(reservaciones,cantidadXtipo,tipo) then do 
            let n = read ninnos:: Int
            let m =  read adultos:: Int
            let suma = m+n
            let cantAdultos1 = cantAdultos+m 
            let cantNinos1 = cantNinos+n 
            let idHab = buscarId( habitaciones, tipo)
            let nuevasHab = actualizarHabs(idHab, habitaciones)

            reservar(tipoHabitaciones ,cantidadXtipo ,tarifas ,reservaciones ,facturas,nuevasHab, newRes++[idHab]++[tipo] ++ [ninnos] ++[adultos],i + 1 ,cantAdultos1,cantNinos1)  
        else 
            do  
            print "Error: No hay más habitaciones de ese tipo disponible"
            reservar (tipoHabitaciones, cantidadXtipo, tarifas, reservaciones, facturas,habitaciones, newRes, i,cantNinos,cantAdultos )        
    else do     
        
        print "Error: Ese tipo de habitacion no existe"                
        -- if  validarCantidadTipoHabitacion tipoHabitaciones suma i then 
           --  reservar (tipoHabitaciones, cantidadXtipo, tarifas, reservaciones, facturas, newRes, i )        
        --else
          --  reservar(tipoHabitaciones ,cantidadXtipo ,tarifas ,reservaciones ,facturas, newRes++[tipo] ++ [ninnos] ++[adultos],i + 1 )  

                 


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



--mnnn = do 
    --let t = cargarTarifas [] [] [] []
    --let cant = 5 
    --let monto = calcularTarifaDias(t,28,31,10,10,0,"Adulto",2020,5)
    --print monto



getCurrFactId facturas  =
    if facturas == [] then "0"
    else if largo(facturas)==1 then  
        head (head facturas) 
    else
        getCurrFactId (tail facturas)


noExisteReserva (tipoHabitaciones, cantidadXtipo, tarifas, reservaciones, facturas,habitaciones)= do
  print "Error: No se encontro ninguna reserva con ese ID"
  generales (tipoHabitaciones, cantidadXtipo, tarifas, reservaciones, facturas,habitaciones)

validarReservaExiste (numReservacion, i, tipoHabitaciones, cantidadXtipo, tarifas, reservaciones, facturas,habitaciones) = do
    if i == largo(reservaciones) then noExisteReserva(tipoHabitaciones, cantidadXtipo, tarifas, reservaciones, facturas,habitaciones)
    else if enesimo(enesimo(reservaciones,i),0)== numReservacion then facturacion(numReservacion, i, tipoHabitaciones, cantidadXtipo, tarifas, reservaciones, facturas,habitaciones)
    else
      validarReservaExiste (numReservacion, i+1, tipoHabitaciones, cantidadXtipo, tarifas, reservaciones, facturas,habitaciones)


facturacion (numReservacion, i, tipoHabitaciones, cantidadXtipo, tarifas, reservaciones, facturas,habitaciones) = do
  print "---------------------"
  print "----- Factura -------"
  print "---------------------"
  let idenFact = read (getCurrFactId facturas) :: Int
  let cadenaIden = "Identificador factura: " ++ show idenFact
  print cadenaIden
  let idenReser = "Identificador reserva: " ++ enesimo(enesimo(reservaciones,i),0)
  print idenReser
  let subTotal = read (enesimo(enesimo(reservaciones,i),7)) :: Int
  let impuesto = 0.13
  let montoImpuesto = impuesto * fromIntegral subTotal
  let cadenaImpuesto = "El impuesto de venta es de: " ++ show impuesto
  print cadenaImpuesto
  let total = impuesto + fromIntegral subTotal
  let cadenaTotal = "El monto total es de: " ++ show total
  print "---------------------"

  -- 
  -- Guardar factura en arreglo
  let c = facturas ++ [ [show idenFact]++[show total]++[show idenReser] ]
  generales (tipoHabitaciones, cantidadXtipo, tarifas, reservaciones, c, habitaciones)



    




admin hab cantxTipo tarifas reser fact habitaciones  = do 
                          putStr "Ingrese una opcion \n"
                          putStr "1.Información de hotel \n"
                          putStr "2.Cargar tipo de habitacion  \n" 
                          putStr "3.Asignar cantidad de habitaciones por tipo \n"
                          putStr "4.Carga de tarifas \n"
                          putStr "5.Consultar reservaciones \n"
                          putStr "6.Consulta de factura \n"
                          putStr "7.Estadísticas de ocupación \n"
                          putStr "8.Salir \n"
                          first <- getLine
                          if first== [head "2"] then                             
                            cargarTipoHabitaciones cantxTipo tarifas reser fact habitaciones
                          else if first== [head "3"] then                             
                            asignarHabitacionexTipo(hab,cantxTipo,tarifas,reser,fact,0,[],habitaciones)
                          else if first== [head "4"] then                             
                            cargaTarifas hab cantxTipo reser fact habitaciones
                          else if first== [head "8"] then                             
                            menu hab cantxTipo tarifas reser fact habitaciones
                          else 
                            admin hab cantxTipo tarifas  reser fact habitaciones
