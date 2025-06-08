sub errores.display				shared
   local oldwmsgfgc%, oldwmsgbgc%
   local e$
   local d%

    oldwmsgfgc% = wmsgfgc%
    oldwmsgbgc% = wmsgbgc%
    wmsgfgc% = wefgc%
    wmsgbgc% = webgc%
    merr% = err

    $if %english

    inf$ = " Please, Inform me about that this error has success"
    select case err
     case 25 : e$ = "Device fault"
     case 27 : e$ = "Out of paper"
     case 51 : e$ = "Internal error"
     case 52 : e$ = "Bad file number"
     case 53 : e$ = "File not found"
     case 54 : e$ = "Bad file mode"
     case 55 : e$ = "File already open"
     case 57 : e$ = "Device I/O error"
     case 58 : e$ = "File already exist"
     case 61 : e$ = "Disk full"
     case 64 : e$ = "Bad file name"
     case 67 : e$ = "Too many files"
     case 70 : e$ = "Write-protected disk"
     case 71 : e$ = "Disk not ready"
     case 74 : e$ = "Rename across disks"
     case 75 : e$ = "Path/File access error"
     case 76 : e$ = "Path not found"
     case 100: e$ = "Empty memory block"
     case 101: e$ = "Inexistent block into disck"
     case 102
      e$ = "Error in bload.ansi function..."
      d% = 1
     case else
       e$ = "Error "+str$(err)+" at address "+str$(eradr)
       d% = 1
    end select

    $else

    inf$ = " Por favor... Informame de que este error ha sucedido "
    select case err
     case 25 : e$ = "Error de dispositivo"
     case 27 : e$ = "No hay papel"
     case 51 : e$ = "Error interno"
     case 52 : e$ = "Numero de fichero no v lido"
     case 53 : e$ = "Fichero no encontrado"
     case 54 : e$ = "Tipo de fichero no v lido"
     case 55 : e$ = "El fichero ya est  abierto"
     case 57 : e$ = "Error de E/S a dispositivo"
     case 58 : e$ = "El fichero ya existe"
     case 61 : e$ = "El disco est  lleno"
     case 64 : e$ = "Nombre de fichero no v lido"
     case 67 : e$ = "Demasiados ficheros"
     case 70 : e$ = "Disco protegido contra grabaci¢n"
     case 71 : e$ = "El disco no est  preparado"
     case 74 : e$ = "Nombrar de nuevo especificando la Unidad"
     case 75 : e$ = "Error de acceso a un camino/fichero"
     case 76 : e$ = "Camino o fichero no v lido"
     case 100: e$ = "El bloque de memoria est  vac¡o"
     case 101: e$ = "El bloque no existe en el disco"
     case 102
      e$ = "Error en la function bload.ansi"
      d% = 1
     case else
       e$ = "Error "+str$(err)+" en la direcci¢n "+str$(eradr)
       d% = 1
    end select

    $endif

     play "L64 N70"
     call wmsg(0,0,e$)
     if d% = 1 then
	call wmsg(0,0, inf$)
	delay 1
     end if
    wmsgfgc% = oldwmsgfgc%
    wmsgbgc% = oldwmsgbgc%

end sub





