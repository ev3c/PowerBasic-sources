
'
' �������������������������������������������������������������������������
'�����������������������������������������������������������������������ͻ�
'� Program......: ON_ETS  .BAS                                           ��
'� Definition...:                                                        ��
'� Author.......: GOODFONT.EXE v1.1      (c) Esteve Valent� 1.991        ��
'� Date.........: 12-20-1991                                             ��
'�����������������������������������������������������������������������͹�
'� OBSERVATIONS:                                                         ��
'�                                                                       ��
'�   Igual funcionamiento que el DIR del DOS, pero tambi�n hace la       ��
'�   b�squeda en los subdirectorios superiores al directorio especifi-   ��
'�   cado.                                                               ��
'�   Igual que en el DIR, se aceptan los caracteres * y ?, y si no se    ��
'�   especifica la unidad o el directorio, se toman los que est�n        ��
'�   activos en ese momento en el DOS. Si no se especifica el fichero    ��
'�   o la extensi�n, se buscar�n todos ( * ).                            ��
'�   Si se llama al programa con el comando /D, los ficheros encontrados ��
'�   ser�n borrados si se pulsa la tecla S tras la pregunta. Si el co-   ��
'�   mando es /DS, los ficheros se borrar�n automaticamente.             ��
'�                                                                       ��
'�   Se puede redireccionar la salida hacia otro dispositivo mediante    ��
'�   la sintaxis del DOS.                                                ��
'�                                                                       ��
'�����������������������������������������������������������������������ͼ
$stack 32766
$string 32
ont# = timer
open "cons:" for output as #1
print #1,"ON_ETS Malparit!  v1.1             (c) Esteve Valent�  1.991"
print #1,
 call splitcommand$(ucase$(command$), path$, fsearch$)
 call tree(path$, fsearch$, e%)

if e% = 18 then
   print #1,
   print #1, "Tiempo tardado: ";timer-ont#
else
   print #1,
   print #1, "����ERROR en los parametros pasados!!!!"
   call the.end
end if
end:


'
'�����������������������������������������������������������������������ͻ
'� SUB :splitcommand                                        12-20-1991   �
'�����������������������������������������������������������������������͹
'� ACTION:                                                               �
'�    Descompone la l�nea de comandos e inicializa las variables.        �
'�                                                                       �
'� TO PASS:                                                              �
'�    param$         ===> linea de comandos                              �
'�                                                                       �
'� RETURN:                                                               �
'�    path$          ===> unidad y directorio                            �
'�    fsearch$       ===> fichero o m�scara a buscar                     �
'�                                                                       �
'� PUBLIC:                                                               �
'�    del%           ===> borrar ficheros encontrados. Pulsar S          �
'�    dely%          ===> borrar ficheros encontrados automaticamente    �
'�                                                                       �
'�����������������������������������������������������������������������ͼ
sub splitcommand( param$, path$, fsearch$ )
   local  d$,p$,f$,e$
   local  f%
   local  file$
   shared del%
   shared dely%
    param$ = ltrim$(rtrim$(param$))
    f% = instr(param$,"/")

    if f% > 0 then
       file$ = left$(param$,f%-1)

       if instr(param$,"/H") > 0 or_
	  instr(param$,"/?") > 0 then
	  call the.end
	  end:
       end if


       if instr(param$,"/D") > 0 then
          del% = -1

	  if instr(param$,"/DS") > 0 then
             dely% = -1
          end if

       end if

    else
       file$ = param$
    end if

     call splitpathfn(file$,d$,p$,f$,e$)

    if d$ = "" then d$ = driveletter$(0)' si no se define la unidad
    if p$ = "" then
       p$ = getdosdir$(drivecode%(d$)) 	' si no se define el path
    end if
    path$ = d$+p$
    if f$ = "" then f$ = "*"		' si no se define fichero. Todos.
    if e$ = "" then e$ = ".*"		' si no se define extensi�n. Todas.
    fsearch$ = f$+e$

'�����������������������������������������������������������������������ͻ
'� END SUB:ON_ETS  .BAS                                     12-20-1991   �
'�����������������������������������������������������������������������ͼ
end sub




'
'�����������������������������������������������������������������������ͻ
'� SUB :tree                                                12-20-1991   �
'�����������������������������������������������������������������������͹
'� ACTION:                                                               �
'�    Programa principal. Busca los ficheros en los directorios          �
'�                                                                       �
'� TO PASS:                                                              �
'�    path$          ===> unidad y directorio                            �
'�    fsearch$       ===> fichero o m�scara a buscar                     �
'�                                                                       �
'� RETURN:                                                               �
'�    ee%            ===> error retornado. 0=no error  18=fin ficheros   �
'�                                                                       �
'�����������������������������������������������������������������������ͼ
sub tree(path$, fsearch$, ee%)
   local f$,fa%,s&,d%,t%,e%		' uso interno
   local p%				' uso interno
   local mpath$				' para guardar path$
   local ltree$()			' para guardar directorios
   static level%			' nivel
   shared tsize&			' total espacio recuperado
   shared del%				' borrar?

     redim ltree$(100)				' para guardar directorios

    incr level%
    mpath$ = path$
    print #1, "buscando   : ";mpath$;fsearch$	' imprimir busqueda
     call tree.findfile(mpath$,fsearch$,ee%)	'  buscar ficheros
    if ee% <> 18 then exit sub			' si hay error salir
     call tree.readdir (mpath$,ltree$(), ee%)	'  leer directorios
    if ee% <> 18 then exit sub			' si hay error salir

    do while ltree$(p%+1) <> ""			' si existen directorios
       incr p%, 1
        call tree(path$+ltree$(p%)+"\", fsearch$, ee%) ' llamada recursiva
    loop

    decr level%

    if level% = 0  then

       if del% then
	  print #1,
	  print #1, "Espacio recuperado....: ";tsize&
	  print #1,
       end if

    end if

'�����������������������������������������������������������������������ͻ
'� END SUB:tree                                             12-20-1991   �
'�����������������������������������������������������������������������ͼ
end sub




'
'�����������������������������������������������������������������������ͻ
'� SUB :tree.readdir                                        12-20-1991   �
'�����������������������������������������������������������������������͹
'� ACTION:                                                               �
'�    Lee todos los subdirectorios del directorio especificado.          �
'�                                                                       �
'� TO PASS:                                                              �
'�    search$        ===> m�scara de b�squeda                            �
'�                                                                       �
'� RETURN:                                                               �
'�    tdir$(1)       ===> array donde guardar los directorios encontrados�
'�    ee%            ===> error retornado. 18 o 0 =  no error            �
'�����������������������������������������������������������������������ͼ
sub tree.readdir(search$, tdir$(1), ee%)
   local f$, fa%, e%
   local p%
     call findfile(search$+"*.*",&h16,f$,fa%,s&,d%,t%,e%)

    do while e% = 0

       if fa%=16 and f$<>"." and f$<>".." then
          incr p%,1
          tdir$(p%) = f$
       end if

        call findfile("",&h16,f$,fa%,s&,d%,t%,e%)
    wend

    ee% = e%
'�����������������������������������������������������������������������ͻ
'� END SUB:tree.readdir                                     12-20-1991   �
'�����������������������������������������������������������������������ͼ
end sub




'
'�����������������������������������������������������������������������ͻ
'� SUB :tree.findfile                                       12-20-1991   �
'�����������������������������������������������������������������������͹
'� ACTION:                                                               �
'�    Busca los ficheros del subdirectorio.                              �
'�                                                                       �
'� TO PASS:                                                              �
'�    path$          ===> unidad y directorio donde buscar               �
'�    fsearch$       ===> m�scara a buscar                               �
'�                                                                       �
'� RETURN:                                                               �
'�    ee%            ===> error retornado. 0 o 18 = no error             �
'�                                                                       �
'�����������������������������������������������������������������������ͼ
sub tree.findfile(path$,fsearch$, ee%)
   local f$, fa%, e%			' uso interno
   local p%, k%, file$, ext$, size$	' uso interno
   local s$				' uso interno
   shared tsize&			' total espacio recuperado
   shared del%				' borrar?
   shared dely%				' borrar sin preguntar?
     call findfile(path$+fsearch$,&h0,f$,fa%,s&,d%,t%,e%) ' busca el
' primer fichero

    do while e% = 0
       k% = instr( f$, "." )			' inicio extensi�n

       if k% <> 0 then				' si extensi�n
          file$ = left$(left$(f$,k%-1)+"        ",8)
          ext$  = left$(mid$(f$,k%,len(f$)+1-k%)+"   ",4)
       else					' si no
          file$ = left$( f$+"        ", 8)
          ext$ = ".   "
       end if

       size$ = str$(s&)
       k% = len(size$)
       size$ = right$("         "+size$,9)
       file$ = file$+ext$+" "+_
	       size$+"  "+_
	       filedate$(d%)+"  "+_
	       filetime$(t%)+"    "

       print #1, " encontrado: ";file$		' imprime fichero

       if del% then				' si borrar

	  if dely% then				'  si borrar autom.
             s$ = "S"
	  else                                  '  si no
	     print #1,
	     print #1, "???????????: Borro el fichero (S/N) ? "

	     do
	       s$ = ucase$(input$(1))		'   borro el fichero?
	     loop until instr("SN",s$) > 0

	  end if


	  if s$="S" then			'  si respuesta = S
	     print #1, "--borrando : ";path$;f$ '   imprimir mensaje
	     kill path$+f$			'   borrar fichero
	     tsize& = tsize& + s&
	  end if

       end if

	call findfile("",&h16,f$,fa%,s&,d%,t%,e%)' sigue buscando
    wend

    ee% = e%					' atualizar error
'�����������������������������������������������������������������������ͻ
'� END SUB:tree.findfile                                    12-20-1991   �
'�����������������������������������������������������������������������ͼ
end sub





'�������������������������������������������������������������������������ͻ
'� Subrrutina ....: SplitPathFN                                            �
'�������������������������������������������������������������������������͹
'� Descompone Path$ en varias variables.                                   �
'�  ej:  Si path$ = "A:\SUBRRUTI\*.*" .                                    �
'�        Drive$ = "A:\"    Dir$ = "SUBRRUTI\"   File$ = "*"  Ext$ = ".*"    �
'�       No comprueva la existencia de algun error,solamente descompone Path$�
'���������������������������������������������������������������������������͹
'�  Path$ ====>   Unidad:\Directorio\SubDirectorio\Fichero.Extensi�n a       �
'�                descomponer.                                               �
'�  Retorna....:                                                             �
'�       Drive$  ====>   Unidad incluidos los ":"                            �
'�       Dir$    ====>   Directorio, trayectoria, incluido el ultimo "\"     �
'�       File$   ====>   Nombre del fichero                                  �
'�       Ext$    ====>   Extensi�n del fichero, incluido el "." al principio �
'���������������������������������������������������������������������������ͼ
SUB SplitPathFN( path$, drive$, mdir$, file$, ext$ ) 		PUBLIC
      LOCAL temp$
      LOCAL i%                          	' start of directory
      LOCAL j%                          	' start of file name
      LOCAL k%                          	' start of extension
      LOCAL l%                          	' end of path
      temp$ = UCASE$(path$)             	' get local copy of path
      temp$ = LTRIM$(RTRIM$(temp$))
      drive$ = ""
      mdir$  = ""
      file$  = ""
      ext$   = ""

     IF temp$="." OR temp$=".." THEN   		' special case
        mdir$ = temp$
        EXIT SUB
     END IF

      l% = LEN(temp$)+1                 	' set end of path
      i% = 2                            	' find colon
      WHILE i%<=l% AND MID$(temp$,i%-1,1)<>":": i% = i%+1: WEND
      IF MID$(temp$,i%-1,1)<>":" THEN i% = 1
      j% = l%                           	' find rightmost backslash
      WHILE j%>i% AND MID$(temp$,j%-1,1)<>"\": j% = j%-1: WEND
      k% = l%                           	' find extension dot
      WHILE k%>j% AND MID$(temp$,k%,1)<>".": k% = k%-1: WEND
      IF MID$(temp$,k%,1)<>"." THEN k% = l%  	' no extension dot found

      drive$ = MID$(temp$,1,i%-1)       	' extract drive
      mdir$  = MID$(temp$,i%,j%-i%)       	' extract directory
      file$  = LEFT$(MID$(temp$,j%,k%-j%),8)  	' extract file name
      ext$   = LEFT$(MID$(temp$,k%,l%-k%),4)   	' extract extension
END SUB

'���������������������������������������������������������������������������ͻ
'�  Subrrutina ....: FindFile                                                �
'���������������������������������������������������������������������������͹
'�  Busca un fichero o grupo de ficheros en la trayectoria especificada en   �
'�  Search$.  ej:  Search$ = A:\*.*  = el primer fichero de A:\*.*           �
'�                     la siguiente llamada se hace con Search$ = ""         �
'�                 Search$ =   ""    = el segundo fichero de A:\*.*          �
'���������������������������������������������������������������������������͹
'�   search$  ====>  Trayectoria de busqueda y fichero si procede.           �
'�   sattrib% ====>  Atributo del fichero a buscar.  ej:  0 = Fichero Normal �
'�                                                                           �
'�   Retorna...:                                                             �
'�      f$       ===> Nombre y tipo de fichero                               �
'�      fattrib% ===> Atributo del fichero                                   �
'�      size&    ===> Longitud del fichero en bytes.                         �
'�      date%    ===> Byte de Fecha de grabaci�n del fichero.                �
'�      time%    ===> Byte de Hora,Minuto,Segundo de grabaci�n del fichero.  �
'�      e%       ===> Codigo de error. 0=No error   2=Path no valido         �
'�                                    18=No hay mas ficheros                 �
'���������������������������������������������������������������������������ͼ
SUB FindFile(search$,sattrib%,f$,fattrib%,size&,date%,time%,e%)	PUBLIC
      STATIC dta%()  			' static disk transfer area
      DIM dta%(0:31)                    ' 64 bytes - preserved between calls
      LOCAL dtas%, dtao%
      STATIC buf$                       ' note: this must be a static variable

      dtas = VARSEG(dta%(0))
      dtao = VARPTR(dta%(0))
      REG 1,&h1A00                    	' DOS set DTA address function
      REG 8,dtas	            	' DS:DX = address of buffer
      REG 4,dtao
      CALL INTERRUPT &H21

      IF search$<>"" THEN               'prepare for find first call
	 buf$ = search$+chr$(0)         ' create a buffer to hold search string
	 DEF SEG                        ' set default data segment
	 REG 1,&h4E00                 	' DOS find first function
         REG 3,sattrib%               	' search attribute byte in CL
	 REG 8,STRSEG(buf$)            	' DS:DX = pointer to buffer
         REG 4,STRPTR(buf$)
      ELSE                              'prepare for find next call
         REG 1,&h4F00                 	' DOS find next function
      END IF
      CALL INTERRUPT &H21
      e% = REG(1)                     	' get error from AX
      IF e%=0 THEN                      	'ok, so extract file info
	 DEF SEG = dtas
	 f$ = PEEK$(dtao+30,13)                ' extract file
	 f$ = LEFT$(f$,INSTR(f$,CHR$(0))-1)	'  extract CHR$(0) of file
	 fattrib% = dta%(10) \ 256              ' extract file attribute
	 size& = CLNG( PEEKL(dtao+26) )	' extract file size
	 date% = dta%(12)			' extract file date
	 time% = dta%(11)			' extract file time
	 DEF SEG
      END IF
END SUB


'�������������������������������������������������������������������������ͻ
'�  Funci�n....: GetDOSDir$                                                �
'�������������������������������������������������������������������������͹
'�  TO PASS:                                                               �
'�     drive% ===> Drive number. 0 for DOS default drive.                  �
'�  RETURN:                                                                �
'�     GetDOSDir$ ===> Current DOS directory of drive asked.               �
'�                      blank string if error                              �
'�������������������������������������������������������������������������ͼ
FUNCTION GetDOSDir$( drive% )					PUBLIC
    LOCAL o,s
    LOCAL d$
      STATIC buf$                       ' note: this must be a static variable
      IF drive%=0 THEN                  ' use default drive
	 REG 1,&h1900			' DOS get current drive function
         CALL interrupt &H21
	 drive% = REG(1) AND &h00FF	' get drive from AL
         drive% = drive%+1              ' convert to 1=A: convention
      END IF

      buf$ = space$(64)                 ' create a buffer to hold directory
      DEF SEG                           ' set default data segment
      s = STRSEG(buf$)
      o = STRPTR(buf$)
      REG 1,&h4700 			' DOS get directory function
      REG 4,drive%			' DL = drive code
      REG 8,s				' DS:SI = pointer to buffer
      REG 5,o
      CALL INTERRUPT &H21
      IF (reg(1) AND &h00FF)=0 THEN	' AL = 0 for successful
         buf$ = ucase$(left$(buf$,instr(buf$,chr$(0))-1))
	 IF buf$<>"" THEN buf$ = buf$+"\"
	 GetDOSDir$ = "\"+buf$
      ELSE
	 GetDOSDir$ = ""
      END IF
END FUNCTION



'�������������������������������������������������������������������������ͻ
'�  Funci�n....: DriveCode%                                                �
'�������������������������������������������������������������������������͹
'�  TO PASS:                                                               �
'�     drive$ ===> Drive letter and ":" of blank string for DOS default.   �
'�  RETURN:                                                                �
'�     DriveCode% ===> drive code                                          �
'�                      -1 if error                                        �
'�������������������������������������������������������������������������ͼ
FUNCTION DriveCode%( drive$ ) 					PUBLIC
      LOCAL d%,dd%,nd%
      REG 1,&h1900 			' get default drive
      CALL INTERRUPT &H21
      dd% = REG(1) AND &h00FF		' drive code from AL (0=A:)
      REG 1,&h0E00			' re-select default drive
      REG 4,dd%
      CALL INTERRUPT &H21		' using only to get # of drives
      nd% = REG(1) AND &h00FF		' # of drives from AL

      drive$ = UCASE$(drive$)
      drive$ = LTRIM$(RTRIM$(drive$))
      IF drive$ = "" THEN
	 DriveCode% = dd%+1		' return default drive
      ELSEIF LEN(drive$)<>2 OR RIGHT$(drive$,1)<>":" THEN
	 DriveCode% = -1		' invalid format
      ELSE
	 d% = ASC(LEFT$(drive$,1))-64	' drive code from string
	 IF d%<1 or d%>nd% THEN		' bad drive code
            DriveCode% = -1
         ELSE
            DriveCode% = d%
         END IF
      END IF
END FUNCTION


'�������������������������������������������������������������������������ͻ
'�  Funci�n....: DriveLetter$                                              �
'�������������������������������������������������������������������������͹
'�  TO PASS:                                                               �
'�     drive% ===> drive number.     0 for get default DOS drive           �
'�  RETURN:                                                                �
'�     DriveLetter$ ====>  Drive Letter with ":"                           �
'�                          blank string if error                          �
'�������������������������������������������������������������������������ͼ
FUNCTION DriveLetter$( drive% ) 				PUBLIC
  LOCAL d$, d%
   IF drive% = 0 THEN drive% = DriveCode%("")
   d$ = CHR$( drive% + 64 ) + ":"

   IF DriveCode%(d$) <> -1 THEN
      DriveLetter$ = d$
   END IF
END FUNCTION


'���������������������������������������������������������������������������ͻ
'�  Funci�n.....: FileDate$          Convierte Byte fecha en  DD/MM/AA     �
'���������������������������������������������������������������������������͹
'�    Date% ====> Es el byte leido del disco que contiene la fecha en que    �
'�                fu� grabado el fichero.                                    �
'�    Retorna....:                                                           �
'�        FileDate$ ===> La fecha de grabaci�n  "DD/MM/AA", total 8 caract.�
'���������������������������������������������������������������������������ͼ
FUNCTION FileDate$( date% ) 					PUBLIC
      LOCAL a%,m%,d%,i%
      LOCAL x$
      a% = (date%\512 + 1980) MOD 100
      m% = date%\32 MOD 16
      d% = date% MOD 32
      x$= RIGHT$(STR$(m%),2)+"/"+RIGHT$(STR$(d%),2)+"/"+RIGHT$(STR$(a%),2)
      FOR i% = 2 TO LEN(x$)
         IF MID$(x$,i%,1)=" " THEN MID$(x$,i%,1) = "0"
      NEXT i%
      FileDate$ = x$
END FUNCTION

'���������������������������������������������������������������������������ͻ
'�  Funci�n.....: FileTime$          Convierte Byte tiempo en HH:MM:SS AM  �
'���������������������������������������������������������������������������͹
'�    Time% ====>  Es el byte leido del disco que contiene la hora,minutos   �
'�                 y segundos en que fu� grabado el fichero.                 �
'�    Retorna....:                                                           �
'�        FileTime$ ===> Retorna la  "HH:MM:SS AM" , total 11 caracteres.  �
'���������������������������������������������������������������������������ͼ
FUNCTION FileTime$( Time% ) 					PUBLIC
      LOCAL h%,m%,s,i%
      LOCAL m$,x$
      IF time%<0 then
         time% = time% and &h7FFF
         h% = 16
      ELSE
         h% = 0
      END IF
      h% = h%+time%\2048
      IF h%>=12 then
         m$ = " PM"
         h% = h%-12
      ELSE
         m$ = " AM"
      END IF
      IF h%=0 THEN h% = 12
      m% = (time%\32) mod 64
      s = (time% mod 32)*2
      x$ = right$(str$(h%),2)+":"+right$(str$(m%),2)+":"+right$(str$(s),2)+m$
      FOR i% = 2 TO 8
         IF mid$(x$,i%,1)=" " THEN mid$(x$,i%,1) = "0"
      NEXT i%
      FileTime$ = x$
END FUNCTION





sub the.end
   local a$
   print #1,
   print #1, "MODO DE USO:"
   print #1,
   print #1, "  ON_ETS [d:] [\path\] [fichero] [.ext]  [/D|DS] [/H|/?]"
   print #1,
   print #1, "    [d:]      = disco donde empezer a buscar"
   print #1, "                 (si no se especifica es el actual)"
   print #1, "    [\path\]  = path donde empezar a buscar"
   print #1, "                 (si no se especifica es el actual)"
   print #1, "    [fichero] = fichero o m�scara a buscar"
   print #1, "                 (si no se especifica es *"
   print #1, "    [.ext]    = extensi�n o m�scara a buscar"
   print #1, "                 (si no se especifica es *"
   print #1, "    [/D]      = preguntar si se quieren borrar los ficheros"
   print #1, "    [/DS]     = borrar todos los ficheros encontrados"
   print #1, "    [/H|/?]   = Esta pantalla de ayuda."
   print #1,
   print #1, "EJEMPLOS:"
   print #1,
   print #1, "    ON ETS c:\ "
   print #1, "    ON_ETS c:\*.bas > filbas.lst"
   print #1, "    ON_ETS a: | sort "
   print #1, "    ON_ETS *.bak  /DS"
   print #1, "

   print #1, "Pulsa una tecla ..."
   a$ = input$(1)

   print #1, " ON_ETS.EXE es un programa de Dominio P�blico."
   print #1,
   print #1, " ON_ETS es un programa que es �til para buscar ficheros en todos"
   print #1, " o alguno de los directorios de los discos. La b�squeda se  hace"
   print #1, " recursivamente y, por tanto, se busca el fichero en  todos  los"
   print #1, " directorios enganchados al 'path' especificado. Si no se  espe-"
   print #1, " cifica se empieza la busqueda desde donde se este en el  disco."
   print #1,
   print #1, " El modo de pasarle el nombre del fichero a  buscar  es  similar"
   print #1, " al del DIR del DOS. Se pueden pasar tanto interrogantes ?  como"
   print #1, " estrellas * y todos los otros caracteres aceptados por  el  DIR"
   print #1, " del DOS."
   print #1, "
   print #1, "
   print #1, " Si quieres recibir el c�digo fuente del programa para:"
   print #1,
   print #1, "		Quick Basic 4.x"
   print #1, "		MicroSoft Basic PDS 7.x"
   print #1, "		Power Basic 2.x"
   print #1, "		Turbo Basic 1.x"
   print #1,
   print #1, " pidelo en la siguiente direcci�n:"
   print #1,
   print #1, "		Esteve Valent�"
   print #1, "		Grup Sot dels Canyers, 6/60"
   print #1, "		17220 - Sant Feliu de Gu�xols"
   print #1, "		Girona - Catalunya - Espa�a"
   print #1, "		TEL..: (972) 32.50.94"
   print #1, "		       (972) 21.37.58"
   print #1, "		FAX..: (972) 20.34.68"
   print #1,
   print #1, "	lo recibiras, todo completo, contra reembolso de 2.000 pts"
   print #1, "	m�s gastos de env�o."

end sub
