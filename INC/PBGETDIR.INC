'NOGOODFONT
' ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออปบ
'บ Packet.......:  PBGETDIR.INC                                            บบ
'บ Definition...:  Get directory, find files, test paths, etc...           บบ
'บ Author.......:  Esteve Valentก Morales                                  บบ
'บ Date.........:  04 - 09 - 91                                            บน
'บ Observations.:  Need $STACK because some routines are recursives.       บบ
'บ                                                                         บบ
'บ                                                                         บบ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนบ
'บ SUB's:                                                                  บบ
'บ         SplitPathFN(path$,drive$,mdir$,file$,ext$)                      บบ
'บ         MergePathFN(path$,dfile$,dext$,newpath$,e%)                     บบ
'บ         FindFile(search$,sattrib%,f$,fattrib%,size&,date%,time%,e%)     บบ
'บ         ReadDir( search$,sa%, dire$(1), e% )                            บบ
'บ         ReadTree( path$, atree$(), e% )                                 บบ
'บ         PutAttrib( path$, fattrib%, e% )                                บบ
'บ         SplitAttribFN( fattrib%, freadonly%, fhidden%, fsystem%,_       บบ
'บ           flabel%, fsubdir%, ffile%, fshareable% )                      บบ
'บ         MergeAttribFN( fattrib%, mreadonly%, mhidden%, msystem%,_       บบ
'บ                           mlabel%, msubdir%, mfile%, mshareable% )      บบ
'บ                                                                         บบ
'บ FUNCTION's:                                                             บบ
'บ         WhoIs%( path$ )                                                 บบ
'บ         GetDOSDir$( drive% )                                            บบ
'บ         GetDOSPath$( drive% )                                           บบ
'บ         DriveCode%( drive$ )                                            บบ
'บ         DriveLetter$( drive% )                                          บบ
'บ         SearchFileDOSPath$( f$, sattrib% )                              บบ
'บ         SearchFile$( search$, atree$() )                                บบ
'บ         GetFreeDiskSpace#( drive% )                                     บบ
'บ         GetTotalDiskSpace#( drive% )                                    บบ
'บ         FileDate$( date% )                                              บบ
'บ         FileTime$( Time% )                                              บบ
'บ         GetAttrib%( path$ )                                             บบ
'บ                                                                         บบ
'บ                                                                         บบ
'บ                                                                         บบ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนบ
'บ  Copyright (C) Robert J. Showalter, 1987.  CompuServe ID 72220,466.     บบ
'บ  Last modification and creation of new SUBs and FUNCTIONs by            บบ
'บ            PsicoSoft & Esteve Valentก  1.992                            บบ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina.: GetComputerConfig                                         บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  RETURN:                                                                บ
'บ     disk%   ===>  number of disks intalled                              บ
'บ     copro%  ===>  -1 if coprocessor installed                           บ
'บ     RS232%  ===>  number of RS232 cards                                 บ
'บ     game%   ===>  -1 if exist target game (only PC and PC/XT)           บ
'บ     modem%  ===>  -1 if exist internal modem                            บ
'บ     PRN%    ===>  number of printers                                    บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB GetComputerConfig(disk%,copro%,RS232%,game%,modem%,PRN%) 	PUBLIC
   LOCAL ax%
     CALL INTERRUPT &h11
    ax% = REG(1)
    
    IF (ax% AND 1) <> 0 THEN_
    disk% = ((ax% MOD 256) \ 64) + 1
    IF (ax% AND 2) <> 0 THEN_
    copro% = -1
    RS232% = (ax% MOD 4096) \ 512
    IF (ax% AND 4096) <> 0 THEN_
    game% = -1
    IF (ax% AND 8192) <> 0 THEN_
    modem%= -1
    PRN% = ax% \ 16384
END SUB





'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Funciขn.....: TotalLowMemory%                                          บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  RETURN:                                                                บ
'บ     TotalLowMemory% ===> Total low memory in Kb.                        บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
FUNCTION TotalLowMemory%
      CALL INTERRUPT &H12
     TotalLowMeMory% = REG(1)
END FUNCTION





'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Funciขn.....: TotalEMS%                                                บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  RETURN:                                                                บ
'บ     TotalHighMemory% ===> Total high memory in Kb.                      บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
FUNCTION TotalEMS%
     REG 1, &h8800
      CALL INTERRUPT &H15
     TotalEMS% = REG(1)
END FUNCTION





'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Funciขn....: DriveExist%                                               บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     drive% ===> Drive number. 0 for DOS default drive.                  บ
'บ  RETURN:                                                                บ
'บ     DriveExist% ==> -1 if exist                                         บ
'บ                      0 if not exist                                     บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
FUNCTION DriveExist%( drive% )
END FUNCTION





'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Funciขn.....: WhoIs%                                                   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     path$ ===> [d:][\subdirectory\][file]                               บ
'บ  RETURN:                                                                บ
'บ     WhoIs% ===> 0 if is a file                                          บ
'บ                 1 if is a directory                                     บ
'บ                -1 if error                                              บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
FUNCTION WhoIs%(path$) 						PUBLIC
    LOCAL fa%
     fa% = GetAttrib%( path$ )

     IF fa% < 0 THEN
        WhoIs% = -1
     ELSE

        IF (fa% AND 16)=0 THEN
           WhoIs% = 0
        ELSE
           WhoIs% = 1
        END IF
     END IF
END FUNCTION





'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Funciขn.....: GetAttrib%                                               บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     path$ ===> [d:][\subdirectory\][file]                               บ
'บ  RETURN:                                                                บ
'บ     GetAttrib% ===> attribute                                           บ
'บ                      -1 if error                                        บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
FUNCTION GetAttrib%( path$ ) 					PUBLIC
    LOCAL o,s,l%
    STATIC buf$		' note: this must be a static variable
     

     IF RIGHT$(path$,1) = "\" THEN
        l% = LEN(path$)
        path$ = LEFT$(path$,l%-1)
     END IF
     
     buf$=path$+chr$(0)			' buffer to hold path
     DEF SEG
     s = STRSEG(buf$)			' get string segment
     o = STRPTR(buf$)			' get string offset
     REG 1, (&h43 * 256) 			' dos CHMOD get attributes function
     REG 8, s				' DS:DX = path name
     REG 4, o
      CALL INTERRUPT &H21

     IF (REG(0) AND 1) <> 0 THEN		' carry flag signals error
        GetAttrib% = -1  			' error
     ELSE
        GetAttrib% = REG(3)		' return file attribute
     END IF
END FUNCTION





'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina..: PutAttrib                                                บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     path$    ===> [d:][\subdirectory\][file]                            บ
'บ     fattrib% ===> file attribute                                        บ
'บ  RETURN:                                                                บ
'บ     e%       ===>  0 if OK                                              บ
'บ                    -1 if error                                          บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB PutAttrib( path$, fattrib%, e% ) 				PUBLIC
   LOCAL o,s,l%
   STATIC buf$		' note: this must be a static variable
    

    IF RIGHT$(path$,1) = "\" THEN
       l% = LEN(path$)
       path$ = LEFT$(path$,l%-1)
    END IF
    
    buf$=path$+chr$(0)			' buffer to hold path
    DEF SEG
    s = STRSEG(buf$)			' get string segment
    o = STRPTR(buf$)			' get string offset
    REG 1, (&h43 * 256) + &h1		' dos CHMOD put attributes function
    REG 3, fattrib%			' atrributes to put
    REG 8, s				' DS:DX = path name
    REG 4, o
     CALL INTERRUPT &H21

    IF (REG(0) AND 1) <> 0 THEN		' carry flag signals error
       e% = -1			' error
    ELSE
       e% = 0			' not error
    END IF
END SUB





'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Funciขn.....: SplitAttribFN                                            บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     fattrib% ===> file attribute                                        บ
'บ  RETURN:                                                                บ
'บ     freadonly%  ===> -1 if true : 0 if not true                         บ
'บ     fhidden%    ===> -1 if true : 0 if not true                         บ
'บ     fsystem%    ===> -1 if true : 0 if not true                         บ
'บ     flabel%     ===> -1 if true : 0 if not true                         บ
'บ     fsubdir%    ===> -1 if true : 0 if not true                         บ
'บ     ffile%      ===> -1 if true : 0 if not true                         บ
'บ     fshareable% ===> -1 if true : 0 if not true(in Novell Netware files)บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB SplitAttribFN( fattrib%, freadonly%, fhidden%, fsystem%, flabel%,_
    fsubdir%, ffile%, fshareable%) 	PUBLIC
   LOCAL fa%
    freadonly% = 0
    fhidden%   = 0
    fsystem%   = 0
    flabel%    = 0
    fsubdir%   = 0
    ffile%     = 0
    fshareable%= 0
    fa% = fattrib%
    IF (fa% AND 1  ) <> 0 THEN freadonly%  = -1	' bit 0
    IF (fa% AND 2  ) <> 0 THEN fhidden%    = -1	' bit 1
    IF (fa% AND 4  ) <> 0 THEN fsystem%    = -1	' bit 2
    IF (fa% AND 8  ) <> 0 THEN flabel%     = -1	' bit 3
    IF (fa% AND 16 ) <> 0 THEN fsubdir%    = -1	' bit 4
    IF (fa% AND 32 ) <> 0 THEN ffile%      = -1	' bit 5
    IF (fa% AND 256) <> 0 THEN fshareable% = -1	' bit 8
    
END SUB





'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Funciขn.....: MergeAttribFN                                            บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     mreadonly%  ===> -1 to active bit                                   บ
'บ     mhidden%    ===> -1     "                                           บ
'บ     msystem%    ===> -1     "                                           บ
'บ     mlabel%     ===> -1     "                                           บ
'บ     msubdir%    ===> -1     "                                           บ
'บ     mfile%      ===> -1     "                                           บ
'บ     mshareable% ===> -1     "                (in Novell Netware files)  บ
'บ  RETURN:                                                                บ
'บ     fattrib% ===> file attribute                                        บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB MergeAttribFN( fattrib%, mreadonly%, mhidden%, msystem%, mlabel%,_
    msubdir%, mfile%, mshareable%) 	PUBLIC
   LOCAL fa%
    IF mreadonly%  THEN fa% = fa% + 1		' bit 0
    IF mhidden%    THEN fa% = fa% + 2		' bit 1
    IF msystem%    THEN fa% = fa% + 4		' bit 2
    IF mlabel%     THEN fa% = fa% + 8		' bit 3
    IF msubdir%    THEN fa% = fa% + 16		' bit 4
    IF mfile%      THEN fa% = fa% + 32		' bit 5
    IF mshareable% THEN fa% = fa% + 256		' bit 8
    fattrib% = fa%
END SUB





'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ Subrrutina ....: SplitPathFN                                            บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ Descompone Path$ en varias variables.                                   บ
'บ  ej:  Si path$ = "A:\SUBRRUTI\*.*" .                                    บ
'บ        Drive$ = "A:\"    Dir$ = "SUBRRUTI\"   File$ = "*"  Ext$ = ".*"    บ
'บ       No comprueva la existencia de algun error,solamente descompone Path$บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  Path$ ====>   Unidad:\Directorio\SubDirectorio\Fichero.Extensiขn a       บ
'บ                descomponer.                                               บ
'บ  Retorna....:                                                             บ
'บ       Drive$  ====>   Unidad incluidos los ":"                            บ
'บ       Dir$    ====>   Directorio, trayectoria, incluido el ultimo "\"     บ
'บ       File$   ====>   Nombre del fichero                                  บ
'บ       Ext$    ====>   Extensiขn del fichero, incluido el "." al principio บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
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

    if file$ <> "" then			' if exist file$
       file$ = left$(file$+"         ",8)	'  force to 8 spaces
    end if
    ext$   = LEFT$(MID$(temp$,k%,l%-k%),4)   	' extract extension

    if ext$ <> "" then 			' if exist ext$
       ext$  =left$(ext$+"    ", 4)		'  force e$ to 4 spaces
    end if
    
END SUB





'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina....: MergePathFN                                            บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  Entraremos la especificaciขn de busqueda  *.*, *.EXE,\SUBRRUTI etc..   บ
'บ  y retorna en newpath$ la Unidad:\Directorio\SubDirectorio\Fichero.Ext  บ
'บ  para utilizar en FindFile o donde sea.                                 บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ                                                                         บ
'บ  Path$  ====>  Trayectoria de busqueda                                  บ
'บ  DFile$ ====>  Fichero por defecto para si no se entra. ej: "*"         บ
'บ  DExt$  ====>  Extensiขn por defecto para si no se entra  ej "*"        บ
'บ                                                                         บ
'บ  Retorno....:                                                           บ
'บ     Newpath$ ===>  Resultado de la mezcla. Unidad\Directorio\Fichero.Extบ
'บ     e%       ===>  Codigo de error.    0 = sin error                    บ
'บ                        1 = Directorio especificado incorrecto.          บ
'บ                        2 = Especificaciขn ".." incorrecta.              บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB MergePathFN( path$, dfile$, dext$, newpath$, e% ) 		PUBLIC
   LOCAL cur.path$,cur.drive$,cur.mdir$,z$
   LOCAL inp.path$,drive$,mdir$,file$,ext$
   LOCAL d%,i%,j%
    '
    inp.path$ = path$
    replace " " with "" in inp.path$
    

    IF RIGHT$(inp.path$,1)<>"\" AND WhoIs%(inp.path$)=1 THEN
       inp.path$ = inp.path$+"\"
    END IF
     CALL SplitPathFN(inp.path$,drive$,mdir$,file$,ext$)
    
    d% = DriveCode%(drive$)

    IF d%<0 THEN
       e% = 1
       EXIT SUB
    END IF
    cur.path$ = GetDOSPath$(d%)
     CALL SplitPathFN(cur.path$,cur.drive$,cur.mdir$,z$,z$)
    '
    IF drive$="" THEN drive$ = cur.drive$
    IF left$(mdir$,1)<>"\" THEN mdir$ = cur.mdir$+mdir$
    IF file$="" THEN file$ = dfile$
    IF ext$="" THEN ext$ = dext$
    '
    i% = INSTR(mdir$,"\.\")

    WHILE i%<>0
       mdir$ = LEFT$(mdir$,i%)+RIGHT$(mdir$,LEN(mdir$)-(i%+2))
       i% = INSTR(mdir$,"\.\")
    WEND

    i% = INSTR(mdir$,"\..\")

    WHILE i%<>0
       j = i%-1
       WHILE j%>0 AND MID$(mdir$,j%,1)<>"\": j% = j%-1: WEND

       IF j%=0 THEN
          e% = 2
          EXIT SUB
       END IF
       mdir$ = LEFT$(mdir$,j%-1)+RIGHT$(mdir$,LEN(mdir$)-(i%+2))
       i% = INSTR(mdir$,"\..\")
    WEND

    newpath$ = ucase$(drive$+mdir$+file$+ext$)
    e% = 0
END SUB





'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina ....: FindFile                                                บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  Busca un fichero o grupo de ficheros en la trayectoria especificada en   บ
'บ  Search$.  ej:  Search$ = A:\*.*  = el primer fichero de A:\*.*           บ
'บ                     la siguiente llamada se hace con Search$ = ""         บ
'บ                 Search$ =   ""    = el segundo fichero de A:\*.*          บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ   search$  ====>  Trayectoria de busqueda y fichero si procede.           บ
'บ   sattrib% ====>  Atributo del fichero a buscar.  ej:  0 = Fichero Normal บ
'บ                                                                           บ
'บ   Retorna...:                                                             บ
'บ      f$       ===> Nombre y tipo de fichero                               บ
'บ      fattrib% ===> Atributo del fichero                                   บ
'บ      size&    ===> Longitud del fichero en bytes.                         บ
'บ      date%    ===> Byte de Fecha de grabaciขn del fichero.                บ
'บ      time%    ===> Byte de Hora,Minuto,Segundo de grabaciขn del fichero.  บ
'บ      e%       ===> Codigo de error. 0=No error   2=Path no valido         บ
'บ                                    18=No hay mas ficheros                 บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
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





'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Funciขn....: GetDOSDir$                                                บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     drive% ===> Drive number. 0 for DOS default drive.                  บ
'บ  RETURN:                                                                บ
'บ     GetDOSDir$ ===> Current DOS directory of drive asked.               บ
'บ                      blank string if error                              บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
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





'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Funciขn....: GetDOSPath$                                               บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     drive% ===> Drive number. 0 for DOS default drive.                  บ
'บ  RETURN:                                                                บ
'บ     GetDOSPath$===> Current DOS path of drive asked.                    บ
'บ                      blank string if error                              บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
FUNCTION GetDOSPath$( drive% ) 					PUBLIC
    LOCAL drive$
    LOCAL mdir$
     drive$ = DriveLetter$( drive% )
     mdir$  = GetDOSDir$( drive% )

     IF drive$ <> "" AND_
        mdir$  <> "" THEN
        GetDOSPath$ = drive$ + mdir$
     ELSE
        GetDOSPath$ = ""
     END IF
END FUNCTION





'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Funciขn....: DriveCode%                                                บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     drive$ ===> Drive letter and ":" of blank string for DOS default.   บ
'บ  RETURN:                                                                บ
'บ     DriveCode% ===> drive code                                          บ
'บ                      -1 if error                                        บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
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





'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Funciขn....: DriveLetter$                                              บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     drive% ===> drive number.     0 for get default DOS drive           บ
'บ  RETURN:                                                                บ
'บ     DriveLetter$ ====>  Drive Letter with ":"                           บ
'บ                          blank string if error                          บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
FUNCTION DriveLetter$( drive% ) 				PUBLIC
    LOCAL d$, d%
     IF drive% = 0 THEN drive% = DriveCode%("")
     d$ = CHR$( drive% + 64 ) + ":"
     

     IF DriveCode%(d$) <> -1 THEN
        DriveLetter$ = d$
     END IF
END FUNCTION





'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Funciขn.....: SerarchFileDOSPath$                                      บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     f$      ===>  file to search in DOS PATH enviroment.  .             บ
'บ     sattrib%===>  attribute to search.                                  บ
'บ  RETURN:                                                                บ
'บ     SearchPath$ ===> path and filename.ext  (d:\dir\filename.ext)       บ
'บ                       blank string if not found or error.               บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
FUNCTION SearchFileDOSPath$( f$, sattrib%, medospath$)		PUBLIC
    LOCAL path$,mdir$,search$,found$
    LOCAL k%,fattrib%,size&,date%,time%,e%
    LOCAL lenpath%
     

     if medospath$ = "" then
        path$ = ENVIRON$("PATH") 		' fetch dos search path
     else
        path$ = medospath$			' Me DOS path
     end if
     path$ = LTRIM$(RTRIM$(path$))
     k% = 1 					' pointer into path$
     mdir$ = GetDOSPath$(0)			' tart with current directory
     search$ = mdir$+f$   			' build search string
      CALL FindFile(search$,sattrib%,found$,fattrib%,size&,date%,time%,e%)
     lenpath% = LEN(path$)

     WHILE e%<>0 AND k%<=lenpath%	     	' continuing search through path
        mdir$ = ""   				' build directory

        WHILE k%<=lenpath% AND MID$(path$,k%,1)<>";"
           mdir$ = mdir$+mid$(path$,k%,1)
           incr k%
        WEND

        INCR k%       				' skip over semicolon
        mdir$ = LTRIM$(RTRIM$(mdir$))
        IF RIGHT$(mdir$,1)<>"\" THEN mdir$=mdir$+"\"
        search$ = mdir$+f$
         CALL FindFile(search$,sattrib%,found$,fattrib%,size&,date%,time%,e%)
     WEND


     IF e%=0 THEN      				' found in mdir$
        SearchFileDOSPath$ = mdir$+found$
     ELSE
        SearchFileDOSPath$ = ""
     END IF
END FUNCTION





'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina....:  ReadDir                                               บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  Search all files using the mask search$ and return these in dire$()    บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     search$  ===>  search trayectory.                                   บ
'บ     sa%      ===>  atribute of files searched.                          บ
'บ  RETURN:                                                                บ
'บ     dire$()  ===>  array with the file name, extension, size, etc...    บ
'บ       	      retrun struct of dire$():                            บ
'บ                     12345678901234567890123456789012345678901234567890  บ
'บ                     FILENAME.EXT -----SIZE  MM/DD/YY  HH:MM:SS AM   at  บ
'บ                                  <DIR>        MKI$(file attribute)___|  บ
'บ     e%       ===>  DOS error code. 0 if not error.                      บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB ReadDir( search$, sa%, dire$(1), lend%, e% ) 		PUBLIC
   LOCAL f$,fa%,s&,d%,t%		' variables used with findfile
   LOCAL file$,ext$,size$		' string file, extension and size
   LOCAL k%			' internal use
   local mext$()
    

     dim mext$(1:25)
    
    p% = LBOUND( dire$(1) )			' first bound
    
    search$ = ucase$(search$)
    

    if right$(search$,1) <> "*" then		'Si no se especifica *
       mask$ = left$(search$,instr(search$,".")-1) + ".*"
       i% = 4
       mext$ = left$(right$(search$,i%),4)	'Busca primera extensiขn

       do
          incr ext%, 1
          mext$(ext%) = rtrim$(mext$)
          incr i%,4
          mext$ = left$(right$(search$,i%),4)		'Busca siguiente
       loop while left$(mext$,1) = "."			
'extensiขn
       mtest% = -1
    else
       mask$ = search$
    end if
    
     CALL FindFile (mask$,sa%,f$,fa%,s&,d%,t%,e%)' find first file
    

    WHILE e% = 0	 				'not error, so
       
       hold% = -1
       

       if mtest% and fa% <> 16 then
          hold% = 0

          for test% = 1 to ext%

             if instr(f$, mext$(test%)) > 0 then
                hold% = -1
                exit for
             end if
          next

       end if
       

       if hold% then
          
          k% = INSTR( f$, "." )			' start of extension

          select case k%
           case 1
             file$ = left$(f$+"               ",12)
           case > 0
             file$ = LEFT$(LEFT$(f$,k%-1)+"        ",8)
             ext$  = LEFT$(MID$(f$,k%,LEN(f$)+1-k%)+"   ",4)
           case else
             file$ = LEFT$( f$+"        ", 8)

             if fa% = 16 then				'  if directory
                ext$ = "    "
             else					'  if not directory
                ext$ = ".   "
             end if
          end select

          

          IF fa% = 16 THEN				' if directory
             size$ = "<DIR>    "
          ELSE                                        ' if not directory
             size$ = STR$(s&)
             size$ = RIGHT$("         "+ size$, 9 )
          END IF
          
          dire$(p%) = file$+ext$+" "			' put file name, ext
          if lend%>0 then dire$(p%)=dire$(p%)+size$+"  "		' put size
          if lend%>1 then dire$(p%)=dire$(p%)+fmtdate$(d%)+"  "	' put date
          if lend%>2 then dire$(p%)=dire$(p%)+fmttime$(t%)		' put time
          dire$(p%) = dire$(p%)+"     "+mki$(fa%)	' put file attribute
          
          INCR p%					' increment pointer to array
       end if
       
        CALL FindFile ("",sa%,f$,fa%,s&,d%,t%,e%)	' find file again
    WEND 						
'wend
    dire$(p%) = ""				' clear last
    IF e% = 18 THEN e% = 0 			' end files found not error
    ' else error=-1
END SUB





'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina....:  ReadTree                                              บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  Get the directory tree from start specified in path$ and return        บ
'บ  these sorted right in dire$().                                         บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     path$    ===>  search trayectory.                                   บ
'บ                     accepts: "", "d:", "d:\dir", "d:\dir\dir\"          บ
'บ  RETURN:                                                                บ
'บ     atree$() ===>  sorted tree directory.                               บ
'บ     e%       ===>  DOS error code. 0 if not error.                      บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB ReadTree( path$, atree$(), e% ) 				PUBLIC
   LOCAL f$,fa%,s&,d%,t%		'variables used with findfile
   STATIC p%			'pointer to array
   STATIC l%			'recursive level control
   LOCAL ip%			'initial pointer to array
   LOCAL lp%			'last pointer to array
   LOCAL x%			'internal use
    '+++++++++++++++++++++++++

    IF l% = 0 THEN				'first level, so         +
       p% = LBOUND(atree$(1))			' prepare pointer        +
       l% = 1					' prepare first level    +
       path$ = UCASE$(LTRIM$(RTRIM$(path$))) 	'prepare path$           +

       IF path$ = "" THEN				'if ""                   +
          path$ = GetDOSPath$(0)			' default DOS path       +
       END IF                                     '                        +

       IF LEN( path$ ) = 2 THEN			'if not path in unity    +
          path$ = GetDOSPath$(DriveCode%(path$))	' default path of unit   +
       END IF                                     '                        +

       IF RIGHT$(path$,1) <> "\" THEN		'if not \                +
          path$ = path$+"\"			' add \ to path$         +
       END IF                                     '                        +
       atree$(p%) = path$				' first element =path$   +
       incr p%					'			 +
    END IF					'+++++++++++++++++++++++++
    ip% = p%					'initial pointer = last
    'pointer in previus call
     CALL FindFile(path$+"*.*",&h16,f$,fa%,s&,d%,t%,e%)'---------------------

    WHILE e% = 0					'not error, so           |

       IF fa% = 16 AND_				'if directory and        |
          f$ <> "." AND f$<> ".." THEN		'not special case        |
          atree$(p%) = path$ + f$ + "\"		' put directory in array |
          INCR p%					' incr. pointer to array |
       END IF					'end if                  |
        CALL FindFile("",&h16,f$,fa%,s&,d%,t%,e%)	'find again              |
    WEND						
'-------------------------
    '-------------------------

    FOR x% = ip% TO p% -1				'new dirs?, so           |
       INCR l% 					' increment level        |
        CALL ReadTree(atree$(x%),atree$(),e%)	'  recursive calls       |
    NEXT x%					
'-------------------------
    'decrement level control
    DECR l%					' after exit
    '+++++++++++++++++++++++++

    IF l% = 0 THEN				'first level, so         +
       ARRAY SORT atree$() FOR p%-1		' sort from first element+
       IF e% = 18 THEN e% = 0 			' if end found not error +
    END IF					' to last element        +
    '+++++++++++++++++++++++++
END SUB





'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Funcion...:  SearchFile$                                               บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     search$  ===>  search mask to find file "*.*", "*.BAS" etc..        บ
'บ                     blank string for search again.                      บ
'บ     atree$() ===>  tree directory array. (returned for ReadTree)        บ
'บ  RETURN:                                                                บ
'บ     SearchFile$ ===> Path and filename found                            บ
'บ                       blank string if not found or error                บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
FUNCTION SearchFile$( search$, atree$() ) 			PUBLIC
    LOCAL f$,fa%,s&,d%,t%,e%	' used with findfile
    STATIC mask$			' search mask
    STATIC p%			' pointer to tree directory array
     search$ = LTRIM$(RTRIM$(UCASE$(search$)))

     IF search$ <> "" THEN			' if first time search
        p% = LBOUND( atree$(1) )		' pointer to first position
        mask$ = search$			' make mask
        search$ = atree$(p%)+mask$		' make coplete mask
     END IF
     

     WHILE atree$(p%) <> ""		' no end of tree, so
         CALL FindFile(search$,&h0,f$,fa%,s&,d%,t%,e%)
        search$ = ""			' clear mask

        IF e% = 0 THEN			' found, so
           SearchFile$ = atree$(p%) + f$	'  make SearchFile$
           EXIT FUNCTION			'  EXIT
        ELSEIF e% = 18 THEN			' end of files, so
           INCR p%				'  increment pointer
           search$ = atree$(p%) + mask$	'  new mask
        ELSE				' error, so
           SearchFile$ = ""			'  no file
           EXIT FUNCTION			'  EXIT
        END IF
     WEND

     
END FUNCTION





'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Funcion...:  GetFreeDiskSpace#                                         บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     drive%  ===> drive number. 0 for DOS default drive                  บ
'บ  RETURN:                                                                บ
'บ     GetFreeDiskSpace# ===> Free disk space.                             บ
'บ                             -1 if error                                 บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
FUNCTION GetFreeDiskSpace#( drive% ) 				PUBLIC
    LOCAL Sectors%
    LOCAL FreeAloc%
    LOCAL BytesPerSector%
     REG 1,&H3600       		' function 36 ---> AH
     REG 4,drive%        		' 0 = default, 1 = A, etc
      CALL INTERRUPT &H21
     Sectors%        = REG(1)	' sectors per alloc or FFFF = invalid drive
     FreeAloc%       = REG(2)	' of free allocation units on drive
     BytesPerSector% = REG(3)  	' bytes per sector
     

     IF Sectors% = -1 THEN
        GetFreeDiskSpace# = -1
     ELSE
        GetFreeDiskSpace# = FreeAloc% *_
        Sectors%   *_
        BytesPerSector%
     END IF
END FUNCTION





'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Funcion...:  GetTotalDiskSpace#                                        บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     drive%  ===> drive number. 0 for DOS default drive                  บ
'บ  RETURN:                                                                บ
'บ     GetTotalDiskSpace# ===> Free disk space.                            บ
'บ                              -1 if error                                บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
FUNCTION GetTotalDiskSpace#( drive% ) 				PUBLIC
    LOCAL Sectors%
    LOCAL BytesPerSector%
    LOCAL TotalAloc%
     REG 1,&H3600       		' function 36 ---> AH
     REG 4,drive%        		' 0 = default, 1 = A, etc
      CALL INTERRUPT &H21
     Sectors%        = REG(1)	' sectors per alloc or FFFF = invalid drive
     BytesPerSector% = REG(3)  	' bytes per sector
     TotalAloc%      = REG(4) 	' total number of allocation units on drive

     IF Sectors% = -1 THEN
        GetTotalDiskSpace# = -1
     ELSE
        GetTotalDiskSpace# = TotalAloc% *_
        Sectors%   *_
        BytesPerSector%
     END IF
END FUNCTION





'------------------------------------------------------------------------------
'------------------------------------------------------------------------------
'------------------       FUNCIONES I SUBRRUTINAS DE USO INTERNO   ------------
'------------------------------------------------------------------------------
'------------------------------------------------------------------------------

FUNCTION FmtDate$ (date%)
    local d%, m%, y%
     d% = date% AND &H1F
     m% = (date% AND &H1E0) \ 32
     y% = (date% AND &HFE00) \ 512 + 1980
     FmtDate$ = using$("##-",m%)+ using$("##-",d%)+  using$("####",y%)
END FUNCTION





FUNCTION FmtTime$ (time%)
    local s%, m%, h%
    local abbr$
     
     s% = (time% AND &H1F) * 2
     m% = (time% AND &H7E0) \ 32
     
     h% = (time% < 0) * (-16) + ((time% AND &H7FFF) \ 2048)
     Abbr$ = " am"
     IF h% = 12 THEN Abbr$ = " pm"
     IF h% = 0  THEN h% = 12
     

     IF h% > 12 THEN   			'Reset to 12 hour clock
        h% = h% MOD 12
        Abbr$ = " pm"
     END IF
     FmtTime$ = using$("##:",h%) + using$("##:",m%) +_
     using$("## "+Abbr$,s% )
     
END FUNCTION





'FUNCTION RStr$ (X%, LX%)
'     local x$
'	X$ = STR$(X%)
'	RStr$ = RIGHT$("00000" + ltrim$(X$), LX%)
'END FUNCTION

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



function dsk.get.file$(file$)
     
      call findfile(file$,&h0,f$,fa%,s&,d%,t%,e%)

     if e% = 0 then
        file% = freefile
        open file$ for binary as file%
        get$ file%, s&, dat$
        close file%
        dsk.get.file$ = dat$
     end if
     
end function





