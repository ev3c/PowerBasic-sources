REM  Programa.....: E-VANSI   ( View ANSI )
REM  Versi¢.......: 1.2
REM  Autor........: Esteve Valent¡ Morales
REM  Llenguatge...: Power Basic 2.1e
REM  Comentaris...: Aquest programa es una utilitat pel E-SCREEN i es
REM                 suministra juntament amb aquest.
REM                 Permet visualitzar un o varis fitxers ANSI sense
REM                 que el driver ANSI.SYS del DOS estigui carregat
REM                 a la mem•ria.

defint a-z

%english = 0

$include "pbvansi.inc"
$include "pbscr.inc"
$include "pbgetdir.inc"

open "con:" for output as #1

on error goto meerror

file$ = command$


 dim file$(1:512)


if file$ = "" then
    call meexit
else

    call splitpathfn(file$,d$,s$,f$,e$)
   if e$ = "" then e$ = ".ANS"
   path$ = d$+s$

    call findfile(path$+f$+e$,&h0,f$,fa%,s&,d%,t%,e%)

   if e% <> 0 then call meexit


   do

      if e% = 0 then
         p% = p% + 1
         file$(p%) = path$+f$
      end if

       call findfile("",&h0,f$,fa%,s&,d%,t%,e%)
   loop while e%=0

   array sort file$() for p%

   for x% = 1 to p%
       call bload.ansi(file$(x%))
   next

   color 7,0
end if


end:

'**********************************************************************

meerror:

 print #1,
 print #1, " ­­­­  ERROR  !!!!  "; err
 print #1,

 call meexit

sub meexit

    $if %english

    print #1, "E-VANSI ( View ANSI )  v1.2   Copyright (c)1.992   Esteve Valent¡ & Psicosoft"
    print #1,
    print #1, " This utility can be included in Screen Editor E-SCREEN"
    print #1, " Can be useful to view one or more ANSI files without the"
    print #1, " ANSI.SYS DOS driver loaded into memory necesarly"
    print #1, "
    print #1, " The program find the files to view and sort ascendig his names"
    print #1, " You must be remember this wen be in use E-VANSI"
    print #1, "
    print #1, " With this utility can be possible make animations viewing"
    print #1, " some ANSI files"
    print #1, "
    print #1, " Press any key to continue..."
    a$ = input$(1)
    print #1,
    print #1, "USAGE:"
    print #1,
    print #1, "    E-VANSI [d:] [\path\] file [.ext]"
    print #1, "
    print #1, "      Optional into []"
    print #1, "      d:     = driver"
    print #1, "      \path\ = directory"
    print #1, "      file   = file name or search mask (same of DOS use)"
    print #1, "      .ext   = extension (default is .ANS)"
    print #1, "
    print #1, "EXAMPLES:
    print #1, "
    print #1, "    E-VANSI cataguai.ans"
    print #1, "    E-VANSI c:\ansi\*.ans"
    print #1, "    E-VANSI e-screen.ans"
    print #1,
    print #1, " Press any key to continue..."
    a$ = input$(1)
    print #1,
    print #1,
    print #1, "SHAREWARE:"
    print #1, "
    print #1, "    This program isn't Public Domain or FreeWare"
    print #1, "    If you want to get the font code of E-VANSI in"
    print #1,
    print #1, "			Power Basic 2.x"
    print #1, "			MS Basic PDS 7.x"
    print #1, "			Quick Basic 4.x"
    print #1, "			Turbo Basic 1.x"
    print #1,
    print #1, "    send a 42$ check paiable to Esteve Valent¡ to:"
    print #1, "
    print #1, "       PsicoSoft C.B."
    print #1, "       Esteve Valent¡"
    print #1, "       Grup Sot dels Canyers, 6/60"
    print #1, "       17220 - Sant Feliu de Gu¡xols"
    print #1, "       Girona - Catalunya - Spain"
    print #1, "       FAX....: 34-72 20.34.68 "
    print #1, "
    print #1, "       RIME Net   BBS: ->ABAFORUM"
    print #1, "                 AREA: QBASIC, SHAREWARE"
    print #1, "       INTERNET: esteve.valenti@abaforum.es"
    print #1, "       FIDO Net: 2:343/106.11"
    print #1,

    $else

    print #1, "E-VANSI ( View ANSI )  v1.2   Copyright (c)1.992   Esteve Valent¡ y PsicoSoft"
    print #1,
    print #1, " Esta utilidad se incluye en el Editor de Pantallas E-SCREEN"
    print #1, " Permite visualizar uno o m s ficheros ANSI sin que el driver"
    print #1, " ANSI.SYS del DOS est‚ cargado en memoria."
    print #1,
    print #1, " El programa busca el/los fichero/s ANSI a visualizar y"
    print #1, " los ordena por su nombre de menor a mayor. Deber s tener"
    print #1, " esto en cuenta al utilizar E-VANSI"
    print #1,
    print #1, " Con esta utilidad podr s crear animaciones en pantalla"
    print #1, " visualizando varios ficheros ANSI"
    print #1,
    print #1, "Pulsa tecla para continuar..."
    a$ = input$(1)
    print #1,
    print #1, "MODO DE USO:"
    print #1,
    print #1, "    E-VANSI [d:] [\path\] fichero [.ext]"
    print #1,
    print #1, "       Entre [] son opcionales."
    print #1, "       d:      = unidad"
    print #1, "       \path\  = subdirectorio"
    print #1, "       fichero = nombre del fichero o m scara de b£squeda,"
    print #1, "                 igual que con el DOS "
    print #1, "       .ext    = extensi¢n del fichero (.ANS por defecto)"
    print #1,
    print #1, "EJEMPLOS:"
    print #1,
    print #1, "    E-VANSI cataguai.ans"
    print #1, "    E-VANSI c:\ansi\*.ans"
    print #1, "    E-VANSI e-screen.ans"
    print #1,
    print #1, "Pulsa tecla para continuar..."
    a$ = input$(1)
    print #1,
    print #1, "SHAREWARE:"
    print #1,
    print #1, "    Este programa no es de dominio p£blico ni gratuito"
    print #1, "    Si deseas recibir el c¢digo fuente de E-VANSI en"
    print #1,
    print #1, "			Power Basic 2.x"
    print #1, "			MS Basic PDS 7.x"
    print #1, "			Quick Basic 4.x"
    print #1, "			Turbo Basic 1.x"
    print #1,
    print #1, "    lo recibir s contra reembolso de 4.360 pts, incluidos"
    print #1, "    gastos de envio solicit ndolo en la siguiente direcci¢n."
    print #1,
    print #1, "       PsicoSoft C.B."
    print #1, "       Esteve Valent¡"
    print #1, "       Grup Sot dels Canyers, 6/60"
    print #1, "       17220 - Sant Feliu de Gu¡xols"
    print #1, "       Girona - Catalunya - Spain"
    print #1, "       FAX....: 972 20.34.68 "
    print #1, "
    print #1, "       RIME Net   BBS: ->ABAFORUM"
    print #1, "                 AREA: QBASIC, SHAREWARE"
    print #1, "       INTERNET: esteve.valenti@abaforum.es"
    print #1, "       FIDO Net: 2:343/106.11"

    $endif

    end:

end sub





