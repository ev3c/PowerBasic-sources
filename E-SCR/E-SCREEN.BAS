defint a-z

on error goto errores

$include "e-screen.dcl"

 call kbd.speed( 0,0 )
 call main.ini.colors
 call main.ini.config

key(30) on
key(31) on
on key(30) gosub key.ayuda:
on key(31) gosub key.ficheros.shell:

$if %beta

 call kbd.putkey( &h00, &h4b )
 call kbd.putkey( &h0d, &h1c )
 call kbd.putkey( &h00, &h48 )
 call kbd.putkey( &h0d, &h1c )

$endif

 call editor.scr.hold
 call main

end:

'***********************************************************************

sub main
   local menu$()
   local op%
   shared ayuda.min%, ayuda.max%

     dim menu$(1:10)

    $if %english
    menu$(1) = "Files"
    menu$(2) = "Write"
    menu$(3) = "Utility"
    menu$(4) = "Blocks"
    menu$(5) = "Config"
    menu$(6) = "Help"
    $else
    menu$(1) = "Ficheros"
    menu$(2) = "Grabar"
    menu$(3) = "Utilidad"
    menu$(4) = "Bloques"
    menu$(5) = "Config"
    menu$(6) = "Ayuda"
    $endif

    do
       ayuda.min% = 1
       ayuda.max% = 8

       op% = wlmenu%(menu$())

       select case op%
        case 1 : call ficheros
        case 2 : call grabar
	case 3 : call utilidad
        case 4 : call bloques
        case 5 : call config
        case 6 : call ayuda
	case else
           call editor
       end select

    loop while -1

end sub




errores:
 call errores.display
resume next

key.ayuda:
 call ayuda.general
return

key.ficheros.shell:
 call ficheros.shell
return

$include "pbprn   .inc"                 ' LIB Impressora
$include "pbscr   .inc"			' LIB Pantalla
$include "pbstr   .inc"			' LIB Variables caracteres
$include "pbkbd   .inc"			' LIB Teclado
$include "pbgetdir.inc"			' LIB Acceso al disco
$include "pbwindow.inc"			' LIB Windows
$include "pbeinmat.inc"			' LIB Einput principal
$include "pbvansi .inc"                 ' LIB Visualitzar ANSI
$include "pbpincel.inc"                 ' LIB Pincel
$include "ficheros.inc"			' E Ficheros

$segment

$include "e-screen.inc"			' E General
$include "e-screrr.inc"			' E Errores
$include "editor  .inc"			' E Editor
$include "grabar  .inc"			' E Grabar
$include "bloques .inc"			' E Bloques
$include "utilidad.inc"			' E Utilidades
$include "config  .inc"			' E Configuraci¢n
$include "ayuda   .inc"			' E Ayuda

