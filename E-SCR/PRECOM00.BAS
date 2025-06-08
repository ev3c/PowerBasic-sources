defint a-z
on error goto errores
 call set.ereturnon
 call set.ekbdclickon
 call set.ekbdbeepon
 dim dynamic p%(4096)			
 dim dynamic wind$(1:25,1:2)		
 dim dynamic blq$(1:36)			
 dim dynamic macro$(0:9)		
posy% = 1
posx% = 1
fgc%  = 7
bgc%  = 0
munidad$   = driveletter$(0)
mprograma$ = " E-Screen  Ver 3.01 "
merr% = 0
dim car%(1:151)
data 204,192,188,185,187,220,221,24,218,195,197,180
data 217,193,194,191,201,186,206,205,179,16,203,202
data 196,200,0,0,0,0,0,0,0,0,0,213,209,184,176,177
data 178,219,214,210,183,0,0,0,0,198,216,181,223,222
data 25,199,215,182,0,0,0,0,0,212,207,190,17,211,208,189
data 240,241,242,243,244,245,246,247,251,236,252,253
for x%=1 to 70
   read car%(x%)
next x%
for x%= 140 to 151
   read car%(x%)
next x%
call pincel.ini				
 call kbd.speed( 0,0 )
 call main.ini.colors
 call main.ini.config
key(30) on
key(31) on
on key(30) gosub key.ayuda:
on key(31) gosub key.ficheros.shell:
 call editor.scr.hold
 call main
end:
errores:
 call errores.display
resume next
key.ayuda:
 call ayuda.general
return
key.ficheros.shell:
 call ficheros.shell
return
$INCLUDE  "PRECOM00.INC"
$INCLUDE  "PRECOM01.INC"
$INCLUDE  "PRECOM02.INC"
