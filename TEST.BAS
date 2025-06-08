'		     -BC7 Co.-BC7 In.-QB45 Co.-QB45 In. -
'--------------------------------------------------------------------------
'ToshibaT2200SX 20Mhz -       -       -        -         -
'sin Copro 387        - 188"  - 196"  -        -         -
'--------------------------------------------------------------------------
'Arche 386sx 16Mhz    -       -       -        -         -
'sin Copro 387        - 236"  - 251"  - 319"   - 323"    -
'--------------------------------------------------------------------------
'D arc 386 33Mhz      -       -       -        -         -
'+ Copro 387          - 62"   -       -        -         -
'--------------------------------------------------------------------------
'otros                -       -       -        -         -
'		     -       -       -        -         -
'--------------------------------------------------------------------------
'BC7 Co.=Basic Compiler 7.1 Compilado
'BC7 In.=Basic Compiler 7.1 Interpretado
'QB45 Co.=Quick Basic 4.5 Compilado
'QB45 In.=Quick Basic 4.5 Interpretado
'************** Progama **************************

CLS
L1$ = "Linea 1 de texto"
L2$ = "Linea 2 de texto"
time1$ = TIME$
'funciones de calculo y texto
FOR i = 1 TO 10000
PRINT "*";
a = SIN(1.23456) ^ COS(1.2345)
b = COS(6.54321) ^ SIN(5.4321)
Texto1$ = MID$(L1$, 3, 2) + LEFT$(L1$, 2)
NEXT i


' funciones graficas
SCREEN 12 'Vga 640*480 16 colores
FOR x = 1 TO 20
CLS
FOR i = 0 TO 480 / 2 STEP 5
LINE (i, i)-(639 - i, 479 - i), 1, B
NEXT i
FOR i = 1 TO 50 STEP 1
PAINT (i * 6, i * 6), 1
NEXT i
NEXT x

'funciones de ficheros
'SCREEN 0 'vuelve a modo texto
'OPEN "Fich" FOR BINARY AS #1
'FOR i = 1 TO 1000
'n = INT(RND(1) * 1000) + 1
'PUT #1, n
'NEXT i
'CLOSE #1
time2$ = TIME$
segundos1 = VAL(RIGHT$(time1$, 2)) + (VAL(MID$(time1$, 4, 2)) * 60) + (VAL(LEFT$(time1$, 2)) * 3600)
segundos2 = VAL(RIGHT$(time2$, 2)) + (VAL(MID$(time2$, 4, 2)) * 60) + (VAL(LEFT$(time2$, 2)) * 3600)
PRINT "Numero de segundos="; segundos2 - segundos1
