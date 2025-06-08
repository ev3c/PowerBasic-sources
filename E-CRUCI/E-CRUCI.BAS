
$STACK 32766
$EVENT ON

OPTION BASE 1
'**** Dimensiona les variables que s'utilitzaran al PROGRAMA
MPrograma$ = " E-CRUCI versi¢n 1.00 "	'El nom y la Versi¢ del programa
MERR% = 0				'Para tratamiento de errores

MCruci$  = "CRUCI"				'Nombre Fichero Crucigramas
DIM MIndexCruci%(9999)				'Indice del F.Crucigramas
DIM MCruciDat$(11,19)				'Los datos de cada crucigrama
DIM MCruciDef%(11,19,2)				'Las definiciones de cada crucigrama
						'1=Horizontal	 2=Vertical
DIM QSFileDat%(10)
DIM Cruci$(99)

DIM MIndexDicciGE%(29999)			'Indice del Dic. General

MDicci$ = ""					'Nombre del Dic. Secundario
DIM MIndexDicci%(9999)				'Indice del Dic. Secundario



DIM  WIND$(20,2)			'Finestres per TBWindow
DIM  WDIR$(513)				'Directori per TBWindow
DIM  MENU000$(8)			'Men£ General
   Menu000$(1) = "C - Crear Crucigrama"
   Menu000$(2) = "E - Editar Crucigrama"
   Menu000$(3) = "B - Borrar Crucigrama"
   Menu000$(4) = "M - Men£ de Diccionarios"
   Menu000$(5) = "U - Men£ de Utilidades"
   Menu000$(6) = "A - Manual de Ayuda"
DIM MENU100$(8)

DIM MENU400$(5)				'Men£ de Diccionarios
   Menu400$(1) = "A - Altas         "
   Menu400$(2) = "B - Bajas         "
   Menu400$(3) = "C - Consultas     "
   Menu400$(4) = "M - Modificaciones"

DIM MENU500$(6)
   Menu500$(1) = "H - Hacer Copia de Seguridad del programa"
   Menu500$(2) = "R - Restaurar Copia de Seguridad del programa"
   Menu500$(3) = "L - Listar palabras de los diccionarios    "
   Menu500$(4) = "I - Inicializar ficheros diccionarios o crucigramas"
   Menu500$(5) = "E - Reindexar ficheros diccionarios o crucigramas"

DIM MenuDicci1$(6) 				'Diccionarios para seleccionar
   MenuDicci1$(1) = "1 - Primero "		'al editar una pantalla
   MenuDicci1$(2) = "2 - Segundo "
   MenuDicci1$(3) = "3 - Tercero "
   MenuDicci1$(4) = "4 - Cuarto  "
   MenuDicci1$(5) = "5 - Quinto  "

DIM MenuDicci2$(7)				'Diccionarios para seleccionar
   MenuDicci2$(1) = "0 - General "		'en las demas partes del programa
   MenuDicci2$(2) = "1 - Primero "
   MenuDicci2$(3) = "2 - Segundo "
   MenuDicci2$(4) = "3 - Tercero "
   MenuDicci2$(5) = "4 - Cuarto  "
   MenuDicci2$(6) = "5 - Quinto  "

'**** Final asignaci¢n variables


'$INCLUDE "CRUCI0  .INC"			'<<< Pantalla de Presentaci¢n


'*****  Llama a la protecci¢n del programa
'CALL GetPro("E-CRUCI.EXE",ActPro%,MaxPro%)
'IF MaxPro% = 0 THEN
'   MaxPro% = 5
'   CALL PutPro("E-CRUCI.EXE",ActPro%,MaxPro%)
'END IF
'*****


CLS
CALL WOPEN(1,1,3,80,1,0,0,7,"")
COLOR 0,7
LOCATE 2,5
PRINT MPrograma$;"               ";FNDia$(VAL(MID$(DATE$,4,2)),VAL(LEFT$(DATE$,2)),VAL(RIGHT$(DATE$,2)));"    ";MID$(DATE$,4,2);" de ";FNMes$(VAL(LEFT$(DATE$,2)));" de ";RIGHT$(DATE$,4);
CALL WOPEN(4,2,15,30,2,1,15,0," Men£ General ")

DO
 CALL WMENU(MENU000$(),0,menu000%)  	'>>>>>>>>>>>> Men£ General del Programa
 SELECT CASE menu000%
   CASE 1 OR  2
     $INCLUDE "CRUCI100.INC"
   case 3
     $INCLUDE "CRUCI300.INC"
   CASE 4
     $INCLUDE "CRUCI400.INC"
   CASE 5
     $INCLUDE "CRUCI500.INC"
   CASE 6
'    $INCLUDE "CRUCI600.INC"
 END SELECT

 IF menu000% = 0 THEN SiNo$=FNWSiNo$(0,0,"Quieres salir de "+Mprograma$+"   ")
LOOP UNTIL SiNo$ = "S"

CALL WCLOSE
CALL WCLOSE
COLOR 15,0
CLS
COLOR 0,7
PRINT " ";MPrograma$;"          "
PRINT "  Esteve Valent¡ Morales         "
PRINT "  Grup Sot dels canyers, 6/60    "
PRINT "  17220 - Sant Feliu de Gu¡xols  "
PRINT "  Girona - Catalunya - Spain     "
COLOR 15,0
PRINT
PRINT "  Gracias por utilizar ";MPrograma$
PRINT
PRINT
DELAY 1
END







'**** Subrrutinas y Funciones Internas del programa
$INCLUDE "CRUCI100.SUB"
$INCLUDE "CRUCI101.SUB"
$INCLUDE "CRUCI300.SUB"
$INCLUDE "CRUCI400.SUB"
'****

'**** Subrrutinas y Funciones Generales
$INCLUDE "REGNAMES.INC"
$INCLUDE "PBWINDOW.INC"
$INCLUDE "PBGETDIR.INC"
$INCLUDE "PBEINPUT.INC"
$INCLUDE "FGC&BGC .FN "
$INCLUDE "DIA$MES$.FN "
$SEGMENT
$INCLUDE "PEPOPRN .SUB"
$INCLUDE "PBQSFILE.INC"
$INCLUDE "PBPROTEC.INC"
'****

