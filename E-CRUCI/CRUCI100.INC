CALL QSOPEN (3,MCruci$+".CRU",1536)
CALL QSReadIndex(MCruci$+".NDX",MIndexCruci%())

IF menu000% = 1 THEN
 $INCLUDE "CRUCI101.INC"		'** Opci¢n 1 del Men£ General
ELSE
 $INCLUDE "CRUCI102.INC"		'** Opci¢n 2 del Men£ General
END IF

CALL ReadDatDicci(1,"DICCI000")
CALL ReadDatDicci(2,MCDic$)

'*** Aqu¡ graba la pantalla de las ventanas.
 CALL WSaveWindows
 CLS
'***

'** Aqu¡ empieza la apertura del crucigrama en pantalla
'** y se llama a la subrrutina editora del crucigrama
SiNo$ = ""
DO
  CALL ImprimirCruci(CY%,CX%)
  CALL ImprimirDatosCruci(CY%,CX%)
  CALL CursorCruci(CY%,CX%)
  SiNo$ = FNWSiNo$(0,0,"Quieres volver al Men£ General ")
  IF FNWSiNo$(0,0,"Quieres grabar el crucigrama ") = "N" THEN FIN101:
  CLS
LOOP UNTIL SiNo$  = "S"
GOTO FIN100:






FIN100:
'** Graba el Diccionario en el fichero
$INCLUDE "CRUCI103.INC"			
'**

'** Graba los Indices del Fichero de crucigramas y de los diccionarios
$INCLUDE "CRUCI104.INC"
'**

'** Abre otra vez las ventanas del Men£ General
CALL WRestoreWindows
'**

FIN101:
FIN102:
CLOSE
'**
