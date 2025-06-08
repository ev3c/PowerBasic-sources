
CALL WOpen(9,20,20,36,2,1,0,7,"DICCIONARIOS")

DO
CALL WMENU(MenuDicci2$(),0,menudicci%)
IF menudicci% <> 0 THEN
  MNumDicci% = menudicci% - 1
  MDicci$ = "DICCI00"+MID$(STR$(menudicci%-1),2,1)
  CALL ReadDatDicci(1,MDicci$)
  CALL WOpen(11,25,20,46,2,1,15,0,"Men£ Crucigrama")
  DO
   CALL WMenu(Menu400$(),0,menu400%)
	IF menu400% <> 0 THEN
		CALL WSaveWindows
		CLS
		CALL ImprimirPantalla(Menu400%)
		CALL ImprimirOpciones(Menu400%)
		CALL ImprimirNombreDicci(MNumDicci%)
	END IF
	SELECT CASE menu400%
	  CASE 1
	    $INCLUDE "CRUCI410.INC"
	  CASE 2
	    $INCLUDE "CRUCI420.INC"
	  CASE 3
	    $INCLUDE "CRUCI430.INC"
	  CASE 4
	    $INCLUDE "CRUCI440.INC"
	END SELECT
	IF menu400% <> 0 THEN CALL WRestoreWindows
  LOOP UNTIL WMENU% = 0
  CALL Wclose
  CLOSE #1
  IF MDicci$ = "DICCI000" THEN
     CALL QSSaveIndex(MDicci$+".NDX",MIndexDicciGE%())
  ELSE
     CALL QSSaveIndex(MDicci$+".NDX",MIndexDicci%())
  END IF
END IF
LOOP UNTIL menudicci% = 0

CALL Wclose





