MLOC.3% = 0

CALL WOPEN(8,20,10,58,1,0,15,0,"")
LOCATE 9,22
PRINT "Nombre del Crucigrama...:"
CALL WOPEN(11,20,14,71,1,0,15,0,"")
LOCATE 12,27
PRINT "Definici¢n que identifica al Crucigrama";
CALL WOPEN(15,20,18,75,1,0,15,0,"")
LOCATE 16,22
PRINT "N£mero de recuadros Horizontales (1 a 19)...:"
LOCATE 17,22
PRINT "N£mero de recuadros Verticales   (1 a 11)...:"


1011:
 DO
   MCNom$ = EINPUT2$(9,48,8,0,7,15,0,0,1,0,0,e%)
   IF RET% = 1 THEN CALL WCLOSE:CALL WCLOSE:CALL WCLOSE:GOTO FIN101
 LOOP UNTIL FNTrim$(MCNom$) <> ""
  CALL QSFindDat(3,"1,8,",MIndexCruci%(),MCNom$,e%)
 IF e% = 0  THEN
   CALL WMessage(0,0,"El Crucigrama ya existe en el Fichero.")
   GOTO 1011:
 END IF

1012:
 DO
   MCDef$ = FNInput$(13,21,50,0,7,15,0,0,0)
   IF RET% = 2 THEN 1011:
   IF RET% = 1 THEN CALL WCLOSE:CALL WCLOSE:CALL WCLOSE:GOTO FIN101
 LOOP UNTIL FNTrim$(MCDef$) <> ""

1013:
 DO
   MCX$ = FNInput$(16,68,2,0,7,15,0,1,0)
   CX% = VAL(MCX$)
   IF RET% = 2 THEN 1012
   IF RET% = 1 THEN CALL WCLOSE:CALL WCLOSE:CALL WCLOSE:GOTO FIN101
 LOOP UNTIL CX% > 0 AND CX% < 20

1014:
 DO
   MCY$ = FNInput$(17,68,2,0,7,15,0,1,0)
   CY% = VAL(MCY$)
   IF RET% = 2 THEN 1013
   IF RET% = 1 THEN CALL WCLOSE:CALL WCLOSE:CALL WCLOSE:GOTO FIN101
 LOOP UNTIL CY% > 0 AND CY% < 12

1015:
 SiNo$ = FNWSiNo$(0,0,"Quieres selecionar otro Diccionario, aparte del General?")
   IF RET% = 2 THEN 1014
   IF RET% = 1 THEN CALL WCLOSE:CALL WCLOSE:CALL WCLOSE:GOTO FIN101
 IF SiNo$ = "S" THEN
   CALL OpenWindowDicci(8,20,0)
   IF menu000% = 0 THEN 1015:
 END IF

CALL WCLOSE:CALL WCLOSE:CALL WCLOSE
