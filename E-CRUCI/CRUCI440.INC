
SiNo$ = "S"
DO
4400:
CALL Consultar(WMenu%)

IF RET%<>1 THEN

IF ESTA$ = "*" THEN
 CALL Message("La palabra esta dada de BAJA. Quieres Recuperarla (S/N)")
 DO
   SiNo$ = UCASE$(INKEY$)
 LOOP UNTIL SiNo$="S" OR SiNo$="N"
END IF

IF SiNo$ = "S" THEN
    LSET ESTA$ = ""
    PUT #1, LOC(1)


4401:
	DO
	  CALL Message("Teclea la Palabra y pulsa INTRO")
	  Palabra$ = FNInput$(12,31,19,0,7,15,0,0,1)
	  ON  RET% GOTO 4400,4401
	LOOP UNTIL FNTrim$(Palabra$) <> ""

4402:
	DO
	  CALL Message("Teclea la definici¢n de la palabra y pulsa INTRO")
	  Definicion$ = FNInput$(17,3,75,0,7,15,0,0,0)
	  ON RET% GOTO 4400,4401
	LOOP UNTIL FNTrim$(Definicion$) <> ""

           				'***Graba el Registro
	LSET PALA$ = Palabra$
	LSET DEFI$ = Definicion$
	PUT #1, LOC(1)
					'***Modifica el registro en el Indice
	CALL QSModDat(1,"1,19,",MIndexDicci%())

 END IF
END IF
LOOP UNTIL RET% = 1
