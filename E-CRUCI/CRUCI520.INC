CALL WCLOSE
CALL WCLOSE
CALL WCLOSE
CLS

IF FNUnidad$ = "A" THEN
	SHELL "RESTORE B:"
ELSE
	SHELL "RESTORE A:"
END IF

CALL WOPEN(1,1,3,80,1,0,0,7,"")
COLOR 0,7
LOCATE 2,5
PRINT MPrograma$;"               ";FNDia$(VAL(MID$(DATE$,4,2)),VAL(LEFT$(DATE$,2)),VAL(RIGHT$(DATE$,2)));"    ";MID$(DATE$,4,2);" de ";FNMes$(VAL(LEFT$(DATE$,2)));" de ";RIGHT$(DATE$,4);
CALL WOPEN(4,2,15,30,2,1,15,0," Men£ General ")
CALL WOpen(11,20,21,75,2,1,15,0,"Menu de Utilidades")

WMENU% = 0		'Para que vuelva a WMenu del Menu General
