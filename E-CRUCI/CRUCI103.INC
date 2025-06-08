
INCR ActPro%,1
Call PutPro("E-CRUCI.EXE",ActPro%,MaxPro%)
IF ActPro% >= MaxPro% THEN
   Call WPro(MPrograma$)  	
ELSE
 CALL PutCruciDat
 IF MLOC.3% <>  0 THEN
        PUT #3,MLOC.3%
        CALL QSModDat(3,"1,8,",MIndexCruci%())
 ELSE
        MLOC.3% = FNQSEof(3)
        PUT #3,MLOC.3%
        CALL QSAddDat(3,"1,8,",MIndexCruci%())
 END IF
END IF

ERASE MCruciDat$ 			'Borra el contenido del crucigrama de
ERASE MCruciDef%			'la memoria y vuelve a dimensionar
DIM MCruciDat$(11,19)			'los ARRAYS que lo contenian
DIM MCruciDef%(11,19,2)