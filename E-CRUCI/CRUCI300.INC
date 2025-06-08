
OPEN "R",#3,MCruci$+".CRU",1536
CALL QSDefFields(3)
CALL QSReadIndex(MCruci$+".NDX",MIndexCruci%())

DO

CALL OpenWindowCruci

 IF menu000% <> 0 THEN
   MCNom$ = LEFT$(Cruci$(menu000%),8)
   IF FNWSiNo$(0,0,"Seguro que quieres borrar el crucigrama "+MCNom$+" ?") = "S" THEN
	MLOC.3% = CVI(LEFT$(Cruci$(menu000%),2))
	GET 3,MLOC.3%
	CALL GetCruciDat
	MCEst$ = "*"
	CALL PutCruciDat
	PUT 3,MLOC.3%
   END IF
 END IF
LOOP UNTIL menu000% = 0

FIN300:
CLOSE


