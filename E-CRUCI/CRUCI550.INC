551:
SiNo$ = FNWSiNo$ (0,0,"ReIndexo el fichero de CRUCIGRAMAS ?")
IF RET% = 1 OR RET% = 2 THEN FIN550:
IF SiNo$ = "S" THEN
   CALL QSOPEN (3,"CRUCI.CRU",1536)
   CALL QSReadIndex("CRUCI.NDX",MIndexCruci%())
   CALL WMSG(0,0,"Este proceso es largo y puede durar varias horas, ten paciencia.")
   CALL QSReIndex(3,"1,8,",MIndexCruci%())
   CALL QSSaveIndex("CRUCI.NDX",MIndexCruci%())
   CLOSE 3
END IF

552:
SiNo$ = FNWSiNo$ (0,0,"ReIndexo el DICCIONARIO GENERAL ?")
IF RET% = 1 THEN FIN550
IF RET% = 2 THEN 551
IF SiNo$ = "S" THEN
   CALL QSOPEN (1,"DICCI000.DIC",100)
   CALL QSReadIndex("DICCI000.NDX",MIndexDicciGE%())
   CALL WMSG(0,0,"Este proceso es largo y puede durar varias horas, ten paciencia.")
   CALL QSReIndex(1,"1,19,",MIndexDicciGE%())
   CALL QSSaveIndex("DICCI000.NDX",MIndexDicciGE%())
   CLOSE 1
END IF

553:
SiNo$ = FNWSiNo$ (0,0,"ReIndexo el DICCIONARIO PRIMERO ?")
IF RET% = 1 THEN FIN550
IF RET% = 2 THEN 552
IF SiNo$ = "S" THEN
   CALL QSOPEN (1,"DICCI001.DIC",100)
   CALL QSReadIndex("DICCI001.NDX",MIndexDicci%())
   CALL WMSG(0,0,"Este proceso es largo y puede durar varias horas, ten paciencia.")
   CALL QSReIndex(1,"1,19,",MIndexDicci%())
   CALL QSSaveIndex("DICCI001.NDX",MIndexDicci%())
   CLOSE 1
END IF

554:
SiNo$ = FNWSiNo$ (0,0,"ReIndexo el DICCIONARIO SEGUNDO ?")
IF RET% = 1 THEN FIN550
IF RET% = 2 THEN 553
IF SiNo$ = "S" THEN
   CALL QSOPEN (1,"DICCI002.DIC",100)
   CALL QSReadIndex("DICCI002.NDX",MIndexDicci%())
   CALL WMSG(0,0,"Este proceso es largo y puede durar varias horas, ten paciencia.")
   CALL QSReIndex(1,"1,19,",MIndexDicci%())
   CALL QSSaveIndex("DICCI002.NDX",MIndexDicci%())
   CLOSE 1
END IF

555:
SiNo$ = FNWSiNo$ (0,0,"ReIndexo el DICCIONARIO TERCERO ?")
IF RET% = 1 THEN FIN550
IF RET% = 2 THEN 554
IF SiNo$ = "S" THEN
   CALL QSOPEN (1,"DICCI003.DIC",100)
   CALL QSReadIndex("DICCI003.NDX",MIndexDicci%())
   CALL WMSG(0,0,"Este proceso es largo y puede durar varias horas, ten paciencia.")
   CALL QSReIndex(1,"1,19,",MIndexDicci%())
   CALL QSSaveIndex("DICCI003.NDX",MIndexDicci%())
   CLOSE 1
END IF

556:
SiNo$ = FNWSiNo$ (0,0,"ReIndexo el DICCIONARIO CUARTO ?")
IF RET% = 1 THEN FIN550
IF RET% = 2 THEN 555
IF SiNo$ = "S" THEN
   CALL QSOPEN (1,"DICCI004.DIC",100)
   CALL QSReadIndex("DICCI004.NDX",MIndexDicci%())
   CALL WMSG(0,0,"Este proceso es largo y puede durar varias horas, ten paciencia.")
   CALL QSReIndex(1,"1,19,",MIndexDicci%())
   CALL QSSaveIndex("DICCI004.NDX",MIndexDicci%())
   CLOSE 1
END IF

557:
SiNo$ = FNWSiNo$ (0,0,"ReIndexo el DICCIONARIO QUINTO ?")
IF RET% = 1 THEN FIN550
IF RET% = 2 THEN 556
IF SiNo$ = "S" THEN
   CALL QSOPEN (1,"DICCI005.DIC",100)
   CALL QSReadIndex("DICCI005.NDX",MIndexDicci%())
   CALL WMSG(0,0,"Este proceso es largo y puede durar varias horas, ten paciencia.")
   CALL QSReIndex(1,"1,19,",MIndexDicci%())
   CALL QSSaveIndex("DICCI005.NDX",MIndexDicci%())
   CLOSE 1
END IF
FIN550:   	   	   			
