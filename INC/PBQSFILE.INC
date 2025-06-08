'$INCLUDE "REGNAMES.INC"	'Es utilizado por TBGETDIR.INC
'$INCLUDE "TBGETDIR.INC"	'Es utilizado por QSReadDir y FNQSEof


'Estas rutinas deben ser compiladas con el TB 1.0, ya que el TB 1.1 tiene
'un grave error....
'Al cerrar un fichero con CLOSE no recupera el espacio definido para las
'variables del fichero mediante FIELD, con lo que el espacio para contener
'las variables alfanumricas se va reduciendo hasta provocar un error.
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina.....: QSOpen                  Abre un fichero para QSTBFILE   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ   FileNum%   ====>  Nฃmero con el que ser  abierto el fichero             บ
'บ   File$      ====>  Nombre y extensiขn del fichero a abrir                บ
'บ   LenReg%    ====>  Longitud de cada registro                             บ
'บ                                                                           บ
'บ  Abre un fichero en modo RANDOM y guarda la longitud de los registros     บ
'บ  y el nombre del fichero en el Array QSFileDat$() para que estos datos    บ
'บ  puedan ser utilizados por las dem s rutinas sin necesidad de volver a    บ
'บ  pasarlos al llamarlas.              			             บ
'บ             *************************************************             บ
'บ             ********   NOTAS IMPORTANTES   ******************             บ
'บ             *************************************************             บ
'บ Es imprescindible dimensionar al principio del programa un Array llamado  บ
'บ QSFileDat%(NumeroFicheros) donde 'NumeroFicheros' ser  el nฃmero m ximo   บ
'บ de ficheros que abriremos.                                                บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB QSOpen(FileNum%,File$,LenReg%)
 SHARED QSFileDat%()
 QSFileDat%(FileNum%) = LenReg%
 OPEN "R",FileNum%,File$,LenReg%
 CALL QSDefFields(FileNum%)
END SUB



'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina.....: QSReadIndex              Lee el Indice de un Fichero    บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB QSReadIndex(NameFile$,Index%(1))
LOCAL X%
LOCAL sattrib%,f$,fattrib%,size&,date%,time%,e%
CALL FindFile(NameFile$,sattrib%,f$,fattrib%,size&,date%,time%,e%)
IF e% = 0 THEN
  DEF SEG = VARSEG(Index%(1))
  BLOAD NameFile$, VARPTR(Index%(1))
  DEF SEG
END IF
FOR X% = UBOUND(Index%(1)) TO 1 STEP -1
   IF Index%(X%) = 0 THEN
      Index%(X%) = X%
   ELSE
      EXIT FOR
   END IF
NEXT X%
END SUB

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina.....: QSSaveIndex              Grava el Indice de un Fichero  บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB QSSaveIndex(NameFile$,Index%(1))
'LOCAL TypeArray%	
  DEF SEG = VARSEG(Index%(1))
'  TypeArray% = VARPTR(Index%
  BSAVE NameFile$, VARPTR(Index%(1)),UBOUND(Index%(1))*2  '2 bytes para cada
  DEF SEG                                                 'campo del Array
END SUB



'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina.....: QSReIndex           Indexa un fichero por varios campos บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  El fichero a Indexar debe estar abierto con QSOpen                       บ
'บ  En la variable DatCase$ pasaremos separados por comas "," la posicion    บ
'บ  inicial del campo y su longitud dentro del registro.  Ejemplo....:       บ
'บ                1         11        21                                     บ
'บ  Registro es "Esteve    Valentก   Morales  "   y queremos indexarlo por   บ
'บ  los apellidos y el nombre.                                               บ
'บ                                  DatCase$ = "11,20,1,10,"                 บ
'บ                                                                           บ
'บ  Es necesario poner una coma "," en el final de DatCase$                  บ
'บ                                                                           บ
'บ      Filenum% ===> Numero con que fu abierto el fichero.                 บ
'บ      DatCase$ ===> Variable por la que se ordenar  el Indice.             บ
'บ      Index%(1)===> Array que contiene el Indice. Leido por QSReadIndex    บ
'บ                 *************************************                     บ
'บ                 ****** NOTAS IMPORTANTES ************                     บ
'บ                 *************************************                     บ
'บ  Debido a que este procedimiento es recursivo (se llama a el mismo) de-   บ
'บ  beremos poner al principio del programa $STACK 32766                     บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB QSReIndex(FileNum%,DatCase$,Index%(1))
  LOCAL X%
  IF FNQSEof(FileNum%) > 1 THEN
     FOR X% = 1 TO UBOUND(Index%(1))
	Index%(X%) = X%
     NEXT X%
     CALL QSReIndex2(FileNum%,DatCase$,1,FNQSEof(FileNum%)-1,Index%())
  END IF
END SUB
'*** Subrrutina recursiva utilizada por QSReIndex ***
SUB QSReIndex2(FileNum%,DatCase$,IPos%,FPos%,Index%(1))
  LOCAL I%,F%,C%,CDat$
  I% = IPos%
  F% = FPos%
  CDat$ = FNDatForIndex$(FileNum%,DatCase$,Index%(INT((IPos%+FPos%)/2)))

  DO
    DO WHILE FNDatForIndex$(FileNum%,DatCase$,Index%(I%))  <  CDat$
        INCR I%
    LOOP
    DO WHILE FNDatForIndex$(FileNum%,DatCase$,Index%(F%))  >  CDat$
        DECR F%
    LOOP
     IF I% <= F%  THEN
       SWAP Index%(I%),Index%(F%)
        INCR I%
        DECR F%
     END IF
   LOOP UNTIL I% > F%

  IF IPos% < F%  THEN
     CALL QSReIndex2(FileNum%,DatCase$,IPos%,F%,Index%())
  END IF

  IF I% < FPos%  THEN
     CALL QSReIndex2(FileNum%,DatCase$,I%,FPos%,Index%())
  END IF
END SUB


'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina.....: QSFile              Ordena un fichero por varios campos บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  El fichero a ordenar debe estar abierto con QSOpen                       บ
'บ  En la variable DatCase$ pasaremos separados por comas "," la posicion    บ
'บ  inicial del campo y su longitud dentro del registro.  Ejemplo....:       บ
'บ                1         11        21                                     บ
'บ  Registro es "Esteve    Valentก   Morales  "   y queremos indexarlo por   บ
'บ  los apellidos y el nombre.                                               บ
'บ                                  DatCase$ = "11,20,1,10,"                 บ
'บ                                                                           บ
'บ  Es necesario poner una coma "," en el final de DatCase$                  บ
'บ                                                                           บ
'บ      Filenum% ===> Numero con que fu abierto el fichero.                 บ
'บ      DatCase$ ===> Variable por la que se ordenar  el Indice.             บ
'บ                 *************************************                     บ
'บ                 ****** NOTAS IMPORTANTES ************                     บ
'บ                 *************************************                     บ
'บ  Debido a que este procedimiento es recursivo (se llama a el mismo) de-   บ
'บ  beremos poner al principio del programa $STACK 32766                     บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB QSFile(FileNum%,DatCase$)
  LOCAL X%
  IF FNQSEof(FileNum%) > 1 THEN
     CALL QSFile2(FileNum%,DatCase$,1,FNQSEof(FileNum%)-1)
  END IF
END SUB
'*** Subrrutina recursiva utilizada por QSReIndex ***
SUB QSFile2(FileNum%,DatCase$,IPos%,FPos%)
  LOCAL I%,F%,C%,CDat$
  I% = IPos%
  F% = FPos%
  CDat$ = FNDatForIndex$(FileNum%,DatCase$,(IPos%+FPos%)\2)

  DO
    DO WHILE FNDatForIndex$(FileNum%,DatCase$,I%)  <  CDat$
        INCR I%
    LOOP
    DO WHILE FNDatForIndex$(FileNum%,DatCase$,F%)  >  CDat$
        DECR F%
    LOOP
     IF I% <= F%  THEN
       CALL QSSwapRecord(FileNum%,I%,F%)
        INCR I%
        DECR F%
     END IF
   LOOP UNTIL I% > F%

  IF IPos% < F%  THEN
     CALL QSFile2(FileNum%,DatCase$,IPos%,F%)
  END IF

  IF I% < FPos%  THEN
     CALL QSFile2(FileNum%,DatCase$,I%,FPos%)
  END IF
END SUB


'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina.....: QSAddDat          Aคade registros a un fichero Indexado บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  El fichero a ordenar debe estar abierto.                                 บ
'บ  En la variable DatCase$ pasaremos separados por comas "," la posicion    บ
'บ  inicial del campo y su longitud dentro del registro.  Ejemplo....:       บ
'บ                1         11        21                                     บ
'บ  Registro es "Esteve    Valentก   Morales  "   y el indice esta creardo   บ
'บ  con los apellidos y el nombre.                                           บ
'บ                                  DatCase$ = "11,20,1,10,"                 บ
'บ                                                                           บ
'บ  Es necesario poner una coma "," en el final de DatCase$                  บ
'บ                                                                           บ
'บ      Filenum% ===> Numero con que fu abierto el fichero.                 บ
'บ      DatCase$ ===> Variable por la que se ordenar  el Indice.             บ
'บ      Index%(1)===> Array que contiene el Indice.                          บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB QSAddDat(FileNum%,DatCase$,Index%(1))
  LOCAL X%
  SHARED QSLocInIndex%

  CALL QSFindDat(FileNum%,DatCase$,Index%(),FNDatForIndex$(FileNum%,DatCase$,LOC(FileNum%)),e%)

  FOR X%=FNQSEof(FileNum%)-1 TO QSLocInIndex% STEP-1
    Index%(X%) = Index%(X%-1)
  NEXT X%
    Index%(QSLocInIndex%) = FNQSEof(FileNum%)-1
END SUB


'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina.....: QSModDat      Modifica registros de un fichero Indexado บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  El fichero a ordenar debe estar abierto.                                 บ
'บ  En la variable DatCase$ pasaremos separados por comas "," la posicion    บ
'บ  inicial del campo y su longitud dentro del registro.  Ejemplo....:       บ
'บ                1         11        21                                     บ
'บ  Registro es "Esteve    Valentก   Morales  "   y el indice esta creardo   บ
'บ  con los apellidos y el nombre.                                           บ
'บ                                  DatCase$ = "11,20,1,10,"                 บ
'บ                                                                           บ
'บ  Es necesario poner una coma "," en el final de DatCase$                  บ
'บ                                                                           บ
'บ      Filenum% ===> Numero con que fu abierto el fichero.                 บ
'บ      DatCase$ ===> Variable por la que se ordenar  el Indice.             บ
'บ      Index%(1)===> Array que contiene el Indice.                          บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB QSModDat(FileNum%,DatCase$,Index%(1))
 LOCAL X%,PFile%,P%
 SHARED QSLocInIndex%
  PFile% = LOC(FileNum%)
  FOR P% = 1 TO FNQSEof(FileNum%)-1
    IF LOC(Filenum%) = Index%(P%) THEN EXIT FOR		'Busca la posiciขn en Indice
  NEXT P%
  FOR X% = P% TO FNQSEof(FileNum%)-1
    Index%(X%) = Index%(X%+1)				'Modifica el Indice
  NEXT X%

  CALL QSFindDat(FileNum%,DatCase$,Index%(),FNDatForIndex(FileNum%,DatCase$,LOC(FileNum%)),e%)

  FOR X%=FNQSEof(FileNum%)-1 TO QSLocInIndex% STEP-1
    Index%(X%) = Index%(X%-1)				'Reindexa el Indice
  NEXT X%
  Index%(QSLocInIndex%) = PFile%
END SUB



'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Funciขn......: FNQSFindDat         Busca un Dato y retorna su posiciขn   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  El fichero a ordenar debe estar abierto.                                 บ
'บ  En la variable DatCase$ pasaremos separados por comas "," la posicion    บ
'บ  inicial del campo y su longitud dentro del registro.  Ejemplo....:       บ
'บ                1         11        21                                     บ
'บ  Registro es "Esteve    Valentก   Morales  "   y queremos indexarlo por   บ
'บ  los apellidos y el nombre.                                               บ
'บ                                  DatCase$ = "11,20,1,10,"                 บ
'บ                                                                           บ
'บ  Es necesario poner una coma "," en el final de DatCase$                  บ
'บ                                                                           บ
'บ      Filenum% ===> Numero con que fu abierto el fichero.                 บ
'บ      DatCase$ ===> Variable por la que se ordenar  el Indice.             บ
'บ      Index%(1)===> Array que contiene el Indice.                          บ
'บ      DatToFind$==> Dato a Buscar, se comparara con la cadena que se consi-บ
'บ                    ga con DatCase$.                                       บ
'บ      e%       ===> Devuelve 1 si no se localiza el dato (error)           บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB QSFindDat(FileNum%,DatCase$,Index%(1),DatToFind$,e%)
  LOCAL C%,I%,F%,QSDat$
  SHARED QSLocInIndex%
  e% = 1
  QSLocInIndex% = 1						'Si solo hay un registro apuntar  a este
IF FNQSEof(FileNum%) > 1 THEN					'Si existen datos en el fichero
  I% = 1                					'Primer Dato Indexado
  F% = FNQSEof(FileNum%)    					'Ultimo dato indexado
  QSDat$ = DatToFind$						'Dato a comparar

  IF QSDat$<=FNDatForIndex$(FileNum%,DatCase$,Index%(1))THEN	'Si el dato es m s pequeคo o igual
     IF QSDat$=FNDatForIndex$(FileNum%,DatCase$,Index%(1))THEN e% = 0
     C% = 1							'que el primer dato indexado
  END IF
 IF C% = 0 THEN
  IF QSDat$>FNDatForIndex$(FileNum%,DatCase$,Index%(F%-1))THEN	'Si el dato es m s grande
     C% = FNQSEof(FileNum%)- 1 					'que el ฃltimo dato indexado
  END IF
 END IF
 IF C% = 0 THEN							'Si no es m s pequeคo que
    DO								'el primero ni m s grande		
      C% = (I%+F%)\2						'que el ultimo, lo busca.
      IF QSDat$ > FNDatForIndex$(FileNum%,DatCase$,Index%(C%)) THEN
        I% = C%
      ELSE
        F% = C%
      END IF
    LOOP UNTIL QSDat$  <=  FNDatForIndex$(FileNum%,DatCase$,Index%(C%)) _
          AND  QSDat$  >   FNDatForIndex$(FileNum%,DatCase$,Index%(C%-1))
    IF QSDat$ = FNDatForIndex$(FileNum%,DatCase$,Index%(C%)) THEN  e% = 0
 END IF
  QSLocInIndex% = C%
  GET FileNum%,Index%(C%)
END IF
END SUB



'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina.....: QSSkip            Busca el siguiente o anterior registroบ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  El fichero a ordenar debe estar abierto.                                 บ
'บ  En la variable DatCase$ pasaremos separados por comas "," la posicion    บ
'บ  inicial del campo y su longitud dentro del registro.  Ejemplo....:       บ
'บ                1         11        21                                     บ
'บ  Registro es "Esteve    Valentก   Morales  "   y queremos indexarlo por   บ
'บ  los apellidos y el nombre.                                               บ
'บ                                  DatCase$ = "11,20,1,10,"                 บ
'บ                                                                           บ
'บ  Es necesario poner una coma "," en el final de DatCase$                  บ
'บ                                                                           บ
'บ      Filenum% ===> Numero con que fu abierto el fichero.                 บ
'บ      Index%(1)===> Array que contiene el Indice.                          บ
'บ      SKipNum% ===> Cantidad de registros a avanzar o retroceder           บ
'บ      e%       ===> Error. Retorna el valor de EOF(Filenum%)               บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB QSSkip(FileNum%,Index%(1),SkipNum%,e%)
SHARED QSLocInIndex%
IF QSLocInIndex% = 0 THEN QSLocInIndex% = 1
e% = 0
 IF Index%(QSLocInIndex%) <> LOC(FileNum%) OR LOC(FileNum%) = 0 THEN
    FOR QSLocInIndex% = 1 TO FNQSEof(FileNum%)-1
       IF LOC(FileNum%) = Index%(QSLocInIndex%) THEN EXIT FOR	'Busca la posiciขn en Indice
    NEXT QSLocInIndex%
 END IF
 IF QSLocInIndex%+SkipNum% < FNQSEof(FileNum%) AND  QSLocInIndex%+SkipNum% > 0 THEN
    QSLocInIndex% = QSLocInIndex% + SkipNum%
    GET FileNum%,Index%(QSLocInIndex%)
 ELSE
    IF P%+SKipNum% <= 0 THEN
        e% = -1
    ELSE
        e% = 1
    END IF
 END IF
END SUB





'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina y Funciones de uso interno de paquete TBQSFILE                บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Funciขn .....:  FNDatForIndex$                                           บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  Lee el registro NumReg% y extrae de el los datos, seleccionados en       บ
'บ  DatCase$, que ser n retornados por la Funciขn para hacer las comparacio- บ
'บ  nes en las otras subrrutinas del paquete TBQSFILE.                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
DEF FNDatForIndex$(FileNum%,DatCase$,NumReg%)
LOCAL DFI$,P%,I%,L%
LOCAL QSFileReg$,QSLenReg%
SHARED QSFileDat()
QSLenReg% = CVI (LEFT$(QSFileDat$(FileNum%),2))
FIELD FileNum%, QSLenReg% AS QSFileReg$
GET FileNum%,NumReg%
P% = 1
DO
  I% = VAL( MID$(DatCase$,P%,INSTR(P%,DatCase$,",")) )
  P% = INSTR(P%,DatCase$,",")+1
  L% = VAL( MID$(DatCase$,P%,INSTR(P%,DatCase$,",")) )
  P% = INSTR(P%,DatCase$,",")+1
  DFI$ = DFI$ + MID$(QSFileReg$,I%,L%)
LOOP UNTIL INSTR(P%,DatCase$,",") = 0
FNDatForIndex$ = DFI$
END DEF


'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina...:  QSSwapRecord     Intercambia dos registros de un fichero บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  Lee el registro situado en LOC1% y lo guarda en TEMP$, Lee el registro   บ
'บ  situado en LOC2% y lo graba en la posiciขn LOC1%, luego graba TEMP$ en   บ
'บ  la posiciขn LOC2%.                                                       บ
'บ  Utilizado por QSFile.                                                    บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB QSSwapRecord (Filenum%,LOC1%,LOC2%)
LOCAL TEMP$,QSLenReg%,QSFileReg$
SHARED QSFileDat$()
 QSLenReg% = CVI (LEFT$(QSFileDat$(FileNum%),2))
 FIELD FileNum%, QSLenReg% AS QSFileReg$
 GET FileNum%,LOC1%
  TEMP$ = QSFileReg$
 GET FileNum%,LOC2%
  LSET QSFileReg$ = QSFileReg$
   PUT FileNum%,LOC1%
  LSET QSFileReg$ = TEMP$
   PUT FileNum%,LOC2%
END SUB



'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Funciขn .....:  FNQSEof(FileNum%)                                        บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ Retorna el fin del fichero + 1.                                           บ
'บ Solo es utilizable si el fichero ha sido abierto con QSOpen.              บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
DEF FNQSEof(FileNum%)
  SHARED QSFileDat%()
  FNQSEof = ( LOF( FileNum% ) \ QSFileDat%( FileNum% )) + 1
END DEF



'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina...:  QSDefFields                                              บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ En esta Subrrutina definiremos los campos de cada registro de cada ficheroบ
'บ Deberemos declarar los campos de cada fichero como SHARED.                บ
'บ Por ejemplo....:                                                          บ
'บ                                                                           บ
'บ  SUB QSDefFields(FileNum%)                                                บ
'บ   SHARED Nomb.1$,Ape1.1$,Ape2.1$     'Campos del fichero #1               บ
'บ   SHARED NCli.2$,Codi.2$,Dato.2$     'Campos del fichero #2               บ
'บ   SHARED .......                     'Campos del fichero #3               บ
'บ   SELECT CASE FileNum%                                                    บ
'บ     CASE 1                                                                บ
'บ       FIELD #1,10 AS Nomb.1$,10 AS Ape1.1$,10 AS Ape2.1$...               บ
'บ     CASE 2                                                                บ
'บ       FIELD #2,5 AS NCli.2$,7 AS Codi.2$,10 AS Dato.2$....                บ
'บ     CASE 3                                                                บ
'บ       .........                                                           บ
'บ   END SELECT                                                              บ
'บ  END SUB                                                                  บ
'บ                                                                           บ
'บ Esta Subrrutina se deber  modificar para cada programa en que vayamos a   บ
'บ utilizar el paquete TBQSFILE.                                             บ
'บ Cuando abramos un fichero desde el programa podemos definir los campos    บ
'บ de la siguiente forma...:                                                 บ
'บ        OPEN #1,"R","FICHERO.DAT",512          'Abrimos el fichero         บ
'บ        CALL QSDefField$(1)                    'Definimos los campos del F.บ
'บ Una vez abierto el fichero y definidos los campos de este no har  falta   บ
'บ volver a definirlos cada vez que llamemos a QSFindDat o QSAddDat o        บ
'บ QSModDat, ya que estas subrrutinas llaman a QSDefFields automaticamente.  บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB QSDefFields(FileNum%)
 SHARED PALA$,DEFI$,ESTA$,NULO$
 SHARED CNom$,CDef$,CDic$,CY$,CX$,CDat$,CXDe$,CYDe$,CEst$,CNul$

 SELECT CASE FileNum%
   CASE 1
     FIELD #1,19 AS PALA$,75 AS DEFI$,1 AS ESTA$,5 AS NULO$
   CASE 2
     FIELD #2,19 AS PALA$,75 AS DEFI$,1 AS ESTA$,5 AS NULO$
   CASE 3
     FIELD #3,8 AS CNom$,50 AS CDef$,8 AS CDic$,2 AS CY$,2 AS CX$,209 AS CDat$_
             ,418 AS CXDe$,418 AS CYDe$,1 AS CEst$,420 AS CNul$
 END SELECT	
END SUB
