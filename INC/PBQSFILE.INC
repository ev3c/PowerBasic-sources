'$INCLUDE "REGNAMES.INC"	'Es utilizado por TBGETDIR.INC
'$INCLUDE "TBGETDIR.INC"	'Es utilizado por QSReadDir y FNQSEof


'Estas rutinas deben ser compiladas con el TB 1.0, ya que el TB 1.1 tiene
'un grave error....
'Al cerrar un fichero con CLOSE no recupera el espacio definido para las
'variables del fichero mediante FIELD, con lo que el espacio para contener
'las variables alfanum�ricas se va reduciendo hasta provocar un error.
'���������������������������������������������������������������������������ͻ
'�  Subrrutina.....: QSOpen                  Abre un fichero para QSTBFILE   �
'���������������������������������������������������������������������������͹
'�   FileNum%   ====>  N�mero con el que ser� abierto el fichero             �
'�   File$      ====>  Nombre y extensi�n del fichero a abrir                �
'�   LenReg%    ====>  Longitud de cada registro                             �
'�                                                                           �
'�  Abre un fichero en modo RANDOM y guarda la longitud de los registros     �
'�  y el nombre del fichero en el Array QSFileDat$() para que estos datos    �
'�  puedan ser utilizados por las dem�s rutinas sin necesidad de volver a    �
'�  pasarlos al llamarlas.              			             �
'�             *************************************************             �
'�             ********   NOTAS IMPORTANTES   ******************             �
'�             *************************************************             �
'� Es imprescindible dimensionar al principio del programa un Array llamado  �
'� QSFileDat%(NumeroFicheros) donde 'NumeroFicheros' ser� el n�mero m�ximo   �
'� de ficheros que abriremos.                                                �
'���������������������������������������������������������������������������ͼ
SUB QSOpen(FileNum%,File$,LenReg%)
 SHARED QSFileDat%()
 QSFileDat%(FileNum%) = LenReg%
 OPEN "R",FileNum%,File$,LenReg%
 CALL QSDefFields(FileNum%)
END SUB



'���������������������������������������������������������������������������ͻ
'�  Subrrutina.....: QSReadIndex              Lee el Indice de un Fichero    �
'���������������������������������������������������������������������������ͼ
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

'���������������������������������������������������������������������������ͻ
'�  Subrrutina.....: QSSaveIndex              Grava el Indice de un Fichero  �
'���������������������������������������������������������������������������ͼ
SUB QSSaveIndex(NameFile$,Index%(1))
'LOCAL TypeArray%	
  DEF SEG = VARSEG(Index%(1))
'  TypeArray% = VARPTR(Index%
  BSAVE NameFile$, VARPTR(Index%(1)),UBOUND(Index%(1))*2  '2 bytes para cada
  DEF SEG                                                 'campo del Array
END SUB



'���������������������������������������������������������������������������ͻ
'�  Subrrutina.....: QSReIndex           Indexa un fichero por varios campos �
'���������������������������������������������������������������������������͹
'�  El fichero a Indexar debe estar abierto con QSOpen                       �
'�  En la variable DatCase$ pasaremos separados por comas "," la posicion    �
'�  inicial del campo y su longitud dentro del registro.  Ejemplo....:       �
'�                1         11        21                                     �
'�  Registro es "Esteve    Valent�   Morales  "   y queremos indexarlo por   �
'�  los apellidos y el nombre.                                               �
'�                                  DatCase$ = "11,20,1,10,"                 �
'�                                                                           �
'�  Es necesario poner una coma "," en el final de DatCase$                  �
'�                                                                           �
'�      Filenum% ===> Numero con que fu� abierto el fichero.                 �
'�      DatCase$ ===> Variable por la que se ordenar� el Indice.             �
'�      Index%(1)===> Array que contiene el Indice. Leido por QSReadIndex    �
'�                 *************************************                     �
'�                 ****** NOTAS IMPORTANTES ************                     �
'�                 *************************************                     �
'�  Debido a que este procedimiento es recursivo (se llama a el mismo) de-   �
'�  beremos poner al principio del programa $STACK 32766                     �
'���������������������������������������������������������������������������ͼ
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


'���������������������������������������������������������������������������ͻ
'�  Subrrutina.....: QSFile              Ordena un fichero por varios campos �
'���������������������������������������������������������������������������͹
'�  El fichero a ordenar debe estar abierto con QSOpen                       �
'�  En la variable DatCase$ pasaremos separados por comas "," la posicion    �
'�  inicial del campo y su longitud dentro del registro.  Ejemplo....:       �
'�                1         11        21                                     �
'�  Registro es "Esteve    Valent�   Morales  "   y queremos indexarlo por   �
'�  los apellidos y el nombre.                                               �
'�                                  DatCase$ = "11,20,1,10,"                 �
'�                                                                           �
'�  Es necesario poner una coma "," en el final de DatCase$                  �
'�                                                                           �
'�      Filenum% ===> Numero con que fu� abierto el fichero.                 �
'�      DatCase$ ===> Variable por la que se ordenar� el Indice.             �
'�                 *************************************                     �
'�                 ****** NOTAS IMPORTANTES ************                     �
'�                 *************************************                     �
'�  Debido a que este procedimiento es recursivo (se llama a el mismo) de-   �
'�  beremos poner al principio del programa $STACK 32766                     �
'���������������������������������������������������������������������������ͼ
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


'���������������������������������������������������������������������������ͻ
'�  Subrrutina.....: QSAddDat          A�ade registros a un fichero Indexado �
'���������������������������������������������������������������������������͹
'�  El fichero a ordenar debe estar abierto.                                 �
'�  En la variable DatCase$ pasaremos separados por comas "," la posicion    �
'�  inicial del campo y su longitud dentro del registro.  Ejemplo....:       �
'�                1         11        21                                     �
'�  Registro es "Esteve    Valent�   Morales  "   y el indice esta creardo   �
'�  con los apellidos y el nombre.                                           �
'�                                  DatCase$ = "11,20,1,10,"                 �
'�                                                                           �
'�  Es necesario poner una coma "," en el final de DatCase$                  �
'�                                                                           �
'�      Filenum% ===> Numero con que fu� abierto el fichero.                 �
'�      DatCase$ ===> Variable por la que se ordenar� el Indice.             �
'�      Index%(1)===> Array que contiene el Indice.                          �
'���������������������������������������������������������������������������ͼ
SUB QSAddDat(FileNum%,DatCase$,Index%(1))
  LOCAL X%
  SHARED QSLocInIndex%

  CALL QSFindDat(FileNum%,DatCase$,Index%(),FNDatForIndex$(FileNum%,DatCase$,LOC(FileNum%)),e%)

  FOR X%=FNQSEof(FileNum%)-1 TO QSLocInIndex% STEP-1
    Index%(X%) = Index%(X%-1)
  NEXT X%
    Index%(QSLocInIndex%) = FNQSEof(FileNum%)-1
END SUB


'���������������������������������������������������������������������������ͻ
'�  Subrrutina.....: QSModDat      Modifica registros de un fichero Indexado �
'���������������������������������������������������������������������������͹
'�  El fichero a ordenar debe estar abierto.                                 �
'�  En la variable DatCase$ pasaremos separados por comas "," la posicion    �
'�  inicial del campo y su longitud dentro del registro.  Ejemplo....:       �
'�                1         11        21                                     �
'�  Registro es "Esteve    Valent�   Morales  "   y el indice esta creardo   �
'�  con los apellidos y el nombre.                                           �
'�                                  DatCase$ = "11,20,1,10,"                 �
'�                                                                           �
'�  Es necesario poner una coma "," en el final de DatCase$                  �
'�                                                                           �
'�      Filenum% ===> Numero con que fu� abierto el fichero.                 �
'�      DatCase$ ===> Variable por la que se ordenar� el Indice.             �
'�      Index%(1)===> Array que contiene el Indice.                          �
'���������������������������������������������������������������������������ͼ
SUB QSModDat(FileNum%,DatCase$,Index%(1))
 LOCAL X%,PFile%,P%
 SHARED QSLocInIndex%
  PFile% = LOC(FileNum%)
  FOR P% = 1 TO FNQSEof(FileNum%)-1
    IF LOC(Filenum%) = Index%(P%) THEN EXIT FOR		'Busca la posici�n en Indice
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



'���������������������������������������������������������������������������ͻ
'�  Funci�n......: FNQSFindDat         Busca un Dato y retorna su posici�n   �
'���������������������������������������������������������������������������͹
'�  El fichero a ordenar debe estar abierto.                                 �
'�  En la variable DatCase$ pasaremos separados por comas "," la posicion    �
'�  inicial del campo y su longitud dentro del registro.  Ejemplo....:       �
'�                1         11        21                                     �
'�  Registro es "Esteve    Valent�   Morales  "   y queremos indexarlo por   �
'�  los apellidos y el nombre.                                               �
'�                                  DatCase$ = "11,20,1,10,"                 �
'�                                                                           �
'�  Es necesario poner una coma "," en el final de DatCase$                  �
'�                                                                           �
'�      Filenum% ===> Numero con que fu� abierto el fichero.                 �
'�      DatCase$ ===> Variable por la que se ordenar� el Indice.             �
'�      Index%(1)===> Array que contiene el Indice.                          �
'�      DatToFind$==> Dato a Buscar, se comparara con la cadena que se consi-�
'�                    ga con DatCase$.                                       �
'�      e%       ===> Devuelve 1 si no se localiza el dato (error)           �
'���������������������������������������������������������������������������ͼ
SUB QSFindDat(FileNum%,DatCase$,Index%(1),DatToFind$,e%)
  LOCAL C%,I%,F%,QSDat$
  SHARED QSLocInIndex%
  e% = 1
  QSLocInIndex% = 1						'Si solo hay un registro apuntar� a este
IF FNQSEof(FileNum%) > 1 THEN					'Si existen datos en el fichero
  I% = 1                					'Primer Dato Indexado
  F% = FNQSEof(FileNum%)    					'Ultimo dato indexado
  QSDat$ = DatToFind$						'Dato a comparar

  IF QSDat$<=FNDatForIndex$(FileNum%,DatCase$,Index%(1))THEN	'Si el dato es m�s peque�o o igual
     IF QSDat$=FNDatForIndex$(FileNum%,DatCase$,Index%(1))THEN e% = 0
     C% = 1							'que el primer dato indexado
  END IF
 IF C% = 0 THEN
  IF QSDat$>FNDatForIndex$(FileNum%,DatCase$,Index%(F%-1))THEN	'Si el dato es m�s grande
     C% = FNQSEof(FileNum%)- 1 					'que el �ltimo dato indexado
  END IF
 END IF
 IF C% = 0 THEN							'Si no es m�s peque�o que
    DO								'el primero ni m�s grande		
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



'���������������������������������������������������������������������������ͻ
'�  Subrrutina.....: QSSkip            Busca el siguiente o anterior registro�
'���������������������������������������������������������������������������͹
'�  El fichero a ordenar debe estar abierto.                                 �
'�  En la variable DatCase$ pasaremos separados por comas "," la posicion    �
'�  inicial del campo y su longitud dentro del registro.  Ejemplo....:       �
'�                1         11        21                                     �
'�  Registro es "Esteve    Valent�   Morales  "   y queremos indexarlo por   �
'�  los apellidos y el nombre.                                               �
'�                                  DatCase$ = "11,20,1,10,"                 �
'�                                                                           �
'�  Es necesario poner una coma "," en el final de DatCase$                  �
'�                                                                           �
'�      Filenum% ===> Numero con que fu� abierto el fichero.                 �
'�      Index%(1)===> Array que contiene el Indice.                          �
'�      SKipNum% ===> Cantidad de registros a avanzar o retroceder           �
'�      e%       ===> Error. Retorna el valor de EOF(Filenum%)               �
'���������������������������������������������������������������������������ͼ
SUB QSSkip(FileNum%,Index%(1),SkipNum%,e%)
SHARED QSLocInIndex%
IF QSLocInIndex% = 0 THEN QSLocInIndex% = 1
e% = 0
 IF Index%(QSLocInIndex%) <> LOC(FileNum%) OR LOC(FileNum%) = 0 THEN
    FOR QSLocInIndex% = 1 TO FNQSEof(FileNum%)-1
       IF LOC(FileNum%) = Index%(QSLocInIndex%) THEN EXIT FOR	'Busca la posici�n en Indice
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
'���������������������������������������������������������������������������ͻ
'�  Subrrutina y Funciones de uso interno de paquete TBQSFILE                �
'���������������������������������������������������������������������������ͼ
'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

'���������������������������������������������������������������������������ͻ
'�  Funci�n .....:  FNDatForIndex$                                           �
'���������������������������������������������������������������������������͹
'�  Lee el registro NumReg% y extrae de el los datos, seleccionados en       �
'�  DatCase$, que ser�n retornados por la Funci�n para hacer las comparacio- �
'�  nes en las otras subrrutinas del paquete TBQSFILE.                       �
'���������������������������������������������������������������������������ͼ
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


'���������������������������������������������������������������������������ͻ
'�  Subrrutina...:  QSSwapRecord     Intercambia dos registros de un fichero �
'���������������������������������������������������������������������������͹
'�  Lee el registro situado en LOC1% y lo guarda en TEMP$, Lee el registro   �
'�  situado en LOC2% y lo graba en la posici�n LOC1%, luego graba TEMP$ en   �
'�  la posici�n LOC2%.                                                       �
'�  Utilizado por QSFile.                                                    �
'���������������������������������������������������������������������������ͼ
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



'���������������������������������������������������������������������������ͻ
'�  Funci�n .....:  FNQSEof(FileNum%)                                        �
'���������������������������������������������������������������������������͹
'� Retorna el fin del fichero + 1.                                           �
'� Solo es utilizable si el fichero ha sido abierto con QSOpen.              �
'���������������������������������������������������������������������������ͼ
DEF FNQSEof(FileNum%)
  SHARED QSFileDat%()
  FNQSEof = ( LOF( FileNum% ) \ QSFileDat%( FileNum% )) + 1
END DEF



'���������������������������������������������������������������������������ͻ
'�  Subrrutina...:  QSDefFields                                              �
'���������������������������������������������������������������������������͹
'� En esta Subrrutina definiremos los campos de cada registro de cada fichero�
'� Deberemos declarar los campos de cada fichero como SHARED.                �
'� Por ejemplo....:                                                          �
'�                                                                           �
'�  SUB QSDefFields(FileNum%)                                                �
'�   SHARED Nomb.1$,Ape1.1$,Ape2.1$     'Campos del fichero #1               �
'�   SHARED NCli.2$,Codi.2$,Dato.2$     'Campos del fichero #2               �
'�   SHARED .......                     'Campos del fichero #3               �
'�   SELECT CASE FileNum%                                                    �
'�     CASE 1                                                                �
'�       FIELD #1,10 AS Nomb.1$,10 AS Ape1.1$,10 AS Ape2.1$...               �
'�     CASE 2                                                                �
'�       FIELD #2,5 AS NCli.2$,7 AS Codi.2$,10 AS Dato.2$....                �
'�     CASE 3                                                                �
'�       .........                                                           �
'�   END SELECT                                                              �
'�  END SUB                                                                  �
'�                                                                           �
'� Esta Subrrutina se deber� modificar para cada programa en que vayamos a   �
'� utilizar el paquete TBQSFILE.                                             �
'� Cuando abramos un fichero desde el programa podemos definir los campos    �
'� de la siguiente forma...:                                                 �
'�        OPEN #1,"R","FICHERO.DAT",512          'Abrimos el fichero         �
'�        CALL QSDefField$(1)                    'Definimos los campos del F.�
'� Una vez abierto el fichero y definidos los campos de este no har� falta   �
'� volver a definirlos cada vez que llamemos a QSFindDat o QSAddDat o        �
'� QSModDat, ya que estas subrrutinas llaman a QSDefFields automaticamente.  �
'���������������������������������������������������������������������������ͼ
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