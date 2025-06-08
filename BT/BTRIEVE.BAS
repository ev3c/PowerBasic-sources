
       $INCLUDE "c:\lenguaje\pb\btrieve\TURBBTRV.BAS"



'********** obrir el fitxer
	OPEN "NUL" AS #1 LEN=28
        field #1, 28 as f1.buf$
	FIELD #1,8 AS f1.CLA1$,10 AS f1.NOM$,10 AS f1.TEL$
        f1.name$ = "clientes.dat" + chr$(0)
        f1.posi$ = space$(128)
        %f1.alfa = 0

       DLEN% = 0
       OP% = 0

       CALL BTRV (op%, STATUS%, f1.posi$, f1.buf$, dlen%, f1.name$, %f1.alfa)

       IF STATUS1% <> 0 THEN
	  PRINT "operation is " OP1% "status is" STATUS1%
       END IF



       KEYNUM% = 0
       DLEN% = 40
       OP1% = 12
       CALL BTRV (OP1%, STATUS1%, POSITION$, FILE1.BUF$,_
		  DLEN%, F1.KEY1$, KEYNUM%)


       IF STATUS1% <> 0 THEN
	  PRINT "operation is " OP1% "status is" STATUS1%
       END IF




'LECTURA DE  UN REGISTRO
	OP%=5
	CLAVE$="LUIS"+SPACE$(4)
	NUM.CLAVE%=0
	LONG%=28
	PRINT NOMBRE$
	PRINT TELEFONO$
	PRINT STATUS

'CIERRE DE FICHERO
	OP%=1
	NUM.CLAVE%=0
	LONG%=28
	FCB%=FCB1%
	CLOSE 1

END




