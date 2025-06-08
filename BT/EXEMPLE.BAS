'
'       BTRIEVE.BAS: Esquema d'us de Btrieve des de QBasic
'
'       CONEC Software S.L.     Joan Sales May_90
'

'-------------------------------------------
' Codis d'error permesos per a la rutina de Btrieve
' (veure mes avall)
'-------------------------------------------

	e1% = 0
	e2% = 4
	e3% = 9

'-------------------------------------------
' Obertura d'un fitxer
'-------------------------------------------


'       Open i fields del arxiu 7.....EMPRESAS


        open "NUL" as #7 len=93   :'Obertura per al Basic


	field#7,3 as CODEMP.7$,30 as NOMBRE.7$,1 as CONSOL.7$,30 as GRUPO.7$,29 as LIBRE.7$

	op%        = 0
	num.clave% = 0
	clave$     = "EMPRESAS"+chr$(0)
	long%      = 93
	fcb7%      = varptr(#7)
	fcb%       = fcb7%
	po$        = "Abre.EMPRESAS"
	gosub btrieve

' A clave$ hi va el nom del fitxer (P.ex. DATOS.DAT) seguit del chr 0




'-------------------------------------------
'    Exemple de utilitzaci¢:
'-------------------------------------------

	po$        = "entra.1"	'...... descripci¢ per localitzar si hi ha error
	op%        = 5		'...... operaci¢ Btrieve a realitzar
	clave$     = "012"	'...... Contingut de la clau
	num.clave% = 0		'...... N§ de clau a utilitzar
	long%      = 93		'...... longitud del fitxer
	fcb%       = fcb7%	'...... FCB tal com dona al fer open

	gosub btrieve      	'...... crida a la subrutina d'interface


'-------------------------------------------
' Tractement dels errors tornats a status%
'-------------------------------------------

        if status%<>0 then
                if status%=4 then
                        print "El registro ";clave$;" no existe"
                else
                        print "Error n§ ";status%
                end if
        end if

'-------------------------------------------
' Tancament d'un fitxer
'-------------------------------------------

	po$        = "Cierra"
	op%        = 1
	num.clave$ = 0
	long%      = 93
	fcb%       = fcb7%
        gosub btrieve

        close#7




'-------------------------------------------
' Fi del programa principal
'-------------------------------------------

        end




'-------------------------------------------
' Rutina de interface amb Btrieve
'
' Fa la operaci¢ indicada a op% y torna si
' status% es e1% e2% o e3%. Sino mostra l'error
'-------------------------------------------

btrieve:


	status%=0
	call btrv (op%,status%,fcb%,long%,clave$,num.clave%)

	if status% <>e1% and status%<>e2% and status%<>e3% then

		color 7,0
		locate	18 , 1 :print "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
		locate	19 , 1 :print "³ Error BTRIEVE num.:                                      en                  ³"
		locate	20 , 1 :print "³ FCB:          OP..:     CLAVE:                                               ³"
                locate  21 , 1 :print "³             Pulsar T o C y revisar la £ltima operaci¢n realizada             ³"
		locate	22 , 1 :print "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		color 15,0

		call sonido(kkk+1)

		locate 19,23:print status%

		locate 19,63:print po$
		locate 20,8 :print fcb%
		locate 20,23:print op%
		locate 20,33:print left$(clave$,40)
		color 15,0


		ke$=input$(1)

		if ke$="T" or ke$="t" then end


		po$ = "Cierra-todo"
		op% = 28
                gosub btrieve

                locate 1,1
                end
	end if

        return

