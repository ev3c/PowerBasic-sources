
cls
c$ = ";"


open "a_vnous.xdb" for random as #1 len = 570

open "a_vnous.wdb"  for output as #2

field #1 ,  21 as nom$,_
	    20 as cog1$,_
	    18 as cog2$,_
	    14 as dni$,_
	    11 as datan$,_
	    30 as adre$,_
	    21 as pobl$,_
	     7 as codp$,_
	    16 as prov$,_
	    12 as tel1$,_
	    12 as tel2$,_
	    17 as prof$,_
	    17 as marc$,_
	    32 as mode$,_
	    11 as colo$,_
	    38 as xass$,_
	    15 as matr$,_
	    12 as datav$,_
	    13 as datam$,_
	    12 as fina$,_
	    14 as formp$,_
	    14 as entr$,_
	    12 as plas$,_
	    14 as cred$,_
	    14 as usat$,_
	    18 as gest$,_
	    65 as nul1$,_
	    62 as nul2$



while not eof(1)

   incr x%
   get #1, x%

   lset marc$= ucase$(marc$)
   lset gest$= ucase$(gest$)

   if instr(ucase$(pobl$), "FELIU") then lset pobl$= "Sant Feliu de Gu¡xols"


   linea$ =  trim$(nom$)+c$+_
	     trim$(cog1$)+c$+_
	     trim$(cog2$)+c$+_
	     trim$(dni$)+c$+_
	     trim$(datan$)+c$+_
	     trim$(adre$)+c$+_
	     trim$(pobl$)+c$+_
	     trim$(codp$)+c$+_
	     trim$(prov$)+c$+_
	     trim$(tel1$)+c$+_
	     trim$(tel2$)+c$+_
	     trim$(prof$)+c$+_
	     trim$(marc$)+c$+_
	     trim$(mode$)+c$+_
	     trim$(colo$)+c$+_
	     trim$(xass$)+c$+_
	     trim$(matr$)+c$+_
	     trim$(datav$)+c$+_
	     trim$(datam$)+c$+_
	     trim$(fina$)+c$+_
	     trim$(formp$)+c$+_
	     trim$(entr$)+c$+_
	     trim$(plas$)+c$+_
	     trim$(cred$)+c$+_
	     trim$(usat$)+c$+_
	     trim$(gest$)+c$+_
	     trim$(nul1$)+" "+_
	     trim$(nul2$)

   replace chr$(13,10) with "" in linea$

   replace chr$(29) with "" in linea$

   print #2, linea$

wend

close




function trim$(text$)
   trim$ = ltrim$(rtrim$(text$))
end function
