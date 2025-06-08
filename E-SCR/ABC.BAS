defint a-z
cls

for x% = 1 to 99

    num$ = right$("000"+ltrim$(rtrim$(str$(x%))),2)

    call findfile("c:\e-screen\blq\a-"+num$+".blq",0,f$,fa%,d%,t%,s&,e%)

    file$ = f$

    if f$ <> "" then

       file$ = "c:\e-screen\abc\a-"+num$+".abc"

       print file$

       open file$ for random as #1 len = 512
       field #1, 512 as blq$

       for c% = 1 to 36

	    f$ = mid$("0123456789abcdefghijklmnopqrstuvwxyz",c%,1)

	    f$ = "c:\e-screen\blq\"+f$+"-"+num$+".blq"

	    blq0$ = bloques.disk.get$( f$ )

	    lset blq$ = blq0$

	    put #1, c%

       next

       close #1

	  kill "c:\e-screen\blq\?-"+num$+".blq"

    end if

next


function bloques.disk.get$( file$ )
    local p%
    local lenfile%
    local file%
     file% = freefile
     blq$ = ""
     open file$ for binary as file%
     lenfile% = lof(file%)
     get$ #file%, lenfile%, blq$
     close #file%
     bloques.disk.get$ = blq$
end function


