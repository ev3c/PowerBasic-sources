

def fnfgc%(attr%)
   local attr$
   local c%, p%, i%
    attr$ = right$("00000000" + bin$(attr%), 8)
    c% = bin( mid$(attr$, 6, 3 ))
    p% = bin( mid$(attr$, 1, 1 ))
    i% = bin( mid$(attr$, 5, 1 ))
    if p% = 1 then c% = c% + 16
    if i% = 1 then c% = c% + 8
    fnfgc% = c%

end def



def fnbgc%(attr%)
   local attr$
   local c%
    attr$ = right$("00000000" + bin$(attr%), 8)
    c% = bin( mid$(attr$, 2, 3 ))
    fnbgc% = c%

end def

function bin(mbin$)
    local v0, v1
    local p%
    local l%
     l% = len(mbin$)

     if l% < 17 then

        for p% = 1 to l%
           v0 = 0

           if mid$(mbin$, p%, 1) = "1" then
              v0 = 1 * 2 ^ (l% - p%)
           end if


           v1 = v1 + v0
        next p%

        bin = v1
     else
        bin = -1
     end if

end function
