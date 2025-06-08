sub main.ini.config
   shared inidri$, inidir$
   shared munidad$
   shared mcurdir$
   shared file$, file.ext$

    inidri$ = driveletter$(0)
    inidir$ = curdir$

     call main.ini.config.get

    if mcurdir$ <> ficheros.curdir$ then
        call splitpathfn( mcurdir$, d$, mdir$, f$, e$)
       if d$ <> driveletter$(0) and d$ <> "" then chdrive(d$)
       if mdir$ <> "" and mdir$ <> "\" then chdir( rtrim$(mcurdir$,"\") )
    end if

    if command$ <> "" then file$ = command$

    call splitpathfn( file$, d$, md$, f$, e$ )
    if e$ = "" then e$ = ".SCR"
    file.ext$ = e$
    file$     = d$+md$+f$

    if f$ = "NONAME  " then
       call bload.scr( munidad$+"\e-screen\e-scrpre.scr" )
    else
       call ficheros.bload.ok( file$, file.ext$ )
    end if

end sub





sub main.ini.config.get				shared
   local file%
   local m$, x%, p%

    file% = main.ini.config.open%
    get #file%, 1
    mpos%    = cvi(cmpos$)
    mcolor%  = cvi(cmcolor$)
    mpincel% = cvi(cmpincel$)
    mchr%    = cvi(cmchr$)
    fgc%     = cvi(cfgc$)
    bgc%     = cvi(cbgc$)
    bfgc%    = cvi(cbfgc$)
    bbgc%    = cvi(cbbgc$)
    cfgc%    = cvi(ccfgc$)
    cbgc%    = cvi(ccbgc$)
    cbfgc%   = cvi(cbfgc$)
    cbbgc%   = cvi(cbbgc$)
    bloques.abc.num% = cvi(cbloques.abc.num$)
    mcurdir$ = trim$(ccurdir$)
    file$    = trim$(cfile$)
    ansi.ini%= cvi(cansi.ini$)
    ansi.len%= cvi(cansi.len$)
    ansi.lin%= cvi(cansi.lin$)
    m$       = cmacro$
    close #file%

    for x% = 1 to 499 step 50
	macro$(p%) = mid$(m$,x%,50)
	incr p%
    next

end sub





sub main.ini.config.put				shared
   local file%
   local text$
   local m$, x%

    $if %english
    text$ = "Configuration file"
    $else
    text$ = "Fichero de configuraci¢n"
    $endif

    for x% = 0 to 9
	m$ = m$ + macro$(x%)
    next

    file% = main.ini.config.open%
    lset ctitulo$  = mprograma$+chr$(13)+text$+chr$(26)
    lset cmpos$	   = mki$(mpos%)
    lset cmcolor$  = mki$(mcolor%)
    lset cmpincel$ = mki$(mpincel%)
    lset cmchr$    = mki$(mchr%)
    lset cfgc$	   = mki$(fgc%)
    lset cbgc$     = mki$(bgc%)
    lset cbfgc$	   = mki$(bfgc%)
    lset cbbgc$    = mki$(bbgc%)
    lset ccfgc$    = mki$(cfgc%)
    lset ccbgc$    = mki$(cbgc%)
    lset ccbfgc$   = mki$(cbfgc%)
    lset ccbbgc$   = mki$(cbbgc%)
    lset cbloques.abc.num$ = mki$(bloques.abc.num%)
    lset ccurdir$  = curdir$("")
    lset cfile$    = file$ + file.ext$
    lset cansi.ini$= mki$(ansi.ini%)
    lset cansi.len$= mki$(ansi.len%)
    lset cansi.lin$= mki$(ansi.lin%)
    lset cnull$    = ""
    lset cmacro$   = m$
    put #file%, 1
    close #file%

end sub





function main.ini.config.open% 		shared
    local dfile$
    local dfile%

     dfile% = frefile
     dfile$ = munidad$+"\e-screen\e-scrcon.dat"

     open dfile$ for random as dfile% len = 750

     field #file%,_
     50 as ctitulo$,_
     2 as cmpos$,_
     2 as cmcolor$,_
     2 as cmpincel$,_
     2 as cmchr$,_
     2 as cfgc$,_
     2 as cbgc$,_
     2 as cbfgc$,_
     2 as cbbgc$,_
     2 as ccfgc$,_
     2 as ccbgc$,_
     2 as ccbfgc$,_
     2 as ccbbgc$,_
     2 as cbloques.abc.num$,_
     70 as ccurdir$,_
     70 as cfile$,_
     2 as cansi.ini$,_
     2 as cansi.len$,_
     2 as cansi.lin$,_
     24 as cnull$,_
     500 as cmacro$

     main.ini.config.open% = dfile%

end function





sub main.ini.colors
   shared mprograma$
   shared munidad$
   local file$
    file$ = munidad$+"\e-screen\e-scrcol.dat"
    f$ = dir$( file$, &h0)

    if f$ <> "" then
        call config.colors.get
    else
        call config.colors.default.color
        call config.colors.put
    end if

end sub


'sub ebeep 				shared
'    if ebeepon% then  play "L64 N70"
'end sub



sub key.esc(x%)
    for x% = 1 to x%
        call kbd.putkey(&h1b, &h01)
    next
end sub





