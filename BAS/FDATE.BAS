$include "pbgetdir.inc"

dim mdir$(1:512)

mask$ = command$
date$ = "03-23-92"
time$ = "11:01"


call splitpathfn(mask$,d$,mdir$,f$,e$)
path$ = d$+mdir$

call findfile(mask$,0,f$,fa%,s&,d%,t%,e%)

do while e% = 0
  incr p%
  ? path$+f$
  open path$+f$ for random as #1 len=1
  field #1, 1 as temp$
  get #1,1
  lset temp$ = temp$
  put #1, 1
  close #1
  call findfile("",0,f$,fa%,s&,d%,t%,e%)
loop

