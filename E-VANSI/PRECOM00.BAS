defint a-z
open "con:" for output as #1
on error goto meerror
file$ = command$
 dim file$(1:512)
if file$ = "" then
    call meexit
else
    call splitpathfn(file$,d$,s$,f$,e$)
   if e$ = "" then e$ = ".ANS"
   path$ = d$+s$
    call findfile(path$+f$+e$,&h0,f$,fa%,s&,d%,t%,e%)
   if e% <> 0 then call meexit
   do
      if e% = 0 then
         p% = p% + 1
         file$(p%) = path$+f$
      end if
       call findfile("",&h0,f$,fa%,s&,d%,t%,e%)
   loop while e%=0
   array sort file$() for p%
   for x% = 1 to p%
       call bload.ansi(file$(x%))
   next
   color 7,0
end if
end:
meerror:
 print #1,
 print #1, " нннн  ERROR  !!!!  "; err
 print #1,
 call meexit
$INCLUDE  "PRECOM00.INC"
