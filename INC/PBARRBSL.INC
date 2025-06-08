'NOGOODFONT
'
' ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออปบ
'บ Library......: ARRAYBSL.INC                                           บบ
'บ Definition...: To fast load and save arrays                           บบ
'บ Author.......: Esteve Valentก Morales                                 บบ
'บ Date.........: 12-21-1991                                             บบ
'บ Observations.: For BLOAD and BSAVE all mode of arrays in the disk     บบ
'บ                In all SUB xxxarrayBLOAD assume the array passed has   บบ
'บ                dimmensioned right, and assume the file exist.         บบ
'บ                                                                       บน
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนบ
'บ SUBs.......:                                                          บบ
'บ            	INTarray1BLOAD ( file$, INTarray%(), )                   บบ
'บ            	INTarray1BSAVE ( file$, INTarray%(), )                   บบ
'บ            	LNGarray1BLOAD ( file$, LNGarray&(), )                   บบ
'บ            	LNGarray1BSAVE ( file$, LNGarray&(), )                   บบ
'บ            	QUDarray1BLOAD ( file$, QUDarray&&(),)                   บบ
'บ            	QUDarray1BSAVE ( file$, QUDarray&&(),)                   บบ
'บ            	SNGarray1BLOAD ( file$, SNGarray!(), )                   บบ
'บ            	SNGarray1BSAVE ( file$, SNGarray!(), )                   บบ
'บ            	DBLarray1BLOAD ( file$, DBLarray#(), )                   บบ
'บ            	DBLarray1BSAVE ( file$, DBLarray#(), )                   บบ
'บ            	EXTarray1BLOAD ( file$, EXTarray##(),)                   บบ
'บ            	EXTarray1BSAVE ( file$, EXTarray##(),)                   บบ
'บ            	FIXarray1BLOAD ( file$, FIXarray@(), )                   บบ
'บ            	FIXarray1BSAVE ( file$, FIXarray@(), )                   บบ
'บ            	BCDarray1BLOAD ( file$, BCDarray@@(),)                   บบ
'บ            	BCDarray1BSAVE ( file$, BCDarray@@(),)                   บบ
'บ            	INTarray2BLOAD ( file$, INTarray%(), )                   บบ
'บ            	INTarray2BSAVE ( file$, INTarray%(), )                   บบ
'บ            	LNGarray2BLOAD ( file$, LNGarray&(), )                   บบ
'บ            	LNGarray2BSAVE ( file$, LNGarray&(), )                   บบ
'บ            	QUDarray2BLOAD ( file$, QUDarray&&(),)                   บบ
'บ            	QUDarray2BSAVE ( file$, QUDarray&&(),)                   บบ
'บ            	SNGarray2BLOAD ( file$, SNGarray!(), )                   บบ
'บ            	SNGarray2BSAVE ( file$, SNGarray!(), )                   บบ
'บ            	DBLarray2BLOAD ( file$, DBLarray#(), )                   บบ
'บ            	DBLarray2BSAVE ( file$, DBLarray#(), )                   บบ
'บ            	EXTarray2BLOAD ( file$, EXTarray##(),)                   บบ
'บ            	EXTarray2BSAVE ( file$, EXTarray##(),)                   บบ
'บ            	FIXarray2BLOAD ( file$, FIXarray@(), )                   บบ
'บ            	FIXarray2BSAVE ( file$, FIXarray@(), )                   บบ
'บ            	BCDarray2BLOAD ( file$, BCDarray@@(),)                   บบ
'บ            	BCDarray2BSAVE ( file$, BCDarray@@(),)                   บบ
'บ            	INTarray3BLOAD ( file$, INTarray%(), )                   บบ
'บ            	INTarray3BSAVE ( file$, INTarray%(), )                   บบ
'บ            	LNGarray3BLOAD ( file$, LNGarray&(), )                   บบ
'บ            	LNGarray3BSAVE ( file$, LNGarray&(), )                   บบ
'บ            	QUDarray3BLOAD ( file$, QUDarray&&(),)                   บบ
'บ            	QUDarray3BSAVE ( file$, QUDarray&&(),)                   บบ
'บ            	SNGarray3BLOAD ( file$, SNGarray!(), )                   บบ
'บ            	SNGarray3BSAVE ( file$, SNGarray!(), )                   บบ
'บ            	DBLarray3BLOAD ( file$, DBLarray#(), )                   บบ
'บ            	DBLarray3BSAVE ( file$, DBLarray#(), )                   บบ
'บ            	EXTarray3BLOAD ( file$, EXTarray##(),)                   บบ
'บ            	EXTarray3BSAVE ( file$, EXTarray##(),)                   บบ
'บ            	FIXarray3BLOAD ( file$, FIXarray@(), )                   บบ
'บ            	FIXarray3BSAVE ( file$, FIXarray@(), )                   บบ
'บ            	BCDarray3BLOAD ( file$, BCDarray@@(),)                   บบ
'บ            	BCDarray3BSAVE ( file$, BCDarray@@(),)                   บบ
'บ                                                                       บน
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนบ
'บ TO PASS:                                                              บบ
'บ                                                                       บบ
'บ   file$     ===> [unit:] [\path\] file [.ext]                         บบ
'บ                   default [unit:]  are DOS unit                       บบ
'บ                   default [\path\] are DOS path                       บบ
'บ                   default [.ext]   are array type .INT .LNG .QUD .SNG บบ
'บ                                                   .BDL .EXT .FIX .BCD บบ
'บ   xxxarrayx()==> array to be bsaved or bloaded                        บบ
'บ                   the SUB assume array has dimmensioned right         บบ
'บ                                                                       บผ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ

'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :intarray1bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub intarray1bload ( file$, intarray%(1) ) public
   local s		'segment
   local o		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:2)

    if instr( file$, "." ) = 0 then file$ = file$ + ".INT"

    for x% = 1 to 1
       l%(x%) = lbound( intarray%(x%) )
    next x%

    s = varseg( intarray%( l%(1) ) )
    o = varptr( intarray%( l%(1) ) )
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:Iinstarray1bload                                 12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :intarray1bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub intarray1bsave ( file$, intarray%(1) ) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:1)

     dim u%(1:1)

    if instr( file$, "." ) = 0  then file$ = file$ + ".INT"
    tbl% = 1

    for x%= 1 to 1
       l%(x%) = lbound( intarray%(x%) )
       u%(x%) = ubound( intarray%(x%) )
       tbl%   = tbl% * ((u%(x%) - (l%(x%)-1) ))
    next x%

    tbl% = tbl% * 2
    s = varseg( intarray%( l%(1) ))
    o = varptr( intarray%( l%(1) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:intarray1bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :intarray2bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub intarray2bload ( file$, intarray%(2) ) public
   local s		'segment
   local o		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:2)

    if instr( file$, "." ) = 0 then file$ = file$ + ".INT"
    tbl% = 1

    for x% = 1 to 2
       l%(x%) = lbound( intarray%(x%) )
    next x%

    s = varseg( intarray%( l%(1), l%(2) ))
    o = varptr( intarray%( l%(1), l%(2) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:intarray2bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :intarray2bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub intarray2bsave ( file$, intarray%(2) ) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:2)

     dim u%(1:2)

    if instr( file$, "." ) = 0 then file$ = file$ + ".INT"
    tbl% = 1

    for x% = 1 to 2
       l%(x%) = lbound( intarray%(x%) )
       u%(x%) = ubound( intarray%(x%) )
       tbl% = tbl% * ((u%(x%) - (l%(x%)-1) ) )
    next x%

    tbl% = tbl% * 2
    s = varseg( intarray%( l%(1), l%(2) ))
    o = varptr( intarray%( l%(1), l%(2) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:intarray2bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :intarray3bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub intarray3bload ( file$, intarray%(3) ) public
   local s		'segment
   local o		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:5)

    if instr( file$, "." ) = 0 then file$ = file$ + ".INT"
    tbl% = 1

    for x% = 1 to 3
       l%(x%) = lbound( intarray%(x%) )
    next x%

    s = varseg( intarray%( l%(1), l%(2), l%(3) ))
    o = varptr( intarray%( l%(1), l%(2), l%(3) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:intarray3bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :intarray3bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub intarray3bsave ( file$, intarray%(3)  ) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:3)

     dim u%(1:3)

    if instr( file$, "." ) = 0 then file$ = file$ + ".INT"
    tbl% = 1

    for x% = 1 to 3
       l%(x%) = lbound( intarray%(x%) )
       u%(x%) = ubound( intarray%(x%) )
       tbl% = tbl% * ((u%(x%) - (l%(x%)-1) ))
    next x%

    tbl% = tbl% * 2
    s = varseg( intarray%( l%(1), l%(2), l%(3) ))
    o = varptr( intarray%( l%(1), l%(2), l%(3) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:intarray3bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :lngarray1bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub lngarray1bload ( file$, lngarray&(1) ) public
   local s 		'segment
   local o 		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:1)

    if instr( file$, "." ) = 0 then file$ = file$ + ".LNG"
    tbl% = 1

    for x% = 1 to 1
       l%(x%) = lbound( lngarray&(x%) )
    next x%

    s = varseg( lngarray&( l%(1) ))
    o = varptr( lngarray&( l%(1) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:lngarray1bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'***************************************************************

'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :lngarray1bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub lngarray1bsave ( file$, lngarray&(1) ) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:1)

     dim u%(1:1)

    if instr( file$, "." ) = 0 then file$ = file$ + ".LNG"
    tbl% = 1

    for x% = 1 to 1
       l%(x%) = lbound( lngarray&(x%) )
       u%(x%) = ubound( lngarray&(x%) )
       tbl% = tbl% * ((u%(x%) - (l%(x%)-1) ))
    next x%

    tbl% = tbl% * 4
    s = varseg( lngarray&( l%(1) ))
    o = varptr( lngarray&( l%(1) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:lngarray1bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :lngarray2bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub lngarray2bload ( file$, lngarray&(2) ) public
   local s 		'segment
   local o 		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:2)

    if instr( file$, "." ) = 0 then file$ = file$ + ".LNG"
    tbl% = 1

    for x% = 1 to 2
       l%(x%) = lbound( lngarray&(x%) )
    next x%

    s = varseg( lngarray&( l%(1), l%(2) ))
    o = varptr( lngarray&( l%(1), l%(2) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:lngarray2bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :lngarray2bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub lngarray2bsave ( file$, lngarray&(2) ) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:2)

     dim u%(1:2)

    if instr( file$, "." ) = 0 then file$ = file$ + ".LNG"
    tbl% = 1

    for x% = 1 to 2
       l%(x%) = lbound( lngarray&(x%) )
       u%(x%) = ubound( lngarray&(x%) )
       tbl% = tbl% * ( ( u%(x%) - (l%(x%)-1) ))
    next x%

    s = varseg( lngarray&( l%(1), l%(2) ))
    o = varptr( lngarray&( l%(1), l%(2) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:lngarray2bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :lngarray3bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub lngarray3bload ( file$, lngarray&(3) ) public
   local s 		'segment
   local o 		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:3)

    if instr( file$, "." ) = 0 then file$ = file$ + ".LNG"
    tbl% = 1

    for x% = 1 to 3
       l%(x%) = lbound( lngarray&(x%) )
    next x%

    s = varseg( lngarray&( l%(1), l%(2), l%(3) ))
    o = varptr( lngarray&( l%(1), l%(2), l%(3) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:lngarray3bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :lngarray3bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub lngarray3bsave ( file$, lngarray&(3) ) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:3)

     dim u%(1:3)

    if instr( file$, "." ) = 0 then file$ = file$ + ".LNG"
    tbl% = 1

    for x% = 1 to 3
       l%(x%) = lbound( lngarray&(x%) )
       u%(x%) = ubound( lngarray&(x%) )
       tbl% = tbl% * ((u%(x%) - (l%(x%)-1) ))
    next x%

    tbl% = tbl% * 4
    s = varseg( lngarray&( l%(1), l%(2), l%(3) ))
    o = varptr( lngarray&( l%(1), l%(2), l%(3) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:lngarray3bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :qudarray1bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub qudarray1bload ( file$, qudarray&&(1) ) public
   local s 		'segment
   local o 		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:1)

    if instr( file$, "." ) = 0 then file$ = file$ + ".QUD"
    tbl% = 1

    for x% = 1 to 1
       l%(x%) = lbound( qudarray&&(x%) )
    next x%

    s = varseg( qudarray&&( l%(1) ))
    o = varptr( qudarray&&( l%(1) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:qudarray1bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :qudarray1bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub qudarray1bsave ( file$, qudarray&&(1) ) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:1)

     dim u%(1:1)

    if instr( file$, "." ) = 0 then file$ = file$ + ".QUD"
    tbl% = 1

    for x% = 1 to 1
       l%(x%) = lbound( qudarray&&(x%) )
       u%(x%) = ubound( qudarray&&(x%) )
       tbl% = tbl% * ((u%(x%) - (l%(x%)-1) ) )
    next x%

    tbl% = tbl% * 8
    s = varseg( qudarray&&( l%(1) ))
    o = varptr( qudarray&&( l%(1) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:qudarray1bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :qudarray2bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub qudarray2bload ( file$, qudarray&&(2) ) public
   local s 		'segment
   local o 		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:2)

    if instr( file$, "." ) = 0 then file$ = file$ + ".QUD"
    tbl% = 1

    for x% = 1 to 2
       l%(x%) = lbound( qudarray&&(x%) )
    next x%

    s = varseg( qudarray&&( l%(1), l%(2) ))
    o = varptr( qudarray&&( l%(1), l%(2) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:qudarray2bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :qudarray2bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub qudarray2bsave ( file$, qudarray&&(2) ) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:2)

     dim u%(1:2)

    if instr( file$, "." ) = 0 then file$ = file$ + ".QUD"
    tbl% = 1

    for x% = 1 to 2
       l%(x%) = lbound( qudarray&&(x%) )
       u%(x%) = ubound( qudarray&&(x%) )
       tbl% = tbl% * ((u%(x%) - (l%(x%)-1) ) )
    next x%

    tbl% = tbl% * 8
    s = varseg( qudarray&&( l%(1), l%(2) ))
    o = varptr( qudarray&&( l%(1), l%(2) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:qudarray2bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :qudarray3bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub qudarray3bload ( file$, qudarray&&(3) ) public
   local s 		'segment
   local o 		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:3)

    if instr( file$, "." ) = 0 then file$ = file$ + ".QUD"
    tbl% = 1

    for x% = 1 to 3
       l%(x%) = lbound( qudarray&&(x%) )
    next x%

    s = varseg( qudarray&&( l%(1), l%(2), l%(3) ))
    o = varptr( qudarray&&( l%(1), l%(2), l%(3) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:qudarray3bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :qudarray3bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub qudarray3bsave ( file$, qudarray&&(3) ) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:3)

     dim u%(1:3)

    if instr( file$, "." ) = 0 then file$ = file$ + ".QUD"
    tbl% = 1

    for x% = 1 to 3
       l%(x%) = lbound( qudarray&&(x%) )
       u%(x%) = ubound( qudarray&&(x%) )
       tbl% = tbl% * ((u%(x%) - (l%(x%)-1) ) )
    next x%

    tbl% = tbl% * 8
    s = varseg( qudarray&&( l%(1), l%(2), l%(3) ))
    o = varptr( qudarray&&( l%(1), l%(2), l%(3) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:qudarray3bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :sngarray1bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub sngarray1bload ( file$, sngarray!(1) ) public
   local s 		'segment
   local o 		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:1)

    if instr( file$, "." ) = 0 then file$ = file$ + ".SNG"
    tbl% = 1

    for x% = 1 to 1
       l%(x%) = lbound( sngarray!(x%) )
    next x%

    s = varseg( sngarray!( l%(1) ))
    o = varptr( sngarray!( l%(1) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:sngarray1bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :sngarray1bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub sngarray1bsave ( file$, sngarray!(1) ) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:1)

     dim u%(1:1)

    if instr( file$, "." ) = 0 then file$ = file$ + ".SNG"
    tbl% = 1

    for x% = 1 to 1
       l%(x%) = lbound( sngarray!(x%) )
       u%(x%) = ubound( sngarray!(x%) )
       tbl% = tbl% * ((u%(x%) - (l%(x%)-1) ))
    next x%

    tbl% = tbl% * 4
    s = varseg( sngarray!( l%(1) ))
    o = varptr( sngarray!( l%(1) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:sngarray1bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :sngarray2bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub sngarray2bload ( file$, sngarray!(2) ) public
   local s 		'segment
   local o 		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:2)

    if instr( file$, "." ) = 0 then file$ = file$ + ".SNG"
    tbl% = 1

    for x% = 1 to 2
       l%(x%) = lbound( sngarray!(x%) )
    next x%

    s = varseg( sngarray!( l%(1), l%(2) ))
    o = varptr( sngarray!( l%(1), l%(2) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:sngarray2bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :sngarray2bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub sngarray2bsave ( file$, sngarray!(2) ) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:2)

     dim u%(1:2)

    if instr( file$, "." ) = 0 then file$ = file$ + ".SNG"
    tbl% = 1

    for x% = 1 to 2
       l%(x%) = lbound( sngarray!(x%) )
       u%(x%) = ubound( sngarray!(x%) )
       tbl% = tbl% * ((u%(x%) - (l%(x%)-1) ))
    next x%

    tbl% = tbl% * 4
    s = varseg( sngarray!( l%(1), l%(2) ))
    o = varptr( sngarray!( l%(1), l%(2) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:sngarray2bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :sngarray3bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub sngarray3bload ( file$, sngarray!(3) ) public
   local s 		'segment
   local o 		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:3)

    if instr( file$, "." ) = 0 then file$ = file$ + ".SNG"
    tbl% = 1

    for x% = 1 to 3
       l%(x%) = lbound( sngarray!(x%) )
    next x%

    s = varseg( sngarray!( l%(1), l%(2), l%(3) ))
    o = varptr( sngarray!( l%(1), l%(2), l%(3) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:sngarray3bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :sngarray3bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub sngarray3bsave ( file$, sngarray!(3) ) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:3)

     dim u%(1:3)

    if instr( file$, "." ) = 0 then file$ = file$ + ".SNG"
    tbl% = 1

    for x% = 1 to 3
       l%(x%) = lbound( sngarray!(x%) )
       u%(x%) = ubound( sngarray!(x%) )
       tbl% = tbl% * ((u%(x%) - (l%(x%)-1) ))
    next x%

    tbl% = tbl% * 4
    s = varseg( sngarray!( l%(1), l%(2), l%(3) ))
    o = varptr( sngarray!( l%(1), l%(2), l%(3) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:sngarray3bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :dblarray1bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub dblarray1bload ( file$, dblarray#(1) ) public
   local s 		'segment
   local o 		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:1)

    if instr( file$, "." ) = 0 then file$ = file$ + ".DBL"
    tbl% = 1

    for x% = 1 to 1
       l%(x%) = lbound( dblarray#(x%) )
    next x%

    s = varseg( dblarray#( l%(1) ))
    o = varptr( dblarray#( l%(1) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:dblarray1bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :dblarray1bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub dblarray1bsave ( file$, dblarray#(1) ) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:1)

     dim u%(1:1)

    if instr( file$, "." ) = 0 then file$ = file$ + ".DBL"
    tbl% = 1

    for x% = 1 to 1
       l%(x%) = lbound( dblarray#(x%) )
       u%(x%) = ubound( dblarray#(x%) )
       tbl% = tbl% * ((u%(x%) - (l%(x%)-1) ))
    next x%

    tbl% = tbl% * 8
    s = varseg( dblarray#( l%(1) ))
    o = varptr( dblarray#( l%(1) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:dblarray1bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :dblarray2bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub dblarray2bload ( file$, dblarray#(2) ) public
   local s 		'segment
   local o 		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:2)

    if instr( file$, "." ) = 0 then file$ = file$ + ".DBL"
    tbl% = 1

    for x% = 1 to 2
       l%(x%) = lbound( dblarray#(x%) )
    next x%

    s = varseg( dblarray#( l%(1), l%(2) ))
    o = varptr( dblarray#( l%(1), l%(2) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:dblarray2bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :dblarray2bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub dblarray2bsave ( file$, dblarray#(2) ) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:2)

     dim u%(1:2)

    if instr( file$, "." ) = 0 then file$ = file$ + ".DBL"
    tbl% = 1

    for x% = 1 to 2
       l%(x%) = lbound( dblarray#(x%) )
       u%(x%) = ubound( dblarray#(x%) )
       tbl% = tbl% * ((u%(x%) - (l%(x%)-1) ))
    next x%

    tbl% = tbl% * 8
    s = varseg( dblarray#( l%(1), l%(2) ))
    o = varptr( dblarray#( l%(1), l%(2) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:dblarray2bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :dblarray3bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub dblarray3bload ( file$, dblarray#(3) ) public
   local s 		'segment
   local o 		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:3)

    if instr( file$, "." ) = 0 then file$ = file$ + ".DBL"
    tbl% = 1

    for x% = 1 to 3
       l%(x%) = lbound( dblarray#(x%) )
    next x%

    s = varseg( dblarray#( l%(1), l%(2), l%(3) ))
    o = varptr( dblarray#( l%(1), l%(2), l%(3) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:dblarray3bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :dblarray3bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub dblarray3bsave ( file$, dblarray#(3) ) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:3)

     dim u%(1:3)

    if instr( file$, "." ) = 0 then file$ = file$ + ".DBL"
    tbl% = 1

    for x% = 1 to 3
       l%(x%) = lbound( dblarray#(x%) )
       u%(x%) = ubound( dblarray#(x%) )
       tbl% = tbl% * ((u%(x%) - (l%(x%)-1) ))
    next x%

    tbl% = tbl% * 8
    s = varseg( dblarray#( l%(1), l%(2), l%(3) ))
    o = varptr( dblarray#( l%(1), l%(2), l%(3) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:dblarray3bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :extarray1bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub extarray1bload ( file$, extarray##(1) ) public
   local s 		'segment
   local o 		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:1)

    if instr( file$, "." ) = 0 then file$ = file$ + ".EXT"
    tbl% = 1

    for x% = 1 to 1
       l%(x%) = lbound( extarray##(x%) )
    next x%

    s = varseg( extarray##( l%(1) ))
    o = varptr( extarray##( l%(1) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:extarray1bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :extarray1bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub extarray1bsave ( file$, extarray##(1) ) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:1)

     dim u%(1:1)

    if instr( file$, "." ) = 0 then file$ = file$ + ".EXT"
    tbl% = 1

    for x% = 1 to 1
       l%(x%) = lbound( extarray##(x%) )
       u%(x%) = ubound( extarray##(x%) )
       tbl% = tbl% * ((u%(x%) - (l%(x%)-1) ) )
    next x%

    tbl% = tbl% * 10
    s = varseg( extarray##( l%(1) ))
    o = varptr( extarray##( l%(1) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:extarray1bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :extarray2bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub extarray2bload ( file$, extarray##(2) ) public
   local s 		'segment
   local o 		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:2)

    if instr( file$, "." ) = 0 then file$ = file$ + ".EXT"
    tbl% = 1

    for x% = 1 to 2
       l%(x%) = lbound( extarray##(x%) )
    next x%

    s = varseg( extarray##( l%(1), l%(2) ))
    o = varptr( extarray##( l%(1), l%(2) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:extarray2bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :extarray2bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub extarray2bsave ( file$, extarray##(2) ) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:2)

     dim u%(1:2)

    if instr( file$, "." ) = 0 then file$ = file$ + ".EXT"
    tbl% = 1

    for x% = 1 to 2
       l%(x%) = lbound( extarray##(x%) )
       u%(x%) = ubound( extarray##(x%) )
       tbl% = tbl% * ((u%(x%) - (l%(x%)-1) ) )
    next x%

    tbl% = tbl% * 10
    s = varseg( extarray##( l%(1), l%(2) ))
    o = varptr( extarray##( l%(1), l%(2) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:extarray2bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :extarray3bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub extarray3bload ( file$, extarray##(3) ) public
   local s 		'segment
   local o 		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:3)

    if instr( file$, "." ) = 0 then file$ = file$ + ".EXT"
    tbl% = 1

    for x% = 1 to 3
       l%(x%) = lbound( extarray##(x%) )
    next x%

    s = varseg( extarray##( l%(1), l%(2), l%(3) ))
    o = varptr( extarray##( l%(1), l%(2), l%(3) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:extarray3bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :extarray3bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub extarray3bsave ( file$, extarray##(3) ) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:3)

     dim u%(1:3)

    if instr( file$, "." ) = 0 then file$ = file$ + ".EXT"
    tbl% = 1

    for x% = 1 to 3
       l%(x%) = lbound( extarray##(x%) )
       u%(x%) = ubound( extarray##(x%) )
       tbl% = tbl% * ((u%(x%) - (l%(x%)-1) ) )
    next x%

    tbl% = tbl% * 10
    s = varseg( extarray##( l%(1), l%(2), l%(3) ))
    o = varptr( extarray##( l%(1), l%(2), l%(3) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:extarray3bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :fixarray1bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub fixarray1bload ( file$, fixarray@(1) ) public
   local s 		'segment
   local o 		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:1)

    if instr( file$, "." ) = 0 then file$ = file$ + ".FIX"
    tbl% = 1

    for x% = 1 to 1
       l%(x%) = lbound( fixarray@(x%) )
    next x%

    s = varseg( fixarray@( l%(1) ))
    o = varptr( fixarray@( l%(1) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:fixarray1bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :fixarray1bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub fixarray1bsave ( file$, fixarray@(1) ) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:1)

     dim u%(1:1)

    if instr( file$, "." ) = 0 then file$ = file$ + ".FIX"
    tbl% = 1

    for x% = 1 to 1
       l%(x%) = lbound( fixarray@(x%) )
       u%(x%) = ubound( fixarray@(x%) )
       tbl% = tbl% * ((u%(x%) - (l%(x%)-1) ))
    next x%

    tbl% = tbl% * 8
    s = varseg( fixarray@( l%(1) ))
    o = varptr( fixarray@( l%(1) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:fixarray1bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :fixarray2bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub fixarray2bload ( file$, fixarray@(2) ) public
   local s 		'segment
   local o 		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:2)

    if instr( file$, "." ) = 0 then file$ = file$ + ".FIX"
    tbl% = 1

    for x% = 1 to 2
       l%(x%) = lbound( fixarray@(x%) )
    next x%

    s = varseg( fixarray@( l%(1), l%(2) ))
    o = varptr( fixarray@( l%(1), l%(2) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:fixarray2bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :fixarray2bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub fixarray2bsave ( file$, fixarray@(2) ) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:2)

     dim u%(1:2)

    if instr( file$, "." ) = 0 then file$ = file$ + ".FIX"
    tbl% = 1

    for x% = 1 to 2
       l%(x%) = lbound( fixarray@(x%) )
       u%(x%) = ubound( fixarray@(x%) )
       tbl% = tbl% * ((u%(x%) - (l%(x%)-1) ))
    next x%

    tbl% = tbl% * 8
    s = varseg( fixarray@( l%(1), l%(2) ))
    o = varptr( fixarray@( l%(1), l%(2) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:fixarray2bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :fixarray3bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub fixarray3bload ( file$, fixarray@(3) ) public
   local s 		'segment
   local o 		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:3)

    if instr( file$, "." ) = 0 then file$ = file$ + ".FIX"
    tbl% = 1

    for x% = 1 to 3
       l%(x%) = lbound( fixarray@(x%) )
    next x%

    s = varseg( fixarray@( l%(1), l%(2), l%(3) ))
    o = varptr( fixarray@( l%(1), l%(2), l%(3) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:fixarray3bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :fixarray3bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub fixarray3bsave ( file$, fixarray@(3) ) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:3)

     dim u%(1:3)

    if instr( file$, "." ) = 0 then file$ = file$ + ".FIX"
    tbl% = 1

    for x% = 1 to 3
       l%(x%) = lbound( fixarray@(x%) )
       u%(x%) = ubound( fixarray@(x%) )
       tbl% = tbl% * ((u%(x%) - (l%(x%)-1) ))
    next x%

    tbl% = tbl% * 8
    s = varseg( fixarray@( l%(1), l%(2), l%(3) ))
    o = varptr( fixarray@( l%(1), l%(2), l%(3) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:fixarray3bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :bcdarray1bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub bcdarray1bload ( file$, bcdarray@@(1) ) public
   local s 		'segment
   local o 		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:1)

    if instr( file$, "." ) = 0 then file$ = file$ + ".BCD"
    tbl% = 1

    for x% = 1 to 1
       l%(x%) = lbound( bcdarray@@(x%) )
    next x%

    s = varseg( bcdarray@@( l%(1) ))
    o = varptr( bcdarray@@( l%(1) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:bcdarray1bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :bcdarray1bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub bcdarray1bsave ( file$, bcdarray@@(1)) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:1)

     dim u%(1:1)

    if instr( file$, "." ) = 0 then file$ = file$ + ".BCD"
    tbl% = 1

    for x% = 1 to 1
       l%(x%) = lbound( bcdarray@@(x%) )
       u%(x%) = ubound( bcdarray@@(x%) )
       tbl% = tbl% * ((u%(x%) - (l%(x%)-1) ))
    next x%

    tbl% = tbl% * 10
    s = varseg( bcdarray@@( l%(1) ))
    o = varptr( bcdarray@@( l%(1) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:bcdarray1bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :bcdarray2bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub bcdarray2bload ( file$, bcdarray@@(2) ) public
   local s 		'segment
   local o 		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:2)

    if instr( file$, "." ) = 0 then file$ = file$ + ".BCD"
    tbl% = 1

    for x% = 1 to 2
       l%(x%) = lbound( bcdarray@@(x%) )
    next x%

    s = varseg( bcdarray@@( l%(1), l%(2) ))
    o = varptr( bcdarray@@( l%(1), l%(2) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:bcdarray2bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :bcdarray2bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub bcdarray2bsave ( file$, bcdarray@@(2)) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:2)

     dim u%(1:2)

    if instr( file$, "." ) = 0 then file$ = file$ + ".BCD"
    tbl% = 1

    for x% = 1 to 2
       l%(x%) = lbound( bcdarray@@(x%) )
       u%(x%) = ubound( bcdarray@@(x%) )
       tbl% = tbl% * ((u%(x%) - (l%(x%)-1) ))
    next x%

    tbl% = tbl% * 10
    s = varseg( bcdarray@@( l%(1), l%(2) ))
    o = varptr( bcdarray@@( l%(1), l%(2) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:bcdarray2bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :bcdarray3bload                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub bcdarray3bload ( file$, bcdarray@@(3) ) public
   local s 		'segment
   local o 		'offset
   local x%		'internal use
   local l%()		'first field of all dimmensions

     dim l%(1:3)

    if instr( file$, "." ) = 0 then file$ = file$ + ".BCD"
    tbl% = 1

    for x% = 1 to 3
       l%(x%) = lbound( bcdarray@@(x%) )
    next x%

    s = varseg( bcdarray@@( l%(1), l%(2), l%(3) ))
    o = varptr( bcdarray@@( l%(1), l%(2), l%(3) ))
    def seg = s
    bload file$, o
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:bcdarray3bload                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :bcdarray3bsave                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub bcdarray3bsave ( file$, bcdarray@@(3)) public
   local s 		'segment
   local o 		'offset
   local tbl%		'total bytes length of the array
   local x%		'internal use
   local l%()		'first field of all dimmensions
   local u%()		'last field of all dimmensions

     dim l%(1:3)

     dim u%(1:3)

    if instr( file$, "." ) = 0 then file$ = file$ + ".BCD"
    tbl% = 1

    for x% = 1 to 3
       l%(x%) = lbound( bcdarray@@(x%) )
       u%(x%) = ubound( bcdarray@@(x%) )
       tbl% = tbl% * ((u%(x%) - (l%(x%)-1) ))
    next x%

    tbl% = tbl% * 10
    s = varseg( bcdarray@@( l%(1), l%(2), l%(3) ))
    o = varptr( bcdarray@@( l%(1), l%(2), l%(3) ))
    def seg = s
    bsave file$, o , tbl%
    def seg

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:bcdarray3bsave                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




