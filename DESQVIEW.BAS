
'$INCLUDE: 'qb.bi'

DvLocate 12, 30

STP$ = "Hello World"

Bprint STP$, 15, 1, 0
DvLocate 22, 2
Bprint HEX$(GetScreenSeg%(Dv$)), 15, 4, 0
DvLocate 23, 2
Bprint "DesqView Version Is.... " + Dv$, 15, 2, 0

'Again it's been 5 or 6 months since I have had any programming time but as I
'recall all the bugs are out of the above code. (if not Heh.. I'll refund your
'money). There are obvious enhancements and trimming that could be done and of
'course the TimeSlice release and function protect code can be added, I just
'don't have the time.

SUB Bprint (StrVar$, fore%, back%, Page%)
'==========================================================
'Simple Bios print routine. Recommend you check for
'DV and use standard color and print routines when not in
'loaded.
'===========================================================

DIM InRegs AS RegTypeX, OutRegs AS RegTypeX
Attrib = (16 * back%) + fore%

InRegs.Ax = (256 * &H13) + 1
InRegs.Bx = (256 * Page%) + Attrib
InRegs.Dx = (256 * GetRow%) + (GetCol%)
InRegs.Cx = LEN(StrVar$)
InRegs.Es = VARSEG(StrVar$)
InRegs.bp = SADD(StrVar$)

CALL InterruptX(&H10, InRegs, OutRegs)

END SUB

SUB DvLocate (row%, col%)
'==========================================================
'Bios cursor positioning routine to keep everything in the
'window
'==========================================================

DIM InRegs AS RegTypeX, OutRegs AS RegTypeX
InRegs.Ax = &H200
InRegs.Bx = &H0
InRegs.Dx = (256 * (row% - 1)) + (col% - 1)
CALL InterruptX(&H10, InRegs, OutRegs)

END SUB

FUNCTION GetCol%
'==========================================================
'Replaces the POS() function
'==========================================================

DIM InRegs AS RegTypeX, OutRegs AS RegTypeX
InRegs.Ax = (256 * &H3) + &H0
CALL InterruptX(&H10, InRegs, OutRegs)
GetCol% = OutRegs.Dx AND &HFF

END FUNCTION

