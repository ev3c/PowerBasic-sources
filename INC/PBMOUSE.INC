'All mouse routines were derived from code for Turbo C found in "Programming
' the Microsoft Mouse" from MS Press

'IsMouse - Returns whether or not a mouse is installed and how many buttons
'written by Dave Navarro

'
'ΙΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝ»
'Ί FUNCTION :IsMouse%                                       02-09-1992   Ί
'ΜΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΉ
'Ί ACTION:                                                               Ί
'Ί                                                                       Ί
'Ί                                                                       Ί
'Ί TO PASS:                                                              Ί
'Ί                                                                       Ί
'Ί RETURN:                                                               Ί
'Ί    IsMouse%       ===>                                                Ί
'Ί                                                                       Ί
'ΘΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΌ
function ismouse% public
     reg 1, &h00
      call interrupt &h33
     stat%=reg(1)
     bttn%=reg(2)
     if bttn%=-1 then bttn%=2
     if stat% then ismouse%=bttn%
     'ΙΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝ»
     'Ί END FUNCTION:IsMouse%                                    02-09-1992   Ί
     'ΘΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΌ
end function





'MouseOn - Turn mouse cursor on
'written by Dave Navarro

'
'ΙΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝ»
'Ί SUB :MouseOn                                             02-09-1992   Ί
'ΜΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΉ
'Ί ACTION:                                                               Ί
'Ί                                                                       Ί
'Ί                                                                       Ί
'Ί TO PASS:                                                              Ί
'Ί                                                                       Ί
'Ί RETURN:                                                               Ί
'Ί                                                                       Ί
'ΘΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΌ
sub mouseon public
    reg 1, &h01
     call interrupt &h33
    'ΙΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝ»
    'Ί END SUB:MouseOn                                          02-09-1992   Ί
    'ΘΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΌ
end sub





'MouseOff - Turn mouse curson off
'written by Dave Navarro

'
'ΙΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝ»
'Ί SUB :MouseOff                                            02-09-1992   Ί
'ΜΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΉ
'Ί ACTION:                                                               Ί
'Ί                                                                       Ί
'Ί                                                                       Ί
'Ί TO PASS:                                                              Ί
'Ί                                                                       Ί
'Ί RETURN:                                                               Ί
'Ί                                                                       Ί
'ΘΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΌ
sub mouseoff public
    reg 1, &h02
     call interrupt &h33
    'ΙΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝ»
    'Ί END SUB:MouseOff                                         02-09-1992   Ί
    'ΘΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΌ
end sub





'MouseStat - Return button pressed and current row & column position of cursor
'            Left button - 1, Right button - 2, Middle button - 4
'written by Dave Navarro

'
'ΙΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝ»
'Ί SUB :MouseStat                                           02-09-1992   Ί
'ΜΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΉ
'Ί ACTION:                                                               Ί
'Ί                                                                       Ί
'Ί                                                                       Ί
'Ί TO PASS:                                                              Ί
'Ί    Button%        ===>                                                Ί
'Ί    Row%           ===>                                                Ί
'Ί    Col%           ===>                                                Ί
'Ί                                                                       Ί
'Ί RETURN:                                                               Ί
'Ί                                                                       Ί
'ΘΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΌ
sub mousestat(button%,row%,col%) public
    reg 1, &h03
     call interrupt &h33
    button%=reg(2)
    row%=reg(4)/8+1
    col%=reg(3)/8+1
    'ΙΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝ»
    'Ί END SUB:MouseStat                                        02-09-1992   Ί
    'ΘΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΌ
end sub





'MLocate - Locates the mouse cursor at Row, Col
'written by Dave Navarro

'
'ΙΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝ»
'Ί SUB :MLocate                                             02-09-1992   Ί
'ΜΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΉ
'Ί ACTION:                                                               Ί
'Ί                                                                       Ί
'Ί                                                                       Ί
'Ί TO PASS:                                                              Ί
'Ί    Row%           ===>                                                Ί
'Ί    Col%           ===>                                                Ί
'Ί                                                                       Ί
'Ί RETURN:                                                               Ί
'Ί                                                                       Ί
'ΘΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΌ
sub mlocate(row%, col%) public
    reg 1, &h04
    reg 3, col%*8-8
    reg 4, row%*8-8
     call interrupt &h33
    'ΙΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝ»
    'Ί END SUB:MLocate                                          02-09-1992   Ί
    'ΘΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΌ
end sub





'MouseWin - defines window for mouse cursor
'written by Dave Navarro

'
'ΙΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝ»
'Ί SUB :MouseWin                                            02-09-1992   Ί
'ΜΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΉ
'Ί ACTION:                                                               Ί
'Ί                                                                       Ί
'Ί                                                                       Ί
'Ί TO PASS:                                                              Ί
'Ί    Row%           ===>                                                Ί
'Ί    Col%           ===>                                                Ί
'Ί    Rows%          ===>                                                Ί
'Ί    Cols%          ===>                                                Ί
'Ί                                                                       Ί
'Ί RETURN:                                                               Ί
'Ί                                                                       Ί
'ΘΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΌ
sub mousewin(row%, col%, rows%, cols%) public
    rows%=row%+rows%-1
    cols%=col%+cols%-1
    reg 1, &h08
    reg 3, row%*8
    reg 4, rows%*8
     call interrupt &h33
    reg 1, &h07
    reg 3, col%*8
    reg 4, cols%*8
     call interrupt &h33
     call mlocate(row%, col%)
    'ΙΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝ»
    'Ί END SUB:MouseWin                                         02-09-1992   Ί
    'ΘΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΝΌ
end sub





