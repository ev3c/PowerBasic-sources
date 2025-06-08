$INCLUDE "PBSCR   .INC"
$INCLUDE "PBKBD   .INC"
$INCLUDE "PBEINERR.INC"
$INCLUDE "PBEINMAT.INC"
$INCLUDE "PBEINMAS.INC"

CALL KBD.Speed(0,0)
SCREEN 0

'
' These declarations are not compulsory for use EInput$ or EInput.MASK$, but
' personify the enviroment.
'
CALL SET.ESmallestErrorText( " Smallest input is : ") 	' previus texts for
CALL SET.EGreatestErrorText( " Greatest input is : ")   ' errors
CALL SET.EExceptErrorText( " Input except : ")
CALL SET.EOnlyErrorText( " Input only : ")
CALL SET.EDateErrorText( " Input date in format : ")
CALL SET.ETimeErrorText( " Input time in format : ")

CALL SET.EDisplayError(1)			' display errors in lines
CALL SET.EErrorBeepON				' beep wen display error
CALL SET.EErrorColor(10,5)			' color to display error

CALL SET.EInputColor( 10,5 )			' input color
CALL SET.EOutPutColor( 5,2 )			' output color
CALL SET.EKBDBeepON				' beep wen key pressed aren't OK
CALL SET.EKBDClickON				' click! wen key is pressed
CALL SET.EReturnON				' Auto Return ON
'CALL SET.EInsertON				'' Auto Insert ON
'---------------------------------------------------------------

'***************************************************************
'***************************************************************
'**  TEST EInput.TEXT$
'***************************************************************
'***************************************************************
color 7,0
cls

LOCATE 5,15: PRINT "TEST EInput.TEXT$";

text0:
   CALL SET.EDefault("Esteve")
   CALL SET.EOnly( "'Esteve    ','Valent¡   ','Morales   '" )
    locate 10,10
    t$ = EInput.TEXT$( 10,0,e% )
    on e% goto endtext,text0,text1,text1,text0

text1:
   CALL SET.ESmallest("AAAAAAAAAA")
   CALL SET.EGreatest("BBBBBBBBBB")
    locate 12,10
    t$ = EInput.TEXT$( 10,1,e% )
    on e% goto endtext,text0,text2,text2,text0

text2:
   CALL SET.EDefault("Esteve")
   CALL SET.EExcept("Esteve")
    locate 14,10
    t$ = EInput.TEXT$( 10,2,e% )
    on e% goto endtext,text1,text3,text3,text1

text3:

    locate 16,10
    t$ = EInput.TEXT$( 10,3,e% )
    on e% goto endtext,text2,text4,text4,text2

text4:

    locate 18,10
    t$ = EInput.TEXT$( 10,4,e% )
    on e% goto endtext,text3,text4,text4,text3

endtext:



'***************************************************************
'***************************************************************
'**  TEST EInput.DATE$
'***************************************************************
'***************************************************************
color 7,0
cls
LOCATE 5,15: PRINT "TEST EInput.DATE$";

dim date%(1:8)
  date%(1) = 0  : date%(2) = 1  : date%(3) = 5  : date%(4) = 6
  date%(5) = 10 : date%(6) = 11 : date%(7) = 15 : date%(8) = 16

CALL SET.EOnly$("'12/26/1967'")		' set the only date accepted at
					' first EInput.DATE$.
p% = 1
do
 locate 2+p%,10
 mdate$ = EInput.DATE$( date%(p%), e% )
 select case e%
   case 0, 3, 4
     incr p%
   case 1
     exit loop
   case 2, 5
     if p% > 1 then decr p%
 end select
loop until p% = 9


'***************************************************************
'***************************************************************
'**  TEST EInput.NUM$
'***************************************************************
'***************************************************************
color 7,0
cls

LOCATE 5,15: PRINT "TEST EInput.NUM$";

dim num%(1:12)
    num%(1) = 0 : num%(2) = 1 : num%(3) = 2 : num%(4) = 3
    num%(5) = 4 : num%(6) = 5 : num%(7) = 10 : num%(8) = 11
    num%(9) = 12 : num%(10) = 13 : num%(11) = 14 : num%(12) = 15

CALL SET.ESmallest("12")			' set the only number accepted
						' at firts EInput.NUM$.
p% = 1
do
   locate 5+p%, 10
   num$ = EInput.NUM$(num%(p%), 14, 14, e%)
   select case e%
     case 0, 3, 4
       incr p%
       if p% > 12 then exit loop
     case 1
       exit loop
     case 2, 5
       if p% > 1 then p% = p% -1
   end select
loop while -1