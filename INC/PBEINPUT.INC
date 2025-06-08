$INCLUDE "PBSCR  .INC"
$INCLUDE "PBKBD  .INC"

CALL KBD.Speed(0,0)

CALL SET.ESmallestErrorText( " Smallest input is : ")
CALL SET.EGreatestErrorText( " Greatest input is : ")
CALL SET.EExceptErrorText( " Input except : ")
CALL SET.EOnlyErrorText( " Input only : ")
CALL SET.EDateErrorText( " Input date in format : ")
CALL SET.ETimeErrorText( " Input time in format : ")

CALL SET.EDisplayError(1)
CALL SET.EErrorBeepON
CALL SET.EKBDBeepON
'---------------------------------------------------------------

SCREEN 0
CALL SET.EErrorColor(10,5)


dim num%(1:12)
    num%(1) = 0
    num%(2) = 1
    num%(3) = 2
    num%(4) = 3
    num%(5) = 4
    num%(6) = 5
    num%(7) = 10
    num%(8) = 11
    num%(9) = 12
    num%(10) = 13
    num%(11) = 14
    num%(12) = 15


p% = 1

for p% = 1 to 12
   locate 5+p%, 10
   num$ = EInput.NUM$(num%(p%), 15, 15, e%)
   select case e%
     case 1
       exit for
     case 2
       if p% > 1 then decr p%, 2
     case 5
       if ereturn% then decr p%, 2
   end select
next p%


'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Function...: EInput.TEXT$                                              บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     lend% ===> lend of dat to input                                     บ
'บ     type% ===> type of text to read                                     บ
'บ                                                                         บ
'บ                  INPUT FORMAT                    	OUTPUT FORMAT      บ
'บ              0 = text   UCASE or LCASE           	"EsTEve vALenTก"   บ
'บ              1 = text all UCASE                  	"ESTEVE VALENTI"   บ
'บ 	        2 = text all LCASE                  	"esteve valentก"   บ
'บ      	3 = firts characters in words UCASE 	"Esteve Valentก"   บ
'บ           	4 = firts characters in words LCASE 	"eSTEVE vALENTI"   บ
'บ                                                                         บ
'บ  RETURN:                                                                บ
'บ     EInput.TEXT$ ===> character string                                  บ
'บ     e%           ===> return code. same EInput$                         บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
FUNCTION EInput.TEXT$( lend%, type%, e% )  PUBLIC
  EInput.TEXT$ = EInput$( lend%, type%, e% )
END FUNCTION





'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Function...: EInput.NUM$                                               บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     type% ===> type of number to input                                  บ
'บ                                                                         บ
'บ               INTEGERS       	  DECIMALS     	    OUTPUT FORMAT  บ
'บ           0 = right adjust		  left adjust	    "    1532.12 " บ
'บ           1 = right adjust		  left adjust,ceros "    1532.120" บ
'บ           2 = right adjust,ceros 	  left adjust 	    "00001532.12 " บ
'บ           3 = right adjust,ceros  	  left adjust,ceros "00001532.120" บ
'บ           4 = right adjust,commas  	  left adjust	    "   1,532.12 " บ
'บ           5 = right adjust,commas  	  left adjust,ceros "   1,532.120" บ
'บ                                                                         บ
'บ          10 = sign,right adjust 	  left adjust	    "+   1532.12 " บ
'บ          11 = sign,right adjust  	  left adjust,ceros "+   1532.120" บ
'บ          12 = sign,ceros,right adjust  left adjust	    "+0001532.12 " บ
'บ          13 = sign,ceros,right adjust  left adjust,ceros "+0001532.120" บ
'บ          14 = sign,commas,right adjust left adjust	    "+  1,532.12 " บ
'บ          15 = sign,commas,right adjust left adjust,ceros "+  1,532.120" บ
'บ                                                                         บ
'บ     nint% ===> number of intergers to input   (max. 15)                 บ
'บ     ndec% ===> number of decimalt to input    (max. 15)                 บ
'บ                                                                         บ
'บ  RETURN:                                                                บ
'บ     EInput.NUM$  ===> character string                                  บ
'บ     e%           ===> return code. same EInput$                         บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
FUNCTION EInput.NUM$( type%, nint%, ndec%, e% )  PUBLIC
  SHARED filterinpreviusfunction%
  LOCAL  posy%, posx%
  LOCAL  nint$, ndec$
  LOCAL  lint$, ldec$
  LOCAL  mask$

  IF nint% > 15 OR  nint% < 0 OR_		' test valid lengt
     ndec% > 15 OR  ndec% < 0 OR_
     (nint% = 0 AND ndec% = 0) THEN
     PRINT "Invalid number lengt. Program break! ":END
  END IF
  IF type%=4  OR type%=5 OR_			' if commas
     type%=14 OR type%=15 THEN
     nint% = nint% + ( nint% \ 4 )
  END IF
  SELECT CASE type%				' select type
     CASE 0:  nint$ = "_10_": ndec$ = "_11_"
     CASE 1:  nint$ = "_10_": ndec$ = "_13_"
     CASE 2:  nint$ = "_12_": ndec$ = "_11_"
     CASE 3:  nint$ = "_12_": ndec$ = "_13_"
     CASE 4:  nint$ = "_14_": ndec$ = "_11_"
     CASE 5:  nint$ = "_14_": ndec$ = "_13_"
     CASE 10: nint$ = "_20_": ndec$ = "_11_"
     CASE 11: nint$ = "_20_": ndec$ = "_13_"
     CASE 12: nint$ = "_22_": ndec$ = "_11_"
     CASE 13: nint$ = "_22_": ndec$ = "_13_"
     CASE 14: nint$ = "_24_": ndec$ = "_11_"
     CASE 15: nint$ = "_24_": ndec$ = "_13_"
     CASE ELSE					'  test valid type
	  PRINT "Invalid number type. Program break! ":END
  END SELECT
  IF nint% <> 0 THEN lint$ = nint$ + SPACE$(nint%)
  IF ndec% <> 0 THEN ldec$ = "." + ndec$ + SPACE$(ndec%)
  mask$ = lint$ + ldec$				' make mask$

     filterinpreviusfunction% = -1
     posy% = CSRLIN
     posx% = POS(0)

     DO
       filtererror% = 0
       LOCATE posy%, posx%
	x$ = EInput.MASK$( mask$, e% )
	IF e%=1 OR e%=2 THEN EXIT FUNCTION

       filtererror% = EFilter%(x$,1,0)		' test error

     LOOP WHILE filtererror%			' loop if error

END FUNCTION




'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Function...: EInput.DATE$                                              บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     type% ===> type of text to read                                     บ
'บ                                                                         บ
'บ              0 = USA, 4 digits year, with /     	"12/26/1967"       บ
'บ              1 = USA, 4 digits year              	"12261967"         บ
'บ 	        5 = USA, 2 digits year, with /       	"12/26/67"         บ
'บ      	6 = USA, 2 digits year              	"122667"           บ
'บ           	10= EUROPA, 4 digits year, with /   	"26/12/1967"       บ
'บ              11= EUROPA, 4 digits year               "26121967"         บ
'บ              15= EUROPA, 2 digits year, with /       "26/12/67"         บ
'บ              16= EUROPA, 2 digits year               "261267"           บ
'บ                                                                         บ
'บ  RETURN:                                                                บ
'บ     EInput.DATE$ ===> character string                                  บ
'บ     e%           ===> return code. same EInput$                         บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
FUNCTION EInput.DATE$( type%, e% )			PUBLIC
   LOCAL mask$, dateformat$
   LOCAL valideinput%
   LOCAL posy%, posx%
   LOCAL m%, d%, y%
   SHARED EDateText$
   SHARED filterinpreviusfunction%
     IF type% > 9 THEN typedate% = 1
     SELECT CASE type%
	CASE 0
	  mask$ = "_12_  /_12_  /_12_    "
	  dateformat$ = "MM/DD/YYYY"
	CASE 1
	  mask$ = "_12_  _12_  _12_    "
	  dateformat$ = "MMDDYYYY"
	CASE 5
	  mask$ = "_12_  /_12_  /_12_  "
	  dateformat$ = "MM/DD/YY"
	CASE 6
	  mask$ = "_12_  _12_  _12_  "
	  dateformat$ = "MMDDYY"
	CASE 10
	  mask$ = "_12_  /_12_  /_12_    "
	  dateformat$ = "DD/MM/YYYY"
	CASE 11
	  mask$ = "_12_  _12_  _12_    "
	  dateformat$ = "DDMMYYYY"
	CASE 15
	  mask$ = "_12_  /_12_  /_12_  "
	  dateformat$ = "DD/MM/YY"
	CASE 16
	  mask$ = "_12_  _12_  _12_  "
	  dateformat$ = "DDMMYY"
	CASE ELSE
	  PRINT "Invalid date type. Program Break!":END
     END SELECT
     filterinpreviusfunction% = -1
     posy% = CSRLIN
     posx% = POS(0)

     DO
       filtererror% = 0

	DO
	  LOCATE posy%, posx%
	  x$ = EInput.MASK$( mask$, e% )
	  IF e%=1 OR e%=2 THEN EXIT FUNCTION
	  CALL SplitDATE(x$, typedate%, m%, d%, y%)
	  valideinput% = TestDATE%(m%, d%, y%)
	  IF NOT valideinput% THEN
	     CALL EDisplayError(EDateText$+dateformat$)
	  END IF
	LOOP UNTIL valideinput%

       filtererror% = EFilter%(x$,2,typedate%)	' test error
     LOOP WHILE filtererror%			' loop if error

END FUNCTION




'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Function...: EInput.MASK$                                              บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     mask$ ===> character string with EInput mask control.               บ
'บ                                                                         บ
'บ           "?"   = no blank space is printed in the screen between       บ
'บ                   the EInput's.                                         บ
'บ           " "   = spaces for the previus EInput instruction (_x_)       บ
'บ                                                                         บ
'บ           "_0_" = text   UCASE or LCASE           	"EsTEve vALenTก"   บ
'บ           "_1_" = text all UCASE                  	"ESTEVE VALENTI"   บ
'บ 	     "_2_" = text all LCASE                  	"esteve valentก"   บ
'บ           "_3_" = firts characters in words UCASE 	"Esteve Valentก"   บ
'บ           "_4_" = firts characters in words LCASE 	"eSTEVE vALENTI"   บ
'บ           "_10_"= Numeric, right adjust               "     1234521"    บ
'บ           "_11_"=    "   , left adjust                "1234521     "    บ
'บ           "_12_"=    "   , with ceros, right adjust   "000001234521"    บ
'บ           "_13_"=    "   , with ceros, left adjust    "123452100000"    บ
'บ           "_14_"=    "   , with comma, right adjust   "   1,234,521"    บ
'บ           "_20_"= Numeric, sign, right adjust         "-    1234521"    บ
'บ           "_21_"=    "   , sign, left adjust          "+1234521    "    บ
'บ           "_22_"=    "   , sign, ceros, right adjust  "-00001234521"    บ
'บ           "_23_"=    "   , sign, ceros, left adjust   "-12345210000"    บ
'บ           "_24_"=    "   , sign, right adjust         "-  1,234,521"    บ
'บ  RETURN:                                                                บ
'บ     EInput.MASK$ ===> character string                                  บ
'บ     e%           ===> return code. same EInput$                         บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ   NOTES:                                                                บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  The EInput.MASK$ function can be util to input same diferents types    บ
'บ  of formated dats supporteds for EInput$ in only one instruccion.       บ
'บ                                                                         บ
'บ   EXAMPLE:                                                              บ
'บ                                                                         บ
'บ       mask$ = "_2_ -_14_          /_2_  "                               บ
'บ                 | || |  |         | |  |                                บ
'บ                 | || |  |         | |  two spaces for EInput$           บ
'บ                 | || |  |         | EInput type 2 (text lower case)     บ
'บ                 | || |  |         character to print in screen          บ
'บ                 | || |  ten spaces for EInput$                          บ
'บ                 | || EInput type 14                                     บ
'บ                 | |character to print in screen                         บ
'บ                 | one space for EInput$                                 บ
'บ                 EInput type 2                                           บ
'บ                                                                         บ
'บ       nif$    = EInput.MASK$( mask$ )                                   บ
'บ       phone$  = EInput.MASK$( "(_12_   )_12_   ._12_  ._12_  "          บ
'บ                                                                         บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
FUNCTION EInput.MASK$( mask$, e% )         			PUBLIC
  SHARED EKBDBeep%		' beep if key pressed don't answer
  SHARED EReturn%		' Automaticall return?
  SHARED EDefault$		' printed in screen if screen = ""
  SHARED EWMenu$		' values displayed in a pull-down window
  SHARED FilterInPreviusFunction%' logical variable for enable and
				 ' disable test of filters.
  LOCAL  oldEReturn%		' to hold EReturn%
  LOCAL  mask%			' pointer to actual EInput
  LOCAL  mask%()		' array to split mask$ parameters
  DIM mask%(1:50,1:4)		' to hold splited parameters of mask$
  LOCAL  type$, mmask$, c$	' to split mask$
  LOCAL  onlend%, ontype%	' to split mask$
  LOCAL  posy%, posx%		' position in screen
  LOCAL  lend%			' lengt total of dat
  LOCAL  x$, p% 		' internal use
  LOCAL  filtererror%		' internal use

  posy% = CSRLIN
  posx% = POS(0)

  IF NOT EReturn% THEN
     oldEReturn% = EReturn%
     CALL SET.EReturnON
  END IF

  FOR p% = 1 TO LEN(mask$)
      c$ = MID$(mask$, p%, 1)
      SELECT CASE c$

	CASE "_"
	  IF ontype% THEN
	     ontype% = 0
	     onlend% = -1
	     INCR mask%, 1
	     mask%(mask%,1)=VAL(type$)
	     mask%(mask%,3)=posy%
	     mask%(mask%,4)=posx%+LEN(mmask$)
	     type$ = ""
	  ELSE
	     ontype% = -1
	     IF onlend% THEN
		onlend% = 0
		mask%(mask%,2) = lend%
		lend% = 0
	     END IF
	  END IF

	CASE " "
	  IF onlend% THEN
	     INCR lend%, 1
	     IF p% = LEN(mask$) THEN
		mask%(mask%,2) = lend%
	     END IF
	  END IF
	  IF NOT ontype% THEN mmask$ = mmask$ + c$

	CASE ELSE
	  IF ontype% THEN
	     type$ = type$ + c$
	  ELSE
	     type$ = ""
	     IF onlend% THEN
		onlend% = 0
		mask%(mask%,2) = lend%
		lend% = 0
	     END IF
	     mmask$ = mmask$ + c$
	  END IF
      END SELECT
  NEXT p%

  lend% = LEN(mmask$) + mask%(mask%,2)		' lengt total of EInput

  IF mask% > 0 THEN				' mask OK!, so

     x$ = SCR.text$(posy%,posx%,lend%)		' screen text in x$
     IF RTRIM$(LTRIM$(x$)) = "" THEN		' not exist text
	IF EDefault$ <> "" THEN			'  exist default text
	   PRINT EDefault$;			'   print default text
	ELSE                                    '  not
	   PRINT mmask$;                        '   print mask
	END IF
     ELSE                                       ' exist text
	IF x$ = mmask$ AND EDefault$ <> "" THEN	'  text = mask$
	   PRINT EDefault$;			'   print default text
	END IF
     END IF
     EDefault$ = ""				' initialize EDefault$

    DO
     filtererror% = 0
     p% = 1					' pointer to mask%()

      DO					' bucle to control e%

       LOCATE mask%(p%,3), mask%(p%,4)
       x$ = EInput$( mask%(p%,2), mask%(p%,1), e% )

       SELECT CASE e%
	 CASE 0					' RETURN
	   IF p% = mask% THEN
	      EXIT LOOP
	   ELSE
	      INCR p%
	   END IF
	 CASE 1					' ESC
	   filterinpreviusfunction% = 0
	   EXIT FUNCTION
	 CASE 2					' UP
	   IF p% > 1 THEN
	      DECR p%,1
	   ELSE
	      EXIT LOOP
	   END IF
	 CASE 3					' RIGHT
	   IF p% < mask% THEN
	      INCR p%,1
	   ELSE
	      IF oldEReturn% THEN
		 EXIT LOOP
	      ELSE
		 IF EKBDBeep% THEN CALL EBeep
	      END IF
	   END IF
	 CASE 4					' DOWN
	   IF p% < mask% THEN
	      INCR p%,1
	   ELSE
	      EXIT LOOP
	   END IF
	 CASE 5					' LEFT
	   IF p% > 1 THEN
	      DECR p%
	   ELSE
	      IF oldEReturn% THEN
		 EXIT LOOP
	      ELSE
		 IF EKBDBeep% THEN CALL EBeep
	      END IF
	   END IF
       END SELECT
      LOOP WHILE -1

     IF NOT filterinpreviusfunction% THEN
	x$ = SCR.text$(posy%,posx%,lend%)	' dat of screen
	filtererror% = EFilter%(x$,0,0)		' test error
     END IF

    LOOP WHILE filtererror%			' loop if error

  ELSE
     PRINT "Ilegall Mask$. Program Break!";:END	' ilegal mask$. END
  END IF

  EInput.MASK$ = SCR.text$(posy%, posx%, lend%)	' text to return

  IF NOT oldEReturn% THEN CALL SET.EReturnOFF	' restore old EReturn%

END FUNCTION











FUNCTION EDATE#( mdate$, type% ) 			PUBLIC
   LOCAL m%,d%,y%
   LOCAL date#

   CALL SplitDATE( mdate$, type%, m%, d%, y% )
   IF NOT TestDATE%(m%, d%, y%) THEN
      PRINT "Incorrect DATE format. Program Break!":END
   END IF
   date# = y% * 10000
   date# = date# + (m% * 100)
   date# = date# + d%
   EDATE# = date#
END FUNCTION


SUB SplitDATE( mdate$, type%, m%, d%, y%) 		PUBLIC
    LOCAL i1%, i2%, i3%
    LOCAL lend%
      lend% = LEN(mdate$)
      IF INSTR(mdate$, "/") OR_		' separated with /
	 INSTR(mdate$, "-") OR_		' separated with -
	 INSTR(mdate$, "\") THEN	' separated with \
	 i1%=1 : i2%=4 : i3%=7
      ELSE
	 i1%=1 : i2%=3 : i3%=5
      END IF
      SELECT CASE type%
	 CASE 0
	   m% = VAL(MID$(mdate$,i1%,2))
	   d% = VAL(MID$(mdate$,i2%,2))
	   y% = VAL(MID$(mdate$,i3%,lend%+1-i3%))
	 CASE 1
	   m% = VAL(MID$(mdate$,i2%,2))
	   d% = VAL(MID$(mdate$,i1%,2))
	   y% = VAL(MID$(mdate$,i3%,lend%+1-i3%))
	 CASE ELSE
	   PRINT "Error in SplitDATE. Program Break!":END
      END SELECT
      IF lend%+1-i3% = 2 THEN y% = y% + 1900
END SUB


FUNCTION TestDate%( m%, d%, y% )			PUBLIC
    IF  m%<1 OR m%>12 OR_
	d%<1 OR d%> VAL(MID$(" 312931303130313130313031",m%*2,2)) OR_
	(m%=2 AND d%>28 AND y%/4.0 > y%\4 ) THEN

	TestDate% = 0
    ELSE
	TestDate% = -1
    END IF
END FUNCTION










FUNCTION ETIME#( mtime$, type% ) PUBLIC

END FUNCTION
























'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina..: SET.EDefault                                             บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     text$ ===> string variable with the default text to print in        บ
'บ                the screen after use the Einput$ functions.              บ
'บ                This SUB need the text$ are passed with the same format  บ
'บ                text returned for the EInput functions and with the exactบ
'บ                lengt.                                                   บ
'บ  EXAMPLES:                                                              บ
'บ     CALL SET.EDefault( "Esteve     " )                                  บ
'บ     CALL SET.EDefault( "   23" )                                        บ
'บ     CALL SET.EDefault( "12/26/1967" )                                   บ
'บ     CALL SET.EDefault( "12:03" )                                        บ
'บ     CALL SET.EDefault( "(972) 32.50.94" )                               บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB SET.EDefault( text$ ) PUBLIC
    SHARED EDefault$
    EDefault$ = text$
END SUB

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina..: SET.ESmallest                                            บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     text$ ===> string variable with small text accepted for EInput$,    บ
'บ                EInput.xxx$ Functions and GET.xxxx SUB procedures.       บ
'บ                This SUB need the text$ are passed with the same text    บ
'บ                returned for the EInput functions but without spaces     บ
'บ                at left and at rigth, or with spaces, but the compara-   บ
'บ                tion use LTRIM and RTRIM to truncate the strings to      บ
'บ                comapare.                                                บ
'บ  EXAMPLES:                                                              บ
'บ     CALL SET.ESmallest( "aaaaaaaaa" )                                   บ
'บ     CALL SET.ESmallest( "0" )                                           บ
'บ     CALL SET.ESmallest( "01/01/1980" )                                  บ
'บ     CALL SET.ESmallest( "00:01" )                                       บ
'บ     CALL SET.ESmallest( "(000)000.00.00" )                              บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB SET.ESmallest( text$ ) PUBLIC
    SHARED ESmallest$
    ESmallest$ = text$
END SUB


'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina..: SET.EGreatest                                            บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     text$ ===> string variable with great text accepted for EInput$,    บ
'บ                EInput.xxx$ Functions and GET.xxxx SUB procedures.       บ
'บ                This SUB need the text$ are passed with the same text    บ
'บ                returned for the EInput functions but without spaces     บ
'บ                at left and at rigth, or with spaces, but the compara-   บ
'บ                tion use LTRIM and RTRIM to truncate the strings to      บ
'บ                comapare.                                                บ
'บ  EXAMPLES:                                                              บ
'บ     CALL SET.EGreatest( "ZZZZZZZZZ" )                                   บ
'บ     CALL SET.EGreatest( "999" )                                         บ
'บ     CALL SET.EGreatest( "01/12/1991" )                                  บ
'บ     CALL SET.EGreatest( "24:60" )                                       บ
'บ     CALL SET.EGreatest( "(999)999.99.99" )                              บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB SET.EGreatest( text$ ) PUBLIC
    SHARED EGreatest$
    EGreatest$ = text$
END SUB


'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina..: SET.EOnly                                                บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     text$ ===> string variable with text's only accepteds for EInput$,  บ
'บ                EInput.xxx$ Functions and GET.xxxx SUB procedures        บ
'บ                separed with accents and commas.                         บ
'บ                This SUB need the text$ are passed with the same text    บ
'บ                returned for the EInput functions but without spaces     บ
'บ                at left and at rigth, or with spaces, but the compara-   บ
'บ                tion use LTRIM and RTRIM to truncate the strings to      บ
'บ                compare.                                                 บ
'บ  EXAMPLES:                                                              บ
'บ                                                                         บ
'บ     CALL SET.EOnly( "'Esteve','Josep','Eduard','Raquel','Oriol'" )      บ
'บ     CALL SET.EOnly( "'Y','N'" )                                         บ
'บ     CALL SET.EOnly( "'1','2','4','7' )                                  บ
'บ     CALL SET.EOnly( "'1,245.10','2503','500' )                          บ
'บ     CALL SET.EOnly( "'12/26/67','122666','12/26/1968','06/01/1992')     บ
'บ     CALL SET.EOnly( "'12:60 am','24:60' )                               บ
'บ     CALL SET.EOnly( "'COMMAND .COM','AUTOEXEC.BAT'"                     บ
'บ     CALL SET.EOnly( "'(972)032.50.94','40.523.237-L'"                   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB SET.EOnly( text$ ) PUBLIC
    SHARED EOnly$
    EOnly$ = text$
END SUB


'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina..: SET.EExcept                                              บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     text$ ===> string variable with text's only not accepteds for EInputบ
'บ                EInput.xxx$ Functions and GET.xxxx SUB procedures        บ
'บ                separed with accents and commas.                         บ
'บ                This SUB need the text$ are passed with the same text    บ
'บ                returned for the EInput functions but without spaces     บ
'บ                at left and at rigth, or with spaces, but the compara-   บ
'บ                tion use LTRIM and RTRIM to truncate the strings to      บ
'บ                compare.                                                 บ
'บ  EXAMPLES:                                                              บ
'บ                                                                         บ
'บ     CALL SET.EExcept( "'Esteve','Josep','Eduard','Raquel','Oriol'" )    บ
'บ     CALL SET.EExcept( "'Y','N'" )                                       บ
'บ     CALL SET.EExcept( "'1','2','4','7' )                                บ
'บ     CALL SET.EExcept( "'1,245.10','2503','500' )                        บ
'บ     CALL SET.EExcept( "'12/26/67','122666','12/26/1968','06/01/1992')   บ
'บ     CALL SET.EExcept( "'12:60 am','24:60' )                             บ
'บ     CALL SET.EExcept( "'COMMAND .COM','AUTOEXEC.BAT'"                   บ
'บ     CALL SET.EExcept( "'(972)032.50.94','40.523.237-L'"                 บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB SET.EExcept( text$ ) PUBLIC
    SHARED EExcept$
    EExcept$ = text$
END SUB


SUB SET.EWMenu( text$ ) PUBLIC
    SHARED EWindowMenu$
    EWindowMenu$ = text$
END SUB

































SUB SET.ESmallestErrorText( text$ ) PUBLIC
  SHARED ESmallesttext$
  ESmallesttext$ = text$
END SUB

SUB SET.EGreatestErrorText( text$ ) PUBLIC
  SHARED EGreatesttext$
  EGreatesttext$ = text$
END SUB

SUB SET.EExceptErrorText( text$ ) PUBLIC
  SHARED EExcepttext$
  EExcepttext$ = text$
END SUB

SUB SET.EOnlyErrorText( text$ ) PUBLIC
  SHARED EOnlytext$
  EOnlytext$ = text$
END SUB

SUB SET.EDateErrorText( text$ ) PUBLIC
  SHARED EDateText$
  EDateText$ = text$
END SUB

SUB SET.ETimeErrorText( text$ ) PUBLIC
  SHARED EtimeText$
  EtimeText$ = text$
END SUB

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina..: SET.EErrorColor                                          บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     fgc% ===> Fore Ground Color to use in EInput$                       บ
'บ     bgc% ===> Back Ground Color to use in EInput$                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB SET.EErrorColor( fgc%, bgc% ) PUBLIC
  SHARED EErrorfgc%
  SHARED EErrorbgc%
    EErrorfgc% = fgc%
    EErrorbgc% = bgc%
END SUB

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina..: SET.EDisplayError                                        บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     type% = type of display error                                       บ
'บ                 0 = not displayed                                       บ
'บ                 1 = displayed in a screen line                          บ
'บ                 2 = displayed in a window                               บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB SET.EDisplayError( type% )PUBLIC
  SHARED EErrorDisplay%
  EErrorDisplay% = type%
END SUB



'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina..: SET.EErrorBeepON                                         บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  ACCTION:                                                               บ
'บ     Turn ON the beep in EInput$                                         บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB SET.EErrorBeepON  PUBLIC
  SHARED EErrorBeep%
  EErrorBeep% = -1
END SUB



'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina..: SET.EErrorBeepOFF                                        บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  ACCTION:                                                               บ
'บ     Turn OFF the beep in EInput$                                        บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB SET.EErrorBeepOFF PUBLIC
  SHARED EErrorBeep%
  EErrorBeep% = 0
END SUB



SUB EDisplayError( text$ )
 SHARED EErrorRow%
 SHARED EErrorBeep%
 SHARED EErrorDisplay%
 SHARED EErrorfgc%, EErrorbgc%
 LOCAL columns%
 LOCAL y%, o%, a$
 LOCAL oldline$
 LOCAL fgc%,bgc%

   fgc% = EErrorfgc%
   bgc% = EErrorbgc%
   IF fgc% = bgc% OR_			' default colors if error
      fgc% < 0 OR fgc% > 16 OR_
      bgc% < 0 OR bgc% > 8 THEN
       fgc% = 0
       bgc% = 7
   END IF
   columns% = SCR.columns
   y% = SCR.rows - 3
   IF EErrorRow% <> 0 THEN y% = ErrorRow%
   IF EErrorBeep% THEN CALL EBeep
   text$ = LEFT$(text$+space$(80),columns%)
   COLOR fgc%,bgc%

   SELECT CASE EErrorDisplay%
     CASE 1
       oldline$ = SCR.ChrAttr$(y%,1,columns%)
       LOCATE y%, 1
       PRINT text$;
       a$ = INPUT$(1)
       o% = SCR.columns * 2 * (y% - 1) + 2 * (1 - 1)
       POKE$ o%, oldline$
     CASE 2


    END SELECT
END SUB



'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Funciขn .....: EInput$       Rev 3.00      02 de Febrero   de 1.991    บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ      lend%   ====> lenght of dat to input                               บ
'บ      type%   ====> type of dat to input:                                บ
'บ                                                                         บ
'บ                  INPUT FORMAT                    	OUTPUT FORMAT      บ
'บ              0 = text   UCASE or LCASE           	"EsTEve vALenTก"   บ
'บ              1 = text all UCASE                  	"ESTEVE VALENTI"   บ
'บ 	        2 = text all LCASE                  	"esteve valentก"   บ
'บ      	3 = firts characters in words UCASE 	"Esteve Valentก"   บ
'บ           	4 = firts characters in words LCASE 	"eSTEVE vALENTI"   บ
'บ              10= Numeric, right adjust               "     1234521"     บ
'บ              11=    "   , left adjust                "1234521     "     บ
'บ              12=    "   , with ceros, right adjust   "000001234521"     บ
'บ              13=    "   , with ceros, left adjust    "123452100000"     บ
'บ              14=    "   , with comma, right adjust   "   1,234,521"     บ
'บ              20= Numeric, sign, right adjust         "-    1234521"     บ
'บ              21=    "   , sign, left adjust          "+1234521    "     บ
'บ              22=    "   , sign, ceros, right adjust  "-00001234521"     บ
'บ              23=    "   , sign, ceros, left adjust   "-12345210000"     บ
'บ              24=    "   , sign, right adjust         "-  1,234,521"     บ
'บ  RETURN:                                                                บ
'บ      EInput$ ===> character string                                      บ
'บ      ERet%   ===> public variable for return code                       บ
'บ                              if RETURN                 ERet%= 0         บ
'บ                              if ESC                    ERet%= 1         บ
'บ                              if UP                  	  ERet%= 2         บ
'บ                              if RIGHT at last                           บ
'บ                                 character and EReturn% ERet%= 3         บ
'บ                              if TAB RIGHT  ---->|      ERet%= 3         บ
'บ                              if DOWN                   ERet%= 4         บ
'บ                              if LEFT at first                           บ
'บ                                 character and EReturn% ERet%= 5         บ
'บ                              if TAB LEFT   |<----      ERet%= 5         บ
'บ      e%      ===> same information of ERet%                             บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ   NOTES:                                                                บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  Assume the column ( POS(0) ) and the row ( CSRLIN ) actives in the     บ
'บ  screen.                                                                บ
'บ                                                                         บ
'บ  EInput$ is susceptible by:                                             บ
'บ                                                                         บ
'บ       SET.EInputColor( fgc%, bgc%)     subrroutine                      บ
'บ       SET.EOutPutColor(fgc%, bgc%)     subrroutine                      บ
'บ       SET.EInsertON    SET.EInsertOFF  subrroutines                     บ
'บ       SET.EReturnON    SET.EReturnOFF  subrroutines                     บ
'บ       SET.EKBDBeepON   SET.EKBDBeepOFF subrroutines                     บ
'บ       SET.EInputFilter$( filter$ )     subrroutine                      บ
'บ                                                                         บ
'บ  The ERet% return code will be util to make this after function.        บ
'บ                                                                         บ
'บ             ON ERet% GOTO label1, label2, label3, label4, label5        บ
'บ                          label1 = label to go if ESC                    บ
'บ                          label2 =     "    previus EInput$              บ
'บ                          lebel3 =     "    secuent    "                 บ
'บ                          label4 =     "    upper      "                 บ
'บ                          lebel5 =     "    lower      "                 บ
'บ   EXAMPLE:                                                              บ
'บ                                                                         บ
'บ      dat1:                                                              บ
'บ       LOCATE 9, 10 	                      ' start EInput$ position     บ
'บ       dat1$ = EInput$(.... . .                                          บ
'บ       ON ERet% GOTO enddat, dat1, dat2, dat1, dat2                      บ
'บ        IF condition to dat1$ not true THEN GOTO dat1                    บ
'บ      dat2:                                                              บ
'บ       CALL SET.InputColor( 15, 3 )	      ' set input colors           บ
'บ       CALL SET.OutputColor( 2, 6 )         ' set output colors          บ
'บ       CALL SET.EReturnON                   ' turn ON EReturn%           บ
'บ       CALL SET.EInserON                    ' turn ON EInsert%           บ
'บ       LOCATE 10, 10                        ' start EInput$ position     บ
'บ       dat2$ = EInput$(.... . .                                          บ
'บ        IF ERet% = 0 THEN GOTO enddat                                    บ
'บ        IF condition to dat2$ not true THEN GOTO dat2                    บ
'บ        IF ERet% = 2 OR ERet% = 5 THEN GOTO dat1                         บ
'บ        IF ERet% = 3 OR ERet% = 4 THEN GOTO dat3                         บ
'บ      dat3:                                                              บ
'บ       LOCATE 11, 10 	                      ' start EInput$ position     บ
'บ       dat3$ = EInput$(.... . .                                          บ
'บ       ON ERet% GOTO enddat, dat2, dat4, dat2, dat4                      บ
'บ        IF condition to dat3$ not true THEN GOTO dat3                    บ
'บ      dat4:                                                              บ
'บ        .                                                                บ
'บ        .                                                                บ
'บ      enddat:                                                            บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
FUNCTION EInput$( lend%, type%, e% )				PUBLIC
 LOCAL t$			' to get te key pressed
 LOCAL num##, num$ 		' to number calculation
 LOCAL musing$			' to use whit PRINT.
 LOCAL w%,x%,f%,l%,d$,n%,m% 	' internal use
 LOCAL posy%, posx%, my%, mx%	' to hold old cursor location
 LOCAL iei%			' internall EInsert
 LOCAL ifgc%, ibgc%		' internall input colors
 LOCAL ofgc%, obgc% 		' internall output colors
 SHARED ERet%			' control ESC,PAGUP,PAGDW
 SHARED EInPutfgc%		' input fore ground color
 SHARED EInPutbgc%		' input back ground color
 SHARED EOutPutfgc%		' output fore ground color
 SHARED EOutPutbgc%		' output back ground color
 SHARED EInsert%		' logical variable for automaticall insertion
 SHARED EReturn%		' logical variable for automaticall return
 SHARED EKBDBeep%		' logical variable for beep if error
 SHARED EKBDFilter$		' filter to keyboad
 STATIC up%                     ' inte.use: Only true if EReturn% and UP
  ERet%= 0				' public return code = 0
  e%   = 0				' param. return code = 0

  posy% = CSRLIN			' line, row to start EInput
  posx% = POS(0)			' column to start EInput

  ifgc% = EInputfgc%			' colors
  ibgc% = EInputbgc%
  ofgc% = EOutputfgc%
  obgc% = EOutPutbgc%
  IF ifgc% = ibgc% OR_			' default colors if error
     ifgc% < 0 OR ifgc% > 16 OR_
     ibgc% < 0 OR ibgc% > 8 THEN
      ifgc% = 0
      ibgc% = 7
  END IF
  IF ofgc% = obgc% OR_			' default colors if error
     ofgc% < 0 OR ofgc% > 16 OR_
     obgc% < 0 OR obgc% > 8 THEN
      ofgc% = 7
      obgc% = 0
  END IF
  COLOR ifgc%, ibgc%			' set colors

  IF EInsert% THEN			' set cursor
     iei% = -1
     LOCATE posy%, posx%, 1, 0, 16
  ELSE
     iei% = 0
     LOCATE posy%, posx%, 1, 13, 16
  END IF

  '*******************************************************************
  '* Print the input values in screen with the input format MUSING$  *
  '* to easy the work at the user.                                   *
  '*******************************************************************
  SELECT CASE type%
   CASE 0 TO 4					' ALPHANUMERIC
     PRINT SCR.Text$(posy%, posx%, lend%);
   CASE 10 TO 14				' NUMERIC without sign
     IF lend% < 1  THEN lend% = 1                ' minium value of lend
     IF lend% > 18 THEN lend% = 18               ' maxium value of lend
      num$  = SCR.text$(posy%,posx%,lend%)        ' get num of screen
      num$  = LegalNum$(num$)
      num## = VAL(num$)				 ' make num##
      f% = LEN(STR$(FIX(num##))) - 1		 ' prepare musing$
      musing$ = STRING$(f%, "#")		 '
      PRINT SPACE$(lend%);                       ' print black space
      LOCATE posy%, posx%                        ' go to start input
      PRINT USING musing$; num##;                ' print num at left
      IF type% = 14 THEN                         ' if commas
	 m% = lend% \ 4		                 '  number of commas
	 lend% = lend% - m%			 '  reduce lend%
      END IF                                     ' end if
   CASE 20 TO 24				' NUMERICS with sign
     IF lend% < 2  THEN lend% = 2		 'minium value of lend
     IF lend% > 19 THEN lend% = 19		 'maxium value of lend
      num$  = SCR.text$(posy%,posx%,lend%)        ' get num of screen
      num$  = LegalNum$(num$)
      num## = VAL(num$)				 ' make num##
      f% = LEN(STR$(FIX(num##))) - 1
      musing$ = "+" + STRING$(f%, "#")
      PRINT SPACE$(lend%);
      LOCATE posy%, posx%
      PRINT USING musing$; num##;
      IF type% = 24 THEN                         ' if commas
	 m% = (lend% - 1) \ 4	                 '  number of commas
	 lend% = lend% - m%			 '  reduce lend%
      END IF                                     ' end if
   CASE ELSE
      PRINT "Error in EInput$ passed parameters. Program break";:END
  END SELECT
  '******************************************************************




'******************************************************************
'* Bucle to filter the input errors                               *
'******************************************************************
DO
 e% = 0                         	' error false
 COLOR ifgc%, ibgc%			' set input colors

 IF EReturn% AND up% THEN		' set cursor again
    up% = 0
    d$ = SCR.Text$(posy%, posx%, lend%)
    mx% = LEN(RTRIM$(d$))
    LOCATE posy%, posx% + mx%
 ELSE
    LOCATE posy%, posx%
 END IF

 IF iei% THEN				' set cursor again
    LOCATE , , 1, 0, 16
 ELSE
    LOCATE , , 1, 13, 16
 END IF

 '*******************************************************************
 '* Trapp keys and print his characters to the screen if his has    *
 '* correct with the type% defined.                                 *
 '*******************************************************************
 DO
  t$ = INKEY$					' t$ = key pressed

  SELECT CASE t$
   CASE CHR$(&H8)				' DELETE TO LEFT
     IF POS(0) > posx% THEN
      f% = (((CSRLIN - 1) * 2 * SCR.columns) + (POS(0) - 2) * 2)
      l% = (((CSRLIN - 1) * 2 * SCR.columns) + (posx% + lend% - 3) * 2)
      FOR x% = f% TO l% STEP 2
	POKE x%, PEEK(x% + 2)
      NEXT x%
      POKE x%, 32
      LOCATE CSRLIN, POS(0) - 1
     ELSE
      IF EReturn% THEN
	 ERet%= 2: up% = -1
	 EXIT LOOP
      END IF
     END IF
   CASE CHR$(&H0) + CHR$(&H53)			' DELETE TO RIGHT
     IF POS(0) < posx% + lend% THEN
      f% = (((CSRLIN - 1) * SCR.columns * 2) + (POS(0) - 1) * 2)
      l% = (((CSRLIN - 1) * SCR.columns * 2) + (posx% + lend% - 3) * 2)
      FOR x% = f% TO l% STEP 2
	POKE x%, PEEK(x% + 2)
      NEXT x%
      POKE x%, 32
     END IF
   CASE CHR$(&H0) + CHR$(&H52)			' INSERT ON/OFF
     IF iei% THEN
	iei% = 0
	LOCATE , , , 13, 16
     ELSE
	iei% = -1
	LOCATE , , , 0, 16
     END IF
   CASE CHR$(&H0) + CHR$(&H75)			' CTRL + FIN
     IF posx% + lend% > POS(0) THEN
	l% = posx% + lend% - POS(0)
	my% = CSRLIN : mx% = POS(0)
	PRINT SPACE$(l%);
	LOCATE my%, mx%
     END IF
   CASE CHR$(&H0) + CHR$(&H77)			' CTRL + INICIO
     IF POS(0) > posx% THEN
	l% = POS(0) - posx%
	LOCATE posy%, posx%
	PRINT SPACE$(l%);
     END IF
   CASE CHR$(&H0) + CHR$(&H4B)			' LEFT
     IF POS(0) > posx% THEN
	LOCATE CSRLIN, POS(0) - 1
     ELSE
	IF EReturn% THEN
	   ERet%= 5: up% = -1
	   EXIT LOOP
	ELSE
	   IF EKBDBeep% THEN CALL EBeep
	END IF
     END IF
   CASE CHR$(&H0) + CHR$(&H4D)			' RIGHT
     IF POS(0) < posx% + lend% THEN
	LOCATE CSRLIN, POS(0) + 1
     ELSE
	IF EReturn% THEN
	   ERet%= 3
	   EXIT LOOP
	ELSE
	   IF EKBDBeep% THEN CALL EBeep
	END IF
     END IF
   CASE CHR$(&H0) + CHR$(&H4F)			' END | FIN
     IF posx% + lend% - 1 >= POS(0) THEN
	l% = posx% + lend% - POS(0)
	d$ = SCR.Text$(posy%, POS(0) + 1, l% - 1) + " "
	w% = INSTR(d$, " ")
	LOCATE CSRLIN, POS(0) + w%
     ELSE
	IF EReturn% THEN
	   ERet%= 3
	   EXIT LOOP
	ELSE
	   IF EKBDBeep% THEN CALL EBeep
	END IF
     END IF
   CASE CHR$(&H0) + CHR$(&H47)			' HOME
     IF POS(0) > posx% THEN
	LOCATE posy%, posx%
     ELSE
	IF EReturn% THEN
	   ERet%= 5: up% = -1
	   EXIT LOOP
	ELSE
	   IF EKBDBeep% THEN CALL EBeep
	END IF
     END IF
   CASE CHR$(&H1B)                              ' ESC           (ERet%=1)
     ERet%= 1 : e% = 1 : EKBDFilter$ = "" : EXIT FUNCTION
   CASE CHR$(&H0) + CHR$(&H48)                  ' UP            (ERet%=2)
     ERet%= 2 : EXIT LOOP
   CASE CHR$(&H0) + CHR$(&HF)                   ' TAB <         (ERet%=2)
     ERet%= 5 : EXIT LOOP
   CASE CHR$(&H0) + CHR$(&H50)                  ' DOWN          (ERet%=3)
     ERet%= 4 : EXIT LOOP
   CASE CHR$(&H9)                               ' TAB >         (ERet%=3)
     ERet%= 4 : EXIT LOOP

   CASE " " TO CHR$(&HFF)			' PRINTING CHARACTERS
    IF EKBDFilter$="" OR_          		' if not filter to keyboard
       (EKBDFilter$<>"" AND _			' or (filter to keyboard and
       INSTR(EKBDFilter$,t$)<>0) THEN		' key pressed is in filter
    SELECT CASE type%
     CASE 0                                     '  ucase and lcase
       p$ = t$
     CASE 1                                     '  only ucase
       p$ = UCASE$(t$)
     CASE 2                                     '  only lcase
       p$ = LCASE$(t$)
     CASE 3                                     '  firsts ucase
       d$ = SCR.Text$(CSRLIN, POS(0) - 1, 1)
       IF d$ = " " OR POS(0) = posx% THEN
	  p$ = UCASE$(t$)
       ELSE
	  p$ = LCASE$(t$)
       END IF
     CASE 4                                     '  firsts lcase
       d$ = SCR.Text$(CSRLIN, POS(0) - 1, 1)
       IF d$ = " " OR POS(0) = posx% THEN
	  p$ = LCASE$(t$)
       ELSE
	  p$ = UCASE$(t$)
       END IF
     CASE 10 TO 14                              ' NUMBERS without sign
       p$ = CHR$(&H7)
       IF INSTR("0123456789", t$) <> 0 THEN
	  p$ = t$
       END IF
     CASE 20 TO 24                              ' NUMBERS with sign
       p$ = CHR$(&H7)
       IF INSTR("0123456789", t$) <> 0 AND_
	  POS(0) <> posx% THEN
	  p$ = t$
       ELSE
	  IF (t$ = "-" OR t$ = "+") AND_
	     posx% = POS(0) AND_
	     NOT iei% THEN
	     p$ = t$
	  END IF
       END IF
    END SELECT
    ELSE                			' filter not passed
       p$ = CHR$(&h07)  			' beep?
    END IF
   CASE ELSE                                    ' if others BEEP
     IF t$ <> "" AND t$ <> CHR$(&HD)_
	AND EKBDBeep% THEN CALL EBeep
  END SELECT

  '****************************************************************
  '* Print the string hold in p$ and control EInsert% and EReturn%*
  '****************************************************************
  IF p$ <> "" THEN                              ' control EInsert
   IF iei% AND p$ <> "" AND p$ <> CHR$(&H7) THEN
     IF POS(0) < posx% + lend% THEN
      f% = (((CSRLIN - 1) * SCR.columns * 2) + (posx% + lend% - 2) * 2)
      l% = (((CSRLIN - 1) * SCR.columns * 2) + (POS(0) * 2))
      FOR x% = f% TO l% STEP -2
	POKE x%, PEEK(x% - 2)
      NEXT x%
      POKE x%, 32
     END IF
   END IF

   IF POS(0) < posx% + lend% THEN               ' Print string
      IF p$ = CHR$(&h07) THEN
	 IF EKBDBeep% THEN CALL EBeep
      ELSE
	 PRINT p$;
      END IF
   ELSE
      IF EKBDBeep% THEN CALL EBeep
   END IF
   p$ = ""

   IF EReturn% AND POS(0) = posx% + lend% THEN  ' Control EReturn
      EXIT LOOP
   END IF
  END IF
  '****************************************************************

 LOOP UNTIL t$ = CHR$(&HD)
 '*******************************************************************

  n% = 0					' prepare exit
  musing$ = ""					' variables
  COLOR ofgc%, obgc%				' set output color
  LOCATE posy%, posx%, 0			' set output cursor

 '*******************************************************************
 '* Print the input values with the exit format.                    *
 '*******************************************************************
  SELECT CASE type%
   CASE 0 TO 4					' ALPHANUMERIC
     PRINT SCR.Text$(posy%, posx%, lend%);
   CASE 10 TO 14				' NUMERIC without sign
     num$  = SCR.Text$(posy%, posx%, lend%)
     num$  = LegalNum$( num$ )
     num## = VAL( num$ )
     SELECT CASE type%
      CASE 10
	PRINT RIGHT$("                  "+num$, lend% );
      CASE 11
	PRINT LEFT$ (num$+"                  ", lend% );
      CASE 12
	PRINT RIGHT$("000000000000000000"+num$,lend% );
      CASE 13
	PRINT LEFT$ (num$+"000000000000000000", lend% );
      CASE 14
	musing$ = REPEAT$(m%,",###")
	lend% = lend% + m%
	musing$ = RIGHT$("###"+musing$,lend%)
	PRINT USING musing$; num##;
     END SELECT
   CASE 20 TO 24				' NUMERIC with sign
     num$  = SCR.text$( posy%, posx%, lend% )
     num$  = LegalNum$(num$)
     num## = VAL(num$)
     IF num## >= 0 THEN
	PRINT "+";
     ELSE
	PRINT "-";
     END IF
     IF num$<>"0" THEN num$ = RIGHT$(num$,LEN(num$)-1)
     SELECT CASE type%
      CASE 20
	PRINT RIGHT$("                  "+num$, lend%-1 );
      CASE 21
	PRINT LEFT$ (num$+"                  ", lend%-1 );
      CASE 22
	PRINT RIGHT$("000000000000000000"+num$, lend%-1 );
      CASE 23
	PRINT LEFT$ (num$+"000000000000000000", lend%-1 );
      CASE 24
	musing$ = REPEAT$(m%,",###")
	lend% = lend% + m%
	musing$ = RIGHT$("###"+musing$,lend%-1)
	PRINT USING musing$; VAL(num$);
     END SELECT
  END SELECT
  '*****************************************************************

LOOP UNTIL NOT e%			' not error, so exit

e% = ERet%				' set return code in two variables
EKBDFilter$ = ""			' clear keyboard filter
EInput$ = SCR.Text$(posy%, posx%, lend%)' exit string characters
COLOR ofgc%, obgc%			' set output  colors

END FUNCTION




'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Funciขn .....: EInputA$      Rev 3.00      02 de Febrero   de 1.991    บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
FUNCTION EInputA$(posy%, posx%, lend%, ifgc%, ibgc%, ofgc%, obgc%,_
		  type%, EReturn%, EInset%, e%)			PUBLIC
   CALL SET.EInputColor(ifgc%,ibgc%)
   CALL SET.EOutputColor(ofgc%,obgc%)
   LOCATE posy%, posx%

   IF EReturn%THEN
      CALL SET.EReturnON
   ELSE
      CALL SET.EReturnOFF
   END IF
   IF EInsert% THEN
      CALL SET.EInsertON
   ELSE
      CALL SET.EInsertOFF
   END IF
    EInputA$ = EInput$( lend%, type%, e% )
END FUNCTION




'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina..: SET.EInPutColor                                          บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     fgc% ===> Fore Ground Color to use in EInput$                       บ
'บ     bgc% ===> Back Ground Color to use in EInput$                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB SET.EInPutColor( fgc%, bgc% ) PUBLIC
  SHARED EInPutfgc%
  SHARED EInPutbgc%
    EInPutfgc% = fgc%
    EInPutbgc% = bgc%
END SUB


'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina..: SET.EOutPutColor                                         บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  TO PASS:                                                               บ
'บ     fgc% ===> Fore Ground Color to use in EInput$ to print after exit   บ
'บ     bgc% ===> Back Ground Color to use in EInput$ to print after exit   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB SET.EOutPutColor( fgc%, bgc% ) PUBLIC
  SHARED EOutPutfgc%
  SHARED EOutPutbgc%
    EOutPutfgc% = fgc%
    EOutPutbgc% = bgc%
END SUB


'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina..: SET.EKBDFilter                                           บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  ACCTION:                                                               บ
'บ     Turn ON the beep in EInput$                                         บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB SET.EKBDFilter( filter$ )				PUBLIC
  SHARED EKBDFilter$
  EKBDFilter$ = filter$
END SUB


'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina..: SET.EKBDBeepON                                           บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  ACCTION:                                                               บ
'บ     Turn ON the beep in EInput$                                         บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB SET.EKBDBeepON  					PUBLIC
  SHARED EKBDBeep%
  EKBDBeep% = -1
END SUB


'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina..: SET.EKBDBeepOFF                                          บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  ACCTION:                                                               บ
'บ     Turn OFF the beep in EInput$                                        บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB SET.EKBDBeepOFF 					PUBLIC
  SHARED EKBDBeep%
  EKBDBeep% = 0
END SUB



'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina..: SET.EInsertON                                            บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  ACCTION:                                                               บ
'บ     Turn ON the insertion in EInput$.                                   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB SET.EInsertON  					PUBLIC
  SHARED EInsert%
  EInsert% = -1
END SUB
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina..: SET.EInsertOFF                                           บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  ACCTION:                                                               บ
'บ     Turn OFF the insertion in EInput$.                                  บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB SET.EInsertOFF 					PUBLIC
  SHARED EInsert%
  EInsert% = 0
END SUB




'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina..: SET.EReturnON                                            บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  ACCTION:                                                               บ
'บ     Turn ON the automaticall return in EINSERT$                         บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB SET.EReturnON  					PUBLIC
  SHARED EReturn%
  EReturn% = -1
END SUB
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina..: SET.EReturnOFF                                           บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  ACCTION:                                                               บ
'บ     Turn OFF the automaticall return in EINSERT$                        บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB SET.EReturnOFF  					PUBLIC
  SHARED EReturn%
  EReturn% = 0
END SUB






'*************************************************************************
'* Internall Packet Functions                                            *
'*************************************************************************

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Subrrutina..: EBeep                                                    บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ  ACCTION:                                                               บ
'บ     Play beep                                                           บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
SUB EBeep PUBLIC
  PLAY "L64 N70"
END SUB




FUNCTION LegalNum$( num$ )	PUBLIC
  LOCAL c%,l$,r$
      num$ = LTRIM$(RTRIM$(num$))
      REPLACE ANY "," WITH " " IN num$
      IF num$ <> "" THEN
	 WHILE INSTR(num$," ") <> 0		 ' prepare num
	    c% = INSTR(num$," ")
	    l$ = LEFT$(num$,c%-1)
	    r$ = RIGHT$(num$, LEN(num$)-c%)
	    num$ = l$ + r$
	 WEND
	 LegalNum$ = num$
      ELSE
	 LegalNum$ = "0"
      END IF
END FUNCTION






'**********************************************************************
'* dat$      ===>  dat to compare                                     *
'* type%     ===>  0=text  1=number  2=date  3=time                   *
'* typEDATE#% ===>  0=USA  1=EUROPA                                    *
'**********************************************************************
FUNCTION EFilter%( dat$, type%, datetype% ) PUBLIC

   SHARED ESmallest$, EGreatest$, EExcept$, EOnly$
   SHARED ESmallesttext$, EGreatesttext$, EExcepttext$, EOnlytext$
   LOCAL temp$()
   DIM   temp$(1:100)
   LOCAL only$, e%, x$
     x$ = RTRIM$(dat$)

     IF ESmallest$ <> "" THEN		' SMALLEST filter
	SELECT CASE type%
	  CASE 0			' text
	    IF x$ < RTRIM$(ESmallest$) THEN e% = -1
	  CASE 1			' numbers
	    IF VAL(LegalNum$(x$)) <_
	       VAL(LegalNum$(ESmallest$)) THEN e% = -1
	  CASE 2			' dates
	    IF EDATE#(x$,datetype%) <_
	       EDATE#(ESmallest$,datetype%) THEN e% = -1
	  CASE 3			' times
	    IF ETIME#(x$,datetype%) <_
	       ETIME#(ESmallest$,datetype%) THEN e% = -1
	END SELECT
	IF e% THEN
	   EFilter% = -1
	   CALL EDisplayError(ESmallesttext$+ESmallest$)
	   EXIT FUNCTION
	END IF
     END IF

     IF EGreatest$ <> "" THEN		' GREATEST filter
	SELECT CASE type%
	  CASE 0			' text
	    IF x$ > RTRIM$(EGreatest$) THEN e% = -1
	  CASE 1			' numbers
	    IF VAL(LegalNum$(x$)) >_
	       VAL(LegalNum$(EGreatest$)) THEN e% = -1
	  CASE 2			' dates
	    IF EDATE#(x$,datetype%) >_
	       EDATE#(EGreatest$,datetype%) THEN e% = -1
	  CASE 3			' times
	    IF ETIME#(x$,datetype%) >_
	       ETIME#(EGreatest$,datetype%) THEN e% = -1
	END SELECT
	IF e% THEN
	   EFilter% = -1
	   CALL EDisplayError(EGreatesttext$+EGreatest$)
	   EXIT FUNCTION
	END IF
     END IF

     IF EExcept$ <> "" THEN		' EXCEPT filter
       CALL SplitMyFormat( EExcept$, temp$() )
       p% = 1
	DO
	 SELECT CASE type%
	  CASE 0			' text
	    IF x$ = RTRIM$(temp$(p%)) THEN e% = -1
	  CASE 1			' numbers
	    IF VAL(LegalNum$(x$)) =_
	       VAL(LegalNum$(temp$(p%))) THEN e% = -1
	  CASE 2			' dates
	    IF EDATE#(x$,datetype%) =_
	       EDATE#(temp$(p%),datetype%) THEN e% = -1
	  CASE 3			' times
	    IF ETIME#(x$,datetype%) =_
	       ETIME#(temp$(p%),datetype%) THEN e% = -1
	 END SELECT
	 IF e% THEN
	   EFilter% = -1
	   CALL EDisplayError(EExcepttext$+temp$(p%))
	   EXIT FUNCTION
	 END IF
	 INCR p%,1
	LOOP UNTIL temp$(p%) = ""
     END IF

     IF EOnly$ <> "" THEN		' ONLY filter
       CALL SplitMyFormat( EOnly$, temp$() )
       p% = 1
       e% = -1
	DO
	 only$ = only$ + temp$(p%)+" "
	 SELECT CASE type%
	  CASE 0			' text
	    IF x$ = RTRIM$(temp$(p%)) THEN e% = 0
	  CASE 1			' numbers
	    IF VAL(LegalNum$(x$)) =_
	       VAL(LegalNum$(temp$(p%))) THEN e% = 0
	  CASE 2			' dates
	    IF EDATE#(x$,datetype%) =_
	       EDATE#(temp$(p%),datetype%) THEN e% = 0
	  CASE 3			' times
	    IF ETIME#(x$,datetype%) =_
	       ETIME#(temp$(p%),datetype%) THEN e% = 0
	 END SELECT
	 INCR p%,1
	LOOP UNTIL temp$(p%) = ""
	IF e% THEN
	   EFilter% = -1
	   CALL EDisplayError(EOnlytext$+only$)
	   EXIT FUNCTION
	END IF
     END IF

     EGreatest$ = ""			' Initialice all
     ESmallest$ = ""			' filters
     EExcept$   = ""
     EOnly$     = ""

END FUNCTION



'*****************************************************************
'*
'*
'*****************************************************************
SUB SplitMyFormat( var$, retarray$() )	PUBLIC
 LOCAL p%
 LOCAL c%
 LOCAL l%
  l% = 1
  WHILE INSTR(l%, var$, "'," ) <> 0
      c% = INSTR(l%,var$,"',")
      INCR p%
      retarray$(p%) = MID$(var$,l%+1, c%-l%-1)
      l% = c% + 2
  WEND
  retarray$(p%+1) = MID$(var$,l%+1, LEN(var$)-l%-1)
  retarray$(p%+2) = ""
END SUB
