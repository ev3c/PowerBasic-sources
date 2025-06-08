'NOGOODFONT
'
' ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออปบ
'บ Library......: PBEINMAT.INC                                           บบ
'บ Definition...:                                                        บบ
'บ Author.......:                                                        บบ
'บ Date.........: 12-21-1991                                             บบ
'บ Observations.:                                                        บน
'บ                                                                       บบ
'บ                                                                       บบ
'บ                                                                       บบ
'บ                                                                       บบ
'บ                                                                       บบ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนบ
'บ SUBs.......:                                                          บบ
'บ FUNCTIONs..:                                                          บบ
'บ FNs........:                                                          บบ
'บ                                                                       บบ
'บ                                                                       บบ
'บ                                                                       บบ
'บ                                                                       บบ
'บ                                                                       บบ
'บ                                                                       บบ
'บ                                                                       บบ
'บ                                                                       บบ
'บ                                                                       บบ
'บ                                                                       บบ
'บ                                                                       บบ
'บ                                                                       บบ
'บ                                                                       บบ
'บ                                                                       บบ
'บ                                                                       บบ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ


'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ FUNCTION :einput$                                        12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    lend%          ===> len of dat to input                            บ
'บ    type%          ===> type of dat to input                           บ
'บ                                                                       บ
'บ                  INPUT FORMAT                    	OUTPUT FORMAT    บ
'บ              0 = text   ucase or lcase           	"EsTEve vALenTก" บ
'บ              1 = text all ucase                  	"ESTEVE VALENTI" บ
'บ 	        2 = text all lcase                  	"esteve valentก" บ
'บ      	3 = firts characters in words ucase 	"Esteve Valentก" บ
'บ           	4 = firts characters in words lcase 	"eSTEVE vALENTI" บ
'บ              10= numeric, right adjust               "     1234521"   บ
'บ              11=    "   , left adjust                "1234521     "   บ
'บ              12=    "   , with ceros, right adjust   "000001234521"   บ
'บ              13=    "   , with ceros, left adjust    "123452100000"   บ
'บ              14=    "   , with comma, right adjust   "   1,234,521"   บ
'บ              20= numeric, sign, right adjust         "-    1234521"   บ
'บ              21=    "   , sign, left adjust          "+1234521    "   บ
'บ              22=    "   , sign, ceros, right adjust  "-00001234521"   บ
'บ              23=    "   , sign, ceros, left adjust   "-12345210000"   บ
'บ              24=    "   , sign, right adjust         "-  1,234,521"   บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ    einput$        ===> character string with input string.            บ
'บ    e%             ===> returned error code                            บ
'บ    eret%          ===> same of e%                                     บ
'บ                              if RETURN                 e%= 0          บ
'บ                              if ESC                    e%= 1          บ
'บ                              if UP                  	  e%= 2          บ
'บ                              if RIGHT at last                         บ
'บ                                 character and EReturn% e%= 3          บ
'บ                              if TAB RIGHT  ---->|      e%= 3          บ
'บ                              if DOWN                   e%= 4          บ
'บ                              if LEFT at first                         บ
'บ                                 character and EReturn% e%= 5          บ
'บ                              if TAB LEFT   |<----      e%= 5          บ
'บ                                                                       บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ   NOTES:                                                              บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ -The EInput Function assume the row (CSRLIN) and the column (POS())   บ
'บ  actives in the screen with the LOCATE y,x statement.                 บ
'บ                                                                       บ
'บ -EInput$ is susceptible by:                                           บ
'บ                                                                       บ
'บ       SET.EInputColor( fgc%, bgc%)        subrroutine                 บ
'บ       SET.EOutPutColor(fgc%, bgc%)        subrroutine                 บ
'บ       SET.EInsertON    SET.EInsertOFF     subrroutines                บ
'บ       SET.EReturnON    SET.EReturnOFF     subrroutines                บ
'บ       SET.EKBDBeepON   SET.EKBDBeepOFF    subrroutines                บ
'บ       SET.EKBDClickON  SET.EKBDClickOFF   subrroutines                บ
'บ       SET.EInputFilter$( filter$ )        subrroutine                 บ
'บ                                                                       บ
'บ -The ERet% return code will be util to make this after function.      บ
'บ                                                                       บ
'บ             ON ERet% GOTO label1, label2, label3, label4, label5      บ
'บ                          label1 = label to go if ESC                  บ
'บ                          label2 =     "    previus EInput$            บ
'บ                          lebel3 =     "    secuent    "               บ
'บ                          label4 =     "    upper      "               บ
'บ                          lebel5 =     "    lower      "               บ
'บ  or this:                                                             บ
'บ             SELECT CASE e%                                            บ
'บ                                                                       บ
'บ                CASE 1        ' ESC                                    บ
'บ                                                                       บ
'บ                CASE 2        ' UP                                     บ
'บ                                                                       บ
'บ                CASE 3        ' RIGHT or TAB RIGHT                     บ
'บ                                                                       บ
'บ                CASE 4        ' DOWN                                   บ
'บ                                                                       บ
'บ                CASE 5        ' LEFT or TAB LEFT                       บ
'บ                                                                       บ
'บ             END SELECT                                                บ
'บ  or ....                                                              บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
function einput$( lend%, type%, e% )				public
    local t$			' to get te key pressed
    local num##, num$ 		' to number calculation
    local musing$			' to use with PRINT
    local w%,x%,f%,l%,d$,n%,m% 	' internal use
    local posy%, posx%, my%, mx%	' to hold old cursor location
    local iei%			' internall EInsert
    local ifgc%, ibgc%		' internall input colors
    local ofgc%, obgc% 		' internall output colors
    shared eret%			' control ESC,PAGUP,PAGDW
    shared einputfgc%		' input fore ground color
    shared einputbgc%		' input back ground color
    shared eoutputfgc%		' output fore ground color
    shared eoutputbgc%		' output back ground color
    shared einsert%		' logical variable for automaticall insertion
    shared ereturn%		' logical variable for automaticall return
    shared ekbdbeep%		' logical variable for beep if error
    shared ekbdclick%		' logical variable for click when press key
    shared ekbdfilter$		' filter to keyboad
    shared estarttolastchr%        ' inte.use: Only true if EReturn% and UP

     eret%= 0				' public return code = 0
     e%   = 0				' param. return code = 0
     posy% = csrlin			' line, row to start EInput
     posx% = pos(0)			' column to start EInput
     ifgc% = einputfgc%			' colors
     ibgc% = einputbgc%
     ofgc% = eoutputfgc%
     obgc% = eoutputbgc%
     if ifgc% = ibgc% or_		' default colors if error
	ifgc% < 0 or ifgc% > 16 or_
	ibgc% < 0 or ibgc% > 8 then
	ifgc% = 0
	ibgc% = 7
     end if

     if ofgc% = obgc% or_		' default colors if error
	ofgc% < 0 or ofgc% > 16 or_
	obgc% < 0 or obgc% > 8 then
	ofgc% = 7
	obgc% = 0
     end if

     color ifgc%, ibgc%			' set colors

     if einsert% then			' set cursor
	iei% = -1
	locate posy%, posx%, 1, 0, 16
     else
	iei% = 0
	locate posy%, posx%, 1, 13, 16
     end if

'*******************************************************************
'* Print the input values in screen with the input format MUSING$  *
'* to easy the work at the user.                                   *
'*******************************************************************
select case type%
 case 0 to 4					' ALPHANUMERIC
   print scr.get.text$(posy%, posx%, lend%);
 case 10 to 14				' NUMERIC without sign
   if lend% < 1  then lend% = 1                	' minium value of lend
   if lend% > 18 then lend% = 18               	' maxium value of lend
   num$  = scr.get.text$(posy%,posx%,lend%)        	' get num of screen
   num$  = elegalnum$(num$)
   num## = val(num$)				' make num##
   f% = len(str$(fix(num##))) - 1		' prepare musing$
   musing$ = string$(f%, "#")		 	'
   print space$(lend%);                       	' print black space
   locate posy%, posx%                        	' go to start input
   print using musing$; num##;               	' print num at left

   if type% = 14 then                         	' if commas
      m% = lend% \ 4		                '  number of commas
      lend% = lend% - m%			'  reduce lend%
   end if
 case 20 to 24					' NUMERICS with sign
   if lend% < 2  then lend% = 2		 	'minium value of lend
   if lend% > 19 then lend% = 19		'maxium value of lend
   num$  = scr.get.text$(posy%,posx%,lend%)        	' get num of screen
   num$  = elegalnum$(num$)
   num## = val(num$)				' make num##
   f% = len(str$(fix(num##))) - 1
   musing$ = "+" + string$(f%, "#")
   print space$(lend%);
   locate posy%, posx%
   print using musing$; num##;

   if type% = 24 then                         	' if commas
      m% = (lend% - 1) \ 4	                '  number of commas
      lend% = lend% - m%			'  reduce lend%
   end if
 case else
   print "Error in EInput$ passed parameters. Program break";:end
end select
'******************************************************************



'******************************************************************
'* Bucle to filter the input errors                               *
'******************************************************************
do
   e% = 0                         		' error false
   color ifgc%, ibgc%				' set input colors

   if ereturn% and estarttolastchr% then	' set cursor again
      estarttolastchr% = 0
      d$ = scr.get.text$(posy%, posx%, lend%)
      mx% = len(rtrim$(d$))
      locate posy%, posx% + mx%
   else
      locate posy%, posx%
   end if


   if iei% then					' set cursor again
      locate , , 1, 0, 16
   else
      locate , , 1, 13, 16
   end if


   '*******************************************************************
   '* Trapp keys and print his characters to the screen if his has    *
   '* correct with the type% defined.                                 *
   '*******************************************************************
   do
      t$ = inkey$				' t$ = key pressed

      select case t$
       case chr$(&h8)				' DELETE TO LEFT

         if pos(0) > posx% then
	    f% = (((csrlin - 1) * 2 * scr.columns%) + (pos(0) - 2) * 2)
	    l% = (((csrlin - 1) * 2 * scr.columns%) + (posx% + lend% - 3) * 2)

            for x% = f% to l% step 2
               poke x%, peek(x% + 2)
            next x%

            poke x%, 32
            locate csrlin, pos(0) - 1
         else

            if ereturn% then
               eret%= 5: estarttolastchr% = -1
               exit loop
            end if

         end if

       case chr$(&h00,&h53)			' DELETE TO RIGHT

         if pos(0) < posx% + lend% then
	    f% = (((csrlin - 1) * scr.columns% * 2) + (pos(0) - 1) * 2)
	    l% = (((csrlin - 1) * scr.columns% * 2) + (posx% + lend% - 3) * 2)

            for x% = f% to l% step 2
               poke x%, peek(x% + 2)
            next x%

            poke x%, 32
         end if

       case chr$(&h00,&h52)			' INSERT ON/OFF

         if iei% then
            iei% = 0
            locate , , , 13, 16
         else
            iei% = -1
            locate , , , 0, 16
         end if

       case chr$(&h00,&h75)			' CTRL + END

         if posx% + lend% > pos(0) then
            l% = posx% + lend% - pos(0)
            my% = csrlin : mx% = pos(0)
            print space$(l%);
            locate my%, mx%
         end if

       case chr$(&h00,&h77)			' CTRL + HOME

         if pos(0) > posx% then
            l% = pos(0) - posx%
            locate posy%, posx%
            print space$(l%);
         end if

       case chr$(&h00,&h4b)			' LEFT

         if pos(0) > posx% then
            locate csrlin, pos(0) - 1
         else

            if ereturn% then
               eret%= 5: estarttolastchr% = -1
               exit loop
            else
               if ekbdbeep% then play "L64 N84"
            end if

         end if

       case chr$(&h00,&h4d)			' RIGHT

         if pos(0) < posx% + lend% then
            locate csrlin, pos(0) + 1
         else

            if ereturn% then
               eret%= 3
               exit loop
            else
               if ekbdbeep% then play "L64 N84"
            end if

         end if

       case chr$(&h00,&h4f)			' END | FIN

         if posx% + lend% - 1 >= pos(0) then
            l% = posx% + lend% - pos(0)
	    d$ = scr.get.text$(posy%, pos(0) + 1, l% - 1) + " "
            w% = instr(d$, " ")
            locate csrlin, pos(0) + w%
         else

            if ereturn% then
               eret%= 3
               exit loop
            else
               if ekbdbeep% then play "L64 N84"
            end if

         end if

       case chr$(&h00,&h47)			' HOME

         if pos(0) > posx% then
            locate posy%, posx%
         else

            if ereturn% then
               eret%= 5: estarttolastchr% = -1
               exit loop
            else
               if ekbdbeep% then play "L64 N84"
            end if

         end if

       case chr$(&h1b)           		' ESC           (ERet%=1)
         eret%= 1 : e% = 1 : ekbdfilter$ = "" : exit function
       case chr$(&h00,&h48) 	  		' UP            (ERet%=2)
         eret%= 2 : exit loop
       case chr$(&h00,&h0f)	   		' TAB <         (ERet%=2)
         eret%= 2 : exit loop
       case chr$(&h00,&h50)	  		' DOWN          (ERet%=3)
         eret%= 4 : exit loop
       case chr$(&h09)   			' TAB >         (ERet%=3)
         eret%= 4 : exit loop
       case " " to chr$(&hff)			' PRINTING CHARACTERS
	 if ekbdfilter$="" or_          	' if not filter to keyboard
         (ekbdfilter$<>"" and _			' or (filter to keyboard and
         instr(ekbdfilter$,t$)<>0) then		' key pressed is in filter
         if ekbdclick% then play "L64 N0"		' Click?

         select case type%
          case 0                                     '  ucase and lcase
            p$ = t$
          case 1                                     '  only ucase
            p$ = ucase$(t$)
          case 2                                     '  only lcase
            p$ = lcase$(t$)
          case 3                                     '  firsts ucase
	    d$ = scr.get.text$(csrlin, pos(0) - 1, 1)

            if d$ = " " or pos(0) = posx% then
               p$ = ucase$(t$)
            else
               p$ = lcase$(t$)
            end if

          case 4                                     '  firsts lcase
	    d$ = scr.get.text$(csrlin, pos(0) - 1, 1)

            if d$ = " " or pos(0) = posx% then
               p$ = lcase$(t$)
            else
               p$ = ucase$(t$)
            end if

          case 10 to 14                              ' NUMBERS without sign
            p$ = chr$(&h7)

            if instr("0123456789", t$) <> 0 then
               p$ = t$
            end if

          case 20 to 24                              ' NUMBERS with sign
            p$ = chr$(&h7)
            if instr("0123456789", t$) <> 0 and_
	       pos(0) <> posx% then
	       p$ = t$
	    else
	       if (t$ = "-" or t$ = "+") and_
		  posx% = pos(0) and_
		  not iei% then
		  p$ = t$
	       end if

	    end if

	  end select

       else                			' filter to keyboard not passed
	  p$ = chr$(&h07)  			' beep?
       end if

      case else                                    ' if others BEEP
	if t$ <> "" and t$ <> chr$(&h0d)_
	   and ekbdbeep% then play "L64 N84"
     end select



     '****************************************************************
     '* Print the string hold in p$ and control EInsert% and EReturn%*
     '****************************************************************
     if p$ <> "" then                              ' control EInsert

	if iei% and p$ <> "" and p$ <> chr$(&h7) then

	   if pos(0) < posx% + lend% then
	      f% = (((csrlin - 1) * scr.columns% * 2) + (posx% + lend% - 2) * 2)
	      l% = (((csrlin - 1) * scr.columns% * 2) + (pos(0) * 2))

	      for x% = f% to l% step -2
		  poke x%, peek(x% - 2)
	      next x%

	      poke x%, 32
	   end if

	end if


	if pos(0) < posx% + lend% then             ' Print string

	   if p$ = chr$(&h07) then
	      if ekbdbeep% then play "L64 N84"
	   else
	      print p$;
	   end if

	else
	   if ekbdbeep% then play "L64 N84"
	end if

	p$ = ""

	if ereturn% and pos(0) = posx% + lend% then' Control EReturn
	   exit loop
	end if

     end if
     '****************************************************************



   loop until t$ = chr$(&h0d)
   '*******************************************************************



   n% = 0					' prepare exit
   musing$ = ""					' variables
   color ofgc%, obgc%				' set output color
   locate posy%, posx%, 0			' set output cursor




   '*******************************************************************
   '* Print the input values with the exit format.                    *
   '*******************************************************************
   select case type%
    case 0 to 4					' ALPHANUMERIC
       print scr.get.text$(posy%, posx%, lend%);
    case 10 to 14				' NUMERIC without sign
       num$  = scr.get.text$(posy%, posx%, lend%)
       num$  = elegalnum$( num$ )
       num## = val( num$ )

       select case type%
	case 10
	   print right$("                  "+num$, lend% );
	case 11
	   print left$ (num$+"                  ", lend% );
	case 12
	   print right$("000000000000000000"+num$,lend% );
	case 13
	   print left$ (num$+"000000000000000000", lend% );
	case 14
	   musing$ = repeat$(m%,",###")
	   lend% = lend% + m%
	   musing$ = right$("###"+musing$,lend%)
	   print using musing$; num##;
       end select

    case 20 to 24				' NUMERIC with sign
      num$  = scr.get.text$( posy%, posx%, lend% )
      num$  = elegalnum$(num$)
      num## = val(num$)

      if num## >= 0 then
	 print "+";
      else
	 print "-";
      end if

      if num$<>"0" then num$ = right$(num$,len(num$)-1)

      select case type%
       case 20
	  print right$("                  "+num$, lend%-1 );
       case 21
	  print left$ (num$+"                  ", lend%-1 );
       case 22
	  print right$("000000000000000000"+num$, lend%-1 );
       case 23
	  print left$ (num$+"000000000000000000", lend%-1 );
       case 24
	  musing$ = repeat$(m%,",###")
	  lend% = lend% + m%
	  musing$ = right$("###"+musing$,lend%-1)
	  print using musing$; val(num$);
      end select

    end select
    '*****************************************************************


loop until not e%			' not error, so exit
'*********************************************************************

e% = eret%				' set return code in two variables
ekbdfilter$ = ""			' clear keyboard filter
einput$ = scr.get.text$(posy%, posx%, lend%)' exit string characters
color ofgc%, obgc%			' set output  colors

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END FUNCTION:PBEINMAT.INC                                12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end function




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ FUNCTION :einputa$                                       12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ    einputa$       ===>                                                บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
function einputa$(posy%, posx%, lend%, ifgc%, ibgc%, ofgc%, obgc%,_
     type%, ereturn%, einset%, e%)			public

      call set.einputcolor(ifgc%,ibgc%)
      call set.eoutputcolor(ofgc%,obgc%)
     locate posy%, posx%

     if ereturn% then
	call set.ereturnon
     else
	call set.ereturnoff
     end if

     if einsert% then
	call set.einserton
     else
	call set.einsertoff
     end if

     einputa$ = einput$( lend%, type%, e% )

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END FUNCTION:einputa$                                    12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end function




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :set.einputcolor                                     12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    fgc%           ===> fore ground color                              บ
'บ    bgc%           ===> cack ground color                              บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub set.einputcolor( fgc%, bgc% ) 			public
   shared einputfgc%
   shared einputbgc%
    einputfgc% = fgc%
    einputbgc% = bgc%

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:set.einputcolor                                  12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :set.eoutputcolor                                    12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    fgc%           ===> fore ground color                              บ
'บ    bgc%           ===> back ground color                              บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub set.eoutputcolor( fgc%, bgc% ) 			public
   shared eoutputfgc%
   shared eoutputbgc%
    eoutputfgc% = fgc%
    eoutputbgc% = bgc%

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:set.eoutputcolor                                 12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :set.ekbdfilter                                      12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ    Hold the value in memory. These value are loked after key are      บ
'บ    pressed, in EInput$ function, and if the key isn't into the value  บ
'บ    don't is printed into the screen, and if SET.EKBDbeepON is called, บ
'บ    beep song to irritate your ears.  :-)                              บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    filter$        ===> character string with the only keyboard        บ
'บ                        characters accepteds                           บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub set.ekbdfilter( filter$ )				public
   shared ekbdfilter$
    ekbdfilter$ = filter$

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:set.ekbdfilter                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :set.ekbdbeepon                                      12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ    Turn ON the beep wen error in EInput$                              บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub set.ekbdbeepon  					public
   shared ekbdbeep%
    ekbdbeep% = -1

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:set.ekbdbeepon                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :set.ekbdbeepoff                                     12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ    Turn OFF the beep wen error in EInput$                             บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub set.ekbdbeepoff 					public
   shared ekbdbeep%
    ekbdbeep% = 0

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:set.ekbdbeepoff                                  12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :set.ekbdclickon                                     12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ    Turno ON the click wen key are pressed                             บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub set.ekbdclickon  					public
   shared ekbdclick%
    ekbdclick% = -1

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:set.ekbdclickon                                  12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :set.ekbdclickoff			            12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ    Turno OFF the click wen key are pressed                            บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub set.ekbdclickoff					public
   shared ekbdclick%
    ekbdclick% = 0

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:set.ekbdclickoff		                    12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :set.einserton                                       12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ     Turno ON the insertion in EInput$                                 บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub set.einserton  					public
   shared einsert%
    einsert% = -1

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:set.einserton                                    12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :set.einsertoff                                      12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ    Turno OFF the insertion in EInput$                                 บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub set.einsertoff 					public
   shared einsert%
    einsert% = 0

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:set.einsertoff                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :set.ereturnon                                       12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ    Turno ON the automaticall return in EInput$                        บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub set.ereturnon  					public
   shared ereturn%
    ereturn% = -1

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:set.ereturnon                                    12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :set.ereturnoff                                      12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ    Turn OFF the automaticall return in EInput$                        บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub set.ereturnoff  					public
   shared ereturn%
    ereturn% = 0

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:set.ereturnoff                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :set.estarttolastchr				    12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ     At previus on secuend EInput the cursor go to last character      บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub set.estarttolastchr					public
   shared estarttolastchr%
    estarttolastchr% = -1

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:set.estarttolastchr		                    12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'----------------------------
'---   INTERNAL USE
'----------------------------

'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ FUNCTION :elegalnum$                                     12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ    Prepare the character string to use with VAL() with errors.        บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    num$           ===> string character with number                   บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ    elegalnum$     ===> string character                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
function elegalnum$( num$ )				public
    local c%,l$,r$
     num$ = ltrim$(rtrim$(num$))
     replace any "," with " " in num$

     if num$ <> "" then

        while instr(num$," ") <> 0		 ' prepare num
           c% = instr(num$," ")
           l$ = left$(num$,c%-1)
           r$ = right$(num$, len(num$)-c%)
           num$ = l$ + r$
        wend

        elegalnum$ = num$
     else
        elegalnum$ = "0"
     end if


'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END FUNCTION:elegalnum$                                  12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end function

