'NOGOODFONT
'
' ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออปบ
'บ Library......: PBEINMAS.INC                                           บบ
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
'บ FUNCTION :einput.text$                                   12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    lend%          ===> len of dat to input                            บ
'บ    type%          ===> type of dat to input                           บ
'บ              0 = text   ucase or lcase               "EsTEve vALenTก" บ
'บ              1 = text all ucase                      "ESTEVE VALENTI" บ
'บ              2 = text all lcase                      "esteve valentก" บ
'บ              3 = firts characters in words ucase     "Esteve Valentก" บ
'บ              4 = firts characters in words lcase     "eSTEVE vALENTI" บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ    einput.text$   ===> string character with input dat                บ
'บ    e%             ===> return code. Same EInput$                      บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
function einput.text$( lend%, type%, e% )  public
    shared filterinpreviusfunction%
    shared esmallest$, egreatest$, eonly$, eexcept$
    local  posy%, posx%
    local  mask$, x$

     if type% < 0 or type% > 4 then
        print "Invalid text type. Program break! ":end
     end if

     mask$ = "_"+ltrim$(rtrim$(str$(type%)))+"_"+space$(lend%)
     filterinpreviusfunction% = -1
     posy% = csrlin
     posx% = pos(0)

     do
        filtererror% = 0
        locate posy%, posx%
        x$ = einput.mask$( mask$, e% )

        if e%=1 or e%=2 then
           esmallest$ = "" : egreatest$ = ""
           eonly$     = "" : eexcept$   = ""
           exit function
        end if

        filtererror% = efilter%(x$,0,0)		' test text error
     loop while filtererror%                    ' loop if error

     einput.text$ = x$

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END FUNCTION:einput.text$                                12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end function




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ FUNCTION :einput.num$                                    12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    type%          ===> type of number to input                        บ
'บ                                                                       บ
'บ             INTEGERS                 DECIMALS          OUTPUT FORMAT  บ
'บ         0 = right adjust             left adjust       "    1532.12 " บ
'บ         1 = right adjust             left adjust,ceros "    1532.120" บ
'บ         2 = right adjust,ceros       left adjust       "00001532.12 " บ
'บ         3 = right adjust,ceros       left adjust,ceros "00001532.120" บ
'บ         4 = right adjust,commas      left adjust       "   1,532.12 " บ
'บ         5 = right adjust,commas      left adjust,ceros "   1,532.120" บ
'บ                                                                       บ
'บ        10 = sign,right adjust        left adjust       "+   1532.12 " บ
'บ        11 = sign,right adjust        left adjust,ceros "+   1532.120" บ
'บ        12 = sign,ceros,right adjust  left adjust       "+0001532.12 " บ
'บ        13 = sign,ceros,right adjust  left adjust,ceros "+0001532.120" บ
'บ        14 = sign,commas,right adjust left adjust       "+  1,532.12 " บ
'บ        15 = sign,commas,right adjust left adjust,ceros "+  1,532.120" บ
'บ                                                                       บ
'บ    nint%          ===> number of intergers to input (0 to 15)         บ
'บ    ndec%          ===> number of decimals to input  (0 to 15)         บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ    einput.num$    ===> character string                               บ
'บ    e%             ===> return code. Same EInput$                      บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
function einput.num$( type%, nint%, ndec%, e% )  public
    shared filterinpreviusfunction%
    shared esmallest$, egreatest$, eonly$, eexcept$
    local  posy%, posx%
    local  nint$, ndec$
    local  lint$, ldec$
    local  mask$, x$
     if type% >=10 and type% <= 15 and nint% > 0 then nint% = nint% + 1
     if nint% > 15 or  nint% < 0 or_		' test valid lengt
        ndec% > 15 or  ndec% < 0 or_
        (nint% = 0 and ndec% = 0) then
     end if

     if type%=4  or type%=5 or_                    ' if commas
        type%=14 or type%=15 then
        nint% = nint% + ( nint% \ 4 )
     end if


     select case type%                               ' select type
      case 0:  nint$ = "_10_": ndec$ = "_11_"
      case 1:  nint$ = "_10_": ndec$ = "_13_"
      case 2:  nint$ = "_12_": ndec$ = "_11_"
      case 3:  nint$ = "_12_": ndec$ = "_13_"
      case 4:  nint$ = "_14_": ndec$ = "_11_"
      case 5:  nint$ = "_14_": ndec$ = "_13_"
      case 10: nint$ = "_20_": ndec$ = "_11_"
      case 11: nint$ = "_20_": ndec$ = "_13_"
      case 12: nint$ = "_22_": ndec$ = "_11_"
      case 13: nint$ = "_22_": ndec$ = "_13_"
      case 14: nint$ = "_24_": ndec$ = "_11_"
      case 15: nint$ = "_24_": ndec$ = "_13_"
      case else                                      '  test valid type
       print "Invalid number type. Program break! ":end
     end select

     if nint% <> 0 then lint$ = nint$ + space$(nint%)
     if ndec% <> 0 then ldec$ = "." + ndec$ + space$(ndec%)
     mask$ = lint$ + ldec$                           ' make mask$
     filterinpreviusfunction% = -1
     posy% = csrlin
     posx% = pos(0)

     do
        filtererror% = 0
        locate posy%, posx%
        x$ = einput.mask$( mask$, e% )

        if e%=1 or e%=2 then
           esmallest$ = "" : egreatest$ = ""
           eonly$     = "" : eexcept$   = ""
           exit function
        end if

        filtererror% = efilter%(x$,1,0)              ' test error
     loop while filtererror%                         ' loop if error

     einput.num$ = x$

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END FUNCTION:einput.num$                                 12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end function




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ FUNCTION :einput.date$                                   12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    type%          ===> type of date to input                          บ
'บ                                                                       บ
'บ              0 = usa, 4 digits year, with /          "12/26/1967"     บ
'บ              1 = usa, 4 digits year                  "12261967"       บ
'บ              5 = usa, 2 digits year, with /          "12/26/67"       บ
'บ              6 = usa, 2 digits year                  "122667"         บ
'บ              10= europa, 4 digits year, with /       "26/12/1967"     บ
'บ              11= europa, 4 digits year               "26121967"       บ
'บ              15= europa, 2 digits year, with /       "26/12/67"       บ
'บ              16= europa, 2 digits year               "261267"         บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ    einput.date$   ===>                                                บ
'บ    e%             ===> return code. Same EInput$                      บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
function einput.date$( type%, e% )			public
    local mask$, dateformat$, x$
    local valideinput%
    local posy%, posx%
    local m%, d%, y%
    shared esmallest$, egreatest$, eonly$, eexcept$
    shared edatetext$
    shared filterinpreviusfunction%
     if type% > 9 then typedate% = 1

     select case type%
      case 0
        mask$ = "_12_  /_12_  /_12_    "
        dateformat$ = "MM/DD/YYYY"
      case 1
        mask$ = "_12_  _12_  _12_    "
        dateformat$ = "MMDDYYYY"
      case 5
        mask$ = "_12_  /_12_  /_12_  "
        dateformat$ = "MM/DD/YY"
      case 6
        mask$ = "_12_  _12_  _12_  "
        dateformat$ = "MMDDYY"
      case 10
        mask$ = "_12_  /_12_  /_12_    "
        dateformat$ = "DD/MM/YYYY"
      case 11
        mask$ = "_12_  _12_  _12_    "
        dateformat$ = "DDMMYYYY"
      case 15
        mask$ = "_12_  /_12_  /_12_  "
        dateformat$ = "DD/MM/YY"
      case 16
        mask$ = "_12_  _12_  _12_  "
        dateformat$ = "DDMMYY"
      case else
        print "Invalid date type. Program Break!":end
     end select

     filterinpreviusfunction% = -1
     posy% = csrlin
     posx% = pos(0)

     do
        filtererror% = 0

        do
           locate posy%, posx%
           x$ = einput.mask$( mask$, e% )

           if e%=1 or e%=2 then
              esmallest$ = "" : egreatest$ = ""
              eonly$     = "" : eexcept$   = ""
              exit function
           end if

            call splitdate(x$, typedate%, m%, d%, y%)
           valideinput% = testdate%(m%, d%, y%)

           if not valideinput% then
               call edisplayerror(edatetext$+dateformat$)
           end if

        loop until valideinput%

        filtererror% = efilter%(x$,2,typedate%)	' test error
     loop while filtererror%			
' loop if error
     einput.date$ = x$

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END FUNCTION:einput.date$                                12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end function




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ FUNCTION :einput.mask$                                   12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    mask$          ===> character string with EInput mask control.     บ
'บ                                                                       บ
'บ           "?"   = no blank space is printed in the screen between     บ
'บ                   the EInput's.                                       บ
'บ           " "   = spaces for the previus einput instruction (_x_)     บ
'บ                                                                       บ
'บ           "_0_" = text   ucase or lcase              "EsTEve vALenTก" บ
'บ           "_1_" = text all ucase                     "ESTEVE VALENTI" บ
'บ           "_2_" = text all lcase                     "esteve valentก" บ
'บ           "_3_" = firts characters in words ucase    "Esteve Valentก" บ
'บ           "_4_" = firts characters in words lcase    "eSTEVE vALENTI" บ
'บ           "_10_"= numeric, right adjust               "     1234521"  บ
'บ           "_11_"=    "   , left adjust                "1234521     "  บ
'บ           "_12_"=    "   , with ceros, right adjust   "000001234521"  บ
'บ           "_13_"=    "   , with ceros, left adjust    "123452100000"  บ
'บ           "_14_"=    "   , with comma, right adjust   "   1,234,521"  บ
'บ           "_20_"= numeric, sign, right adjust         "-    1234521"  บ
'บ           "_21_"=    "   , sign, left adjust          "+1234521    "  บ
'บ           "_22_"=    "   , sign, ceros, right adjust  "-00001234521"  บ
'บ           "_23_"=    "   , sign, ceros, left adjust   "-12345210000"  บ
'บ           "_24_"=    "   , sign, right adjust         "-  1,234,521"  บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ    einput.mask$   ===> character string                               บ
'บ    e%             ===> return code. Same EInput$                      บ
'บ                                                                       บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ   NOTES:                                                              บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ                                                                       บ
'บ  The EInput.MASK$ function can be util to input same diferents types  บ
'บ  of formated dats supporteds for EInput$ in only one instruccion.     บ
'บ                                                                       บ
'บ   EXAMPLE:                                                            บ
'บ                                                                       บ
'บ       mask$ = "_2_ -_14_          /_2_  "                             บ
'บ                 | || |  |         | |  |                              บ
'บ                 | || |  |         | |  two spaces for EInput$         บ
'บ                 | || |  |         | EInput type 2 (text lower case)   บ
'บ                 | || |  |         character to print in screen        บ
'บ                 | || |  ten spaces for EInput$                        บ
'บ                 | || EInput type 14                                   บ
'บ                 | |character to print in screen                       บ
'บ                 | one space for EInput$                               บ
'บ                 EInput type 2                                         บ
'บ                                                                       บ
'บ       nif$    = EInput.MASK$( mask$ )                                 บ
'บ       phone$  = einput.mask$( "(_12_   )_12_   ._12_  ._12_  "        บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
function einput.mask$( mask$, e% )         			public
    shared ekbdbeep%		' beep if key pressed don't answer
    shared ereturn%		' Automaticall return?
    shared estarttolasteinput%	' Go to last EInput in mask$.
    shared edefault$		' printed in screen if screen = ""
    shared ewmenu$		' values displayed in a pull-down window
    shared eoutputfgc%
    shared eoutputbgc%
    shared filterinpreviusfunction%' logical variable for enable and
                                   ' disable test of filters.
    local  oldereturn%		' to hold EReturn%
    local  mask%			' pointer to actual EInput
    local  mask%()		' array to split mask$ parameters

      dim mask%(1:50,1:4)       ' to hold splited parameters of mask$
      
    local  type$, mmask$, c$	' to split mask$
    local  onlend%, ontype%	' to split mask$
    local  posy%, posx%		' position in screen
    local  lend%			' lengt total of dat
    local  x$, p% 		' internal use
    local  filtererror%		' internal use
     posy% = csrlin
     posx% = pos(0)
     ofgc% = eoutputfgc%
     obgc% = eoutputbgc%
     if ofgc% = obgc% or_			' default colors if error
        ofgc% < 0 or ofgc% > 16 or_
        obgc% < 0 or obgc% > 8 then
        ofgc% = 7
        obgc% = 0
     end if

     color ofgc%, obgc%                    ' set colors
     oldereturn% = ereturn%
      call set.ereturnon

     for p% = 1 to len(mask$)
         c$ = mid$(mask$, p%, 1)

        select case c$
         case "_"

           if ontype% then
              ontype% = 0
              onlend% = -1
              incr mask%, 1
              mask%(mask%,1)=val(type$)
              mask%(mask%,3)=posy%
              mask%(mask%,4)=posx%+len(mmask$)
              type$ = ""
           else
              ontype% = -1
       
              if onlend% then
                 onlend% = 0
                 mask%(mask%,2) = lend%
                 lend% = 0
              end if

           end if

         case " "
    
           if onlend% then
              incr lend%, 1

              if p% = len(mask$) then
                 mask%(mask%,2) = lend%
              end if

           end if

           if not ontype% then mmask$ = mmask$ + c$
         case else

           if ontype% then
              type$ = type$ + c$
           else
              type$ = ""

              if onlend% then
                 onlend% = 0
                 mask%(mask%,2) = lend%
                 lend% = 0
              end if

              mmask$ = mmask$ + c$
           end if

        end select

     next p%

     lend% = len(mmask$)                           ' lengt total of EInput

     if mask% > 0 then                             ' mask OK!, so
        x$ = scr.text$(posy%,posx%,lend%)          ' screen text in x$

        if rtrim$(ltrim$(x$)) = "" then            ' not exist text

           if edefault$ <> "" then                 '  exist default text
              print edefault$;                     '   print default text
           else                                    '  not
              print mmask$;                        '   print mask
           end if

        else                                       ' exist text

           if x$ = mmask$ and edefault$ <> "" then '  text = mask$
              print edefault$;                     '   print default text
           end if

        end if

        edefault$ = ""                             ' initialize EDefault$

        do
           filtererror% = 0

           if estarttolasteinput% then             ' if go to last EInput
              estarttolasteinput% = 0              '  ok, restart flag
              p% = mask%                           '  pointer at mask%() to last
           else                                    ' else
              p% = 1                                       '  pointer to first
           end if


           do                                      ' bucle to control e%
              locate mask%(p%,3), mask%(p%,4)
              x$ = einput$( mask%(p%,2), mask%(p%,1), e% )

              select case e%
               case 0                                      ' RETURN

                 if p% = mask% then
                    exit loop
                 else
                    incr p%
                 end if
         
               case 1                                      ' ESC
                 filterinpreviusfunction% = 0
                 exit function
               case 2                                      ' UP
         
                 if p% > 1 then
                    decr p%,1
                 else
                    exit loop
                 end if

               case 3                                      ' RIGHT

                 if p% < mask% then
                    incr p%,1
                 else
     
                    if oldereturn% then
                       exit loop
                    else
                       if ekbdbeep% then play "L64 N70"
                    end if
    
                 end if

               case 4                                      ' DOWN

                 if p% < mask% then
                    incr p%,1
                 else
                    exit loop
                 end if
           
               case 5                                      ' LEFT

                 if p% > 1 then
                    decr p%
                 else

                    if oldereturn% then
                       estarttolasteinput% = -1
                       exit loop
                    else
                       if ekbdbeep% then play "L64 N70"
                    end if
           
                 end if

              end select

           loop while -1


           if not filterinpreviusfunction% then
              x$ = scr.text$(posy%,posx%,lend%)    ' dat of screen
              filtererror% = efilter%(x$,0,0)      ' test error
           end if

        loop while filtererror%                    ' loop if error
     else
        print "Ilegall Mask$. Program Break!";:end ' ilegal mask$. END
     end if

     einput.mask$ = scr.text$(posy%,posx%,lend%)   ' text to return
     if not oldereturn% then call set.ereturnoff   ' restore old EReturn%

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END FUNCTION:einput.mask$                                12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end function





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ FUNCTION :etime#                                         12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    mtime$         ===>                                                บ
'บ    type%          ===>                                                บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ    etime#         ===>                                                บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
function etime#( mtime$, type% ) public

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END FUNCTION:etime#                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end function




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :set.estarttolasteinput                              12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ    At secuent EInput.MASK$ the fisrt EInput is the makad last in mask$บ
'บ    Same SET.EStartToLastCHR in EInput packet.                         บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub set.estarttolasteinput					public
   shared estarttolasteinput%
    estarttolasteinput% = -1

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:set.estarttolasteinput                           12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :set.edefault                                        12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ    Hold defauld value in memory, and wen you use EInput.xxx functions บ
'บ    this value are printed in the screen if the screen space reserved  บ
'บ    to print the input text are blank.                                 บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    text$          ===> character string with the default text to printบ
'บ                        in screen after input with EInput. functions   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub set.edefault( text$ ) 				public
   shared edefault$
    edefault$ = text$

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:set.edefault                                     12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :set.esmallest                                       12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ    Hold value in memory to compare this, befor exit of the EInput.xxx บ
'บ    functions, with the value returned for these.                      บ
'บ    If the format of value to hold and the returned for EInput.xxx     บ
'บ    functions isn't equal, don't run OK.                               บ
'บ    If you use with a date, you must pass in text string a character   บ
'บ    string with a same date format. Equal for numbers and others.      บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    text$          ===> charecter string with the small value accepted บ
'บ                        in EInput.xxx functions.                       บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub set.esmallest( text$ ) 				public
   shared esmallest$
    esmallest$ = text$

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:set.esmallest                                    12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :set.egreatest                                       12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ    Hold value in memory to compare this, befor exit of the EInput.xxx บ
'บ    functions, with the value returned for these.                      บ
'บ    If the format of value to hold and the returned for EInput.xxx     บ
'บ    functions isn't equal, don't run OK.                               บ
'บ    If you use with a date, you must pass in text string a character   บ
'บ    string with a same date format. Equal for numbers and others.      บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    text$          ===> charecter string with the great value accepted บ
'บ                        in EInput.xxx functions.                       บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub set.egreatest( text$ ) 				public
   shared egreatest$
    egreatest$ = text$

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:set.egreatest                                    12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :set.eonly                                           12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ    Hold value in memory to compare this, befor exit of the EInput.xxx บ
'บ    functions, with the value returned for these.                      บ
'บ    If the format of value to hold and the returned for EInput.xxx     บ
'บ    functions isn't equal, don't run OK.                               บ
'บ    If you use with a date, you must pass in text string a character   บ
'บ    string with a same date format. Equal for numbers and others.      บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    text$          ===> charecter string with the only values acceptedsบ
'บ                        in EInput.xxx functions.                       บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub set.eonly( text$ ) 					public
   shared eonly$
    eonly$ = text$

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:set.eonly                                        12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :set.eexcept                                         12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ    Hold value in memory to compare this, befor exit of the EInput.xxx บ
'บ    functions, with the value returned for these.                      บ
'บ    If the format of value to hold and the returned for EInput.xxx     บ
'บ    functions isn't equal, don't run OK.                               บ
'บ    If you use with a date, you must pass in text string a character   บ
'บ    string with a same date format. Equal for numbers and others.      บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    text$()        ===> array with the only values accepteds in        บ
'บ                        EInput.xxx functions                           บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub set.eexcept( text$ ) 				public
   shared eexcept$
    eexcept$ = text$

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:set.eexcept                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :set.ewmenu                                          12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ    Hold values in memory and display these, into a window with a      บ
'บ    pull-down menu, to select directly on of these values.             บ
'บ    If you use with a date, you must pass in text string a character   บ
'บ    string with a same date format. Equal for numbers and others.      บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    text$()        ===> array with values to display into the window   บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub set.ewmenu( text$ ) 				public
   shared ewmenu$
    ewmenu$ = text$

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:set.ewmenu                                       12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub










'*****************************************************************
'*****************************************************************
'*            INTERNAL  SUBs AND FUNCTIONs                       *
'*****************************************************************
'*****************************************************************




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :splitmeformat                                       12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ    Split a character string with values sapareds for me format into   บ
'บ    reatarray$()                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    var$           ===> character string with diferents values betwen  บ
'บ                        accents ' and separeds with commas , .         บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ    retarray$()    ===> array with the var$ good splited               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub splitmeformat( var$, retarray$() )                  public
   local p%
   local c%
   local l%
    l% = 1

    while instr(l%, var$, "'," ) <> 0
       c% = instr(l%,var$,"',")
       incr p%
       retarray$(p%) = mid$(var$,l%+1, c%-l%-1)
       l% = c% + 2
    wend

    retarray$(p%+1) = mid$(var$,l%+1, len(var$)-l%-1)
    retarray$(p%+2) = ""

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:splitmeformat                                    12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ FUNCTION :edate#                                         12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    mdate$         ===> character string with the date                 บ
'บ    type%          ===> type of date passed                            บ
'บ                         0 = USA                                       บ
'บ                         1 = EUROPA                                    บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ    edate#         ===> double-precision number with the date in       บ
'บ                        in format YYYYMMDD                             บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
function edate#( mdate$, type% ) 			public
    local m%,d%,y%
    local date#
      call splitdate( mdate$, type%, m%, d%, y% )

     if not testdate%(m%, d%, y%) then
        print "Incorrect DATE format. Program Break!":end
     end if

     date# = y% * 10000
     date# = date# + (m% * 100)
     date# = date# + d%
     edate# = date#

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END FUNCTION:edate#                                      12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end function




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :splitdate                                           12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ    Split the date and return the month, day an year into tree integersบ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    mdate$         ===> character string with date                     บ
'บ    type%          ===> type of date passed                            บ
'บ                         0 = USA                                       บ
'บ                         1 = EUROPA                                    บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ    m%             ===> month                                          บ
'บ    d%             ===> day                                            บ
'บ    y%             ===> year                                           บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub splitdate( mdate$, type%, m%, d%, y%) 		public
   local i1%, i2%, i3%
   local lend%
    lend% = len(mdate$)
    if instr(mdate$, "/") or_		' separated with /
       instr(mdate$, "-") or_              ' separated with -
       instr(mdate$, "\") then     ' separated with \
       i1%=1 : i2%=4 : i3%=7
    else
       i1%=1 : i2%=3 : i3%=5
    end if


    select case type%
     case 0
       m% = val(mid$(mdate$,i1%,2))
       d% = val(mid$(mdate$,i2%,2))
       y% = val(mid$(mdate$,i3%,lend%+1-i3%))
     case 1
       m% = val(mid$(mdate$,i2%,2))
       d% = val(mid$(mdate$,i1%,2))
       y% = val(mid$(mdate$,i3%,lend%+1-i3%))
     case else
       print "Error in SplitDATE. Program Break!":end
    end select

    if lend%+1-i3% = 2 then y% = y% + 1900

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END SUB:splitdate                                        12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ FUNCTION :testdate%                                      12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ    Lock the date to determine if is correct or not.                   บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    m%             ===> month                                          บ
'บ    d%             ===> day                                            บ
'บ    y%             ===> year                                           บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ    testdate%      ===> true (-1) if the date is correct, false (0) if บ
'บ                        not is correct.                                บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
function testdate%( m%, d%, y% )			public
     if m%<1 or m%>12 or_
        d%<1 or d%> val(mid$(" 312931303130313130313031",m%*2,2)) or_
        (m%=2 and d%>28 and y%/4.0 > y%\4 ) then
        testdate% = 0                              ' false date
     else
        testdate% = -1                             ' true date
     end if

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END FUNCTION:testdate%                                   12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end function




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ FUNCTION :efilter%                                       12-21-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ    Test the datum passed with the filters with SET.xxx before         บ
'บ    use EInput.xxx and if don't pass the test display error in the     บ
'บ    screen, if pass the test return true (-1) and initialize all       บ
'บ    filter variables.                                                  บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    dat$           ===> datum to compare                               บ
'บ    type%          ===> type of datum to compare                       บ
'บ                     0 = text                                          บ
'บ                     1 = number                                        บ
'บ                     2 = date                                          บ
'บ                     3 = time                                          บ
'บ    datetype%      ===> only if type% = date. why type of date?        บ
'บ                     0 = USA                                           บ
'บ                     1 = EUROPA                                        บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ    efilter%       ===> true (-1) if filter was good passed, if not    บ
'บ                        return false (0).                              บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
function efilter%( dat$, type%, datetype% ) 		public
    shared esmallest$, egreatest$, eexcept$, eonly$
    shared esmallesttext$, egreatesttext$, eexcepttext$, eonlytext$
    local temp$()

      dim   temp$(1:100)

    local only$, e%, x$
     x$ = rtrim$(dat$)

     if esmallest$ <> "" then			' SMALLEST filter

        select case type%
         case 0				' text
           if x$ < rtrim$(esmallest$) then e% = -1
         case 1				' numbers
           if val(elegalnum$(x$)) <_
           val(elegalnum$(esmallest$)) then e% = -1
         case 2				' dates
           if edate#(x$,datetype%) <_
           edate#(esmallest$,datetype%) then e% = -1
         case 3				' times
           if etime#(x$,datetype%) <_
           etime#(esmallest$,datetype%) then e% = -1
        end select


        if e% then
           efilter% = -1
            call edisplayerror(esmallesttext$+esmallest$)
           exit function
        end if

     end if


     if egreatest$ <> "" then			' GREATEST filter

        select case type%
         case 0				' text
           if x$ > rtrim$(egreatest$) then e% = -1
         case 1				' numbers
           if val(elegalnum$(x$)) >_
           val(elegalnum$(egreatest$)) then e% = -1
         case 2				' dates
           if edate#(x$,datetype%) >_
           edate#(egreatest$,datetype%) then e% = -1
         case 3				' times
           if etime#(x$,datetype%) >_
           etime#(egreatest$,datetype%) then e% = -1
        end select


        if e% then
           efilter% = -1
            call edisplayerror(egreatesttext$+egreatest$)
           exit function
        end if

     end if


     if eexcept$ <> "" then			' EXCEPT filter
	 call esplitmyformat( eexcept$, temp$() )
        p% = 1

        do

           select case type%
            case 0				' text
              if x$ = rtrim$(temp$(p%)) then e% = -1
            case 1				' numbers
              if val(elegalnum$(x$)) =_
              val(elegalnum$(temp$(p%))) then e% = -1
            case 2				' dates
              if edate#(x$,datetype%) =_
              edate#(temp$(p%),datetype%) then e% = -1
            case 3				' times
              if etime#(x$,datetype%) =_
              etime#(temp$(p%),datetype%) then e% = -1
           end select


           if e% then
              efilter% = -1
               call edisplayerror(eexcepttext$+temp$(p%))
              exit function
           end if

           incr p%,1
        loop until temp$(p%) = ""

     end if


     if eonly$ <> "" then			' ONLY filter
         call splitmyformat( eonly$, temp$() )
        p% = 1
        e% = -1

        do
           only$ = only$ + temp$(p%)+" "

           select case type%
            case 0				' text
              if x$ = rtrim$(temp$(p%)) then e% = 0
            case 1				' numbers
              if val(elegalnum$(x$)) =_
              val(elegalnum$(temp$(p%))) then e% = 0
            case 2				' dates
              if edate#(x$,datetype%) =_
              edate#(temp$(p%),datetype%) then e% = 0
            case 3				' times
              if etime#(x$,datetype%) =_
              etime#(temp$(p%),datetype%) then e% = 0
           end select

           incr p%,1
        loop until temp$(p%) = ""


        if e% then
           efilter% = -1
            call edisplayerror(eonlytext$+only$)
           exit function
        end if

     end if

     egreatest$ = ""		' Initialice all
     esmallest$ = ""		' filters because
     eexcept$   = ""		' the test has good
     eonly$     = ""		' passed.

'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ END FUNCTION:efilter%                                    12-21-1991   บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end function




