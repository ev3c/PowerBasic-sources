

'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :bload.ansi                                          02-09-1992   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    file$          ===>                                                บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub bload.ansi (file$)
    shared cfgc%, cbgc%

    ansi$ = dsk.get.file$(file$)		' get the file from the disk

    cfgc% = 7
    cbgc% = 0
    fgc%  = 7
    bgc%  = 0

    y% = 1
    x% = 1

    columns% = scr.columns%
    esc$ = chr$(27)
    crlf$ = chr$(13,10)
    find0$ = esc$+"[s" + crlf$+esc$+"[u"

     replace find0$ with "" in ansi$
     replace "๚[" with esc$+"[" in ansi$
     replace "`[" with esc$+"[" in ansi$

    lenansi% = len(ansi$)


    do
       ansi.esc$ = bload.ansi.get.esc$(ansi$)
       lenansi% = lenansi% - len(ansi.esc$)
       p% = instr(ansi.esc$, any "ABCDfHhJKlmnRsu")
       num$ = mid$(ansi.esc$, 3, p% - 3)
       ins$ = mid$(ansi.esc$, p%, 1)
       txt$ = right$(ansi.esc$, len(ansi.esc$) - p%)


       select case ins$

	case "A"                 ' UP n lines. if n=0 then n=1
          n% = val(num$)
          if n% = 0 then n% = 1
	  y% = y% - n%

	case "B"                 ' DOWN n lines. if n=0 then n=1
          n% = val(num$)
          if n% = 0 then n% = 1
	  y% = y% + n%

	case "C"                 ' RIGHT n lines. if n=0 then n=1
          n% = val(num$)
          if n% = 0 then n% = 1
	  x% = x% + n%

	case "D"                 ' LEFT n lines. if n=0 then n=1
          n% = val(num$)
          if n% = 0 then n% = 1
	  x% = x% - n%

	case "f", "H", "R"   	' LOCATE n;f. if n or f = 0 then 1
          if num$ <> "" then

	     m% = instr(num$, ";")

	     if m% > 0 then
		y% = val(left$(num$, m%-1))
		x% = val(mid$(num$,m%+1,len(num$)-m%))
             else
                y% = val(num$)
                x% = 1
             end if

          else
             y% = 1
             x% = 1
          end if

          if c$ = "R" then
	     oldy% = y%
	     oldx% = x%
          end if

	case "h", "l"  		' type of screen
          scr% = val(num$)

          select case scr%
	   case 0 		' 40*25 black and white
             screen 0
             width 40
             columns% = 40
	   case 1 		' 40*25 color
             screen 0
             width 40
             columns% = 40
	   case 2:screen 0: width 80	' 80*25 black and withe
	   case 3:screen 0: width 80 	' 80*25 color
	   case 4:screen 1 		' 320*200 color
	   case 5:screen 1		' 320*200 black and white
	   case 6:screen 2		' 640*200 black and white
	   case 7:scroll%=-1: screen 0	' scroll up when end of screen
	   case else: screen 0		' other case
          end select

	case "J"  		' clear screen. CLS
          if fgc% <> 0 or bgc% <> 0 then
	     cfgc% = fgc%
	     cbgc% = bgc%
	  else
	     cfgc% = 7
	     cbgc% = 0
          end if

	  color cfgc%, cbgc%
	  cls

        case "K"                 ' clear to end of line
	  color cfgc%, cbgc%
	  print space$(80 - x%);
          color fgc%, bgc%
          locate y%, x%

	case "m"                 ' set colors
	  call scr.ansi2color.fn( num$, fgc%, bgc% )

	case "n", "s"            ' hold the cursor position in memo
	  oldy% = y%
	  oldx% = x%

	case "u"                 ' restart the cursor position holded.
          y% = oldy%
          x% = oldx%

	case else
	  error 102
       end select


       if mfgc% <> fgc% or mbgc% <> bgc% then
	  color fgc%, bgc%
	  mfgc% = fgc%
	  mbgc% = bgc%
       end if

       if x% < 1  then x% = 1
       if y% < 1 then y% = 1


       crlf% = instr(txt$, crlf$)
       if crlf% > 0 then
	  replace crlf$ with "" in txt$
	  locate y%, x%
	  print left$(txt$,crlf%);

	  txt$ = right$(txt$, len(txt$)+1-crlf%)
	  incr y%
	  x% = 1
       end if

       if txt$ <> "" then
	  locate y%, x%
	  print txt$;
	  y% = csrlin
	  x% = pos(0)
       end if

    loop while lenansi%

end sub





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ FUNCTION :bload.ansi.getesc$                               02-09-1992 บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    ansi$          ===>                                                บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ    bload.ansi.getesc===>                                              บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
function bload.ansi.get.esc$ (ansi$)

    static p%
     if p% = 0 then p% = 1
     e% = instr(p%+1, ansi$, chr$(27) )

     if e% > 0 then
	bload.ansi.get.esc$ = mid$(ansi$, p%, e%-p% )
        p% = e%
     else
	bload.ansi.get.esc$ = mid$(ansi$, p%, len(ansi$)+1-p%)
        p% = 0
     end if

end function








