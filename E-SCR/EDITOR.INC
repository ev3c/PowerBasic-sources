'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :editor                                              02-10-1992   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub editor
   shared file.save%		' flag para grabar pantalla
   shared file$			' nombre del fichero editado
   shared mchr$(),mchr%		' pincell$ i pincell seleccionat
   shared mpincel%		' pincel ON/OFF
   shared car%()		' caracters especials
   shared fgc%, bgc%		' color seleccionat
   shared posy%, posx%		' posiciข a la pantalla
   shared bloques.abc.num%	' alfabeto seleccionado
   shared ayuda.min%, ayuda.max%' nivel de ayuda

    ayuda.min% = 7
    ayuda.max% = 8

     call editor.scr.rest

    do

       t$ = inkey$

       locate posy%, posx%, 1, 0, 15
       color fgc%, bgc%

        call kbd.flags(mins%, mcap%, mnum%, mdes%, malt%, mctl%, mlsh%, mrsh%)
        call scr.defseg

       if t$ <> "" then call editor.line.rest


       select case t$

        case chr$(&h00,&h53) 	   		' Delete to right

          dat$ = scr.get.chrattr$(posy%,posx%,80+1-posx%)
          dat$ = mid$(dat$,3,len(dat$)-2) + " "+chr$(7)
          m% = ((posy%-1)*160) + ((posx%-1)*2)
          poke$ m%, dat$
	  file.save% = -1

        case chr$(&h00,&h4b)	    		' Left
	  way$ = "left"
	  decr posx%, 1
        case chr$(&h00,&h4d)   			' Right
	  way$ = "right"
          incr posx%, 1
        case chr$(&h00,&h48) 			' Up
	  way$ = "up"
          decr posy%, 1
        case chr$(&h00,&h50)			' Down
	  way$ = "down"
          incr posy%, 1
        case chr$(&h00,&h4f):posx% = 80		' End
        case chr$(&h00,&h47):posx% = 1		' Home
        case chr$(&h0,&h049):posy% = 1		' Pag up
        case chr$(&h0,&h051):posy% = 25		' Pag Down
        case chr$(&h00,&h52)			' Insert
        case chr$(&h1b)				' Esc
          exit loop
        case chr$(&h00,&h3b) to chr$(&h00,&h44) ' Function Keys

          select case right$(t$,1)
	   case chr$(&h3b): call ficheros.bload		' F1
	   case chr$(&h3c): call grabar.gra		' F2
           case chr$(&h3d): call utilidad.vercolor	' F3
	   case chr$(&h3e): call utilidad.verpos	' F4
           case chr$(&h3f): call utilidad.color		' F5
	   case chr$(&h40)              		' F6
	      call utilidad.ascii
	      file.save% = -1
	   case chr$(&h41)              		' F7
	      call utilidad.pincel
	      way$ = ""
	   case chr$(&h42)				' F8
              call bloques.scr.scr
              call editor.scr.hold
	      file.save% = -1
           case chr$(&h43)				' F9
              call bloques.scr.disk
              call editor.scr.hold
           case chr$(&h44)				' F10
              call bloques.disk.scr
              call editor.scr.hold
	      file.save% = -1
           case else
          end select

	case > ""  				' Print character

          t$ = right$(t$,1)

          if mdes% then				' Bloqueo Desplazador

             blq$ = bloques.abc.get$( bloques.abc.num%, t$ )

             if blq$ <> "" then
		 call bloques.scr.put( blq$ )
		 file.save% = -1
	     end if

          elseif malt% then			' Alt

	     p$ = chr$( car%( asc(t$) + 20 ) )

          elseif mctl% then			' Control

	     p$ = chr$( car%( asc(t$) ) )

          elseif t$ = chr$(&h08) then		' Delete to left

             if posx% > 1 then
                dat$ = scr.get.chrattr$(posy%, posx%, 80+1-posx%)
                m% = ((posy%-1)*160) + ((posx%-2)*2)
                dat$ = dat$+" "+chr$(7)
                poke$ m%, dat$
                posx% = posx% -1
		file.save% = -1
	     end if

	  elseif t$ = chr$(&h0d) then		' Intro

	     incr posy%, 1

	  else

	     p$ = t$

	  end if


       end select


       if p$ <> "" then

	  if mins% then				' if insert
	     dat$ = scr.get.chrattr$(posy%, posx%, 80+1-posx%)
	     dat$ = left$(dat$,len(dat$)-2)
	     m% = ((posy%-1)*160) + (posx% * 2)
	     poke$ m%, dat$
	  end if

	  m% = (160*(posy%-1)) + (2*(posx%-1))
	  p% = asc(p$)
	  poke m%, p%
	  attr% = scr.color2attr%(fgc%,bgc%)
	  if attr% then poke m%+1, attr%
	  incr posx%, 1
	  file.save% = -1
	  p$ = ""

       end if

       if posy% > 25 then posy% = 1
       if posy% < 1  then posy% = 25
       if posx% > 80 then posx% = 1
       if posx% < 1  then posx% = 80

       if mpincel% and way$<>"" then
	  call pincel.putchr(csrlin, pos(0), way$)
	  way$ = ""
	  file.save% = -1
       end if

       if t$ <> "" then
           call editor.line.hold
           call editor.line.see( mins%, mcap%, mnum%, mdes%)
       end if

    loop while -1

     call editor.scr.hold
    locate , , 0

end sub





sub editor.line.see( mins%, mcap%, mnum%, mdes%) 		shared
   local valor%

    color wmsgfgc%,wmsgbgc%

    locate ,,0

    if mpincel% then
       locate 25,1

       select case mchr%
        case 1 : print "ฺยฟรลดภมูณฤ ";
        case 2 : print "ษหปฬฮนศสผบอ ";
        case 3 : print "ึาทวืถำะฝบฤ ";
        case 4 : print "ีัธฦุติฯพณอ ";
	case 5
	   $if %english
		 print " Only color ";
	   $else
		 print " Solo color ";
	   $endif
	case 6
	   $if %english
		 print " Character ";mchr.chr$;
	   $else
		 print " Caracter ";mchr.chr$;
	   $endif

       end select

    end if

    if mcolor% then
       locate 25,12
       valor% = peek( ((posy%-1)*160)+((posx%-1)*2)+1 )
       print using " fgc(##) bgc(##) attr(###) "_
       ;scr.attr2fgc%(valor%);scr.attr2bgc%(valor%);valor%;
    end if

    if mpos% then
       locate 25,38
       print using " y(##) x(##) mem(####) "_
       ;posy%;posx%;((posy%-1)*160)+((posx%-1)*2);
    end if

    if mins% then
       locate 25,60
       print " INS ";
    end if

    if mcap% then
       locate 25,64
       print " CAPS ";
    end if

    if mnum% then
       locate 25,69
       print " NUM ";
    end if

    if mdes% then
       locate 25,73
       print " SCROLL";
    end if

end sub





sub editor.line.hold
   shared mlinea$
    mlinea$ = scr.get.chrattr$(25,1,80)
end sub





sub editor.line.rest
   shared mlinea$
    poke$ 3840, mlinea$
end sub





sub editor.scr.hold
   shared mpantalla$
     call scr.defseg
    mpantalla$ = peek$(0,4000)
     call editor.line.hold
end sub





sub editor.scr.rest
   shared mpantalla$
     call scr.defseg
    poke$ 0, mpantalla$
     call editor.line.rest
end sub





