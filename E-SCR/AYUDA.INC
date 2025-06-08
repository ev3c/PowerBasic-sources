sub ayuda
   shared wmfgc%, wmbgc%
   local menu$(), menu%
   static oldmenu%
   shared ayuda.min%, ayuda.max%
   shared munidad$

     dim menu$(1:10)

    incr y%
    $if %english
	menu$(y%) = "General Help      F11"
    $else
	menu$(y%) = "Ayuda General     F11"
    $endif


    $if %beta

     $if %english
	 menu$(y%)   = "Register program"
     $else
	 menu$(y%)   = "Registrar programa"
     $endif
     incr y%

     f$ = dir$( munidad$+"\E-SCREEN\MAIL.BLQ", 0)
     if f$ <> "" then
	incr y%
	menu$(y%) = "Mail"
     end if

    $endif

     call wopen(2,55,3+y%,78,1,0,wmfgc%,wmbgc%,"")

    do
       ayuda.min% = 6
       ayuda.max% = 6

        call set.wmenu.leftrighton
	menu% = wmenu(menu$(), oldmenu%)

        call wsavewindows

       select case menu%
        case 1
          ayuda.min% = 1
          ayuda.max% = 8
           call ayuda.general
          $if %beta
        case 2 : call ayuda.registrar
          $endif

	case 3 : call ayuda.mail

       end select

        call wrestorewindows

    loop until menu% = 0


     call wclose
end sub





sub ayuda.general
   shared fgc%, bgc%
   shared munidad$
   shared ayuda.min%, ayuda.max%
   local  x%

    $if %english
    fhelp$ = "E-HELPE"
    $else
    fhelp$ = "E-HELPS"
    $endif

    x% = ayuda.min%

    path$ = munidad$+"\e-screen\"

     call wsavescreen(mpantalla$,1,1,25,80)


    do
       t$ = inkey$


       if x% <> oldx% then

          blq.name$ = path$ + fhelp$ + trim$(str$(x%)) + ".BLQ"

          blq$ = bloques.disk.get$( blq.name$ )

           call wrestorescreen(mpantalla$,1,1,25,80)


          if blq$ <> "" then
              call bloques.scr.put.blq( 0, 0, blq$)
          end if


          oldx% = x%

       end if



       select case t$

        case chr$(&h00,&h49), chr$(&h00,&h48)		' UP

          if x% > ayuda.min% then
             decr x%
          end if


        case chr$(&h00,&h51), chr$(&h00,&h50)		' DOWN

          if x% < ayuda.max% then
             incr x%
          end if


       end select


    loop while t$ <> chr$(&h1b)


     call wrestorescreen(mpantalla$,1,1,25,80)

end sub





$if %beta

sub ayuda.registrar
   shared munidad$

    $if %english
    blq.name$ = munidad$ + "\E-SCREEN\E-SCREGE.BLQ"
    file$     = munidad$ + "\E-SCREEN\ORDERFOR.DOC"
    text$     = "Print ORDER FORM ? "
    $else
    blq.name$ = munidad$ + "\E-SCREEN\E-SCREGS.BLQ"
    file$     = munidad$ + "\E-SCREEN\REGISTRO.DOC"
    text$     = "¨ Imprimo la HOJA DE REGISTRO ? "
    $endif

    blq$ = bloques.disk.get$( blq.name$ )
     call bloques.scr.put.blq( 0, 0, blq$ )

    a$ = input$(1)


    if wsino%(0,0,text$,e%) then


       if utilidad.print.start% then
          ifile% = freefile
          open file$ for input as ifile%

          do while not eof(ifile%)
             line input #ifile%, mlinea$
             lprint mlinea$
          loop

          close ifile%
       end if

    end if


end sub


sub ayuda.mail
   shared munidad$

    blq.name$ = munidad$ + "\E-SCREEN\MAIL.BLQ"
    blq$ = bloques.disk.get$( blq.name$ )
     call bloques.scr.put.blq( 0, 0, blq$ )

    a$ = input$(1)

end sub

$endif
