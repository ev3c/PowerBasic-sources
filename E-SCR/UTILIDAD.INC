sub utilidad
   shared wmfgc%, wmbgc%
   local menu$()
   static oldmenu%
   shared ayuda.min%, ayuda.max%
   shared fascii%


     dim menu$(1:10)


    $if %english
    menu$(1) = "Select color        F5"
    menu$(2) = "Input all ASCII     F6"
    menu$(3) = "Brush in cursor     F7"
    menu$(4) = "Merge screen"
    menu$(5) = "Print screen"
    menu$(6) = "See color position  F3"
    menu$(7) = "See cursor position F4"
    menu$(8) = "Clear screen"
    $else
    menu$(1) = "Seleccionar color   F5"
    menu$(2) = "Entrar ASCII        F6"
    menu$(3) = "Pincel cursor       F7"
    menu$(4) = "Mezclar pantalla"
    menu$(5) = "Imprimir pantalla"
    menu$(6) = "Ver color posici¢n  F3"
    menu$(7) = "Ver posici¢n cursor F4"
    menu$(8) = "Borrar pantalla"
    $endif

     call wopen(2,25,11,50,1,0,wmfgc%,wmbgc%,"")


    do

       ayuda.min% = 3
       ayuda.max% = 3

        call set.wmenu.leftrighton
	menu% = wmenu(menu$(), oldmenu%)

       select case menu%
        case 1 : call utilidad.color
        case 2
           call wsavewindows
           call editor.scr.rest
           call utilidad.ascii
           call wrestorewindows
          if fascii% then call key.esc(2)

        case 3 : call utilidad.pincel
        case 4 : call utilidad.merge
        case 5 : call utilidad.print
        case 6 : call utilidad.vercolor
        case 7 : call utilidad.verpos
        case 8 : call utilidad.cls
       end select

    loop until menu% = 0

     call wclose

end sub





sub utilidad.color  			shared
     call wcolor (10,20,fgc%,bgc%)
end sub





sub utilidad.ascii
   shared file.save%
   shared fgc%, bgc%
   shared posy%, posx%
   shared fascii%

    fascii% = 0

     call editor.line.rest

    do
       t$ = wascii$(4,7)

       if t$ <> "" then
          file.save% = -1
          blq$ = mki$(posy%)+mki$(posx%)+mki$(posy%)+mki$(posx%)+_
          t$+chr$(scr.color2attr%(fgc%,bgc%))
           call bloques.scr.put( blq$ )
           call editor.scr.hold
          fascii% = -1
       end if

    loop while t$ <> ""

end sub





sub utilidad.pincel
   shared mpincel%
   shared mchr%, mchr.chr$
   static oldmchr%

    if mpincel% then
       mpincel% = 0
       $if %english
       msg$ = " Brush in cursor OFF "
       $else
       msg$ = " Pincel en cursor OFF "
       $endif

        call wmsg( 0, 0, msg$ )
    else

	dim menu$(1:5)

       menu$(1) = "A ÚÂ¿ ÃÅ´ ÀÁÙ ³ Ä "
       menu$(2) = "B ÉË» ÌÎ¹ ÈÊ¼ º Í "
       menu$(3) = "C ÖÒ· Ç×¶ ÓÐ½ º Ä "
       menu$(4) = "D ÕÑ¸ ÆØµ ÔÏ¾ ³ Í "
       $if %english
	   menu$(5) = "Only change color"
	   menu$(6) = "Paint with character"
       $else
	   menu$(5) = "Solo cambiar color"
	   menu$(6) = "Pintar con caracter"
       $endif

	mchr% = wsmenu%(-7, 0, menu$(), oldmchr%)

       if mchr% = 0 then
          mpincel% = 0
       else
	  if mchr% = 6 then mchr.chr$ = wascii$(4,7)
	  mpincel% = -1
       end if

    end if

end sub





sub utilidad.merge
   shared file.save%
   shared p%()
   local path$
   local  x%

    path$ = ficheros.curdir$



    file$ = wdir$(8, 20, 1, path$+"*.SCR.ASC.ANS.BIN",&h16, 0, 1 )

    if file$ <> "" then

        call wsavewindows
	call editor.scr.rest

        call splitpathfn(file$,d$,p$,f$,e$)

        call ficheros.bload.ok( f$, e$ )

       $if %english
       sino$ = " Do you want to merge this screen ?"
       $else
       sino$ = "¨ Quieres mezclar esta pantalla ?"
       $endif

       if wsino%( 10, 0, sino$, e% ) then

          file.save% = -1

           call readscreen
           call editor.scr.rest

          for x% = 0 to 4000 step 2

             if peek(x%) = 32 then

		if scr.attr2bgc%(peek(x%+1)) = 0 then
                   poke x%, p%(x%)
                   poke x%+1, p%(x%+1)
                end if

             end if

          next x%

           call editor.scr.hold
	   call key.esc(3)
	   call kbd.putkey(&h0d, &h1c)

       end if

        call wrestorewindows

    end if

end sub





sub utilidad.print
   static oldmenu%
   local menu$()
   local menu%

     dim menu$(1:5)

    $if %english
    menu$(1) = " Screen text "
    menu$(2) = " Screen colors "
    $else
    menu$(1) = " Texto pantalla "
    menu$(2) = " Colores pantalla"
    $endif

    do
	menu% = wsmenu%( -6, 0, menu$(), oldmenu% )

       if menu% <> 0 then

          if utilidad.print.start% then
              call wsavewindows
              call editor.scr.rest

             select case menu%
              case 1 : call utilidad.print.scr
              case 2 : call utilidad.print.col
             end select

              call wrestorewindows
          end if

       end if

    loop until menu% = 0

end sub





function utilidad.print.start%

     $if %english
     sino$ = "Printer ON-LINE ?"
     $else
     sino$ = "¨ Impresora ON-LINE ?"
     $endif

      call prn.start
     utilidad.print.start% = wsino%(0,0,sino$,e%)

end function





sub utilidad.print.head( text$ )
   shared mprograma$
   shared file$

     call splitpathfn( file$, d$, dir$, f$, e$)

    lprint chr$(15)
    width "LPT1:",132

    lprint:lprint:lprint
    lprint "	   Ú";string$(80,"Ä");"¿"
    lprint "	   ³	    ";mprograma$;"   por  Esteve Valent¡ Morales ";date$;"	     ³"
    lprint "	   ³         Pantalla....:   ";f$;e$;space$(51-len(f$+e$));"³"
    lprint "	   Ã";string$(80,"Ä");"´"
    lprint "	   ³		  **** ";text$;" ****		  ³"
    lprint "	   À";string$(80,"Ä");"Ù"
    lprint:lprint:lprint

end sub





sub utilidad.print.scr

     call utilidad.print.head( " I M P R E S I O N   D E   P A N T A L L A " )

    lprint "COLUMNA	   10	     20	       30	 40	   50	     60	       70	 80 COLUMNA"
    lprint "       ";string$(8,"123456789|")
    lprint " LINEA										     LINEA"


    for y% = 1 to 25
       lprint using "  ##   ";y%;
       lprint scr.text$( y%, 1, 80 );
       lprint using "   ##";y%
    next y%


    lprint " LINEA										     LINEA"
    lprint "       ";string$(8,"123456789|")
    lprint "COLUMNA	   10	     20	       30	 40	   50	     60	       70	 80 COLUMNA"

    lprint chr$(12)
    
end sub





sub utilidad.print.col

     call utilidad.print.head( " I M P R E S I O N    D E    C O L O R E S " )

    lprint string$(7," LIN,COL=FGC,BGC ")
    lprint string$(7," _______________ ")

    ypos% = 1  		'Posicion Y
    xpos% = 0  		'Posicion X
    iypos%=1        	'Ultima Linia impresa
    ico% = 0   		'Ultimo color Impreso
    p%=0
    
     call scr.defseg
    

    for x% = 0 to 4000 step 2
       incr xpos%,1


       if peek(x%+1)<>ico%  then
          incr p%,1
          ico% = peek(x%+1)
          iypos%=ypos%
          lprint using "  ##";ypos%;
          lprint ",";
          lprint using "##";xpos%;
          lprint " = ";
	  lprint using "##";scr.attr2fgc%(peek(x%+1));
          lprint ",";
	  lprint using "##  ";scr.attr2bgc%(peek(x%+1));
       end if

       if p% = 7 and xpos% <> 80 then
          lprint
          p%=0
       elseif p%=7 then
          p%=0
       end if

       if xpos%=80 then
          if iypos%=ypos% then lprint
          incr ypos%,1
          xpos% = 0
          p%=0
       end if

    next x%

    lprint chr$(12)

end sub





sub utilidad.vercolor
   shared mcolor%

    $if %english
    msg0$ = " See color position OFF "
    msg1$ = " See color position ON "
    $else
    msg0$ = " Ver color posici¢n OFF "
    msg1$ = " Ver color posici¢n ON "
    $endif


    if mcolor%  then
       mcolor% = 0
       text$ = msg0$
    else
       mcolor% = -1
       text$ =  msg1$
    end if

     call wmsg(0,0, text$)

end sub





sub utilidad.verpos
   shared mpos%

    $if %english
    msg0$ = " See cursor position OFF "
    msg1$ = " See cursor position ON "
    $else
    msg0$ = " Ver posici¢n cursor OFF "
    msg1$ = " Ver posici¢n cursor ON "
    $endif


    if mpos%  then
       mpos% = 0
       text$ = msg0$
    else
       mpos% = -1
       text$ = msg1$
    end if

     call wmsg(0,0, text$)

end sub





sub utilidad.cls    			shared
    local msg$

    $if %english
	msg$ = " Clear screen ? "
    $else
	msg$ = " ¨ Borrar pantalla ? "
    $endif

     call wcolor(10,20,cfgc%, cbgc%)

    if e% = 0 then
        call wsavewindows
        call editor.scr.rest
       color cfgc%, cbgc%

       if wsino%(0,0,msg$,e%) then
	  if file.save% then
	     call ficheros.writed
	  end if
	  file$     = "NONAME"
	  file.ext$ = ".SCR"
	  file.save% = 0
	  color cfgc%, cbgc%
	  cls
       end if

        call editor.scr.hold
        call wrestorewindows

        call key.esc(3)
	call kbd.putkey(&h0d, &h1c)
    end if

end sub






