sub bloques
   shared file.save%
   shared wmfgc%, wmbgc%
   local menu$()
   static oldmenu%
   shared ayuda.min%, ayuda.max%


     dim menu$(1:10)



    $if %english
       menu$(1) = "A Screen to Screen     F8"
       menu$(2) = "B Screen to Disk       F9"
       menu$(3) = "C Screen to Memory"
       menu$(4) = "D Disk to Screen       F10"
       menu$(5) = "E Disk to Memory"
       menu$(6) = "F Memory to Screen"
       menu$(7) = "G Memory to Disk"
       menu$(8) = "H Select Alphabet"
       menu$(9) = "I Clear screen block
       menu$(10)= "J Change color scr. block"
    $else
       menu$(1) = "A Pantalla a Pantalla  F8"
       menu$(2) = "B Pantalla a Disco     F9"
       menu$(3) = "C Pantalla a Memoria"
       menu$(4) = "D Disco a Pantalla     F10"
       menu$(5) = "E Disco a Memoria"
       menu$(6) = "F Memoria a Pantalla"
       menu$(7) = "G Memoria a Disco"
       menu$(8) = "H Seleccionar Alfabeto"
       menu$(9) = "I Borrar bloque pantalla"
       menu$(10)= "J Cambiar color bloque p."
    $endif


     call wopen(2,35,13,65,1,0,wmfgc%,wmbgc%,"")


    do

       ayuda.min% = 4
       ayuda.max% = 4

        call set.wmenu.leftrighton
       menu% = wmenu(menu$(), oldmenu%)


       if menu% <> 0 then
          file.save% = -1
           call wsavewindows
           call editor.scr.rest


          select case menu%
           case 1 : call bloques.scr.scr
           case 2 : call bloques.scr.disk
           case 3 : call bloques.scr.mem
           case 4 : call bloques.disk.scr
           case 5 : call bloques.disk.mem
           case 6 : call bloques.mem.scr
           case 7 : call bloques.mem.disk
           case 8 : call bloques.abc
	   case 9 : call bloques.scr.cls
	   case 10: call bloques.scr.color
	   case 10:
	  end select


           call editor.scr.hold
           call wrestorewindows

	   call key.esc(3)
	   call kbd.putkey(&h0d, &h1c)

       end if


    loop until menu% = 0

    
     call wclose
end sub





sub bloques.scr.scr

    blq$ = bloques.scr.get$

    if blq$ <> "" then
        call bloques.scr.put(blq$)
    end if


end sub





sub bloques.scr.disk

    blq$ = bloques.scr.get$

    if blq$ <> "" then
       
       blq.nam$ = bloques.nam$
       

       if blq.nam$ <> "" then


          if bloques.overwrite%(blq.nam$) then
             
              call bloques.disk.put(blq.nam$,blq$)
             
          end if

          
       end if

       
    end if


end sub





sub bloques.scr.mem

    blq$ = bloques.scr.get$

    if blq$ <> "" then
        call bloques.mem.put( blq$ )
    end if

end sub





sub bloques.disk.scr
   shared munidad$
   local path$

    path$ = munidad$+"\E-SCREEN\BLQ\*.BLQ"

    blq.nam$=wdir$(8,20,1,path$,&h0,0,1)

    if blq.nam$ <> "" then
       blq$ = bloques.disk.get$( blq.nam$ )
        call bloques.scr.put(blq$)
    end if

end sub





sub bloques.disk.mem
   shared munidad$
   local path$
    
    path$ = munidad$+"\E-SCREEN\BLQ\*.BLQ"
    
    blq.nam$ = wdir$(8,20,1,path$,&h0,0,1)
    
    if blq.nam$ <> "" then
       blq$ = bloques.disk.get$( blq.nam$ )
        call bloques.mem.put( blq$)
    end if

end sub





sub bloques.mem.scr
   local blq$

    blq$ = bloques.mem.get$

    if blq$ <> "" then
        call bloques.scr.put(blq$)
    end if

end sub





sub bloques.mem.disk

    blq$ = bloques.mem.get$

    if blq$ <> "" then

       blq.nam$ = bloques.nam$

       if blq.nam$ <> "" then

          if bloques.overwrite%(blq.nam$) then
              call bloques.disk.put( blq.nam$, blq$)
          end if

       end if

    end if

end sub





sub bloques.abc
   shared wmsgfgc%, wmsgbgc%
   shared munidad$
   shared bloques.abc.num%

    $if %english
       text$   = "Do you want select that alphabet ? "
       titulo$ = " ALPHABET : "
    $else
       text$   = "Quieres seleccionar este alfabeto ? "
       titulo$ = " ALFABETO : "
    $endif

    path$ = munidad$+"\E-SCREEN\ABC\"

    num% = bloques.abc.num%
    if num% = 0 then num% = 1

    do
       t$ = inkey$

       if old.num% <> num% then
          old.num% = num%

          blq$ = bloques.abc.get$( num%, "A" )

           call wopen(3, 20, 22, 50, 1, 1, _
          wmsgfgc%, wmsgbgc%, titulo$ + str$(num%))

          if blq$ <> "" then
              call bloques.scr.put.blq( 7, 22, blq$ )
          else
             t$ = chr$(&h00,&h50)
          end if

       end if

       select case t$

        case chr$(&h00,&h48), chr$(&h00,&h49)	' Up

          do while num% > 1
             decr num% , 1
             abc$ = right$("00000"+trim$(str$(num%)),2)
             abc$ = "A-"+abc$+".ABC"
             f$ = dir$(path$+abc$, &h0)

             if f$ <> "" then
                 call wclose
                exit loop
             end if

          loop

          if f$ = "" then num% = old.num%

        case chr$(&h00,&h50), chr$(&h00,&h51)	' Down

          do while num% < 100
             incr num% , 1
             abc$ = right$("00000"+trim$(str$(num%)),2)
             abc$ = "A-"+abc$+".ABC"
             f$ = dir$(path$+abc$, &h0)

             if f$ <> "" then
                 call wclose
                exit loop
             end if

          loop

          if f$ = "" then num% = old.num%

        case chr$(&h0d)			' Intro

          if wsino%(0,0,text$,e%) then
             bloques.abc.num% = num%
             exit loop
          end if

       end select

    loop while t$ <> chr$(&h1b)

     call wclose

end sub





function bloques.abc.get$( num%, letter$ )
    shared munidad$

     letter% = instr("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ", ucase$(letter$))

     if letter% > 0 then
        path$ = munidad$ + "\e-screen\abc\"
        abc$ = right$("00000"+trim$(str$(num%)),2)
        abc$ = "A-"+abc$+".ABC"
        f$ = dir$(path$+abc$, &h0)

        if f$ <> "" then
           fabc% = frefile
           open path$+abc$ for random as fabc% len = 512
           field #fabc%, 512 as blq$
           get #fabc%, letter%
           bloques.abc.get$ = blq$
           close #fabc%
        end if

     end if

end function





function bloques.scr.get$
    shared e%
    shared posy%, posx%

     locate posy%,posx%,1
      call cursor(0,0,0,0,0, blq$)

     if e% = 0 then

        boty% = posy%
        botx% = posx%

         call cursor(boty%-1, botx%-1, 0, 0, 1, blq$)

        if e% = 0  then

           blq$ = bloques.scr.get.blq$( boty%, botx%, posy%, posx% )

           $if %english
              sino$ = " Clear the screen part corresponding that block ?"
           $else
              sino$ = "จ Borro la parte de la pantalla correspondiente al Bloque ?"
           $endif

           sino% = wsino%( 0, 0, sino$, e% )

           if e% = 0 then

              if sino% then 	  		'** Borra El Bloque de la Pantalla

		  call bloques.scr.cls.ok( blq$ )

              end if

              bloques.scr.get$ = blq$

           end if

        end if

     end if

end function







sub bloques.scr.cls
    shared e%
    shared posy%, posx%

     locate posy%,posx%,1
      call cursor(0,0,0,0,0, blq$)

     if e% = 0 then

        boty% = posy%
        botx% = posx%

         call cursor(boty%-1, botx%-1, 0, 0, 1, blq$)

        if e% = 0  then

           blq$ = bloques.scr.get.blq$( boty%, botx%, posy%, posx% )

	   call bloques.scr.cls.ok( blq$ )

        end if

     end if


end sub







sub bloques.scr.cls.ok( blq$ )
   static cfgc%, cbgc%
   shared e%
   static menu.ini%

    mblq$ = blq$
    y1% = cvi(mid$(blq$,1,2))
    x1% = cvi(mid$(blq$,3,2))
    y2% = cvi(mid$(blq$,5,2))
    x2% = cvi(mid$(blq$,7,2))

     dim menu$(1:3)

    $if %english
       menu$(1) = "All characters"
       menu$(2) = "Character to character"
       msg$ = "Change selected character with ..."
    $else
       menu$(1) = "Todos los caracteres"
       menu$(2) = "Caracter a caracter"
       msg$ = "Cambiar el caracter seleccionado por..."
    $endif

    menu% = wsmenu%(0, 0, menu$(), menu.ini% )

    select case menu%

     case 1

	   call wcolor(10,20,cfgc%, cbgc%)
	  cattr% = scr.color2attr%(cfgc%, cbgc%)
	  cattr$ = chr$(cattr%)
          if e% then exit sub

	cchr$ = wascii$(4,7)

	if e% = 0 then

	 for p% = 9 to len(mblq$) step 2
	   mid$(mblq$, p%, 1) = cchr$
	   if cattr% then
	     mid$(mblq$, p%+1, 1) = cattr$
	   end if
	 next p%

	 call bloques.scr.put.blq( y1%, x1%, mblq$ )

	end if

     case 2

      do

	   call wcolor(10,20,cfgc%, cbgc%)
	  cattr% = scr.color2attr%(cfgc%, cbgc%)
	  cattr$ = chr$(cattr%)
          if e% then exit sub

	schr$ = wascii$(4,7)

	if e% = 0 then

	   call wmsg$(0,0,msg$)

	   cchr$ = wascii$(4,7)

	   if e% = 0 then

	     for p% = 9 to len(mblq$) step 2
		 if mid$(mblq$,p%,1) = schr$ then
		    mid$(mblq$,p%,1) = cchr$
		   if cattr% then
		     mid$(mblq$, p%+1, 1) = cattr$
		   end if
		 end if
	     next p%

	     call bloques.scr.put.blq( y1%, x1%, mblq$ )

	   end if

	end if

      loop while e% = 0

     end select

end sub





sub bloques.scr.put( blq$ )
   shared e%
   shared posy%,posx%
   shared bfgc%, bbgc%

    y1% = cvi(mid$(blq$,1,2))
    x1% = cvi(mid$(blq$,3,2))
    y2% = cvi(mid$(blq$,5,2))
    x2% = cvi(mid$(blq$,7,2))


    if posy% =< 25-(y2%-y1%) and posx% =< 80-(x2%-x1%) then
       locate posy%, posx%
    else
       locate y1%,x1%
    end if

     call cursor (0, 0, y2%-y1%, x2%-x1%, 2, blq$)


    if e% = 0 then
        call bloques.scr.put.blq( posy%, posx%, blq$ )

       $if %english
          sino$ = " Change original colors of block ?"
       $else
          sino$ = "จ Modificar los colores originales del Bloque ?"
       $endif

       if wsino%( 10, 0, sino$, e%) then

	   call bloques.scr.color.ok( blq$ )

       end if

       posx% = posx% + x2% + 2 - x1%

    end if

end sub




sub bloques.scr.color
    shared e%
    shared posy%, posx%

     locate posy%,posx%,1
      call cursor(0,0,0,0,0, blq$)

     if e% = 0 then

        boty% = posy%
        botx% = posx%

         call cursor(boty%-1, botx%-1, 0, 0, 1, blq$)

        if e% = 0  then

           blq$ = bloques.scr.get.blq$( boty%, botx%, posy%, posx% )

	   posy% = boty%
	   posx% = botx%

	   call bloques.scr.color.ok( blq$ )

        end if

     end if

end sub





sub bloques.scr.color.ok( blq$ )
   shared e%
   shared posy%, posx%
   static menu.ini%
   static sfgc%, sbgc%
   static cfgc%, cbgc%

    y1% = cvi(mid$(blq$,1,2))
    x1% = cvi(mid$(blq$,3,2))
    y2% = cvi(mid$(blq$,5,2))
    x2% = cvi(mid$(blq$,7,2))

     dim menu$(1:3)

    $if %english
       menu$(1) = "All colors"
       menu$(2) = "Color to color"
       msg$ = "Change selected color with ..."
    $else
       menu$(1) = "Todos los colores"
       menu$(2) = "Color a color"
       msg$ = "Cambiar el color seleccionado por..."
    $endif

    menu% = wsmenu%(0, 0, menu$(), menu.ini% )

    select case menu%
     case 1
	call wcolor (13,25,cfgc%,cbgc%)

       if e% = 0 then
	  cattr% = scr.color2attr% (cfgc%,cbgc%)

	  if not cattr% then

	     cattr$ = chr$(cattr%)

             for p% = 10 to len(blq$) step 2
		 mid$(blq$,p%,1) = cattr$
             next p%

	   call bloques.scr.put.blq( posy%, posx%, blq$ )

	  end if

       end if

     case 2

       do
           call wcolor(10,20,sfgc%, sbgc%)
          sattr% = scr.color2attr%(sfgc%, sbgc%)
          sattr$ = chr$(sattr%)
          if e% then exit sub

           call wmsg(0,0,msg$)

           call wcolor(10,20,cfgc%, cbgc%)
          cattr% = scr.color2attr%(cfgc%, cbgc%)
          cattr$ = chr$(cattr%)
          if e% then exit sub

          if cattr% then

             for p% = 10 to len(blq$) step 2

                if sattr% then

                   if mid$(blq$,p%,1) = sattr$ then
                      mid$(blq$,p%,1) = cattr$
                   end if

                else
                   mid$(blq$,p%,1) = cattr$
                end if

             next p%

	      call bloques.scr.put.blq( posy%, posx%, blq$ )

          end if

       loop while e% = 0

    end select

end sub





function bloques.mem.get$
    shared blq$()
    local blq.num%


     do
        blq.num% = bloques.num%
        if blq.num% = 0 then exit function
        if trim$(blq$(blq.num%)) = "" then error 100
     loop while trim$(blq$(blq.num%)) = ""


     bloques.mem.get$ = blq$(blq.num%)

end function





sub bloques.mem.put( blq$ )
   shared blq$()


    $if %english
       sino$ = "The memory block is empty. Overwrite ? "
    $else
       sino$ = "El Bloque de Memoria no est  vacio. จ Copio encima ? "
    $endif



    do
       blq.num% = bloques.num%
       if blq.num% = 0 then exit sub


       if trim$(blq$(blq.num%)) <> "" then
          sino% = wsino%( 0, 0, sino$, e% )
          if e% = 1 then exit sub
       else
          sino% = -1
       end if

       
    loop while not sino%

    
    blq$(blq.num%) = blq$
end sub





function bloques.disk.get$( file$ )
    local p%
    local lenfile%
    local file%
     file% = freefile
     blq$ = ""
     open file$ for binary as file%
     lenfile% = lof(file%)
     get$ #file%, lenfile%, blq$
     close #file%
     bloques.disk.get$ = blq$
end function





sub bloques.disk.put(file$,blq$)
   local p%
   local file%
    file% = freefile
    open file$ for binary as file%
    put$ #file%, blq$
    close #file%
end sub





'------------------------------------------------------------------------

function bloques.overwrite%(file$)
    local sino$
     
     bloques.overwrite% = -1
     
     f$ = dir$( file$, &h00)
     

     if f$ <> "" then
        

        $if %english
           sino$ = "The block "+f$+" exist.  Do you want overwrite ?"
        $else
           sino$ = "El bloque "+f$+" ya existe. จ Quieres grabar encima ?"
        $endif



        if wsino%( 0, 0, sino$, e% ) then
           kill file$
        else
           bloques.overwrite% = 0
        end if


     end if

     
end function





function bloques.nam$
    shared munidad$
    local  path$
    static blq.nam$
     
     path$ = munidad$+"\E-SCREEN\BLQ\"
     

     do
        ltext$ = path$
        rtext$ = ".BLQ"
         call set.weinput.default(blq.nam$)
        blq.nam$ = weinput$( 0, 0, ltext$, rtext$, 8, 1, e% )
        blq.nam$ = trim$(blq.nam$)
        
        if e% <> 0 then exit function
        
     loop until blq.nam$<>""

     
     bloques.nam$ = path$ + blq.nam$ + ".BLQ"

end function





function bloques.num%
    local ltext$
    static dato$
     
     $if %english
        ltext$ = "Block name in the memory (0 to 9) or (A to Z) ....: "
     $else
        ltext$ = "Nombre del bloque en la memoria (0 a 9) o (A a Z) ....: "
     $endif

     do
         call set.ekbdfilter("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
         call set.weinput.default(dato$)
        dato$ = weinput$(0,0,ltext$,"", 1, 1, e% )
        if e% <> 0 then exit function

     loop while dato$ = " "

     bloques.num% = instr("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ",dato$)

end function





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :cursor                                              12-25-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    y1%            ===>                                                บ
'บ    x1%            ===>                                                บ
'บ    y2%            ===>                                                บ
'บ    x2%            ===>                                                บ
'บ    mode%          ===>                                                บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub cursor( y1%, x1%, y2%, x2%, mode%, blq$)
   shared posx%,posy%
   shared woattrib%
   shared e%
   shared wmessagefgc%,wmessagebgc%


    if mode% = 0 then

     call scr.holdcur
     call wopen(22,15,25,65,1,0,wmessagefgc%,wmessagebgc%,"")

     $if %english
         call wprint(1,0,"Select the upper left corner  ฺ  with ")
         call wprint(2,0,"the cursor keys and then press RETURN." )
     $else
         call wprint(1,0,"Selecciona con las teclas del cursor la esquina ")
         call wprint(2,0,"superior izquierda  ฺ  del bloque y pulsa INTRO." )
     $endif
     call scr.restcur

    end if


    e% = 0
    mattr$ = chr$(woattrib%)

    do

       t$=inkey$
       locate ,,1

       if oldy% <> posy% or posx% <> oldx% then

           call wsavewindows

          if posy% < y1% +1 then posy% = 25-y2%
          if posy% > 25-y2% then posy% = y1% +1
          if posx% < x1% +1 then posx% = 80-x2%
          if posx% > 80-x2% then posx% = x1% +1

	  locate posy%, posx%,1

          select case mode%
           case 1
             blq$ = bloques.scr.get.blq$( y1%+1, x1%+1, posy%, posx%)

             for p% = 10 to len(blq$) step 2
                mid$(blq$,p%,1) = mattr$
             next

              call bloques.scr.put.blq(y1%+1, x1%+1, blq$)

           case 2
              call bloques.scr.put.blq( posy%, posx%, blq$ )

          end select

          oldy% = posy%
          oldx% = posx%
       end if

       select case t$
        case chr$(&h00,&h4b): decr posx%, 1    	   	' Left
        case chr$(&h00,&h4d): incr posx%, 1     	' Right
        case chr$(&h00,&h48): decr posy%, 1		' Up
        case chr$(&h00,&h50): incr posy%, 1   		' Down
        case chr$(&h00,&h47): posx% = 200 	       	' Home
        case chr$(&h00,&h4f): posx% = -5	       	' End
        case chr$(&h00,&h49): posy% = 100		' Pag Up
        case chr$(&h00,&h51): posy% = -5		' Pag Down
        case chr$(&h1b)					' Esc
          e% = 1
          exit loop
       end select

       if oldy% <> posy% or posx% <> oldx% then
           call wrestorewindows
       end if

    loop while t$ <> chr$(&h0d)

     call wrestorewindows
     if mode% = 0 then call wclose

end sub





sub bloques.scr.put.blq( boty%, botx%, blq$ )

     call scr.defseg

    y1% = cvi(mid$(blq$,1,2))
    x1% = cvi(mid$(blq$,3,2))
    y2% = cvi(mid$(blq$,5,2))
    x2% = cvi(mid$(blq$,7,2))

    lx% = (x2% + 1 - x1%) * 2

    if boty% = 0 then boty% = y1%
    if botx% = 0 then botx% = x1%

    p% = 9

    for y% = boty% to boty% + y2% - y1%
       m% = (160 * (y%-1)) + (2 * (botx%-1))
       poke$ m%, mid$(blq$,p%,lx%)
       incr p%, lx%
    next y%

end sub





function bloques.scr.get.blq$( boty%, botx%, topy%, topx% )

     y1% = boty%
     x1% = botx%
     y2% = topy%
     x2% = topx%

     blq$ = blq$ + mki$(y1%)_
     + mki$(x1%) + mki$(y2%) + mki$(x2%)

     for y% = y1% to y2%
        blq$ = blq$ + scr.get.chrattr$( y%, x1%, x2%+1-x1%)
     next y%

     bloques.scr.get.blq$ = blq$
end function





