sub ficheros
   shared wmfgc%, wmbgc%
   shared fichero.bload%
   local menu$
   static oldmenu%
   shared ayuda.min%, ayuda.max%

     dim menu$(1:10)

    $if %english
    menu$(1) = "Load screen        F1"
    menu$(2) = "Kill file"
    menu$(3) = "Rename file"
    menu$(4) = "Directory"
    menu$(5) = "Change Directory"
    menu$(6) = "OS Shell          F12"
    menu$(7) = "Exit"
    $else
    menu$(1) = "Leer pantalla      F1"
    menu$(2) = "Borrar fichero"
    menu$(3) = "Renombrar fichero"
    menu$(4) = "Directorio"
    menu$(5) = "Cambiar Directorio"
    menu$(6) = "OS Shell          F12"
    menu$(7) = "Salir"
    $endif

     call wopen(2,3,10,27,1,0,wmfgc%,wmbgc%,"")

    do

       ayuda.min% = 1
       ayuda.max% = 1

        call set.wmenu.leftrighton
	menu% = wmenu(menu$(), oldmenu%)

       select case menu%
        case 1
           call wsavewindows
           call ficheros.bload
           call wrestorewindows

          if fichero.bload% then
              call key.esc(2)
             fichero.bload% = 0
          end if

        case 2 : call ficheros.del
        case 3 : call ficheros.ren
        case 4 : call ficheros.dir
        case 5 : call ficheros.chdir
        case 6 : call ficheros.shell
        case 7 : call ficheros.exit
       end select

    loop until menu% = 0

     call wclose

end sub





sub ficheros.bload
   shared fichero.bload%
   shared file$, file.ext$
   local  path$

    path$ = ficheros.curdir$

    nfile$ = wdir$( 3, 20, 1, path$+"*.SCR.ASC.BIN.ANS",&h16 ,0 ,1 )


    if nfile$ <> "" then

        call ficheros.writed

       file$ = nfile$
	call splitpathfn(file$,d$,p$,f$,e$)

       file$     = d$+p$+f$			' nombre sin extensi¢n
       file.ext$ = e$				' extensi¢n

	call editor.scr.rest
	call ficheros.bload.ok( file$, e$ )
        call editor.scr.hold

       fichero.bload% = -1

    end if


end sub





sub ficheros.del
   local file$
   local  path$

    path$ = ficheros.curdir$

    do
       file$ = wdir$( 4, 20, 1, path$+"*.*", &h16,  0, 1 )

       if file$ <> "" then

          $if %english
          sino$ = "¨ Do you want delete the file "+file$+" ?"
          $else
          sino$ = "¨ Quieres borrar el fichero "+file$+" ?"
          $endif

          if wsino%( 0, 0, sino$, e%)  then
             kill file$
              call splitpathfn(file$, d$, mdir$, f$, e$)
             path$ = d$+mdir$
          end if

       end if

    loop until file$=""

end sub





sub ficheros.ren
   local  path$

    path$ = ficheros.curdir$


    do

       file$ = wdir$( 5, 20, 1, path$+"*.*", &h16, 0 ,1 )
       

       if file$ <> "" then
          

          do
              call set.weinput.default(file$)
             newfile$ = weinput$( 0, 0, "", "", 70 , 1, e% )
             newfile$ = trim$( newfile$ )
             if e% <> 0 then exit sub

          loop while newfile$ = ""

          
          name file$ as newfile$
           call splitpathfn(file$, d$, mdir$, f$, e$)
          path$ = d$+mdir$

       end if


    loop until file$ = ""


end sub





sub ficheros.dir
   shared wsmfgc%, wsmbgc%
   local menu$(), menu%
   local  path$
   static oldmenu%

    path$ = ficheros.curdir$


     dim menu$(1:5)


    $if %english
    menu$(1) = "Name"
    menu$(2) = "Extension"
    menu$(3) = "Size"
    menu$(4) = "Date"
    menu$(5) = "Time"
    $else
    menu$(1) = "Nombre "
    menu$(2) = "Extensi¢n"
    menu$(3) = "Kbytes "
    menu$(4) = "Fecha "
    menu$(5) = "HH:MM:SS"
    $endif

     call wopen( 6, 20, 12, 32, 1, 0, wsmfgc%, wsmbgc%, "" )

    do
	menu% = wmenu%(menu$(), oldmenu%)

       if menu% <> 0 then
          m$ = wdir$( 8, 25, 1, path$+"*.*", &h16, menu%-1, 3)
       end if

    loop until menu% = 0

     call wclose

end sub





sub ficheros.chdir
   shared merr%
   local  path$


    do

       path$ = ficheros.curdir$

       merr% = 0
        call set.weinput.default( path$)
       npath$ = weinput$( 0, 0, "", "", 70, 1, e% )
       npath$ = trim$( npath$ )
       if e% <> 0 then exit sub

        call splitpathfn( npath$, d$, mdir$, f$, e$)

       if d$ <> driveletter$(0) and d$ <> "" then chdrive(d$)


       if mdir$ <> "" then
          if mdir$ <> "\" then npath$ = rtrim$(npath$,"\")
          chdir( npath$ )
       end if


    loop while merr% <> 0


end sub





sub ficheros.shell
   shared mprograma$
   shared munidad$
   shared fgc%, bgc%
   local  olddir$, olddri$

    $if %english
    msg$ = "Type EXIT to return to "
    $else
    msg$ = "Teclea EXIT para regresar a "
    $endif

    olddri$ = driveletter$(0)
    olddir$ = curdir$

     call scr.holdcur
     call wsavewindows

    color 7,0
    cls

    print
    print msg$;mprograma$
    print
    print

    shell

     call wrestorewindows

    chdrive(olddri$)

    if olddir$ <> "" then chdir(olddir$)

    color fgc%,bgc%
     call scr.restcur

end sub





sub ficheros.exit			shared

     call main.ini.config.put
     call ficheros.writed

    color 7,0
    cls

    $if %beta

    $if %english
	print mprograma$;" Copyright (c) 1990-1992 Esteve Valent¡ & PsicoSoft"
	print
	print " This is a Shareware product. It is not free software or in the"	print " public domain. You are granted the right to use these for the"	print " 30-day evaluation period. If you find this program of value, please"	print " send the ORDER FORM to the address above for the registration. You"	print " will receive the registration package with the latest version on it,"	print " MORE OF 70 ALPHABETS , mail, fax or Net support and information"
	print " about future program versions."	print
	print " Please support the Shareware concept and you will help the development"	print " of this program."	print
	print " Offer this program at your friends."
	print
	print " Thank You to use ";mprograma$
	print
	print

    $else
	print mprograma$;" Copyright (c) 1990-1992 Esteve Valent¡ y PsicoSoft"
	print
	print " Este es un programa ShareWare. No es ni de  Dominio  P£blico"
	print " ni de uso gratuito. Tienes el derecho de utilizarlo  durante"
	print " un periodo de evaluaci¢n de 30 dias y si este va a serte  de"
	print " utilidad, por favor,  env¡a la   HOJA DE REGISTRO    (Opci¢n"
	print " 'Registrar Programa' del men£ 'Ayuda')."
	print " Recibir s la £ltima versi¢n del programa  registrada  y  con"
	print " m s de 70 ALFABETOS diferentes para tu uso, as¡ como soporte"
	print " via tel‚fono, correo o  BBS  y  informaci¢n  acerca  de  las"
	print " nuevas versiones del programa."
	print
	print " Por favor, cumple con el concepto ShareWare y de esta  forma"
	print " ayudar s al desarrollo de este programa.
	print
	print " Ofrece este programa a tus amigos."
	print
	print " Gracias por utilizar ";mprograma$
	print
	print
    $endif

    delay 1.5

    $endif


    chdrive(inidri$)
    if inidir$ <> "" then chdir(inidir$)

    end:

end sub





sub ficheros.writed
   shared file.save%
   shared file$, file.ext$

    call splitpathfn(file$, d$, md$, f$, e$ )

    $if %english
	text$ = " File "+f$+file.ext$+" not saved. Save ? "
    $else
	text$ = " Fichero "+f$+file.ext$+" no grabado. ¨ Lo grabo ? "
    $endif

    if file.save% then

       if wsino%( 0, 0, text$, e% ) then
           call editor.scr.rest
	   select case file.ext$
	     case ".SCR" : call grabar.gra
	     case ".ANS" : call grabar.ans
	     case ".ASC" : call grabar.asc
	     case ".BIN" : call grabar.bin
	   end select
       end if

    end if

end sub





function ficheros.curdir$
     ficheros.curdir$ = rtrim$(curdir$(""),"\")+"\"
end function





sub ficheros.bload.ok( file$, e$ )
   shared file.save%

    select case e$
     case ".SCR" : call bload.scr(file$+e$)
     case ".BIN" : call bload.binary(file$+e$)
     case ".ANS" : call bload.ansi(file$+e$)
     case else   : call bload.ascii(file$+e$)
    end select

    file.save% = 0

end sub




sub bload.scr(file$)
    shared cfgc%, cbgc%

    call scr.defseg
    bload file$, 0
    cfgc$ = peek$(4000, 2)
    cbgc$ = peek$(4002, 2)
    cfgc% = cvi(cfgc$)
    cbgc% = cvi(cbgc$)

end sub


sub bload.ascii(file$)
   local  file%,y%
   local  mline$
   shared cfgc%, cbgc%

    color cfgc%, cbgc%
    cls
    columns% = scr.columns%

    file% = freefile
    open file$ for input as file%

    do
       incr y%,1
       line input #file%, mline$
       locate y%,1

       if len(mline$) > columns% then
	  print left$(mline$,columns%);
       else
          print mline$;
       end if

    loop until eof(file%) or y% > 24

    close file%
end sub





sub bload.binary(file$)
   shared cfgc%, cbgc%
   local file%
   local lenfile%
   local pant$
     call scr.defseg
    file% = freefile
    open file$ for binary as file%
    lenfile% = lof(file%)
    get$ file%, lenfile%, pant$
    poke$ 0, pant$
    close file%
    cfgc% = 7
    cbgc% = 0

end sub





