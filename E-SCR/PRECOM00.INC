sub main
   local menu$()
   local op%
   shared ayuda.min%, ayuda.max%
     dim menu$(1:10)
    menu$(1) = "Ficheros"
    menu$(2) = "Grabar"
    menu$(3) = "Utilidad"
    menu$(4) = "Bloques"
    menu$(5) = "Config"
    menu$(6) = "Ayuda"
    do
       ayuda.min% = 1
       ayuda.max% = 8
       op% = wlmenu%(menu$())
       select case op%
        case 1 : call ficheros
        case 2 : call grabar
	case 3 : call utilidad
        case 4 : call bloques
        case 5 : call config
        case 6 : call ayuda
	case else
           call editor
       end select
    loop while -1
end sub
sub kbd.speed(mrate%, mdelay%)
    reg 1, &h0305
    reg 2, (mdelay% * 256) + mrate%
     call interrupt &h16
    
    
    
end sub
FUNCTION DriveLetter$( drive% ) 				PUBLIC
    LOCAL d$, d%
     IF drive% = 0 THEN drive% = DriveCode%("")
     d$ = CHR$( drive% + 64 ) + ":"
     IF DriveCode%(d$) <> -1 THEN
        DriveLetter$ = d$
     END IF
END FUNCTION
sub set.ekbdbeepon  					public
   shared ekbdbeep%
    ekbdbeep% = -1
end sub
sub set.ekbdclickon  					public
   shared ekbdclick%
    ekbdclick% = -1
end sub
sub set.ereturnon  					public
   shared ereturn%
    ereturn% = -1
end sub
sub pincel.ini
   shared mchr$()
   local p%, y%, x%
     dim mchr$(4, 4, 3)
    for p% = 1 to 4
       for y% = 1 to 4
          for x% = 1 to 3
             read mchr$(p%, y%, x%)
          next x%
       next y%
    next p%
    data "Ú","Â","¿","Ã","Å","´","À","Á","Ù","³","Ä",""
    data "É","Ë","»","Ì","Î","¹","È","Ê","¼","º","Í",""
    data "Ö","Ò","·","Ç","×","¶","Ó","Ð","½","º","Ä",""
    data "Õ","Ñ","¸","Æ","Ø","µ","Ô","Ï","¾","³","Í",""
    
    
    
end sub
sub ficheros
   shared wmfgc%, wmbgc%
   shared fichero.bload%
   local menu$
   static oldmenu%
   shared ayuda.min%, ayuda.max%
     dim menu$(1:10)
    menu$(1) = "Leer pantalla      F1"
    menu$(2) = "Borrar fichero"
    menu$(3) = "Renombrar fichero"
    menu$(4) = "Directorio"
    menu$(5) = "Cambiar Directorio"
    menu$(6) = "OS Shell          F12"
    menu$(7) = "Salir"
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
sub ficheros.shell
   shared mprograma$
   shared munidad$
   shared fgc%, bgc%
   local  olddir$, olddri$
    msg$ = "Teclea EXIT para regresar a "
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
sub main.ini.config
   shared inidri$, inidir$
   shared munidad$
   shared mcurdir$
   shared file$, file.ext$
    inidri$ = driveletter$(0)
    inidir$ = curdir$
     call main.ini.config.get
    if mcurdir$ <> ficheros.curdir$ then
        call splitpathfn( mcurdir$, d$, mdir$, f$, e$)
       if d$ <> driveletter$(0) and d$ <> "" then chdrive(d$)
       if mdir$ <> "" and mdir$ <> "\" then chdir( rtrim$(mcurdir$,"\") )
    end if
    if command$ <> "" then file$ = command$
    call splitpathfn( file$, d$, md$, f$, e$ )
    if e$ = "" then e$ = ".SCR"
    file.ext$ = e$
    file$     = d$+md$+f$
    if f$ = "NONAME  " then
       call bload.scr( munidad$+"\e-screen\e-scrpre.scr" )
    else
       call ficheros.bload.ok( file$, file.ext$ )
    end if
end sub
sub main.ini.colors
   shared mprograma$
   shared munidad$
   local file$
    file$ = munidad$+"\e-screen\e-scrcol.dat"
    f$ = dir$( file$, &h0)
    if f$ <> "" then
        call config.colors.get
    else
        call config.colors.default.color
        call config.colors.put
    end if
end sub
sub key.esc(x%)
    for x% = 1 to x%
        call kbd.putkey(&h1b, &h01)
    next
end sub
sub errores.display				shared
   local oldwmsgfgc%, oldwmsgbgc%
   local e$
   local d%
    oldwmsgfgc% = wmsgfgc%
    oldwmsgbgc% = wmsgbgc%
    wmsgfgc% = wefgc%
    wmsgbgc% = webgc%
    merr% = err
    inf$ = " Por favor... Informame de que este error ha sucedido "
    select case err
     case 25 : e$ = "Error de dispositivo"
     case 27 : e$ = "No hay papel"
     case 51 : e$ = "Error interno"
     case 52 : e$ = "Numero de fichero no v lido"
     case 53 : e$ = "Fichero no encontrado"
     case 54 : e$ = "Tipo de fichero no v lido"
     case 55 : e$ = "El fichero ya est  abierto"
     case 57 : e$ = "Error de E/S a dispositivo"
     case 58 : e$ = "El fichero ya existe"
     case 61 : e$ = "El disco est  lleno"
     case 64 : e$ = "Nombre de fichero no v lido"
     case 67 : e$ = "Demasiados ficheros"
     case 70 : e$ = "Disco protegido contra grabaci¢n"
     case 71 : e$ = "El disco no est  preparado"
     case 74 : e$ = "Nombrar de nuevo especificando la Unidad"
     case 75 : e$ = "Error de acceso a un camino/fichero"
     case 76 : e$ = "Camino o fichero no v lido"
     case 100: e$ = "El bloque de memoria est  vac¡o"
     case 101: e$ = "El bloque no existe en el disco"
     case 102
      e$ = "Error en la function bload.ansi"
      d% = 1
     case else
       e$ = "Error "+str$(err)+" en la direcci¢n "+str$(eradr)
       d% = 1
    end select
     play "L64 N70"
     call wmsg(0,0,e$)
     if d% = 1 then
	call wmsg(0,0, inf$)
	delay 1
     end if
    wmsgfgc% = oldwmsgfgc%
    wmsgbgc% = oldwmsgbgc%
end sub
sub editor
   shared file.save%		
   shared file$			
   shared mchr$(),mchr%		
   shared mpincel%		
   shared car%()		
   shared fgc%, bgc%		
   shared posy%, posx%		
   shared bloques.abc.num%	
   shared ayuda.min%, ayuda.max%
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
        case chr$(&h00,&h53) 	   		
          dat$ = scr.get.chrattr$(posy%,posx%,80+1-posx%)
          dat$ = mid$(dat$,3,len(dat$)-2) + " "+chr$(7)
          m% = ((posy%-1)*160) + ((posx%-1)*2)
          poke$ m%, dat$
	  file.save% = -1
        case chr$(&h00,&h4b)	    		
	  way$ = "left"
	  decr posx%, 1
        case chr$(&h00,&h4d)   			
	  way$ = "right"
          incr posx%, 1
        case chr$(&h00,&h48) 			
	  way$ = "up"
          decr posy%, 1
        case chr$(&h00,&h50)			
	  way$ = "down"
          incr posy%, 1
        case chr$(&h00,&h4f):posx% = 80		
        case chr$(&h00,&h47):posx% = 1		
        case chr$(&h0,&h049):posy% = 1		
        case chr$(&h0,&h051):posy% = 25		
        case chr$(&h00,&h52)			
        case chr$(&h1b)				
          exit loop
        case chr$(&h00,&h3b) to chr$(&h00,&h44) 
          select case right$(t$,1)
	   case chr$(&h3b): call ficheros.bload		
	   case chr$(&h3c): call grabar.gra		
           case chr$(&h3d): call utilidad.vercolor	
	   case chr$(&h3e): call utilidad.verpos	
           case chr$(&h3f): call utilidad.color		
	   case chr$(&h40)              		
	      call utilidad.ascii
	      file.save% = -1
	   case chr$(&h41)              		
	      call utilidad.pincel
	      way$ = ""
	   case chr$(&h42)				
              call bloques.scr.scr
              call editor.scr.hold
	      file.save% = -1
           case chr$(&h43)				
              call bloques.scr.disk
              call editor.scr.hold
           case chr$(&h44)				
              call bloques.disk.scr
              call editor.scr.hold
	      file.save% = -1
           case else
          end select
	case > ""  				
          t$ = right$(t$,1)
          if mdes% then				
             blq$ = bloques.abc.get$( bloques.abc.num%, t$ )
             if blq$ <> "" then
		 call bloques.scr.put( blq$ )
		 file.save% = -1
	     end if
          elseif malt% then			
	     p$ = chr$( car%( asc(t$) + 20 ) )
          elseif mctl% then			
	     p$ = chr$( car%( asc(t$) ) )
          elseif t$ = chr$(&h08) then		
             if posx% > 1 then
                dat$ = scr.get.chrattr$(posy%, posx%, 80+1-posx%)
                m% = ((posy%-1)*160) + ((posx%-2)*2)
                dat$ = dat$+" "+chr$(7)
                poke$ m%, dat$
                posx% = posx% -1
		file.save% = -1
	     end if
	  elseif t$ = chr$(&h0d) then		
	     incr posy%, 1
	  else
	     p$ = t$
	  end if
       end select
       if p$ <> "" then
	  if mins% then				
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
sub editor.scr.hold
   shared mpantalla$
     call scr.defseg
    mpantalla$ = peek$(0,4000)
     call editor.line.hold
end sub
sub config
   shared wmfgc%, wmbgc%
   local menu$
   static oldmenu%
   shared ayuda.min%, ayuda.max%
     dim menu$(1:5)
    menu$(1) = "Modificar colores"
    menu$(2) = "Modificar macros "
     call wopen(2,50,5,72,1,0,wmfgc%,wmbgc%,"")
    do
       ayuda.min% = 5
       ayuda.max% = 5
        call set.wmenu.leftrighton
	menu% = wmenu(menu$(), oldmenu%)
       select case menu%
        case 1 : call config.colors
	case 2 : call config.macros
       end select
    loop until menu% = 0
     call wclose
end sub
sub ayuda
   shared wmfgc%, wmbgc%
   local menu$(), menu%
   static oldmenu%
   shared ayuda.min%, ayuda.max%
   shared munidad$
     dim menu$(1:10)
     incr y%
	menu$(y%) = "Ayuda General     F11"
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
    fhelp$ = "E-HELPS"
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
        case chr$(&h00,&h49), chr$(&h00,&h48)		
          if x% > ayuda.min% then
             decr x%
          end if
        case chr$(&h00,&h51), chr$(&h00,&h50)		
          if x% < ayuda.max% then
             incr x%
          end if
       end select
    loop while t$ <> chr$(&h1b)
     call wrestorescreen(mpantalla$,1,1,25,80)
end sub
sub scr.defseg
    def seg = scr.get.seg
    
    
    
end sub
function scr.get.chrattr$( posy%, posx%, lend% )
    local o%			
    local b%			
      call scr.defseg      	
     o% = scr.columns% * 2 * (posy% - 1) + 2 * (posx% - 1)
     scr.get.chrattr$ = peek$( o%, lend% * 2)
     
     
     
end function
sub scr.holdcur
   shared scr.cur%()
   shared scr.curp%
    if scr.curp% = 0 then
        redim scr.cur%(1:15,1:2)
    end if
    incr scr.curp%
    scr.cur%(scr.curp%,1) = scrlin
    scr.cur%(scr.curp%,2) = pos(0)
    
    
    
end sub
sub scr.restcur
   shared scr.cur%()
   shared scr.curp%
   local y%, x%
    if scr.curp% >0 then
       y% = scr.cur%(scr.curp%,1)
       x% = scr.cur%(scr.curp%,2)
       if y%>0 and x%>0 then
          locate y%,x%
       else
          locate 1,1
       end if
       decr scr.curp%
    else
       locate 1,1
    end if
    
    
    
end sub
function scr.color2attr%( fgc%, bgc% )
    local temp%
     temp%=(bgc% * 16) + fgc%
     if fgc% > 15 then temp% = temp% + 112
     scr.color2attr% = temp%
end function
function trim$(var$)
     trim$ = ltrim$( rtrim$( var$ ))
     
     
     
end function
sub kbd.putkey(character%, scancode%)
    do
       reg 1, &h0500			
       reg 3, scancode% * 256 + character%
        call interrupt &h16
    loop while (reg(1) mod 256) <> 0	
    
    
    
end sub
sub kbd.flags(mins%, mcap%, mnum%, mdes%, malt%, mctl%, mlsh%, mrsh%)
    reg(1), &h0200
     call interrupt &h16
    mins% = 0
    mcap% = 0
    mnum% = 0
    mdes% = 0
    malt% = 0
    mctl% = 0
    mlsh% = 0
    mrsh% = 0
    if (reg(1) and 1)   <> 0 then mrsh%  = -1	
    if (reg(1) and 2)   <> 0 then mlsh%  = -1	
    if (reg(1) and 4)   <> 0 then mctl%  = -1	
    if (reg(1) and 8)   <> 0 then malt%  = -1	
    if (reg(1) and 16)  <> 0 then mdes%  = -1	
    if (reg(1) and 32)  <> 0 then mnum%  = -1	
    if (reg(1) and 64)  <> 0 then mcap%  = -1	
    if (reg(1) and 128) <> 0 then mins%  = -1	
end sub
SUB SplitPathFN( path$, drive$, mdir$, file$, ext$ ) 		PUBLIC
   LOCAL temp$
   LOCAL i%                          	
   LOCAL j%                          	
   LOCAL k%                          	
   LOCAL l%                          	
    temp$ = UCASE$(path$)             	
    temp$ = LTRIM$(RTRIM$(temp$))
    drive$ = ""
    mdir$  = ""
    file$  = ""
    ext$   = ""
    IF temp$="." OR temp$=".." THEN   		
       mdir$ = temp$
       EXIT SUB
    END IF
    l% = LEN(temp$)+1                 	
    i% = 2                            	
    WHILE i%<=l% AND MID$(temp$,i%-1,1)<>":": i% = i%+1: WEND
    IF MID$(temp$,i%-1,1)<>":" THEN i% = 1
    j% = l%                           	
    WHILE j%>i% AND MID$(temp$,j%-1,1)<>"\": j% = j%-1: WEND
    k% = l%                           	
    WHILE k%>j% AND MID$(temp$,k%,1)<>".": k% = k%-1: WEND
    IF MID$(temp$,k%,1)<>"." THEN k% = l%  	
    drive$ = MID$(temp$,1,i%-1)       	
    mdir$  = MID$(temp$,i%,j%-i%)       	
    file$  = LEFT$(MID$(temp$,j%,k%-j%),8)  	
    if file$ <> "" then			
       file$ = left$(file$+"         ",8)	
    end if
    ext$   = LEFT$(MID$(temp$,k%,l%-k%),4)   	
    if ext$ <> "" then 			
       ext$  =left$(ext$+"    ", 4)		
    end if
END SUB
FUNCTION DriveCode%( drive$ ) 					PUBLIC
    LOCAL d%,dd%,nd%
     REG 1,&h1900 			
      CALL INTERRUPT &H21
     dd% = REG(1) AND &h00FF		
     REG 1,&h0E00			
     REG 4,dd%
      CALL INTERRUPT &H21		
     nd% = REG(1) AND &h00FF		
     drive$ = UCASE$(drive$)
     drive$ = LTRIM$(RTRIM$(drive$))
     IF drive$ = "" THEN
        DriveCode% = dd%+1		
     ELSEIF LEN(drive$)<>2 OR RIGHT$(drive$,1)<>":" THEN
        DriveCode% = -1		
     ELSE
        d% = ASC(LEFT$(drive$,1))-64	
        IF d%<1 or d%>nd% THEN		
           DriveCode% = -1
        ELSE
           DriveCode% = d%
        END IF
     END IF
END FUNCTION
sub wopen (boty%,botx%,topy%,topx%,bord%,type%,mwfgc%,mwbgc%,titulo$)
   local x%
   local y%
   local d$()
   shared wind$()
   shared wnum%
     dim d$(1:10)
     call wtestcolor(mwfgc%, mwbgc%)
    color mwfgc%,mwbgc%
    incr wnum%,1
    locate ,,0
    wind$(wnum%,2)=  mki$(botx%)+_
    mki$(boty%)+_
    mki$(topx%)+_
    mki$(topy%)+_
    mki$(type%)+_
    mki$(mwfgc%)+_
    mki$(mwbgc%)+_
    mki$(1)+mki$(1)+_
    mki$(csrlin)+_
    mki$(pos(0))
     call wsavescreen(wind$(wnum%,1), boty%, botx%, topy%, topx%)
    select case bord%
     case  1
       d$(1)="Ú":d$(2)="Ä":d$(3)="¿":d$(4)="³":d$(5)="À":d$(6)="Ù":d$(7)="Ã":d$(8)="´"
     case  2
       d$(1)="É":d$(2)="Í":d$(3)="»":d$(4)="º":d$(5)="È":d$(6)="¼":d$(7)="Ì":d$(8)="¹"
     case  3
       d$(1)="Õ":d$(2)="Í":d$(3)="¸":d$(4)="³":d$(5)="Ô":d$(6)="¾":d$(7)="Æ":d$(8)="µ"
     case  4
       d$(1)="Ö":d$(2)="Ä":d$(3)="·":d$(4)="º":d$(5)="Ó":d$(6)="½":d$(7)="Ç":d$(8)="¶"
     case else
       d$(1)=" ":d$(2)=" ":d$(3)=" ":d$(4)=" ":d$(5)=" ":d$(6)=" ":d$(7)=" ":d$(8)=" "
    end select
    select case type%
       locate boty%, botx%
       print d$(1); string$(topx%-botx%-1,d$(2));d$(3);
     case 1, 2
       locate boty%+1, botx%
       print d$(4);space$(topx% - botx% - 1  );d$(4);
       locate boty%+2, botx%
       print d$(7);string$(topx% - botx% - 1 ,d$(2));d$(8);
       for y% = boty% + 3 to topy% - 3
          locate y%, botx%
          print d$(4);space$( topx% - botx% -1 );d$(4);
       next y%
       locate y%, botx%
       print d$(7);string$(topx%-botx%-1,d$(2));d$(8);
       locate y%+1,botx%
       print d$(4);space$(topx% - botx% - 1);d$(4);
       if  len(titulo$) < topx%-2-botx% then
          locate boty%+1,botx%+int(((topx%-botx%)-len(titulo$))/2)
          print titulo$;
       end if
       if type% = 1 and topx%-botx% > 9 then
          locate y%+1,botx%+int((topx%-5-botx%)/4)
          print "ESC";
          locate y%+1,botx%+int((topx%-1-botx%)/2)
          print chr$(24);" ";chr$(25);
          locate y%+1,topx%-int((topx%+5-botx%)/4)
          print chr$(17);chr$(196);chr$(217);
       end if
       if type% = 2 and topx%-botx% > 13 then
          locate y%+1,botx%+int((topx%-5-botx%)/4)
          print  "ESC";
          locate y%+1,botx%+int((topx%-1-botx%)/2)
          print "PUP";
          locate y%+1,topx%-int((topx%+5-botx%)/4)
          print "PDW";
       end if
     case else
       if titulo$<>"" and len(titulo$) < topx%-4-botx% then
          locate boty%,botx%+int(((topx%-botx%-1)-len(titulo$))/2)
          print ;chr$(17);
          color mwbgc%,mwfgc%
          print titulo$;
          color mwfgc%,mwbgc%
          print chr$(16);
       end if
       for y% = boty% + 1 to topy% - 1
          locate y%, botx%
          print d$(4);space$(topx% - botx% - 1);d$(4);
       next y%
    end select
    locate topy%, botx%
    print d$(5); string$(topx% - botx% - 1,d$(2));d$(6);
    
    
    
end sub
function wlmenu%(menu$())
    shared wlmfgc%		
    shared wlmbgc%		
    local lastmenu%		
    local fastmenu$		
    local part%			
    local x%, columns%
    static p%
    static linea$
     if p% = 0 then p% = 1
     columns% = scr.columns%
     if linea$ = "" then
        linea$ = scr.chrattr$(1,1,columns%)	
     end if
     locate 1,1
     color wlmfgc%, wlmbgc%
     print space$(columns%);			
     do while menu$(lastmenu%+1) <> ""
        incr lastmenu%                            
        fastmenu$ = fastmenu$ + left$(menu$(lastmenu%),1)	
     loop
     part% = scr.columns% \ lastmenu%
     do
        t$ = inkey$
        for x% = 1 to lastmenu%
           if x% = p% then
              color wlmbgc%, wlmfgc%
           else
              color wlmfgc%, wlmbgc%
           end if
           locate 1, 5+ (part% *( x%-1))
           print menu$(x%);
        next
        select case t$
         case chr$(&h00,&h50), chr$(&h0d)	
           wlmenu% = p%
           exit function
         case chr$(&h00,&h4b)			
           if p% > 1 then
              decr p%
           else
              p% = lastmenu%
           end if
         case chr$(&h00,&h4d)			
           if p% < lastmenu% then
              incr p%
           else
              p% = 1
           end if
         case chr$(&h1b)			
           wlmenu% = 0
            call scr.defseg
           poke$ 0, linea$
           linea$ = ""
           exit function
         case > ""				
           x% = instr(fastmenu$, ucase$(t$))
           if x% > 0 then
              p% = x%
	       call kbd.putkey(&h0d, &h1c)
           end if
        end select
     loop while -1
end function
sub wclose
   shared wind$(),wnum%
    if wnum% > 0 then
       botx% = cvi(mid$(wind$(wnum%,2),1,2))
       boty% = cvi(mid$(wind$(wnum%,2),3,2))
       topx% = cvi(mid$(wind$(wnum%,2),5,2))
       topy% = cvi(mid$(wind$(wnum%,2),7,2))
        call wrestorescreen(wind$(wnum%,1), boty%, botx%, topy%, topx%)
       wind$(wnum%,1)=""
       wind$(wnum%,2)=""
       decr wnum%,1
    else
       ? "error"
    end if
    
    
    
end sub
sub set.wmenu.leftrighton
   shared wmenu.leftright%
    wmenu.leftright% = -1
end sub
sub wmsg( boty%, botx%, text$ )
   shared wmsgfgc%
   shared wmsgbgc%
     call wtestcolor(wmsgfgc%,wmsgbgc%)
    by% = boty%+12
    bx% = (40-(len(text$)/2))-2
    ty% = boty%+14
    tx% = (40+(len(text$)/2))+2
     call wopen( by%, bx%, ty%,tx%, 3, 0, wmsgfgc%, wmsgbgc%, "" )
     call wprint(1,0,text$)
    delay .5
     call wclose
end sub
sub wsavewindows
   shared wnum%,wind$()
    incr wnum%,1
     call wsavescreen(wind$(wnum%,1),1,1,25,80)
end sub
sub wrestorewindows
   shared wnum%,wind$()
     call wrestorescreen(wind$(wnum%,1),1,1,25,80)
    wind$(wnum%,1) = ""
    wind$(wnum%,2) = ""
    decr wnum%,1
end sub
sub wsavescreen(var$, boty%, botx%, topy%, topx% )
   local y%, l%
     call scr.defseg
    l% = (topx% + 1 - botx%) * 2
    for y% = boty% to topy%
       m% = (160 * (y%-1)) + (2 * (botx%-1))
       var$ = var$ + peek$(m%, l%)
    next y%
end sub
sub wrestorescreen(var$, boty%, botx%, topy%, topx% )
     call scr.defseg
    p% = 1
    l% = (topx% + 1 - botx%) * 2
    for y% = boty% to topy%
       m% = (160 * (y%-1)) + (2 * (botx%-1))
       poke$ m%, mid$(var$, p%, l%)
       incr p%, l%
    next y%
end sub
sub bload.ansi (file$)
    shared cfgc%, cbgc%
    ansi$ = dsk.get.file$(file$)		
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
     replace "ú[" with esc$+"[" in ansi$
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
	case "A"                 
          n% = val(num$)
          if n% = 0 then n% = 1
	  y% = y% - n%
	case "B"                 
          n% = val(num$)
          if n% = 0 then n% = 1
	  y% = y% + n%
	case "C"                 
          n% = val(num$)
          if n% = 0 then n% = 1
	  x% = x% + n%
	case "D"                 
          n% = val(num$)
          if n% = 0 then n% = 1
	  x% = x% - n%
	case "f", "H", "R"   	
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
	case "h", "l"  		
          scr% = val(num$)
          select case scr%
	   case 0 		
             screen 0
             width 40
             columns% = 40
	   case 1 		
             screen 0
             width 40
             columns% = 40
	   case 2:screen 0: width 80	
	   case 3:screen 0: width 80 	
	   case 4:screen 1 		
	   case 5:screen 1		
	   case 6:screen 2		
	   case 7:scroll%=-1: screen 0	
	   case else: screen 0		
          end select
	case "J"  		
          if fgc% <> 0 or bgc% <> 0 then
	     cfgc% = fgc%
	     cbgc% = bgc%
	  else
	     cfgc% = 7
	     cbgc% = 0
          end if
	  color cfgc%, cbgc%
	  cls
        case "K"                 
	  color cfgc%, cbgc%
	  print space$(80 - x%);
          color fgc%, bgc%
          locate y%, x%
	case "m"                 
	  call scr.ansi2color.fn( num$, fgc%, bgc% )
	case "n", "s"            
	  oldy% = y%
	  oldx% = x%
	case "u"                 
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
sub pincel.putchr(ypos%, xpos%, way$)
   shared mchr$()
   shared mchr%, mchr.chr$
   shared fgc%, bgc%
   select case mchr%
     case 5
       p% = 255
     case 6
       p% = asc(mchr.chr$)
     case else
     call scr.defseg
    up%    = pincel.getchr%(ypos% - 1, xpos%, "up")
    down%  = pincel.getchr%(ypos% + 1, xpos%, "down")
    left%  = pincel.getchr%(ypos%, xpos% - 1, "left")
    right% = pincel.getchr%(ypos%, xpos% + 1, "right")
    select case way$
     case "up"    : up%    = -1
     case "down"  : down%  = -1
     case "left"  : left%  = -1
     case "right" : right% = -1
    end select
    
    if up% and left% and down% and right% then    y% = 2: x% = 2: goto mputchr
    if up% and left% and down% then               y% = 2: x% = 3: goto mputchr
    if left% and down% and right% then            y% = 1: x% = 2: goto mputchr
    if down% and right% and up% then              y% = 2: x% = 1: goto mputchr
    if right% and up% and left% then              y% = 3: x% = 2: goto mputchr
    if up% and left% then                         y% = 3: x% = 3: goto mputchr
    if left% and down% then                       y% = 1: x% = 3: goto mputchr
    if down% and right% then                      y% = 1: x% = 1: goto mputchr
    if right% and up% then                        y% = 3: x% = 1: goto mputchr
    if (left% and right%) or left% or right% then y% = 4: x% = 2: goto mputchr
    if (up% and down%) or up% or down% then       y% = 4: x% = 1: goto mputchr
    
    mputchr:
    p% = asc( mchr$(mchr%, y%, x%) )
   end select
   if kbd.ins% then				
      dat$ = scr.get.chrattr$(ypos%, xpos%, 80+1-xpos%)
      dat$ = left$(dat$,len(dat$)-2)
      m% = ((ypos%-1)*160) + (xpos% * 2)
      poke$ m%, dat$
   end if
   m% = (160*(ypos%-1)) + (2*(xpos%-1))
   if p% <> 255 then poke m%, p%
   attr% = scr.color2attr%(fgc%,bgc%)
   if attr% then poke m%+1, attr%
    
    
    
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
       file$     = d$+p$+f$			
       file.ext$ = e$				
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
          sino$ = "¨ Quieres borrar el fichero "+file$+" ?"
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
    menu$(1) = "Nombre "
    menu$(2) = "Extensi¢n"
    menu$(3) = "Kbytes "
    menu$(4) = "Fecha "
    menu$(5) = "HH:MM:SS"
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
sub ficheros.exit			shared
     call main.ini.config.put
     call ficheros.writed
    color 7,0
    cls
    chdrive(inidri$)
    if inidir$ <> "" then chdir(inidir$)
    end:
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
sub main.ini.config.get				shared
   local file%
   local m$, x%, p%
    file% = main.ini.config.open%
    get #file%, 1
    mpos%    = cvi(cmpos$)
    mcolor%  = cvi(cmcolor$)
    mpincel% = cvi(cmpincel$)
    mchr%    = cvi(cmchr$)
    fgc%     = cvi(cfgc$)
    bgc%     = cvi(cbgc$)
    bfgc%    = cvi(cbfgc$)
    bbgc%    = cvi(cbbgc$)
    cfgc%    = cvi(ccfgc$)
    cbgc%    = cvi(ccbgc$)
    cbfgc%   = cvi(cbfgc$)
    cbbgc%   = cvi(cbbgc$)
    bloques.abc.num% = cvi(cbloques.abc.num$)
    mcurdir$ = trim$(ccurdir$)
    file$    = trim$(cfile$)
    ansi.ini%= cvi(cansi.ini$)
    ansi.len%= cvi(cansi.len$)
    ansi.lin%= cvi(cansi.lin$)
    m$       = cmacro$
    close #file%
    for x% = 1 to 499 step 50
	macro$(p%) = mid$(m$,x%,50)
	incr p%
    next
end sub
sub editor.line.see( mins%, mcap%, mnum%, mdes%) 		shared
   local valor%
    color wmsgfgc%,wmsgbgc%
    locate ,,0
    if mpincel% then
       locate 25,1
       select case mchr%
        case 1 : print "ÚÂ¿ÃÅ´ÀÁÙ³Ä ";
        case 2 : print "ÉË»ÌÎ¹ÈÊ¼ºÍ ";
        case 3 : print "ÖÒ·Ç×¶ÓÐ½ºÄ ";
        case 4 : print "ÕÑ¸ÆØµÔÏ¾³Í ";
	case 5
		 print " Solo color ";
	case 6
		 print " Caracter ";mchr.chr$;
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
sub editor.scr.rest
   shared mpantalla$
     call scr.defseg
    poke$ 0, mpantalla$
     call editor.line.rest
end sub
sub grabar
   shared mprograma$
   shared munidad$
   shared wmfgc%, wmbgc%
   shared file$
   local menu$(), menu%
   static oldmenu%
   shared ayuda.min%, ayuda.max%
     dim menu$(1:15)
       menu$(1) = "G Gr fico  BSAVE    F2"
       menu$(2) = "A Texto ASCII"
       menu$(3) = "I Binary"
       menu$(4) = "N Ansi"
       menu$(5) = "L Basic lineal"
       menu$(6) = "B Basic Estructurado"
       menu$(7) = "D Clipper, dBASE"
       menu$(8) = "P Pascal"
       menu$(9) = "S Assembler"
       menu$(10)= "C C, C++"
       menu$(11)= "O Cobol"
     call wopen(2,15,14,40,1,0,wmfgc%,wmbgc%,"")
    do
       ayuda.min% = 2
       ayuda.max% = 2
        call set.wmenu.leftrighton
       menu% = wmenu(menu$(), oldmenu%)
       if menu% <> 0 then
           call wsavewindows
           call editor.scr.rest
          if not mgetscr% then
              call readscreen
             mgetscr% = -1
          end if
          select case menu%
           case 1 : call grabar.gra
           case 2 : call grabar.asc
           case 3 : call grabar.bin
           case 4 : call grabar.ans
           case 5 : call grabar.gwb
           case 6 : call grabar.tub
           case 7 : call grabar.cdb
           case 8 : call grabar.pas
           case 9 : call grabar.asm
           case 10: call grabar.ccc
           case 11: call grabar.cob
          end select
           call wrestorewindows
       end if
    loop until menu% = 0
     call wclose
end sub
sub grabar.gra
   shared file$, file.ext$
   shared cfgc%, cbgc%
   shared file.save%
    fichero$ = file$ + ".SCR"
       msg$ = " Grabando pantalla grafica ( BSAVE ) "
    if grabar.overwrite%(fichero$) then
        call wmsg(0, 0, msg$)
        call scr.defseg
       col$ = mki$(cfgc%)+mki$(cbgc%)
       poke$ 4000, col$
       bsave fichero$, 0, 4004
       if file.ext$ = ".SCR" then  file.save% = 0
    end if
end sub
sub bloques
   shared file.save%
   shared wmfgc%, wmbgc%
   local menu$()
   static oldmenu%
   shared ayuda.min%, ayuda.max%
     dim menu$(1:10)
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
sub bloques.abc
   shared wmsgfgc%, wmsgbgc%
   shared munidad$
   shared bloques.abc.num%
       text$   = "Quieres seleccionar este alfabeto ? "
       titulo$ = " ALFABETO : "
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
        case chr$(&h00,&h48), chr$(&h00,&h49)	
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
        case chr$(&h00,&h50), chr$(&h00,&h51)	
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
        case chr$(&h0d)			
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
          sino$ = "¨ Modificar los colores originales del Bloque ?"
       if wsino%( 10, 0, sino$, e%) then
	   call bloques.scr.color.ok( blq$ )
       end if
       posx% = posx% + x2% + 2 - x1%
    end if
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
sub utilidad
   shared wmfgc%, wmbgc%
   local menu$()
   static oldmenu%
   shared ayuda.min%, ayuda.max%
   shared fascii%
     dim menu$(1:10)
    menu$(1) = "Seleccionar color   F5"
    menu$(2) = "Entrar ASCII        F6"
    menu$(3) = "Pincel cursor       F7"
    menu$(4) = "Mezclar pantalla"
    menu$(5) = "Imprimir pantalla"
    menu$(6) = "Ver color posici¢n  F3"
    menu$(7) = "Ver posici¢n cursor F4"
    menu$(8) = "Borrar pantalla"
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
