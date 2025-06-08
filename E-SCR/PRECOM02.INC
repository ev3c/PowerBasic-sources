sub grabar.asc
   shared file$, file.ext$
   local y%
    fichero$ = file$ + ".ASC"
       msg$ = " Grabando pantalla ASCII "
    if grabar.overwrite%(fichero$) then
        call wmsg(0, 0, msg$)
       open fichero$ for output as #10
       for y% = 1 to 25
          print #10, rtrim$(scr.get.text$(y%, 1, 80))
       next y%
       close #10
       if file.ext$ = ".ASC" then  file.save% = 0
    end if
end sub
sub grabar.bin
   shared file$, file.ext$
   shared mpantalla$
    fichero$ = file$ + ".BIN"
       msg$ = " Grabando pantalla BINARY "
    if grabar.overwrite%(fichero$) then
        call grabar.openmsg(msg$)
       open "R", #10, fichero$, 4000
       field #10, 4000 as dato$
       lset dato$ = mpantalla$
       put #10, 1
       close #10
       if file.ext$ = ".BIN" then  file.save% = 0
        call wclose
    end if
end sub
sub grabar.asm
   shared file$
    fichero$ = file$ + ".ASM"
    if grabar.overwrite%(fichero$) then
        call grabar.open( fichero$, f$, ".ASM")
        call grabar.write("ASM2.GEN", f$, ".ASM")
        call grabar.close
    end if
end sub
sub grabar.ccc
   shared file$
   static opccc%
    fichero$ = file$ + ".C"
     dim menu$(1:2)
       menu$(1) = "A - C¢digo fuente tipo gotoxy(x,y);cprintf.. ( C1.GEN )"
       menu$(2) = "B - C¢digo fuente tipo volcado de memoria    ( C2.GEN )"
    menu% = wsmenu%(0,0, menu$(), opccc% )
    if menu% then
       if grabar.overwrite%(fichero$) then
           call grabar.open(fichero$, f$, ".C")
          select case menu%
           case 1
	      fgen$ = "C1.GEN"		
           case 2
	      fgen$ = "C2.GEN"
          end select
	      call grabar.write( fgen$,f$,".C")
           call grabar.close
       end if
    end if
end sub
sub grabar.cdb
   shared file$
   static opcdb%
     dim menu$(1:3)
       menu$(1) = "A - C¢digo fuente para dBASE III, FoxBase    ( DBASE1  .GEN )"
       menu$(2) = "B - C¢digo fuente para Clipper Summer 87     ( CLIPS871.GEN )"
       menu$(3) = "C - C¢digo fuente para Clipper v5.x          ( CLIP50X1.GEN )"
    menu% = wsmenu%( 0, 0, menu$(), opcdb% )
    select case menu%
     case 1
       e$ = ".PRG"
       ifile$ = "dbase1.gen"
     case 2
       e$ = ".CSH"
       ifile$ = "clips871.gen"
     case 3
       e$ = ".CH"
       ifile$ = "clip50x1.gen"
     case else
       exit sub
    end select
    fichero$ = file$ + e$
    if grabar.overwrite%(fichero$) then
        call grabar.open( fichero$, f$, e$ )
        call grabar.write(ifile$, f$, e$ )
        call grabar.close
    end if
end sub
sub grabar.cob
   shared file$
   static opcob%
     dim menu$(1:2)
       menu$(1) = "R - C¢digo fuente para RMCobol          ( RMCOBOL1.GEN )"
       menu$(2) = "M - C¢digo fuente para MSCobol          ( MSCOBOL1.GEN )"
    menu% = wsmenu%(0,0,menu$(), opcob%)
    select case menu%
     case 1
       e$ = ".CBL"
       ifile$ = "rmcobol1.gen"
     case 2
       e$ = ".COB"
       ifile$ = "mscobol1.gen"
     case else
       exit sub
    end select
    fichero$ = file$ + e$
    if grabar.overwrite%(fichero$) then
        call grabar.open( fichero$, f$, e$ )
        call grabar.write(ifile$, f$, e$ )
        call grabar.close
    end if
end sub
sub grabar.gwb
   shared file$
   shared mprograma$
   shared cfgc%, cbgc%
   shared p%()
    coma$ = chr$(34)
    fichero$ = file$ + ".GWB"
       ltext$ = "Indica el primer n£mero de l¡nea ( 0 = sin n£mero l¡nea ) ...: "
       msg$   = " Grabando pantalla BASIC LINEAL "
    if grabar.overwrite%(fichero$) then
        call set.weinput.default( "1000" )
       bas.lin = val( weinput$( 0, 0, ltext$, "", 4, 10, e%) )
       if e% <> 0 then exit sub
        call grabar.openmsg( msg$ )
        call splitpathfn(fichero$, d$, mdir$, f$, me$ )
       open fichero$ for output as #10        
       nlin$ = grabar.gwb.lin$(bas.lin)
       print #10, nlin$ + " REM  " + mprograma$ + "   por  Esteve Valent¡ & Psicosoft"
       nlin$ = grabar.gwb.lin$(bas.lin)
       print #10, nlin$ + " REM   Nombre de la Pantalla ...: " + f$
       nlin$ = grabar.gwb.lin$(bas.lin)
       cfgc$ = str.int2chr$(cfgc%)
       cbgc$ = str.int2chr$(cbgc%)
       print #10, nlin$ + " COLOR "+cfgc$+","+cbgc$
       nlin$ = grabar.gwb.lin$(bas.lin)
       print #10, nlin$ + " CLS"
       cattr% = scr.color2attr%( cfgc%, cbgc% )
       for y% = 1 to 25
          oldx%  = -1
          oldax% = 0
          dato$  = ""
          for x% = 1 to 80
             m% = ((y%-1)*160)+((x%-1)*2)
             car$  = chr$(p%(m%))
             attr% = p%(m%+1)
             if car$ <> " " or dato$ <> "" or_
                (car$ = " " and scr.attr2bgc%(attr%)<>cbgc%) then
                if oldx% = -1 then
                   oldx% = x%
                end if
                dato$ = dato$ + car$
                if car$ = " " and attr% = cattr% then
                   mcar$ = mcar$ + car$
                else
                   mcar$ = ""
                end if
                if attr% <> p%(m%+3) or_
                   x% = 80 or len(mcar$) > 5 then
                   if len(mcar$) > 5 then
                      dato$ = rtrim$(dato$, " ")
                      mcar$ = ""
                   end if
                   if dato$ <> "" then
                      
                      
                      
                      mfgc$ = fgc$
                      mbgc$ = bgc$
                      fgc$  = str.int2chr$(scr.attr2fgc%(attr%))
                      bgc$  = str.int2chr$(scr.attr2bgc%(attr%))
                      if mfgc$ <> fgc$ or mbgc$ <> bgc$ then
                         nlin$ = grabar.gwb.lin$(bas.lin)
                         print #10, nlin$+" COLOR "+fgc$+","+bgc$
                      end if
                      
                      xpos% = oldx% - oldax%		
		      xpos$ = ""			
                      if xpos% > 0 then			
                         nlin$ = grabar.gwb.lin$(bas.lin)
                         y$ = str.int2chr$(y%)
                         x$ = str.int2chr$(oldx%)
                         print #10, nlin$+" LOCATE "+y$+","+x$
                      end if
		      oldax% = oldx% + len(dato$)	
                      nlin$ = grabar.gwb.lin$(bas.lin)
                      print #10, nlin$+ " PRINT "+coma$+dato$+coma$+";"
                      oldx% = -1
                      dato$ = ""
                   end if
                end if
             else
                oldx%  = -1
                dato$  = ""
             end if
          next x%
       next y%
       print #10, chr$(&h1a)
       close #10
        call wclose
    end if
end sub
sub grabar.pas
   shared file$
   static oppas%
    fichero$ = file$ + ".PAS"
     dim menu$(1:2)
       menu$(1) = "A - C¢digo fuente tipo gotoxy(x,y)...     (PASCAL1.GEN)"
       menu$(2) = "B - C¢digo fuente tipo volcado de memoria (PASCAL2.GEN)"
    menu% = wsmenu%(0,0,menu$(),oppas%)
    if menu% = 0 then exit sub
    if grabar.overwrite%(fichero$) then
        call grabar.open(fichero$, f$, ".PAS")
       select case menu%
	case 1 : fgen$ = "pascal1.gen"
	case 2 : fgen$ = "pascal2.gen"
       end select
       call grabar.write( fgen$ ,f$,".PAS")
        call grabar.close
    end if
end sub
sub grabar.tub
   shared file$
    fichero$ = file$ + ".TUB"
    if grabar.overwrite%(fichero$) then
        call grabar.open(fichero$, f$, ".TUB")
        call grabar.write("tb&qb1.gen",f$,".TUB")
        call grabar.close
    end if
end sub
sub readscreen
   local x%
   shared p%()
    for x% = 0 to 4000
       p%(x%) = peek(x%)
    next x%
end sub
sub bloques.scr.mem
    blq$ = bloques.scr.get$
    if blq$ <> "" then
        call bloques.mem.put( blq$ )
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
              sino$ = "¨ Borro la parte de la pantalla correspondiente al Bloque ?"
           sino% = wsino%( 0, 0, sino$, e% )
           if e% = 0 then
              if sino% then 	  		
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
       menu$(1) = "Todos los colores"
       menu$(2) = "Color a color"
       msg$ = "Cambiar el color seleccionado por..."
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
sub bloques.disk.put(file$,blq$)
   local p%
   local file%
    file% = freefile
    open file$ for binary as file%
    put$ #file%, blq$
    close #file%
end sub
function bloques.overwrite%(file$)
    local sino$
     bloques.overwrite% = -1
     f$ = dir$( file$, &h00)
     if f$ <> "" then
           sino$ = "El bloque "+f$+" ya existe. ¨ Quieres grabar encima ?"
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
sub cursor( y1%, x1%, y2%, x2%, mode%, blq$)
   shared posx%,posy%
   shared woattrib%
   shared e%
   shared wmessagefgc%,wmessagebgc%
    if mode% = 0 then
     call scr.holdcur
     call wopen(22,15,25,65,1,0,wmessagefgc%,wmessagebgc%,"")
         call wprint(1,0,"Selecciona con las teclas del cursor la esquina ")
         call wprint(2,0,"superior izquierda  Ú  del bloque y pulsa INTRO." )
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
        case chr$(&h00,&h4b): decr posx%, 1    	   	
        case chr$(&h00,&h4d): incr posx%, 1     	
        case chr$(&h00,&h48): decr posy%, 1		
        case chr$(&h00,&h50): incr posy%, 1   		
        case chr$(&h00,&h47): posx% = 200 	       	
        case chr$(&h00,&h4f): posx% = -5	       	
        case chr$(&h00,&h49): posy% = 100		
        case chr$(&h00,&h51): posy% = -5		
        case chr$(&h1b)					
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
       sino$ = "¨ Quieres mezclar esta pantalla ?"
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
    menu$(1) = " Texto pantalla "
    menu$(2) = " Colores pantalla"
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
sub utilidad.cls    			shared
    local msg$
	msg$ = " ¨ Borrar pantalla ? "
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
function scr.get.text$ ( posy%, posx%, lend% )
    local text$
    local t$
    local x%
     text$ = scr.get.chrattr$(posy%, posx%, lend%)
     for x% = 1 to len(text$) step 2
        t$ = t$ + mid$(text$,x%,1)
     next x%
     scr.get.text$ = t$
     
     
     
end function
sub scr.attr2ansi.fn( attr%, afgc%, abgc%, inten%, blink% )
    afgc% = attr% mod 8			
    select case afgc%			
     case 1 : afgc% = 4
     case 3 : afgc% = 6
     case 4 : afgc% = 1
     case 6 : afgc% = 3
    end select
    abgc% = attr% \ 16			
    abgc% = abgc% mod 8
    select case abgc%			
     case 1 : abgc% = 4
     case 3 : abgc% = 6
     case 4 : abgc% = 1
     case 6 : abgc% = 3
    end select
    inten% = 0
    blink% = 0
    if (attr% and 8)   <> 0 then inten% = -1	
    if (attr% and 128) <> 0 then blink% = -1	
end sub
function rinstr( iv%, var$, s$ )
    local p%
    local lvar%
    local ls%
     lvar% = len(var$)		
     ls% = len(s$)		
     if iv% > 0 then lvar% = iv%
     if lvar% > 0 and ls% > 0 then
        do
           p% = instr(c%+1, left$(var$,lvar%)+s$, s$)
           if p% < lvar% then
              c% = p%
           else
              if c% > 0 then rinstr = c%
              exit loop
           end if
        loop while -1
     end if
     
     
     
end function
function str.int2chr$( num% )
     str.int2chr$ = ltrim$(str$( num% ))
end function
SUB FindFile(search$,sattrib%,f$,fattrib%,size&,date%,time%,e%)	PUBLIC
   STATIC dta%()  			
     DIM dta%(0:31)                    
   LOCAL dtas%, dtao%
   STATIC buf$                       
    dtas = VARSEG(dta%(0))
    dtao = VARPTR(dta%(0))
    REG 1,&h1A00                    	
    REG 8,dtas	            	
    REG 4,dtao
     CALL INTERRUPT &H21
    IF search$<>"" THEN               
       buf$ = search$+chr$(0)         
       DEF SEG                        
       REG 1,&h4E00                 	
       REG 3,sattrib%               	
       REG 8,STRSEG(buf$)            	
       REG 4,STRPTR(buf$)
    ELSE                              
       REG 1,&h4F00                 	
    END IF
     CALL INTERRUPT &H21
    e% = REG(1)                     	
    IF e%=0 THEN                      	
       DEF SEG = dtas
       f$ = PEEK$(dtao+30,13)                
       f$ = LEFT$(f$,INSTR(f$,CHR$(0))-1)	
       fattrib% = dta%(10) \ 256              
       size& = CLNG( PEEKL(dtao+26) )	
       date% = dta%(12)			
       time% = dta%(11)			
       DEF SEG
    END IF
END SUB
SUB ReadDir( search$, sa%, dire$(1), lend%, e% ) 		PUBLIC
   LOCAL f$,fa%,s&,d%,t%		
   LOCAL file$,ext$,size$		
   LOCAL k%			
   local mext$()
     dim mext$(1:25)
    p% = LBOUND( dire$(1) )			
    search$ = ucase$(search$)
    if right$(search$,1) <> "*" then		
       mask$ = left$(search$,instr(search$,".")-1) + ".*"
       i% = 4
       mext$ = left$(right$(search$,i%),4)	
       do
          incr ext%, 1
          mext$(ext%) = rtrim$(mext$)
          incr i%,4
          mext$ = left$(right$(search$,i%),4)		
       loop while left$(mext$,1) = "."			
       mtest% = -1
    else
       mask$ = search$
    end if
     CALL FindFile (mask$,sa%,f$,fa%,s&,d%,t%,e%)
    WHILE e% = 0	 				
       hold% = -1
       if mtest% and fa% <> 16 then
          hold% = 0
          for test% = 1 to ext%
             if instr(f$, mext$(test%)) > 0 then
                hold% = -1
                exit for
             end if
          next
       end if
       if hold% then
          k% = INSTR( f$, "." )			
          select case k%
           case 1
             file$ = left$(f$+"               ",12)
           case > 0
             file$ = LEFT$(LEFT$(f$,k%-1)+"        ",8)
             ext$  = LEFT$(MID$(f$,k%,LEN(f$)+1-k%)+"   ",4)
           case else
             file$ = LEFT$( f$+"        ", 8)
             if fa% = 16 then				
                ext$ = "    "
             else					
                ext$ = ".   "
             end if
          end select
          IF fa% = 16 THEN				
             size$ = "<DIR>    "
          ELSE                                        
             size$ = STR$(s&)
             size$ = RIGHT$("         "+ size$, 9 )
          END IF
          dire$(p%) = file$+ext$+" "			
          if lend%>0 then dire$(p%)=dire$(p%)+size$+"  "		
          if lend%>1 then dire$(p%)=dire$(p%)+fmtdate$(d%)+"  "	
          if lend%>2 then dire$(p%)=dire$(p%)+fmttime$(t%)		
          dire$(p%) = dire$(p%)+"     "+mki$(fa%)	
          INCR p%					
       end if
        CALL FindFile ("",sa%,f$,fa%,s&,d%,t%,e%)	
    WEND 						
    dire$(p%) = ""				
    IF e% = 18 THEN e% = 0 			
    
END SUB
function wsmenu%( mboty%, mbotx%, menu$(1), ip% )
   shared wsmfgc%, wsmbgc%
   col% = len(menu$(p%))
   do while menu$(p%+1) <> ""
      incr p%, 1
      if col% < len(menu$(p%)) then
	 col% = len(menu$(p%))
      end if
   loop
   boty% = 12 + mboty% - (p% \ 2 )
   botx% = 40 + mbotx% - ((col% \ 2)+2)
   topy% = boty% + p% + 1
   topx% = botx% + col% + 2
   call wopen( boty%, botx%, topy%, topx%, 1, 0, wsmfgc%, wsmbgc%, "" )
   wsmenu% = wmenu%( menu$(), ip% )
   call wclose
end function
sub wmessage( boty%, botx%, text$ )
   shared wmessagefgc%,wmessagebgc%
    text$ = text$ + " Pulsa INTRO"
     call wtestcolor(wmessagefgc%,wmessagebgc%)
    by% = boty%+12
    bx% = (40-(len(text$)/2))-2
    ty% = boty%+14
    tx% = (40+(len(text$)/2))+2
     call wopen( by%, bx%, ty%, tx%, 2, 0, wmessagefgc%, wmessagebgc%, "" )
     call wprint(1,0,text$)
    do
       a$ = ucase$(inkey$)
       
    loop until a$ = chr$(&h0d)
     call wclose
    
    
    
end sub
sub wcolor( boty%, botx%, wfgc%, wbgc% )
   shared e%
   shared wcolorfgc%,wcolorbgc%
    e% = 0
    fgc% = wfgc%
    bgc% = wbgc%
    mfgc% = -1
    mbgc% = -1
    msg0$ = " El de la Pantalla  "
    msg1$ = " Color Seleccionado "
     call wtestcolor(wcolorfgc%,wcolorbgc%)
     call wopen (boty%,botx%,boty%+12,botx%+36,2,0,wcolorfgc%,wcolorbgc%,"")
    for y% = 0 to 7
       locate boty%+2+y%, botx%+3
       for x% = 0 to 31
          color x%,y%
          print "E";
       next x%
    next y%
    do
       t$ = inkey$
       if fgc%<>mfgc% or bgc%<>mbgc% then
          if fgc% > 31 then fgc% = 0
          if fgc% < 0  then fgc% = 31
          if bgc% > 7  then bgc% = 0
          if bgc% < 0  then bgc% = 7
          color wcolorfgc%, wcolorbgc%
          locate boty%+1, botx%+3
          print space$(32)
          for y% = 0 to 7
             locate boty%+2+y%, botx%+2
             print " ";
          next y%
          locate boty%+10, botx%+3
          print "fgc bgc attr";
          locate boty%+1, botx%+3+fgc%
          print chr$(25);
          locate boty%+2+bgc%, botx%+2
          print chr$(26);
          locate boty%+11, botx%+4
	  print using "##   #  ### ";fgc%; bgc%; scr.color2attr%(fgc%,bgc%);
          if fgc% or bgc% then
             color fgc%,bgc%
             print msg1$;
          else
             print msg0$;
          end if
          mfgc% = fgc%
          mbgc% = bgc%
       end if
       select case t$
        case chr$(&h00,&h48):decr bgc%, 1	
        case chr$(&h00,&h50):incr bgc%, 1	
        case chr$(&h00,&h4b):decr fgc%, 1	
        case chr$(&h00,&h4d):incr fgc%, 1	
        case chr$(&h0d)				
          wfgc% = fgc%
          wbgc% = bgc%
          exit loop
        case chr$(&h1b)				
          e% = 1
          exit loop
       end select
    loop while -1
     call wclose
end sub
function wascii$(boty%,botx%)
    shared e%
    shared wsfgc%,wsbgc%
    static my%, mx%
    static mascii$(), flag%
       text$ = " Decimal ASCII = ###        Caracter ASCII = \ \"
     if not flag% then
        flag% = -1
         dim mascii$(255)
        for y% = 0 to 255
           select case y%
	    case 0,7,9,10,11,12,13,28,29,30,31,32,255,256
              mascii$(y%) = " "
            case else
              mascii$(y%) = chr$(y%)
           end select
        next y%
     end if
      call wtestcolor(wsfgc%,wsbgc%)
      call wopen (boty%,botx%,boty%+20,botx%+66,2,0,wsfgc%,wsbgc%,"")
     color wsfgc%,wsbgc%
     boty% = boty% + 2
     botx% = botx% + 2
     for y% = 0 to 15 step 2
	for x% = 0 to 63 step 2
	   locate boty%+y%, botx%+x%
	   print mascii$(p%);
           incr p%, 1
        next x%
     next y%
     if mx% <> 0 or my% <> 0 then
        y% = my%
        x% = mx%
     else
        y%=0
        x%=0
     end if
     disp% = -1
     do
        t$ = inkey$
        if x%<>mx% or y%<>my% or disp% then
	   if x% > 62 then x% = 0
	   if x% < 0  then x% = 62
	   if y% > 14  then y% = 0
	   if y% < 0  then y% = 14
           disp% = 0
           color wsfgc%, wsbgc%
	   locate boty%+my%, botx%+mx%
	   mascii% = ((my% * 32) + mx%) \2
           print mascii$(mascii%)
	   mascii% = ((y% * 32) + x% )\ 2
	   locate boty%+16, botx% + 7
	   print using text$; mascii%; mascii$(mascii%);
	   color 15, 0
	   locate boty%+y%, botx%+x%
           print mascii$(mascii%)
           mx% = x%
           my% = y%
        end if
        select case t$
	 case chr$(&h00,&h48):decr y%,2		
	 case chr$(&h00,&h50):incr y%,2		
	 case chr$(&h00,&h4b):decr x%,2		
	 case chr$(&h00,&h4d):incr x%,2		
	 case chr$(&h0d)			
	   mascii% = ((y% * 32 ) + x% ) \ 2
	   wascii$ = mascii$(mascii%)
           exit loop
	end select
     loop until t$ = chr$(&h1b)
      call wclose
end function
sub set.einputcolor( fgc%, bgc% ) 			public
   shared einputfgc%
   shared einputbgc%
    einputfgc% = fgc%
    einputbgc% = bgc%
end sub
sub set.eoutputcolor( fgc%, bgc% ) 			public
   shared eoutputfgc%
   shared eoutputbgc%
    eoutputfgc% = fgc%
    eoutputbgc% = bgc%
end sub
sub set.ekbdfilter( filter$ )				public
   shared ekbdfilter$
    ekbdfilter$ = filter$
end sub
function elegalnum$( num$ )				public
    local c%,l$,r$
     num$ = ltrim$(rtrim$(num$))
     replace any "," with " " in num$
     if num$ <> "" then
        while instr(num$," ") <> 0		 
           c% = instr(num$," ")
           l$ = left$(num$,c%-1)
           r$ = right$(num$, len(num$)-c%)
           num$ = l$ + r$
        wend
        elegalnum$ = num$
     else
        elegalnum$ = "0"
     end if
end function
sub grabar.openmsg( text$ )
   shared wmsgfgc%
   shared wmsgbgc%
     call wtestcolor(wmsgfgc%,wmsgbgc%)
    by% = 12
    bx% = (40-(len(text$)/2))-2
    ty% = 14
    tx% = (40+(len(text$)/2))+2
     call wopen( by%, bx%, ty%,tx%, 3, 0, wmsgfgc%, wmsgbgc%, "" )
     call wprint(1,0,text$)
end sub
sub grabar.open( fichero$, f$, e$ )
   shared munidad$
   shared mprograma$
    open fichero$ for output as #11
     call splitpathfn(fichero$, d$, mdir$, f$, me$ )
    credit$ = mprograma$  + "Copyright (c)1990-1992 Esteve Valent¡ & PsicoSoft"
       msg1$ = " Grabando pantalla "
       select case e$
        case ".TUB"
          msg$  =  "BASIC ESTRUCTURADO "
          lin1$ =  "REM  "
          lin2$ =  "REM   Nombre de la Pantalla ...: " + f$
        case ".PRG"
          msg$  = "dBASE III, FOXBASE "
          lin1$ = "*"
          lin2$ = "*   Nombre de la Pantalla ...: " + f$
        case ".CSH"
          msg$  = "CLIPPER SUMMER 87 "
          lin1$ = "*"
          lin2$ = "*   Prodecimiento para Clipper Summer 87 por  Manolo Est‚vez "
          lin3$ = "*   Nombre de la Pantalla ...: " + f$
        case ".CH"
          msg$  = "CLIPPER 5.0x "
          lin1$ = "//  "
          lin2$ = "//   Procedimiento para Clipper 5.0x por  Manolo Est‚vez "
          lin3$ = "//   Nombre de la Pantalla ...: " + f$
        case ".COB"
          msg$  = "MS COBOL "
          lin1$ =  "      *"
          lin2$ =  "      * Procedimiento para  MSCobol por  Joan Fust‚ "
          lin3$ =  "      * Pantalla.....:  "+ f$
        case ".CBL"
          msg$  = "RM COBOL "
          lin1$ =  "      *"
          lin2$ =  "      * Procedimiento para  RMCobol por  Joan Fust‚ "
          lin3$ =  "      * Pantalla.....:  "+ f$
        case ".C"
          msg$  = "C, C++ "
          lin1$ = "/*"
          lin2$ = "   Procedimiento para  'C'   por Jos‚ Rodriguez    "
          lin3$ = "   Pantalla.....:  "+ f$+ "                      */"
        case ".PAS"
          msg$  = "PASCAL "
          lin1$ = "{**"
          lin2$ = "    Procedimiento para PASCAL por Juan Vilari¤o      "
          lin3$ = "    Pantalla.....:  "+f$+"                      **}"
        case ".ASM"
          msg$  = "ASSEMBLER "
          lin1$ = " ;**"
          lin2$ = " ;** Pantalla.....:  "+ f$
       end select
    print #11, lin1$+credit$
    print #11, lin2$
    print #11, lin3$
     call grabar.openmsg( msg1$+msg$+msg2$ )
end sub
sub config.colors.default.mono				shared
    
    ifgc%   	= 0		
    ibgc%   	= 7  	       	
    ofgc%   	= 7      	
    obgc%   	= 0             
    wlmfgc% 	= 0		
    wlmbgc% 	= 7		
    wmfgc%  	= 0		
    wmbgc%  	= 7		
    wsmfgc% 	= 0		
    wsmbgc% 	= 7		
    wsfgc%      = 0
    wsbgc%      = 7
    wdfgc%  	= 0		
    wdbgc%  	= 7 		
    wifgc%  	= 0		
    wibgc%  	= 7 		
    wefgc%  	= 0		
    webgc%  	= 7		
    wmessagefgc%= 0		
    wmessagebgc%= 7   		
    wmsgfgc%    = 0		
    wmsgbgc%    = 7		
    wcolorfgc%  = 7          	
    wcolorbgc%  = 0		
    woattrib%   = 112		
    
     call set.einputcolor(ifgc%, ibgc%)
     call set.eoutputcolor(ofgc%, obgc%)
end sub
sub config.colors.open( file% )			shared
   local file$
    file$ = munidad$+"\e-screen\e-scrcol.dat"
    open "R", file%, file$, 125
    field file%,_
    41 as titulo$,_
    2 as ifgc$,_
    2 as ibgc$,_
    2 as ofgc$,_
    2 as obgc$,_
    2 as wlmfgc$,_
    2 as wlmbgc$,_
    2 as wmfgc$,_
    2 as wmbgc$,_
    2 as wsmfgc$,_
    2 as wsmbgc$,_
    2 as wsfgc$,_
    2 as wsbgc$,_
    2 as wdfgc$,_
    2 as wdbgc$,_
    2 as wifgc$,_
    2 as wibgc$,_
    2 as wefgc$,_
    2 as webgc$,_
    2 as wmessagefgc$,_
    2 as wmessagebgc$,_
    2 as wmsgfgc$,_
    2 as wmsgbgc$,_
    2 as wcolorfgc$,_
    2 as wcolorbgc$,_
    2 as woattrib$,_
    26 as nul$
end sub
FUNCTION FmtDate$ (date%)
    local d%, m%, y%
     d% = date% AND &H1F
     m% = (date% AND &H1E0) \ 32
     y% = (date% AND &HFE00) \ 512 + 1980
     FmtDate$ = using$("##-",m%)+ using$("##-",d%)+  using$("####",y%)
END FUNCTION
FUNCTION FmtTime$ (time%)
    local s%, m%, h%
    local abbr$
     s% = (time% AND &H1F) * 2
     m% = (time% AND &H7E0) \ 32
     h% = (time% < 0) * (-16) + ((time% AND &H7FFF) \ 2048)
     Abbr$ = " am"
     IF h% = 12 THEN Abbr$ = " pm"
     IF h% = 0  THEN h% = 12
     IF h% > 12 THEN   			
        h% = h% MOD 12
        Abbr$ = " pm"
     END IF
     FmtTime$ = using$("##:",h%) + using$("##:",m%) +_
     using$("## "+Abbr$,s% )
END FUNCTION
function grabar.gwb.lin$( gwb.lin )
     lin$ = ""
     if gwb.lin <> 0 then
	incr gwb.lin, 10
	lin$ = str.lng2chr$( gwb.lin )
     end if
     grabar.gwb.lin$ = lin$
end function
sub grabar.write( ifile$, file$, e$ )
   shared munidad$
   shared macro$()
   shared cfgc%, cbgc%
   shared p%()
    cattr% = scr.color2attr%( cfgc%, cbgc% )
    ifile$ = munidad$+"\E-SCREEN\"+ifile$
    open ifile$ for input as #10
    msub$ = file$
    repl$ = "§¦!ú$&/()=?¨­@#ª|^;:,.-{}[]<>+"
    for x% = 1 to len(repl$)
       xrep$ = mid$(repl$, x%, 1)
       replace any xrep$ with "_" in msub$
    next
    do
       line input #10,linea$
       if instr(linea$,"@end@") then exit loop		
       if instr(ucase$(linea$),"@INIO@") = 0 then
          replace "@pant@" with msub$ in linea$
          replace "@date@" with date$ in linea$
          if instr(linea$, "@cfgc@") > 0 or_
             instr(linea$, "@cbgc@") > 0 or_
             instr(linea$, "@cattr@")>0 then
             attr% = scr.color2attr%( cfgc%, cbgc%)
              call grabar.write.color.fn( attr%, e$, mfgc$, mbgc$ )
             replace "@cfgc@" with mfgc$ in linea$
             replace "@cbgc@" with mbgc$ in linea$
             attr$ = grabar.write.attr$( attr%, e$ )
             replace "@cattr@" with attr$ in linea$
          end if
          if instr(linea$, "@macro") then
             for x% = 0 to 9
                nmacro$ = "@macro"+str.int2chr$(x%)+"@"
                macro$  = rtrim$(macro$(x%))
                replace nmacro$ with macro$ in linea$
             next
          end if
	  if instr(linea$, "@chars@") then
	     chars$ = grabar.write.chars$( e$ )
	     replace "@chars@" with chars$ in linea$
	  end if
          print #11,linea$
       else						
          do
             line input #10, linea$
             p% = instr(ucase$(linea$),"@ENDO@")
             if p% = 0 then
                lineas$ = lineas$ + rtrim$(linea$)+chr$(13,10)
             end if
          loop until p% <> 0
          for y% = 1 to 25
             oldx% = -1
             dato$ = ""
             for x% = 1 to 80
                m% = ((y%-1)*160)+((x%-1)*2)
                car$  = chr$(p%(m%))
                attr% = p%(m%+1)
                if car$ <> " " or dato$ <> "" or_
                   (car$ = " " and scr.attr2bgc%(attr%)<>cbgc%) then
                   dato$ = dato$ + car$
                   if oldx% = -1 then
                      oldx% = x%
                   end if
                   if car$ = " " and attr% = cattr% then
                      mcar$ = mcar$ + car$
                   else
                      mcar$ = ""
                   end if
                   if attr% <> p%(m%+3) or_
                      x% = 80 or len(mcar$) > 5 then
                      if len(mcar$) > 5 then
                         dato$ = rtrim$(dato$, " ")
                         mcar$ = ""
                      end if
                      if dato$ <> "" then
                         do
                            mlinea$ = lineas$
                            
                             call grabar.write.color.fn( attr%, e$, mfgc$, mbgc$ )
                            replace "@fgc@" with mfgc$ in mlinea$
                            replace "@bgc@" with mbgc$ in mlinea$
                            
                            
                            if instr(mlinea$, "@attr@") then
                               attr$ = grabar.write.attr$( attr%, e$ )
                               replace "@attr@" with attr$ in mlinea$
                            end if
                            
                            
                            select case e$
                             case ".CH", ".CSH", ".PRG"	
                               x$ = str.int2chr$(oldx%-1)
                               y$ = str.int2chr$(y%-1)
                             case else			
                               x$ = str.int2chr$(oldx%)
                               y$ = str.int2chr$(y%)
                            end select
                            replace "@x@" with x$ in mlinea$
                            replace "@y@" with y$ in mlinea$
                            
                            
                            select case e$
                             case ".CBL",".COB"
                               if len(dato$) > 52 then
                                  d$    = left$(dato$,52)
                                  dato$ = right$(dato$,len(dato$)-52)
                                  oldx% = oldx% + 52
                               else
                                  d$ = dato$
                                  oldx% = -1
                                  dato$ = ""
                               end if
                             case else
                               d$ = dato$
                               oldx% = -1
                               dato$ = ""
                            end select
                            replace "@text@" with d$ in mlinea$
                            
                            mlinea$ = rtrim$( mlinea$, chr$(13,10) )
                            print #11, mlinea$
                         loop while dato$ <> ""
                      end if
                   end if
                else
                   oldx% = -1
                   dato$ = ""
                end if
             next x%
          next y%
       end if
    loop while not eof(10)
    close #10
end sub
sub grabar.close
    close #11
     call wclose
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
       menu$(1) = "Todos los caracteres"
       menu$(2) = "Caracter a caracter"
       msg$ = "Cambiar el caracter seleccionado por..."
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
       sino$ = "El Bloque de Memoria no est  vacio. ¨ Copio encima ? "
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
function utilidad.print.start%
     sino$ = "¨ Impresora ON-LINE ?"
      call prn.start
     utilidad.print.start% = wsino%(0,0,sino$,e%)
end function
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
    ypos% = 1  		
    xpos% = 0  		
    iypos%=1        	
    ico% = 0   		
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
sub prn.start						PUBLIC
    local a,b
     def seg = 64
     a=peek(8)+256*peek(9)
     b=(inp(a+1) and 248) xor 72
     def seg
     out a+2,8
     out a+2,12
end sub
function str.lng2chr$( num )
     str.lng2chr$ = ltrim$(str$( num ))
end function
sub grabar.write.color.fn( attr%, e$, mfgc$, mbgc$)
    select case e$
       
       
     case ".CH", ".CSH", ".PRG"
        call str.clipper.color.fn( attr%, mfgc$, mbgc$ )
     case else
        call str.basic.color.fn( attr%, mfgc$, mbgc$ )
    end select
end sub
function grabar.write.attr$( attr%, e$)
     select case e$
      case ".CBL"   	        		
        attr$ = str.rmcobol.attr$( attr% )
      case ".COB"				
        attr$ = str.mscobol.attr$( attr% )
      case else					
        attr$ = str.int2chr$(attr%)
     end select
     grabar.write.attr$ = rtrim$(attr$,",")
end function
function grabar.write.chars$( e$ )
     shared p%()
     shared mpantalla$
     crlf$ = chr$(13,10)
             select case e$
              case ".PAS"
		for x% = 0 to 3999
                   incr i%
		   linea$ = linea$ + "#" + str.int2chr$(p%(x%)) + ","
                   if i% = 20 then
		      mlinea$ = mlinea$ + crlf$ + linea$
                      linea$ = ""
                      i% = 0
                   end if
                next x%
		mlinea$ = rtrim$(mlinea$, ",")
              case ".C"
		for x% = 0 to 3999
                   incr i%
                   chex$ = hex$(p%(x%))
                   if len(chex$) = 1 then
                      linea$ = linea$ + "0x0" + chex$ + ","
                   else
                      linea$ = linea$ + "0x" + chex$ + ","
                   end if
                   if i% = 15 then
		      mlinea$ = mlinea$ + crlf$ + linea$
                      linea$ = ""
                      i% = 0
                   end if
                next x%
		mlinea$ = rtrim$(mlinea$, ",")
	      case ".ASM"
                mtab$ = chr$(&h9)
                i% = 1
		for x% = 0 to 3999 step 50
		   linea$ = mid$(mpantalla$,x%+1,50)
		   mlinea$ = mlinea$ + mtab$ + "DB '" + linea$ + "'"+ crlf$
                next x%
             end select
	     grabar.write.chars$ = mlinea$
end function
function bloques.num%
    local ltext$
    static dato$
        ltext$ = "Nombre del bloque en la memoria (0 a 9) o (A a Z) ....: "
     do
         call set.ekbdfilter("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
         call set.weinput.default(dato$)
        dato$ = weinput$(0,0,ltext$,"", 1, 1, e% )
        if e% <> 0 then exit function
     loop while dato$ = " "
     bloques.num% = instr("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ",dato$)
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
sub str.clipper.color.fn( attr%, mfgc$, mbgc$ )
   static clipper.color$()
    if clipper.color$(0) = "" then
        dim clipper.color$(0:7)
       clipper.color$(0) = "N"
       clipper.color$(1) = "B"
       clipper.color$(2) = "G"
       clipper.color$(3) = "BG"
       clipper.color$(4) = "R"
       clipper.color$(5) = "RB"
       clipper.color$(6) = "GR"
       clipper.color$(7) = "W"
    end if
     call scr.attr2all.fn( attr%, fgc%, bgc%, inten%, blink% )
    mfgc$ = clipper.color$(fgc%)
    if inten% then  mfgc$ = mfgc$ + "+"
    mbgc$ = clipper.color$(bgc%)
    if blink% then  mfgc$ = mfgc$ + "*"
    
    
    
end sub
sub str.basic.color.fn( attr%, mfgc$, mbgc$ )
     call scr.attr2color.fn( attr%, fgc%, bgc% )
    mfgc$ = ltrim$(str$(fgc%))
    mbgc$ = ltrim$(str$(bgc%))
    
    
    
end sub
function str.rmcobol.attr$( attr% )
      call scr.attr2all.fn( attr%, fgc%, bgc%, inten%, blink% )
     if inten% then
        mint$ = "HIGH, "
     else
        mint$ = "LOW, "
     end if
     if blink% then mbli$ = "BLINK, "
     if bgc% <> 0 then mrev$ = "REVERSE, "
     str.rmcobol.attr$ = mint$+ mbli$+ mrev$
     
     
     
end function
function str.mscobol.attr$( attr% )
      call scr.attr2all.fn( attr%, fgc%, bgc%, inten%, blink% )
     if inten% then mint$ = "HIGHLIGHT, "
     if blink% then mbli$ = "BLINK, "
     if bgc% <> 0 then mrev$ = "REVERSE-VIDEO, "
     str.mscobol.attr$ = mint$+ mbli$+ mrev$
     
     
     
end function
sub scr.attr2color.fn( attr%, fgc%, bgc% )
    fgc% = attr% mod 8
    if (attr% and 8)   <> 0 then fgc% = fgc% + 8	
    if (attr% and 128) <> 0 then fgc% = fgc% + 16	
    bgc% = attr% \ 16
end sub
sub scr.attr2all.fn( attr%, afgc%, abgc%, inten%, blink% )
    afgc% = attr% mod 8			
    abgc% = attr% \ 16			
    abgc% = abgc% mod 8
    inten% = 0
    blink% = 0
    if (attr% and 8)   <> 0 then inten% = -1	
    if (attr% and 128) <> 0 then blink% = -1	
end sub
