sub grabar.ans
   shared file$, file.ext$
   shared p%()
   shared cfgc%, cbgc%
   shared ansi.ini%, ansi.len%, ansi.lin%
    fichero$ = file$ + ".ANS"
     dim menu$(1:5)
     dim mlin$(1:5)
    esc$   = chr$(27)+"["
    crlf$  = chr$(13,10)
    crlf1$ = esc$+ "s"+ crlf$+ esc$+ "u"
    crlf2$ = crlf$+ esc$+ "A"
    fgc%   = 7
    bgc%   = 0
    cattr% = scr.color2attr%( cfgc%, cbgc% )
       menu$(1) = "Borrar pantalla"
       menu$(2) = "Cursor a origen"
       menu$(3) = "Nada"
       tline$   = " Longitud m xima de cada l¡nea (0 = ninguna) (32 a 255) "
       mlin$(1) = "Hasta l¡nea 23"
       mlin$(2) = "Hasta l¡nea 24"
       mlin$(3) = "Hasta l¡nea 25"
       text$    = " Grabando pantalla ANSI "
    if grabar.overwrite%(fichero$) then
       menu% = wsmenu%(0,0,menu$(), ansi.ini%)
       select case menu%
        case 0
          exit sub
        case 1
           call scr.attr2ansi.fn( cattr%, fgc%, bgc%, inten%, blink% )
          if inten% then inten$ = "1;"
          if blink% then blink$ = "5;"
          if fgc% <> 7 then fgc$ = using$("##;",fgc%+30)
          if bgc% <> 0 then bgc$ = using$("##;",bgc%+40)
          col$ = "0;"+inten$+blink$+fgc$+bgc$
          if len(col$) > 0 then
             col$ = rtrim$(col$,";")
             col$ = esc$ + col$ + "m"
          end if
          pant$ = col$+esc$+"2J"
        case 2
          pant$ = esc$+"H"
       end select
       
       do						
           call set.weinput.default( str$(ansi.len%) )	
          lenl% = val(weinput$(0,0,tline$,"",3,10,e%))
          if e% = 1 then exit sub
       loop while (lenl%<32 and lenl%<>0) or lenl%>255
       
       ansi.len% = lenl%
       menu% = wsmenu%(0,0,mlin$(), ansi.lin%)
       select case menu%
        case 0 : exit sub
        case 1 : topy% = 23
        case 2 : topy% = 24
        case 3 : topy% = 25
       end select
        call grabar.openmsg( text$)
       oldlen% = len(pant$)
       my% = 1
       for y% = 1 to topy%
          if y% > 1 then yflag% = -1
          oldx%  = -1
          dato$  = ""
          linea$ = ""
          for x% = 1 to 80
             m% = ((y%-1)*160)+((x%-1)*2)
             car$ = chr$(p%(m%))
             attr% = p%(m%+1)
             if car$ <> " " or dato$ <> "" or_
                (car$ = " " and scr.attr2bgc%(attr%) <> cbgc%) then
                if oldx% = -1 then
                   oldx% = x% -1
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
		      dato$ = rtrim$(dato$)
                      mcar$ = ""
                   end if
                   if dato$ <> "" then
                      
                      
                      
                      mfgc%   = fgc%
                      mbgc%   = bgc%
                       call scr.attr2ansi.fn( attr%, fgc%, bgc%, inten%, blink% )
                      ini$   = ""
                      fgc$   = ""
                      bgc$   = ""
                      inten$ = ""
                      blink$ = ""
                      if (mblink% and not blink%) or_
                         (minten% and not inten%) then
                         ini$ = "0;"
                         minten% = 0
                         mblink% = 0
                      end if
                      if blink% and not mblink% then
                         blink$ = "5;"
                         mblink% = -1
                      end if
                      if inten% and not minten% then
                         inten$ = "1;"
                         minten% = -1
                      end if
                      if ini$ = "0;" then
                         if fgc% <> 7 then
                            fgc$ = using$("##;",fgc%+30 )
                         end if
                         if bgc% <> 0 then
                            bgc$ = using$("##;",bgc%+40 )
                         end if
                      else
                         if mfgc% <> fgc% then fgc$ = using$("##;",fgc%+30 )
                         if mbgc% <> bgc% then bgc$ = using$("##;",bgc%+40 )
                      end if
                      col$ = ini$+inten$+blink$+fgc$+bgc$
                      if len(col$) > 0 then
                         col$ = rtrim$(col$,";")
                         col$ = esc$ + col$ + "m"
                      end if

                      
                      
                      
                      xpos% = oldx% - oldax%		
                      xpos$ = ""
                      if yflag% then
                         yflag% = 0
                         if ansi.ini% = 3 then
                            ypos% = y%-my%
                            my%   = y%
                            if oldax% = 80 then
                               decr ypos%
                               xpos%  = oldx%
                               oldax% = 0
                            end if
                            if ypos% > 1 then			
                               xpos$ = esc$ + str.int2chr$(ypos%)+"B"
                            end if
                            if ypos% = 1 then xpos$ = esc$ + "B"
                            if oldax% < oldx% then
                               xpos% = oldx% - oldax%
                               if xpos% > 1 then			
                                  xpos$ = xpos$ + esc$+str.int2chr$(xpos%)+"C"
                               end if
                               if xpos% = 1 then xpos$ = xpos$ + esc$ + "C"
                            else
                               xpos% = oldax%  - oldx%
                               if xpos% > 1 then			
                                  xpos$ = xpos$ + esc$ + str.int2chr$(xpos%)+"D"
                               end if
                               if xpos% = 1 then xpos$ = xpos$ + "D"
                            end if
                         else
                            xpos$ = esc$ + str.int2chr$(y%)	
                            if oldx% > 0 then
                               xpos$ = xpos$ + ";" + str.int2chr$(oldx%+1)
                            end if
                            xpos$ = xpos$ + "H"
                         end if
                      else
                         if xpos% > 1 then			
                            xpos$ = esc$+str.int2chr$(xpos%)+"C"
                         end if
                         if xpos% = 1 then xpos$ = esc$ + "C"
                      end if
                      oldax% = oldx% + len(dato$)
                      if lenl% = 0 then				
                         linea$ = linea$+xpos$+col$+dato$	
                      else					
                         inst$ = xpos$+col$			
                         if oldlen%+len(inst$) < lenl%-5 then
                            linea$ = linea$ + inst$
                            oldlen% = oldlen% + len(inst$)
                         else
                            linea$ = linea$ + crlf1$ + inst$
                            oldlen% = len(inst$) + 3
                         end if
			 inst$ = ""			
                         do while dato$ <> ""
                            if oldlen%+len(dato$) < lenl%-5 then
                               linea$ = linea$ + dato$
                               oldlen% = oldlen% + len(dato$)
                               dato$ = ""
                            else
                               l% = lenl% - 5 - oldlen%
                               linea$ = linea$ + left$(dato$, l%) + crlf1$
                               oldlen% = 3
                               dato$ = right$(dato$, len(dato$)-l%)
                            end if
			 loop			 	
		      end if				
                   end if
                   oldx% = -1
                   dato$ = ""
                end if
             else
                oldx%  = -1
                dato$  = ""
             end if
          next x%
          pant$ = pant$ + linea$
       next y%
       file% = freefile				
       open fichero$ for output as #file%	
       pant$ = pant$ + esc$+ "0m"		
       print #file%, pant$			
       close #file%				
       if file.ext$ = ".ANS" then  file.save% = 0
	call wclose
    end if
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
       msg$ = " Pincel en cursor OFF "
        call wmsg( 0, 0, msg$ )
    else
	dim menu$(1:5)
       menu$(1) = "A ÚÂ¿ ÃÅ´ ÀÁÙ ³ Ä "
       menu$(2) = "B ÉË» ÌÎ¹ ÈÊ¼ º Í "
       menu$(3) = "C ÖÒ· Ç×¶ ÓÐ½ º Ä "
       menu$(4) = "D ÕÑ¸ ÆØµ ÔÏ¾ ³ Í "
	   menu$(5) = "Solo cambiar color"
	   menu$(6) = "Pintar con caracter"
	mchr% = wsmenu%(-7, 0, menu$(), oldmchr%)
       if mchr% = 0 then
          mpincel% = 0
       else
	  if mchr% = 6 then mchr.chr$ = wascii$(4,7)
	  mpincel% = -1
       end if
    end if
end sub
sub utilidad.vercolor
   shared mcolor%
    msg0$ = " Ver color posici¢n OFF "
    msg1$ = " Ver color posici¢n ON "
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
    msg0$ = " Ver posici¢n cursor OFF "
    msg1$ = " Ver posici¢n cursor ON "
    if mpos%  then
       mpos% = 0
       text$ = msg0$
    else
       mpos% = -1
       text$ = msg1$
    end if
     call wmsg(0,0, text$)
end sub
sub config.colors 					shared
   local sino$
   local cy%, cx%
   local menu%
   local menu$()
   local modif%
   static oldmenu%
     dim menu$(1:15)
    menu$(1)  = "A  Entrada datos - Entrada "
    menu$(2)  = "B  Entrada datos - Impresi¢n"
    menu$(3)  = "C  L¡nea men£ horizontal"
    menu$(4)  = "D  Ventana men£s"
    menu$(5)  = "E  Ventana submen£s"
    menu$(6)  = "F  Ventana directorio"
    menu$(7)  = "G  Ventana selecci¢n"
    menu$(8)  = "H  Ventana entrada de datos"
    menu$(9)  = "I  Ventana mensaje de error"
    menu$(10) = "J  Ventana mensajes (INTRO)"
    menu$(11) = "K  Ventana mensajes (Tiempo)"
    menu$(12) = "L  Ventana selecci¢n colores"
    menu$(13) = "M  Ventana marcar bloques"
    menu$(14) = "N  Colores por defecto (Color)"
    menu$(15) = "O  Colores por def.(Monocromo)"
    sino$ = "¨ Grabo las modificaciones en el Disco ?"
     call wopen(4,25,20,60,1,0,wsmfgc%,wsmbgc%,"")
    cy% = 11
    cx% = 8
    do
	menu% =  wmenu%(menu$(), oldmenu% )
	if menu% <> 0 then modif% = -1
       select case menu%
        case 1 : call wcolor(cy%,cx%,ifgc%,ibgc%)
        case 2 : call wcolor(cy%,cx%,ofgc%,obgc%)
        case 3 : call wcolor(cy%,cx%,wlmfgc%,wlmbgc%)
        case 4 : call wcolor(cy%,cx%,wmfgc%,wmbgc%)
        case 5 : call wcolor(cy%,cx%,wsmfgc%,wsmbgc%)
        case 6 : call wcolor(cy%,cx%,wdfgc%,wdbgc%)
        case 7 : call wcolor(cy%,cx%,wsfgc%,wsbgc%)
        case 8 : call wcolor(cy%,cx%,wifgc%,wibgc%)
        case 9 : call wcolor(cy%,cx%,wefgc%,webgc%)
        case 10: call wcolor(cy%,cx%,wmessagefgc%,wmessagebgc%)
        case 11: call wcolor(cy%,cx%,wmsgfgc%,wmsgbgc%)
        case 12: call wcolor(cy%,cx%,wcolorfgc%,wcolorbgc%)
        case 13
	  cfgc% = scr.attr2fgc%(woattrib%)
	  cbgc% = scr.attr2bgc%(woattrib%)
           call wcolor(cy%,cx%,cfgc%,cbgc%)
	  woattrib% = scr.color2attr%(cfgc%,cbgc%)
        case 14
           call config.colors.default.color
        case 15
           call config.colors.default.mono
       end select
    loop until menu% = 0
    if modif% then
       if wsino%( 0, 0, sino$, e% ) then
	  call config.colors.put
       end if
       call key.esc(2)
    end if
    call wclose
end sub
sub config.macros
    shared wifgc%, wibgc%
    shared eoutputfgc%,eoutputbgc%
    shared macro$()
    by% = 5
    bx% = 10
    ty% = 16
    tx% = 70
    call wopen(by%,bx%,ty%,tx%,1,0,wifgc%,wibgc%,"")
    by% = by%+1
    bx% = bx%+1
    for x% = 0 to 9
	locate by%+x%, bx%
	color wifgc%, wibgc%
	print "@macro"+ ltrim$(str$(x%))+ "@:";
	color eoutputfgc%,eoutputbgc%
	print macro$(x%);
    next
    bx% = bx%+9
    do
       locate by%+p%, bx%
       macro$ = einput$(50,0,e%)
       op% = p%
       select case e%
	 case 1: exit loop	
	 case 2,5: decr p%	
	 case 0,3,4: incr p%	
       end select
       macro$(op%) = macro$
    loop while p% < 10 and p% > -1
    call wclose
end sub
sub config.colors.put					shared
   local file%
   local text$
    text$ = "Fichero de colores"
     call config.colors.open(file%)
    lset titulo$  	=  mprograma$+ chr$(13)+ text$+ chr$(26)
    lset ifgc$    	=  mki$(ifgc%)		
    lset ibgc$    	=  mki$(ibgc%)		
    lset ofgc$    	=  mki$(ofgc%)    	
    lset obgc$    	=  mki$(obgc%)          
    lset wlmfgc$  	=  mki$(wlmfgc%)	
    lset wlmbgc$  	=  mki$(wlmbgc%)	
    lset wmfgc$   	=  mki$(wmfgc%)		
    lset wmbgc$   	=  mki$(wmbgc%)		
    lset wsmfgc$  	=  mki$(wsmfgc%)	
    lset wsmbgc$  	=  mki$(wsmbgc%)	
    lset wsfgc$         =  mki$(wsfgc%)
    lset wsbgc$		=  mki$(wsbgc%)
    lset wdfgc$   	=  mki$(wdfgc%)		
    lset wdbgc$   	=  mki$(wdbgc%)		
    lset wifgc$   	=  mki$(wifgc%)		
    lset wibgc$   	=  mki$(wibgc%) 	
    lset wefgc$   	=  mki$(wefgc%)		
    lset webgc$   	=  mki$(webgc%)		
    lset wmessagefgc$ 	=  mki$(wmessagefgc%)	
    lset wmessagebgc$ 	=  mki$(wmessagebgc%)
    lset wmsgfgc$     	=  mki$(wmsgfgc%)	
    lset wmsgbgc$     	=  mki$(wmsgbgc%)	
    lset wcolorfgc$   	=  mki$(wcolorfgc%)	
    lset wcolorbgc$   	=  mki$(wcolorbgc%)	
    lset woattrib$    	=  mki$(woattrib%)	
    put #file%,1
    close #file%
end sub
sub config.colors.get					shared
   local file%
    file% = freefile
     call config.colors.open(file%)
    get #file%,1
    ifgc%   	=  cvi(ifgc$)			
    ibgc%   	=  cvi(ibgc$)			
    ofgc%   	=  cvi(ofgc$)       	        
    obgc%   	=  cvi(obgc$)               	
    wlmfgc%	=  cvi(wlmfgc$)			
    wlmbgc% 	=  cvi(wlmbgc$)			
    wmfgc%  	=  cvi(wmfgc$)			
    wmbgc%  	=  cvi(wmbgc$)			
    wsmfgc% 	=  cvi(wsmfgc$)			
    wsmbgc% 	=  cvi(wsmbgc$)			
    wsfgc%	=  cvi(wsfgc$)
    wsbgc%	=  cvi(wsbgc$)
    wdfgc%  	=  cvi(wdfgc$)			
    wdbgc%  	=  cvi(wdbgc$)			
    wifgc%  	=  cvi(wifgc$)			
    wibgc%  	=  cvi(wibgc$) 			
    wefgc%  	=  cvi(wefgc$)			
    webgc%  	=  cvi(webgc$)			
    wmessagefgc%=  cvi(wmessagefgc$)		
    wmessagebgc%=  cvi(wmessagebgc$)		
    wmsgfgc%    =  cvi(wmsgfgc$)		
    wmsgbgc%    =  cvi(wmsgbgc$)		
    wcolorfgc%  =  cvi(wcolorfgc$)     		
    wcolorbgc%  =  cvi(wcolorbgc$)		
    woattrib%   =  cvi(woattrib$)		
     call set.einputcolor(ifgc%, ibgc%)
     call set.eoutputcolor(ofgc%, obgc%)
    close file%
end sub
sub config.colors.default.color				shared
    
    ifgc%   	= 4		
    ibgc%   	= 2
    ofgc%   	= 2      	
    obgc%   	= 4             
    wlmfgc% 	= 14		
    wlmbgc% 	= 1		
    wmfgc%  	= 7		
    wmbgc%  	= 1		
    wsmfgc% 	= 1		
    wsmbgc% 	= 3		
    wsfgc%      = 1
    wsbgc%      = 7
    wdfgc%  	= 14		
    wdbgc%  	= 5 		
    wifgc%  	= 15		
    wibgc%  	= 7 		
    wefgc%  	= 12		
    webgc%  	= 4		
    wmessagefgc%= 14		
    wmessagebgc%= 7   		
    wmsgfgc%    = 15		
    wmsgbgc%    = 3		
    wcolorfgc%  = 15          	
    wcolorbgc%  = 7		
    woattrib%   = 32		
    
     call set.einputcolor(ifgc%, ibgc%)
     call set.eoutputcolor(ofgc%, obgc%)
end sub
sub ayuda.mail
   shared munidad$
    blq.name$ = munidad$ + "\E-SCREEN\MAIL.BLQ"
    blq$ = bloques.disk.get$( blq.name$ )
     call bloques.scr.put.blq( 0, 0, blq$ )
    a$ = input$(1)
end sub
function scr.get.seg
    local ah%, al%, bh%
    local s%
    local o%
    local w%
     reg (1), &hf * 256   	
      call interrupt &h10  	
     ah% = reg(1) \ 256  	
     al% = reg(1) mod 256  	
     bh% = reg(2) \ 256  	
     w% = bh%       		
     if ah% = 80 then
        o% = &h100
     else
        o% = &h80
     end if
     select case al%  		
      case &h0 to &h6 		
        s% = &hb800  		
      case &h7       		
        s% = &hb000  		
      case &h8 to &ha  		
        s% = &hb800   		
      case &hd to &h13  	
        s% = &ha000  		
      case else
        s% = &hb800
     end select
     scr.get.seg = s% + (o% * w%)
     
     
     
end function
function scr.columns%
    local columns%
     reg 1, &h0f00
      call interrupt &h10
     columns% = reg(1) \ 256
     scr.columns% = columns%
end function
function scr.attr2fgc%( attr% )
    local fgc%
     fgc% = attr% mod 8
     if (attr% and 8)   <> 0 then fgc% = fgc% + 8	
     if (attr% and 128) <> 0 then fgc% = fgc% + 16	
     scr.attr2fgc% = fgc%
end function
function scr.attr2bgc%( attr% )
    local bgc%
     bgc% = attr% \ 16
     scr.attr2bgc% = bgc% mod 8
end function
sub scr.ansi2color.fn( num$, fgc%, bgc% )
   static blink%
   static hilite%
   shared cfgc%, cbgc%
    p% = 0
    do
       m% = instr(p% + 1, num$, ";") - 1
       if m% < 1 then m% = len(num$) - p%
       c$ = mid$(num$, p% + 1, m%)
       c% = val(c$)
       select case c%
        case 0, 8		
          fgc% = 7
          bgc% = 0
          hilite% = 0
          blink% = 0
        case 1			
          hilite% = -1
        case 2
          hilite% = 0
        case 5			
          blink% = -1
        case 30 to 37		
          c% = c% - 30
          select case c%
           case 1: fgc% = 4
           case 3: fgc% = 6
           case 4: fgc% = 1
           case 6: fgc% = 3
           case else: fgc% = c%
          end select
        case 40 to 47		
          c% = c% - 40
          select case c%
           case 1: bgc% = 4
           case 3: bgc% = 6
           case 4: bgc% = 1
           case 6: bgc% = 3
           case else: bgc% = c%
          end select
       end select
       p% = instr(p% + 1, num$, ";")
    loop while p%
    if hilite% then
       select case fgc%
        case <8 , 16 to 23
          fgc% = fgc% + 8
       end select
    end if
    if blink%  and fgc% < 16 then fgc% = fgc% + 16
end sub
function kbd.ins%
      call kbd.flags(mins%, mcap%, mnum%, mdes%, malt%, mctl%, mlsh%, mrsh%)
     kbd.ins% = mins%
end function
function dsk.get.file$(file$)
      call findfile(file$,&h0,f$,fa%,s&,d%,t%,e%)
     if e% = 0 then
        file% = freefile
        open file$ for binary as file%
        get$ file%, s&, dat$
        close file%
        dsk.get.file$ = dat$
     end if
end function
function wmenu%( menu$(1), ipos% )
   shared wind$(),wnum%
   shared wmenu.keytype%
   shared wmenu.leftright%		
    botx% = cvi(mid$(wind$(wnum%,2),1,2))
    boty% = cvi(mid$(wind$(wnum%,2),3,2))
    topx% = cvi(mid$(wind$(wnum%,2),5,2))
    topy% = cvi(mid$(wind$(wnum%,2),7,2))
    type% = cvi(mid$(wind$(wnum%,2),9,2))
    mwfgc%  = cvi(mid$(wind$(wnum%,2),11,2))
    mwbgc%  = cvi(mid$(wind$(wnum%,2),13,2))
    iv%   = cvi(mid$(wind$(wnum%,2),15,2))
    do  				
       incr fv%,1			
       l$ = menu$(fv%)
       l$ = ucase$(left$(ltrim$(l$),1))
       opcio$=opcio$+l$			
    loop while menu$(fv%) <> ""
    if type% = 1 or type%=2 then d%=3 else d% = 1	
    lenwind% = (topy%-d%-boty%-d%+1)
    if ipos% = 0 then ipos% = 1				
    color mwfgc%,mwbgc%
    locate ,,0
    for y% = boty%+d% to topy%-d%
       locate y%,botx%+1
       print mid$(space$(70),1,topx%-botx%-1);
    next y%
    do
       locate ,,0
       a$ = ucase$(inkey$)
       select case a$
        case chr$(&h00,&h4b)			
          if wmenu.leftright% then
              call kbd.putkey(&h1b, &h01)
              call kbd.putkey(&h00, &h4b)
              call kbd.putkey(&h0d, &h1c)
          end if
        case chr$(&h00,&h4d)			
          if wmenu.leftright% then		
              call kbd.putkey(&h1b, &h01)
              call kbd.putkey(&h00, &h4d)
              call kbd.putkey(&h0d, &h1c)
          end if
	case chr$(&h00,&h47)                    
	  ipos% = 1
	case chr$(&h00,&h4f)			
	  ipos% = fv% - 1
        case  chr$(&h00,&h48)			
          decr ipos%,1
        case  chr$(&h00,&h50)			
          incr ipos%,1
	case  chr$(&h00,&h49)			
	  decr ipos%,lenwind%
	  if ipos% < 1 then ipos% = 1
        case  chr$(&h00,&h51)			
	  incr ipos%,lenwind%
	  if ipos% > fv% -1 then
	     ipos% = fv% -1
	  end if
	case  chr$(&h0d)			
	  wmenu% = ipos%
	  wind$(wnum%,2)=left$(wind$(wnum%,2),14)+mki$(iv%)
	  exit loop
        case > ""				
	 if instr(opcio$, a$ ) > 0  then
	    opci% = instr(ipos%+1, opcio$, a$ )
	    if opci% <= 0 then opci% = instr( opcio$, a$ )
	    ipos% = opci%
	    if not wmenu.keytype% then
	       call kbd.putkey(&h0d, &h1c)
	    end if
          end if
       end select
       if ipos% < 1   then ipos% = fv% - 1
       if ipos% > fv% - 1 then ipos% = 1
       if ipos% => iv%+lenwind%-1 then iv% = ipos%-(lenwind%-1)
       if ipos% < iv% then iv% = ipos%
       for p% = 0  to lenwind%-1
	  text$ = menu$(iv%+p%)
	  if text$ = "" then exit for
	  locate boty%+d%+p%,botx%+1
	  color mwfgc%,mwbgc%
	  if ipos% = iv%+ p% then
             if mwfgc% > 7 then
                color mwbgc%,7				
             else
                color mwbgc%,mwfgc%
             end if
          end if
	  print left$(" "+text$+space$(60),topx%-botx%-1);
       next
    loop until a$ = chr$(&h1b)
    wmenu.leftright% = 0
    wmenu.keytype%   = 0
end function
function wdir$(boty%,botx%,bord%,mask$,sattr%,typesort%,lendir%)
    shared f$
    shared wmenu.keytype%
    shared wdfgc%,wdbgc%
    local  rdir$()				
      dim dynamic rdir$(1:2048)
     select case typesort%
      case 1 : xsort% = 10			
      case 2 : xsort% = 13        		
      case 3 : xsort% = 23			
      case 4 : xsort% = 36			
      case else : xsort% = 1			
     end select
     select case lendir%
      case  1 : topx% = 25			
      case  2 : topx% = 38			
      case  3 : topx% = 51			
      case else : topx% = 15			
     end select
     topx% = topx%+botx%
      call wtestcolor(wdfgc%,wdbgc%)
      call wopen(boty%,botx%,boty%+15,topx%,bord%,1,wdfgc%,wdbgc%,mask$)
     do
        locate boty%+1,botx%+1			
        color wdfgc%,wdbgc%
        print space$(topx%-1-botx%);
        if len(mask$) < topx%-4-botx% then
           locate boty%+1,botx%+int(((topx%-botx%)-len(mask$))/2)
           print mask$;
        else
           locate boty%+1,botx%+1
           print right$(mask$,topx%-1-botx%);
        end if
	call readdir(mask$,sattr%,rdir$(),lendir%,e%)	

        p% = 0
        while rdir$(p%+1)<>"": incr p% : wend
        if p% > 1 then
           array sort rdir$() for p%, from xsort% to len(rdir$(p%))
        end if
        
	 wmenu.keytype% = -1
	 menu% = wmenu%( rdir$(), ip% )
	if menu% = 0 then exit loop
	fa% = cvi(right$(rdir$(menu%),2))
	f$  = left$(rdir$(menu%),12)
        f$  = rtrim$(f$, any " ")
        p%  = rinstr(0, mask$,"\")
        p1% = rinstr(p%,mask$,"\")
        ext$ = right$(mask$, len(mask$) - p% )
        select case fa%
         case 16
           select case f$
            case "."
              mask$ = left$(mask$,3) + ext$
            case ".."
              mask$ = left$(mask$,p1%) + ext$
            case else
              mask$ = left$(mask$, p%) + f$ + "\" + ext$
           end select
         case else
           wfile$ = left$(mask$,p%)+f$
        end select
     loop while wfile$ = ""
     erase rdir$
     wdir$ = wfile$
      call wclose
     
     
     
end function
sub wprint(y%,x%,text$)
   shared wind$(),wnum%
   shared wprintfgc%,wprintbgc%
    botx% = cvi(mid$(wind$(wnum%,2),1,2))
    boty% = cvi(mid$(wind$(wnum%,2),3,2))
    topx% = cvi(mid$(wind$(wnum%,2),5,2))
    type% = cvi(mid$(wind$(wnum%,2),9,2))
    fgc%  = cvi(mid$(wind$(wnum%,2),11,2))
    bgc%  = cvi(mid$(wind$(wnum%,2),13,2))
    if type% > 0 then boty% = boty% + 2
    if wprintfgc% = 0 and wprintbgc% = 0 then
       color fgc%,bgc%
    else
       color wprintfgc%,wprintbgc%
    end if
    select case  x%
     case -1
       locate boty% + y% , botx% + 1
     case 0
       locate boty% + y% , (botx% + 1) + (((topx% - botx%) - len (text$)) \ 2)
     case 1
       locate boty% + y% , topx%  - len (text$)
     case else
       locate boty% + y% , botx% + x%
    end select
    print text$;
    
    
    
end sub
function weinput$( boty%, botx%, ltext$, rtext$, lend%, type%, e% )
    shared weinput.default$
    shared wifgc%, wibgc%
     text$ = ltext$+space$(lend%)+rtext$
      call wtestcolor(wifgc%,wibgc%)
     by% = boty% + 12
     bx% = (40-(len(text$)/2))-2
     ty% = boty%+14
     tx% = (40+(len(text$)/2))+2
      call wopen( by%, bx%, ty%, tx%, 1, 0, wifgc%, wibgc%, "" )
     locate by% + 1, bx% + 2
     print text$;
     locate by% + 1, bx% + 2 + len(ltext$)
     print weinput.default$
     locate by% + 1, bx% + 2 + len(ltext$)
     weinput$ = einput$( lend%, type%, e% )
      call wclose
     weinput.default$ = ""
end function
sub set.weinput.default( text$ )
   shared weinput.default$
    weinput.default$ = text$
end sub
function wsino%( boty%, botx%, text$, e% )
    local default$
    local filter$
    local resp$
     mtext$   = text$ + " (S\N) "
     default$ = "S"
     filter$  = "SNsn"
      call set.weinput.default( default$ )
      call set.ekbdfilter( filter$ )
     resp$ = weinput$( 0, 0, mtext$, "", 1, 1, e% )
     if e% = 0 then
        if resp$ = default$ then wsino% = -1
     end if
end function
sub wtestcolor(fgc%,bgc%)
    if fgc% = 0 and bgc% = 0 then
       fgc% = 0
       bgc% = 7
    end if
end sub
function einput$( lend%, type%, e% )				public
    local t$			
    local num##, num$ 		
    local musing$			
    local w%,x%,f%,l%,d$,n%,m% 	
    local posy%, posx%, my%, mx%	
    local iei%			
    local ifgc%, ibgc%		
    local ofgc%, obgc% 		
    shared eret%			
    shared einputfgc%		
    shared einputbgc%		
    shared eoutputfgc%		
    shared eoutputbgc%		
    shared einsert%		
    shared ereturn%		
    shared ekbdbeep%		
    shared ekbdclick%		
    shared ekbdfilter$		
    shared estarttolastchr%        
     eret%= 0				
     e%   = 0				
     posy% = csrlin			
     posx% = pos(0)
     ifgc% = einputfgc%			
     ibgc% = einputbgc%
     ofgc% = eoutputfgc%
     obgc% = eoutputbgc%
     if ifgc% = ibgc% or_
	ifgc% < 0 or ifgc% > 16 or_
	ibgc% < 0 or ibgc% > 8 then
	ifgc% = 0
	ibgc% = 7
     end if
     if ofgc% = obgc% or_		
	ofgc% < 0 or ofgc% > 16 or_
	obgc% < 0 or obgc% > 8 then
	ofgc% = 7
	obgc% = 0
     end if
     color ifgc%, ibgc%			
     if einsert% then			
	iei% = -1
	locate posy%, posx%, 1, 0, 16
     else
	iei% = 0
	locate posy%, posx%, 1, 13, 16
     end if
select case type%
 case 0 to 4					
   print scr.get.text$(posy%, posx%, lend%);
 case 10 to 14				
   if lend% < 1  then lend% = 1                	
   if lend% > 18 then lend% = 18               	
   num$  = scr.get.text$(posy%,posx%,lend%)        	
   num$  = elegalnum$(num$)
   num## = val(num$)				
   f% = len(str$(fix(num##))) - 1		
   musing$ = string$(f%, "#")		 	
   print space$(lend%);                       	
   locate posy%, posx%                        	
   print using musing$; num##;               	
   if type% = 14 then                         	
      m% = lend% \ 4		                
      lend% = lend% - m%			
   end if
 case 20 to 24					
   if lend% < 2  then lend% = 2		 	
   if lend% > 19 then lend% = 19		
   num$  = scr.get.text$(posy%,posx%,lend%)
   num$  = elegalnum$(num$)
   num## = val(num$)				
   f% = len(str$(fix(num##))) - 1
   musing$ = "+" + string$(f%, "#")
   print space$(lend%);
   locate posy%, posx%
   print using musing$; num##;
   if type% = 24 then                         	
      m% = (lend% - 1) \ 4	                
      lend% = lend% - m%			
   end if
 case else
   print "Error in EInput$ passed parameters. Program break";:end
end select
do
   e% = 0                         		
   color ifgc%, ibgc%				
   if ereturn% and estarttolastchr% then	
      estarttolastchr% = 0
      d$ = scr.get.text$(posy%, posx%, lend%)
      mx% = len(rtrim$(d$))
      locate posy%, posx% + mx%
   else
      locate posy%, posx%
   end if
   if iei% then					
      locate , , 1, 0, 16
   else
      locate , , 1, 13, 16
   end if
   
   
   
   
   do
      t$ = inkey$				
      select case t$
       case chr$(&h8)				
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
       case chr$(&h00,&h53)			
         if pos(0) < posx% + lend% then
	    f% = (((csrlin - 1) * scr.columns% * 2) + (pos(0) - 1) * 2)
	    l% = (((csrlin - 1) * scr.columns% * 2) + (posx% + lend% - 3) * 2)
            for x% = f% to l% step 2
               poke x%, peek(x% + 2)
            next x%
            poke x%, 32
         end if
       case chr$(&h00,&h52)			
         if iei% then
            iei% = 0
            locate , , , 13, 16
         else
            iei% = -1
            locate , , , 0, 16
         end if
       case chr$(&h00,&h75)			
         if posx% + lend% > pos(0) then
            l% = posx% + lend% - pos(0)
            my% = csrlin : mx% = pos(0)
            print space$(l%);
            locate my%, mx%
         end if
       case chr$(&h00,&h77)			
         if pos(0) > posx% then
            l% = pos(0) - posx%
            locate posy%, posx%
            print space$(l%);
         end if
       case chr$(&h00,&h4b)			
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
       case chr$(&h00,&h4d)			
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
       case chr$(&h00,&h4f)			
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
       case chr$(&h00,&h47)			
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
       case chr$(&h1b)           		
         eret%= 1 : e% = 1 : ekbdfilter$ = "" : exit function
       case chr$(&h00,&h48) 	  		
         eret%= 2 : exit loop
       case chr$(&h00,&h0f)	   		
         eret%= 2 : exit loop
       case chr$(&h00,&h50)	  		
         eret%= 4 : exit loop
       case chr$(&h09)
         eret%= 4 : exit loop
       case " " to chr$(&hff)
	 if ekbdfilter$="" or_          	
         (ekbdfilter$<>"" and _			
         instr(ekbdfilter$,t$)<>0) then
         if ekbdclick% then play "L64 N0"		
         select case type%
          case 0                                     
            p$ = t$
          case 1                                     
            p$ = ucase$(t$)
          case 2                                     
            p$ = lcase$(t$)
          case 3                                     
	    d$ = scr.get.text$(csrlin, pos(0) - 1, 1)
            if d$ = " " or pos(0) = posx% then
               p$ = ucase$(t$)
            else
               p$ = lcase$(t$)
            end if
          case 4                                     
	    d$ = scr.get.text$(csrlin, pos(0) - 1, 1)
            if d$ = " " or pos(0) = posx% then
               p$ = lcase$(t$)
            else
               p$ = ucase$(t$)
            end if
          case 10 to 14                              
            p$ = chr$(&h7)
            if instr("0123456789", t$) <> 0 then
               p$ = t$
            end if
          case 20 to 24                              
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
       else
	  p$ = chr$(&h07)  			
       end if
      case else                                    
	if t$ <> "" and t$ <> chr$(&h0d)_
	   and ekbdbeep% then play "L64 N84"
     end select
     
     
     
     if p$ <> "" then                              
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
	if pos(0) < posx% + lend% then             
	   if p$ = chr$(&h07) then
	      if ekbdbeep% then play "L64 N84"
	   else
	      print p$;
	   end if
	else
	   if ekbdbeep% then play "L64 N84"
	end if
	p$ = ""
	if ereturn% and pos(0) = posx% + lend% then
	   exit loop
	end if
     end if
     
   loop until t$ = chr$(&h0d)
   
   n% = 0					
   musing$ = ""					
   color ofgc%, obgc%				
   locate posy%, posx%, 0			
   
   
   
   select case type%
    case 0 to 4
       print scr.get.text$(posy%, posx%, lend%);
    case 10 to 14
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
    case 20 to 24				
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
    
loop until not e%
e% = eret%				
ekbdfilter$ = ""			
einput$ = scr.get.text$(posy%, posx%, lend%)
color ofgc%, obgc%
end function
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

sub ficheros.writed
   shared file.save%
   shared file$, file.ext$
    call splitpathfn(file$, d$, md$, f$, e$ )
	text$ = " Fichero "+f$+file.ext$+" no grabado. ¨ Lo grabo ? "
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



$segment





function pincel.getchr% (yp%, xp%, way$)
    shared mchr$()
    shared mchr%
     pincel.getchr% = 0
    local p%
    local mpos%()
      dim mpos%(1:10)
     mpos%(1) = mchr%
     scrchr$ = chr$(peek(160 * (yp% - 1) + 2 * (xp% - 1)))
     select case lcase$(way$)
      case "up"             
        select case mchr%    
	 case 1 : mpos%(2) = 4
	 case 2 : mpos%(2) = 3
	 case 3 : mpos%(2) = 2
	 case 4 : mpos%(2) = 1
        end select
        if yp% > 0 then
           for p% = 1 to 2
              for y% = 1 to 2
                 for x% = 1 to 3
                    if scrchr$ = mchr$(mpos%(p%), y%, x%) then 
                       pincel.getchr% = -1
                       exit function
                    end if
                 next x%
              next y%
           next p%
           if scrchr$ = mchr$(mpos%(1), 4, 1) then
              pincel.getchr% = -1
              exit function
           end if
        end if
      case "down"           
        select case mchr%
	 case 1 : mpos%(2) = 4
	 case 2 : mpos%(2) = 3
	 case 3 : mpos%(2) = 2
	 case 4 : mpos%(2) = 1
        end select
        if yp% < 26 then
           for p% = 1 to 2
              for y% = 2 to 3
                 for x% = 1 to 3
                    if scrchr$ = mchr$(mpos%(p%), y%, x%) then
                       pincel.getchr% = -1
                       exit function
                    end if
                 next x%
              next y%
           next p%
           if scrchr$ = mchr$(mpos%(1), 4, 1) then     
              pincel.getchr% = -1
              exit function
           end if
        end if
      case "left"           
        select case mchr%
	 case 1 : mpos%(2) = 3
	 case 2 : mpos%(2) = 4
	 case 3 : mpos%(2) = 1
	 case 4 : mpos%(2) = 2
        end select
        if xp% > 0 then
           for p% = 1 to 2
              for y% = 1 to 3
                 for x% = 1 to 2
                    if scrchr$ = mchr$(mpos%(p%), y%, x%) then 
                       pincel.getchr% = -1
                       exit function
                    end if
                 next x%
              next y%
           next p%
           if scrchr$ = mchr$(mpos%(1), 4, 2) then
              pincel.getchr% = -1
              exit function
           end if
        end if
      case "right"          
        select case mchr%
         case 1 :	mpos%(2) = 3
         case 2 :	mpos%(2) = 4
         case 3 :	mpos%(2) = 1
         case 4 :	mpos%(2) = 2
        end select
        if xp% < 81  then
           for p% = 1 to 2
              for y% = 1 to 3
                 for x% = 2 to 3
                    if scrchr$ = mchr$(mpos%(1), y%, x%) then
                       pincel.getchr% = -1
                       exit function
                    end if
                 next x%
              next y%
           next p%
           if scrchr$ = mchr$(mpos%(1), 4, 2) then
              pincel.getchr% = -1
              exit function
           end if
        end if
     end select
end function


sub main.ini.config.put				shared
   local file%
   local text$
   local m$, x%
    text$ = "Fichero de configuraci¢n"
    for x% = 0 to 9
	m$ = m$ + macro$(x%)
    next
    file% = main.ini.config.open%
    lset ctitulo$  = mprograma$+chr$(13)+text$+chr$(26)
    lset cmpos$	   = mki$(mpos%)
    lset cmcolor$  = mki$(mcolor%)
    lset cmpincel$ = mki$(mpincel%)
    lset cmchr$    = mki$(mchr%)
    lset cfgc$	   = mki$(fgc%)
    lset cbgc$     = mki$(bgc%)
    lset cbfgc$	   = mki$(bfgc%)
    lset cbbgc$    = mki$(bbgc%)
    lset ccfgc$    = mki$(cfgc%)
    lset ccbgc$    = mki$(cbgc%)
    lset ccbfgc$   = mki$(cbfgc%)
    lset ccbbgc$   = mki$(cbbgc%)
    lset cbloques.abc.num$ = mki$(bloques.abc.num%)
    lset ccurdir$  = curdir$("")
    lset cfile$    = file$ + file.ext$
    lset cansi.ini$= mki$(ansi.ini%)
    lset cansi.len$= mki$(ansi.len%)
    lset cansi.lin$= mki$(ansi.lin%)
    lset cnull$    = ""
    lset cmacro$   = m$
    put #file%, 1
    close #file%
end sub
function main.ini.config.open% 		shared
    local dfile$
    local dfile%
     dfile% = frefile
     dfile$ = munidad$+"\e-screen\e-scrcon.dat"
     open dfile$ for random as dfile% len = 750
     field #file%,_
     50 as ctitulo$,_
     2 as cmpos$,_
     2 as cmcolor$,_
     2 as cmpincel$,_
     2 as cmchr$,_
     2 as cfgc$,_
     2 as cbgc$,_
     2 as cbfgc$,_
     2 as cbbgc$,_
     2 as ccfgc$,_
     2 as ccbgc$,_
     2 as ccbfgc$,_
     2 as ccbbgc$,_
     2 as cbloques.abc.num$,_
     70 as ccurdir$,_
     70 as cfile$,_
     2 as cansi.ini$,_
     2 as cansi.len$,_
     2 as cansi.lin$,_
     24 as cnull$,_
     500 as cmacro$
     main.ini.config.open% = dfile%
end function


function grabar.overwrite%(fichero$)
    shared file$
    local newfile$
    shared merr%
     if mid$(fichero$,2,1) <> ":" then
        fichero$ = ficheros.curdir$+fichero$
     end if
     do
        merr% = 0
        do
            call set.weinput.default(fichero$)
           newfile$ = weinput$( 0, 0, "", "", 70, 1, e%)
           newfile$ = trim$( newfile$ )
           if e% <> 0 then exit function
        loop while newfile$ = ""
        f$ = dir$(newfile$, &h0)
        open "R", #1, newfile$, 1
        if merr% = 0 then
           close #1
        end if
     loop while merr% <> 0
     if f$ = "" then
	kill newfile$
     else
	   sino$ = "El fichero "+f$+" ya existe. ¨ Quieres grabar encima ?"
        if wsino%( 0, 0, sino$, e% ) then
           kill newfile$
        else
           exit function
        end if
     end if
     call splitpathfn( newfile$, d$, md$, f$, e$ )
     file$ = d$+ md$+ f$
     fichero$ = newfile$
     grabar.overwrite% = -1
end function


