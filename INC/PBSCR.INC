'
' ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออปบ
'บ Library......: PBSCR   .INC                                           บบ
'บ Definition...:                                                        บบ
'บ Author.......:                                                        บบ
'บ Date.........: 12-25-1991                                             บบ
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
'บ SUB :scr.defseg                                          12-25-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub scr.defseg
    def seg = scr.get.seg
    'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
    'บ END SUB:scr.defseg                                       12-25-1991   บ
    'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ FUNCTION :scr.seg                                        12-25-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ    scr.seg        ===>                                                บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
function scr.get.seg
    local ah%, al%, bh%
    local s%
    local o%
    local w%
     reg (1), &hf * 256   	'funciขn "Buscar modo de vกdeo"
      call interrupt &h10  	'de la ROM BIOS
     ah% = reg(1) \ 256  	'Extrae AH del registro AX del micropr.
     al% = reg(1) mod 256  	'Extrae AL del registro AX del micropr.
     bh% = reg(2) \ 256  	'Extrae BH del registro BX del micropr.
     w% = bh%       		'Pantalla activa
     

     if ah% = 80 then
        o% = &h100
     else
        o% = &h80
     end if
     

     select case al%  		'Tipo de vกdeo (ver manual para IBM PC de )
      case &h0 to &h6 		'Texto o gr ficos baja y media resoluciขn.
        s% = &hb800  		' segmento inicial pantalla = &hB800
      case &h7       		'Monocromo
        s% = &hb000  		' segmento inicial pantalla = &hB000
      case &h8 to &ha  		'Modo de vกdeo exclusivo del PCjr
        s% = &hb800   		' desconozco el segmento de la pantalla
      case &hd to &h13  	'Gr ficos alta y super alta resoluciขn
        s% = &ha000  		' segmento inicial pantalla = &hA000
      case else
        s% = &hb800
     end select

     
     scr.get.seg = s% + (o% * w%)
     'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
     'บ END FUNCTION:scr.seg                                     12-25-1991   บ
     'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end function





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ FUNCTION :scr.text$                                      12-25-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    posy%          ===>                                                บ
'บ    posx%          ===>                                                บ
'บ    lend%          ===>                                                บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ    scr.text$      ===>                                                บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
function scr.get.text$ ( posy%, posx%, lend% )
    local text$
    local t$
    local x%
     text$ = scr.get.chrattr$(posy%, posx%, lend%)
     

     for x% = 1 to len(text$) step 2
        t$ = t$ + mid$(text$,x%,1)
     next x%

     
     scr.get.text$ = t$
     'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
     'บ END FUNCTION:scr.text$                                   12-25-1991   บ
     'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end function





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ FUNCTION :scr.chrattr$                                   12-25-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    posy%          ===>                                                บ
'บ    posx%          ===>                                                บ
'บ    lend%          ===>                                                บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ    scr.chrattr$   ===>                                                บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
function scr.get.chrattr$( posy%, posx%, lend% )
    local o%			'offset to start read
    local b%			'leng in bytes to read
      call scr.defseg      	'Automaticall DEF SEG =
     o% = scr.columns% * 2 * (posy% - 1) + 2 * (posx% - 1)
     scr.get.chrattr$ = peek$( o%, lend% * 2)
     'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
     'บ END FUNCTION:scr.chrattr$                                12-25-1991   บ
     'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end function





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :getvideoconfig                                      12-25-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    a              ===>                                                บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub getvideoconfig(a) public
    
    'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
    'บ END SUB:getvideoconfig                                   12-25-1991   บ
    'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :scr.holdcur                                         12-25-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub scr.holdcur
   shared scr.cur%()
   shared scr.curp%
    

    if scr.curp% = 0 then
       

        redim scr.cur%(1:15,1:2)
       
    end if
    
    incr scr.curp%
    scr.cur%(scr.curp%,1) = scrlin
    scr.cur%(scr.curp%,2) = pos(0)
    'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
    'บ END SUB:scr.holdcur                                      12-25-1991   บ
    'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :scr.restcur                                         12-25-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
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
    
    'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
    'บ END SUB:scr.restcur                                      12-25-1991   บ
    'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub





function scr.color2attr%( fgc%, bgc% )
    local temp%
     temp%=(bgc% * 16) + fgc%
     if fgc% > 15 then temp% = temp% + 112
     scr.color2attr% = temp%
     
end function





sub scr.def.mode( mode% )
    reg 1, mode%
     call interrupt &h10
end sub





function scr.mode%
     reg 1, &h0f00
      call interrupt &h10
     scr.get.mode% = reg(1) mod 256
end function





function scr.page%
     reg 1, &h0f00
      call interrupt &h10
     scr.page% = reg(2) \ 256
end function





function scr.rows%
    local rows%
     reg 1, &h1130			' func. 11h subfunc. 30h
      call interrupt &h10			' get letter type (EGA,MCGA,VGA)
     rows% = (reg(4) mod 256) + 1		' extract DL
     

     if rows% <> 0 then
        scr.rows% = rows%
     else
        scr.rows% = 25
     end if
     
end function





function scr.columns%
    local columns%
     reg 1, &h0f00
      call interrupt &h10
     columns% = reg(1) \ 256
     scr.columns% = columns%
     
end function





sub scr.text.8x8
    reg 1, &h1112
    reg 2, 0
     call interrupt &h10
    
end sub





sub scr.text.8x14
    reg 1, &h1111
    reg 2, 0
     call interrupt &h10
    
end sub





sub scr.text.8x16
    reg 1, &h1114
    reg 2, 0
     call interrupt &h10
    
end sub





sub scr.attr2color.fn( attr%, fgc%, bgc% )
    
    fgc% = attr% mod 8
    
    if (attr% and 8)   <> 0 then fgc% = fgc% + 8	' intensity
    if (attr% and 128) <> 0 then fgc% = fgc% + 16	' blink
    
    bgc% = attr% \ 16
    
end sub





function scr.attr2fgc%( attr% )
    local fgc%
     
     fgc% = attr% mod 8
     
     if (attr% and 8)   <> 0 then fgc% = fgc% + 8	' intensity
     if (attr% and 128) <> 0 then fgc% = fgc% + 16	' blink
     
     scr.attr2fgc% = fgc%
     
end function





function scr.attr2bgc%( attr% )
    local bgc%
     
     bgc% = attr% \ 16
     
     scr.attr2bgc% = bgc% mod 8
     
end function





sub scr.attr2all.fn( attr%, afgc%, abgc%, inten%, blink% )
    
    afgc% = attr% mod 8			' extract fore ground color
    
    abgc% = attr% \ 16			' extract back ground color
    abgc% = abgc% mod 8
    
    inten% = 0
    blink% = 0
    
    if (attr% and 8)   <> 0 then inten% = -1	' intensity
    if (attr% and 128) <> 0 then blink% = -1	' blink
    
end sub





sub scr.attr2ansi.fn( attr%, afgc%, abgc%, inten%, blink% )
    
    afgc% = attr% mod 8			' extract fore ground color
    

    select case afgc%			' ansi filter
     case 1 : afgc% = 4
     case 3 : afgc% = 6
     case 4 : afgc% = 1
     case 6 : afgc% = 3
    end select

    
    abgc% = attr% \ 16			' extract back ground color
    abgc% = abgc% mod 8
    

    select case abgc%			' ansi filter
     case 1 : abgc% = 4
     case 3 : abgc% = 6
     case 4 : abgc% = 1
     case 6 : abgc% = 3
    end select

    
    inten% = 0
    blink% = 0
    
    if (attr% and 8)   <> 0 then inten% = -1	' intensity
    if (attr% and 128) <> 0 then blink% = -1	' blink
    
end sub





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
        case 0, 8		' clear color
          fgc% = 7
          bgc% = 0
          hilite% = 0
          blink% = 0
          
        case 1			' intensity
          hilite% = -1
          
        case 2
          hilite% = 0
          
        case 5			' blink
          blink% = -1
          
        case 30 to 37		' fore ground
          c% = c% - 30
          

          select case c%
           case 1: fgc% = 4
           case 3: fgc% = 6
           case 4: fgc% = 1
           case 6: fgc% = 3
           case else: fgc% = c%
          end select

          
        case 40 to 47		' back ground
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





