sub config
   shared wmfgc%, wmbgc%
   local menu$
   static oldmenu%
   shared ayuda.min%, ayuda.max%

     dim menu$(1:5)

    $if %english
    menu$(1) = "Change colors"
    menu$(2) = "Change macros"
    $else
    menu$(1) = "Modificar colores"
    menu$(2) = "Modificar macros "
    $endif
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





sub config.colors 					shared

   local sino$
   local cy%, cx%
   local menu%
   local menu$()
   local modif%
   static oldmenu%

     dim menu$(1:15)


    $if %english
    menu$(1)  = "A  Screen Input - Input color "
    menu$(2)  = "B  Screen Input - Output color"
    menu$(3)  = "C  Menu line"
    menu$(4)  = "D  Menu window"
    menu$(5)  = "E  Submenu window"
    menu$(6)  = "F  Directory window"
    menu$(7)  = "G  Selection window"
    menu$(8)  = "H  Input window"
    menu$(9)  = "I  Error messages window"
    menu$(10) = "J  Messages window (RETURN)"
    menu$(11) = "K  Messages window (Time)"
    menu$(12) = "L  Selection colors window"
    menu$(13) = "M  Mark block window"
    menu$(14) = "N  Default colors (Color)"
    menu$(15) = "O  Default colors (Monochrome)"
    sino$ = " Do you want to write the changes ?"
    $else
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
    $endif

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
	 case 1: exit loop	' esc
	 case 2,5: decr p%	' up, left
	 case 0,3,4: incr p%	' right, down
       end select

       macro$(op%) = macro$

    loop while p% < 10 and p% > -1

    call wclose

end sub











sub config.colors.put					shared

   local file%
   local text$


    $if %english
    text$ = "Colors file"
    $else
    text$ = "Fichero de colores"
    $endif

     call config.colors.open(file%)

    lset titulo$  	=  mprograma$+ chr$(13)+ text$+ chr$(26)
    lset ifgc$    	=  mki$(ifgc%)		'Iutput Fore Ground Color
    lset ibgc$    	=  mki$(ibgc%)		'Iutput Back Ground Color
    lset ofgc$    	=  mki$(ofgc%)    	'Output Fore Ground Color
    lset obgc$    	=  mki$(obgc%)          'Output Back Ground Color
    lset wlmfgc$  	=  mki$(wlmfgc%)	'Window Present F.G.C.
    lset wlmbgc$  	=  mki$(wlmbgc%)	'Window Present B.G.C.
    lset wmfgc$   	=  mki$(wmfgc%)		'Window Menu Fore Ground Color
    lset wmbgc$   	=  mki$(wmbgc%)		'Window Menu Back Ground Color
    lset wsmfgc$  	=  mki$(wsmfgc%)	'Window SubMenu Fore Ground Color
    lset wsmbgc$  	=  mki$(wsmbgc%)	'Window SubMenu Back Ground Color
    lset wsfgc$         =  mki$(wsfgc%)
    lset wsbgc$		=  mki$(wsbgc%)
    lset wdfgc$   	=  mki$(wdfgc%)		'Window Directory Fore Ground Color
    lset wdbgc$   	=  mki$(wdbgc%)		'Window Directory Back Ground Color
    lset wifgc$   	=  mki$(wifgc%)		'Window Input Fore Ground Color
    lset wibgc$   	=  mki$(wibgc%) 	'Window Input Back Ground Color
    lset wefgc$   	=  mki$(wefgc%)		'Window Error Fore Ground Color
    lset webgc$   	=  mki$(webgc%)		'Window Error Back Ground Color
    lset wmessagefgc$ 	=  mki$(wmessagefgc%)	'Window WMessage Fore Ground Color
    lset wmessagebgc$ 	=  mki$(wmessagebgc%)	'Window WMessage Back Ground Color
    lset wmsgfgc$     	=  mki$(wmsgfgc%)	'Window WMSG Fore Ground Color
    lset wmsgbgc$     	=  mki$(wmsgbgc%)	'Window WMSG Back Ground Color
    lset wcolorfgc$   	=  mki$(wcolorfgc%)	'Window WColor Fore Ground Color
    lset wcolorbgc$   	=  mki$(wcolorbgc%)	'Window WColor Back Ground Color
    lset woattrib$    	=  mki$(woattrib%)	'Atribute for window of bloques

    put #file%,1

    close #file%

end sub





sub config.colors.get					shared

   local file%
    file% = freefile

     call config.colors.open(file%)

    get #file%,1

    ifgc%   	=  cvi(ifgc$)			'Iutput Fore Ground Color
    ibgc%   	=  cvi(ibgc$)			'Iutput Back Ground Color
    ofgc%   	=  cvi(ofgc$)       	        'Output Fore Ground Color
    obgc%   	=  cvi(obgc$)               	'Output Back Ground Color
    wlmfgc%	=  cvi(wlmfgc$)			'Window Present F.G.C.
    wlmbgc% 	=  cvi(wlmbgc$)			'Window Present B.G.C.
    wmfgc%  	=  cvi(wmfgc$)			'Window Menu Fore Ground Color
    wmbgc%  	=  cvi(wmbgc$)			'Window Menu Back Ground Color
    wsmfgc% 	=  cvi(wsmfgc$)			'Window SubMenu Fore Ground Color
    wsmbgc% 	=  cvi(wsmbgc$)			'Window SubMenu Back Ground Color
    wsfgc%	=  cvi(wsfgc$)
    wsbgc%	=  cvi(wsbgc$)
    wdfgc%  	=  cvi(wdfgc$)			'Window Directory Fore Ground Color
    wdbgc%  	=  cvi(wdbgc$)			'Window Directory Back Ground Color
    wifgc%  	=  cvi(wifgc$)			'Window Input Fore Ground Color
    wibgc%  	=  cvi(wibgc$) 			'Window Input Back Ground Color
    wefgc%  	=  cvi(wefgc$)			'Window Error Fore Ground Color
    webgc%  	=  cvi(webgc$)			'Window Error Back Ground Color
    wmessagefgc%=  cvi(wmessagefgc$)		'Window WMessage Fore Ground Color
    wmessagebgc%=  cvi(wmessagebgc$)		'Window WMessage Back Ground Color
    wmsgfgc%    =  cvi(wmsgfgc$)		'Window WMSG Fore Ground Color
    wmsgbgc%    =  cvi(wmsgbgc$)		'Window WMSG Back Ground Color
    wcolorfgc%  =  cvi(wcolorfgc$)     		'Window WColor Fore Ground Color
    wcolorbgc%  =  cvi(wcolorbgc$)		'Window WColor Back Ground Color
    woattrib%   =  cvi(woattrib$)		'Atribute for window of bloques

     call set.einputcolor(ifgc%, ibgc%)
     call set.eoutputcolor(ofgc%, obgc%)

    close file%

end sub





sub config.colors.default.color				shared

    '------------------------ COLORES DEL ENTORNO --------------------------------
    ifgc%   	= 4		'Input Fore Ground Color
    ibgc%   	= 2  	       	'Input Back Ground Color
    ofgc%   	= 2      	'Output Fore Ground Color
    obgc%   	= 4             'Output Back Ground Color
    wlmfgc% 	= 14		'Window Present F.G.C.
    wlmbgc% 	= 1		'Window Present B.G.C.
    wmfgc%  	= 7		'Window Menu Fore Ground Color
    wmbgc%  	= 1		'Window Menu Back Ground Color
    wsmfgc% 	= 1		'Window SubMenu Fore Ground Color
    wsmbgc% 	= 3		'Window SubMenu Back Ground Color
    wsfgc%      = 1
    wsbgc%      = 7
    wdfgc%  	= 14		'Window Directory Fore Ground Color
    wdbgc%  	= 5 		'Window Directory Back Ground Color
    wifgc%  	= 15		'Window Input Fore Ground Color
    wibgc%  	= 7 		'Window Input Back Ground Color
    wefgc%  	= 12		'Window Error Fore Ground Color
    webgc%  	= 4		'Window Error Back Ground Color
    wmessagefgc%= 14		'Window WMessage Fore Ground Color
    wmessagebgc%= 7   		'Window WMessage Back Ground Color
    wmsgfgc%    = 15		'Window WMSG Fore Ground Color
    wmsgbgc%    = 3		'Window WMSG Back Ground Color
    wcolorfgc%  = 15          	'Window WColor Fore Ground Color
    wcolorbgc%  = 7		'Window WColor Back Ground Color
    woattrib%   = 32		'Atribute for window of bloques
    '-----------------------------------------------------------------------------
     call set.einputcolor(ifgc%, ibgc%)
     call set.eoutputcolor(ofgc%, obgc%)

end sub





sub config.colors.default.mono				shared

    '------------------------ COLORES DEL ENTORNO --------------------------------
    ifgc%   	= 0		'Input Fore Ground Color
    ibgc%   	= 7  	       	'Input Back Ground Color
    ofgc%   	= 7      	'Output Fore Ground Color
    obgc%   	= 0             'Output Back Ground Color
    wlmfgc% 	= 0		'Window Present F.G.C.
    wlmbgc% 	= 7		'Window Present B.G.C.
    wmfgc%  	= 0		'Window Menu Fore Ground Color
    wmbgc%  	= 7		'Window Menu Back Ground Color
    wsmfgc% 	= 0		'Window SubMenu Fore Ground Color
    wsmbgc% 	= 7		'Window SubMenu Back Ground Color
    wsfgc%      = 0
    wsbgc%      = 7
    wdfgc%  	= 0		'Window Directory Fore Ground Color
    wdbgc%  	= 7 		'Window Directory Back Ground Color
    wifgc%  	= 0		'Window Input Fore Ground Color
    wibgc%  	= 7 		'Window Input Back Ground Color
    wefgc%  	= 0		'Window Error Fore Ground Color
    webgc%  	= 7		'Window Error Back Ground Color
    wmessagefgc%= 0		'Window WMessage Fore Ground Color
    wmessagebgc%= 7   		'Window WMessage Back Ground Color
    wmsgfgc%    = 0		'Window WMSG Fore Ground Color
    wmsgbgc%    = 7		'Window WMSG Back Ground Color
    wcolorfgc%  = 7          	'Window WColor Fore Ground Color
    wcolorbgc%  = 0		'Window WColor Back Ground Color
    woattrib%   = 112		'Atribute for window of bloques
    '-----------------------------------------------------------------------------
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





