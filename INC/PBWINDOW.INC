'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :wopen                                               12-25-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    (boty%         ===>                                                บ
'บ    botx%          ===>                                                บ
'บ    topy%          ===>                                                บ
'บ    topx%          ===>                                                บ
'บ    bord%          ===>                                                บ
'บ    type%          ===>                                                บ
'บ    mwfgc%         ===>                                                บ
'บ    mwbgc%         ===>                                                บ
'บ    titulo$        ===>                                                บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
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
       d$(1)="ฺ":d$(2)="ฤ":d$(3)="ฟ":d$(4)="ณ":d$(5)="ภ":d$(6)="ู":d$(7)="ร":d$(8)="ด"
     case  2
       d$(1)="ษ":d$(2)="อ":d$(3)="ป":d$(4)="บ":d$(5)="ศ":d$(6)="ผ":d$(7)="ฬ":d$(8)="น"
     case  3
       d$(1)="ี":d$(2)="อ":d$(3)="ธ":d$(4)="ณ":d$(5)="ิ":d$(6)="พ":d$(7)="ฦ":d$(8)="ต"
     case  4
       d$(1)="ึ":d$(2)="ฤ":d$(3)="ท":d$(4)="บ":d$(5)="ำ":d$(6)="ฝ":d$(7)="ว":d$(8)="ถ"
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
    
    'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
    'บ END SUB:wopen                                            12-25-1991   บ
    'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub





function wlmenu%(menu$())
    shared wlmfgc%		' Fore Ground Color
    shared wlmbgc%		' Back Ground Color
    local lastmenu%		' number of options in menฃ
    local fastmenu$		' first letter of menu
    local part%			' division of line by lastmenu%
    local x%, columns%
    static p%
    static linea$
     
     if p% = 0 then p% = 1
     columns% = scr.columns%


     if linea$ = "" then
        linea$ = scr.chrattr$(1,1,columns%)	' get line of screen
     end if


     locate 1,1
     color wlmfgc%, wlmbgc%
     print space$(columns%);			' print blank line


     do while menu$(lastmenu%+1) <> ""
        incr lastmenu%                            ' get number of menฃ options
        fastmenu$ = fastmenu$ + left$(menu$(lastmenu%),1)	' get first letters
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

         case chr$(&h00,&h50), chr$(&h0d)	' CUR_DOWN or RETURN
           wlmenu% = p%
           exit function

         case chr$(&h00,&h4b)			' CUR_LEFT

           if p% > 1 then
              decr p%
           else
              p% = lastmenu%
           end if

         case chr$(&h00,&h4d)			' CUR_RIGHT

           if p% < lastmenu% then
              incr p%
           else
              p% = 1
           end if

         case chr$(&h1b)			' ESC
           wlmenu% = 0
            call scr.defseg
           poke$ 0, linea$
           linea$ = ""
           exit function

         case > ""				' OTHERWISE
           x% = instr(fastmenu$, ucase$(t$))

           if x% > 0 then
              p% = x%
	       call kbd.putkey(&h0d, &h1c)
           end if

        end select


     loop while -1


end function





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :wclose                                              12-25-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
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


    'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
    'บ END SUB:wclose                                           12-25-1991   บ
    'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :wtext                                               12-25-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    text$          ===>                                                บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub wtext(text$)
   shared wind$()
   shared wnum%
   local botx%
   local boty%
   local topx%
   local topy%
   local type%
   local mwfgc%
   local mwbgc%
   local iv%
   local ipos%
    
    ipos% = 1
    pa% = 1
    botx% = cvi(mid$(wind$(wnum%,2),1,2))
    boty% = cvi(mid$(wind$(wnum%,2),3,2))
    topx% = cvi(mid$(wind$(wnum%,2),5,2))
    topy% = cvi(mid$(wind$(wnum%,2),7,2))
    type% = cvi(mid$(wind$(wnum%,2),9,2))
    mwfgc%  = cvi(mid$(wind$(wnum%,2),11,2))
    mwbgc%  = cvi(mid$(wind$(wnum%,2),13,2))
    color mwfgc%,mwbgc%
    if type% = 1 or type%=2 then d% = 3 else d% = 1


    do
       

       if type% = 1 or type%=2 then
          locate boty%+1,topx%-12
          print using "P gina ##";pa%
       else
          locate boty%,topx%-14
          print chr$(17);
          color mwbgc%,mwfgc%
          print using "P gina ##";pa%;
          color mwfgc%,mwbgc%
          print chr$(16)
       end if



       for y% = boty%+d% to topy%-d%
          locate y%,botx%+2:? spc(topx%-3-botx%);
          locate y%,botx%+2:? mid$(text$,ipos%,topx%-3-botx%);
          ipos% = ipos% + topx%-3-botx%
       next y%


       a$ = inkey$


       if a$=chr$(&h00,&h49) and ipos%>=((topx%-3-botx%)*(topy%-(d%*2-2)-boty%)) then
          ipos% = ipos% -((topx%-3-botx%)*(topy%-(d%*2-1)-boty%)*2)
          pa% = pa% - 1
       end if



       if a$=chr$(&h00,&h51) and mid$(text$,ipos%,topx%-3-botx%)<>"" then
          pa% = pa% + 1
       end if


    loop until a$=chr$(&h1b)


    'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
    'บ END SUB:wtext                                            12-25-1991   บ
    'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub







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







sub set.wmenu.leftrighton
   shared wmenu.leftright%
    wmenu.leftright% = -1
end sub



'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :wmenu                                               12-25-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    menu$(1)       ===>                                                บ
'บ    keytype%       ===>                                                บ
'บ    ip%            ===>                                                บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
function wmenu%( menu$(1), ipos% )
   shared wind$(),wnum%
   shared wmenu.keytype%
   shared wmenu.leftright%		' To active LEFT_CUR and RIGHT_CUR

    botx% = cvi(mid$(wind$(wnum%,2),1,2))
    boty% = cvi(mid$(wind$(wnum%,2),3,2))
    topx% = cvi(mid$(wind$(wnum%,2),5,2))
    topy% = cvi(mid$(wind$(wnum%,2),7,2))
    type% = cvi(mid$(wind$(wnum%,2),9,2))
    mwfgc%  = cvi(mid$(wind$(wnum%,2),11,2))
    mwbgc%  = cvi(mid$(wind$(wnum%,2),13,2))
    iv%   = cvi(mid$(wind$(wnum%,2),15,2))

    do  				'Busca si se detectar  tecla rapida
       incr fv%,1			'Final Variable
       l$ = menu$(fv%)
       l$ = ucase$(left$(ltrim$(l$),1))
       opcio$=opcio$+l$			'Teclado Abreviado
    loop while menu$(fv%) <> ""

    if type% = 1 or type%=2 then d%=3 else d% = 1	'Margen superior inferior
    lenwind% = (topy%-d%-boty%-d%+1)

    if ipos% = 0 then ipos% = 1				'Si se acaba de abrir

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
        case chr$(&h00,&h4b)			' LEFT

          if wmenu.leftright% then  		' only if first level
              call kbd.putkey(&h1b, &h01)
              call kbd.putkey(&h00, &h4b)
              call kbd.putkey(&h0d, &h1c)
          end if

        case chr$(&h00,&h4d)			' RIGHT

          if wmenu.leftright% then		' only if first level
              call kbd.putkey(&h1b, &h01)
              call kbd.putkey(&h00, &h4d)
              call kbd.putkey(&h0d, &h1c)
          end if

	case chr$(&h00,&h47)                    ' HOME
	  ipos% = 1

	case chr$(&h00,&h4f)			' END
	  ipos% = fv% - 1

        case  chr$(&h00,&h48)			'***CURSOR ARRIBA
          decr ipos%,1

        case  chr$(&h00,&h50)			'***CURSOR ABAJO
          incr ipos%,1

	case  chr$(&h00,&h49)			'***PAGUP
	  decr ipos%,lenwind%
	  if ipos% < 1 then ipos% = 1
        case  chr$(&h00,&h51)			'***PAGDW
	  incr ipos%,lenwind%
	  if ipos% > fv% -1 then
	     ipos% = fv% -1
	  end if
	case  chr$(&h0d)			'*** INTRO
	  wmenu% = ipos%
	  wind$(wnum%,2)=left$(wind$(wnum%,2),14)+mki$(iv%)
	  exit loop

        case > ""				'*** TECLA ABREVIATURA

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
                color mwbgc%,7				'Color de la BARRA
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





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ DEF FN:wdir$                                             12-25-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    (boty%         ===>                                                บ
'บ    botx%          ===>                                                บ
'บ    bord%          ===>                                                บ
'บ    mask$      ===>                                                บ
'บ    typesort%      ===>                                                บ
'บ    lendir%        ===>                                                บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
function wdir$(boty%,botx%,bord%,mask$,sattr%,typesort%,lendir%)
    shared f$
    shared wmenu.keytype%
    shared wdfgc%,wdbgc%
    local  rdir$()				'Declara estatico el array para el


      dim dynamic rdir$(1:2048)


     select case typesort%
      case 1 : xsort% = 10			'Ordenado por Extensiขn
      case 2 : xsort% = 13        		'Ordenado por Bytes
      case 3 : xsort% = 23			'Ordenado por Fecha
      case 4 : xsort% = 36			'Ordenado por Hora:Minuto
      case else : xsort% = 1			'Ordenado por Nombre
     end select

     select case lendir%
      case  1 : topx% = 25			'Muestra hasta Bytes
      case  2 : topx% = 38			'Muestra hasta Fecha
      case  3 : topx% = 51			'Muestra hasta Hora:Minuto
      case else : topx% = 15			'Muestra hasta Extensiขn
     end select

     topx% = topx%+botx%

      call wtestcolor(wdfgc%,wdbgc%)

      call wopen(boty%,botx%,boty%+15,topx%,bord%,1,wdfgc%,wdbgc%,mask$)


     do

        locate boty%+1,botx%+1			' print mask$
        color wdfgc%,wdbgc%
        print space$(topx%-1-botx%);

        if len(mask$) < topx%-4-botx% then
           locate boty%+1,botx%+int(((topx%-botx%)-len(mask$))/2)
           print mask$;
        else
           locate boty%+1,botx%+1
           print right$(mask$,topx%-1-botx%);
        end if

	call readdir(mask$,sattr%,rdir$(),lendir%,e%)	'Lee directorio

        '******************* Ordena el Directori ***********************
        p% = 0
        while rdir$(p%+1)<>"": incr p% : wend
        if p% > 1 then
           array sort rdir$() for p%, from xsort% to len(rdir$(p%))
        end if
        '***************************************************************

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

     'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
     'บ END DEF:wdir$                                            12-25-1991   บ
     'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end function





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :wprint                                              12-25-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    y%             ===>                                                บ
'บ    x%             ===>                                                บ
'บ    text$          ===>                                                บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
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

    'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
    'บ END SUB:wprint                                           12-25-1991   บ
    'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
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





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :wmessage                                            12-25-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    (boty%         ===>                                                บ
'บ    botx%          ===>                                                บ
'บ    text$          ===>                                                บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub wmessage( boty%, botx%, text$ )
   shared wmessagefgc%,wmessagebgc%
    
    $if %english
    text$ = text$ + " Press RETURN"
    $else
    text$ = text$ + " Pulsa INTRO"
    $endif
    
     call wtestcolor(wmessagefgc%,wmessagebgc%)
    
    by% = boty%+12
    bx% = (40-(len(text$)/2))-2
    ty% = boty%+14
    tx% = (40+(len(text$)/2))+2
     call wopen( by%, bx%, ty%, tx%, 2, 0, wmessagefgc%, wmessagebgc%, "" )
     call wprint(1,0,text$)
    

    do
       a$ = ucase$(inkey$)
       '       if a$<>chr$(&h0d) and a$<>"" then call ebeep
    loop until a$ = chr$(&h0d)

    
     call wclose
    'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
    'บ END SUB:wmessage                                         12-25-1991   บ
    'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :wmsg                                                12-25-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    (boty%         ===>                                                บ
'บ    botx%          ===>                                                บ
'บ    text$          ===>                                                บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
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





function wsino%( boty%, botx%, text$, e% )
    local default$
    local filter$
    local resp$
     
     $if %english
     mtext$   = text$ + " (Y\N) "
     default$ = "Y"
     filter$  = "YNyn"
     $else
     mtext$   = text$ + " (S\N) "
     default$ = "S"
     filter$  = "SNsn"
     $endif
     
      call set.weinput.default( default$ )
      call set.ekbdfilter( filter$ )
     resp$ = weinput$( 0, 0, mtext$, "", 1, 1, e% )

     if e% = 0 then
        if resp$ = default$ then wsino% = -1
     end if


end function





sub wcolor( boty%, botx%, wfgc%, wbgc% )
   shared e%
   shared wcolorfgc%,wcolorbgc%
    e% = 0
    fgc% = wfgc%
    bgc% = wbgc%
    mfgc% = -1
    mbgc% = -1

    $if %english
    msg0$ = "   Same of Screen   "
    msg1$ = "   Selected Color   "
    $else
    msg0$ = " El de la Pantalla  "
    msg1$ = " Color Seleccionado "
    $endif

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
        case chr$(&h00,&h48):decr bgc%, 1	' Up
        case chr$(&h00,&h50):incr bgc%, 1	' Down
        case chr$(&h00,&h4b):decr fgc%, 1	' Left
        case chr$(&h00,&h4d):incr fgc%, 1	' Right
        case chr$(&h0d)				' Intro
          wfgc% = fgc%
          wbgc% = bgc%
          exit loop
        case chr$(&h1b)				' Esc
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

    $if %english
       text$ = " Decimal ASCII = ###        Character ASCII = \ \"
    $else
       text$ = " Decimal ASCII = ###        Caracter ASCII = \ \"
    $endif


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
	 case chr$(&h00,&h48):decr y%,2		' Up
	 case chr$(&h00,&h50):incr y%,2		' Down
	 case chr$(&h00,&h4b):decr x%,2		' Left
	 case chr$(&h00,&h4d):incr x%,2		' Right
	 case chr$(&h0d)			' Intro
	   mascii% = ((y% * 32 ) + x% ) \ 2
	   wascii$ = mascii$(mascii%)
           exit loop
	end select

     loop until t$ = chr$(&h1b)

      call wclose

end function




'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :wsavewindows                                        12-25-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub wsavewindows
   shared wnum%,wind$()
    incr wnum%,1
     call wsavescreen(wind$(wnum%,1),1,1,25,80)
end sub





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :wrestorewindows                                     12-25-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub wrestorewindows
   shared wnum%,wind$()
     call wrestorescreen(wind$(wnum%,1),1,1,25,80)
    wind$(wnum%,1) = ""
    wind$(wnum%,2) = ""
    decr wnum%,1
end sub





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :wsavescreen                                         12-25-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    (var$          ===>                                                บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub wsavescreen(var$, boty%, botx%, topy%, topx% )
   local y%, l%
     call scr.defseg
    
    l% = (topx% + 1 - botx%) * 2
    
    for y% = boty% to topy%
       m% = (160 * (y%-1)) + (2 * (botx%-1))
       var$ = var$ + peek$(m%, l%)
    next y%

    
end sub





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :wrestorescreen                                      12-25-1991   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    var$           ===>                                                บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
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





sub wtestcolor(fgc%,bgc%)

    if fgc% = 0 and bgc% = 0 then
       fgc% = 0
       bgc% = 7
    end if

end sub





