'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :pincel.ini                                          02-12-1992   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
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


    data "ฺ","ย","ฟ","ร","ล","ด","ภ","ม","ู","ณ","ฤ",""
    data "ษ","ห","ป","ฬ","ฮ","น","ศ","ส","ผ","บ","อ",""
    data "ึ","า","ท","ว","ื","ถ","ำ","ะ","ฝ","บ","ฤ",""
    data "ี","ั","ธ","ฦ","ุ","ต","ิ","ฯ","พ","ณ","อ",""

    'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
    'บ END SUB:pincel.ini                                       02-12-1992   บ
    'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :pincel.putchr                                       02-12-1992   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    ypos%          ===>                                                บ
'บ    xpos%          ===>                                                บ
'บ    way$           ===>                                                บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
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


    '**** Caracteres Centrales *****
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
    '****

    mputchr:

    p% = asc( mchr$(mchr%, y%, x%) )

   end select

   if kbd.ins% then				' if insert
      dat$ = scr.get.chrattr$(ypos%, xpos%, 80+1-xpos%)
      dat$ = left$(dat$,len(dat$)-2)
      m% = ((ypos%-1)*160) + (xpos% * 2)
      poke$ m%, dat$
   end if

   m% = (160*(ypos%-1)) + (2*(xpos%-1))
   if p% <> 255 then poke m%, p%
   attr% = scr.color2attr%(fgc%,bgc%)
   if attr% then poke m%+1, attr%

    'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
    'บ END SUB:pincel.putchr                                    02-12-1992   บ
    'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ FUNCTION :pincel.getchr%                                 02-12-1992   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    (yp%           ===>                                                บ
'บ    xp%            ===>                                                บ
'บ    way$           ===>                                                บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ    pincel.getchr% ===>                                                บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
function pincel.getchr% (yp%, xp%, way$)
    shared mchr$()
    shared mchr%
     pincel.getchr% = 0 		'False
    local p%
    local mpos%()


      dim mpos%(1:10)


     mpos%(1) = mchr%             'firts check same actual graph.char.

     scrchr$ = chr$(peek(160 * (yp% - 1) + 2 * (xp% - 1)))


     select case lcase$(way$)

      case "up"             'Comprobar Arriba


        select case mchr%    'second check
	 case 1 : mpos%(2) = 4
	 case 2 : mpos%(2) = 3
	 case 3 : mpos%(2) = 2
	 case 4 : mpos%(2) = 1
        end select


        if yp% > 0 then


           for p% = 1 to 2


              for y% = 1 to 2


                 for x% = 1 to 3


                    if scrchr$ = mchr$(mpos%(p%), y%, x%) then 'Si caracter superior
                       pincel.getchr% = -1
                       exit function
                    end if


                 next x%


              next y%


           next p%



           if scrchr$ = mchr$(mpos%(1), 4, 1) then       'Si caracter vertical
              pincel.getchr% = -1
              exit function

           end if


        end if


      case "down"           'Comprobar Abajo


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
                    

                    if scrchr$ = mchr$(mpos%(p%), y%, x%) then 'Si caracter inferior
                       pincel.getchr% = -1
                       exit function
                    end if

                    
                 next x%

                 
              next y%

              
           next p%

           

           if scrchr$ = mchr$(mpos%(1), 4, 1) then     'Si caracter vertical
              pincel.getchr% = -1
              exit function
           end if

           
        end if

        
      case "left"           'Coprovar Izquierda


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
                    

                    if scrchr$ = mchr$(mpos%(p%), y%, x%) then 'Si caracter izquierda
                       pincel.getchr% = -1
                       exit function
                    end if

                    
                 next x%

                 
              next y%

              
           next p%

           

           if scrchr$ = mchr$(mpos%(1), 4, 2) then     'Si caracter horizontal
              pincel.getchr% = -1
              exit function
           end if

           
        end if

        
      case "right"          'Coprobar Derecha


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
                    

                    if scrchr$ = mchr$(mpos%(1), y%, x%) then 'Si caracter derecha
                       pincel.getchr% = -1
                       exit function
                    end if

                    
                 next x%

                 
              next y%

              
           next p%

           

           if scrchr$ = mchr$(mpos%(1), 4, 2) then     'Si caracter horizontal
              pincel.getchr% = -1
              exit function
           end if

           
        end if

        
     end select

     
     'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
     'บ END DEF:pincel.getchr%                                   02-12-1992   บ
     'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end function





