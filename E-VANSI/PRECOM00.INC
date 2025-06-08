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
sub meexit
    print #1, "E-VANSI ( View ANSI )  v1.2   Copyright (c)1.992   Esteve Valent¡ & Psicosoft"
    print #1,
    print #1, " This utility can be included in Screen Editor E-SCREEN"
    print #1, " Can be useful to view one or more ANSI files without the"
    print #1, " ANSI.SYS DOS driver loaded into memory necesarly"
    print #1, "
    print #1, " The program find the files to view and sort ascendig his names"
    print #1, " You must be remember this wen be in use E-VANSI"
    print #1, "
    print #1, " With this utility can be possible make animations viewing"
    print #1, " some ANSI files"
    print #1, "
    print #1, " Press any key to continue..."
    a$ = input$(1)
    print #1,
    print #1, "USAGE:"
    print #1,
    print #1, "    E-VANSI [d:] [\path\] file [.ext]"
    print #1, "
    print #1, "      Optional into []"
    print #1, "      d:     = driver"
    print #1, "      \path\ = directory"
    print #1, "      file   = file name or search mask (same of DOS use)"
    print #1, "      .ext   = extension (default is .ANS)"
    print #1, "
    print #1, "EXAMPLES:
    print #1, "
    print #1, "    E-VANSI cataguai.ans"
    print #1, "    E-VANSI c:\ansi\*.ans"
    print #1, "    E-VANSI e-screen.ans"
    print #1,
    print #1, " Press any key to continue..."
    a$ = input$(1)
    print #1,
    print #1,
    print #1, "SHAREWARE:"
    print #1, "
    print #1, "    This program isn't Public Domain or FreeWare"
    print #1, "    If you want to get the font code of E-VANSI in"
    print #1,
    print #1, "			Power Basic 2.x"
    print #1, "			MS Basic PDS 7.x"
    print #1, "			Quick Basic 4.x"
    print #1, "			Turbo Basic 1.x"
    print #1,
    print #1, "    send a 42$ check paiable to Esteve Valent¡ to:"
    print #1, "
    print #1, "       PsicoSoft C.B."
    print #1, "       Esteve Valent¡"
    print #1, "       Grup Sot dels Canyers, 6/60"
    print #1, "       17220 - Sant Feliu de Gu¡xols"
    print #1, "       Girona - Catalunya - Spain"
    print #1, "       FAX....: 34-72 20.34.68 "
    print #1, "
    print #1, "       RIME Net   BBS: ->ABAFORUM"
    print #1, "                 AREA: QBASIC, SHAREWARE"
    print #1, "       INTERNET: esteve.valenti@abaforum.es"
    print #1, "       FIDO Net: 2:343/106.11"
    print #1,
    end:
end sub
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
function scr.columns%
    local columns%
     reg 1, &h0f00
      call interrupt &h10
     columns% = reg(1) \ 256
     scr.columns% = columns%
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
