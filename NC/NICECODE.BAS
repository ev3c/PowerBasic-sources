
REM  This is a litle part of NiceCode.BAS to look the type of Source Code.
REM  If you are interest to buy all code send the REGISTER.DOC at
REM  specified adress.

'
' �������������������������������������������������������������������������
'�����������������������������������������������������������������������ͻ�
'� Program......: NiceCode.BAS                                           ��
'� Definition...: Source Code Beautify for various languages.            ��
'� Author.......: NiceCode.EXE v1.0�     (c) Esteve Valent� 1.991        ��
'� Date.........: 12-26-1991                                             ��
'� Notice.......: ShareWare product.                                     ��
'� Notes........:                                                        ��
'� Version......: 1.0�                                                   ��
'� History......: NiceCode.HIS                                           ��
'�����������������������������������������������������������������������͹�
'� OBSERVATIONS:                                                         ��
'�                                                                       ��
'�   split.commad$ function split the command line and initialize some   ��
'�   variables.                                                          ��
'�   get.nc.file sub get instructions of the .nc file specified in       ��
'�   command line.                                                       ��
'�   nc sub is the main sub and all others subs and functions can be     ��
'�   calleds from this and his names are referenceds with previus        ��
'�   sub or function.                                                    ��
'�     i.e.:      nc.otab$                                               ��
'�                nc.findfile%                                           ��
'�                nc.rembox                                              ��
'�                nc.rembox.put                                          ��
'�����������������������������������������������������������������������ͼ
ont# = timer

open "cons:" for output as #1
print #1,
print #1, "NICECODE.EXE  v1.0     Copyright (c) 1992 - PsicoSoft & Esteve Valent�"
print #1,
ifile$ = split.command$( ucase$( command$ ))
 call get.nc.file
 call nc(ifile$)
print #1,
print #1, "    Time used....: ";timer-ont#

end:

$include "PBSTR   .INC"			' Power Basic String library
$include "PBSCR   .INC"			' Power Basic Screen library
$include "PBGETDIR.INC"			' Power Basic Files  library
$include "NCREMBOX.INC"			' NiceCode Remember Boxes
$include "NCSPLITL.INC"			' NiceCode Split Source Code Lines

'
'�����������������������������������������������������������������������ͻ
'� SUB :nc                                                  12-26-1991   �
'�����������������������������������������������������������������������͹
'� ACTION: Main SUB of NiceCode.BAS                                      �
'�         Beautify the Source Code of file specified in ifile$          �
'�                                                                       �
'� TO PASS:                                                              �
'�    ifile$         ===> name of main file to beautify                  �
'�                                                                       �
'�����������������������������������������������������������������������ͼ
sub nc(ifile$)
   shared linc$, rinc$			' include istruction and commas
   shared lrem$, rrem$			' rem identifiers
   shared trun$				' truncate line identification
   shared proext$, incext$		' program and libraryes extension
   shared disk%				' write to disk or screen
   shared disk.bak%			' delete .BAK
   shared minc%				' include files
   shared rembox%			' remember boxes
   shared mtab%				' spaces at left
   shared muca%, mlca%			' upper case or lower case
   shared disk.sec%     		' put NiceCode security
   local  ifile%, ofile%		' input output file numbers
   local  ofile$			' output file name
   local  linst$
   local  linea$,linea.ins$, linea.rem$	' to hold line, instructions and rems
   local  otab%				' old spaces at left
   local  typefile%, p%			' type of file remember box
   local  protected%, blank%		' if file was protected -1
   shared  inc$(), inc%			' to hold include file names
    

     redim inc$(1000)
    
    incr inc%
    inc$(1) = ifile$
    

    do while inc$(p%+1) <> ""
       incr p%
       file% = nc.findfile%( inc$(p%), path$, f$, e$ )
       

       if file% then					'file finded
          
          ifile$ = path$+f$+e$
          ofile$ = path$+f$+".NEW"
          bfile$ = path$+f$+".BAK"
          ifile% = freefile
          open ifile$  for input as ifile%
          blank% = -1
          

          if inc% = 1 and e$ = proext$ then
             typefile% = 1				'main
          elseif instr(incext$, e$ ) then		'library
             typefile% = 2
          end if
          

          if disk% then					' if write to disc
             ofile% = freefile				'  get file number
             open ofile$  for output as ofile%		'  open
          else						' else
             ofile% = 1					'  write to screen
          end if
          

          do while not eof(ifile%)			' if not end of file
             linea.ins$ = ""
             

             do						' get line or lines
                line input #ifile%, linea$
                 call nc.splitlinefn(linea$, lins$, linea.rem$)  '/L,/U,anytime
                

                if right$(rtrim$(lins$,any " "+chr$(9)),1) = trun$ and_
                   trun$ <> "" then
                   if mtab% then lins$ = ltrim$(lins$, any " "+chr$(9))+linea.rem$
                   linea.ins$ = linea.ins$ + lins$ + chr$(13,10)
                else
                   

                   if linea.ins$ <> "" and mtab% then
                      lins$ = ltrim$(lins$, any " "+chr$(9))
                   end if
                   
                   linea.ins$ = linea.ins$ + lins$
                   exit loop
                end if
                
             loop while -1

             

             if blank% then				' if firts line
                

                if len(linea.ins$+linea.rem$) then	' if linea
                   blank% = 0                           '  restore flag
                end if
                

                if instr(ucase$(ltrim$(linea.rem$)),_	
                   lrem$+"NONiceCode") = 1  then
                   print #1,"XXXXX  $INCLUDE ";f$;e$;" file WAS been PROTECTED  "
                   protected% = -1                      '  protected = true
                   exit loop				'  fast exit
                else
                   

                   if disk.sec% and disk% then		'  if security and to disk
                      print #ofile%, lrem$;"NONiceCode";rrem$
                   end if
                   
                end if
                
             end if
             

             if rembox% then				' /R put rem boxes
                 call nc.rembox( ifile%, ofile%, linea.ins$,_
                linea.rem$, typefile%, f$+e$ )
             end if
             

             if minc% then 				' /$ search include file
                 call nc.include(linea.ins$)
             end if
             

             if mtab% then				' /T put tabs in linea.ins$
                linea.ins$ = nc.otab$( linea.ins$, otab% )
             end if
             

             if trim$(linea.ins$+linea.rem$) = "" then
                incr white%
             else
                white% = 0
             end if
             

             if white% <= 1 then
                print #ofile%, linea.ins$;linea.rem$
             end if
             
          loop

          
          close #ifile%
          

          if disk% then					' /D change file names
             close #ofile%
             

             if protected% then				' if NONiceCode
                kill ofile$					'  kill ofile
                protected% = 0				'  restore flag
             else						' else
                

                if disk.bak% then			'  if /DB and .bak
                    call findfile(bfile$,0,f$,sa%,s&,d%,t%,e%)
                   if e% = 0 then kill bfile$           '   kill .BAK
                   name ifile$ as bfile$                '   rename ifile to .BAK
                else                                    '  else
                   kill ifile$				'   kill .BAK
                end if
                
                name ofile$ as ifile$			' rename .NEW as ifile
             end if
             
          end if
          
       end if
       
    loop

    
    '�����������������������������������������������������������������������ͻ
    '� END SUB:nc                                               12-26-1991   �
    '�����������������������������������������������������������������������ͼ
end sub





'
'�����������������������������������������������������������������������ͻ
'� FUNCTION :nc.otab$                                       12-26-1991   �
'�����������������������������������������������������������������������͹
'� ACTION:                                                               �
'�                                                                       �
'�                                                                       �
'� TO PASS:                                                              �
'�    linea$         ===>                                                �
'�    oldtab%        ===>                                                �
'�                                                                       �
'� RETURN:                                                               �
'�    nc.otab$       ===>                                                �
'�                                                                       �
'�����������������������������������������������������������������������ͼ
function nc.otab$( linea$, oldtab% )
    local p%
    local inst$
    local finst$,einst$
    local leni%
    local inst%
    shared inst$()
    shared odwn%()
    shared otab%()
     linea$  = ltrim$(linea$, any " "+chr$(9) )
     mlinea$ = rtrim$(linea$, any " "+chr$(9) )+chr$(13,10)
     mlinea$ = ucase$(mlinea$)
     

     if mlinea$ <> chr$(13,10) then
        

        do while inst$(p%+1) <> ""
           incr p%
           inst$ = ucase$(inst$(p%))
           inf% = instr(inst$,"�")
           leni% = len(inst$)
           

           if inf% > 0 then  				' if two instructions
              finst$ = left$ (inst$,inf%-1)		'  split firts inst.
              einst$ = right$(inst$,leni%-inf%)		'  split last inst.
              inst$  = finst$
              leni%  = len(inst$)
           end if
           

           if left$(mlinea$,leni%) = inst$ then
              

              if inf% > 0 then				' if two instructions
                 

                 if instr(mlinea$, einst$) > 0 then
                    inst% = -1
                    exit loop
                 end if
                 
              else
                 inst% = -1				' ok, tab% = true
                 exit loop
              end if
              
           end if
           
        loop

        
     end if
     

     if inst% then				' if instruction has finded
        oldtab% = oldtab% + otab%(p%,1)		'  get tab after print
        if oldtab% < 0 then oldtab% = 0		'  look error
        linea$  = space$(oldtab%)+linea$	'  put tab after print
        oldtab% = oldtab% + otab%(p%,2)         '  get tab before print
        replace chr$(13,10) with _
        chr$(13,10)+space$(oldtab%) in linea$	'  look truncate lines
        

        for x% = 1 to odwn%(p%,1)		'  make lines up
           linea$ = chr$(13,10)+linea$
        next x%

        

        for x% = 1 to odwn%(p%,2)		'  make lines down
           linea$ = linea$+chr$(13,10)
        next x%

        
     else
        replace chr$(13,10) with _
        chr$(13,10)+space$(oldtab%) in linea$	'  look truncate lines
        linea$ = space$(oldtab%)+linea$
     end if
     
     nc.otab$ = linea$
     
     '�����������������������������������������������������������������������ͻ
     '� END FUNCTION:nc.otab$                                    12-26-1991   �
     '�����������������������������������������������������������������������ͼ
end function





'
'�����������������������������������������������������������������������ͻ
'� FUNCTION :nc.findfile%                                   12-26-1991   �
'�����������������������������������������������������������������������͹
'� ACTION:                                                               �
'�                                                                       �
'�                                                                       �
'� TO PASS:                                                              �
'�    ifile$         ===>                                                �
'�    path$          ===>                                                �
'�    f$             ===>                                                �
'�    e$             ===>                                                �
'�                                                                       �
'� RETURN:                                                               �
'�    nc.findfile%   ===>                                                �
'�                                                                       �
'�����������������������������������������������������������������������ͼ
function nc.findfile%( ifile$, path$, f$, e$ )
    shared proext$
    shared ncpath$
    local d$
    local p$
    local mfile$
     path$ = ""
     f$ = ""
     e$ = ""
      call splitpathfn(ifile$,d$,p$,f$,e$)
     if e$ = "" then e$ = proext$
     mfile$ = f$+e$
     ifile$ = d$+p$+f$+e$
     

     if d$+p$ = "" then					' if not path then
        ifile$ = searchfiledospath$(ifile$, 0, ncpath$ )'    search in ncpath$
        

        if ifile$ = "" then				'  if not good finded
           ifile$ = searchfiledospath$( ifile$, 0, "" ) '    search in dos path
        end if
        
         call splitpathfn(ifile$, d$, p$, f$, e$)	'  split ifile$
     end if
     
     path$ = d$+p$
     ifile$ = path$+f$+e$
     

     if ifile$ <> "" then
        print #1,"+++++  $INCLUDE ";mfile$;" file HAS finded "
        nc.findfile% = -1
     else
        print #1,"-----  $INCLUDE ";mfile$;" file HASN'T finded   "
     end if
     
     '�����������������������������������������������������������������������ͻ
     '� END FUNCTION:nc.findfile%                                12-26-1991   �
     '�����������������������������������������������������������������������ͼ
end function





'
'�����������������������������������������������������������������������ͻ
'� SUB :nc.include                                          12-26-1991   �
'�����������������������������������������������������������������������͹
'� ACTION:                                                               �
'�                                                                       �
'�                                                                       �
'� TO PASS:                                                              �
'�    linea$         ===>                                                �
'�                                                                       �
'� RETURN:                                                               �
'�                                                                       �
'�����������������������������������������������������������������������ͼ
sub nc.include(linea$)
   shared linc$, rinc$
   shared incl$
   shared inc$(), inc%
   local file$, d$,p$,f$,e$
   local leninc%
   local i%
   local c1%
   local c2%
    mlinea$ = ltrim$(linea$, any " "+chr$(9))
    leninc% = len(incl$)
    i% = instr( ltrim$(ucase$(mlinea$)), incl$)
    

    if i% = 1 then
       c1% = instr(mlinea$,linc$) + 1
       c2% = instr(c1%,mlinea$,rinc$)
       incr inc%
       file$ = mid$( mlinea$, c1%, c2%-c1% )
        call splitpathfn(file$ ,d$,p$,f$,e$)
       inc$(inc%) = f$+e$
    end if
    
    '�����������������������������������������������������������������������ͻ
    '� END SUB:nc.include                                       12-26-1991   �
    '�����������������������������������������������������������������������ͼ
end sub





'
'�����������������������������������������������������������������������ͻ
'� FUNCTION :split.command$                                 12-26-1991   �
'�����������������������������������������������������������������������͹
'� ACTION:                                                               �
'�                                                                       �
'�                                                                       �
'� TO PASS:                                                              �
'�    param$         ===>                                                �
'�                                                                       �
'� RETURN:                                                               �
'�    split.command$ ===>                                                �
'�                                                                       �
'�����������������������������������������������������������������������ͼ
function split.command$( param$ )
    shared ncpath$		' Path where to find files
    shared filedat$		' name of .NC file
    shared minc%		' include files ?  if /$  = true
    shared disk%                ' write to disk ?  if /D  = true
    shared disk.bak%            ' preserve .BAK ?  if /DB = false
    shared disk.sec%            ' put NONiceCode?  if /DS = true
    shared rembox%              ' put rem boxes ?  if /R  = true
    shared rembox.bak%          ' pres.old boxes?  if /RB = false
    shared muca%                ' upper case    ?  if /U  = true
    shared mlca%                ' lower case    ?  if /L  = true
    shared mtab%                ' tabulation    ?  if /T  = true
    local ifile$		' local name of main file
    local p%, l%, f%, d% 	' internal use
     
     filedat$ = "pb.nc"		' default language intructions file
     
     disk.bak% = -1
     rembox.bak% = -1
     f% = instr(param$,"/")
     

     if f% > 0 then
        ifile$ = left$(param$, f%-1)
        
        d% = instr(param$,"/F-")
        

        if d% > 0 then				' get .NC file name
           f% = instr(d%+1,param$+"/","/")
           filedat$ = mid$(param$, d%+3, f%-(d%+3))
        end if
        

        if instr(param$,"/H") or_		
           instr(param$,"/?") then
            call the.end				' < nota
        end if
        

        if instr(param$,"/D") then		' Write to disk
           disk% = -1				' default to screen
           

           if instr(param$,"/DB") then		' Not copy old file to .BAK
              disk.bak% = 0
           end if
           

           if instr(param$,"/DS") then		' Security. Put NONiceCode.
              disk.sec% = -1
           end if
           
        end if
        

        if instr(param$,"/U") then		' Upper case
           muca% = -1
        end if
        

        if instr(param$,"/L") then		' Lower case
           mlca% = -1
        end if
        

        if instr(param$,"/T") then		' Tabulation
           mtab% = -1
        end if
        

        if instr(param$,"/R") then		' Put REM boxes
           rembox% = -1
           

           if instr(param$, "/RB") then		' Don't preserve old boxes
              rembox.bak% = 0
           end if
           
        end if
        

        if instr(param$,"/$") then		' Bautify include files
           minc% = -1
        end if
        

        if instr(param$,"/P-") then		' Path where find include files
           p% = instr(param$,"/P-") + 3
           l% = instr(p%, param$+"/", "/")
           ncpath$ = mid$(param$, p%, l%-p% )
        end if
        
     else
        ifile$ = param$
     end if
     
     ifile$ = trim$(ifile$)
     split.command$ = ifile$
     '�����������������������������������������������������������������������ͻ
     '� END FUNCTION:split.command$                              12-26-1991   �
     '�����������������������������������������������������������������������ͼ
end function





'
'�����������������������������������������������������������������������ͻ
'� SUB :get.nc.file                                         12-26-1991   �
'�����������������������������������������������������������������������͹
'� ACTION:                                                               �
'�                                                                       �
'�                                                                       �
'� TO PASS:                                                              �
'�                                                                       �
'� RETURN:                                                               �
'�                                                                       �
'�����������������������������������������������������������������������ͼ
sub get.nc.file
   shared ncpath$
   shared filedat$
   shared func$()
   shared inst$()
   shared odwn%()
   shared otab%()
   shared incl$, linc$, rinc$, lstr$, rstr$
   shared lrem$, rrem$
   shared trun$
   shared proext$, incext$
   local linea$
   local file
   local p%, l%
   local firts%,last%
   local rembox%
   local type%
    

     redim func$(1:40)
    

     redim inst$(1:100)
    

     redim odwn%(1:100,1:2)
    

     redim otab%(1:100,1:2)
     call splitpathfn(filedat$, d$, p$, f$, e$)
    if e$ = "" then e$ = ".NC"
    

    if d$ <> "" then
       filedat$ = d$+p$+f$+e$
    else
       filedat$ = searchfiledospath$(f$+e$, 0 , ncpath$ )
       

       if filedat$ = "" then
          filedat$ = searchfiledospath$( f$+e$, 0, "" )
       end if
       
    end if
    

    if filedat$ = "" then
       print #1," ***************"
       print #1," **** ERROR ****  ";f$;e$;" instructions file hasn't good finded"
       print #1," ***************"
        call the.end
    else
       incl$ = ""
       file = freefile
       open filedat$ for input as #file
       

       do while not eof(file)
          input #file, linea$
          linea$ = trim$(linea$)
          

          if left$(linea$,1) <> ";" then
             rembox% = -1
             

             if linea$ <> "" then
                replace "�" with " " in linea$
                replace "�" with chr$(13,10) in linea$
                

                select case type%
                 case 0
                   incl$ = linea$			' include instruction
                   input #file, linc%		' left identifier
                   linc$ = chr$(linc%)
                   input #file, rinc%		' right identifier
                   rinc$ = chr$(rinc%)
                   input #file, lstr%
                   lstr$ = chr$(lstr%)
                   input #file, rstr%
                   rstr$ = chr$(rstr%)
                   input #file, lrem$ 		' left rem
                   input #file, rrem$		' right rem
                   input #file, trun$		' truncate delimiter
                   input #file, stat$		' statement delimiter
                   input #file, proext$    		' program extension
                   input #file, incext$		' libraryes extensions
                 case 1
                   incr p%
                   func$(p%) = linea$
                 case 2
                   incr p%
                   inst$(p%) = linea$
                   input #file, odwn%(p%,1)
                   input #file, otab%(p%,1)
                   input #file, otab%(p%,2)
                   input #file, odwn%(p%,2)
                end select

                
             end if
             
          else
             

             if rembox% then
                incr type%
                p% = 0
             end if
             
             rembox% = 0
             
          end if
          
       loop

       
       close file
    end if
    
    '�����������������������������������������������������������������������ͻ
    '� END SUB:get.nc.file                                      12-26-1991   �
    '�����������������������������������������������������������������������ͼ
end sub





'
'�����������������������������������������������������������������������ͻ
'� SUB :the.end                                             12-26-1991   �
'�����������������������������������������������������������������������͹
'� ACTION:                                                               �
'�                                                                       �
'�                                                                       �
'� TO PASS:                                                              �
'�                                                                       �
'� RETURN:                                                               �
'�                                                                       �
'�����������������������������������������������������������������������ͼ
sub the.end
   local a$
    print #1, " PARAMETERS:"
    print #1, "    NICECODE [d:][\dir\] file[.ext] /F-filedat [/P-incpath]  [/$] [/D|/DB|/DS] "
    print #1, "                                               [/U|/L] [/T] [/R|/RB] [/H|/?]"
    print #1, "    d:          = drive"
    print #1, "    \dir\       = directory"
    print #1, "    file        = file to beautify"
    print #1, "    .ext        = extension. Default on the .NC file selected"
    print #1, "    /F-filedat  = Instructions file.nc (��see .nc files!!)"
    print #1, "    /P-incpath  = PATH where find include files. Same DOS path."
    print #1, "    /$          = beautify also INCLUDE files"
    print #1, "    /D          = write beautify Source Code to file and not dispay"
    print #1, "                  to screen. Default is not write and display"
    print #1, "    /DB         = same /D but don't rename old file to .BAK"
    print #1, "    /DS         = put NiceCode security word (NONiceCode), at first"
    print #1, "                  line of output file, between REMs. This word"
    print #1, "                  preserves source code to new NiceCode use."
    print #1, "    /U /L       = change to Upper or Lower case the output source code"
    print #1, "    /T          = enable formated output source lines to up,left,rigt,down"
    print #1, "    /R          = include remember boxes after all files, FUNCTIONS,..."
    print #1, "    /RB         = same /R and delete old remember boxes"
    print #1, "    /H /?       = This HELP screen"
    print #1, "...Press key to continue..."
    a$ = input$(1)
    print #1,
    print #1, " DESCRIPTION:"
    print #1, "    Beautify the Source Code of file specified, and of INCLUDEs"
    print #1, "    files if /$ is present, depending by parameters passeds."
    print #1, "    Don't touch the REM lines and characters strigs betwen"
    print #1, "    string delimiters (i.e. ";chr$(34);" )"
    print #1,
    print #1, " EXAMPLES:"
    print #1, "    NiceCode test  /F=pb.nc /$ /P=c:\pb\;c:\pb\inc\;c:\pb\bas\"
    print #1, "    NiceCode prova /F=pb.nc /RB"
    print #1, "    NiceCode test  /F=qb.nc /$ /DB /DS /T /RB"
    print #1,
    print #1, "...Press key to continue..."
    a$ = input$(1)
    print #1, " SHAREWARE:        PsicoSoft - Esteve Valent�"
    print #1, "                   Sot dels Canyers, 6/60"
    print #1, "                   17220 - Sant Feliu de Gu�xols"
    print #1, "                   Girona - Catalunya - Spain"
    print #1, "                   fax: 34-72 203468"
    print #1, "   NiceCode is a Shareware product. It is not Free Software or in the Public
    print #1, "   Domain. You are garanted the right to use NiceCode for the 30-day
    print #1, "   evaluation period. If you find this program of value, please send US$15 to"
    print #1, "   the address above for the registration. You will receive the registration"
    print #1, "   package with the latest version on it."
    print #1,
    print #1, "   Add US$50 if you will receive the Power Basic Source Code of this program"
    print #1,
    print #1, "   Please support the shareware concept and will help the development of it"
    print #1, "   Please give this program to everybody. "
    print #1,
    print #1, "   Thank You!"
    print #1,
    print #1, " SUPPORT:   Net FIDO:   Id: Esteve Valenti    Addr: 2:343/116.11"
    print #1, "            Net RIME:   Id: Esteve Valenti    Addr: ->ABAFORUM"
    print #1, "                                        Conference: Rime-QBasic"
    close #1
    end		'<<<<<<<<<  Nota
    '�����������������������������������������������������������������������ͻ
    '� END SUB:the.end                                          12-26-1991   �
    '�����������������������������������������������������������������������ͼ
end sub





