'NOGOODFONT

'
'浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
'� SUB :getpro                                              12-25-1991   �
'麺様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
'� ACTION:                                                               �
'�                                                                       �
'�                                                                       �
'� TO PASS:                                                              �
'�    (namefile$     ===>                                                �
'�    actpro%        ===>                                                �
'�    maxpro%        ===>                                                �
'�                                                                       �
'� RETURN:                                                               �
'�                                                                       �
'藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
sub getpro (namefile$,actpro%,maxpro%)
   local dato$
   local file%
    file% = freefile
    open "R",file%,namefile$,2
    field #file%,2 as dato$
    get #file%,55
    actpro% = cvi(dato$)
    get #file%,56
    maxpro% = cvi(dato$)
    close #file%
    '浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
    '� END SUB:getpro                                           12-25-1991   �
    '藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
end sub





'
'浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
'� SUB :putpro                                              12-25-1991   �
'麺様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
'� ACTION:                                                               �
'�                                                                       �
'�                                                                       �
'� TO PASS:                                                              �
'�    namefile$      ===>                                                �
'�    actpro%        ===>                                                �
'�    maxpro%        ===>                                                �
'�                                                                       �
'� RETURN:                                                               �
'�                                                                       �
'藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
sub putpro(namefile$,actpro%,maxpro%)
   local dato$
   local file%
    file% = freefile
    open "R",file%,namefile$,2
    field #file%,2 as dato$
    lset dato$ = mki$(actpro%)
    put #file%,55
    lset dato$ = mki$(maxpro%)
    put #file%,56
    close #file%
    '浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
    '� END SUB:putpro                                           12-25-1991   �
    '藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
end sub





'
'浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
'� SUB :wpro                                                12-25-1991   �
'麺様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
'� ACTION:                                                               �
'�                                                                       �
'�                                                                       �
'� TO PASS:                                                              �
'�    nameprogram$   ===>                                                �
'�                                                                       �
'� RETURN:                                                               �
'�                                                                       �
'藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
sub wpro(nameprogram$)
   local a$
      dim temp$(1:15)

      $if %english
	 temp$(1) = "You has been sobrepassed the maxium"
	 temp$(2) = "number of screen to save with shareware"
	 temp$(3) = "versi�n of "+nameprogram$
	 temp$(4) = "If do you will follow using the program"
	 temp$(5) = "contact with me at."
	 temp$(7) = "Esteve Valent� Morales"
	 temp$(8) = "Grup Sot dels Canyers, 6/60"
	 temp$(9) = "17220 - Sant Feliu de Gu�xols"
	 temp$(10)= "Girona - Catalunya - Spain"
	 temp$(11)= "Telf.: 3-72  32.50.94"
	 temp$(12)= "       3-72  21.37.58"
	 temp$(13)= "FAX..: 3-72  20.34.68"
	 temp$(15)= " Press a key to continue "

      $else
	 temp$(1) = "Has sobrepasado el n�mero m�ximo de"
	 temp$(2) = "grabaciones que te permite la versi�n"
	 temp$(3) = "shareware de "+nameprogram$
	 temp$(4) = "Si quieres seguir utiliz�ndolo"
	 temp$(5) = "ponte en contacto conmigo."
	 temp$(7) = "Esteve Valent� Morales"
	 temp$(8) = "Grup Sot dels Canyers, 6/60"
	 temp$(9) = "17220 - Sant Feliu de Gu�xols"
	 temp$(10)= "Girona - Catalunya - Spain"
	 temp$(11)= "Telf.:(972) 32.50.94"
	 temp$(12)= "      (972) 21.37.58"
	 temp$(13)= "FAX..:(972) 20.34.68"
	 temp$(15)= " Pulsa tecla para continuar "

      $endif


     call wopen(5,20,21,60,1,0,1,4,"")
     for x% = 1 to 14
	 call wprint(x%, 0, temp$(x%))
     next x%

     a$ = input$(1)
     call wclose

    '浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
    '� END SUB:wpro                                             12-25-1991   �
    '藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
end sub





