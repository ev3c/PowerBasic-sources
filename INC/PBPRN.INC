'NOGOODFONT
'
'浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
'� FUNCTION :prn.stat$                                      02-09-1992   �
'麺様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
'� ACTION:                                                               �
'�    look stat of printer.                                              �
'�                                                                       �
'� RETURN:                                                               �
'�    prn.stat$      ===> string with 10 characters with " ON  LINE " or �
'�                                                       " OFF LINE "    �
'藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
function prn.stat$        				PUBLIC
    local a,b

     def seg = 64
     a=peek(8)+256*peek(9)
     b=(inp(a+1) and 248) xor 72


     if (b and 128)<>128 then
        prn.stat$ = " OFF LINE "
     else
        prn.stat$ = " ON  LINE "
     end if

'浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
'� END FUNCTION:prn.stat$                                   02-09-1992   �
'藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
end function





'
'浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
'� SUB :prn.start                                           02-09-1992   �
'麺様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
'� ACTION:                                                               �
'�    Set the printer to ON LINE.                                        �
'�                                                                       �
'藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
sub prn.start						PUBLIC
    local a,b

     def seg = 64
     a=peek(8)+256*peek(9)
     b=(inp(a+1) and 248) xor 72
     def seg
     out a+2,8
     out a+2,12
'浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
'� END SUB:prn.start                                        02-09-1992   �
'藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
end sub




 
