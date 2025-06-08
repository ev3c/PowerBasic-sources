'ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
'ณ			      REGNAMES.INC				     ณ
'ณ									     ณ
'ณ This file is to be used as a $INCLUDE file whenever you use the CALL      ณ
'ณ INTERRUPT statement in your PowerBASIC program. The file contains         ณ
'ณ named constants that represent the registers the CALL INTERRUPT statement ณ
'ณ can manipulate.							     ณ
'ณ									     ณ
'ณ In order to use this file include it in your programs using the $INCLUDE  ณ
'ณ metastatement:                                          		     ณ
'ณ	                                                            	     ณ
'ณ   $INCLUDE "REGNAMES.INC"						     ณ
'ณ	                                  				     ณ
'ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

'
' ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออปบ
'บ Library......: REGNAMES.INC                                           บบ
'บ Definition...:                                                        บบ
'บ Author.......:                                                        บบ
'บ Date.........: 06-29-1992                                             บบ
'บ Notice.......:                                                        บบ
'บ Notes........:                                                        บบ
'บ Reserved.....:                                                        บบ
'บ Function.....:                                                        บบ
'บ Version......:                                                        บบ
'บ History......:                                                        บบ
'บ Library......:                                                        บบ
'บ Compile......:                                                        บบ
'บ Link.........:                                                        บบ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนบ
'บ OBSERVATIONS:                                                         บบ
'บ                                                                       บบ
'บ                                                                       บบ
'บ                                                                       บบ
'บ                                                                       บบ
'บ                                                                       บบ
'บ                                                                       บน
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนบ
'บ FUNCTIONs..:                                                          บบ
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
'บ                                                                       บผ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ 
%flags = 0
%ax    = 1
%bx    = 2
%cx    = 3
%dx    = 4
%si    = 5
%di    = 6
%bp    = 7
%ds    = 8
%es    = 9
