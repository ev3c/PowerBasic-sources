%key.esc = 27
%key.spa = &h20
%key.ret = &h0d
%key.tab = &h09
%key.dle = &h08

%key.pup = &h0049
%key.pdw = &h0051

%key.cup = &h0048
%key.cdw = &h0050
%key.cle = &h004b
%key.cri = &h004d

%key.ins = &h0052
%key.del = &h0053
%key.hom = &h0047
%key.end = &h004f

%key.f1  = &h3b
%key.f2  = &h3c
%key.f3  = &h3d
%key.f4  = &h3e
%key.f5  = &h3f
%key.f6  = &h40
%key.f7  = &h41
%key.f8  = &h42
%key.f9  = &h43
%key.f10 = &h44



'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :kbd.speed                                           02-09-1992   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    mrate%         ===>                                                บ
'บ    mdelay%        ===>                                                บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub kbd.speed(mrate%, mdelay%)
    reg 1, &h0305
    reg 2, (mdelay% * 256) + mrate%
     call interrupt &h16
    'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
    'บ END SUB:kbd.speed                                        02-09-1992   บ
    'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :kbd.putkey                                          02-09-1992   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ    character%     ===>                                                บ
'บ    scancode%      ===>                                                บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub kbd.putkey(character%, scancode%)


    do
       reg 1, &h0500			' put key to DOS key buffer
       reg 3, scancode% * 256 + character%
        call interrupt &h16
    loop while (reg(1) mod 256) <> 0	
' if AL <> 0
    'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
    'บ END SUB:kbd.putkey                                       02-09-1992   บ
    'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :kbd.numlockoff                                      02-09-1992   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub kbd.numlockoff
    def seg = &h40
    poke &h17,peek(&h17) and 223
    'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
    'บ END SUB:kbd.numlockoff                                   02-09-1992   บ
    'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :kbd.numlockon                                       02-09-1992   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub kbd.numlockon
    def seg = &h40
    poke &h17,peek(&h17) or 32
    'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
    'บ END SUB:kbd.numlockon                                    02-09-1992   บ
    'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :kbd.capslockoff                                     02-09-1992   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub kbd.capslockoff
    def seg = &h40
    poke &h17,peek(&h17) and 171
    'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
    'บ END SUB:kbd.capslockoff                                  02-09-1992   บ
    'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :kbd.capslockon                                      02-09-1992   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub kbd.capslockon
    def seg = &h40
    poke &h17,peek(&h17) or 64
    'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
    'บ END SUB:kbd.capslockon                                   02-09-1992   บ
    'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :kbd.off                                             02-09-1992   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub kbd.off
    def seg = 64
    out 97,204
    'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
    'บ END SUB:kbd.off                                          02-09-1992   บ
    'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub





'
'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ SUB :kbd.on                                              02-09-1992   บ
'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
'บ ACTION:                                                               บ
'บ                                                                       บ
'บ                                                                       บ
'บ TO PASS:                                                              บ
'บ                                                                       บ
'บ RETURN:                                                               บ
'บ                                                                       บ
'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
sub kbd.on
    def seg = 64
    out 97,76
    'ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
    'บ END SUB:kbd.on                                           02-09-1992   บ
    'ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
end sub





sub kbd.flags(mins%, mcap%, mnum%, mdes%, malt%, mctl%, mlsh%, mrsh%)
    
    reg(1), &h0200
     call interrupt &h16
    
    mins% = 0
    mcap% = 0
    mnum% = 0
    mdes% = 0
    malt% = 0
    mctl% = 0
    mlsh% = 0
    mrsh% = 0
    
    if (reg(1) and 1)   <> 0 then mrsh%  = -1	' bit 0
    if (reg(1) and 2)   <> 0 then mlsh%  = -1	' bit 1
    if (reg(1) and 4)   <> 0 then mctl%  = -1	' bit 2
    if (reg(1) and 8)   <> 0 then malt%  = -1	' bit 3
    if (reg(1) and 16)  <> 0 then mdes%  = -1	' bit 4
    if (reg(1) and 32)  <> 0 then mnum%  = -1	' bit 5
    if (reg(1) and 64)  <> 0 then mcap%  = -1	' bit 6
    if (reg(1) and 128) <> 0 then mins%  = -1	' bit 7
    
end sub





function kbd.ins%
      call kbd.flags(mins%, mcap%, mnum%, mdes%, malt%, mctl%, mlsh%, mrsh%)
     kbd.ins% = mins%
end function





function kbd.cap%
      call kbd.flags(mins%, mcap%, mnum%, mdes%, malt%, mctl%, mlsh%, mrsh%)
     kbd.cap% = mcap%
end function





function kbd.num%
      call kbd.flags(mins%, mcap%, mnum%, mdes%, malt%, mctl%, mlsh%, mrsh%)
     kbd.num% = mnum%
end function





function kbd.des%
      call kbd.flags(mins%, mcap%, mnum%, mdes%, malt%, mctl%, mlsh%, mrsh%)
     kbd.des% = mdes%
end function





function kbd.alt%
      call kbd.flags(mins%, mcap%, mnum%, mdes%, malt%, mctl%, mlsh%, mrsh%)
     kbd.alt% = malt%
end function





function kbd.ctl%
      call kbd.flags(mins%, mcap%, mnum%, mdes%, malt%, mctl%, mlsh%, mrsh%)
     kbd.ctl% = mctl%
end function





function kbd.lsh%
      call kbd.flags(mins%, mcap%, mnum%, mdes%, malt%, mctl%, mlsh%, mrsh%)
     kbd.lsh% = mlsh%
end function





function kbd.rsh%
      call kbd.flags(mins%, mcap%, mnum%, mdes%, malt%, mctl%, mlsh%, mrsh%)
     kbd.rsh% = mrsh%
end function





