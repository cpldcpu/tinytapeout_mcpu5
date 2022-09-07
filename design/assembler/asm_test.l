SMAL32 (rev  9/11)              asm_test.as                  02:06:00  Page  1
                                                             Wed Sep 07 2022

                                 1  ;---------------------------------------------------------------------
                                 2  ; MCPU5
                                 3  ; Fibonacci numbers
                                 4  ;
                                 5  ; cpldcpu Sep 2022
                                 6  ;
                                 7  ;---------------------------------------------------------------------
                                 8  
                                 9  USE "MCPU5.inc"
                                10  
                                11  ;       a=0
                                12  ;   b=1
                                13  ;  loop:
                                14  ;   newval=a+b
                                15  ;   print(newval)
                                16  ;   a=b;
                                17  ;   b=newval
                                18  ;  goto loop
                                19  
                                20  
                                21  .=0
                                22  init:
 00000000: 17                   23      LDI  7
 00000001: 18                   24      LDI  -8
 00000002: 21                   25      ADD  R1
 00000003: 2A                   26      STA  R2
 00000004: 33                   27      LDA  R3
 00000005: 38                   28      NOT
 00000006: 39                   29      NEG
 00000007: 3A                   30      JMPA
 00000008: 3B                   31      OUT
                                32  loop:
 00000009: 39                   33      NEG
 0000000A: 38                   34      NOT
 0000000B: 0E                   35      BCC loop
                                36  END
                    no errors
