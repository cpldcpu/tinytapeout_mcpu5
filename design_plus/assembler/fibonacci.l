SMAL32 (rev  9/11)              fibonacci.as                 21:47:46  Page  1
                                                             Wed Sep 07 2022

                                 1  ;---------------------------------------------------------------------
                                 2  ; MCPU5
                                 3  ; calculate Fibonacci numbers up to 8 bit limit, then idle.
                                 4  ;
                                 5  ; cpldcpu Sep 2022
                                 6  ;
                                 7  ;---------------------------------------------------------------------
                                 8  
                                 9  USE "MCPU5plus.inc"
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
 00000000: 10                   23      LDI  0
 00000001: 28                   24      STA  R0  ; a = 0
 00000002: 11                   25      LDI  1
 00000003: 29                   26      STA  R1  ; b = 1
                                27  loop:
 00000004: 31                   28      LDA  R1
 00000005: 2A                   29      STA  R2  ; temp = b
                                30      
 00000006: 20                   31      ADD  R0
 00000007: 29                   32      STA  R1  ; b' = a + b
                                33  
 00000008: 32                   34      LDA  R2
 00000009: 28                   35      STA  R0  ; a = b
                                36  
 0000000A: 3B                   37      OUT      ; display b
                                38  
 0000000B: 09                   39      BCC loop
                                40  idle:
 0000000C: 00                   41      BCC idle
                                42  END
                    no errors
