SMAL32 (rev  9/11)              primes.as                    17:11:14  Page  1
                                                             Wed Sep 07 2022

                                 1  ;---------------------------------------------------------------------
                                 2  ; MCPU5 primes
                                 3  ; Calculate the prime number sequence
                                 4  ;
                                 5  ; cpldcpu Sep 2022
                                 6  ;
                                 7  ;---------------------------------------------------------------------
                                 8  
                                 9  USE "MCPU5.inc"
                                10  
                                11  ;       sub=-2; 
                                12  ;       while ((number+sub)>0)
                                13  ;       {
                                14  ;               test=number;
                                15  ;               while (test>0) test+=sub;
                                16  ;               if (test==0) return 0;
                                17  ;               sub+=-1;
                                18  ;       }
                                19  ;       return 1;
                                20  
                                21  ;       divisor=2;      
                                22  ;       while (divisor<number)
                                23  ;       {
                                24  ;               test=number;
                                25  ;               while (test>0) test-=divisor;
                                26  ;               if (test==0) return 0;
                                27  ;               divisor+=-1;
                                28  ;       }
                                29  ;       return 1;
                                30  
                                31  ;       divisor=2;      
                                32  ;       while (divisor<number)
                                33  ;       {
                                34  ;               test=-number;
                                35  ;               while (test<0) test+=divisor;
                                36  ;               if (test==0) return 0;
                                37  ;               divisor+=-1;
                                38  ;       }
                                39  ;       return 1;
                                40  
                                41  
                                42  number     = R0
                                43  divisor    = R1
                                44  allone     = R7
                                45  
                                46  .=0
                                47  start:
 00000000: 1F                   48      LDI -1
 00000001: 2F                   49      STA allone
                                50  
 00000002: 12                   51      LDI 2
 00000003: 28                   52      STA number
                                53  
 00000004: 3B                   54      OUT                 ; first prime is 2
                                55  
                                56  outerloop:
SMAL32 (rev  9/11)              primes.as                    17:11:14  Page  2
                                                             Wed Sep 07 2022

 00000005: 12                   57      LDI 2
 00000006: 29                   58          STA divisor             ;divisor = 2
                                59  
 00000007: 11                   60      LDI 1
 00000008: 20                   61      ADD number
 00000009: 28                   62      STA number
                                63  loop:
 0000000A: 30                   64      LDA number          ; test=-number;
 0000000B: 39                   65      NEG
                                66  innerloop:
 0000000C: 21                   67          ADD     divisor         ; while (test<0) test+=divisor;
 0000000D: 0F                   68          BCCL innerloop
                                69  
 0000000E: 27                   70          ADD     allone           ; if (test==0) return 0;
 0000000F: 02  04  15  10       71          BCCL outerloop       ; No prime
 00000013: 3A 
                                72  
 00000014: 11                   73      LDI 1               ; divisor+=1;
 00000015: 21                   74          ADD     divisor
 00000016: 29                   75          STA     divisor
                                76  
 00000017: 39                   77      NEG                 ; while (divisor<number)
 00000018: 20                   78      ADD number
 00000019: 02  04  1A  10       79          BCCL loop
 0000001D: 3A 
                                80  
                                81  prime:
 0000001E: 30                   82      LDA number          ; Display prime number
 0000001F: 3B                   83      OUT
                                84  
 00000020: 15  10  3A           85      JMP outerloop
                                86  
                                87  END
                    no errors
