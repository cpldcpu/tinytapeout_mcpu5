SMAL32 (rev  9/11)              primes.as                    21:22:42  Page  1
                                                             Wed Sep 07 2022

                                 1  ;---------------------------------------------------------------------
                                 2  ; MCPU5 primes
                                 3  ; Calculate the prime number sequence
                                 4  ;
                                 5  ; cpldcpu Sep 2022
                                 6  ;
                                 7  ;---------------------------------------------------------------------
                                 8  
                                 9  USE "MCPU5plus.inc"
                                10  
                                11  ;       divisor=2;      
                                12  ;       while (divisor<number)
                                13  ;       {
                                14  ;               test=-number;
                                15  ;               while (test<0) test+=divisor;
                                16  ;               if (test==0) return 0;
                                17  ;               divisor+=-1;
                                18  ;       }
                                19  ;       return 1;
                                20  
                                21  
                                22  number     = R0
                                23  divisor    = R1
                                24  
                                25  .=0
                                26  start:
 00000000: 12                   27      LDI 2
 00000001: 28                   28      STA number
                                29  
 00000002: 3B                   30      OUT                 ; first prime is 2
                                31  
                                32  outerloop:
 00000003: 12                   33      LDI 2
 00000004: 29                   34          STA divisor             ;divisor = 2
                                35  
 00000005: 11                   36      LDI 1
 00000006: 20                   37      ADD number
 00000007: 28                   38      STA number
                                39  loop:
 00000008: 30                   40      LDA number          ; test=-number;
 00000009: 39                   41      NEG
                                42  innerloop:
 0000000A: 21                   43          ADD     divisor         ; while (test<0) test+=divisor;
 0000000B: 0F                   44          BCC innerloop
                                45  
 0000000C: 39                   46          NEG                  ; if (test==0) return 0;
 0000000D: 03  14  0F           47          BCSL outerloop       ; No prime
                                48  
 00000010: 11                   49      LDI 1               ; divisor+=1;
 00000011: 21                   50          ADD     divisor
 00000012: 29                   51          STA     divisor
                                52  
 00000013: 39                   53      NEG                 ; while (divisor<number)
 00000014: 20                   54      ADD number
 00000015: 12  0F               55          BCCL loop
                                56  
SMAL32 (rev  9/11)              primes.as                    21:22:42  Page  2
                                                             Wed Sep 07 2022

                                57  prime:
 00000017: 30                   58      LDA number          ; Display prime number
 00000018: 3B                   59      OUT
                                60  
 00000019: 01  18  0E           61      JMP outerloop
                                62  
                                63  END
                    no errors
