;---------------------------------------------------------------------
; MCPU5 primes
; Calculate the prime number sequence
;
; cpldcpu Sep 2022
;
;---------------------------------------------------------------------

USE "MCPU5plus.inc"

;	divisor=2;	
;	while (divisor<number)
;	{
;		test=-number;
;		while (test<0) test+=divisor;
;		if (test==0) return 0;
;		divisor+=-1;
;	}
;	return 1;


number     = R0
divisor    = R1

.=0
start:
    LDI 2
    STA number

    OUT                 ; first prime is 2

outerloop:
    LDI 2
	STA divisor	        ;divisor = 2

    LDI 1
    ADD number
    STA number
loop:
    LDA number          ; test=-number;
    NEG
innerloop:
	ADD	divisor	        ; while (test<0) test+=divisor;
	BCC innerloop

	NEG                  ; if (test==0) return 0;
	BCSL outerloop       ; No prime

    LDI 1               ; divisor+=1;
	ADD	divisor
	STA	divisor

    NEG                 ; while (divisor<number)
    ADD number
	BCCL loop

prime:
    LDA number          ; Display prime number
    OUT

    JMP outerloop

