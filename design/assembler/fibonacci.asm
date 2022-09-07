;---------------------------------------------------------------------
; MCPU5
; calculate Fibonacci numbers up to 8 bit limit, then idle.
;
; cpldcpu Sep 2022
;
;---------------------------------------------------------------------

USE "MCPU5.inc"

;	a=0
;   b=1
;  loop:
;   newval=a+b
;   print(newval)
;   a=b;
;   b=newval
;  goto loop


.=0
init:
    LDI  0
    STA  R0  ; a = 0
    LDI  1
    STA  R1  ; b = 1
loop:
    LDA  R1
    STA  R2  ; temp = b
    
    ADD  R0
    STA  R1  ; b' = a + b

    LDA  R2
    STA  R0  ; a = b

    OUT      ; display b

    BCC loop
idle:
    BCC idle
