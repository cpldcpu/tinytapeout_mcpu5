; Zinnia (MCPU5)
; SMAL32 include file
;
; cpldcpu 2022-Sep-7


;------------------------------------
; Boundary checks

MACRO checkmem =mem
	IF ( mem<0 ) ! ( mem>255 )
		ERROR Adress out of range!
	ENDIF
ENDMAC

MACRO checksimm =simm
	IF ( simm<-8 ) ! ( simm>7 )
		ERROR Immediate out of range!
	ENDIF
ENDMAC

MACRO checkbranch =simm
	IF ( simm<-8 ) ! ( simm>7 )
		ERROR branch out of range!
	ENDIF
ENDMAC


MACRO checkreg =reg
	IF ( reg<0 ) ! ( reg>7 )
		ERROR Unknown register!
	ENDIF
ENDMAC


;------------------------------
;Instructions


; Branch
MACRO BCC tgt
	tmp = tgt - .
	checkbranch tmp
	B ( 2#0000000 ! (tmp&15) )
ENDMAC

;Load immediate
MACRO LDI tgt
	checksimm tgt
	B ( 2#0010000 ! (tgt&15) )
ENDMAC

;Add register
MACRO ADD reg
	checkreg reg
	B ( 2#0100000 ! reg )
ENDMAC

;sta register
MACRO STA reg
	checkreg reg
	B ( 2#0101000 ! reg )
ENDMAC

;lda register
MACRO LDA reg
	checkreg reg
	B ( 2#0110000 ! reg )
ENDMAC

;NOT
MACRO NOT
	B ( 2#0111000 )
ENDMAC

;NEG 
MACRO NEG
	B ( 2#0111001 )
ENDMAC

;JMPA
MACRO JMPA
	B ( 2#0111010 )
ENDMAC

;OUT
MACRO OUT
	B ( 2#0111011 )
ENDMAC

;------------------------------
;Useful Macros

;Load immediate 8 bit
MACRO LDI8 tgt
	checkmem tgt
	B ( 2#0010000 ! (tgt&15) )
	B ( 2#0010000 ! (tgt>>4) )
ENDMAC

; absolute jump
MACRO JMP tgt
	LDI8 tgt
	JMPA
ENDMAC

;Flexible branch
MACRO BCCL tgt
	tmp = tgt - .
	IF ( tmp<-8 ) ! ( tmp>7 )
		B ( 2#0000000 ! 2 )
		B ( 2#0000000 ! 4 )
		JMP tgt
	ELSE
		BCC tgt
	ENDIF
ENDMAC

MACRO BCSL tgt
	tmp = tgt - .
	IF ( tmp<-8 ) ! ( tmp>7 )
		B ( 2#0000000 ! 4 )
		JMP tgt
	ELSE
		BCS tgt
	ENDIF
ENDMAC

MACRO BCS tgt
	BCC .+2
	BCC tgt
ENDMAC

MACRO DCB (xx)
	B xx
ENDMAC

;------------------------------------------------------------------------
; Define Registers
;------------------------------------------------------------------------

R0  =  0
R1  =  1
R2  =  2
R3  =  3
R4  =  4
R5  =  5
R6  =  6
R7  =  7
R8  =  8
