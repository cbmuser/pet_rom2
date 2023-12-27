!to "901465-01-02.bin",plain
; ----------------------------------------------------------------------------
basend_lo       = $2E                        ; Pointer End of BASIC Arrays (lo)
basend_hi       = $2F                        ; Pointer End of BASIC Arrays (hi)
FORPNT_L        = $46
FORPNT_H        = $47
L0051           = $51
temp_54         = $54                        ; Temporary storage for FLPT value
temp_55         = $55                        ; Temporary storage for FLPT value
temp_56         = $56                        ; Temporary storage for FLPT value
temp_57         = $57                        ; Temporary storage for FLPT value
temp_58         = $58                        ; Temporary storage for FLPT value
L001F           = $1F
CHRGET          = $70                        ; Subroutine: Get Next Byte of BASIC Text
CHRGOT          = $76                        ; Entry to Get Same Byte of Text Again
L007D           = $7D
L3580           = $3580
L5246           = $5246
LC074           = $c074 
LC075           = $c075
LC076           = $c076
LC000           = $c000
LC001           = $c001   
LC091           = $c091
LC092           = $c092
LC192           = $c192
CHKIN           = $FFC6                        ; Set Input
CHKOUT          = $FFC9                        ; Set Output
CLRCH           = $FFCC                        ; Restore I/O Vector
CHRIN           = $FFCF                        ; Input Vector, chrin
CHROUT          = $FFD2                        ; Output Vector, chrout
STOPKEY         = $FFE1                        ; Vector: test for Stop-key
GET             = $FFE4                        ; Vector: GET
CLRALL          = $FFE7                        ; Vector: Close all channels
; ----------------------------------------------------------------------------
;ROMS 901465-01-02 BASIC 2
*=$c000
; offsets for basic token addr +1
  !by $40,$c7                ; STOP, END, BREAK
  !by $57,$c6                ; FOR-NEXT
  !by $1f,$cc                ; NEXT  
  !by $ff,$c7                ; DATA  
  !by $a6,$ca                ; INPUT #
  !by $c0,$ca                ; INPUT 
  !by $62,$cf                ; DIM
  !by $06,$cb                ; READ 
  !by $ac,$c8                ; LET
  !by $ac,$c7                ; GOTO
  !by $84,$c7                ; RUN
  !by $2f,$c8                ; IF
  !by $2f,$c7                ; RESTORE
  !by $8f,$c7                ; GOSUB
  !by $d9,$c7                ; RETURN
  !by $42,$c8                ; REM
  !by $3e,$c7                ; STOP
  !by $52,$c8                ; ON
  !by $0f,$d7                ; WAIT
  !by $d4,$ff                ; LOAD   (KERNAL)
  !by $d7,$ff                ; SAVE   (KERNAL)
  !by $da,$ff                ; VERIFY (KERNAL)
  !by $8c,$d2                ; DEF
  !by $06,$d7                ; POKE
  !by $8a,$c9                ; PRINT#
  !by $aa,$c9                ; PRINT
  !by $6a,$c7                ; CONT
  !by $b4,$c5                ; LIST
  !by $76,$c5                ; CLR
  !by $90,$c9                ; CMD
  !by $dd,$ff                ; SYS    (KERNAL)
  !by $bf,$ff                ; OPEN   (KERNAL)
  !by $c2,$ff                ; CLOSE  (KERNAL)
  !by $7c,$ca                ; GET
  !by $5a,$c5                ; NEW    
; offsets for basic token addr *
  !by $45,$db                ; SGN 
  !by $d8,$db                ; INT
  !by $64,$db                ; ABS
  !by $00,$00                ; USER-FUNCTION
  !by $59,$d2                ; FRE
  !by $7a,$d2                ; POS
  !by $5e,$de                ; SQR      
  !by $7f,$df                ; RND
  !by $f6,$d8                ; LOG 
  !by $da,$de                ; EXP 
  !by $d8,$df                ; COS
  !by $df,$df                ; SIN
  !by $28,$e0                ; TAN    (KERNAL)
  !by $8c,$e0                ; ATN    (KERNAL) 
  !by $e8,$d6                ; PEEK
  !by $56,$d6                ; LEN 
  !by $3f,$d3                ; STR$
  !by $87,$d6                ; VAL
  !by $65,$d6                ; ASC
  !by $c6,$d5                ; CHR$
  !by $da,$d5                ; LEFT$
  !by $06,$d6                ; RIGHT$
  !by $11,$d6                ; MID$

  !by $79,$75,$d7,$79,$35,$d7,$7b,$36,$d9,$7b,$1d,$da  
  !by $7f,$67,$de,$50,$ca,$ce,$46,$c7,$ce,$7d,$a0,$de,$5a,$ce,$cd,$64,$f7,$ce

  !by $45,$4e,$c4,$46,$4f,$d2,$4e,$45,$58,$d4,$44,$41,$54,$c1,$49,$4e ; EN.FO.NEX.DAT.IN
  !by $50,$55,$54,$a3,$49,$4e,$50,$55,$d4,$44,$49,$cd,$52,$45,$41,$c4 ; PUT.INPU.DI.REA.
  !by $4c,$45,$d4,$47,$4f,$54,$cf,$52,$55,$ce,$49,$c6,$52,$45,$53,$54 ; LE.GOT.RU.I.REST
  !by $4f,$52,$c5,$47,$4f,$53,$55,$c2,$52,$45,$54,$55,$52,$ce,$52,$45 ; OR.GOSU.RETUR.RE
  !by $cd,$53,$54,$4f,$d0,$4f,$ce,$57,$41,$49,$d4,$4c,$4f,$41,$c4,$53 ; .STO.O.WAI.LOA.S
  !by $41,$56,$c5,$56,$45,$52,$49,$46,$d9,$44,$45,$c6,$50,$4f,$4b,$c5 ; AV.VERIF.DE.POK.
  !by $50,$52,$49,$4e,$54,$a3,$50,$52,$49,$4e,$d4,$43,$4f,$4e,$d4,$4c ; PRINT.PRIN.CON.L
  !by $49,$53,$d4,$43,$4c,$d2,$43,$4d,$c4,$53,$59,$d3,$4f,$50,$45,$ce ; IS.CL.CM.SY.OPE.
  !by $43,$4c,$4f,$53,$c5,$47,$45,$d4,$4e,$45,$d7,$54,$41,$42,$a8,$54 ; CLOS.GE.NE.TAB.T
  !by $cf,$46,$ce,$53,$50,$43,$a8,$54,$48,$45,$ce,$4e,$4f,$d4,$53,$54 ; .F.SPC.THE.NO.ST
  !by $45,$d0,$ab,$ad,$aa,$af,$de,$41,$4e,$c4,$4f,$d2,$be,$bd,$bc,$53 ; E......AN.O....S
  !by $47,$ce,$49,$4e,$d4,$41,$42,$d3,$55,$53,$d2,$46,$52,$c5,$50,$4f ; G.IN.AB.US.FR.PO
  !by $d3,$53,$51,$d2,$52,$4e,$c4,$4c,$4f,$c7,$45,$58,$d0,$43,$4f,$d3 ; .SQ.RN.LO.EX.CO.
  !by $53,$49,$ce,$54,$41,$ce,$41,$54,$ce,$50,$45,$45,$cb,$4c,$45,$ce ; SI.TA.AT.PEE.LE.
  !by $53,$54,$52,$a4,$56,$41,$cc,$41,$53,$c3,$43,$48,$52,$a4,$4c,$45 ; STR.VA.AS.CHR.LE
  !by $46,$54,$a4,$52,$49,$47,$48,$54,$a4,$4d,$49,$44,$a4,$47,$cf,$00 ; FT.RIGHT.MID.G..
  !by $4e,$45,$58,$54,$20,$57,$49,$54,$48,$4f,$55,$54,$20,$46,$4f,$d2 ; NEXT WITHOUT FO.
  !by $53,$59,$4e,$54,$41,$d8,$52,$45,$54,$55,$52,$4e,$20,$57,$49,$54 ; SYNTA.RETURN WIT
  !by $48,$4f,$55,$54,$20,$47,$4f,$53,$55,$c2,$4f,$55,$54,$20,$4f,$46 ; HOUT GOSU.OUT OF
  !by $20,$44,$41,$54,$c1,$49,$4c,$4c,$45,$47,$41,$4c,$20,$51,$55,$41 ; DAT.ILLEGAL QUA
  !by $4e,$54,$49,$54,$d9,$4f,$56,$45,$52,$46,$4c,$4f,$d7,$4f,$55,$54 ; NTIT.OVERFLO.OUT
  !by $20,$4f,$46,$20,$4d,$45,$4d,$4f,$52,$d9,$55,$4e,$44,$45,$46,$27 ;  OF MEMOR.UNDEF'
  !by $44,$20,$53,$54,$41,$54,$45,$4d,$45,$4e,$d4,$42,$41,$44,$20,$53 ; D STATEMEN.BAD S
  !by $55,$42,$53,$43,$52,$49,$50,$d4,$52,$45,$44,$49,$4d,$27,$44,$20 ; UBSCRIP.REDIM'D 
  !by $41,$52,$52,$41,$d9,$44,$49,$56,$49,$53,$49,$4f,$4e,$20,$42,$59 ; ARRA.DIVISION BY
  !by $20,$5a,$45,$52,$cf,$49,$4c,$4c,$45,$47,$41,$4c,$20,$44,$49,$52 ; ZER.ILLEGAL DIR
  !by $45,$43,$d4,$54,$59,$50,$45,$20,$4d,$49,$53,$4d,$41,$54,$43,$c8 ; EC.TYPE MISMATC.
  !by $53,$54,$52,$49,$4e,$47,$20,$54,$4f,$4f,$20,$4c,$4f,$4e,$c7,$46 ; STRING TOO LON.F
  !by $49,$4c,$45,$20,$44,$41,$54,$c1,$46,$4f,$52,$4d,$55,$4c,$41,$20 ; ILE DAT.FORMULA 
  !by $54,$4f,$4f,$20,$43,$4f,$4d,$50,$4c,$45,$d8,$43,$41,$4e,$27,$54 ; TOO COMPLE.CAN'T
  !by $20,$43,$4f,$4e,$54,$49,$4e,$55,$c5,$55,$4e,$44,$45,$46,$27,$44 ; CONTINU.UNDEF'D
  !by $20,$46,$55,$4e,$43,$54,$49,$4f,$ce,$20,$45,$52,$52,$4f,$52,$00 ; FUNCTIO. ERROR.
  !by $20,$49,$4e,$20,$00,$0d,$0a,$52,$45,$41,$44,$59,$2e,$0d,$0a,$00 ; IN ...READY....
  !by $0d,$0a,$42,$52,$45,$41,$4b,$00                                 ; ..BREAK..

; ----------------------------------------------------------------------------
; LOOK FOR FOR-LOOP IN STACK
;*=$c2aa
FNDFOR:
        tsx
        inx
        inx
        inx
        inx
LC2AF:  lda     $0101,x
        cmp     #$81
        bne     LC2D7
        lda     $47
        bne     LC2C4
        lda     $0102,x
        sta     $46
        lda     $0103,x
        sta     $47
LC2C4:  cmp     $0103,x
        bne     LC2D0
        lda     $46
        cmp     $0102,x
        beq     LC2D7
LC2D0:  txa
        clc
        adc     #$12
        tax
        bne     LC2AF
LC2D7:  rts
; ----------------------------------------------------------------------------
; CREATE SPACE FOR INSERTED BASICLINE
BLTU:   jsr     LC328
        sta     basend_lo
        sty     basend_hi
; MEMORY-BLOCK TRANSFER
BLTUC:  sec
        lda     $57
        sbc     $5C
        sta     L001F
        tay
        lda     $58
        sbc     $5D
        tax
        inx
        tya
        beq     LC313
        lda     $57
        sec
        sbc     L001F
        sta     $57
        bcs     LC2FC
        dec     $58
        sec
LC2FC:  lda     $55
        sbc     L001F
        sta     $55
        bcs     LC30C
        dec     $56
        bcc     LC30C
LC308:  lda     ($57),y
        sta     ($55),y
LC30C:  dey
        bne     LC308
        lda     ($57),y
        sta     ($55),y
LC313:  dec     $58
        dec     $56
        dex
        bne     LC30C
        rts
; ----------------------------------------------------------------------------
; CHECK FOR STACK-SPACE
LC31B:  asl     
        adc     #$3E
        bcs     ERRORIN
        sta     L001F
        tsx
        cpx     L001F
        bcc     ERRORIN
        rts
; ----------------------------------------------------------------------------
; CHECK FOR ENOUGH STACK-SPACE
LC328:  cpy     $31
        bcc     LC354
        bne     LC332
        cmp     $30
        bcc     LC354
LC332:  pha
        ldx     #$09
        tya
LC336:  pha
        lda     $54,x
        dex
        bpl     LC336
        jsr     LD400
        ldx     #$F7
LC341:  pla
        sta     $5E,x
        inx
        bmi     LC341
        pla
        tay
        pla
        cpy     $31
        bcc     LC354
        bne     ERRORIN
        cmp     $30
        bcs     ERRORIN
LC354:  rts
; ----------------------------------------------------------------------------
; ERROR HANDLING/OUTPUT
ERRORIN:ldx     #$4D
; OUTPUT ERROR (X-REG)
ERROR:  lsr     $0D          ; Flag to suppress PRINT or PRINT#
        lda     $0E
        beq     LC364
        jsr     CLRCH
        lda     #$00
        sta     $0E
LC364:  jsr     LC9E2
        jsr     $ca43 
LC36A:  lda     LC192,x
        pha
        and     #$7F
        jsr     LCA45
        inx
        pla
        bpl     LC36A
        jsr     LC593
        lda     #$8B
        ldy     #$C2
LC37E:  jsr     LCA1C
        ldy     $37
        iny
        beq     READY
        jsr     LDCCE
; BASIC WARMSTART, OUTPUT READY
READY:  lsr     $0D
        lda     #$97
        ldy     #$C2
        jsr     LCA1C
; INTERPRET BASIC LINE
MAIN:   jsr     LC46F
        stx     $77          ; Pointer: Current Byte of BASIC Text lo
        sty     $78          ; Pointer: Current Byte of BASIC Text hi
        jsr     CHRGET
        tax
        beq     MAIN
        ldx     #$FF
        stx     $37          ; Current BASIC Line Number 
        bcc     LC3AB
        jsr     LC495
        jmp     LC6F7
; ----------------------------------------------------------------------------
;BASIC INTERPRETER
LC3AB:  jsr     LC873
        jsr     LC495
        sty     $05
        jsr     LC52C
        bcc     LC3FC
        ldy     #$01
        lda     ($5C),y
        sta     $20
        lda     $2A
        sta     L001F
        lda     $5D
        sta     $22
        lda     $5C
        dey
        sbc     ($5C),y
        clc
        adc     $2A          ; Pointer: Start of BASIC	Variables lo
        sta     $2A
        sta     $21
        lda     $2B          ; Pointer: Start of BASIC	Variables hi 
        adc     #$FF
        sta     $2B
        sbc     $5D
        tax
        sec
        lda     $5C
        sbc     $2A
        tay
        bcs     LC3E6
        inx
        dec     $22
LC3E6:  clc
        adc     L001F
        bcc     LC3EE
        dec     $20
        clc
LC3EE:  lda     ($1F),y
        sta     ($21),y
        iny
        bne     LC3EE
        inc     $20
        inc     $22
        dex
        bne     LC3EE
LC3FC:  jsr     LC572
        jsr     LC442
        lda     $0200
        beq     MAIN
        clc
        lda     $2A
        sta     $57
        adc     $05
        sta     $55
        ldy     $2B
        sty     $58
        bcc     LC417
        iny
LC417:  sty     $56
        jsr     BLTU
        lda     $11
        ldy     $12
        sta     $01FE
        sty     $01FF
        lda     basend_lo
        ldy     basend_hi
        sta     $2A
        sty     $2B
        ldy     $05
        dey
LC431:  lda     $01FC,y
        sta     ($5C),y
        dey
        bpl     LC431
; REARRANGE BASIC
FINI:   jsr     LC572
        jsr     LC442
        jmp     MAIN
; ----------------------------------------------------------------------------
; BIND LINE (INSERT/DELETE)
LC442:  lda     $28
        ldy     $29
        sta     L001F
        sty     $20
        clc
LC44B:  ldy     #$01
        lda     (L001F),y
        beq     LC46E
        ldy     #$04
LC453:  iny
        lda     (L001F),y
        bne     LC453
        iny
        tya
        adc     L001F
        tax
        ldy     #$00
        sta     (L001F),y
        lda     $20
        adc     #$00
        iny
        sta     (L001F),y
        stx     L001F
        sta     $20
        bcc     LC44B
LC46E:  rts
; ----------------------------------------------------------------------------
; GET CHAR FROM KEYBOARD TO BUFFER
LC46F:  ldx     #$00
LC471:  jsr     LC481
        cmp     #$0D
        beq     LC47E
        sta     $0200,x
        inx
        bne     LC471
LC47E:  jmp     LC9D5
; ----------------------------------------------------------------------------
; GET CHAR FROM ACTIVE CHANNEL
LC481:  jsr     CHRIN
        ldy     $0E
        bne     LC494
        cmp     #$0F
        bne     LC494
        pha
        lda     $0D
        eor     #$FF
        sta     $0D
        pla
LC494:  rts
; ----------------------------------------------------------------------------
; INTEPRET TEXT TO TOKEN 
LC495:  ldx     $77
        ldy     #$04
        sty     $09
LC49B:  lda     $0200,x
        bpl     LC4A7
        cmp     #$FF
        beq     LC4E2
        inx
        bne     LC49B
LC4A7:  cmp     #$20
        beq     LC4E2
        sta     $04
        cmp     #$22
        beq     LC507
        bit     $09
        bvs     LC4E2
        cmp     #$3F
        bne     LC4BD
        lda     #$99
        bne     LC4E2
LC4BD:  cmp     #$30
        bcc     LC4C5
        cmp     #$3C
        bcc     LC4E2
LC4C5:  sty     $6E
        ldy     #$00
        sty     $05
        dey
        stx     $77
        dex
LC4CF:  iny
        inx
LC4D1:  lda     $0200,x
        sec
        sbc     LC092,y
        beq     LC4CF
        cmp     #$80
        bne     LC50E
        ora     $05
LC4E0:  ldy     $6E
LC4E2:  inx
        iny
        sta     $01FB,y
        lda     $01FB,y
        beq     LC522
        sec
        sbc     #$3A
        beq     LC4F5
        cmp     #$49
        bne     LC4F7
LC4F5:  sta     $09
LC4F7:  sec
        sbc     #$55
        bne     LC49B
        sta     $04
LC4FE:  lda     $0200,x
        beq     LC4E2
        cmp     $04
        beq     LC4E2
LC507:  iny
        sta     $01FB,y
        inx
        bne     LC4FE
LC50E:  ldx     $77
        inc     $05
LC512:  iny
        lda     LC091,y
        bpl     LC512
        lda     LC092,y
        bne     LC4D1
        lda     $0200,x
        bpl     LC4E0
LC522:  sta     $01FD,y
        dec     $78
        lda     #$FF
        sta     $77
        rts
; ----------------------------------------------------------------------------
; CHECKS BASIC LINE-NUMBER
LC52C:  lda     $28
        ldx     $29
LC530:  ldy     #$01
        sta     $5C
        stx     $5D
        lda     ($5C),y
        beq     LC559
        iny
        iny
        lda     $12
        cmp     ($5C),y
        bcc     LC55A
        beq     LC547
        dey
        bne     LC550
LC547:  lda     $11
        dey
        cmp     ($5C),y
        bcc     LC55A
        beq     LC55A
LC550:  dey
        lda     ($5C),y
        tax
        dey
        lda     ($5C),y
        bcs     LC530
LC559:  clc
LC55A:  rts
; ----------------------------------------------------------------------------
; BASIC: NEW
        bne     LC55A
        lda     #$00
        tay
        sta     ($28),y      ; Pointer: Start of BASIC	Text
        iny
        sta     ($28),y
        lda     $28
        clc
        adc     #$02
        sta     $2A
        lda     $29
        adc     #$00
        sta     $2B
LC572:  jsr     LC5A7
        lda     #$00
;BASIC: CLR
        bne     LC5A6
LC579:  lda     $34          ; Pointer: Highest Address Used by BASIC
        ldy     $35
        sta     $30          ; Pointer: Bottom of String Storage
        sty     $31
        jsr     CLRALL
        lda     $2A          ; Pointer: Start of BASIC	Variables
        ldy     $2B
        sta     $2C          ; Pointer: Start of BASIC Arrays
        sty     $2D
        sta     basend_lo
        sty     basend_hi
        jsr     LC730
LC593:  ldx     #$16
        stx     $13
        pla
        tay
        pla
        ldx     #$FA
        txs
        pha
        tya
        pha
        lda     #$00
        sta     $3B          ; Pointer: BASIC Statement for CONT
        sta     $0A          ; Flag: Subscript Ref / User Function Call
LC5A6:  rts
; ----------------------------------------------------------------------------
; RESET CHRGET-POINTER
LC5A7:  clc
        lda     $28
        adc     #$FF
        sta     $77
        lda     $29
        adc     #$FF
        sta     $78
        rts
; ----------------------------------------------------------------------------
; BASIC: LIST
        bcc     LC5BD
        beq     LC5BD
        cmp     #$AB
        bne     LC5A6
LC5BD:  jsr     LC873
        jsr     LC52C
        jsr     CHRGOT
        beq     LC5D4
        cmp     #$AB
        bne     LC55A
        jsr     CHRGET
        jsr     LC873
        bne     LC55A
LC5D4:  pla
        pla
        lda     $11
        ora     $12
        bne     LC5E2
        lda     #$FF
        sta     $11
        sta     $12
LC5E2:  ldy     #$01
        sty     $09
        lda     ($5C),y
        beq     LC62D
        jsr     STOPKEY
        jsr     LC9E2
        iny
        lda     ($5C),y
        tax
        iny
        lda     ($5C),y
        cmp     $12
        bne     LC5FF
        cpx     $11
        beq     LC601
LC5FF:  bcs     LC62D
LC601:  sty     $46
        jsr     LDCD9
        lda     #$20
LC608:  ldy     $46
        and     #$7F
LC60C:  jsr     LCA45
        cmp     #$22
        bne     LC619
        lda     $09
        eor     #$FF
        sta     $09
LC619:  iny
        beq     LC62D
        lda     ($5C),y
        bne     LC630
        tay
        lda     ($5C),y
        tax
        iny
        lda     ($5C),y
        stx     $5C
        sta     $5D
        bne     LC5E2
LC62D:  jmp     READY
; ----------------------------------------------------------------------------
LC630:  bpl     LC60C
        cmp     #$FF
        beq     LC60C
        bit     $09
        bmi     LC60C
        sec
        sbc     #$7F
        tax
        sty     $46
        ldy     #$FF
LC642:  dex
        beq     LC64D
LC645:  iny
        lda     LC092,y
        bpl     LC645
        bmi     LC642
LC64D:  iny
        lda     LC092,y
        bmi     LC608
        jsr     LCA45
        bne     LC64D
; BASIC: STOP, END, BREAK , FOR-NEXT LOOP
STOP:   lda     #$80
        sta     $0A
        jsr     LC8AD
        jsr     FNDFOR
        bne     LC669
        txa
        adc     #$0F
        tax
        txs
LC669:  pla
        pla
        lda     #$09
        jsr     LC31B
        jsr     LC80E
        clc
        tya
        adc     $77
        pha
        lda     $78
        adc     #$00
        pha
        lda     $37
        pha
        lda     $36
        pha
        lda     #$A4
        jsr     LCDFA
        jsr     LCC8E
        jsr     LCC8B
        lda     $63
        ora     #$7F
        and     $5F
        sta     $5F
        lda     #$A1
        ldy     #$C6
        sta     L001F
        sty     $20
        jmp     LCD44
; ----------------------------------------------------------------------------
        lda     #$C8
        ldy     #$D8
        jsr     LDAAE
        jsr     CHRGOT
        cmp     #$A9
        bne     LC6B5
        jsr     CHRGET
        jsr     LCC8B
LC6B5:  jsr     LDB37
        jsr     LCD39
        lda     $47
        pha
        lda     $46
        pha
        lda     #$81
        pha
LC6C4:  jsr     STOPKEY
        lda     $77
        ldy     $78
        cpy     #$02
        nop
        beq     LC6D4
        sta     $3A
        sty     $3B
LC6D4:  ldy     #$00
        lda     ($77),y
        bne     LC71A
        ldy     #$02
        lda     ($77),y
        clc
        bne     LC6E4
        jmp     LC75B
; ----------------------------------------------------------------------------
LC6E4:  iny
        lda     ($77),y
        sta     $36
        iny
        lda     ($77),y
        sta     $37
        tya
        adc     $77
        sta     $77
        bcc     LC6F7
        inc     $78
LC6F7:  jsr     CHRGET
        jsr     LC700
        jmp     LC6C4
; ----------------------------------------------------------------------------
; RUN BASIC-COMMAND
LC700:  beq     LC73E
LC702:  sbc     #$80
        bcc     LC717
        cmp     #$23
        bcs     LC721
        asl
        tay
        lda     LC001,y
        pha
        lda     LC000,y
        pha
        jmp     CHRGET
; ----------------------------------------------------------------------------
LC717:  jmp     LC8AD
; ----------------------------------------------------------------------------
LC71A:  cmp     #$3A
        beq     LC6F7
LC71E:  jmp     LCE03        ; Syntax-Error Vector
; ----------------------------------------------------------------------------
LC721:  cmp     #$4B
        bne     LC71E
        jsr     CHRGET
        lda     #$A4
        jsr     LCDFA
        jmp     LC7AD
; ----------------------------------------------------------------------------
;BASIC: RESTORE
LC730:  sec
        lda     $28
        sbc     #$01
        ldy     $29
        bcs     LC73A
        dey
LC73A:  sta     $3E
        sty     $3F
LC73E:  rts
; ----------------------------------------------------------------------------
; BASIC: STOP
        bcs     LC742
; BASIC: FOR-NEXT
FOR_NEXT:
        clc
LC742:  bne     LC784
        lda     $77
        ldy     $78
        ldx     $37
        inx
        beq     LC759
        sta     $3A
        sty     $3B
LC751:  lda     $36
        ldy     $37
        sta     $38
        sty     $39
LC759:  pla
        pla
LC75B:  lda     #$A2
        ldy     #$C2
        ldx     #$00
        stx     $0D
        bcc     LC768
        jmp     LC37E
; ----------------------------------------------------------------------------
LC768:  jmp     READY
; ----------------------------------------------------------------------------
;BASIC: CONT
        bne     LC784
        ldx     #$DB
        ldy     $3B
        bne     LC776
        jmp     ERROR
; ----------------------------------------------------------------------------
LC776:  lda     $3A
        sta     $77
        sty     $78
        lda     $38
        ldy     $39
        sta     $36
        sty     $37
LC784:  rts
; ----------------------------------------------------------------------------
; BASIC: RUN
        bne     LC78A
        jmp     LC572
; ----------------------------------------------------------------------------
LC78A:  jsr     LC579
        jmp     LC7A4
; ----------------------------------------------------------------------------
; BASIC: GOSUB
        lda     #$03
        jsr     LC31B
        lda     $78
        pha
        lda     $77
        pha
        lda     $37
        pha
        lda     $36
        pha
        lda     #$8D
        pha
LC7A4:  jsr     CHRGOT
        jsr     LC7AD
        !by $4C
        !by $C4
; BASIC: GOTO
GOTO:   !by $C6
LC7AD:  jsr     LC873
        jsr     LC811
        lda     $37
        cmp     $12
        bcs     LC7C4
        tya
        sec
        adc     $77
        ldx     $78
        bcc     LC7C8
        inx
        bcs     LC7C8
LC7C4:  lda     $28
        ldx     $29
LC7C8:  jsr     LC530
        bcc     LC7EB
        lda     $5C
        sbc     #$01
        sta     $77
        lda     $5D
        sbc     #$00
        sta     $78
LC7D9:  rts
; ----------------------------------------------------------------------------
;BASIC: RETURN
        bne     LC7D9
        lda     #$FF
        sta     $47
        jsr     FNDFOR
        txs
        cmp     #$8D
        beq     LC7F3
        ldx     #$16
        !by  $2C
LC7EB:  ldx     #$5A
        jmp     ERROR
; ----------------------------------------------------------------------------
LC7F0:  jmp     LCE03
; ----------------------------------------------------------------------------
LC7F3:  pla
        pla
        sta     $36
        pla
        sta     $37
        pla
        sta     $77
        pla
        sta     $78
; BASIC: DATA
DATA:   jsr     LC80E
LC803:  tya
        clc
        adc     $77
        sta     $77
        bcc     LC80D
        inc     $78
LC80D:  rts
; ----------------------------------------------------------------------------
; LOOK FOR NEXT BASIC-COMMAND
LC80E:  ldx     #$3A
        !by $2C
LC811:  ldx     #$00
        stx     $03
        ldy     #$00
        sty     $04
LC819:  lda     $04
        ldx     $03
        sta     $03
        stx     $04
LC821:  lda     ($77),y
        beq     LC80D
        cmp     $04
        beq     LC80D
        iny
        cmp     #$22
        bne     LC821
        beq     LC819
        jsr     LCC9F
        jsr     CHRGOT
        cmp     #$89
        beq     LC83F
        lda     #$A7
        jsr     LCDFA
LC83F:  lda     $5E
        bne     LC848
;BASIC: REM
        jsr     LC811
        beq     LC803
LC848:  jsr     CHRGOT
        bcs     LC850
        jmp     LC7AD
; ----------------------------------------------------------------------------
LC850:  jmp     LC700
; ----------------------------------------------------------------------------
;BASIC: ON
        jsr     LD678
        pha
        cmp     #$8D
        beq     LC85F
LC85B:  cmp     #$89
        bne     LC7F0
LC85F:  dec     $62
        bne     LC867
        pla
        jmp     LC702
; ----------------------------------------------------------------------------
LC867:  jsr     CHRGET
        jsr     LC873
        cmp     #$2C
        beq     LC85F
        pla
LC872:  rts
; ----------------------------------------------------------------------------
LC873:  ldx     #$00
        stx     $11
        stx     $12
        bcs     LC872
        sbc     #$2F
        sta     $03
        lda     $12
        sta     L001F
        cmp     #$19
        bcs     LC85B
        lda     $11
        asl
        rol     L001F
        asl
        rol     L001F
        adc     $11
        sta     $11
        lda     L001F
        adc     $12
        sta     $12
        asl     $11
        rol     $12
        lda     $11
        adc     $03
        sta     $11
        bcc     LC8A7
        inc     $12
LC8A7:  jsr     CHRGET
        !by   $4C
        !by   $79
; BASIC: LET
LET:    iny
LC8AD:  jsr     LCF6D
        sta     $46
        sty     $47
        lda     #$B2
        jsr     LCDFA
        lda     $08
        pha
        lda     $07
        pha
        jsr     LCC9F
        pla
        rol
        jsr     LCC91
        bne     LC8E1
        pla
LC8CA:  bpl     LC8DE
        jsr     LDB27
        jsr     LD09A
        ldy     #$00
        lda     $61
        sta     ($46),y
        iny
        lda     $62
        sta     ($46),y
        rts
; ----------------------------------------------------------------------------
LC8DE:  jmp     LDADC
; ----------------------------------------------------------------------------
LC8E1:  pla
LC8E2:  ldy     $47
        cpy     #$DE
        bne     LC937
        jsr     LD580
        cmp     #$06
        bne     LC92F
        ldy     #$00
        sty     $5E
        sty     $63
LC8F5:  sty     $6E
        jsr     LC928
        jsr     LD9EE
        inc     $6E
        ldy     $6E
        jsr     LC928
        jsr     LDB18
        tax
        beq     LC90F
        inx
        txa
        jsr     LD9F9
LC90F:  ldy     $6E
        iny
        cpy     #$06
        bne     LC8F5
        jsr     LD9EE
        jsr     LDBA7
        ldx     #$02
        sei
LC91F:  lda     $60,x
        sta     $8D,x
        dex
        bpl     LC91F
        cli
        rts
; ----------------------------------------------------------------------------
LC928:  lda     (L001F),y
        jsr     L007D
        bcc     LC932
LC92F:  jmp     LD123
; ----------------------------------------------------------------------------
LC932:  sbc     #$2F
        jmp     LDC8A
; ----------------------------------------------------------------------------
LC937:  ldy     #$02
        lda     ($61),y
        cmp     $31
        bcc     LC956
        bne     LC948
        dey
        lda     ($61),y
        cmp     $30
        bcc     LC956
LC948:  ldy     $62
        cpy     $2B
        bcc     LC956
        bne     LC95D
        lda     $61
        cmp     $2A
        bcs     LC95D
LC956:  lda     $61
        ldy     $62
        jmp     LC973
; ----------------------------------------------------------------------------
LC95D:  ldy     #$00
        lda     ($61),y
        jsr     LD34F
        lda     $4D
        ldy     $4E
        sta     $6C
        sty     $6D
        jsr     LD554
        lda     #$5E
        ldy     #$00
LC973:  sta     $4D
        sty     $4E
        jsr     LD5B5
        ldy     #$00
        lda     ($4D),y
        sta     ($46),y
        iny
        lda     ($4D),y
        sta     ($46),y
        iny
        lda     ($4D),y
        sta     ($46),y
        rts
; ----------------------------------------------------------------------------
; BASIC: PRINT #
        jsr     LC991
        jmp     LCAB7
; ----------------------------------------------------------------------------
; BASIC: CMD
LC991:  jsr     LD678
        beq     LC99B
        lda     #$2C
        jsr     LCDFA
LC99B:  php
        jsr     CHKOUT
        stx     $0E
        plp
        jmp     LC9AB
; ----------------------------------------------------------------------------
LC9A5:  jsr     LCA1F
LC9A8:  jsr     CHRGOT
; BASIC: PRINT
LC9AB:  beq     LC9E2
LC9AD:  beq     LC9EE
        cmp     #$A3
        beq     LC9FC
        cmp     #$A6
        clc
        beq     LC9FC
        cmp     #$2C
        beq     LC9EF
        cmp     #$3B
        beq     LCA11
        jsr     LCC9F
        bit     $07
        bmi     LC9A5
        jsr     LDCE9
        jsr     LD361
        jsr     LCA1F
        jsr     LCA39
        bne     LC9A8
LC9D5:  lda     #$00
        sta     $0200,x
        ldx     #$FF
        ldy     #$01
        lda     $0E
        bne     LC9EE
LC9E2:  lda     #$0D
        jsr     LCA45
        lda     #$0A
        jsr     LCA45
LC9EC:  eor     #$FF
LC9EE:  rts
; ----------------------------------------------------------------------------
LC9EF:  lda     $C6
        sec
LC9F2:  sbc     #$0A
        bcs     LC9F2
        eor     #$FF
        adc     #$01
        bne     LCA0C
LC9FC:  php
        jsr     LD675
        cmp     #$29
        bne     LCA61
        plp
        bcc     LCA0D
        txa
        sbc     $C6
        bcc     LCA11
LCA0C:  tax
LCA0D:  inx
LCA0E:  dex
        bne     LCA17
LCA11:  jsr     CHRGET
        jmp     LC9AD
; ----------------------------------------------------------------------------
LCA17:  jsr     LCA39
        bne     LCA0E
LCA1C:  jsr     LD361
LCA1F:  jsr     LD580
        tax
        ldy     #$00
        inx
LCA26:  dex
        beq     LC9EE
        lda     (L001F),y
        jsr     LCA45
        iny
        cmp     #$0D
        bne     LCA26
        jsr     LC9EC
        jmp     LCA26
; ----------------------------------------------------------------------------
LCA39:  lda     $0E
        beq     LCA40
        lda     #$20
        !by   $2C
LCA40:  lda     #$1D
        !by   $2C
LCA43:  lda     #$3F
LCA45:  bit     $0D
        bmi     LCA4C
        jsr     CHROUT
LCA4C:  and     #$FF
        rts
; ----------------------------------------------------------------------------
LCA4F:  lda     $0B
        beq     LCA64
        bmi     LCA59
        ldy     #$FF
        bne     LCA5D
LCA59:  lda     $3C
        ldy     $3D
LCA5D:  sta     $36
        sty     $37
LCA61:  jmp     LCE03
; ----------------------------------------------------------------------------
LCA64:  lda     $0E
        beq     LCA6D
        ldx     #$BF
        jmp     ERROR
; ----------------------------------------------------------------------------
LCA6D:  lda     #$0D
        ldy     #$CC
        jsr     LCA1C
        lda     $3A
        ldy     $3B
        sta     $77
        sty     $78
        rts
; ----------------------------------------------------------------------------
; BASIC: GET
        jsr     LD280
        cmp     #$23
        bne     LCA94
        jsr     CHRGET
        jsr     LD678
        lda     #$2C
        jsr     LCDFA
        jsr     CHKIN
        stx     $0E
LCA94:  ldx     #$01
        ldy     #$02
        lda     #$00
        sta     $0201
        lda     #$40
        jsr     LCB10
        ldx     $0E
        bne     LCAB9
        rts
; ----------------------------------------------------------------------------
; BASIC: INPUT#
INPUT_: jsr     LD678
        lda     #$2C
        jsr     LCDFA
        jsr     CHKIN
        stx     $0E
        jsr     LCAD2
LCAB7:  lda     $0E
LCAB9:  jsr     CLRCH
        ldx     #$00
        stx     $0E
        rts
; ----------------------------------------------------------------------------
; BASIC: INPUT
INPUT:  lsr     $0D
        cmp     #$22
        bne     LCAD2
        jsr     LCDB8
        lda     #$3B
        jsr     LCDFA
        jsr     LCA1F
LCAD2:  jsr     LD280
        lda     #$2C
        sta     $01FF
LCADA:  jsr     LCAFA
        lda     $0E
        beq     LCAED
        lda     $96
        and     #$02
        beq     LCAED
        jsr     LCAB7
        jmp     DATA
; ----------------------------------------------------------------------------
LCAED:  lda     $0200
        bne     LCB0E
        lda     $0E
        bne     LCADA
        clc
        jmp     LC751
; ----------------------------------------------------------------------------
LCAFA:  lda     $0E
        bne     LCB04
        jsr     LCA43
        jsr     LCA39
LCB04:  jmp     LC46F
; ----------------------------------------------------------------------------
; BASIC: READ
READ:   ldx     $3E
        ldy     $3F
        lda     #$98
        !by   $2C
LCB0E:  lda     #$00
LCB10:  sta     $0B
        stx     $40
        sty     $41
LCB16:  jsr     LCF6D
        sta     $46
        sty     $47
        lda     $77
        ldy     $78
        sta     $48
        sty     $49
        ldx     $40
        ldy     $41
        stx     $77
        sty     $78
        jsr     CHRGOT
        bne     LCB52
        bit     $0B
        bvc     LCB42
        jsr     GET
        sta     $0200
        ldx     #$FF
        ldy     #$01
        bne     LCB4E
LCB42:  bmi     LCBB9
        lda     $0E
        bne     LCB4B
        jsr     LCA43
LCB4B:  jsr     LCAFA
LCB4E:  stx     $77
        sty     $78
LCB52:  jsr     CHRGET
        !by   $24
LCB56:  !by   $07
        bpl     LCB8A
        bit     $0B
        bvc     LCB66
        inx
        stx     $77
        lda     #$00
        sta     $03
        beq     LCB72
LCB66:  sta     $03
        cmp     #$22
        beq     LCB73
        lda     #$3A
        sta     $03
        lda     #$2C
LCB72:  clc
LCB73:  sta     $04
        lda     $77
        ldy     $78
        adc     #$00
        bcc     LCB7E
        iny
LCB7E:  jsr     LD367
        jsr     LD6BD
        jsr     LC8E2
        jmp     LCB92
; ----------------------------------------------------------------------------
LCB8A:  jsr     LDBFF
        lda     $08
        jsr     LC8CA
LCB92:  jsr     CHRGOT
        beq     LCB9E
        cmp     #$2C
        beq     LCB9E
        jmp     LCA4F
; ----------------------------------------------------------------------------
LCB9E:  lda     $77
        ldy     $78
        sta     $40
        sty     $41
        lda     $48
        ldy     $49
        sta     $77
        sty     $78
        jsr     CHRGOT
        beq     LCBDF
        jsr     LCDF8
        jmp     LCB16
; ----------------------------------------------------------------------------
LCBB9:  jsr     LC80E
        iny
        tax
        bne     LCBD2
        ldx     #$2A
        iny
        lda     ($77),y
        beq     LCC34
        iny
        lda     ($77),y
        sta     $3C
        iny
        lda     ($77),y
        iny
        sta     $3D
LCBD2:  lda     ($77),y
        tax
        jsr     LC803
        cpx     #$83
        bne     LCBB9
        jmp     LCB52
; ----------------------------------------------------------------------------
LCBDF:  lda     $40
        ldy     $41
        ldx     $0B
        bpl     LCBEA
        jmp     LC73A
; ----------------------------------------------------------------------------
LCBEA:  ldy     #$00
        lda     ($40),y
        beq     LCBFB
        lda     $0E
        bne     LCBFB
        lda     #$FC
        ldy     #$CB
        jmp     LCA1C
; ----------------------------------------------------------------------------
LCBFB:  rts
; ----------------------------------------------------------------------------
        !by  $3f,$45,$58,$54,$52,$41,$20,$49 ;   `?EXTRA 
        !by  $47,$4e,$4f,$52,$45,$44,$0d,$0a ; IGNORED..
        !by  $00,$3f,$52,$45,$44,$4f,$20,$46 ; ..?REDO 
        !by  $52,$4f,$4d,$20,$53,$54,$41,$52 ; FROM STAR
        !by  $54,$0d,$0a,$00                 ; T...
; BASIC: NEXT
NEXT:   bne     LCC26
        ldy     #$00
        beq     LCC29
LCC26:  jsr     LCF6D
LCC29:  sta     $46
        sty     $47
        jsr     FNDFOR
        beq     LCC36
        ldx     #$00
LCC34:  beq     LCC9C
LCC36:  txs
        txa
        clc
        adc     #$04
        pha
        adc     #$06
        sta     $21
        pla
        ldy     #$01
        jsr     LDAAE
        tsx
        lda     $0109,x
        sta     $63
        lda     $46
        ldy     $47
        jsr     LD773
        jsr     LDADC
        ldy     #$01
        jsr     LDB69
        tsx
        sec
        sbc     $0109,x
        beq     LCC79
        lda     $010F,x
        sta     $36
        lda     $0110,x
        sta     $37
        lda     $0112,x
        sta     $77
        lda     $0111,x
        sta     $78
LCC76:  jmp     LC6C4
; ----------------------------------------------------------------------------
LCC79:  txa
        adc     #$11
        tax
        txs
        jsr     CHRGOT
        cmp     #$2C
        bne     LCC76
        jsr     CHRGET
        jsr     LCC26
LCC8B:  jsr     LCC9F
LCC8E:  clc
        !by   $24
LCC90:  sec
LCC91:  bit     $07
        bmi     LCC98
        bcs     LCC9A
LCC97:  rts
; ----------------------------------------------------------------------------
LCC98:  bcs     LCC97
LCC9A:  ldx     #$A3
LCC9C:  jmp     ERROR
; ----------------------------------------------------------------------------
LCC9F:  ldx     $77
        bne     LCCA5
        dec     $78
LCCA5:  dec     $77
        ldx     #$00
        !by   $24
LCCAA:  pha
        txa
        pha
        lda     #$01
        jsr     LC31B
        jsr     LCD84
        lda     #$00
        sta     $4A
LCCB9:  jsr     CHRGOT
LCCBC:  sec
        sbc     #$B1
        bcc     LCCD8
        cmp     #$03
        bcs     LCCD8
        cmp     #$01
        rol
        eor     #$01
        eor     $4A
        cmp     $4A
        bcc     LCD31
        sta     $4A
        jsr     CHRGET
        jmp     LCCBC
; ----------------------------------------------------------------------------
LCCD8:  ldx     $4A
        bne     LCD08
        bcs     LCD59
        adc     #$07
        bcc     LCD59
        adc     $07
        bne     LCCE9
        jmp     LD517
; ----------------------------------------------------------------------------
LCCE9:  adc     #$FF
        sta     L001F
        asl
        adc     L001F
        tay
LCCF1:  pla
        cmp     LC074,y
        bcs     LCD5E
        jsr     LCC8E
LCCFA:  pha
LCCFB:  jsr     LCD21
        pla
        ldy     $48
        bpl     LCD1A
        tax
        beq     LCD5C
        bne     LCD67
LCD08:  lsr     $07
        txa
        rol
        ldx     $77
        bne     LCD12
        dec     $78
LCD12:  dec     $77
        ldy     #$1B
        sta     $4A
        bne     LCCF1
LCD1A:  cmp     LC074,y
        bcs     LCD67
        bcc     LCCFA
LCD21:  lda     LC076,y
        pha
        lda     LC075,y
        pha
        jsr     LCD34
        lda     $4A
        jmp     LCCAA
; ----------------------------------------------------------------------------
LCD31:  jmp     LCE03
; ----------------------------------------------------------------------------
LCD34:  lda     $63
        ldx     LC074,y
LCD39:  tay
        pla
        sta     L001F
        inc     L001F
        pla
        sta     $20
        tya
        pha
LCD44:  jsr     LDB27
        lda     $62
        pha
        lda     $61
        pha
        lda     $60
        pha
        lda     $5F
        pha
        lda     $5E
        pha
        jmp     (L001F)
; ----------------------------------------------------------------------------
LCD59:  ldy     #$FF
        pla
LCD5C:  beq     LCD81
LCD5E:  cmp     #$64
        beq     LCD65
        jsr     LCC8E
LCD65:  sty     $48
LCD67:  pla
        lsr
        sta     $0C
        pla
        sta     $66
        pla
        sta     $67
        pla
        sta     $68
        pla
        sta     $69
        pla
        sta     $6A
        pla
        sta     $6B
        eor     $63
        sta     $6C
LCD81:  lda     $5E
        rts
; ----------------------------------------------------------------------------
LCD84:  lda     #$00
        sta     $07
LCD88:  jsr     CHRGET
        bcs     LCD90
LCD8D:  jmp     LDBFF
; ----------------------------------------------------------------------------
LCD90:  jsr     LCFF7
        bcs     LCE0F
        cmp     #$FF
        bne     LCDA8
        lda     #$A3
        ldy     #$CD
        jsr     LDAAE
        jmp     CHRGET
; ----------------------------------------------------------------------------
        !by   $82
        eor     #$0F
        !by   $DA
        !by   $A1
LCDA8:  cmp     #$2E
        beq     LCD8D
        cmp     #$AB
        beq     LCE08
        cmp     #$AA
        beq     LCD88
        cmp     #$22
        bne     LCDC7
LCDB8:  lda     $77
        ldy     $78
        adc     #$00
        bcc     LCDC1
        iny
LCDC1:  jsr     LD361
        jmp     LD6BD
; ----------------------------------------------------------------------------
LCDC7:  cmp     #$A8
        bne     LCDDE
        ldy     #$18
        bne     LCE0A
        jsr     LD09A
        lda     $62
        eor     #$FF
        tay
        lda     $61
        eor     #$FF
        jmp     LD26D
; ----------------------------------------------------------------------------
LCDDE:  cmp     #$A5
        bne     LCDE5
        jmp     LD2CE
; ----------------------------------------------------------------------------
LCDE5:  cmp     #$B4
        bcc     LCDEC
        jmp     LCE89
; ----------------------------------------------------------------------------
LCDEC:  jsr     LCDF5
        jsr     LCC9F
LCDF2:  lda     #$29
        !by   $2C
LCDF5:  lda     #$28
        !by   $2C
LCDF8:  lda     #$2C
LCDFA:  ldy     #$00
        cmp     ($77),y
        bne     LCE03
        jmp     CHRGET
; ----------------------------------------------------------------------------
LCE03:  ldx     #$10
        jmp     ERROR
; ----------------------------------------------------------------------------
LCE08:  ldy     #$15
LCE0A:  pla
        pla
        jmp     LCCFB
; ----------------------------------------------------------------------------
LCE0F:  jsr     LCF6D
        sta     $61
        sty     $62
        lda     $42
        ldy     $43
        ldx     $07
        beq     LCE43
        ldx     #$00
        stx     $6D
        bit     $62
        bpl     LCE42
        cmp     #$54
        bne     LCE42
        cpy     #$C9
        bne     LCE42
        jsr     LCE69
        sty     $5B
        dey
        sty     $6E
        ldy     #$06
        sty     $5A
        ldy     #$24
        jsr     LDD74
        jmp     LD349
; ----------------------------------------------------------------------------
LCE42:  rts
; ----------------------------------------------------------------------------
LCE43:  ldx     $08
        bpl     LCE54
        ldy     #$00
        lda     ($61),y
        tax
        iny
        lda     ($61),y
        tay
        txa
        jmp     LD26D
; ----------------------------------------------------------------------------
LCE54:  bit     $62
        bpl     LCE82
        cmp     #$54
        bne     LCE75
        cpy     #$49
        bne     LCE82
        jsr     LCE69
        tya
        ldx     #$A0
        jmp     LDB5B
; ----------------------------------------------------------------------------
; time to ti
LCE69:  lda     #$8B
        ldy     #$00
        sei
        jsr     LDAAE
        cli
        sty     $5F
        rts
; ----------------------------------------------------------------------------
LCE75:  cmp     #$53
        bne     LCE82
        cpy     #$54
        bne     LCE82
        lda     $96
        jmp     LDB48
; ----------------------------------------------------------------------------
LCE82:  lda     $61
        ldy     $62
        jmp     LDAAE
; ----------------------------------------------------------------------------
; check function
LCE89:  asl     
        pha
        tax
        jsr     CHRGET
        cpx     #$8F
        bcc     LCEB3
        jsr     LCDF5        ; check for open bracket
        jsr     LCC9F        ; input and compute all terms
        jsr     LCDF8        ; check for comma
        jsr     LCC90        ; if var no string : type mismatch error
        pla
        tax
        lda     $62
        pha
        lda     $61
        pha
        txa
        pha
        jsr     LD678
        pla
        tay
        txa
        pha
        jmp     LCEB8
; ----------------------------------------------------------------------------
LCEB3:  jsr     LCDEC
        pla
        tay
LCEB8:  lda     $BFDE,y
        sta     $52
        lda     $BFDF,y
        sta     $53
        jsr     L0051
        jmp     LCC8E
; ----------------------------------------------------------------------------
        ldy     #$FF
        bit     $00A0
        sty     $05
        jsr     LD09A
        lda     $61
        eor     $05
        sta     $03
        lda     $62
        eor     $05
        sta     $04
        jsr     LDB08
        jsr     LD09A
        lda     $62
        eor     $05
        and     $04
        eor     $05
        tay
        lda     $61
        eor     $05
        and     $03
        eor     $05
        jmp     LD26D
; ----------------------------------------------------------------------------
        jsr     LCC91
        bcs     LCF10
        lda     $6B
        ora     #$7F
        and     $67
        sta     $67
        lda     #$66
        ldy     #$00
        jsr     LDB67
        tax
        jmp     LCF43
; ----------------------------------------------------------------------------
LCF10:  lda     #$00
        sta     $07
        dec     $4A
        jsr     LD580
        sta     $5E
        stx     $5F
        sty     $60
        lda     $69
        ldy     $6A
        jsr     LD584
        stx     $69
        sty     $6A
        tax
        sec
        sbc     $5E
        beq     LCF38
        lda     #$01
        bcc     LCF38
        ldx     $5E
        lda     #$FF
LCF38:  sta     $63
        ldy     #$FF
        inx
LCF3D:  iny
        dex
        bne     LCF48
        ldx     $63
LCF43:  bmi     LCF54
        clc
        bcc     LCF54
LCF48:  lda     ($69),y
        cmp     ($5F),y
        beq     LCF3D
        ldx     #$FF
        bcs     LCF54
        ldx     #$01
LCF54:  inx
        txa
        rol
        and     $0C
        beq     LCF5D
        lda     #$FF
LCF5D:  jmp     LDB48
; ----------------------------------------------------------------------------
; BASIC: DIM
LCF60:  jsr     LCDF8
DIM:    tax
        jsr     LCF72
        jsr     CHRGOT
        bne     LCF60
        rts
; ----------------------------------------------------------------------------
LCF6D:  ldx     #$00
        jsr     CHRGOT
LCF72:  stx     $06
LCF74:  sta     $42
        jsr     CHRGOT
        jsr     LCFF7
        bcs     LCF81
LCF7E:  jmp     LCE03
; ----------------------------------------------------------------------------
LCF81:  ldx     #$00
        stx     $07
        stx     $08
        jsr     CHRGET
        bcc     LCF91
        jsr     LCFF7
        bcc     LCF9C
LCF91:  tax
LCF92:  jsr     CHRGET
        bcc     LCF92
        jsr     LCFF7
        bcs     LCF92
LCF9C:  cmp     #$24
        bne     LCFA6
        lda     #$FF
        sta     $07
        bne     LCFB6
LCFA6:  cmp     #$25
        bne     LCFBD
        lda     $0A
        bne     LCF7E
        lda     #$80
        sta     $08
        ora     $42
        sta     $42
LCFB6:  txa
        ora     #$80
        tax
        jsr     CHRGET
LCFBD:  stx     $43
        sec
        ora     $0A
        sbc     #$28
        bne     LCFC9
        jmp     LD0AC
; ----------------------------------------------------------------------------
LCFC9:  lda     #$00
        sta     $0A
        lda     $2A
        ldx     $2B
        ldy     #$00
LCFD3:  stx     $5D
LCFD5:  sta     $5C
        cpx     $2D
        bne     LCFDF
        cmp     $2C
        beq     LD001
LCFDF:  lda     $42
        cmp     ($5C),y
        bne     LCFED
        lda     $43
        iny
        cmp     ($5C),y
        beq     LD069
        dey
LCFED:  clc
        lda     $5C
        adc     #$07
        bcc     LCFD5
        inx
        bne     LCFD3
LCFF7:  cmp     #$41
        bcc     LD000
        sbc     #$5B
        sec
        sbc     #$A5
LD000:  rts
; ----------------------------------------------------------------------------
LD001:  pla
        pha
        cmp     #$11
        bne     LD00C
LD007:  lda     #$1F
        ldy     #$DE
LD00B:  rts
; ----------------------------------------------------------------------------
LD00C:  lda     $42
        ldy     $43
        cmp     #$54
        bne     LD01F
        cpy     #$C9
        beq     LD007
        cpy     #$49
        bne     LD01F
LD01C:  jmp     LCE03
; ----------------------------------------------------------------------------
LD01F:  cmp     #$53
        bne     LD027
        cpy     #$54
        beq     LD01C
LD027:  lda     $2C
        ldy     $2D
        sta     $5C
        sty     $5D
        lda     basend_lo
        ldy     basend_hi
        sta     $57
        sty     $58
        clc
        adc     #$07
        bcc     LD03D
        iny
LD03D:  sta     $55
        sty     $56
        jsr     BLTU
        lda     $55
        ldy     $56
        iny
        sta     $2C
        sty     $2D
        ldy     #$00
        lda     $42
        sta     ($5C),y
        iny
        lda     $43
        sta     ($5C),y
        lda     #$00
        iny
        sta     ($5C),y
        iny
        sta     ($5C),y
        iny
        sta     ($5C),y
        iny
        sta     ($5C),y
        iny
        sta     ($5C),y
LD069:  lda     $5C
        clc
        adc     #$02
        ldy     $5D
        bcc     LD073
        iny
LD073:  sta     $44
        sty     $45
        rts
; ----------------------------------------------------------------------------
LD078:  lda     $05
        asl
        adc     #$05
        adc     $5C
        ldy     $5D
        bcc     LD084
        iny
LD084:  sta     $55
        sty     $56
        rts
; ----------------------------------------------------------------------------
!by $90,$80        
        brk
        brk
LD08D:  jsr     CHRGET
        jsr     LCC9F
LD093:  jsr     LCC8E
        lda     $63
        bmi     LD0A7
LD09A:  lda     $5E
        cmp     #$90
        bcc     LD0A9
        lda     #$89
        ldy     #$D0
        jsr     LDB67
LD0A7:  bne     LD123
LD0A9:  jmp     LDBA7
; ----------------------------------------------------------------------------
LD0AC:  lda     $06
        ora     $08
        pha
        lda     $07
        pha
        ldy     #$00
LD0B6:  tya
        pha
        lda     $43
        pha
        lda     $42
        pha
        jsr     LD08D
        pla
        sta     $42
        pla
        sta     $43
        pla
        tay
        tsx
        lda     $0102,x
        pha
        lda     $0101,x
        pha
        lda     $61
        sta     $0102,x
        lda     $62
        sta     $0101,x
        iny
        jsr     CHRGOT
        cmp     #$2C
        beq     LD0B6
        sty     $05
        jsr     LCDF2
        pla
        sta     $07
        pla
        sta     $08
        and     #$7F
        sta     $06
        ldx     $2C
        lda     $2D
LD0F7:  stx     $5C
        sta     $5D
        cmp     $2F
        bne     LD103
        cpx     basend_lo
        beq     LD13C
LD103:  ldy     #$00
        lda     ($5C),y
        iny
        cmp     $42
        bne     LD112
        lda     $43
        cmp     ($5C),y
        beq     LD128
LD112:  iny
        lda     ($5C),y
        clc
        adc     $5C
        tax
        iny
        lda     ($5C),y
        adc     $5D
        bcc     LD0F7
LD120:  ldx     #$6B
        !by   $2C
LD123:  ldx     #$35
LD125:  jmp     ERROR
; ----------------------------------------------------------------------------
LD128:  ldx     #$78
        lda     $06
        bne     LD125
        jsr     LD078
        lda     $05
        ldy     #$04
        cmp     ($5C),y
        bne     LD120
        jmp     LD1C6
; ----------------------------------------------------------------------------
LD13C:  jsr     LD078
        jsr     LC328
        lda     #$00
        tay
        sta     $6F
        ldx     #$05
        lda     $42
        sta     ($5C),y
        bpl     LD150
        dex
LD150:  iny
        lda     $43
        sta     ($5C),y
        bpl     LD159
        dex
        dex
LD159:  stx     $6E
        lda     $05
        iny
        iny
        iny
        sta     ($5C),y
LD162:  ldx     #$0B
        lda     #$00
        bit     $06
        bvc     LD172
        pla
        clc
        adc     #$01
        tax
        pla
        adc     #$00
LD172:  iny
        sta     ($5C),y
        iny
        txa
        sta     ($5C),y
        jsr     LD228
        stx     $6E
        sta     $6F
        ldy     L001F
        dec     $05
        bne     LD162
        adc     $56
        bcs     LD1E7
        sta     $56
        tay
        txa
        adc     $55
        bcc     LD195
        iny
        beq     LD1E7
LD195:  jsr     LC328
        sta     basend_lo
        sty     basend_hi
        lda     #$00
        inc     $6F
        ldy     $6E
        beq     LD1A9
LD1A4:  dey
        sta     ($55),y
        bne     LD1A4
LD1A9:  dec     $56
        dec     $6F
        bne     LD1A4
        inc     $56
        sec
        lda     basend_lo
        sbc     $5C
        ldy     #$02
        sta     ($5C),y
        lda     basend_hi
        iny
        sbc     $5D
        sta     ($5C),y
        lda     $06
        bne     LD227
        iny
LD1C6:  lda     ($5C),y
        sta     $05
        lda     #$00
        sta     $6E
LD1CE:  sta     $6F
        iny
        pla
        tax
        sta     $61
        pla
        sta     $62
        cmp     ($5C),y
        bcc     LD1EA
        bne     LD1E4
        iny
        txa
        cmp     ($5C),y
        bcc     LD1EB
LD1E4:  jmp     LD120
; ----------------------------------------------------------------------------
LD1E7:  jmp     ERRORIN
; ----------------------------------------------------------------------------
LD1EA:  iny
LD1EB:  lda     $6F
        ora     $6E
        clc
        beq     LD1FC
        jsr     LD228
        txa
        adc     $61
        tax
        tya
        ldy     L001F
LD1FC:  adc     $62
        stx     $6E
        dec     $05
        bne     LD1CE
        sta     $6F
        ldx     #$05
        lda     $42
        bpl     LD20D
        dex
LD20D:  lda     $43
        bpl     LD213
        dex
        dex
LD213:  stx     $25
        lda     #$00
        jsr     LD231
        txa
        adc     $55
        sta     $44
        tya
        adc     $56
        sta     $45
        tay
        lda     $44
LD227:  rts
; ----------------------------------------------------------------------------
LD228:  sty     L001F
        lda     ($5C),y
        sta     $25
        dey
        lda     ($5C),y
LD231:  sta     $26
        lda     #$10
        sta     $5A
        ldx     #$00
        ldy     #$00
LD23B:  txa
        asl
        tax
        tya
        rol
        tay
        bcs     LD1E7
        asl     $6E
        rol     $6F
        bcc     LD254
        clc
        txa
        adc     $25
        tax
        tya
        adc     $26
        tay
        bcs     LD1E7
LD254:  dec     $5A
        bne     LD23B
        rts
; ----------------------------------------------------------------------------
;BASIC: FRE
        lda     $07
        beq     LD260
        jsr     LD580
LD260:  jsr     LD400
        sec
        lda     $30
        sbc     basend_lo
        tay
        lda     $31
        sbc     basend_hi
LD26D:  ldx     #$00
        stx     $07
        sta     $5F
        sty     $60
        ldx     #$90
        jmp     LDB50
; ----------------------------------------------------------------------------
;BASIC: POS
        ldy     $C6
LD27C:  lda     #$00
        beq     LD26D
LD280:  ldx     $37
        inx
        bne     LD227
        ldx     #$95
        !by   $2C
LD288:  ldx     #$E9
        jmp     ERROR
; ----------------------------------------------------------------------------
;BASIC: DEF
        jsr     LD2BB
        jsr     LD280
        jsr     LCDF5
        lda     #$80
        sta     $0A
        jsr     LCF6D
        jsr     LCC8E
        jsr     LCDF2
        lda     #$B2
        jsr     LCDFA
        pha
        lda     $45
        pha
        lda     $44
        pha
        lda     $78
        pha
        lda     $77
        pha
        jsr     DATA
        jmp     LD329
; ----------------------------------------------------------------------------
LD2BB:  lda     #$A5
        jsr     LCDFA
        ora     #$80
        sta     $0A
        jsr     LCF74
        sta     $4B
        sty     $4C
        jmp     LCC8E
; ----------------------------------------------------------------------------
LD2CE:  jsr     LD2BB
        lda     $4C
        pha
        lda     $4B
        pha
        jsr     LCDEC
        jsr     LCC8E
        pla
        sta     $4B
        pla
        sta     $4C
        ldy     #$02
        lda     ($4B),y
        sta     $44
        tax
        iny
        lda     ($4B),y
        beq     LD288
        sta     $45
        iny
LD2F2:  lda     ($44),y
        pha
        dey
        bpl     LD2F2
        ldy     $45
        jsr     LDAE0
        lda     $78
        pha
        lda     $77
        pha
        lda     ($4B),y
        sta     $77
        iny
        lda     ($4B),y
        sta     $78
        lda     $45
        pha
        lda     $44
        pha
        jsr     LCC8B
        pla
        sta     $4B
        pla
        sta     $4C
        jsr     CHRGOT
        beq     LD323
        jmp     LCE03
; ----------------------------------------------------------------------------
LD323:  pla
        sta     $77
        pla
        sta     $78
LD329:  ldy     #$00
        pla
        sta     ($4B),y
        pla
        iny
        sta     ($4B),y
        pla
        iny
        sta     ($4B),y
        pla
        iny
        sta     ($4B),y
        pla
        iny
        sta     ($4B),y
        rts
; ----------------------------------------------------------------------------
;BASIC: STR$
        jsr     LCC8E
        ldy     #$00
        jsr     LDCEB
        pla
        pla
LD349:  lda     #$FF
        ldy     #$00
        beq     LD361
LD34F:  ldx     $61
        ldy     $62
        stx     $4D
        sty     $4E
LD357:  jsr     LD3CE
        stx     $5F
        sty     $60
        sta     $5E
        rts
; ----------------------------------------------------------------------------
LD361:  ldx     #$22
        stx     $03
        stx     $04
LD367:  sta     $6C
        sty     $6D
        sta     $5F
        sty     $60
        ldy     #$FF
LD371:  iny
        lda     ($6C),y
        beq     LD382
        cmp     $03
        beq     LD37E
        cmp     $04
        bne     LD371
LD37E:  cmp     #$22
        beq     LD383
LD382:  clc
LD383:  sty     $5E
        tya
        adc     $6C
        sta     $6E
        ldx     $6D
        bcc     LD38F
        inx
LD38F:  stx     $6F
        lda     $6D
        beq     LD399
        cmp     #$02
        bne     LD3A4
LD399:  tya
        jsr     LD34F
        ldx     $6C
        ldy     $6D
        jsr     LD562
LD3A4:  ldx     $13
        cpx     #$1F
        bne     LD3AF
        ldx     #$C8
LD3AC:  jmp     ERROR
; ----------------------------------------------------------------------------
LD3AF:  lda     $5E
        sta     $00,x
        lda     $5F
        sta     $01,x
        lda     $60
        sta     $02,x
        ldy     #$00
        stx     $61
        sty     $62
        sty     $6D
        dey
        sty     $07
        stx     $14
        inx
        inx
        inx
        stx     $13
        rts
; ----------------------------------------------------------------------------
LD3CE:  lsr     $09
LD3D0:  pha
        eor     #$FF
        sec
        adc     $30
        ldy     $31
        bcs     LD3DB
        dey
LD3DB:  cpy     basend_hi
        bcc     LD3F0
        bne     LD3E5
        cmp     basend_lo
        bcc     LD3F0
LD3E5:  sta     $30
        sty     $31
        sta     $32
        sty     $33
        tax
        pla
        rts
; ----------------------------------------------------------------------------
LD3F0:  ldx     #$4D
        lda     $09
        bmi     LD3AC
        jsr     LD400
        lda     #$80
        sta     $09
        pla
        bne     LD3D0
LD400:  ldx     $34
        lda     $35
LD404:  stx     $30
        sta     $31
        ldy     #$00
        sty     $4C
        sty     $4B
        lda     basend_lo
        ldx     basend_hi
        sta     $5C
        stx     $5D
        lda     #$16
        ldx     #$00
        sta     L001F
        stx     $20
LD41E:  cmp     $13
        beq     LD427
        jsr     LD4A1
        beq     LD41E
LD427:  lda     #$07
        sta     $50
        lda     $2A
        ldx     $2B
        sta     L001F
        stx     $20
LD433:  cpx     $2D
        bne     LD43B
        cmp     $2C
        beq     LD440
LD43B:  jsr     LD497
        beq     LD433
LD440:  sta     $55
        stx     $56
        lda     #$03
        sta     $50
LD448:  lda     $55
        ldx     $56
LD44C:  cpx     basend_hi
        bne     LD457
        cmp     basend_lo
        bne     LD457
        jmp     LD4E0
; ----------------------------------------------------------------------------
LD457:  sta     L001F
        stx     $20
        ldy     #$00
        lda     (L001F),y
        tax
        iny
        lda     (L001F),y
        php
        iny
        lda     (L001F),y
        adc     $55
        sta     $55
        iny
        lda     (L001F),y
        adc     $56
        sta     $56
        plp
        bpl     LD448
        txa
        bmi     LD448
        iny
        lda     (L001F),y
        ldy     #$00
        asl
        adc     #$05
        adc     L001F
        sta     L001F
        bcc     LD488
        inc     $20
LD488:  ldx     $20
LD48A:  cpx     $56
        bne     LD492
        cmp     $55
        beq     LD44C
LD492:  jsr     LD4A1
        beq     LD48A
LD497:  lda     (L001F),y
        bmi     LD4D0
        iny
        lda     (L001F),y
        bpl     LD4D0
        iny
LD4A1:  lda     (L001F),y
        beq     LD4D0
        iny
        lda     (L001F),y
        tax
        iny
        lda     (L001F),y
        cmp     $31
        bcc     LD4B6
        bne     LD4D0
        cpx     $30
        bcs     LD4D0
LD4B6:  cmp     $5D
        bcc     LD4D0
        bne     LD4C0
        cpx     $5C
        bcc     LD4D0
LD4C0:  stx     $5C
        sta     $5D
        lda     L001F
        ldx     $20
        sta     $4B
        stx     $4C
        lda     $50
        sta     $52
LD4D0:  lda     $50
        clc
        adc     L001F
        sta     L001F
        bcc     LD4DB
        inc     $20
LD4DB:  ldx     $20
        ldy     #$00
        rts
; ----------------------------------------------------------------------------
LD4E0:  lda     $4C
        ora     $4B
        beq     LD4DB
        lda     $52
        and     #$04
        lsr
        tay
        sta     $52
        lda     ($4B),y
        adc     $5C
        sta     $57
        lda     $5D
        adc     #$00
        sta     $58
        lda     $30
        ldx     $31
        sta     $55
        stx     $56
        jsr     BLTUC
        ldy     $52
        iny
        lda     $55
        sta     ($4B),y
        tax
        inc     $56
        lda     $56
        iny
        sta     ($4B),y
        jmp     LD404
; ----------------------------------------------------------------------------
LD517:  lda     $62
        pha
        lda     $61
        pha
        jsr     LCD84
        jsr     LCC90
        pla
        sta     $6C
        pla
        sta     $6D
        ldy     #$00
        lda     ($6C),y
        clc
        adc     ($61),y
        bcc     LD537
        ldx     #$B0
        jmp     ERROR
; ----------------------------------------------------------------------------
LD537:  jsr     LD34F
        jsr     LD554
        lda     $4D
        ldy     $4E
        jsr     LD584
        jsr     LD566
        lda     $6C
        ldy     $6D
        jsr     LD584
        jsr     LD3A4
        jmp     LCCB9
; ----------------------------------------------------------------------------
LD554:  ldy     #$00
        lda     ($6C),y
        pha
        iny
        lda     ($6C),y
        tax
        iny
        lda     ($6C),y
        tay
        pla
LD562:  stx     L001F
        sty     $20
LD566:  tay
        beq     LD573
        pha
LD56A:  dey
        lda     (L001F),y
        sta     ($32),y
        tya
        bne     LD56A
        pla
LD573:  clc
        adc     $32
        sta     $32
        bcc     LD57C
        inc     $33
LD57C:  rts
; ----------------------------------------------------------------------------
LD57D:  jsr     LCC90
LD580:  lda     $61
        ldy     $62
LD584:  sta     L001F
        sty     $20
        jsr     LD5B5
        php
        ldy     #$00
        lda     (L001F),y
        pha
        iny
        lda     (L001F),y
        tax
        iny
        lda     (L001F),y
        tay
        pla
        plp
        bne     LD5B0
        cpy     $31
        bne     LD5B0
        cpx     $30
        bne     LD5B0
        pha
        clc
        adc     $30
        sta     $30
        bcc     LD5AF
        inc     $31
LD5AF:  pla
LD5B0:  stx     L001F
        sty     $20
        rts
; ----------------------------------------------------------------------------
LD5B5:  cpy     $15
        bne     LD5C5
        cmp     $14
        bne     LD5C5
        sta     $13
        sbc     #$03
        sta     $14
        ldy     #$00
LD5C5:  rts
; ----------------------------------------------------------------------------
;BASIC: CHR$
        jsr     LD67B
        txa
        pha
        lda     #$01
        jsr     LD357
        pla
        ldy     #$00
        sta     ($5F),y
        pla
        pla
        jmp     LD3A4
; ----------------------------------------------------------------------------
;BASIC: LEFT$
        jsr     LD63B
        cmp     ($4D),y
        tya
LD5E0:  bcc     LD5E6
        lda     ($4D),y
        tax
        tya
LD5E6:  pha
LD5E7:  txa
LD5E8:  pha
        jsr     LD357
        lda     $4D
        ldy     $4E
        jsr     LD584
        pla
        tay
        pla
        clc
        adc     L001F
        sta     L001F
        bcc     LD5FF
        inc     $20
LD5FF:  tya
        jsr     LD566
        jmp     LD3A4
; ----------------------------------------------------------------------------
;BASIC: RIGHT$
        jsr     LD63B
        clc
        sbc     ($4D),y
        eor     #$FF
        jmp     LD5E0
; ----------------------------------------------------------------------------
;BASIC: MID$
        lda     #$FF
        sta     $62
        jsr     CHRGOT
        cmp     #$29
        beq     LD622
        jsr     LCDF8
        jsr     LD678
LD622:  jsr     LD63B
        beq     LD672
        dex
        txa
        pha
        clc
        ldx     #$00
        sbc     ($4D),y
        bcs     LD5E7
        eor     #$FF
        cmp     $62
        bcc     LD5E8
        lda     $62
        bcs     LD5E8
LD63B:  jsr     LCDF2
        pla
        tay
        pla
        sta     $52
        pla
        pla
        pla
        tax
        pla
        sta     $4D
        pla
        sta     $4E
        lda     $52
        pha
        tya
        pha
        ldy     #$00
        txa
        rts
; ----------------------------------------------------------------------------
;BASIC: LEN
        jsr     LD65C
        jmp     LD27C
; ----------------------------------------------------------------------------
LD65C:  jsr     LD57D
        ldx     #$00
        stx     $07
        tay
        rts
; ----------------------------------------------------------------------------
;BASIC: ASC
        jsr     LD65C
        beq     LD672
        ldy     #$00
        lda     (L001F),y
        tay
        jmp     LD27C
; ----------------------------------------------------------------------------
LD672:  jmp     LD123
; ----------------------------------------------------------------------------
LD675:  jsr     CHRGET
LD678:  jsr     LCC8B
LD67B:  jsr     LD093
        ldx     $61
        bne     LD672
        ldx     $62
        jmp     CHRGOT
; ----------------------------------------------------------------------------
;BASIC: VAL
        jsr     LD65C
        bne     LD68F
        jmp     LD803
; ----------------------------------------------------------------------------
LD68F:  ldx     $77
        ldy     $78
        stx     $6E
        sty     $6F
        ldx     L001F
        stx     $77
        clc
        adc     L001F
        sta     $21
        ldx     $20
        stx     $78
        bcc     LD6A7
        inx
LD6A7:  stx     $22
        ldy     #$00
        lda     ($21),y
        pha
        lda     #$00
        sta     ($21),y
        jsr     CHRGOT
        jsr     LDBFF
        pla
        ldy     #$00
        sta     ($21),y
LD6BD:  ldx     $6E
        ldy     $6F
        stx     $77
        sty     $78
        rts
; ----------------------------------------------------------------------------
LD6C6:  jsr     LCC8B
        jsr     LD6D2
LD6CC:  jsr     LCDF8
        jmp     LD678
; ----------------------------------------------------------------------------
LD6D2:  lda     $63
        bmi     LD672
        lda     $5E
        cmp     #$91
        bcs     LD672
        jsr     LDBA7
        lda     $61
        ldy     $62
        sty     $11
        sta     $12
        rts
; ----------------------------------------------------------------------------
;BASIC: PEEK
        lda     $12
        pha
        lda     $11
        pha
        jsr     LD6D2
        ldy     #$00
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        lda     ($11),y
        tay
        pla
        sta     $11
        pla
        sta     $12
        jmp     LD27C
; ----------------------------------------------------------------------------
; BASIC: POKE
        jsr     LD6C6
        txa
        ldy     #$00
        sta     ($11),y
        rts
; ----------------------------------------------------------------------------
; BASIC: WAIT
        jsr     LD6C6
        stx     $46
        ldx     #$00
        jsr     CHRGOT
        beq     LD745
        jsr     LD6CC
LD71F:  stx     $47
        ldy     #$00
LD723:  lda     ($11),y
        eor     $47
        and     $46
        beq     LD723
LD72B:  rts
; ----------------------------------------------------------------------------
LD72C:  lda     #$1D
        ldy     #$DE
        jmp     LD773
; ----------------------------------------------------------------------------
LD733:  jsr     LD998
LD736:  lda     $63
        eor     #$FF
        sta     $63
        eor     $6B
        sta     $6C
        lda     $5E
        jmp     LD776
; ----------------------------------------------------------------------------
LD745:  lda     $11
        cmp     #$66
        bne     LD71F
        lda     $12
        sbc     #$19
        bne     LD71F
        sta     $11
        tay
        lda     #$80
        sta     $12
LD758:  ldx     #$0A
LD75A:  lda     $E081,x
        and     #$3F
        sta     ($11),y
        iny
        bne     LD766
        inc     $12
LD766:  dex
        bne     LD75A
        dec     $46
        bne     LD758
        rts
; ----------------------------------------------------------------------------
LD76E:  jsr     LD8A5
        bcc     LD7AF
LD773:  jsr     LD998
LD776:  bne     LD77B
        jmp     LDB08
; ----------------------------------------------------------------------------
LD77B:  ldx     $6D
        stx     $53
        ldx     #$66
        lda     $66
LD783:  tay
        beq     LD72B
        sec
        sbc     $5E
        beq     LD7AF
        bcc     LD79F
        sty     $5E
        ldy     $6B
        sty     $63
        eor     #$FF
        adc     #$00
        ldy     #$00
        sty     $53
        ldx     #$5E
        bne     LD7A3
LD79F:  ldy     #$00
        sty     $6D
LD7A3:  cmp     #$F9
        bmi     LD76E
        tay
        lda     $6D
        lsr     $01,x
        jsr     LD8BC
LD7AF:  bit     $6C
        bpl     LD80A
        ldy     #$5E
        cpx     #$66
        beq     LD7BB
        ldy     #$66
LD7BB:  sec
        eor     #$FF
        adc     $53
        sta     $6D
        lda     $04,y
        sbc     $04,x
        sta     $62
        lda     $03,y
        sbc     $03,x
        sta     $61
        lda     $02,y
        sbc     $02,x
        sta     $60
        lda     $01,y
        sbc     $01,x
        sta     $5F
LD7DE:  bcs     LD7E3
        jsr     LD853
LD7E3:  ldy     #$00
        tya
        clc
LD7E7:  ldx     $5F
        bne     LD835
        ldx     $60
        stx     $5F
        ldx     $61
        stx     $60
        ldx     $62
        stx     $61
        ldx     $6D
        stx     $62
        sty     $6D
        adc     #$08
        cmp     #$20
        bne     LD7E7
LD803:  lda     #$00
LD805:  sta     $5E
LD807:  sta     $63
        rts
; ----------------------------------------------------------------------------
LD80A:  adc     $53
        sta     $6D
        lda     $62
        adc     $6A
        sta     $62
        lda     $61
        adc     $69
        sta     $61
        lda     $60
        adc     $68
        sta     $60
        lda     $5F
        adc     $67
        sta     $5F
        jmp     LD842
; ----------------------------------------------------------------------------
LD829:  adc     #$01
        asl     $6D
        rol     $62
        rol     $61
        rol     $60
        rol     $5F
LD835:  bpl     LD829
        sec
        sbc     $5E
        bcs     LD803
        eor     #$FF
        adc     #$01
        sta     $5E
LD842:  bcc     LD852
LD844:  inc     $5E
        beq     LD88A
        ror     $5F
        ror     $60
        ror     $61
        ror     $62
        ror     $6D
LD852:  rts
; ----------------------------------------------------------------------------
LD853:  lda     $63
        eor     #$FF
        sta     $63
LD859:  lda     $5F
        eor     #$FF
        sta     $5F
        lda     $60
        eor     #$FF
        sta     $60
        lda     $61
        eor     #$FF
        sta     $61
        lda     $62
        eor     #$FF
        sta     $62
        lda     $6D
        eor     #$FF
        sta     $6D
        inc     $6D
        bne     LD889
LD87B:  inc     $62
        bne     LD889
        inc     $61
        bne     LD889
        inc     $60
        bne     LD889
        inc     $5F
LD889:  rts
; ----------------------------------------------------------------------------
LD88A:  ldx     #$45
        jmp     ERROR
; ----------------------------------------------------------------------------
LD88F:  ldx     #$22
LD891:  ldy     $04,x
        sty     $6D
        ldy     $03,x
        sty     $04,x
        ldy     $02,x
        sty     $03,x
        ldy     $01,x
        sty     $02,x
        ldy     $65
        sty     $01,x
LD8A5:  adc     #$08
        bmi     LD891
        beq     LD891
        sbc     #$08
        tay
        lda     $6D
        bcs     LD8C6
LD8B2:  asl     $01,x
        bcc     LD8B8
        inc     $01,x
LD8B8:  ror     $01,x
        ror     $01,x
LD8BC:  ror     $02,x
        ror     $03,x
        ror     $04,x
        ror     
        iny
        bne     LD8B2
LD8C6:  clc
        rts
; ----------------------------------------------------------------------------
        sta     ($00,x)
        brk
        brk
        brk
        !by   $03
        !by   $7F
        lsr     LCB56,x
        adc     $1380,y
        !by   $9B
        !by   $0B
        !by   $64
        !by   $80
        ror     $38,x
        !by   $93
        asl     $82,x
        sec
        tax
        !by   $3B
        jsr     L3580
        !by   $04
        !by   $F3
        !by   $34
        sta     ($35,x)
        !by   $04
        !by   $F3
        !by   $34
        !by   $80
        !by   $80
        brk
        brk
        brk
        !by   $80
        and     ($72),y
        !by   $17
        sed
LD8F6:  jsr     LDB37
        beq     LD8FD
        bpl     LD900
LD8FD:  jmp     LD123
; ----------------------------------------------------------------------------
LD900:  lda     $5E
        sbc     #$7F
        pha
        lda     #$80
        sta     $5E
        lda     #$E2
        ldy     #$D8
        jsr     LD773
        lda     #$E7
        ldy     #$D8
        jsr     LDA1B
        lda     #$C8
        ldy     #$D8
        jsr     LD733
        lda     #$CD
        ldy     #$D8
        jsr     LDF2D
        lda     #$EC
        ldy     #$D8
        jsr     LD773
        pla
        jsr     LDC8A
        lda     #$F1
        ldy     #$D8
LD934:  jsr     LD998
        bne     LD93C
        jmp     LD997
; ----------------------------------------------------------------------------
LD93C:  jsr     LD9C3
        lda     #$00
        sta     $23
        sta     $24
        sta     $25
        sta     $26
        lda     $6D
        jsr     LD965
        lda     $62
        jsr     LD965
        lda     $61
        jsr     LD965
        lda     $60
        jsr     LD965
        lda     $5F
        jsr     LD96A
        jmp     LDA9B
; ----------------------------------------------------------------------------
LD965:  bne     LD96A
        jmp     LD88F
; ----------------------------------------------------------------------------
LD96A:  lsr     
        ora     #$80
LD96D:  tay
        bcc     LD989
        clc
        lda     $26
        adc     $6A
        sta     $26
        lda     $25
        adc     $69
        sta     $25
        lda     $24
        adc     $68
        sta     $24
        lda     $23
        adc     $67
        sta     $23
LD989:  ror     $23
        ror     $24
        ror     $25
        ror     $26
        ror     $6D
        tya
        lsr
        bne     LD96D
LD997:  rts
; ----------------------------------------------------------------------------
LD998:  sta     L001F
        sty     $20
        ldy     #$04
        lda     (L001F),y
        sta     $6A
        dey
        lda     (L001F),y
        sta     $69
        dey
        lda     (L001F),y
        sta     $68
        dey
        lda     (L001F),y
        sta     $6B
        eor     $63
        sta     $6C
        lda     $6B
        ora     #$80
        sta     $67
        dey
        lda     (L001F),y
        sta     $66
        lda     $5E
        rts
; ----------------------------------------------------------------------------
LD9C3:  lda     $66
LD9C5:  beq     LD9E6
        clc
        adc     $5E
        bcc     LD9D0
        bmi     LD9EB
        clc
        !by   $2C
LD9D0:  bpl     LD9E6
        adc     #$80
        sta     $5E
        bne     LD9DB
        jmp     LD807
; ----------------------------------------------------------------------------
LD9DB:  lda     $6C
        sta     $63
        rts
; ----------------------------------------------------------------------------
LD9E0:  lda     $63
        eor     #$FF
        bmi     LD9EB
LD9E6:  pla
        pla
        jmp     LD803
; ----------------------------------------------------------------------------
LD9EB:  jmp     LD88A
; ----------------------------------------------------------------------------
LD9EE:  jsr     LDB18
        tax
        beq     LDA04
        clc
        adc     #$02
        bcs     LD9EB
LD9F9:  ldx     #$00
        stx     $6C
        jsr     LD783
        inc     $5E
        beq     LD9EB
LDA04:  rts
; ----------------------------------------------------------------------------
        sty     $20
        brk
        brk
        brk
LDA0A:  jsr     LDB18
        lda     #$05
        ldy     #$DA
        ldx     #$00
LDA13:  stx     $6C
        jsr     LDAAE
        jmp     LDA1E
; ----------------------------------------------------------------------------
LDA1B:  jsr     LD998
LDA1E:  beq     LDA96
        jsr     LDB27
        lda     #$00
        sec
        sbc     $5E
        sta     $5E
        jsr     LD9C3
        inc     $5E
        beq     LD9EB
        ldx     #$FC
        lda     #$01
LDA35:  ldy     $67
        cpy     $5F
        bne     LDA4B
        ldy     $68
        cpy     $60
        bne     LDA4B
        ldy     $69
        cpy     $61
        bne     LDA4B
        ldy     $6A
        cpy     $62
LDA4B:  php
        rol
        bcc     LDA58
        inx
        sta     $26,x
        beq     LDA86
        bpl     LDA8A
        lda     #$01
LDA58:  plp
        bcs     LDA69
LDA5B:  asl     $6A
        rol     $69
        rol     $68
        rol     $67
        bcs     LDA4B
        bmi     LDA35
        bpl     LDA4B
LDA69:  tay
        lda     $6A
        sbc     $62
        sta     $6A
        lda     $69
        sbc     $61
        sta     $69
        lda     $68
        sbc     $60
        sta     $68
        lda     $67
        sbc     $5F
        sta     $67
        tya
        jmp     LDA5B
; ----------------------------------------------------------------------------
LDA86:  lda     #$40
        bne     LDA58
LDA8A:  asl     
        asl
        asl
        asl
        asl
        asl
        sta     $6D
        plp
        jmp     LDA9B
; ----------------------------------------------------------------------------
LDA96:  ldx     #$85
        jmp     ERROR
; ----------------------------------------------------------------------------
LDA9B:  lda     $23
        sta     $5F
        lda     $24
        sta     $60
        lda     $25
        sta     $61
        lda     $26
        sta     $62
        jmp     LD7E3
; ----------------------------------------------------------------------------
LDAAE:  sta     L001F
        sty     $20
        ldy     #$04
        lda     (L001F),y
        sta     $62
        dey
        lda     (L001F),y
        sta     $61
        dey
        lda     (L001F),y
        sta     $60
        dey
        lda     (L001F),y
        sta     $63
        ora     #$80
        sta     $5F
        dey
        lda     (L001F),y
        sta     $5E
        sty     $6D
        rts
; ----------------------------------------------------------------------------
LDAD3:  ldx     #$59
        !by   $2C
LDAD6:  ldx     #$54
        ldy     #$00
        beq     LDAE0
LDADC:  ldx     $46
        ldy     $47
LDAE0:  jsr     LDB27
        stx     L001F
        sty     $20
        ldy     #$04
        lda     $62
        sta     (L001F),y
        dey
        lda     $61
        sta     (L001F),y
        dey
        lda     $60
        sta     (L001F),y
        dey
        lda     $63
        ora     #$7F
        and     $5F
        sta     (L001F),y
        dey
        lda     $5E
        sta     (L001F),y
        sty     $6D
        rts
; ----------------------------------------------------------------------------
LDB08:  lda     $6B
LDB0A:  sta     $63
        ldx     #$05
LDB0E:  lda     $65,x
        sta     $5D,x
        dex
        bne     LDB0E
        stx     $6D
        rts
; ----------------------------------------------------------------------------
LDB18:  jsr     LDB27
LDB1B:  ldx     #$06
LDB1D:  lda     $5D,x
        sta     $65,x
        dex
        bne     LDB1D
        stx     $6D
LDB26:  rts
; ----------------------------------------------------------------------------
LDB27:  lda     $5E
        beq     LDB26
        asl     $6D
        bcc     LDB26
LDB2F:  jsr     LD87B
        bne     LDB26
        jmp     LD844
; ----------------------------------------------------------------------------
LDB37:  lda     $5E
        beq     LDB44
LDB3B:  lda     $63
LDB3D:  rol     
        lda     #$FF
        bcs     LDB44
        lda     #$01
LDB44:  rts
; ----------------------------------------------------------------------------
;BASIC: SGN
        jsr     LDB37
LDB48:  sta     $5F
        lda     #$00
        sta     $60
        ldx     #$88
LDB50:  lda     $5F
        eor     #$FF
        rol
LDB55:  lda     #$00
        sta     $62
        sta     $61
LDB5B:  stx     $5E
        sta     $6D
        sta     $63
        jmp     LD7DE
; ----------------------------------------------------------------------------
;BASIC: ABS
        lsr     $63
        rts
; ----------------------------------------------------------------------------
LDB67:  sta     $21
LDB69:  sty     $22
        ldy     #$00
        lda     ($21),y
        iny
        tax
        beq     LDB37
        lda     ($21),y
        eor     $63
        bmi     LDB3B
        cpx     $5E
        bne     LDB9E
        lda     ($21),y
        ora     #$80
        cmp     $5F
        bne     LDB9E
        iny
        lda     ($21),y
        cmp     $60
        bne     LDB9E
        iny
        lda     ($21),y
        cmp     $61
        bne     LDB9E
        iny
        lda     #$7F
        cmp     $6D
        lda     ($21),y
        sbc     $62
        beq     LDBC6
LDB9E:  lda     $63
        bcc     LDBA4
        eor     #$FF
LDBA4:  jmp     LDB3D
; ----------------------------------------------------------------------------
LDBA7:  lda     $5E
        beq     LDBF5
        sec
        sbc     #$A0
        bit     $63
        bpl     LDBBB
        tax
        lda     #$FF
        sta     $65
        jsr     LD859
        txa
LDBBB:  ldx     #$5E
        cmp     #$F9
        bpl     LDBC7
        jsr     LD8A5
        sty     $65
LDBC6:  rts
; ----------------------------------------------------------------------------
LDBC7:  tay
        lda     $63
        and     #$80
        lsr     $5F
        ora     $5F
        sta     $5F
        jsr     LD8BC
        sty     $65
        rts
; ----------------------------------------------------------------------------
;BASIC: INT
LDBD8:  lda     $5E
        cmp     #$A0
        bcs     LDBFE
        jsr     LDBA7
        sty     $6D
        lda     $63
        sty     $63
        eor     #$80
        rol
        lda     #$A0
        sta     $5E
        lda     $62
        sta     $03
        jmp     LD7DE
; ----------------------------------------------------------------------------
LDBF5:  sta     $5F
        sta     $60
        sta     $61
        sta     $62
        tay
LDBFE:  rts
; ----------------------------------------------------------------------------
LDBFF:  ldy     #$00
        ldx     #$0A
LDC03:  sty     $5A,x
        dex
        bpl     LDC03
        bcc     LDC19
        cmp     #$2D
        bne     LDC12
        stx     $64
        beq     LDC16
LDC12:  cmp     #$2B
        bne     LDC1B
LDC16:  jsr     CHRGET
LDC19:  bcc     LDC76
LDC1B:  cmp     #$2E
        beq     LDC4D
        cmp     #$45
        bne     LDC53
        jsr     CHRGET
        bcc     LDC3F
        cmp     #$AB
        beq     LDC3A
        cmp     #$2D
        beq     LDC3A
        cmp     #$AA
        beq     LDC3C
        cmp     #$2B
        beq     LDC3C
        bne     LDC41
LDC3A:  ror     $5D
LDC3C:  jsr     CHRGET
LDC3F:  bcc     LDC9D
LDC41:  bit     $5D
        bpl     LDC53
        lda     #$00
        sec
        sbc     $5B
        jmp     LDC55
; ----------------------------------------------------------------------------
LDC4D:  ror     $5C
        bit     $5C
        bvc     LDC16
LDC53:  lda     $5B
LDC55:  sec
        sbc     $5A
        sta     $5B
        beq     LDC6E
        bpl     LDC67
LDC5E:  jsr     LDA0A
        inc     $5B
        bne     LDC5E
        beq     LDC6E
LDC67:  jsr     LD9EE
        dec     $5B
        bne     LDC67
LDC6E:  lda     $64
        bmi     LDC73
        rts
; ----------------------------------------------------------------------------
LDC73:  jmp     LDEA1
; ----------------------------------------------------------------------------
LDC76:  pha
        bit     $5C
        bpl     LDC7D
        inc     $5A
LDC7D:  jsr     LD9EE
        pla
        sec
        sbc     #$30
        jsr     LDC8A
        jmp     LDC16
; ----------------------------------------------------------------------------
LDC8A:  pha
        jsr     LDB18
        pla
        jsr     LDB48
        lda     $6B
        eor     $63
        sta     $6C
        ldx     $5E
        jmp     LD776
; ----------------------------------------------------------------------------
LDC9D:  lda     $5B
        cmp     #$0A
        bcc     LDCAC
        lda     #$64
        bit     $5D
        bmi     LDCBA
        jmp     LD88A
; ----------------------------------------------------------------------------
LDCAC:  asl     
        asl
        clc
        adc     $5B
        asl
        clc
        ldy     #$00
        adc     ($77),y
        sec
        sbc     #$30
LDCBA:  sta     $5B
        jmp     LDC3C
; ----------------------------------------------------------------------------
        !by   $9B
        rol     $1FBC,x
        sbc     $6E9E,x
        !by   $6B
        !by   $27
        sbc     $6E9E,x
        !by   $6B
        plp
        brk
LDCCE:  lda     #$92
        ldy     #$C2
        jsr     LDCE6
        lda     $37
        ldx     $36
LDCD9:  sta     $5F
        stx     $60
        ldx     #$90
        sec
        jsr     LDB55
        jsr     LDCE9
LDCE6:  jmp     LCA1C
; ----------------------------------------------------------------------------
LDCE9:  ldy     #$01
LDCEB:  lda     #$20
        bit     $63
        bpl     LDCF3
        lda     #$2D
LDCF3:  sta     $FF,y
        sta     $63
        sty     $6E
        iny
        lda     #$30
        ldx     $5E
        bne     LDD04
        jmp     LDE10
; ----------------------------------------------------------------------------
LDD04:  lda     #$00
        cpx     #$80
        beq     LDD0C
        bcs     LDD15
LDD0C:  lda     #$C9
        ldy     #$DC
        jsr     LD934
        lda     #$F7
LDD15:  sta     $5A
LDD17:  lda     #$C4
        ldy     #$DC
        jsr     LDB67
        beq     LDD3E
        bpl     LDD34
LDD22:  lda     #$BF
        ldy     #$DC
        jsr     LDB67
        beq     LDD2D
        bpl     LDD3B
LDD2D:  jsr     LD9EE
        dec     $5A
        bne     LDD22
LDD34:  jsr     LDA0A
        inc     $5A
        bne     LDD17
LDD3B:  jsr     LD72C
LDD3E:  jsr     LDBA7
        ldx     #$01
        lda     $5A
        clc
        adc     #$0A
        bmi     LDD53
        cmp     #$0B
        bcs     LDD54
        adc     #$FF
        tax
        lda     #$02
LDD53:  sec
LDD54:  sbc     #$02
        sta     $5B
        stx     $5A
        txa
        beq     LDD5F
        bpl     LDD72
LDD5F:  ldy     $6E
        lda     #$2E
        iny
        sta     $FF,y
        txa
        beq     LDD70
        lda     #$30
        iny
        sta     $FF,y
LDD70:  sty     $6E
LDD72:  ldy     #$00
LDD74:  ldx     #$80
LDD76:  lda     $62
        clc
        adc     LDE25,y
        sta     $62
        lda     $61
        adc     LDE24,y
        sta     $61
        lda     $60
        adc     LDE23,y
        sta     $60
        lda     $5F
        adc     LDE22,y
        sta     $5F
        inx
        bcs     LDD9A
        bpl     LDD76
        bmi     LDD9C
LDD9A:  bmi     LDD76
LDD9C:  txa
        bcc     LDDA3
        eor     #$FF
        adc     #$0A
LDDA3:  adc     #$2F
        iny
        iny
        iny
        iny
        sty     $44
        ldy     $6E
        iny
        tax
        and     #$7F
        sta     $FF,y
        dec     $5A
        bne     LDDBE
        lda     #$2E
        iny
        sta     $FF,y
LDDBE:  sty     $6E
        ldy     $44
        txa
        eor     #$FF
        and     #$80
        tax
        cpy     #$24
        beq     LDDD0
        cpy     #$3C
        bne     LDD76
LDDD0:  ldy     $6E
LDDD2:  lda     $FF,y
        dey
        cmp     #$30
        beq     LDDD2
        cmp     #$2E
        beq     LDDDF
        iny
LDDDF:  lda     #$2B
        ldx     $5B
        beq     LDE13
        bpl     LDDEF
        lda     #$00
        sec
LDDEA:  sbc     $5B
        tax
        lda     #$2D
LDDEF:  sta     $0101,y
        lda     #$45
        sta     $0100,y
        txa
        ldx     #$2F
        sec
LDDFB:  inx
        sbc     #$0A
        bcs     LDDFB
        adc     #$3A
        sta     $0103,y
        txa
        sta     $0102,y
        lda     #$00
        sta     $0104,y
        beq     LDE18
LDE10:  sta     $FF,y
LDE13:  lda     #$00
        sta     $0100,y
LDE18:  lda     #$00
        ldy     #$01
        rts
; ----------------------------------------------------------------------------
;TABLE FOR EXP-CALC.
        !by  $80,$00,$00,$00,$00
LDE22:  !by  $FA
LDE23:  !by  $0a     
LDE24:  !by  $1F
LDE25:  !by  $00,$00,$98,$96,$80,$ff
        !by  $f0,$bd,$c0,$00,$01,$86   
        !by  $a0,$ff,$ff,$d8,$f0,$00  
LDE37:  !by  $00,$03,$e8,$ff,$ff,$ff 
        !by  $9C,$00,$00,$00,$0a,$ff
        !by  $FF,$FF,$FF,$FF,$df,$0a
        !by  $80,$00,$03,$4b,$c0,$ff
        !by  $FF,$73,$60,$00,$00,$0e
        !by  $10,$ff,$FF,$FD,$A8,$00
        !by  $00,$00,$3C
;BASIC: SQR
        jsr     LDB18
        lda     #$1D
        ldy     #$DE
        jsr     LDAAE
        beq     LDEDA
        lda     $66
        bne     LDE71
        jmp     LD805
; ----------------------------------------------------------------------------
LDE71:  ldx     #$4B
        ldy     #$00
        jsr     LDAE0
        lda     $6B
        bpl     LDE8B
        jsr     LDBD8
        lda     #$4B
        ldy     #$00
        jsr     LDB67
        bne     LDE8B
        tya
        ldy     $03
LDE8B:  jsr     LDB0A
        tya
        pha
        jsr     LD8F6
        lda     #$4B
        ldy     #$00
        jsr     LD934
        jsr     LDEDA
        pla
        lsr
        bcc     LDEAB
LDEA1:  lda     $5E
        beq     LDEAB
        lda     $63
        eor     #$FF
        sta     $63
LDEAB:  rts
; ----------------------------------------------------------------------------

       !by $81,$38,$aa,$3b,$29,$07,$71,$34 
       !by $58,$3e,$56,$74,$16,$7e,$b3,$1b 
       !by $77,$2f,$ee,$e3,$85,$7a,$1d,$84
       !by $1c,$2a,$7c,$63,$59,$58,$0a,$7e 
       !by $75,$fd,$e7,$c6,$80,$31,$72,$18 
       !by $10,$81,$00,$00,$00,$00 

LDEDA:  lda     #$AC
        ldy     #$DE
        jsr     LD934
        lda     $6D
        adc     #$50
        bcc     LDEEA
        jsr     LDB2F
LDEEA:  sta     $53
        jsr     LDB1B
        lda     $5E
        cmp     #$88
        bcc     LDEF8
LDEF5:  jsr     LD9E0
LDEF8:  jsr     LDBD8
        lda     $03
        clc
        adc     #$81
        beq     LDEF5
        sec
        sbc     #$01
        pha
        ldx     #$05
LDF08:  lda     $66,x
        ldy     $5E,x
        sta     $5E,x
        sty     $66,x
        dex
        bpl     LDF08
        lda     $53
        sta     $6D
        jsr     LD736
        jsr     LDEA1
        lda     #$B1
        ldy     #$DE
        jsr     LDF43
        lda     #$00
        sta     $6C
        pla
        jsr     LD9C5
        rts
; ----------------------------------------------------------------------------
LDF2D:  sta     $6E
        sty     $6F
        jsr     LDAD6
        lda     #$54
        jsr     LD934
        jsr     LDF47
        lda     #$54
        ldy     #$00
        jmp     LD934
; ----------------------------------------------------------------------------
LDF43:  sta     $6E
        sty     $6F
LDF47:  jsr     LDAD3
        lda     ($6E),y
        sta     $64
        ldy     $6E
        iny
        tya
        bne     LDF56
        inc     $6F
LDF56:  sta     $6E
        ldy     $6F
LDF5A:  jsr     LD934
        lda     $6E
        ldy     $6F
        clc
        adc     #$05
        bcc     LDF67
        iny
LDF67:  sta     $6E
        sty     $6F
        jsr     LD773
        lda     #$59
        ldy     #$00
        dec     $64
        bne     LDF5A
        rts
; ----------------------------------------------------------------------------
        tya
        and     $44,x
        !by   $7A
        pla
        plp
        lda     ($46),y
        jsr     LDB37
        bmi     LDFB2
        bne     LDF9D
        lda     $E844
        sta     $5F
        lda     $E848
        sta     $60
        lda     $E845
        sta     $61
        lda     $E849
        sta     $62
        jmp     LDFC2
; ----------------------------------------------------------------------------
LDF9D:  lda     #$88
        ldy     #$00
        jsr     LDAAE
        lda     #$77
        ldy     #$DF
        jsr     LD934
        lda     #$7B
        ldy     #$DF
        jsr     LD773
LDFB2:  ldx     $62
        lda     $5F
        sta     $62
        stx     $5F
        ldx     $60
        lda     $61
        sta     $60
        stx     $61
LDFC2:  lda     #$00
        sta     $63
        lda     $5E
        sta     $6D
        lda     #$80
        sta     $5E
        jsr     LD7E3
        ldx     #$88
        ldy     #$00
        jmp     LDAE0
; ----------------------------------------------------------------------------
;BASIC: COS
        lda     #$54
        ldy     #$E0
        jsr     LD773
;BASIC: SIN
        jsr     LDB18
        lda     #$59
        ldy     #$E0
        ldx     $6B
        jsr     LDA13
        jsr     LDB18
        jsr     LDBD8
        lda     #$00
        sta     $6C
        jsr     LD736
        lda     #$5E
        ldy     #$E0
        jsr     LD733
        !by   $A5
