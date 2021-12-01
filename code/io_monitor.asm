; io_monitor.asm
;
; by David R. Van Wagner
; dave@davevw.com
; techwithdave.davevw.com
;
; MIT LICENSE
;
;
; hooks C64 kernel entry points via modifying entry points after copying ROM to RAM, running Kernel/BASIC in RAM
;
; SYS 49152: REM initialize hooks and then running system from RAM (note can call again to clear out counts)
; SYS 49155: REM display hit counts

start=$C000 ; machine language org
chrout=$f1ca ;$ffd2

; "hires ml"
* = start
        jmp init_hooks
        jmp display_counts

init_hooks
        jsr bank_norm

        jsr disp_copyright

        jsr reset_counts

        ldy #$00
        lda #$E0
        sty $FB
        sta $FC

-       lda ($FB), y
        sta ($FB), Y
        iny
        bne -
        inc $FC
        bne -

        lda #$A0
        sta $FC

-       lda ($FB), y
        sta ($FB), Y
        iny
        bne -
        inc $FC
        ldx $FC
        cpx #$C0
        bne -

        jsr hook_entries

        lda #$05 ; BASIC/KERNEL ROMs replaced with RAM, leave I/O as is
        jsr bank_select
        rts

reset_counts
        ldx #0
        txa
-       sta call_counts, x
        inx
        bne -
        rts

bank_norm
        lda $01
        ora #$07
        sta $01
        rts

bank_ram
        lda $01
        and #$f8
        sta $01
        rts

bank_select
        sta $FB
        lda $01
        and #$f8
        ora $FB
        sta $01
        rts

display_counts
        ldx #0 ; initialize index

-       lda call_counts, x
        beq + ; skip if zero

        txa
        pha ; save index to stack

        ; display kernel address
        lda #$ff
        jsr disp_hex
        pla ; restore index
        pha ; save index again
        jsr disp_hex

        lda #$20 ; space
        jsr chrout

        pla ; restore index
        pha ; save index again
        tax
        lda call_counts, x ; retrieve non-zero count again for display
        jsr disp_hex

        lda #$0D ; carriage return
        jsr chrout

        pla ; restore index
        tax 
+       inx
        bne -

        rts

disp_hex
        pha
        lsr
        lsr
        lsr
        lsr
        jsr disp_digit
        pla
        and #$0f
        ; fall thru disp_digit

disp_digit
        cmp #16
        bcs ++
        cmp #10
        bcs +
        adc #'0'
        jmp chrout
+       sbc #$0A
        adc #$40
        jmp chrout
++      rts        

disp_copyright
        ldy #0
-       lda copyright,y
        beq +
        jsr chrout
        iny
        bne -
+       rts        

hook_entries
        sei
        lda #$ff
        sta $FC

        ldy #0
        ldx #0
-       lda kernel_entries, x
        beq +

        sta $FB
        lda $FC
        jsr disp_hex
        lda $FB
        jsr disp_hex
        lda #$20
        jsr chrout
;        lda ($FB),y
;        jsr disp_hex
;        lda #$20
;        jsr chrout
;        lda #$0D
;        jsr chrout

        lda #$20
        inx
        sta kernel_entries, x

        lda #<hook        
        inx
        sta kernel_entries, x

        lda #>hook      
        inx
        sta kernel_entries, x

        lda ($FB),y
        inx
        sta kernel_entries, x
        lda #$4C
        sta ($FB),y

        iny
        lda ($FB),y
        inx
        sta kernel_entries, x
        txa
        sec
        sbc #$04
        clc
        adc #<kernel_entries
        php
        sta ($FB),y

        iny
        lda ($FB),y
        inx
        sta kernel_entries, x
        clc
        lda #>kernel_entries
        plp
        adc #$00
        sta ($FB),y

        inx
        ldy #$00
        beq -

+       cli

        lda #$0D
        jsr chrout

        rts

hook
        php
        sei
        pha
        txa
        pha
        tya
        pha

        tsx
        lda $105, x
        sec
        sbc #$03
        sta $fb
        lda $106, x
        sbc #$00
        sta $fc
        ldy #0
        lda ($fb),y
        tax
        inc call_counts, x
        bne +
        dec call_counts, x ; return to max value
+       inc $d020

        pla
        tay
        pla
        tax
        pla
        cli
        plp
        rts

kernel_entries
        !byte $CF, 00, 00, 00, 00, 00, 00
        !byte $D2, 00, 00, 00, 00, 00, 00
        !byte $A5, 00, 00, 00, 00, 00, 00
        !byte $C6, 00, 00, 00, 00, 00, 00
        !byte $C9, 00, 00, 00, 00, 00, 00
        !byte $A8, 00, 00, 00, 00, 00, 00
        !byte $E7, 00, 00, 00, 00, 00, 00
        !byte $C3, 00, 00, 00, 00, 00, 00
        !byte $CC, 00, 00, 00, 00, 00, 00
        !byte $E4, 00, 00, 00, 00, 00, 00
        !byte $B1, 00, 00, 00, 00, 00, 00
        !byte $D5, 00, 00, 00, 00, 00, 00
        !byte $C0, 00, 00, 00, 00, 00, 00
        !byte $B7, 00, 00, 00, 00, 00, 00
        !byte $D8, 00, 00, 00, 00, 00, 00
        !byte $93, 00, 00, 00, 00, 00, 00
        !byte $BA, 00, 00, 00, 00, 00, 00
        !byte $BD, 00, 00, 00, 00, 00, 00
        !byte $B4, 00, 00, 00, 00, 00, 00
        !byte $96, 00, 00, 00, 00, 00, 00
        !byte $AE, 00, 00, 00, 00, 00, 00
        !byte $AB, 00, 00, 00, 00, 00, 00
        !byte 0

call_counts ; indexed by $FFXX low byte of entry point, not all bytes are used, but very simple container
        !byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        !byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        !byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        !byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        !byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        !byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        !byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        !byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        !byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        !byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        !byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        !byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        !byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        !byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        !byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        !byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

copyright 
        !byte 14 ; upper/lowercase character sets
        !byte 147 ; clear screen
        !text "c64 io mONITOR 1.0"
        !byte 13 ; carriage return
        !text "(c) 2021 BY dAVID r. vAN wAGNER"
        !byte 13
        !text "DAVE@DAVEVW.COM"
        !byte 13
        !text "TECHWITHDAVE.DAVEVW.COM"
        !byte 13
        !text "GITHUB.COM/DAVERVW"
        !byte 13
        !text "mit license"
        !byte 13
        !text "sys 49152 : rem hook kernel entries"
        !byte 13
        !text "sys 49155 : rem display counts"
        !byte 13
        !byte 13
        !byte 0 ; end of string