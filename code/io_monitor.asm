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
        jsr disp_name

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

disp_name
        ldx #0
-       ldy kernel_entries, x
        beq ++
        eor kernel_entries, x
        beq +
        eor kernel_entries, x

        inx
        inx
        inx
        inx
        inx
        inx
        inx
        inx
        inx

        bne -
        beq ++

+       lda kernel_entries+7, x
        sta $fb
        lda kernel_entries+8, x
        sta $fc
        ldy #0
-       lda ($fb),Y
        beq ++
        jsr chrout
        iny
        bne -
++      rts

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
        inx
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
        !byte $A5, 00, 00, 00, 00, 00, 00, <n_acptr, >n_acptr
        !byte $C6, 00, 00, 00, 00, 00, 00, <n_chkin, >n_chkin
        !byte $C9, 00, 00, 00, 00, 00, 00, <n_chkout, >n_chkout
        !byte $CF, 00, 00, 00, 00, 00, 00, <n_chrin, >n_chrin
        !byte $D2, 00, 00, 00, 00, 00, 00, <n_chrout, >n_chrout
        !byte $A8, 00, 00, 00, 00, 00, 00, <n_ciout, >n_ciout
        !byte $E7, 00, 00, 00, 00, 00, 00, <n_clall, >n_clall
        !byte $C3, 00, 00, 00, 00, 00, 00, <n_close, >n_close
        !byte $CC, 00, 00, 00, 00, 00, 00, <n_clrchn, >n_clrchn
        !byte $E4, 00, 00, 00, 00, 00, 00, <n_getin, >n_getin
        !byte $B1, 00, 00, 00, 00, 00, 00, <n_listen, >n_listen
        !byte $D5, 00, 00, 00, 00, 00, 00, <n_load, >n_load
        !byte $C0, 00, 00, 00, 00, 00, 00, <n_open, >n_open
        !byte $B7, 00, 00, 00, 00, 00, 00, <n_readst, >n_readst
        !byte $D8, 00, 00, 00, 00, 00, 00, <n_save, >n_save
        !byte $93, 00, 00, 00, 00, 00, 00, <n_second, >n_second
        !byte $BA, 00, 00, 00, 00, 00, 00, <n_setlfs, >n_setlfs
        !byte $BD, 00, 00, 00, 00, 00, 00, <n_setnam, >n_setnam
        !byte $B4, 00, 00, 00, 00, 00, 00, <n_talk, >n_talk
        !byte $96, 00, 00, 00, 00, 00, 00, <n_tksa, >n_tksa
        !byte $AE, 00, 00, 00, 00, 00, 00, <n_unlsn, >n_unlsn
        !byte $AB, 00, 00, 00, 00, 00, 00, <n_untlk, >n_untlk
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

n_acptr !text "ACPTR"
        !byte 0
n_chkin !text "CHKIN"
        !byte 0
n_chkout !text "CHKOUT"
        !byte 0
n_chrin !text "CHRIN"
        !byte 0
n_chrout !text "CHROUT"
        !byte 0
n_ciout !text "CIOUT"
        !byte 0
n_clall !text "CLALL"
        !byte 0
n_close !text "CLOSE"
        !byte 0
n_clrchn !text "CLRCHN"
        !byte 0
n_getin !text "GETIN"
        !byte 0
n_listen !text "LISTEN"
        !byte 0
n_load  !text "LOAD"
        !byte 0
n_open  !text "OPEN"
        !byte 0
n_readst !text "READST"
        !byte 0
n_save  !text "SAVE"
        !byte 0
n_second !text "SECOND"
        !byte 0
n_setlfs !text "SETLFS"
        !byte 0
n_setnam !text "SETNAM"
        !byte 0
n_talk  !text "TALK"
        !byte 0
n_tksa  !text "TKSA"
        !byte 0
n_unlsn !text "UNLSN"
        !byte 0
n_untlk !text "UNTALK"
        !byte 0

copyright 
        !byte 14 ; upper/lowercase character sets
        !byte 147 ; clear screen
        !text "c64 io mONITOR 1.10"
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