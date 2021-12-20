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
;   also sets log destination past BASIC program's RAM (55/56) if room before $A000
; SYS 49155: REM display hit counts
; SYS 49158: REM display log of hits

start=$C000 ; machine language org
chrout=$f1ca ;$ffd2

; "hires ml"
* = start
        jmp init_hooks
        jmp display_counts
        jmp display_log

init_hooks
        jsr bank_norm
        jsr disp_copyright
        jsr reset_counts

        ; copy KERNEL $E000-$FFFF to RAM
        ldy #$00
        lda #$E0
        sty $FB
        sta $FC
-       lda ($FB), y
        sta ($FB), Y
        iny
        bne -
        inc $FC
        bne - ; stop at $0000

        ; copy BASIC $A000-$BFFF to RAM
        ; because there is no banking mode where BASIC is ROM, KERNEL is RAM
        ; stuck copying both to RAM
        lda #$A0
        sta $FC
-       lda ($FB), y
        sta ($FB), Y
        iny
        bne -
        inc $FC
        ldx $FC
        cpx #$C0 ; stop at $C000
        bne -

        ; change KERNEL JUMP TABLE in RAM so we get control for those entries
        jsr hook_entries

        lda #$05 ; BASIC/KERNEL ROMs replaced with RAM, leave I/O as is
        jsr bank_select

        ; copy 55/56 to log_ptr, and compute space available (up to $A000) in log_rem
        lda 55 ; low byte end of BASIC RAM (variables) area
        sta log_ptr
        sec
        lda #$00
        sbc 55
        sta log_rem
        lda 56 ; high byte end of BASIC RAM (variables) area
        sta log_ptr+1
        lda #$A0
        sbc 56
        bcs + ; greater than or equal to, okay
        lda #0 ; less than, zero out log_rem
        sta log_rem
+       sta log_rem+1

        ; display $abcd log bytes free
        lda #'$'
        jsr chrout
        lda log_rem+1
        jsr disp_hex
        lda log_rem
        jsr disp_hex
        ldx #<bytes_free
        ldy #>bytes_free
        jsr disp_string

; clear log
        lda log_rem+1
        beq + ; corner case, don't support clearing a single partial page
        tax
        lda #0
        sta $fb
        lda log_ptr+1
        sta $fc
        ldy log_ptr
        lda #0
-       sta ($fb),y
        iny
        bne -
        inc $fc
        dex
        bne -

+       rts

reset_counts
        ldx #0
        txa
-       sta call_counts, x
        inx
        bne -
        rts

bank_norm ; bank 7
        lda $01
        ora #$07
        sta $01
        rts

bank_ram ; bank 0
        lda $01
        and #$f8
        sta $01
        rts

bank_select ; bank from .A register
        sta $FB
        lda $01
        and #$f8
        ora $FB
        sta $01
        rts

; bank  A000  D000  D800  E000
; 0     RAM   RAM   RAM   RAM
; 1     RAM   CHARG CHARG RAM
; 2     RAM   CHARG CHARG ROM
; 3     ROM   CHARG CHARG ROM
; 4     RAM   RAM   RAM   RAM
; 5     RAM   I/O   NYBLE RAM
; 6     RAM   I/O   NYBLE ROM
; 7     ROM   I/O   NYBLE ROM

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

display_log
        lda 55
        sta $fb
        lda 56
        sta $fc
        ldy #0
-       lda $fc
        cmp log_ptr+1
        bne +
        lda $fb
        cmp log_ptr
        beq ++
+       lda ($fb),y
        jsr chrout
        inc $fb
        bne -
        inc $fc
        bne -
++      rts

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
        ldx #<copyright
        ldy #>copyright
        jmp disp_string

get_name ; INPUT .A is low byte of kernel jump table address $FFXX
         ; OUTPUT .X is low byte of name address, .Y is high byte of name address
        ldx #0
-       ldy kernel_entries, x   ; retrieve low byte to check for end
        beq ++                  ; branch if end
        eor kernel_entries, x   ; compare
        beq +                   ; jump if found it
        eor kernel_entries, x   ; restore .A

        ; x+=10 skip size of non-matching entry record
        inx
        inx
        inx
        inx
        inx
        inx
        inx
        inx
        inx
        inx

        jmp -                   ; continue looking

+       lda kernel_entries+8, x ; high byte of pointer to name string
        tay
        lda kernel_entries+7, x ; low byte of pointer to name string
        tax

        rts

disp_name ; INPUT .A is low byte of kernel jump table address $FFXX
        jsr get_name ; get address in .X/.Y
disp_string
        stx $fb
        sty $fc

        ldy #0
-       lda ($fb),Y
        beq ++
        jsr chrout
        iny
        bne -
        inc $fc
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
        inx
        ldy #$00
        beq -

+       cli

        lda #$0D
        jsr chrout

        rts

hook
        php ; save flags and registers
        sei
        pha
        txa
        pha
        tya
        pha

        lda log_busy
        bne +
        inc log_busy
        
        jsr get_index
        jsr log_inputs
        jsr log_addr
        jsr hook_return

        lda #13
        jsr log_char

        dec log_busy

+       pla ; restore registers and flags
        tay
        pla
        tax
        pla
        cli
        plp
        rts

hook2
        php ; save flags and registers
        sei
        pha
        txa
        pha
        tya
        pha

        jsr get_index
        jsr log_outputs

        pla ; restore registers and flags
        tay
        pla
        tax
        pla
        cli
        plp
        rts

get_index
        tsx ; caller trail is on the stack, so transfer stack index to X for our use
        lda $107, x ; get the low byte of the return to caller, should be our code in kernel_entries
        sec ; prepare to subtract, no borrow yet
        sbc #$03 ; subtract 3 to compute a pointer of our record of the low byte of the kernel vector $FFXX 
        sta $fb ; save low byte in zero page pointer
        lda $108, x ; get high byte
        sbc #$00 ; account for subtraction borrow
        sta $fc ; save high byte in zero page pointer
        ldy #0 ; prep for dereferencing
        lda ($fb),y ; dereference pointer to get the $FFXX low byte
        tax ; going to use it as an index
        inc call_counts, x
        bne + ; branch not zero
        dec call_counts, x ; oops, on overflow to zero, return to max value
+       inc $d020 ; toggle border color showing something is happening
        rts
        
get_pushed_a
        tsx ; saved registers are on the stack, so transfer stack index to X for our use
        lda $108, x ; get the low byte of the return to caller, should be our code in kernel_entries
        rts

get_pushed_x
        tsx ; saved registers are on the stack, so transfer stack index to X for our use
        lda $107, x ; get the low byte of the return to caller, should be our code in kernel_entries
        rts

get_pushed_y
        tsx ; saved registers are on the stack, so transfer stack index to X for our use
        lda $106, x ; get the low byte of the return to caller, should be our code in kernel_entries
        rts

get_flags ; input .X index
        ldy #0
-       txa
        cmp kernel_entries, y
        beq +
        tya
        clc
        adc #10
        tay
        bcc -
        bcs ++
+       lda kernel_entries+9,Y
++      rts

log_inputs
        txa ; save .X to stack (low address of hooked kernel entry)
        pha

        jsr get_flags
        and #$01
        beq +
        jsr get_pushed_a
        tax
        lda #'A'
        jsr log_register

        pla ; restore .X from stack and re-save
        tax
        pha
        cpx #$D2
        bne +
        jsr get_pushed_a
        cmp #$7E
        bcs +
        cmp #$1F
        bcc +
        pha
        lda #' '
        jsr log_char
        pla
        jsr log_char

+       pla ; restore .X from stack and re-save
        tax
        pha
        jsr get_flags
        and #$02
        beq +
        jsr get_pushed_x
        tax
        lda #'X'
        jsr log_register

+       pla ; restore .X from stack and re-save
        tax
        pha
        jsr get_flags
        and #$04
        beq +
        jsr get_pushed_y
        tax
        lda #'Y'
        jsr log_register

+       pla ; restore .X from stack
        tax

        rts

log_outputs rts

hook_return rts

log_register
        tay ; save .X to stack, then .A
        txa
        pha
        tya
        pha

        lda #' '
        jsr log_char

        pla ; restore .A input
        jsr log_char

        lda #'='
        jsr log_char

        pla ; was .X input
        jmp log_hex

log_string
        stx $fb
        sty $fc
        lda log_rem+1
        bne +
        lda log_rem
        beq ++
+       ldx log_ptr
        ldy log_ptr+1
        stx $fd
        sty $fe
        ldy #0
-       lda ($fb),y
        beq ++
        sta ($fd),y
        inc log_ptr
        bne +
        inc log_ptr+1
+       dec log_rem
        bne +
        dec log_rem+1
        beq ++
+       iny
        bne -
++      rts
        
log_char
        ldx log_rem+1
        bne +
        ldx log_rem
        beq ++
+       ldx log_ptr
        ldy log_ptr+1
        stx $fb
        sty $fc
        ldy #0
        sta ($fb),y
        inc log_ptr
        bne +
        inc log_ptr+1
+       dec log_rem
        bne ++
        dec log_rem+1
++      rts

log_addr ; low address of hook in X
        txa     ; save X to stack twice
        pha
        pha
        
        lda #' '
        jsr log_char

        lda #$FF        ; display high byte
        jsr log_hex

        pla             ; restore low byte
        jsr log_hex

        lda #' '
        jsr log_char

        pla     ; restore X from stack
        tax
        pha     ; push back

        jsr get_name
        jsr log_string

        pla     ; restore X from stack
        tax

        rts

log_hex
        pha
        lsr
        lsr
        lsr
        lsr
        jsr log_digit
        pla
        and #$0f
        ; fall thru disp_digit

log_digit
        cmp #16
        bcs ++
        cmp #10
        bcs +
        adc #'0'
        jmp log_char
+       sbc #$0A
        adc #$40
        jmp log_char
++      rts        

kernel_entries
; offset, 3 bytes for JSR code to hook, 3 bytes for original code, 2 byte name of routine, and diag bit flags
; diag bit flags
; $01 = display .A input
; $02 = display .X input
; $04 = display .Y input
; $08 = LOAD flag - display .X/.Y if SA=0
; $10 = SAVE flag - display indirect address zero page from .A
; $20 = display .Y output
; $40 = display .X output
; $80 = display .A output
        !byte $A5, 00, 00, 00, 00, 00, 00, <n_acptr, >n_acptr, $80
        !byte $C6, 00, 00, 00, 00, 00, 00, <n_chkin, >n_chkin, $02
        !byte $C9, 00, 00, 00, 00, 00, 00, <n_chkout, >n_chkout, $02
        !byte $CF, 00, 00, 00, 00, 00, 00, <n_chrin, >n_chrin, $80
        !byte $D2, 00, 00, 00, 00, 00, 00, <n_chrout, >n_chrout, $01
        !byte $A8, 00, 00, 00, 00, 00, 00, <n_ciout, >n_ciout, $00
        !byte $E7, 00, 00, 00, 00, 00, 00, <n_clall, >n_clall, $00
        !byte $C3, 00, 00, 00, 00, 00, 00, <n_close, >n_close, $01
        !byte $CC, 00, 00, 00, 00, 00, 00, <n_clrchn, >n_clrchn, $00
        !byte $E4, 00, 00, 00, 00, 00, 00, <n_getin, >n_getin, $80
        !byte $B1, 00, 00, 00, 00, 00, 00, <n_listen, >n_listen, $01
        !byte $D5, 00, 00, 00, 00, 00, 00, <n_load, >n_load, $69
        !byte $C0, 00, 00, 00, 00, 00, 00, <n_open, >n_open, $00
        !byte $B7, 00, 00, 00, 00, 00, 00, <n_readst, >n_readst, $80
        !byte $D8, 00, 00, 00, 00, 00, 00, <n_save, >n_save, $16
        !byte $93, 00, 00, 00, 00, 00, 00, <n_second, >n_second, $01
        !byte $BA, 00, 00, 00, 00, 00, 00, <n_setlfs, >n_setlfs, $07
        !byte $BD, 00, 00, 00, 00, 00, 00, <n_setnam, >n_setnam, $20
        !byte $B4, 00, 00, 00, 00, 00, 00, <n_talk, >n_talk, $01
        !byte $96, 00, 00, 00, 00, 00, 00, <n_tksa, >n_tksa, $00
        !byte $AE, 00, 00, 00, 00, 00, 00, <n_unlsn, >n_unlsn, $00
        !byte $AB, 00, 00, 00, 00, 00, 00, <n_untlk, >n_untlk, $00
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

log_busy !byte 0

log_ptr !byte 0, 0
log_rem !byte 0, 0

bytes_free !text " log bytes free"
        !byte 13, 0

copyright 
        !byte 14 ; upper/lowercase character sets
        !byte 147 ; clear screen
        !text "c64 io mONITOR 1.20"
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
        !text "poke 56,128 : clr : rem start of log"
        !byte 13
        !text "sys 49152 : rem hook kernel entries"
        !byte 13
        !text "sys 49155 : rem display counts"
        !byte 13
        !text "sys 49158 : rem display log"
        !byte 13
        !text "rem stop + restore to stop hooks"
        !byte 13
        !byte 13
        !byte 0 ; end of string