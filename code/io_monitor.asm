; io_monitor.asm
;
; by David R. Van Wagner
; dave@davevw.com
; techwithdave.davevw.com
;
; MIT LICENSE
;
;
; hooks C64 kernal entry points via modifying entry points after copying ROM to RAM, running Kernal/BASIC in RAM
;
; SYS 49152: REM initialize hooks and then running system from RAM (note can call again to clear out counts)
;   also sets log destination past BASIC program's RAM (55/56) if room before $A000
; SYS 49155: REM display hit counts
; SYS 49158: REM display log of hits, return log_ptr in x,y 
; SYS 49161: REM clear log
; SYS 49164: REM set log start indirect ZP addr in x
; SYS 49167: REM set log end addr in x,y
; SYS 49170: REM toggle don't copy basic

start=$C000 ; machine language org
chrout=$f1ca ;$ffd2

; "hires ml"
* = start
        jmp init_hooks
        jmp display_counts
        jmp display_log
        jmp clear_log
        jmp set_start
        jmp set_end
        jmp basic_no_copy

init_hooks
        jsr bank_norm
        jsr disp_copyright_usage

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
copy_basic? = * + 1
        lda #0
        bne +
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
+       jsr hook_entries

        lda #$05 ; BASIC/KERNEL ROMs replaced with RAM, leave I/O as is
        jsr bank_select

        ; copy 55/56 to log_ptr, and compute space available (up to $A000) in log_rem
        log_lsb = * + 1
        lda 55 ; initally, low byte end of BASIC RAM (variables) area
        sta log_start
        log_msb = * + 1
        lda 56 ; initially, high byte end of BASIC RAM (variables) area
        sta log_start+1

clear_log
        lda log_start
        sta log_ptr
        lda log_start+1
        sta log_ptr+1
        jsr reset_counts
        sec
        lda log_end
        sbc log_start
        sta log_rem
        lda log_end+1
        sbc log_start+1
        bcs + ; greater than or equal to, okay
        lda #0 ; less than, zero out log_rem
        sta log_rem
+       sta log_rem+1

        ; display $abcd log bytes free
        lda #18
        jsr chrout
        lda #'$'
        jsr chrout
        lda log_rem+1
        jsr disp_hex
        lda log_rem
        jsr disp_hex
        ldx #<bytes_free
        ldy #>bytes_free
        jsr disp_string
        lda #146
        jsr chrout

;clear memory of log
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

        ; display kernal address
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
        lda log_start
        sta $fb
        lda log_start+1
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
++      ldx log_ptr ; return log_ptr in x,y
        ldy log_ptr+1
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

disp_copyright_usage
        ldx #<copyright_usage
        ldy #>copyright_usage
        jmp disp_string

get_name ; INPUT .A is low byte of kernal jump table address $FFXX
         ; OUTPUT .X is low byte of name address, .Y is high byte of name address
        ldx #0
-       ldy kernal_entries, x   ; retrieve low byte to check for end
        beq ++                  ; branch if end
        eor kernal_entries, x   ; compare
        beq +                   ; branch if found it
        eor kernal_entries, x   ; restore .A

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

+       lda kernal_entries+8, x ; high byte of pointer to name string
        tay
        lda kernal_entries+7, x ; low byte of pointer to name string
        tax
        rts

++      ldx #<not_found_string
        ldy #>not_found_string
        rts

disp_name ; INPUT .A is low byte of kernal jump table address $FFXX
        jsr get_name ; get address in .X/.Y

disp_string
        stx $fb
        sty $fc

        ldy #0
-       lda ($fb),Y
        beq +
        jsr chrout
        iny
        bne -
        inc $fc
        bne -

+       rts

len_string ; INPUT x/y is address of string, nul terminated
        stx $fb
        sty $fc

        ldy #0
-       lda ($fb),Y
        beq +
        iny
        bne -

+       tya ; OUTPUT a is length in bytes (0 if overflow), x/y is restored
        ldx $fb
        ldy $fc
        rts


log_string_n
        sta log_string_length
        stx log_string_load+1
        sty log_string_load+2
        ora #0
        beq +

        ldy #0
        sty log_string_index
log_string_load
        lda $0000,y
        jsr log_char
        inc log_string_index
        ldy log_string_index
        dec log_string_length
        bne log_string_load

+       rts

hook_entries
        sei
        lda #$ff
        sta $FC

        ldy #0
        ldx #0
-       lda kernal_entries, x
        beq +

        sta $FB

        lda #$20
        inx
        sta kernal_entries, x

        lda #<hook_entry
        inx
        sta kernal_entries, x

        lda #>hook_entry
        inx
        sta kernal_entries, x

        lda ($FB),y
        inx
        sta kernal_entries, x
        lda #$4C
        sta ($FB),y

        iny
        lda ($FB),y
        inx
        sta kernal_entries, x
        txa
        sec
        sbc #$04
        clc
        adc #<kernal_entries
        php
        sta ($FB),y

        iny
        lda ($FB),y
        inx
        sta kernal_entries, x
        clc
        lda #>kernal_entries
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

        jsr disp_hooks

        rts

disp_hooks
        ldx #0
        stx $fd
-       lda kernal_entries, x ; low byte of kernal jump table address $FFxx
        beq + ; check if end of table

        txa ; push x to stack for safe keeping
        pha

        lda #$ff
        jsr disp_hex

        lda kernal_entries, x ; display entry
        jsr disp_hex
        lda #' '
        jsr chrout
        lda kernal_entries+8, x ; high address of name
        tay
        lda kernal_entries+7, x ; low address of name
        tax
        jsr len_string
        sta sublen+1    ; self-modifying code
        jsr disp_string
        inc $fd
        lda $fd
        cmp #3
        bne ++
        lda #13
        jsr chrout
        lda #0
        sta $fd
        jmp +++

++      sec
        lda #8          ; most are 6 or less
sublen  sbc #0          ; subtract the length
        tax
        beq +++
        lda #' '
--      jsr chrout
        dex
        bne --

+++     pla ; restore x, advance by 10, loop
        clc
        adc #10
        tax
        bcc -

+       rts

hook_entry
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
        stx active_index
        jsr log_inputs
        jsr log_addr
        jsr set_hook_return

        dec log_busy

+       pla ; restore registers and flags
        tay
        pla
        tax
        pla
        plp
        rts

hook_result
        php ; save flags and registers
        sei
        pha
        txa
        pha
        tya
        pha

        jsr log_outputs

        pla ; restore registers and flags
        tay
        pla
        tax
        pla
        plp
        rts

get_index
        tsx ; caller trail is on the stack, so transfer stack index to X for our use
        lda $107, x ; get the low byte of the return to caller, should be our code in kernal_entries
        sec ; prepare to subtract, no borrow yet
        sbc #$03 ; subtract 3 to compute a pointer of our record of the low byte of the kernal vector $FFXX
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
        lda $108, x ; get the low byte of the return to caller, should be our code in kernal_entries
        rts

get_pushed_x
        tsx ; saved registers are on the stack, so transfer stack index to X for our use
        lda $107, x ; get the low byte of the return to caller, should be our code in kernal_entries
        rts

get_pushed_y
        tsx ; saved registers are on the stack, so transfer stack index to X for our use
        lda $106, x ; get the low byte of the return to caller, should be our code in kernal_entries
        rts

get_flags ; input .X index
        ldy #0
-       txa
        cmp kernal_entries, y
        beq +
        tya
        clc
        adc #10
        tay
        bcc -
        lda #0
        bcs ++
+       lda kernal_entries+9,Y
++      rts

log_inputs
        txa ; save .X to stack (low address of hooked kernal entry)
        pha

        jsr get_flags
        and #$01 ; A INPUT
        beq +
        jsr get_pushed_a
        tax
        lda #'A'
        jsr log_register

+       pla ; restore .X from stack and re-save
        tax
        pha
        cpx #$D2 ; CHROUT
        bne +
        jsr get_pushed_a
        jsr conditional_log_char

+       pla
        tax
        pha 
        jsr get_flags
        and #$02 ; X INPUT
        beq +
        jsr get_pushed_x
        tax
        lda #'X'
        jsr log_register

+       pla
        tax
        pha
        jsr get_flags
        and #$04 ; Y INPUT
        beq +
        jsr get_pushed_y
        tax
        lda #'Y'
        jsr log_register

+       pla
        tax
        pha 
        jsr get_flags
        and #$08 ; LOAD input format, conditional on zero SA
        beq +
        lda $B9 ; SA
        bne +
        ldx #<xy_addr
        ldy #>xy_addr
        jsr log_string
        jsr get_pushed_y
        jsr log_hex
        jsr get_pushed_x
        jsr log_hex

+       
        pla
        tax
        pha
        jsr get_flags
        and #$10 ; SAVE input format
        beq +
        ldx #<a_deref
        ldy #>a_deref
        jsr log_string
        jsr get_pushed_a
        sta $fb
        ldy #0
        sty $fc
        iny
        lda ($fb),y
        jsr log_hex
        jsr get_pushed_a
        sta $fb
        ldy #0
        sty $fc
        lda ($fb),y
        jsr log_hex
        ldx #<xy_addr
        ldy #>xy_addr
        jsr log_string
        jsr get_pushed_y
        jsr log_hex
        jsr get_pushed_x
        jsr log_hex

+       pla ; restore .X from stack and re-save
        tax
        pha
        cpx #$BD ; SETNAM
        bne +
        lda #' '
        jsr log_char
        lda #0x22 ; double quote
        jsr log_char
        jsr get_pushed_a
        sta $fb
        jsr get_pushed_x
        sta $fc
        jsr get_pushed_y
        tay
        ldx $fc
        lda $fb
        jsr log_string_n
        lda #0x22 ; double quote
        jsr log_char

+       pla ; restore .X from stack
        tax

        rts

log_outputs
        pha ; need something on stack

        ldx active_index
        cpx #$D5 ; special case for LOAD
        bne +
        ldx #<xy_addr
        ldy #>xy_addr
        jsr log_string
        jsr get_pushed_y
        jsr log_hex
        jsr get_pushed_x
        jsr log_hex
        jmp ++

+       jsr get_flags
        and #$80
        beq +
        jsr get_pushed_a
        tax
        lda #'A'
        jsr log_register

+       ldx active_index
        cpx #$CF ; special case CHARIN
        bne +
        jsr get_pushed_a
        jsr conditional_log_char

+       ldx active_index
        jsr get_flags
        and #$40
        beq +
        jsr get_pushed_x
        tax
        lda #'X'
        jsr log_register

+       ldx active_index
        jsr get_flags
        and #$20
        beq +
        jsr get_pushed_y
        tax
        lda #'Y'
        jsr log_register

++
+       lda #13
        jsr log_char

        pla ; fix stack pointer
        rts

set_hook_return ; insert our desired return address hook on the stack at just the right spot
        tsx
        dex
        stx $fb
        inx
        inx
        stx $fd
        lda #$01
        sta $fc
        sta $fe
        ldy #0
-       lda ($fd),y
        sta ($fb),y
        iny
        cpy #8
        bne -
        lda #<hook_result-1
        sta ($fb),y
        lda #>hook_result-1
        iny
        sta ($fb),y
        tsx
        dex
        dex
        txs
        rts

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

conditional_log_char
        cmp #$7E
        bcs +
        cmp #$1F
        bcc +
        pha
        lda #' '
        jsr log_char
        pla
        jsr log_char
+       rts

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

log_addr ; low address of hook_entry in X
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

set_start
        stx log_lsb
        inx
        stx log msb
        rts
set_end
        stx log_end
        sty log_end+1
        rts
basic_no_copy
        lda copy_basic?
        eor #$FF
        sta copy_basic?
        rts

kernal_entries
; offset, 3 bytes for JSR code to hook_entry, 3 bytes for original code(JMP or JMP(), 2 byte name of routine, and diag bit flags
; diag bit flags
; $01 = display .A input
; $02 = display .X input
; $04 = display .Y input
; $08 = LOAD flag - display .X/.Y if SA zero
; $10 = SAVE flag - display indirect address zero page from .A
; $20 = display .Y output
; $40 = display .X output
; $80 = display .A output
        ;   $FFxx,JSR,<hk,>hk,JMP,<ad,>ad, <name,    >name,    diag_flags
        !byte $A5, 00, 00, 00, 00, 00, 00, <n_acptr,  >n_acptr,  $80
        !byte $C6, 00, 00, 00, 00, 00, 00, <n_chkin,  >n_chkin,  $02
        !byte $C9, 00, 00, 00, 00, 00, 00, <n_chkout, >n_chkout, $02
        !byte $CF, 00, 00, 00, 00, 00, 00, <n_chrin,  >n_chrin,  $80
        !byte $D2, 00, 00, 00, 00, 00, 00, <n_chrout, >n_chrout, $01
        !byte $A8, 00, 00, 00, 00, 00, 00, <n_ciout,  >n_ciout,  $00
        !byte $E7, 00, 00, 00, 00, 00, 00, <n_clall,  >n_clall,  $00
        !byte $C3, 00, 00, 00, 00, 00, 00, <n_close,  >n_close,  $01
        !byte $CC, 00, 00, 00, 00, 00, 00, <n_clrchn, >n_clrchn, $00
        !byte $E4, 00, 00, 00, 00, 00, 00, <n_getin,  >n_getin,  $80
        !byte $B1, 00, 00, 00, 00, 00, 00, <n_listen, >n_listen, $01
        !byte $D5, 00, 00, 00, 00, 00, 00, <n_load,   >n_load,   $69
        !byte $C0, 00, 00, 00, 00, 00, 00, <n_open,   >n_open,   $00
        !byte $B7, 00, 00, 00, 00, 00, 00, <n_readst, >n_readst, $80
        !byte $D8, 00, 00, 00, 00, 00, 00, <n_save,   >n_save,   $10
        !byte $93, 00, 00, 00, 00, 00, 00, <n_second, >n_second, $01
        !byte $BA, 00, 00, 00, 00, 00, 00, <n_setlfs, >n_setlfs, $07
        !byte $BD, 00, 00, 00, 00, 00, 00, <n_setnam, >n_setnam, $00
        !byte $B4, 00, 00, 00, 00, 00, 00, <n_talk,   >n_talk,   $01
        !byte $96, 00, 00, 00, 00, 00, 00, <n_tksa,   >n_tksa,   $00
        !byte $AE, 00, 00, 00, 00, 00, 00, <n_unlsn,  >n_unlsn,  $00
        !byte $AB, 00, 00, 00, 00, 00, 00, <n_untlk,  >n_untlk,  $00
        !byte 0 ; end of table marker

call_counts ; indexed by $FFXX low byte of entry point, not all bytes are used, but very simple container (256 bytes)
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

log_start !byte 0, 0
log_ptr !byte 0, 0
log_rem !byte 0, 0
log_end !byte 0, $A0
active_index !byte 0

log_string_length !byte 0
log_string_index !byte 0

bytes_free !text " LOG BYTES FREE"
        !byte 13, 0

a_deref !text " [A]="
        !byte 0

xy_addr !text " XY="
        !byte 0

not_found_string !text "???"
        !byte 0

copyright_usage
        !byte 14 ; upper/lowercase character sets
        !byte 147 ; clear screen
        !byte 18 ; reverse on
        !text "c64 io mONITOR 1.28"
        !byte 146 ; reverse off
        !byte 13 ; carriage return
        !text "tRACE/COUNT kernal i/o JUMP TABLE CALLS"
        !byte 13
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
        !text "sys 49152 : rem hook kernal entries"
        !byte 13
        !text "sys 49155 : rem display counts"
        !byte 13
        !text "sys 49158 : rem display log"
        !byte 13
        !text "sys 49161 : rem clear log"
        !byte 13
        !text "rem stop + restore to stop hooks"
        !byte 13
        !byte 13
        !byte 0 ; end of string
