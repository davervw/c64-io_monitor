; io_monitor.asm
;
; by David R. Van Wagner
; dave@davevw.com
; blog.davevw.com
;
; MIT LICENSE
;
;
; hooks C64 kernal entry points via modifying entry points after copying ROM to RAM, running Kernal/BASIC in RAM
;
; SYS 49152: REM initialize hooks and then running system from RAM (note can call again to clear out counts)
;   also sets log destination past BASIC program's RAM (55/56) if room before $A000
; SYS 49155: REM display hit counts
; SYS 49158: REM display log of hits
; SYS 49161: REM clear log

start=$C000 ; machine language org
chrout=$f1ca ;$ffd2

; "hires ml"
* = start
        jmp init_hooks
        jmp display_counts
        jmp display_log
        jmp clear_log

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
        sta log_start
        lda 56 ; high byte end of BASIC RAM (variables) area
        sta log_start+1

clear_log
        lda log_start
        sta log_ptr
        lda log_start+1
        sta log_ptr+1
        jsr reset_counts
        sec
        lda #$00
        sbc log_start
        sta log_rem
        lda #$A0
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
        sta call_counts_high, x
        inx
        bne -

-       lda #0
        sta vector_entries+10, x
        sta vector_entries+11, x
        txa
        clc
        adc #12 ; advance to index of next vector_entries record
        tax
        cpx #vector_entries_max
        bcc -

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
        ora call_counts_high, x
        bne +       
        jmp ++
 
        ; ***WAS*** supposed to check vector if call_counts were zero, but not working yet, skipping over this part

        txa ; save X
        pha
        jsr get_vector_count
        bne +++ ; found a vector

        ; no vector
--      pla ; restore X and skip
        tax
        jmp ++

+++     stx $fb ; test count lo/hi if zero
        tya
        ora $fb
        beq --
        pla ; restore X and skip
        tax
        jmp ++

+       txa
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

        pla ; restore index
        pha ; save index again
        jsr get_name
        jsr len_string
        sta sublen2+1 ; self-modifying code
        sec
        lda #7
sublen2 sbc #0
        tax
--      lda #$20 ; space
        jsr chrout
        dex
        bne --

        pla ; restore index
        pha ; save index again
        tax
        lda call_counts_high, x ; retrieve non-zero count again for display
        jsr disp_hex

        pla ; restore index
        pha ; save index again
        tax
        lda call_counts, x ; retrieve non-zero count again for display
        jsr disp_hex

        lda #' '
        jsr chrout

        pla ; restore index
        pha ; save index again
        tax
        jsr get_vector_count
        beq + ; branch if no vector
        pha ; save $03XX offset too
        lda #'('
        jsr chrout
        lda #3
        jsr disp_hex
        pla ; restore $03XX offset
        pha ; save again
        jsr disp_hex
        lda #')'
        jsr chrout
        lda #' '
        jsr chrout
        pla ; restore $03XX offset
        pla ; restore index
        pha ; save index again
        tax
        jsr get_vector_count ; repeat, because lost X/Y
        txa
        pha
        tya
        jsr disp_hex
        pla
        jsr disp_hex

+       lda #$0D ; carriage return
        jsr chrout

        pla ; restore index
        tax

++      inx
        beq +
        jmp -

+       rts

get_vector_count ; INPUT: .X $FFXX low byte address, A = $03XX vector low addr. or zero, OUTPUT Z clear if vector found, X/Y = low/high byte vector count
        jsr kernal_xx_to_offset
        lda kernal_entries+10, x
        beq +
        pha ; save $03XX vector low address
        tax
        jsr vector_to_offset
        lda vector_entries+11, x
        tay
        lda vector_entries+10, x
        tax
        pla ; should clear Z flag because is non-zero based on branch above
+       rts

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

        ; x+=11 skip size of non-matching entry record
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
        inx ; +1
        sta kernal_entries, x

        lda #<hook_entry
        inx ; +2
        sta kernal_entries, x

        lda #>hook_entry
        inx ; +3
        sta kernal_entries, x

        lda ($FB),y
        inx ; +4
        sta kernal_entries, x
        lda #$4C
        sta ($FB),y

        iny
        lda ($FB),y
        inx ; +5
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
        inx ; +6
        sta kernal_entries, x
        clc
        lda #>kernal_entries
        plp
        adc #$00
        sta ($FB),y

        inx ; +7
        inx ; +8
        inx ; +9
        inx ; +10
        lda kernal_entries, x
        beq ++ ; no vector
        jsr hook_vector
++      inx ; +11
        ldy #$00
        beq -

+       cli

        jsr disp_hooks

        rts

hook_vector
        tay ; save $03XX vector offset in .Y

        txa ; save .X to stack
        pha

        lda #<vector_entries
        sta $fd
        lda #>vector_entries
        sta $fe
        tya ; restore vector offset
        tax ; save offset in .X
        ldy #0

-       eor ($fd),y ; look for vector offset match
        bne + ; branch if no match
        iny
        iny
        lda #<vector_enter
        sta ($fd),y
        iny
        lda #>vector_enter
        sta ($fd),y
        iny
        iny
        iny
        lda ($fd),y ; check stored vector high byte
        bne ++ ; vector already set
        dey
        lda $0300,x ; get vector low byte
        sta ($fd),y
        lda $0301,x ; get vector high byte
        iny
        sta ($fd),y
        iny
        iny
        lda #<vector_exit
        sta ($fd),y
        iny
        lda #>vector_exit
        sta ($fd),y
        tya
        sec
        sbc #(9-1)
        clc
        adc $fd
        sta $0300,x
        lda $fe
        adc #0
        sta $0301,x
        txa
        jmp ++

+       tya ; advance to next index
        clc
        adc #12
        tay
        txa ; restore offset to .A
        cpy #vector_entries_max
        bcc -

++      pla ; restore .X
        tax

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

        lda kernal_entries+10, x
        beq no_vector
        lda #'*'
        bne ++++
no_vector
        lda #' '
++++    jsr chrout

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

+++     pla ; restore x, advance by 11, loop
        clc
        adc #11
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
        jsr inc_count
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

vector_enter
        php
        sei
        pha
        txa
        pha
        tya
        pha

        tsx
        lda $105, x
        ldy $106, x
        tax
        jsr find_vector_by_stack_addr
        bne + ; didn't find

        inc vector_entries+10,x
        bne + ; didn't overflow low byte
        inc vector_entries+11,x
        bne + ; didin't overflow high byte
        dec vector_entries+10,x ; overflow 16-bit, so reset to max
        dec vector_entries+11,x

+       pla
        tay
        pla
        tax
        pla
        plp
        rts

vector_exit
        rts

find_vector_by_stack_addr ; INPUT x/y from stack return address of JSR .vector_enter
; OUTPUT will be Z set if found, .X offset within vector_entries
        stx findvl+1
        sty findvh+1
        sec
findvl  lda #0
        sbc #<(vector_entries+3)
        tax
findvh  lda #0
        sbc #>(vector_entries+3)
        bcc +
        cmp #vector_entries_max
        beq +
        bcs +
        lda #0 ; set Z
        rts
+       ldx #vector_entries_max
        lda #1 ; clear Z
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
        rts

inc_count
        inc call_counts, x
        bne + ; branch not zero
        inc call_counts_high, x
        bne + ; branch not zero
        dec call_counts, x ; oops, on overflow to zero, return to max value
        dec call_counts_high, x ; oops, on overflow to zero, return to max value
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
        adc #11
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

kernal_xx_to_offset
        ldx #0
-       ldy kernal_entries,x
        bne +
        rts
+       cmp kernal_entries,x
        bne +
        rts ; found!

        ; x+=11
+       inx
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
        jmp -

kernal_n_to_offset
        clc
        lda #0
        cpx #0
        beq +
-       adc #11
        dex
        bne -
+       tax
        rts

vector_to_offset ; INPUT .A is $3XX vector address, OUTPUT .X offset in .vector_entries
        ldx #0
-       cmp vector_entries, x
        beq ++

        ; .X += 12
        pha
        clc
        txa
        adc #12
        tax
        pla

        cpx #vector_entries_max
        bcc -

+       ldx #$FF ; invalid
++      rts

kernal_entries
; IMPORTANT! must be 23 or fewer entries to not go over 255 bytes
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
        ; 11 byte records
        ;   $FFxx,JSR,<hk,>hk,JMP,<ad,>ad, <name,    >name, diag_flags, 0x300 vector offset
        !byte $A5, 00, 00, 00, 00, 00, 00, <n_acptr,  >n_acptr,  $80, 0x00
        !byte $C6, 00, 00, 00, 00, 00, 00, <n_chkin,  >n_chkin,  $02, 0x1E
        !byte $C9, 00, 00, 00, 00, 00, 00, <n_chkout, >n_chkout, $02, 0x20
        !byte $CF, 00, 00, 00, 00, 00, 00, <n_chrin,  >n_chrin,  $80, 0x24
        !byte $D2, 00, 00, 00, 00, 00, 00, <n_chrout, >n_chrout, $01, 0x26
        !byte $A8, 00, 00, 00, 00, 00, 00, <n_ciout,  >n_ciout,  $00, 0x00
        !byte $E7, 00, 00, 00, 00, 00, 00, <n_clall,  >n_clall,  $00, 0x2C
        !byte $C3, 00, 00, 00, 00, 00, 00, <n_close,  >n_close,  $01, 0x1C
        !byte $CC, 00, 00, 00, 00, 00, 00, <n_clrchn, >n_clrchn, $00, 0x22
        !byte $E4, 00, 00, 00, 00, 00, 00, <n_getin,  >n_getin,  $80, 0x2A
        !byte $B1, 00, 00, 00, 00, 00, 00, <n_listen, >n_listen, $01, 0x00
        !byte $D5, 00, 00, 00, 00, 00, 00, <n_load,   >n_load,   $69, 0x30
        !byte $C0, 00, 00, 00, 00, 00, 00, <n_open,   >n_open,   $00, 0x1A
        !byte $B7, 00, 00, 00, 00, 00, 00, <n_readst, >n_readst, $80, 0x00
        !byte $D8, 00, 00, 00, 00, 00, 00, <n_save,   >n_save,   $10, 0x32
        !byte $93, 00, 00, 00, 00, 00, 00, <n_second, >n_second, $01, 0x00
        !byte $BA, 00, 00, 00, 00, 00, 00, <n_setlfs, >n_setlfs, $07, 0x00
        !byte $BD, 00, 00, 00, 00, 00, 00, <n_setnam, >n_setnam, $00, 0x00
        !byte $B4, 00, 00, 00, 00, 00, 00, <n_talk,   >n_talk,   $01, 0x00
        !byte $96, 00, 00, 00, 00, 00, 00, <n_tksa,   >n_tksa,   $00, 0x00
        !byte $AE, 00, 00, 00, 00, 00, 00, <n_unlsn,  >n_unlsn,  $00, 0x00
        !byte $AB, 00, 00, 00, 00, 00, 00, <n_untlk,  >n_untlk,  $00, 0x00
        !byte 0 ; end of table marker

vector_entries ; 12 bytes each
        ; 0x3XX offset, JSR entry, JSR routine, JMP exit, count(16-bit)
        !byte 0x1A, 0x20, 0x00, 0x00, 0x20, 0x00, 0x00, 0x4C, 0x0, 0x00, 0x00, 0x00
        !byte 0x1C, 0x20, 0x00, 0x00, 0x20, 0x00, 0x00, 0x4C, 0x0, 0x00, 0x00, 0x00
        !byte 0x1E, 0x20, 0x00, 0x00, 0x20, 0x00, 0x00, 0x4C, 0x0, 0x00, 0x00, 0x00
        !byte 0x20, 0x20, 0x00, 0x00, 0x20, 0x00, 0x00, 0x4C, 0x0, 0x00, 0x00, 0x00
        !byte 0x22, 0x20, 0x00, 0x00, 0x20, 0x00, 0x00, 0x4C, 0x0, 0x00, 0x00, 0x00
        !byte 0x24, 0x20, 0x00, 0x00, 0x20, 0x00, 0x00, 0x4C, 0x0, 0x00, 0x00, 0x00
        !byte 0x26, 0x20, 0x00, 0x00, 0x20, 0x00, 0x00, 0x4C, 0x0, 0x00, 0x00, 0x00
        !byte 0x2A, 0x20, 0x00, 0x00, 0x20, 0x00, 0x00, 0x4C, 0x0, 0x00, 0x00, 0x00
        !byte 0x2C, 0x20, 0x00, 0x00, 0x20, 0x00, 0x00, 0x4C, 0x0, 0x00, 0x00, 0x00
        !byte 0x30, 0x20, 0x00, 0x00, 0x20, 0x00, 0x00, 0x4C, 0x0, 0x00, 0x00, 0x00
        !byte 0x32, 0x20, 0x00, 0x00, 0x20, 0x00, 0x00, 0x4C, 0x0, 0x00, 0x00, 0x00
vector_entries_end
        !byte 0
vector_entries_max = (vector_entries_end-vector_entries)

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

call_counts_high ; indexed by $FFXX low byte of entry point, not all bytes are used, but very simple container (256 bytes)
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
        !text "c64 io mONITOR 1.29"
        !byte 146 ; reverse off
        !byte 13 ; carriage return
        !text "tRACE/COUNT kernal io JMP TABLE&VECTORS"
        !byte 13
        !text "(c) 2021 BY dAVID r. vAN wAGNER"
        !byte 13
        !text "DAVE@DAVEVW.COM"
        !byte 13
        !text "BLOG.DAVEVW.COM"
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

finis
