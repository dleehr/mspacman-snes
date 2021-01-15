; --- Aliases / labels
; aliases for memory mapped registers
INIDISP     = $2100     ; Initial settings for screen
OBJSEL      = $2101     ; Object size $ object data area designation
OAMADDL     = $2102     ; Address (Low) for accessing OAM
OAMADDH     = $2103     ; high
OAMDATA     = $2104     ; data for OAM write
VMAINC      = $2115     ; VRAM Address increment value designation
VMADDL      = $2116     ; address (Low) for VRAM write
VMADDH      = $2117     ; high
VMDATAL     = $2118     ; Data (low) for vram write
VMDATAH     = $2119     ; high
CGADD       = $2121     ; address for CGRAM read/write
CGDATA      = $2122     ; Data for CGRAM write
TM          = $212c     ; main screen designation
NMITIMEN    = $4200     ; enable flag for v-blank
MDMAEN      = $420b     ; DMA Enable Register
RDNMI       = $4210     ; Read the NMI flag status
DMAP0       = $4300     ; DMA Control register, channel 0
BBAD0       = $4301     ; DMA Destination register, channel 0
A1T0L       = $4302     ; DMA Source address register low, channel 0
A1T0H       = $4303     ; DMA Source address register high, channel 0
A1T0B       = $4304     ; DMA Source address register bank, channel 0
DAS0L       = $4305     ; DMA size register low, channel 0
DAS0H       = $4306     ; DMA size register high, channel 0
; ---

; --- Memory Map WRAM (just the layout of memory locations)
HOR_SPEED   = $0300     ; The horizontal speed
VER_SPEED   = $0301     ; the vertical speed
OAMMIRROR   = $0400     ; location of OAMRAM mirror in WRAM
; ---

; --- Game Constants
; Use these to check for collisions with screen boundaries
SCREEN_LEFT     = $00   ; left screen boundary = 0
SCREEN_RIGHT    = $ff   ; right screen boundary = 255
SCREEN_TOP      = $00   ; top screen boundary = 0
SCREEN_BOTTOM   = $df   ; bottom screen boundary = 223
; simple constant to define sprite movement speed
SPRITE_SPEED    = $02   ; sprites will move 2 pixels per frame
; makes the code a bit more readable
SPRITE_SIZE     = $08   ; sprites are 8 by 8 pixels
OAMMIRROR_SIZE  = $0220 ; OAMRAM can hold 128 spites, 4 bytes each (oh right, this is just the object attributes - x, y, name, flip/prio/palette)
; ---

; --- assembler directives
.p816                   ; tell the assembler this is 65816 code
; ---

; --- includes
.segment "SPRITEDATA"
SpriteData: .incbin "Sprites.vra"
ColorData:  .incbin "SpriteColors.pal"
; ---

.segment "CODE"
; ---
; entry point
; ---
.proc ResetHandler
    sei             ; Disable interrupts
    clc             ; clear carry flag
    xce             ; switch to native mode
    rep #$10        ; set X and Y to 16-bit
    sep #$20        ; set A to 8-bit
    lda #$8f        ; force v-blank
    sta INIDISP
    stz NMITIMEN    ; store zero to NMITIMEN
    ; set stack pointer to $1fff
    ldx #$1fff
    txs             ; copy x to stack pointer

    ; load sprites into VRAM
    tsx             ; save current stack pointer
    pea $0000       ; push VRAM destination address to stack (is this a memory map offset thing later?)
    pea SpriteData  ; push sprite source address to stack
    pea $0080       ; push count of bytes (128 / $80) to transfer to stack
    jsr LoadVRAM    ; transfer vram data in subroutine
    txs             ; "delete" data on stack by restoring old stack pointer

    ; load color data into CGRAM - palettes
    tsx             ; save current stack pointer (is this a no-op from before?)
    lda #$80        ; push CGRAM destination address to stack
    pha             ; through A (why not pea? - guess because that's 2 bytes and we only want 1?)
    pea ColorData   ; Push paletes source address to stack
    pea $0020       ; push count of bytes (32 / $20) to transfer to stack
    jsr LoadCGRAM   ; transfer color data into CGRAM
    txs             ; "delete" data on stack by restoring old stack pointer

    ; set up initial data in OAMRAM mirror, using X as index
    ldx #$00
    ; upper-left sprite, starts at halfway point
    lda #(SCREEN_RIGHT/2 - SPRITE_SIZE); sprite 1, horizontal position to put at center of screen
    sta OAMMIRROR, X    ; store A into OAMMIRROR memory location, offset by X (like c pointer arithmetic offset)
    inx                 ; increment index
    lda #(SCREEN_BOTTOM/2 - SPRITE_SIZE) ; sprite 1, vertical position
    sta OAMMIRROR, X
    inx
    lda #$00            ; sprite 1, name is 00
    sta OAMMIRROR, X
    inx
    lda #$00            ; no filp, palette 0
    sta OAMMIRROR, X
    inx
    ; upper-right sprite
    lda #(SCREEN_RIGHT/2)  ;    sprite 2, horizontal position - to the immediate right of sprite 1
    sta OAMMIRROR, X
    inx
    lda #(SCREEN_BOTTOM/2 - SPRITE_SIZE)    ; sprite 2, vertical position
    sta OAMMIRROR, X
    inx
    lda #$01            ; sprite 2, name is 01
    sta OAMMIRROR, X
    inx
    lda #$00            ; no flip, palette 0
    sta OAMMIRROR, X
    inx
    ; lower-left sprite
    lda #(SCREEN_RIGHT/2 - SPRITE_SIZE) ;   sprite 3, horizontal position, now the lower left
    sta OAMMIRROR, X
    inx
    lda #(SCREEN_BOTTOM/2)      ; sprite 3, vertical position
    sta OAMMIRROR, X
    inx
    lda #$02            ; sprite 3, name is 02
    sta OAMMIRROR, X
    inx
    lda #$00            ; no flip, palette 0
    sta OAMMIRROR, X
    inx
    ; lower-right sprite
    lda #(SCREEN_RIGHT/2)       ; sprite 4, horizontal position
    sta OAMMIRROR, X
    inx
    lda #(SCREEN_BOTTOM/2)      ; sprite 4, vertical position
    sta OAMMIRROR, X
    inx
    lda #$03            ; sprite 4, name is 03
    sta OAMMIRROR, X
    inx
    lda #$00            ; no flip, palette 0
    sta OAMMIRROR, X
    inx
    ; move other sprites off screen in a loop
    lda #$ff            ; this is 255, we will use it for both horizontal and vertical position
OAMLoop:
    sta OAMMIRROR, X
    inx
    cpx #OAMMIRROR_SIZE
    bne OAMLoop         ; set every remaining coordinate (and other attributes) to 255

    ; correct extra OAM byte for first four sprites (what?)
    ldx #$0200
    lda #$00
    sta OAMMIRROR, X

    ; set initial horizontal and vertical speed
    lda #SPRITE_SPEED
    sta HOR_SPEED
    sta VER_SPEED

    ; .byte $42, $00        ; debugger breakpoint

    ; make objects visible
    lda #$10
    sta TM
    ; release forced blanking, set screen to full brightness
    lda #$0f
    sta INIDISP
    ; enable NMI, turn on automatic joypad polling
    lda #$81
    sta NMITIMEN

    jmp GameLoop        ; all init is done
.endproc
; ---

; ---
; after reset handler will jump to here
; ---
; .smart ; keep track of registers widths
.proc GameLoop
    wai              ; wait for NMI/V-Blank

    .byte $42, $00
    ; game logic: move the sprites
    ; move sprite 1 horizontally
    ; check collision left boundary
    lda HOR_SPEED
    bpl RightBoundaryCheck  ; if speed is positive, sprites are moving right so don't check left boundary check
    lda OAMMIRROR           ; load the horizontal position of the first sprite, which is the first byte at OAMMIRROR
    clc                     ; clear carry flag because we'll be adding and want to make sure it's not set
    adc HOR_SPEED           ; Add speed to the x position to get new x position
    bcs UpdateHorPosition   ; if carry is set, we're still in range?
    ; if clear, we moved below zero, reset x position
    stz OAMMIRROR       ; store zero to h position
    bra InvertHorSpeed  ; flip the horizontal speed vector
    ; check right boundary
RightBoundaryCheck:
    lda OAMMIRROR
    clc
    adc HOR_SPEED
    cmp #(SCREEN_RIGHT - 2 * SPRITE_SIZE) ; Check if x position is below edge minus 2 sprite widths
    bcc UpdateHorPosition   ; if carry is clear, we're still in range
    ; if set, we moved past the edge of the screen, back off x
    lda #(SCREEN_RIGHT - 2 * SPRITE_SIZE)
    sta OAMMIRROR       ; store x position of sprite
    bra InvertHorSpeed  ; flip horizontal speed vector
UpdateHorPosition:
    sta OAMMIRROR       ; store new x position of sprite
    bra VerticalCheck   ; Now check vertical collisions
; invert horizontal speed after horizontal bounce
InvertHorSpeed:
    lda HOR_SPEED       ; load current speed
    eor #$ff            ; negate - first by xoring all bits
    clc                 ; then add 1, must clear carry first
    adc #$01
    sta HOR_SPEED       ; store inverted speed

; move sprite 1 vertically
VerticalCheck:
    ; check upper collision boundary
    lda VER_SPEED
    bpl CheckLowerBoundary  ; if sprites are moving down, skip upper check
    lda OAMMIRROR + $01     ; load current y position of first sprite
    clc
    adc VER_SPEED
    bcs UpdateVerPosition ; if carry is set we're still in range
    ; else, reposition sprite 1 vertical position to zero
    stz OAMMIRROR + $01   ; store new y position
    bra InvertVerSpeed    ; bounce off top of screen
; check lower boundary
CheckLowerBoundary:
    lda OAMMIRROR + $01   ; load current y position of current sprite
    clc
    adc VER_SPEED
    cmp #(SCREEN_BOTTOM - 2 * SPRITE_SIZE) ; compare to bottom edge
    bcc UpdateVerPosition ; if carry clear, we're still in range
    ; else, reposition sprite 1 vertical position to edge
    lda #(SCREEN_BOTTOM - 2 * SPRITE_SIZE) ; bottom edge
    sta OAMMIRROR + $01   ; store y position
    bra InvertVerSpeed    ; bounce off bottom of screen
UpdateVerPosition:
    sta OAMMIRROR + $01     ; store new y position of sprite
    bra UpdateOtherSprites  ; update rest of sprites based on first one
; invert vertical speed after vertical bounce
InvertVerSpeed:
    lda VER_SPEED           ; load current vertical speed
    eor #$ff                ; flip all bits
    clc
    adc #$01
    sta VER_SPEED           ; store inverted speed

UpdateOtherSprites:
    lda OAMMIRROR           ; get x position of 1st sprite
    sta OAMMIRROR + $08     ; set x position of 3rd sprite, same as 1
    clc
    adc #SPRITE_SIZE        ; x position of 2 and 4 will be offset by witdh of a sprite
    sta OAMMIRROR + $04     ; set x position of sprite 2
    sta OAMMIRROR + $0c     ; set x position of sprite 4
    ; vertical positions
    lda OAMMIRROR + $01     ; get y position of 1st sprite
    sta OAMMIRROR + $05     ; set y position of 2nd sprite
    clc
    adc #SPRITE_SIZE        ; y position of 3 and 4 will be offset by height of sprite
    sta OAMMIRROR + $09     ; set y position of 3rd sprite
    sta OAMMIRROR + $0d     ; set y position of 4th sprite

    jmp GameLoop
.endproc
; ---

; ---
; called during v-blank every frame
; ---
.proc   NMIHandler
        lda RDNMI           ; read NMI status, acknowledge NMI

        ; this is where we do graphics update
        tsx                 ; save old stack pointer
        pea OAMMIRROR       ; push mirror address to stack
        jsr UpdateOAMRAM    ; update OAMRAM
        txs                 ; restore old stack pointer

        rti
.endproc
; ---

; ---
; irq not used here
; ---
.proc   IRQHandler
        ; code
        rti
.endproc
; ---

; ---
; load sprite data into VRAM
; params: NumBytes: .byte, SrcPointer: .addr, DestPointer: .addr
; ---
.proc   LoadVRAM
        phx         ; save old stack pointer that was transferred to x
        ; create frame pointer
        phd         ; push direct register to stack
        tsc         ; transfer stack pointer to C
        tcd         ; transfer C to direct register
        ; using constants to access args on stack with direct register
        NumBytes    = $07   ; number of bytes - 2 bytes
        SrcPointer  = $09   ; source memory address - 2 bytes
        DestPointer = $0b   ; dest memory address - 2 bytes

        ; set dest address in VRAM and address inc after a write
        ldx DestPointer ; load destination pointer ... this comes in as 0000 from pea
        stx VMADDL      ; store it to the VRAM Address low $2116
        lda #$80        ; ... this is not 1 as next line comment says
        sta VMAINC      ; increment VRAM address by 1 when writing to VMDATAH

        ; loop through source data and transfer to VRAM
        ldy #$0000      ; set register Y to zero, we will use Y as a loop counter / offset
VRAMLoop:
        lda (SrcPointer, S), Y  ; get bitplane 0/2 byte from sprite data
        sta VMDATAL             ; write the byte in A to VRAM
        iny                     ; increment loop counter
        lda (SrcPointer, S), Y  ; get bitplane 1/3 byte from sprite data
        sta VMDATAH             ; write the byte in A to VRAM
        iny                     ; increment loop counter
        cpy NumBytes            ; Check if we have written all the bytes
        bcc VRAMLoop            ; if Y is smaller than the count, continue

        ; all done
        pld                     ; restore caller's frame pointer
        plx                     ; restore old stack pointer
        rts
.endproc
; ---

; ---
; load color data into CGRAM
; params: NumBytes: .byte, SrcPointer: .addr, DestPointer: :byte
; ---
.proc   LoadCGRAM
        phx                     ; save old stack pointer
        ; create a frame pointer
        phd                     ; push direct register to stack
        tsc                     ; transfer stack to ...
        tcd                     ; direct register
        .byte $42, $00
        ; constants to access args on stack with direct addressing
        NumBytes    = $07       ; number of bytes to transfer
        SrcPointer  = $09       ; source address of sprite data
        DestPointer = $0b      ; dest address in CGRAM

        ; set CGRAM destination address
        lda DestPointer         ; load dest addr into a
        sta CGADD               ; set CGRAM dest address

        ldy #$0000              ; set Y to zero, will be loop counter
CGRAMLoop:
        lda (SrcPointer, S), Y  ; get the color low byte
        sta CGDATA              ; store in CGRAM
        iny                     ; increase counter
        lda (SrcPointer, S), Y  ; get color high byte
        sta CGDATA              ; store in CGRAM
        iny
        cpy NumBytes            ; compare to transfer count
        bcc CGRAMLoop           ; loop until done

        ; all done
        pld                     ; pull back direct register
        plx                     ; restore old stack pointer into x
        rts                     ; return to caller
.endproc
; ---

;---
; Copy mirror into OAMRAM using DMA
;---
.proc   UpdateOAMRAM
        phx                     ; save old stack pointer
        ; create  aframe pointer
        phd
        tsc
        tcd
        ; constants for args offset
        MirrorAddr =    $07     ; address of mirror

        ; set up dma channel 0 to transfer data to OAMRAM
        lda #%00000010          ; set DMA Channel 0
        sta DMAP0
        lda #$04                ; set destination to OAMDATA
        sta BBAD0
        ldx MirrorAddr          ; load address of OAMRAM Mirror arg
        stx A1T0L               ; set low and high byte of address
        stz A1T0B               ; set bank to zero, since mirror in WRAM
        ldx #OAMMIRROR_SIZE     ; number of bytes to transfer
        stx DAS0L               ; set size

        lda #$01                ; load 1 into a...
        sta MDMAEN              ; ... and store it to begin DMA transfer

        ; OAMRAM update is done, clean up and return
        pld
        plx
        rts
.endproc
;---

;-------------------------------------------------------------------------------
;   Interrupt and Reset vectors for the 65816 CPU
;-------------------------------------------------------------------------------
.segment "VECTOR"
; native mode   COP,        BRK,        ABT,
.addr           $0000,      $0000,      $0000
;               NMI,        RST,        IRQ
.addr           NMIHandler, $0000,      $0000

.word           $0000, $0000    ; four unused bytes

; emulation m.  COP,        BRK,        ABT,
.addr           $0000,      $0000,      $0000
;               NMI,        RST,        IRQ
.addr           $0000,      ResetHandler, $0000
