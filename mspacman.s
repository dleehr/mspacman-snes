; --- Aliases / labels
; aliases for memory mapped registers
INIDISP     = $2100     ; Initial settings for screen
OBJSEL      = $2101     ; Object size $ object data area designation
OAMADDL     = $2102     ; Address (Low) for accessing OAM
OAMADDH     = $2103     ; high
OAMDATA     = $2104     ; data for OAM write
BGMODE      = $2105     ; background screen mode
BG1TMADD    = $2107     ; BG1 tile map location
BG2TMADD    = $2108     ; BG2 tile map location
BG3TMADD    = $2109     ; BG3 tile map location
BG4TMADD    = $210A     ; BG4 tile map location
BG12CADD    = $210B     ; BG1, BG2 Character location
BG34CADD    = $210C     ; BG3, BG4 Character location
VMAINC      = $2115     ; VRAM Address increment value designation
VMADDL      = $2116     ; address (Low) for VRAM write
VMADDH      = $2117     ; high
VMDATAL     = $2118     ; Data (low) for vram write
VMDATAH     = $2119     ; high
CGADD       = $2121     ; address for CGRAM read/write
CGDATA      = $2122     ; Data for CGRAM write
TM          = $212c     ; main screen designation
NMITIMEN    = $4200     ; enable flag for v-blank
IOPORTW     = $4201     ; IO Port Write Register for joy
MDMAEN      = $420b     ; DMA Enable Register
RDNMI       = $4210     ; Read the NMI flag status
JOY1A       = $4219     ; Joypad 1 status
JOY1B       = $4218     ; Joypad 1 status
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
HOR_OFFSET  = $0302     ; the sprite-edge offset for x
VER_OFFSET  = $0303     ; the sprite-edge offset for y
OAMMIRROR   = $0400     ; location of OAMRAM mirror in WRAM, $220 bytes long
; ---

; --- Joypad memory locations
JOY1AW      = $0700     ;B, Select, Start, Up, Down, Left, Right
JOY1BW      = $0701     ;A, X, L, R, iiii-ID

BG_COLLISION    = $0710 ; will be writing the current background tile here

; ---- Joypad bits
JOY_UP = $08
JOY_DOWN = $04
JOY_LEFT = $02
JOY_RIGHT = $01


; --- Game Constants
; Use these to check for collisions with screen boundaries
SCREEN_LEFT     = $00   ; left screen boundary = 0
SCREEN_RIGHT    = $df   ; right screen boundary = 25
SCREEN_TOP      = $00   ; top screen boundary = 0
SCREEN_BOTTOM   = $df   ; bottom screen boundary = 223
STARTING_X      = $68
STARTING_Y      = $83
TOP_OFFSET      = $05
BOTTOM_OFFSET   = $13
LEFT_OFFSET     = $04
RIGHT_OFFSET    = $14

; simple constant to define sprite movement speed
SPRITE_SPEED    = $00   ; initial speed is stopped
; makes the code a bit more readable
SPRITE_SIZE     = $10   ; sprites are 16x16
OAMMIRROR_SIZE  = $0220 ; OAMRAM can hold 128 spites, 4 bytes each (oh right, this is just the object attributes - x, y, name, flip/prio/palette)
; ---

; --- assembler directives
.p816                   ; tell the assembler this is 65816 code
; ---

; --- includes
.segment "SPRITEDATA"
SpriteData: .incbin "Sprites.vra"
WallData:   .incbin "Walls.vra"
MsPacmanPalette:    .incbin "mspacman.pal"
Level1Palette:      .incbin "level1.pal"
Level1Map:          .incbin "level1.tlm"
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

    ; load background palette to CGRAM
    tsx             ; save current stack pointer (is this a no-op from before?)
    lda #$00        ; push CGRAM destination address to stack
    pha             ; through A (why not pea? - guess because that's 2 bytes and we only want 1?)
    pea Level1Palette   ; Push paletes source address to stack
    pea $0008       ; push count of bytes (8 / $08) to transfer to stack
    jsr LoadCGRAM   ; transfer color data into CGRAM
    txs             ; "delete" data on stack by restoring old stack pointer

    ; load sprite palette to CGRAM
    tsx             ; save current stack pointer (is this a no-op from before?)
    lda #$80        ; push CGRAM destination address to stack
    pha             ; through A (why not pea? - guess because that's 2 bytes and we only want 1?)
    pea MsPacmanPalette   ; Push paletes source address to stack
    pea $0008       ; push count of bytes (8 / $08) to transfer to stack
    jsr LoadCGRAM   ; transfer color data into CGRAM
    txs             ; "delete" data on stack by restoring old stack pointer

    ; load sprites into VRAM
    tsx             ; save current stack pointer
    pea $2000       ; push VRAM destination address to stack
                    ; (is this a memory map offset thing later? NO, it's the VRAM offset)
                    ; remember, VRAM is accessed through memory mapped registers and there
                    ; are two bytes per location. so this $2000 means that data will be loaded
                    ; starting at $4000. Character sprites start at 0000 / 40000 / 8000 / c000
                    ; so this works
    pea SpriteData  ; push sprite source address to stack
    pea $0400       ; push count of bytes (1024 / $400) to transfer to stack
    jsr LoadVRAM    ; transfer vram data in subroutine
    txs             ; "delete" data on stack by restoring old stack pointer

    ; load background tiles into VRAM
    ; Since these are right after the sprites, could just icnrease the count above
    ; but I want to understand it
    tsx
    pea $0000       ; push VRAM destination address - start where the old one left off
    pea WallData    ; wall tiles source address
    pea $0160       ; count of bytes (352) to transfer
    jsr LoadVRAM    ; transfer data in subroutine
    txs             ; "delete" data on stack by restoring old stack pointer

    ; load background
    tsx
    jsr LoadBG
    txs

;    lda #%00000001  ; set up OAM for sprite size andlocation of tiles - they start at $2000
    lda #$01
    sta OBJSEL       ; $2101 Object size $ object data area designation

    ; set up initial data in OAMRAM mirror, using X as index
    ldx #$00
    ; upper-left sprite, starts at halfway point
    lda #STARTING_X     ; starting X position for player
    sta OAMMIRROR, X    ; store A into OAMMIRROR memory location, offset by X (like c pointer arithmetic offset)
    inx                 ; increment index
    lda #STARTING_Y     ; starting Y position for player
    sta OAMMIRROR, X
    inx
    lda #$00            ; sprite 1, name is 00
    sta OAMMIRROR, X
    inx
    lda #$20            ; vhoopppN 00100000 = no h flip, no v flip, priority 2, palette 0, N=0
    sta OAMMIRROR, X
    inx
    ; move other sprites off screen in a loop
OAMLoop:
    stz OAMMIRROR, X    ; x position (0)
    inx
    lda #$e0            ; y position (224)
    sta OAMMIRROR, X
    inx
    stz OAMMIRROR, X    ; name 00
    inx
    stz OAMMIRROR, X    ; vhoopppN 00000000 = no h flip, no v flip, priority 2, palette 0, N=0
    inx
    cpx #OAMMIRROR_SIZE
    bne OAMLoop         ; set every remaining coordinate (and other attributes) to 255

    ; correct extra OAM byte for first four sprites (what?)
    ldx #$0200
    lda #%0000010  ; this sets sprites to larger size - 16x16
    sta OAMMIRROR, X

    ; set initial horizontal and vertical speed
    lda #SPRITE_SPEED
    sta HOR_SPEED
    sta VER_SPEED

    ; make objects visible - and BG1!
    lda #$11
    sta TM
    ; release forced blanking, set screen to full brightness
    lda #$0f
    sta INIDISP
    ; enable NMI, turn on automatic joypad polling
    lda #$81
    sta NMITIMEN
    lda #$c0     ;have the automatic read of the SNES read the first pair of JoyPads
    sta IOPORTW  ;store to io port write rgister

    jmp GameLoop        ; all init is done
.endproc
; ---


; ---
; after reset handler will jump to here
; ---
; .smart ; keep track of registers widths
.proc GameLoop
    wai              ; wait for NMI/V-Blank
    ; Check joypad
Joypad:
    lda JOY1A
    sta JOY1AW
    lda JOY1B
    sta JOY1BW
    ; Check up/down direction
CheckUp:
    lda JOY1AW
    and #JOY_UP
    beq CheckDown
    ; up was pressed....
    lda #$ff        ; -1 means dig up, stupid
    sta VER_SPEED
    lda #TOP_OFFSET
    sta VER_OFFSET
    stz HOR_SPEED   ; only cardinal directions
    lda #LEFT_OFFSET
    sta HOR_OFFSET
    jmp CheckTopWall
CheckDown:
    lda JOY1AW
    and #JOY_DOWN
    beq CheckLeft    ; Down not pressed either,
    ; down pressed
    lda #$01        ; down pressed, dig down
    sta VER_SPEED
    lda #BOTTOM_OFFSET
    sta VER_OFFSET
    stz HOR_SPEED   ; only cardinal directions
    lda #LEFT_OFFSET
    sta HOR_OFFSET
    jmp CheckBottomWall
CheckLeft:
    lda JOY1AW
    and #JOY_LEFT        ; Check left
    beq CheckRight  ; Left no pressed, check the last one
    ; Left pressed
    lda #$ff        ; -1, go left
    sta HOR_SPEED
    lda #LEFT_OFFSET
    sta HOR_OFFSET
    stz VER_SPEED   ; only cardinal directions
    lda #TOP_OFFSET
    sta VER_OFFSET
    jmp CheckLeftWall
CheckRight:
    lda JOY1AW
    and #JOY_RIGHT        ; Check right
    beq CheckExistingHorizontalMovement   ; nothing pressed, done checking buttons
    lda #$01        ; +1, go right
    sta HOR_SPEED
    lda #RIGHT_OFFSET
    sta HOR_OFFSET
    stz VER_SPEED   ; only cardinal directions
    lda #TOP_OFFSET
    sta VER_OFFSET
    jmp CheckRightWall
; Nothing pressed, check existing movement
CheckExistingHorizontalMovement:
    lda HOR_SPEED
    ; if zero do nothing
    beq CheckExistingVerticalMovement
    ; if -1, CheckLeftWall
    bmi CheckLeftWall
    ; if 1, CheckRightWall
    bpl CheckRightWall
CheckExistingVerticalMovement:
    lda VER_SPEED
    ; if zero do nothing
    beq UpdatePosition
    ; if -1 check top wall
    bmi CheckTopWall
    ; if 1 check bottom wall
    bpl CheckBottomWall
CheckLeftWall:
CheckRightWall:
CheckTopWall:
CheckBottomWall:
CheckAgainstBackground:
    rep #$20            ; set A to 16-bit so that we can transfer it to X
    lda OAMMIRROR + $01     ; load y position into A
    clc
    adc VER_SPEED           ; add the veritcal speed to get the new y coordinate
    clc
    adc VER_OFFSET          ; handle top/bottom offset
    and #$00f8
    asl A
    asl A
    asl A
    pha                  ; push A
    lda OAMMIRROR        ; load x position into A
    clc
    adc HOR_SPEED        ; add the horizontal speed to get the new x coordinate
    ; the x position of the sprite is 5 pixels to the left of the character
    ; so we should check for collisions 5 pixels to the left
    clc
    adc HOR_OFFSET             ; handle left/right offset
    and #$00f8
    lsr A                ; Divide
    lsr A                ; by 4 - because we divide y 8 and then double
    clc
    adc $01, S          ; add y index to x index
    ; now A has the offset of the tile
    ; transfer it to x
    tax
    pla                 ; clear up stack
    lda Level1Map, X
    sta BG_COLLISION    ; for debugging
    sep #$20        ; set A back to 8-bit
    ; A now has the lower byte of the background tile. 00 is empty. lower bits on upper byte could be set but we don't use that many
    beq UpdatePosition   ; target tile is empty, go ahed with move
    ; stop
    stz HOR_SPEED
    stz VER_SPEED
UpdatePosition:
    ; game logic: move the sprites
    lda OAMMIRROR           ; load the horizontal position of the first sprite, which is the first byte at OAMMIRROR
    clc                     ; clear carry flag because we'll be adding and want to make sure it's not set
    adc HOR_SPEED           ; Add speed to the x position to get new x position
    sta OAMMIRROR       ; store new x position of sprite


; move sprite 1 vertically
    ; check upper collision boundary
    lda OAMMIRROR + $01     ; load current y position of first sprite
    clc
    adc VER_SPEED
    sta OAMMIRROR + $01     ; store new y position of sprite
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

.proc LoadBG
    phx ; save x
    phd ; create frame pointer
    tsc
    tcd
SetupBGLocations:
    lda #$01
    sta BGMODE   ; Mode 1 (for 16 colors per tile), 8x8 tiles
    lda #$30     ; map starting location in vram ...
    sta BG1TMADD ; becomes $6000 in VRAM
    lda #$00     ; tiles starting location in vram...
    sta BG12CADD ; ...lower 4 bits are for BG1, becomes $0000 in VRAM
LoadTileData:
    ; this happens in LoadVRAM
LoadChrData:
    ldx #$3000        ; we write to both VMADDL and VMADDH with a 16-bit register
    stx VMADDL
    lda #$80          ;
    sta VMAINC        ; increment VRAM address by 1 when writing to VMDATAH
    ldx #$0000
    ; Set a tile
    ; high vhopppcc
    ; low cccccccc
FinishChrBG:
    lda Level1Map, X
    sta VMDATAL
    inx
    lda Level1Map, X
    sta VMDATAH
    inx
    cpx #$0800
    bne FinishChrBG
DoneBG:
    pld; restore frame pointer
    plx; restore x
    rts
.endproc

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
