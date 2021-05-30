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
BG1HSCROLL  = $210D     ; BG1 Horizontal Scroll
BG1VSCROLL  = $210E     ; BG1 Vertical Scroll
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

PLAYER1_X   = $0300     ; absolute X
PLAYER1_Y   = $0301     ; absolute Y
TARGET_X1   = $0302     ; x1 coordinate of where player will move
TARGET_Y1   = $0303     ; y1 coordinate of where player will move
TARGET_X2   = $0304     ; x2 coordinate of where player will move
TARGET_Y2   = $0305     ; y2 coordinate of where player will move
BG_TILE1_IDX= $0306     ; tile map index of x1/y1
BG_TILE2_IDX= $0307     ; tile map index of x2/y2
BG_IS_WALL  = $0308     ; 1 if either or both BG_TILE#_IDX points to a wall tile
BG_TILE1    = $0309     ; will be writing the current background tiles here
BG_TILE2    = $030A     ;
SCROLL_X    = $0310     ; X offset of scrolling, not used
SCROLL_Y    = $0311     ; Y Offset of scrolling

OAMMIRROR   = $0400     ; location of OAMRAM mirror in WRAM, $220 bytes long
; ---

; --- Joypad memory locations
JOY1DIR      = $0700     ; Active Joypad 1 Up, Down, Left, Right
MOM1DIR      = $0701     ; Momentum 1 Up, Down, Left, Right

; -- Player coordinates

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
STARTING_Y      = $B3
PLAYER_OFFSET   = $04

; --- Scrolling-related
INITIAL_SCROLL_X= $ef
INITIAL_SCROLL_Y= $10
END_ZONE_TOP    = $10
END_ZONE_BOTTOM = $cf

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
    pea $000A       ; push count of bytes (8 / $08) to transfer to stack
    jsr LoadCGRAM   ; transfer color data into CGRAM
    txs             ; "delete" data on stack by restoring old stack pointer

    ; load sprite palette to CGRAM
    tsx             ; save current stack pointer (is this a no-op from before?)
    lda #$80        ; push CGRAM destination address to stack
    pha             ; through A (why not pea? - guess because that's 2 bytes and we only want 1?)
    pea MsPacmanPalette   ; Push paletes source address to stack
    pea $0020       ; push count of bytes (32 / $20) to transfer to stack
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
    pea $0400       ; count of bytes to transfer
    jsr LoadVRAM    ; transfer data in subroutine
    txs             ; "delete" data on stack by restoring old stack pointer

    ; load background
    tsx
    jsr LoadBG
    txs

    ; clear other memory locations
    stz MOM1DIR     ; this only gets copied from JOY1DIR when there is joypad input, must be initialized to 0
    stz BG_TILE1
    stz BG_TILE2
    stz TARGET_X1
    stz TARGET_Y1
    stz TARGET_X2
    stz TARGET_Y2
    stz BG_TILE1_IDX
    stz BG_TILE2_IDX
    stz BG_IS_WALL

    lda #$01         ; set up OAM for sprite size andlocation of tiles - they start at $2000
    sta OBJSEL       ; $2101 Object size $ object data area designation

    ; set up initial data in OAMRAM mirror, using X as index
    ldx #$00
    ; upper-left sprite, starts at halfway point
    ; need to get INITIAL_SCROLL_X somewhere we can add it, will put it on stack
    clc
    lda #INITIAL_SCROLL_X
    eor #$ff
    clc
    inc
    pha                 ; push it to stack, we will add it here
    lda #STARTING_X     ; starting X position for player
    sta PLAYER1_X       ; store A into absolute X position
    clc
    adc $01, S
    sta OAMMIRROR, X    ; store A into OAMMIRROR memory location, offset by X (like c pointer arithmetic offset)
    pla
    inx                 ; increment index
    ; need to get INITIAL_SCROLL_Y somewhere we can add it, will put it on stack
    clc
    lda #INITIAL_SCROLL_Y
    eor #$ff
    clc
    inc
    pha                 ; push it to stack, we will add it here
    lda #STARTING_Y     ; starting Y position for player (absolute)
    sta PLAYER1_Y       ; store Y position into absolute Y coordinate
    clc
    adc $01, S          ; the negated initial scroll Y is on the stack
                        ; To get the sprite position, need to subtract INITIAL_SCROLL_Y
    sta OAMMIRROR, X
    pla                 ; pop the arg off the stack
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
BeginProcessPlayerDirection:
    jsr ReadJoypad1  ; read joypad1 bits into JOY1DIR (and A)
    ; Check if anything pressed
    beq ProcessPlayerMomentumDirection
ProcessPlayerJoypadDirection:
    ; something pressed, check that out
    tsx
    lda JOY1DIR       ; push active direction onto the stack
    pha
    lda PLAYER1_X     ; push x coordinate onto the stack
    pha
    lda PLAYER1_Y     ; push y coordinate onto the stack
    pha
    jsr HandlePlayerDirection
    txs
    ; no way to avoid checking the tiles twice but having a subroutine does help with momentum
    ; above subroutine sets HandlePlayerDirection based on JOY1DIR
    ; TARGET_X1, TARGET_Y1, TARGET_X2, TARGET_Y2, BG_TILE1_IDX, BG_TILE2_IDX
    ; Check BG_TILE1_IDX, BG_TILE2_IDX to see if they point to wall tiles
    jsr CheckBGTilesAreWall
    lda BG_IS_WALL
    bne ProcessPlayerMomentumDirection  ; if non-zero, joypad movement headed player into a wall, ignore it
CopyJoypadToMomentum:                   ; Joypad movement doesn't take player into a wall, copy it to momentum
    lda JOY1DIR
    sta MOM1DIR
    jmp EndProcessPlayerDirection       ; Just set MOM1DIR and have checked movement. OK to move...
    ; if so, update MOM1DIR and jump to move player
    ; Nothing pressed
ProcessPlayerMomentumDirection:
    ; Check Momentum
    lda MOM1DIR
    beq EndMovePlayer ; no momentum, don't bother moving or checking
    tsx               ; prep for call to HandlePlayerDirection
    lda MOM1DIR       ; push momentum direction onto the stack
    pha
    lda PLAYER1_X     ; push x coordinate onto the stack
    pha
    lda PLAYER1_Y     ; push y coordinate onto the stack
    pha
    jsr HandlePlayerDirection     ; A contains momentum
    txs
    jsr CheckBGTilesAreWall       ; Sets BG_IS_WALL based on momentum
    lda BG_IS_WALL
    beq BeginMovePlayer     ; momentum did not move us into a wall, handle movement
StopPlayerMovement:
    stz MOM1DIR
    jmp EndMovePlayer
EndProcessPlayerDirection:
BeginMovePlayer:
    ; Movement is good to go. read MOM1DIR and move player
    ; read MOM1DIR and move player
EndMovePlayer:
    jmp GameLoop
.endproc
; ---

; Check tiles at BG_TILE1_IDX and BG_TILE2_IDX to see if player can enter them
; If yes, sets BG_IS_WALL = 0
; If not, sets BG_IS_WALL = 1.

; implementation here is pretty lazy
; could instead slide up all those bits into one byte and just see if it's all zero

.proc CheckBGTilesAreWall
    lda BG_TILE1_IDX
    rep #$20            ; set A to 16-bit
    and #$00ff          ; clear high bits
    tax
    sep #$20            ; set A back to 8-bit
    lda Level1Map, X
    ; now tile 1 is in a
    sta BG_TILE1
    lda BG_TILE2_IDX
    rep #$20            ; set A to 16-bit
    and #$00ff          ; clear high bits
    tax                 ; transfer to X because X is an index register used for offset from Level1Map
    sep #$20        ; set A back to 8-bit
    lda Level1Map, X
    ; now tile 2 is in A
    sta BG_TILE2
    and #$f0        ; Check the top 4 bits of tile 2
    bne FoundWall   ; if any of the top 4 bits are set, that tile is a wall tile
    lda BG_TILE1
    and #$f0        ; Check the top 4 bits of tile 1
    bne FoundWall   ; if any of the top 4 bits are set, that tile is a wall tile
    ; At this point, checked both BG_TILE#_IDX and found no wall
    stz BG_IS_WALL
    jmp EndCheckBGTilesAreWall
FoundWall:
    lda #$01
    sta BG_IS_WALL
EndCheckBGTilesAreWall:
    rts
.endproc


; ---
; Load controller 1, store into JOY1DIR
.proc ReadJoypad1
    lda JOY1A       ; B, Select, Start, Up, Down, Left, Right
    and #$0f        ; Only interested in direction
    sta JOY1DIR
    rts
.endproc

; ---
; Given X/Y position and direction (passed on stack)
; Set globals TARGET_X1, TARGET_Y1, TARGET_X2, TARGET_Y2, BG_TILE1_IDX, BG_TILE2_IDX
; (global, player-based locations)

; Inputs: Direction
.proc HandlePlayerDirection
    phx                     ; save old stack pointer
    ; create a frame pointer
    phd                     ; push direct register to stack
    tsc                     ; transfer stack to ...
    tcd                     ; direct register
    ; stack offsets for params
    YPosition     = $07
    XPosition     = $08
    Direction     = $09
CheckDirection:
    ; Check each direction
    ; compute target coordinate

    ; -- constants for edge detection
    CHAR_OFFSET  = $04   ; how much to add to player coordinate to figure top-left corner for edge detection
    EDGE_OFFSET  = $07   ; how much to add to above offset coordinate when calculating other corners for edge detection
    ; The top line of the SNES gets dropped so Y position
    ; needs to be of by one to correct. This was put here
    ; before I implemented scrolling so might work better there.
    lda YPosition
    clc
    adc #(CHAR_OFFSET + 1)
    sta YPosition
    lda XPosition
    clc
    adc #CHAR_OFFSET
    sta XPosition
CheckUp:
    lda Direction
    and #JOY_UP
    beq CheckDown
    ; up was pressed....
    lda YPosition           ; moving UP, start with sprite Y position.
    dec                     ; just consider the top edge
    sta TARGET_Y1
    sta TARGET_Y2
    lda XPosition
    sta TARGET_X1
    clc
    adc #EDGE_OFFSET
    sta TARGET_X2
    jmp EndCheckDirection
CheckDown:
    lda Direction
    and #JOY_DOWN
    beq CheckLeft           ; Down not pressed either,
    ; down pressed
    lda YPosition           ; moving DOWN. start with sprite Y position.
    clc
    adc #(EDGE_OFFSET + 1)       ; have to consider the bottom edge
    sta TARGET_Y1
    sta TARGET_Y2
    lda XPosition
    sta TARGET_X1
    clc
    adc #EDGE_OFFSET
    sta TARGET_X2
    jmp EndCheckDirection
CheckLeft:
    lda Direction
    and #JOY_LEFT        ; Check left
    beq CheckRight  ; Left no pressed, check the last one
    ; Left pressed
    lda XPosition        ; moving LEFT. start with sprite X position.
    dec                  ; just consider the left edge
    sta TARGET_X1
    sta TARGET_X2
    lda YPosition
    sta TARGET_Y1
    clc
    adc #EDGE_OFFSET
    sta TARGET_Y2
    jmp EndCheckDirection
CheckRight:
    lda Direction
    and #JOY_RIGHT        ; Check left
    beq EndCheckDirection       ; Right not pressed, done checking
    ; Right pressed
    lda XPosition        ; moving RIGHT
    clc
    adc #(EDGE_OFFSET + 1)    ; have to consider the right edge
    sta TARGET_X1
    sta TARGET_X2
    lda YPosition
    sta TARGET_Y1
    clc
    adc #EDGE_OFFSET
    sta TARGET_Y2
EndCheckDirection:
    ; At this point, TARGET_[X|Y][1|2] should be current
    ; compute target tile locations
    ; have TARGET_X1 and Y1
    ; call GetBGTileIdx for X1/Y1
    tsx
    lda TARGET_X1
    pha
    lda TARGET_Y1
    pha
    lda #$00      ; blank byte for return value
    pha
    jsr GetBGTileIdx
    ; pull a back and store it to BG_TILE1_IDX
    pla
    sta BG_TILE1_IDX
    txs

    ; call GetBGTileIdx  for X2/Y2
    tsx
    lda TARGET_X2
    pha
    lda TARGET_Y2
    pha
    lda #$00      ; blank byte for return value
    pha
    jsr GetBGTileIdx
    ; pull a back and store it to BG_TILE2_IDX
    pla
    sta BG_TILE2_IDX
    txs

    ; Now BG_TILE1_IDX and BG_TILE2_IDX are set

EndHandlePlayerDirection:
    ; cleanup and return
    pld                     ; pull back direct register
    plx                     ; restore old stack pointer into x
    rts                     ; return to caller
.endproc

.proc GetBGTileIdx
    phx                     ; save old stack pointer
    ; create a frame pointer
    phd                     ; push direct register to stack
    tsc                     ; transfer stack to ...
    tcd                     ; direct register

    ; constants to access args on stack with direct addressing
    TileIdx      = $07      ; return value
    YPosition    = $08      ; Y position of 16x16 character sprite
    XPosition    = $09      ; X position of 16x16 character sprite

    lda YPosition
    and #$f8                ; Clear lower 3 because we're shifting those away
    asl A
    asl A
    asl A
    pha                  ; push A
    lda XPosition        ; load x position into A
    and #$f8
    lsr A                ; Divide
    lsr A                ; by 4 - because we divide y 8 and then double
    clc
    adc $01, S          ; add y index to x index
    ; now A has the offset of the tile
    sta TileIdx
    pla                 ; Pull off and discard pushed y-position so that stack is back on track, jack.

    ; subroutine cleanup and return
    pld                     ; pull back direct register
    plx                     ; restore old stack pointer into x
    rts                     ; return to caller
.endproc

; 2021-03-13 old code below here

; ---
; called during v-blank every frame
; ---
.proc   NMIHandler
        lda RDNMI           ; read NMI status, acknowledge NMI

        ; Update the scroll offset - does this need to happen during v-blank?
        lda SCROLL_Y
        sta BG1VSCROLL  ; write low byte
        stz BG1VSCROLL  ; write high byte

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
LoadScrollOffset:
    ; just vertical scrolling
    lda #INITIAL_SCROLL_Y
    sta SCROLL_Y
    sta BG1VSCROLL  ; write low byte
    stz BG1VSCROLL  ; write high byte
    lda #INITIAL_SCROLL_X
    sta SCROLL_X
    sta BG1HSCROLL
    stz BG1HSCROLL
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
