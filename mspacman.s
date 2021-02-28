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
SCROLL_X    = $0300     ; X offset of scrolling, not used
SCROLL_Y    = $0301     ; Y Offset of scrolling

BG_TILE1    = $0302     ; will be writing the current background tiles here
BG_TILE2    = $0303     ;
PLAYER_DIRECTION    = $0304 ; curent direction of player
TARGET_X1   = $0310
TARGET_Y1   = $0311
TARGET_X2   = $0312
TARGET_Y2   = $0313
BG_TILE1T   = $0320     ; temp
BG_TILE2T   = $0321     ; temp
MISSY_X     = $0330     ; absolute X
MISSY_Y     = $0331     ; absolute Y
OAMMIRROR   = $0400     ; location of OAMRAM mirror in WRAM, $220 bytes long
; ---

; --- Joypad memory locations
JOY1AW      = $0700     ;B, Select, Start, Up, Down, Left, Right
JOY1BW      = $0701     ;A, X, L, R, iiii-ID


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
INITIAL_SCROLL_X= $ef
INITIAL_SCROLL_Y= $10
END_ZONE_TOP    = $10
END_ZONE_BOTTOM = $cf

; Walkable tiles
TILE_BLANK      = $00
TILE_DOT        = $10
TILE_POWER      = $11

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

;    lda #%00000001  ; set up OAM for sprite size andlocation of tiles - they start at $2000
    lda #$01
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
    sta MISSY_X         ; store A into absolute X position
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
    sta MISSY_Y         ; store Y position into absolute Y coordinate
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

.proc GetBGTile
    phx                     ; save old stack pointer
    ; create a frame pointer
    phd                     ; push direct register to stack
    tsc                     ; transfer stack to ...
    tcd                     ; direct register
    ; constants to access args on stack with direct addressing
    Tile         = $07      ; return value
    YPosition    = $08      ; Y position of 16x16 character sprite
    XPosition    = $09      ; X position of 16x16 character sprite

    lda YPosition
    rep #$20            ; set A to 16-bit
    and #$00f8          ; Clear high bits because we only had 8 significant bits. Clear lower 3 because we're shifting those away
    asl A
    asl A
    asl A
    pha                  ; push A
    sep #$20        ; set A back to 8-bit since it comes from the stack
    lda XPosition        ; load x position into A
    rep #$20            ; set A to 16-bit
    and #$00f8
    lsr A                ; Divide
    lsr A                ; by 4 - because we divide y 8 and then double
    clc
    adc $01, S          ; add y index to x index
    ; now A has the offset of the tile
    ; transfer it to x
    tax
    pla                 ; clear up stack
    sep #$20        ; set A back to 8-bit
    ; index into the tile map to get the tile

    lda Level1Map, X
    ; A now has the lower byte of the background tile.
    ; subroutine cleanup and return

    pld                     ; pull back direct register
    plx                     ; restore old stack pointer into x
    rts                     ; return to caller
.endproc

; Direction, XPosition, YPosition (inout)
; based on an initial character position and a direction,
; return the background tile that would be crossed
; Can't just do top-left and bottom right. Edges need to be based on movement direction
.proc GetTargetBGTiles
    ; initial subroutine setup
    phx                     ; save old stack pointer
    ; create a frame pointer
    phd                     ; push direct register to stack
    tsc                     ; transfer stack to ...
    tcd                     ; direct register
    ; constants to access args on stack with direct addressing
    TileTL       = $07      ; return value
    TileBR       = $08      ; return value
    YPosition    = $09      ; Y position of 16x16 character sprite
    XPosition    = $0A      ; X position of 16x16 character sprite
    Direction    = $0B      ; Direction bits from joystick
    OFFSET = $07
ProcessDirection:
    ; The top line of the SNES gets dropped so Y position needs to be of by one to correct
    lda YPosition
    clc
    adc #(PLAYER_OFFSET + 1)
    sta YPosition
    lda XPosition
    clc
    adc #PLAYER_OFFSET
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
    adc #OFFSET
    sta TARGET_X2
    jmp ComputeTile1
CheckDown:
    lda Direction
    and #JOY_DOWN
    beq CheckLeft           ; Down not pressed either,
    ; down pressed
    lda YPosition           ; moving DOWN. start with sprite Y position.
    clc
    adc #(OFFSET + 1)       ; have to consider the bottom edge
    sta TARGET_Y1
    sta TARGET_Y2
    lda XPosition
    sta TARGET_X1
    clc
    adc #OFFSET
    sta TARGET_X2
    jmp ComputeTile1
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
    adc #OFFSET
    sta TARGET_Y2
    jmp ComputeTile1
CheckRight:
    lda Direction
    and #JOY_RIGHT        ; Check left
    beq ComputeTile1       ; Right not pressed, done checking
    ; Right pressed
    lda XPosition        ; moving RIGHT
    clc
    adc #(OFFSET + 1)    ; have to consider the right edge
    sta TARGET_X1
    sta TARGET_X2
    lda YPosition
    sta TARGET_Y1
    clc
    adc #OFFSET
    sta TARGET_Y2
    jmp ComputeTile1
ComputeTile1:
    tsx
    lda TARGET_X1
    pha
    lda TARGET_Y1
    pha                     ; Push Y Position
    lda #$00
    pha                     ; Push Tile1
    jsr GetBGTile           ;
    pla                     ; Pull calculated tile out
    sta TileTL              ; save this as top-left tile
    sta BG_TILE1T
    txs                     ; restore stack pointer
ComputeTile2:
    tsx
    lda TARGET_X2
    pha
    lda TARGET_Y2
    pha
    lda #$00
    pha
    jsr GetBGTile
    pla
    sta TileBR
    sta BG_TILE2T
    txs
    ; subroutine cleanup and return
ReturnFromGetTargetBGTile:
    ; all done
    pld                     ; pull back direct register
    plx                     ; restore old stack pointer into x
    rts                     ; return to caller
.endproc

.proc MovePlayer
MoveCheckUp:
    lda PLAYER_DIRECTION
    and #JOY_UP
    beq MoveCheckDown
    ; up was pressed....
    lda MISSY_Y
    dec
    sta MISSY_Y
    rts
MoveCheckDown:
    lda PLAYER_DIRECTION
    and #JOY_DOWN
    beq MoveCheckLeft
    lda MISSY_Y
    inc
    sta MISSY_Y
    rts
MoveCheckLeft:
    lda PLAYER_DIRECTION
    and #JOY_LEFT
    beq MoveCheckRight
    lda MISSY_X
    dec
    sta MISSY_X
    rts
MoveCheckRight:
    lda PLAYER_DIRECTION
    and #JOY_RIGHT
    beq MoveCheckDone
    lda MISSY_X
    inc
    sta MISSY_X
MoveCheckDone:
    rts
.endproc

; Direction, XPosition, YPosition (inout)
; Tile 1 id, Tile 2 id, CanMove (return)
; Check if both tiles are wokkable
; return 1 if wokkable, 0 if not.
; Might do static scoring in here too eventually. That's a hack but can get it done
; Can't just do top-left and bottom right. Edges need to be based on movement direction
.proc CheckWokkable
    .byte $42, $00
    ; initial subroutine setup
    phx                     ; save old stack pointer
    ; create a frame pointer
    phd                     ; push direct register to stack
    tsc                     ; transfer stack to ...
    tcd                     ; direct register

    ; constants to access args on stack with direct addressing
    CanMove      = $07      ; return value
    Tile2        = $08      ; Tile 2
    Tile1        = $09      ; Tile 1
CheckTile1Wokkable:
    lda Tile1       ; Check the top 4 bits of tile 1
    and #$f0
    bne NotWokkable
CheckTile2Wokkable:
    lda Tile2       ; Check the top 4 bits of tile 2
    and #$f0
    bne NotWokkable
Wokkable:
    lda #$01
    sta CanMove
    jmp FinishCheckWokkable
NotWokkable:
    ; If non-zero, this is a wall, no move
    ; tile was non-zero. this is a wall, don't move
    stz CanMove
    ; all done
FinishCheckWokkable:
    pld                     ; pull back direct register
    plx                     ; restore old stack pointer into x
    rts                     ; return to caller
.endproc

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
    ; Check if any direction currently pressed
;    lda JOY1AW         ; This is already in A, so can be skipped
    and #$0f           ; B, Select, Start, Up, Down, Left, Right
    beq CheckDirection ; no direction held, skip part 1
HandleActiveJoypadInput:
    ; Joypad direction currently held. Try that movement first
    ; Get ready to call GetTargetCoordinate
    tsx     ; save current stack pointer before pushing things for subroutine
    lda JOY1AW
    pha     ; push the direction
    lda MISSY_X             ; Get current X position
    pha                     ; push it onto the stack before call
    lda MISSY_Y             ; Get current Y Position
    pha                     ; push it onto the stack before call
    ; now I push 00 twice. Could do a pea $0000 instead but i'll leave it here for consistency
    lda #$00
    pha                     ; tile 1 return value
    lda #$00
    pha                     ; tile 2 return value
    jsr GetTargetBGTiles
    pla
    ; Check if tile empty
    sta BG_TILE1
    pla
    sta BG_TILE2
    txs                     ; restore stack pointer to before the call
    ; Get ready to call CheckWokkable - need Tile1, Tile2, and a return value
    ; This is very repetitious with the above cleanup, easy optimization later
    tsx
    lda BG_TILE1
    pha
    lda BG_TILE2
    pha
    lda #$00
    pha
    jsr CheckWokkable
    pla                 ; now wokkable is in a. 01 = wokkable, 00 = not
    txs         ; restore stack pointer
    beq CheckDirection      ; Joypad attempted to move us into a wall. ignore that action and try to process existing movement
    ; at this point, joypad movement is good!
    lda JOY1AW              ; store existing joypad movement ...
    sta PLAYER_DIRECTION    ; ... into PLAYER_DIRECTION
    jsr MovePlayer          ; move the player according to PLAYER_DIRECTION
    ; at this point, player has been moved
    jmp FinishMovePlayer
CheckDirection:
    tsx         ; save current stack pointer
    lda PLAYER_DIRECTION ; load last good direction
    pha
    lda MISSY_X       ; current X position
    pha
    lda MISSY_Y       ; current Y position
    pha
    pea $0000           ; 2 bytes for target tiles
    jsr GetTargetBGTiles
    pla                 ; tile 1
    sta BG_TILE1
    pla
    sta BG_TILE2
    txs         ; restore stack pointer

    ; Get ready to call CheckWokkable - need Tile1, Tile2, and a return value
    ; This is very repetitious with the above cleanup, easy optimization later
    tsx
    lda BG_TILE1
    pha
    lda BG_TILE2
    pha
    lda #$00
    pha
    jsr CheckWokkable
    pla                 ; now wokkable is in a. 01 = wokkable, 00 = not
    txs         ; restore stack pointer
    beq FinishMovePlayer      ; Joypad attempted to move us into a wall. ignore that action and try to process existing movement
    ; at this point, existing movement is good
    jsr MovePlayer
    ; at this point, player has been moved
FinishMovePlayer:
HandleScroll:
    ; Check absolute position of missy (MISSY_Y)
    ; if below the top end-zone, don't change scroll
CheckEndZoneTop:
    lda MISSY_Y
    clc
    cmp #(END_ZONE_TOP + 6)
    bcs CheckEndZoneBottom      ; if we're above the top end zone, check the bottom
    ; handle top end zone
    ; TODO: compute the scroll offset - should be between $00 (0) and $10 (16)
    lda MISSY_Y
    ; subtract ... 6?
    clc
    adc #$fa            ; -6 in 2's-c
    sta SCROLL_Y
    jmp TranslatePlayerCoordinates
CheckEndZoneBottom:
    clc
    cmp #END_ZONE_BOTTOM
    bcc HandleMiddleZone  ; we're not yet in the bottom end zone
    ; handle bottom end zone
    ; compute the scroll offset
    ; SCROLL_Y = MISSY_Y - $bf
    lda MISSY_Y
    clc
    adc #$41     ; 2's complement of $bf - helps to use #$ (immediate) instead of $ (memory address)
    ; TODO: Limit the scroll position
    sta SCROLL_Y
    jmp TranslatePlayerCoordinates
    ; if above the bottom end zone, don't change scroll
    ; if between the end zones, update scroll offset
HandleMiddleZone:
    ; In the middle zone, scroll offset should be Y
    lda #INITIAL_SCROLL_Y
    sta SCROLL_Y
    ; Translate absolute position (MISSY_X, MISSY_Y) to relative coordinates
TranslatePlayerCoordinates:
    lda SCROLL_Y
    eor #$ff
    clc
    inc
    pha                 ; push it to stack, we will add it here
    lda MISSY_Y
    clc
    adc $01, S          ; the negated initial scroll Y is on the stack
                        ; To get the sprite position, need to subtract INITIAL_SCROLL_Y
    sta OAMMIRROR + $01 ; store the offset Y position into screen coordinates
    pla
    lda SCROLL_X
    eor #$ff
    clc
    inc
    pha
    lda MISSY_X
    clc
    adc $01, S
    sta OAMMIRROR
    pla
    jmp GameLoop
.endproc
; ---

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
