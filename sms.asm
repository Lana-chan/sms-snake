;----------------------------------------
; Master System test game
; Svetlana 2014
;----------------------------------------

;----------------------------------------
; Macros
;----------------------------------------

.DEF VDPDATA $BE
.DEF VDPCTRL $BF
.DEF IOH $DC
.DEF IOL $DD

;.DEF K1UP %00000001
;.DEF K1DN %00000010
;.DEF K1LT %00000100
;.DEF K1RT %00001000
.DEF K1TL %00010000
.DEF K1TR %00100000

.DEF DIRUP %00000001
.DEF DIRDN %00000010
.DEF DIRLT %00000100
.DEF DIRRT %00001000
.DEF DIRVR %00000011
.DEF DIRHR %00001100

.DEF CHEADV $30
.DEF CHEADH $31
.DEF CTAILV $32
.DEF CTAILH $33
.DEF VFLIP %00000100
.DEF HFLIP %00000010

; Writes registers or addresses to VDP $BF
.MACRO vdpreg ARGS nb
    ld a,<nb    ;high byte
    out (VDPCTRL),a
    ld a,>nb    ;low byte
    out (VDPCTRL),a
.ENDM

; Writes byte at VDP's address to $BE
.MACRO vdpbyt ARGS nb
    ld a,(nb)
    out (VDPDATA),a
.ENDM

;----------------------------------------
; Init
;----------------------------------------

; Declares ROM and RAM mapping
.MEMORYMAP
  DEFAULTSLOT 0
  SLOTSIZE $4000 ;16k
  SLOT 0 $0000 ;ROM
  SLOT 1 $4000
  ;SLOT 2 $8000
  SLOTSIZE $2000 ;8k
  SLOT 2 $C000 ;RAM
  SLOT 3 $E000
.ENDME

; Declares ROM banks
.ROMBANKMAP
  BANKSTOTAL 2 ; 4 * 16k = 64k
  BANKSIZE $4000
  BANKS 2
.ENDRO

; RAM space for variables
.STRUCT pos
  x     db
  y     db
.ENDST

.RAMSECTION "Variables" SLOT 2
  lpcnt db
  count db
  inp   db
  dir   db
  cdir  ds 2
  tdir  ds 256
  len   db
  head INSTANCEOF pos
  tail INSTANCEOF pos
  food INSTANCEOF pos
.ENDS

; Tags, checksum
.SDSCTAG 1.0, "SMS Test", "", "Svetlana 2014"

; Tiles
;.BANK 2 SLOT 2
.BANK 1 SLOT 1
.ORG 0 ; Offset
gfx:
  .INCBIN "picture.sms" READ 8192

.BANK 0 SLOT 0
.ORG 0
;jp init ; Jumps to main init code
;.ORG $0038 ; Not sure
;RETI ; Interrupt return

init:
  vdpreg $8100 + $1<<6 ; Reg $01 - Enable display (bit 6)
  vdpreg $c001     ; Palette position 1
  vdpbyt %00111111 ; White
  vdpbyt %00000010 ; Red 1
  vdpbyt %00000011 ; Red 2
  vdpbyt %00101011 ; Red 3
  vdpbyt %00001001 ; Green 1
  vdpbyt %00001110 ; Green 2
  vdpbyt %00101110 ; Green 3
  vdpreg $82ff ; Reg $02 - Sets VRAM to $3800
  vdpreg $8808 ; Reg $08 - Scroll to 08, leftmost
  
;----------------------------------------
; CHR Load
;----------------------------------------
  
;  vdpbf $4000 ; Point to VRAM $4000
;  ld HL,gfx   ; Set address to graphics
;  ld c,$be ; Set destination to $BE
;  ld b,255 ; OTIR loop length
;  ld a,8   ; Repeat counter, VRAM = 8 * 255
;  ld (num),a ; Store counter into RAM
;copy:
;  otir       ; Runs OTIR copy loop
;  ld b,255   ; Reset loop length
;  ld a,(num) ; Load counter from RAM
;  dec a      ; Decrease counter
;  ld (num),a ; Store counter back into RAM
;  jp nz,copy ; If counter isn't 0, repeat OTIR

  vdpreg $4000 ; Point to VRAM $4000
  ld HL,gfx    ; Set address to graphics
  ld c,$be ; Set destination to $BE
.REPT 8
  ld b,255 ; OTIR loop length
  otir     ; Runs OTIR copy loop
.ENDR

;----------------------------------------
; Main
;----------------------------------------
  
  ;ld a,20      ; 60 frames = 1 second (if 60hz machine)
  ;ld (frame),a ; Store framecounter in RAM
  ;ld a, delay
  ;ld (count),a

.DEF DELAY 5

gameinit:
  ld a,10       ; Start at x,y = 10
  ld (head.x),a ; Store in RAM
  ld (head.y),a
  ld a,4        ; Tail length starts at 2
  ld (len),a    ; Store in RAM
gameloop:
  ;ld hl,head.x ; Load X from RAM
  ;ld d,(hl)    ; x = RAM
  ;ld hl,head.y ; Load Y from RAM
  ;ld e,(hl)    ; y = RAM
  ;call adrxy   ; Sets VRAM address to X, Y position
  ;---
  ;ld hl,str_empty ; Load string address
  ;call write      ; Print to nametable
  ;---
  call delayframe
  ;---
  ;ld hl,head.x ; Load X from RAM
  ;ld d,(hl)    ; x = RAM
  ;ld hl,head.y ; Load Y from RAM
  ;ld e,(hl)    ; y = RAM
  ;call adrxy   ; Sets VRAM address to X, Y position
  ;---
  ;ld hl,str_msg ; Load string address
  ;call write    ; Print to nametable
  call drawsnake
  ;---
  call vblank ; Wait for VBlank
  ;---
  jp gameloop    ; Main loop

gameover:
  jp gameover
  
vblank:
  call control   ; Polls for input
  in a,(VDPCTRL) ; Read VDP control port
  and $1<<7      ; Filter to bit 7 (VBlank)
  jp z,vblank    ; In case of 0, VBlank hasn't happened
  ret            ; Return in case of VBlank

delayframe:
  ld a,(count)   ; Load delay counter
  or 0           ; Affect zero flag
  jp z,logic     ; Jumps to logic if counter reached zero
  dec a          ; Decreases counter otherwise
  ld (count),a   ; Saves counter to RAM
  ret
  
logic:
  ;--- Reset frame delay
  ld a,DELAY     ; Set A to delay
  ld (count),a   ; Save to RAM
  ;--- Shift tail stack
  ld hl,tdir     ; Set HL to tail stack address
  ld d,0         ; Load length as 16bit in DE
  ld a,(len)
  ld e,a
  add hl,de      ; Move to stack end
  ld b,d         ; Copy length to BC
  ld c,e
  ld d,h         ; DE is one byte after HL
  ld e,l
  dec hl
  lddr           ; Load loop, this will shift stack
  ;--- Update direction
  ld a,(inp)     ; Load input buffer
  cp DIRUP       ; Check against direction
  jp z,lgiup     ; Jump in case of true
  cp DIRDN
  jp z,lgidn
  cp DIRLT
  jp z,lgilt
  cp DIRRT
  jp z,lgirt
  jp lgiend
lgiup:
  ld b,DIRDN     ; Store opposite direction in B
  jp lgichk
lgidn:
  ld b,DIRUP
  jp lgichk
lgilt:
  ld b,DIRRT
  jp lgichk
lgirt:
  ld b,DIRLT
lgichk:
  ld a,(dir)     ; Loads current direction
  cp b           ; Checks if it's not the opposite
  jp z,lgiend
  ld a,(inp)     ; If it's not, replace direction with new
  ld (dir),a
lgiend:
  ld a,0         ; Clear input
  ld (inp),a
  ;--- Move head position
  ld a,(dir)     ; Load direction from RAM
  cp DIRUP       ; Check against direction
  jp z,lgup      ; Jump in case of true
  cp DIRDN
  jp z,lgdn
  cp DIRLT
  jp z,lglt
  cp DIRRT
  jp z,lgrt
  jp lendmove
lgup:
  ld a,(head.y)  ; Load head position
  dec a          ; Move position
  ld (head.y),a  ; Store back in RAM
  ld a,DIRDN     ; Set B to opposite direction
  ld d,CHEADV    ; Tile to use
  ld e,0         ; Tile status
  jp lendmove    ; Don't check for other directions
lgdn:
  ld a,(head.y)
  inc a
  ld (head.y),a
  ld a,DIRUP
  ld d,CHEADV
  ld e,VFLIP
  jp lendmove
lglt:
  ld a,(head.x)
  dec a
  ld (head.x),a
  ld a,DIRRT
  ld d,CHEADH
  ld e,0
  jp lendmove
lgrt:
  ld a,(head.x)
  inc a
  ld (head.x),a
  ld a,DIRLT
  ld d,CHEADH
  ld e,HFLIP
lendmove:
  ld (tdir),a    ; Save opposite direction to top of tail queue
  ld (cdir),de   ; Save what tile to place
  ret

control:
  in a,(IOH)    ; Read IO
  cpl           ; Invert bits (input is low-enable)
  and %00001111 ; Mask directional keys
  jp z,cnend    ; Don't erase buffer if there was no input
  ld (inp),a    ; Store input buffer
cnend:
  ret

; Load DE as XY position in nametable address
adrxy:
  ld a,e  ; Y
  sla a   ; * 64
  sla a
  sla a
  sla a
  sla a
  sla a
  add a,d ; +X * 2
  add a,d
  out (VDPCTRL),a ; High byte
  ld a,e ; Y
  srl a  ; / 4
  srl a
  add a,$78 ; VRAM position ($3800) with write bit enabled
  out (VDPCTRL),a ; Low byte
  ret

drawsnake:
  ld a,(len)    ; Load length
  ld b,a        ; Sets B as length for tail loop
  ld a,(head.x) ; Copy head X and Y to tail
  ld (tail.x),a
  ld a,(head.y)
  ld (tail.y),a
  ld hl,tdir    ; Set DE as stack address
  or 0          ; Check for 0
  jp nz,tailloop
  ret
tailloop:
  ld a,(hl)     ; Pull from stack
  cp DIRUP      ; Check direction
  jp z,tlup
  cp DIRDN
  jp z,tldn
  cp DIRLT
  jp z,tllt
  cp DIRRT
  jp z,tlrt
  jp tlnoerase  ; Escape case, didn't find a direction
tlup:
  ld a,(tail.y) ; Move tail end position
  dec a
  ld (tail.y),a
  jp tlchk
tldn:
  ld a,(tail.y)
  inc a
  ld (tail.y),a
  jp tlchk
tllt:
  ld a,(tail.x)
  dec a
  ld (tail.x),a
  jp tlchk
tlrt:
  ld a,(tail.x)
  inc a
  ld (tail.x),a
tlchk:
  ;ld a,(head.x) ; Load head x into D
  ;ld d,a
  ;ld a,(tail.x) ; Load tail x into A
  ;cp d          ; Compare
  ;jp nz,tlend   ; Exits if not equal
  ;ld a,(head.y) ; Load head y into D
  ;ld d,a
  ;ld a,(tail.x) ; Load tail y into A
  ;cp d          ; Compare
  ;jp z,gameover ; Game over if equal
tlend:
  inc hl
  ;dec b         ; Loops until B is 0
  ;jp nz,tailloop
  djnz tailloop
  ld hl,tail.x ; Set HL to RAM address where x position is
  ld d,(hl)    ; Copy RAM to D
  ld hl,tail.y ; Set HL to RAM address where y position is
  ld e,(hl)    ; Copy RAM to E
  call adrxy   ; Sets VRAM address to X, Y position
  vdpbyt 0     ; Draw blank tile
tlnoerase:
  ld hl,head.x ; Set HL to RAM address where x position is
  ld d,(hl)    ; Copy RAM to D
  ld hl,head.y ; Set HL to RAM address where y position is
  ld e,(hl)    ; Copy RAM to E
  call adrxy   ; Sets VRAM address to X, Y position
  ld de,(cdir) ; Tile to draw
  vdpbyt d     ; Draw tile
  vdpbyt e
  ret

; Writes string from address HL to nametable
write:
  ld a,(hl)       ; Load one character
  sub 47          ; Offset between CHR and ASCII
  jp z,write_end  ; ASCII 47 terminates string
  out (VDPDATA),a ; Write to nametable
  vdpbyt 0        ; Nametable takes 2 bytes (Replace with 4 for upside-down)
  inc hl          ; Next character
  jp write        ; Loop
write_end:
  ret

.ASCIITABLE
MAP 'A' TO 'Z' = 0
MAP ' ' = '@'
MAP ',' = '>'
MAP '!' = ';'
.ENDA

str_msg:
  ;.db "HELLO@WORLD>@I@LIVE;/"
  .ASC "HELLO WORLD, I LIVE!/"
str_empty:
  ;.db "@@@@@@@@@@@@@@@@@@@@/"
  .ASC "                    /"