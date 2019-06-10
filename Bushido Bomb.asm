;Bushido Bomb
;This a simple 2 player platform fighter. Its a hobby, open source, home brew project made to run on the nes. 
;The objective is to die the least and the first player to get 9 deaths loose the match. 
;You die in one hit, the idea came from the game Bushido Blade on the PS1. 
;The player’s are bombs and you attack each other with a lit torch. 
;The players and inside jokes on the map the pits are based off the video game website GientBomb. 
;
;Controls:
;a: jump
;b: attack
;start: pause
;select: returns to main menu when in 
;left/right: move
;down + attack: block
;down + jump: fall though platform
;pressing attack or block before hitting a wall ill make you bounce off the wall if you already have momentum.
;
;todo:
;Sound
;Graphical effect enhancements
;
; iNES header
;
.segment "HEADER"

INES_MAPPER = 0 ; 0 = NROM
INES_MIRROR = 1 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 0 ; 1 = battery backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A ; ID
.byte $02 ; 16k PRG chunk count
.byte $01 ; 8k CHR chunk count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding

;==oma==
; sprite OAM data to be uploaded by DMA
nesMaxSpriteCount = 256

.segment "OAM"
oam: .res nesMaxSpriteCount


;
; vectors placed at top 6 bytes of memory area
;
.segment "VECTORS"
.word nmi
.word reset
.word irq


;=== zero page and macros ===
.segment "ZEROPAGE"
nmi_lock: .res 1 ; prevents NMI re-entry
nmi_ready: .res 1 ; set to 1 to push a PPU frame update, 2 to turn rendering off next NMI
colorPalletSize = 32
colorPallet: .res  colorPalletSize
nmiScreenShakeState: .res 1


;rng
rngIndex: .res 1
rngTmp: .res 1 ;used to store x value

;pointers
tmpPointer: .res 2

;temp variables
tmpVarZp: .res 1
tmpVar2Zp: .res 1

;drawing
hideSprite = $FF
showSprite = $0

;main menu cursor
cursorSpriteIndex = $D0
cursorPauseAmount = 10 ;in frames
cursorX = 50
cursorY: .res 1
cursorIndex: .res 1
cursorPauseNewScreen = 30

.enum cursorEnum
	TotallyTall
	PitPlat
	BombBasement
	sizeMax
.endenum


;blink state drawing
blinkTest = %00000010
blinkTestSlow = %01000000
blinkState: .res 1
blinkSwapColor = $38 ;blink this color
blinkSwapColorNew = $25

;sin table drawing
sinTableIndex: .res 1
screenShakeTimer: .res 1
screenShakeMask = %00000111
screenShakeTimerAmount = 10

;PlayerDraw vars
spritePointer: .res 2
;these below are temps and are only used in the drawplayer func
yAxisOAM: .res 1 ;keeps what row we are drawing
xAxisOAM: .res 1
xOffsetOAM: .res 1 ;keeps how much to offset x/y from orgin
yOffsetOAM: .res 1
tileOffset: .res 1
tempPlayerPallet: .res 1
tileOffsetAddTmpX: .res 1


;true/false
.enum bool
	false
	true
.endenum

;pause state
.enum   pauseStateEnum
		notPaused
		paused
.endenum
inPause: .res 1

.enum   pauseKeyStateEnum
		pauseKeyUp
		pauseKeyDown
.endenum

pauseKeyState: .res 1


;level data
ppuEscapeLevelData = $FF
levelKillGround = 230 ;if player's y > than this they die
levelCeiling = 10 ;celeling for all level

.enum   gameStateEnum
		TitleScreen		
		ThePits ;called bomb basement in the title screen
		PitPlat
		TooTallTower ;named totally tall on title screen
.endenum
gameState: .res 1 ;what map/level/scree/scece your on

.enum   platfromDataEnm		
		levelWallLeftX
		levelWallRightX
		noWalls ;turn off walls
		
		platsSharedY
		platLeftStartX
		platLeftEndX
		platRightStartX
		platRightEndX
		
		platsGroundY
		platsGroundStartX
		platsGroundEndX
		
		platSizeBytes
.endenum
platfromData: .res platfromDataEnm::platSizeBytes


;player data
;player directions
.enum   dirs
		right
		left
.endenum

;player size
nesSpriteWidth = 8
nesSpriteHeight = nesSpriteWidth
playerWidthPixels = nesSpriteWidth * 3
playerHeightPixels = playerWidthPixels

playerWidthTiles = playerWidthPixels / nesSpriteWidth
playerHeightTiles = playerWidthTiles

;player count
.enum   playerId

		player1
		player2
		playerCountMax
.endenum

;players data
playerPad: .res playerId::playerCountMax
playerFacing: .res playerId::playerCountMax
playerHit: .res playerId::playerCountMax ;hit tells other player got hit, other player decides what happens next
playerDieCount: .res playerId::playerCountMax
playerInKnockback: .res playerId::playerCountMax ;so you only get knockback once
playerAttackCount: .res playerId::playerCountMax ;used to limit attacking to one frame

leftSpeed: .res playerId::playerCountMax
rightSpeed: .res playerId::playerCountMax
upSpeed: .res playerId::playerCountMax
downSpeed: .res playerId::playerCountMax
playerOnGround: .res playerId::playerCountMax
playerJumpTimer: .res playerId::playerCountMax ;not a normal timer, used to pick jump height

playerX: .res playerId::playerCountMax
playerY: .res playerId::playerCountMax
PlayerRight: .res playerId::playerCountMax
PlayerBottom: .res playerId::playerCountMax

playerMoveCooldown: .res playerId::playerCountMax ;also used for stunning
playerAttackCooldown: .res playerId::playerCountMax
playerAttackingTimer: .res playerId::playerCountMax
playerInvincibilityTimer: .res playerId::playerCountMax
playerBlockTimer: .res playerId::playerCountMax
playerBlockCooldown: .res playerId::playerCountMax
playerDeadTimer: .res playerId::playerCountMax

playerSpriteId: .res playerId::playerCountMax ;starting sprite (id is first 8x8 sprite of the 24x24 ones)
playerSpriteIdEnd: .res playerId::playerCountMax ;ending sprite in animation
playerSpriteState: .res playerId::playerCountMax ;what frame your on
playerSpriteTimerStart: .res playerId::playerCountMax ;starting value of the timer
playerSpriteTimer: .res playerId::playerCountMax
playerPalletId: .res playerId::playerCountMax
playerSpriteHide: .res playerId::playerCountMax

;default player values
StartingPlayerY = 50

player1Facing = dirs::right
player1StartingX = 45
player1Pallet = %00000000

player2Facing = dirs::left
player2StartingX = 185
player2Pallet = %00000001

playerPalletDead = %00000011

;player values
playerMaxSpeed = 7 ;in pixels per frame, after the bitshift
playerMaxHspeed = 80 ;in game unit

blockTimer = 30
blockTimerCooldown = blockTimer + 30

attackFrameCount = 3
attackTimePerFrame = 8
attackTimer = attackFrameCount * attackTimePerFrame
attackTimerCoolDown = attackTimer + 30
attackTestOnFrame = spriteAttackStart + playerWidthTiles ;only check for hit on this frame, (here is frame 2)
attackKnockBackAmount = 45

gettingHitStunTimer = 30 ;when you get hit in block

playerMaxDeaths = 9

;in fixed point off by 3 bits
playerHspeed = 6

playerJump = 100
playerShortJump = 70
playerJumpTimerTest = 10
playerGravity = 4

;dieing
deadTimerAmount = 30
deadJumpSpeed = playerJump
invincibleAfterDeathTimer = 60

;sprite data
spriteRowSize = $10

;Idle
;picks a random sprite
spriteIdleStart = $06
spriteIdleEnd = spriteIdleStart + playerWidthTiles * 3
spriteIdleTimer = 28 ;in frames


;jump
spriteJumpStart = $39
spriteJumpEnd = spriteJumpStart + playerWidthTiles * 2
spriteJumpTimer = 10

;fall
spriteFallStart = $60
spriteFallEnd = spriteFallStart + playerWidthTiles * 2
spriteFallTimer = spriteJumpTimer

;walk
spriteWalkStart = $0
spriteWalkEnd = spriteWalkStart + playerWidthTiles * 2
spriteWalkTimer = 8

;block
spriteBlockStart = $66
spriteBlockEnd = spriteBlockStart + playerWidthTiles * 2
spriteBlockTimer = 5

;attacking
spriteAttackStart = $30
spriteAttackEnd = spriteAttackStart + playerWidthTiles * attackFrameCount
spriteAttackTimer = attackTimePerFrame

;dieing
spriteDieStart = $90
spriteDieEnd = spriteDieStart + playerWidthTiles * 3
spriteDieTimer = 5

;start pose
spriteStartStart = $6C
spriteStartSEnd = spriteStartStart
spriteStartSTimer = gameStartEndTimerAmount

;win sprite
spriteWinStart = $99
spriteWinEnd = spriteWinStart
spriteWinTimer = gameStartEndTimerAmount

;loose sprite
spriteLooseStart = $9C
spriteLooseEnd = spriteLooseStart
spriteLooseTimer = gameStartEndTimerAmount

;scoring
spriteScoreStart = $E0
spriteScoreChangeFame0 = $EA
spriteScoreChangeFame1 = $EB
spriteScoreY = 18


;misc game state
;
gameStartTimer: .res 1
gameEndTImer: .res 1
gameStartEndTimerAmount = 60 * 3
gameStartCountdownX = 124
gameStartCountdownY = 112


;
;==ram vars==
;
.segment "BSS"


;
; gamepad
;
;reading controller
;https://wiki.nesdev.com/w/index.php/Controller_reading_code
PAD_A      = 1 << 7
PAD_B      = 1 << 6
PAD_SELECT = 1 << 5
PAD_START  = 1 << 4
PAD_U      = 1 << 3
PAD_D      = 1 << 2
PAD_L      = 1 << 1
PAD_R      = 1 << 0


.segment "CODE"
gamepad_poll:
    lda #$01
    sta $4016
    sta playerPad + playerId::player2  ; player 2's buttons double as a ring counter
    lsr a         ; now A is 0
    sta $4016
loop:
    lda $4016
    and #%00000011  ; ignore bits other than controller
    cmp #$01        ; Set carry if and only if nonzero
    rol playerPad + playerId::player1   ; Carry -> bit 0; bit 7 -> Carry
    lda $4017     ; Repeat
    and #%00000011
    cmp #$01
    rol playerPad + playerId::player2    ; Carry -> bit 0; bit 7 -> Carry
    bcc loop
	
    rts
	

;
; reset routine
;
reset:
	sei       ; mask interrupts
	lda #0
	sta $2000 ; disable NMI
	sta $2001 ; disable rendering
	sta $4015 ; disable APU sound
	sta $4010 ; disable DMC IRQ
	lda #$40
	sta $4017 ; disable APU IRQ
	cld       ; disable decimal mode
	ldx #$FF
	txs       ; initialize stack
	; wait for first vblank
	bit $2002
	:
		bit $2002
		bpl :-
	; clear all RAM to 0
	lda #0
	ldx #0
	:
		sta $0000, X
		sta $0100, X
		sta $0200, X
		sta $0300, X
		sta $0400, X
		sta $0500, X
		sta $0600, X
		sta $0700, X
		inx
		bne :-
	; place all sprites offscreen at Y=255
	lda #255
	ldx #0
	:
		sta oam, X
		inx
		inx
		inx
		inx
		bne :-
	; wait for second vblank
	:
		bit $2002
		bpl :-
	; NES is initialized, ready to begin!
	; enable the NMI for graphical updates, and jump to our main program
	lda #%10001000
	sta $2000
	jmp main
	
	
;
; nmi routine
;
nmi:
	; save registers
	pha
	txa
	pha
	tya
	pha
	
	; prevent NMI re-entry
	lda nmi_lock
	beq :+
		jmp @nmi_end
	:
	lda #1
	sta nmi_lock
	
	;
	lda nmi_ready
	bne :+ ; nmi_ready == 0 not ready to update PPU
		jmp @ppu_update_end
	:
	
	cmp #2 ; nmi_ready == 2 turns rendering off
	bne :+
		lda #%00000000
		sta $2001
		ldx #0
		stx nmi_ready
		jmp @ppu_update_end
	:
	
	;read nmi
	lda $2002
	
	; sprite OAM DMA
	ldx #0
	stx $2003
	lda #>oam
	sta $4014
	
	; palettes	
	lda #$3F
	sta $2006
	stx $2006 ; set PPU address to $3F00
	ldx #0
	:
		lda colorPallet, x		
		sta $2007
		inx
		cpx #colorPalletSize
		bcc :-
		
	
@end_ppu_draw:
	; set horizontal nametable increment
	lda #%10001000
	sta $2000 
	
	;freeze scroll
	;scroll x
	lda #0
	sta $2005
	
	;scroll y
	lda nmiScreenShakeState
	sta $2005

	; enable rendering
	lda #%00011110
	ora inPause ;make screen black and white in pause
	sta $2001
	
	; flag PPU update complete
	ldx #0
	stx nmi_ready
	
@ppu_update_end:
	;
	;sound code here
	;


	;
	;sound code end
	;
	
	; unlock re-entry flag
	lda #0
	sta nmi_lock
		
@nmi_end:
	; restore registers and return
	pla
	tay
	pla
	tax
	pla
	rti

	
;
; irq
;
.segment "CODE"
irq:
	rti
	
	
;
; drawing utilities
;

; ppu_update: waits until next NMI, turns rendering on (if not already), uploads OAM, palette, and nametable update to PPU
ppu_update:
	lda #1
	sta nmi_ready
	:
		lda nmi_ready
		bne :-
	rts


; ppu_off: waits until next NMI, turns rendering off (now safe to write PPU directly via $2007)
ppu_off:
	lda #2
	sta nmi_ready
	:
		lda nmi_ready
		bne :-
	rts
	
PpuHelper:
	;blink pallet
	;blink color via find and replace	
	lda blinkState
	and #blinkTestSlow >> 1 ;used to slow down how often im running this
	bne PpuHelperSkipBlink
	
	ldx #0
	:
		;blink color via find and replace
		lda rom_palette, X
		cmp #blinkSwapColor
		bne :+
			lda blinkState
			and #blinkTestSlow
 			bne :+
			;new color
			lda #blinkSwapColorNew
			jmp :++
		:
			;else load old color
			lda rom_palette, X
		:
		
		sta colorPallet, x
		inx
		cpx #colorPalletSize
		bcc :---
PpuHelperSkipBlink:

		;screen shake
		lda screenShakeTimer
		beq :+
			;save new offset
			jsr Prng
			and #screenShakeMask
			dec screenShakeTimer
			jmp :++
		:
			lda #0
		:
		sta nmiScreenShakeState

	
	rts
	
	
;
;=== main ===
;
main:
	;load starting map
	lda #gameStateEnum::TitleScreen
	sta gameState
	jsr LoadLevelData
	
	;init pause
	lda #pauseKeyStateEnum::pauseKeyUp
	sta inPause
	sta pauseKeyState
	
	;init players
	jsr SetPlayerDefaultValues
	
	;reset draw pointer to start of OMA
	lda #<oam
	sta spritePointer
	lda #>oam
	sta spritePointer + 1
		
	;start main menu cursor
	lda cursorYPos
	sta cursorY
	
	;init color pallet
	ldx #0
	:
		lda rom_palette, x
		sta colorPallet, x
		inx
		cpx #colorPalletSize 
		bne :-
	
	
	; main loop
	;===========
mainLoop:
	;poll gamepad
	jsr gamepad_poll
	;reset draw pointer to start of OAM
	ldy #0
	sty spritePointer
	;ppu helper
	jsr PpuHelper
	
	;
	;main menu code
	;
	lda gameState
	cmp #gameStateEnum::TitleScreen
	bne :+
		jsr CursorControls
		jsr DrawCursor
		;skip game code if main menu
		jmp mainDraw
	:
	
	
	;
	;pause game key, works only on key down
	;
	lda playerPad + playerId::player1
	ora playerPad + playerId::player2
	and #PAD_START
	beq :++
	;run pause code if keys is not down, set key is down
		ldx pauseKeyState
		bne :+
			lda inPause
			;invert pause state
			eor #1
			sta inPause
			;set pause key state
			ldx #pauseKeyStateEnum::pauseKeyDown
			stx pauseKeyState
		:
		jmp endPauseKey
	:
	;else
	ldx #pauseKeyStateEnum::pauseKeyUp
	stx pauseKeyState
endPauseKey:
	;if game is paused skip to drawing
	ldx inPause
	cpx #pauseStateEnum::paused
	bne :++
		;hold down select to exit to menu
		lda playerPad
		ora playerPad + 1
		and #PAD_SELECT
		beq :+
			jmp ReturnToMainMenu
		:
		jmp mainDraw
	:
	;end of pausing
	
	;
	;==main game code===
	;clean up sprites
	jsr ResetSprites
	;draw start game timer if any
	jsr DrawStartGameTimer

	
	;player state procesing
	ldx #0
	:
		jsr PlayerControls
		jsr playerStepEvent
		jsr PlayerPhysics
		jsr PlayerEndStep
		jsr PlayerDraw
		jsr DrawPlayerScore
		jsr DrawPlayerShadow
		inx
		cpx #playerId::playerCountMax
		bne :-
	
	
	;end game timer
	lda gameEndTImer
	beq :++
		dec gameEndTImer
		;on last frame jump back to main menu
		bne :+
			jmp ReturnToMainMenu
		:
	:
	
	
	;==main loop drawing==
mainDraw:
	;up blink
	inc blinkState
	;wait for next frame
	jsr ppu_update
	jmp mainLoop 	;end of drawing, will loop
;
; end of main loop
;

;============
SetPlayerDefaultValues:
	;player default values
	ldy #0
	
DefaultValuesLoopStart:
		;zero out value
		lda #0
		sta playerPad, y
		sta playerHit, y
		sta playerOnGround, y
		sta playerDieCount, y
		sta playerInKnockback, y
		sta playerAttackCount, y
		
		sta leftSpeed, y
		sta rightSpeed, y
		sta upSpeed, y
		sta downSpeed, y
		sta playerJumpTimer, y
		
		sta playerMoveCooldown, y
		sta playerAttackCooldown, y
		sta playerAttackingTimer, y
		sta playerInvincibilityTimer, y
		sta playerBlockTimer, y
		sta playerBlockCooldown, y
		
		sta playerSpriteId, y
		sta playerSpriteIdEnd, y
		sta playerSpriteState, y
		sta playerSpriteTimerStart, y
		sta playerSpriteTimer, y
		sta playerSpriteHide, y
		sta playerDeadTimer, y
		
		;specific values
		lda #StartingPlayerY
		sta playerY, y
		
		lda #playerHeightPixels + StartingPlayerY - 1
		sta PlayerBottom, y
		
		
		;player specific
		cpy #playerId::player1
		bne :+
			;player 1
			lda #player1Facing
			sta playerFacing, y
			
			lda #player1StartingX
			sta playerX, y 
			
			lda #player1StartingX + playerWidthPixels - 1
			sta PlayerRight, y
			
			lda #player1Pallet
			sta playerPalletId, y
			
			jmp defaultPerPlayerValuesEnd
		:
		
		;player 2
		lda #player2Facing
		sta playerFacing, y
		
		lda #player2StartingX
		sta playerX, y 
		
		lda #player2StartingX + playerWidthPixels - 1
		sta PlayerRight, y
		
		lda #player2Pallet
		sta playerPalletId, y
		
defaultPerPlayerValuesEnd:
		
		;end loop code
		iny
		cpy #playerId::playerCountMax
		beq :+
			jmp	DefaultValuesLoopStart
		:
		rts
		
		
;==============
PlayerControls:
	;check movecooldown
	lda playerMoveCooldown, x
	beq :+
		rts
	:

	;up
	lda playerPad, x
	and #PAD_U
	beq :+

	:
	
	
	;blocking (down)
	lda playerPad, x
	and #PAD_D 
	beq :++
		;block is down+b
		lda playerPad, x
		and #PAD_B 
		beq :+
		
		;make sure cooldown is passed
		lda playerBlockCooldown, x
		bne :+
		
		;set timers
		lda #blockTimer
		sta playerMoveCooldown, x
		sta playerBlockTimer, x
		sta playerInvincibilityTimer, x
		
		lda #blockTimerCooldown
		sta playerBlockCooldown, x
		rts
		
		;test if down + jump to fall though platfroms
		:
		;test jump
		lda playerPad, x
		and #PAD_A
		beq :+
	
		;only when on ground
		lda playerOnGround, x 
		beq :+
		
		;and bottom is same as platfroms y
		lda PlayerBottom, x
		cmp platfromData + platfromDataEnm::platsSharedY
		bne :+
	
		;set not on ground
		lda #bool::false
		sta playerOnGround, x
	
		;clip though ground
		inc playerY, x
		inc PlayerBottom, x
	:
	
	
	;ignore left + right
	lda playerPad, x
	and #PAD_L + PAD_R
	cmp #PAD_L + PAD_R
	bne :+
		jmp playerContolsSkipLeftRight
	:
	
	;left dpad
	lda playerPad, x
	and #PAD_L 
	beq :+
		;check if under speed limti
		lda leftSpeed, x
		cmp #playerMaxHspeed
		bcs :+
	
		;add speed
		clc
		adc #playerHspeed
		sta leftSpeed, x
		
		;face new direction
		lda #dirs::left
		sta playerFacing, x 
	:
	
	;right dpad
	lda playerPad, x
	and #PAD_R
	beq :+
		lda rightSpeed, x
		cmp #playerMaxHspeed
		bcs :+
		
		;add speed
		clc
		adc #playerHspeed
		sta rightSpeed, x
		
		;face new direction
		lda #dirs::right
		sta playerFacing, x 
	:
playerContolsSkipLeftRight:
	
	;
	;attacking
	;
	;test attack button
	lda playerPad, x
	and #PAD_B
	beq playerContolsAttackEnd

	;dont attack if pressing down, thats for blocking
	lda playerPad, x
	and #PAD_D
	bne playerContolsAttackEnd

	;make sure cooldown is passed
	lda playerAttackCooldown, x
	bne playerContolsAttackEnd
	
	;signal a new attack for step event
	inc playerAttackCount, x
	
	;set attack timers timers
	lda #attackTimer
	sta playerMoveCooldown, x
	sta playerAttackingTimer, x
	
	lda #attackTimerCoolDown
	sta playerAttackCooldown, x
	rts ;if playerMoveCooldown gets set you must return
playerContolsAttackEnd:
	
	;
	;jump key
	;
	;reset jump timer if not on ground
	lda playerOnGround, x
	cmp #bool::true
	beq :+
		lda #0
		sta playerJumpTimer, x
		jmp playerContolsSkipJump
	:
	;test jump button
	lda playerPad, x
	and #PAD_A
	beq :++
		;start timer
		inc playerJumpTimer, x
		;test if short or long jump
		lda playerJumpTimer, x
		cmp #playerJumpTimerTest
		bcs:+
			jmp playerContolsSkipJump
		:
		;store jump amount
		lda #playerJump
		sta tmpVarZp
		;jmp
		jmp playerContolsJump
	:
	;test if short jump
	lda playerJumpTimer, x
	beq :+
		;short jump
		lda #playerShortJump
		sta tmpVarZp
		jmp playerContolsJump
	:
	jmp playerContolsSkipJump
playerContolsJump:
		;add jump to up speed
		lda upSpeed, x
		clc
		adc tmpVarZp
		sta upSpeed, x
		
		;reset timer
		lda #0
		sta playerJumpTimer, x
		
		;set not on ground
		lda #bool::false
		sta playerOnGround, x
	
playerContolsSkipJump:
	
	;select
	lda playerPad, x
	and #PAD_SELECT
	beq :+
		;
	:
	
	rts
	
	
;==========
playerStepEvent:

	;attacking
	;check if your on the attacking frame
	lda playerSpriteState, x
	cmp #attackTestOnFrame
	bne playerStepSkipHitTest

	;check if this is a new attack
	lda playerAttackCount, x
	beq playerStepSkipHitTest
	
	;list it as not a new attack
	dec playerAttackCount, x

	;check if player is overlapping
	jsr PlayerTestOverlap
	cmp #bool::true
	bne playerStepSkipHitTest
	
	;set other player as hit
	txa
	eor #1 ;to select other player
	tay
	lda #bool::true
	sta playerHit, y
	
playerStepSkipHitTest:
	rts

	
;===================
PlayerPhysics:	
	;
	;hspeed
	;
	
	;friction
	;left
	lda leftSpeed, x
	tay
	cmp frictionTable, y
	bcs :+
		lda #0
		jmp :++
	:
	;else
	sec
	sbc frictionTable, y
	:
	sta leftSpeed, x
	
	;right
	lda rightSpeed, x
	tay
	cmp frictionTable, y
	bcs :+
		lda #0
		jmp :++
	:
	;else
	sec
	sbc frictionTable, y
	:
	sta rightSpeed, x
	
	
	;find diffrance between in speed of left and right
	ldy #0
	lda leftSpeed, x
	cmp rightSpeed, x
	bcc :+
		;left is bigger
		;remove speed from left from right
		sec
		sbc rightSpeed, x
		sta leftSpeed, x
		
		;remove all speed from right
		sty rightSpeed, x
		
		jmp :++
	:
	;else right is bigger
	lda rightSpeed, x
	sec
	sbc leftSpeed, x
	sta rightSpeed, x
	
	;remove all speed from right
	sty leftSpeed, x
	:
	
	;get whole part of speed from fixed point
	lsr
	lsr
	lsr
	lsr
	
	;make sure its not over speed limit
	cmp #playerMaxSpeed
	bcc :+
		lda #playerMaxSpeed
	:
	;end hspeed code if no speed left
	cmp #0
	bne :+
		jmp PlayerPhysicsHspeedExit
	:
	
	;moving left/right code
	sta tmpVarZp ;save speed in temp
	
	;find out which way your moving
	ldy leftSpeed, x
	bne :+
		jmp PlayerPhysicsRight
	:
	
	;move left
	;move x
	lda playerX, x
	sec
	sbc tmpVarZp
	sta playerX, x
	
	jmp PlayerPhysicsSkipRight
PlayerPhysicsRight:
	;move right
	;move x
	lda playerX, x
	clc
	adc tmpVarZp
	sta playerX, x

	
PlayerPhysicsSkipRight:
	;update right side offset
	clc
	adc #playerWidthPixels - 1
	sta PlayerRight, x

	
PlayerPhysicsHspeedExit:
	;	
	;left/right wall collision detection
	lda platfromData + platfromDataEnm::noWalls
	beq :+
		jmp PlayerPhysicsDownSkipWallCheck
	:
	
	;left wall
	lda playerX, x
	cmp platfromData + platfromDataEnm::levelWallLeftX
	bcs :++
		;clip player into level
		lda platfromData + platfromDataEnm::levelWallLeftX
		sec
		sbc #1 ;fix off by 1
		sta playerX, x
		clc
		adc #playerWidthPixels - 1
		sta PlayerRight, x
		
		;bounce when lost moves
		lda playerMoveCooldown, x
		beq :+
			lda leftSpeed, x
			sta rightSpeed, x
			lda #0
			sta leftSpeed, x
			jmp :++
		:
		;else just stop
		lda #0
		sta leftSpeed, x
	:
	
	;right wall
	lda PlayerRight, x
	cmp platfromData + platfromDataEnm::levelWallRightX
	bcc :++
		lda platfromData + platfromDataEnm::levelWallRightX
		sta PlayerRight, x
		sec
		sbc #playerWidthPixels - 1
		sta playerX, x
		
		;hit a wall set
		lda #bool::true
		sta tmpVarZp
		
		;bounce when lost moves
		lda playerMoveCooldown, x
		beq :+
			lda rightSpeed, x
			sta leftSpeed, x
			lda #0
			sta rightSpeed, x
			jmp :++
		:
		lda #0
		sta rightSpeed, x
	:
PlayerPhysicsDownSkipWallCheck:
	
	
	;
	;vspeed
	;
	;start platfrom collision detecting
	
	;gravity	
	lda downSpeed, x
	clc
	adc #playerGravity
	sta downSpeed, x
	
	;find diffrance up/down
	ldy #0
	lda upSpeed, x
	cmp downSpeed, x
	bcc :+
		;up is bigger
		;remove speed from other speed
		sec
		sbc downSpeed, x
		sta upSpeed, x
		
		;remove all speed
		sty downSpeed, x
		
		jmp :++
	:
	;else down is bigger
	lda downSpeed, x
	sec
	sbc upSpeed, x
	sta downSpeed, x
	
	;remove all speed from right
	sty upSpeed, x
	:
	
	;get whole part of speed from fixed point
	lsr
	lsr
	lsr
	lsr
	
	;make sure its not over speed limit
	cmp #playerMaxSpeed
	bcc :+
		lda #playerMaxSpeed
	:
	;end hspeed code if no speed left
	cmp #0
	bne :+
		jmp PlayerPhysicsPlatfromCollision
	:
	
	;moving up/down
	sta tmpVarZp ;save speed in temp
	
	;find out which way your moving
	ldy upSpeed, x
	bne :+
		ldy tmpVarZp
		jmp PlayerPhysicsDown
	:
	
	;move up
	lda playerY, x
	sec
	sbc tmpVarZp
	sta playerY, x
	
	;update bottom offset
	clc
	adc #playerHeightPixels - 1
	sta PlayerBottom, x
	
	;ceiling
	lda playerY, x
	cmp #levelCeiling
	bcs :+
		;set top
		lda #levelCeiling
		sta playerY, x
	
		;set bottom
		lda #levelCeiling + playerHeightPixels - 1
		sta PlayerBottom, x
		
		;remove speed
		lda #0
		sta upSpeed, x
	:
	
	jmp PlayerPhysicsPlatfromCollision ;skip moving down
	
PlayerPhysicsDown:
	
	;ground plat
	lda PlayerBottom, x
	cmp platfromData + platfromDataEnm::platsGroundY
	bne :+
		;check left
		lda PlayerRight, x
		cmp platfromData + platfromDataEnm::platsGroundStartX
		bcc :+
		;check right
		lda playerX, x
		cmp platfromData + platfromDataEnm::platsGroundEndX
		bcs :+
	
		jmp PlayerPhysicsLanded
	:
	
	;plat2
	lda PlayerBottom, x
	cmp platfromData + platfromDataEnm::platsSharedY
	bne :+
		;check left
		lda PlayerRight, x
		cmp platfromData + platfromDataEnm::platLeftStartX
		bcc :+
		;check right
		lda playerX, x
		cmp platfromData + platfromDataEnm::platLeftEndX
		bcs :+
	
		jmp PlayerPhysicsLanded
	:
	
	;plat3
	lda PlayerBottom, x
	cmp platfromData + platfromDataEnm::platsSharedY
	bne :+
		;check left
		lda PlayerRight, x
		cmp platfromData + platfromDataEnm::platRightStartX
		bcc :+
		;check right
		lda playerX, x
		cmp platfromData + platfromDataEnm::platRightEndX
		bcs :+
	
		jmp PlayerPhysicsLanded
	:
	
	;move player down after checks
	inc playerY, x
	inc PlayerBottom, x

	;loop
	dey
	bne PlayerPhysicsDown
	
	;set not on ground
	lda #bool::false
	sta playerOnGround, x
	
	jmp PlayerPhysicsPlatfromCollision

PlayerPhysicsLanded:
	;set on ground
	lda #bool::true
	sta playerOnGround, x
	
	;zero down speed
	lda #0
	sta downSpeed, x
	
PlayerPhysicsPlatfromCollision:
	;test if fell out of level aka kill ground
	lda PlayerBottom, x
	cmp #levelKillGround
	bcc :+
		;move player ontop of ground
		lda #levelKillGround
		sta PlayerBottom, x
		sec
		sbc #playerHeightPixels - 1
		sta playerY, x
		
		ldy #0
		sty downSpeed, x

		;kill them
		lda #bool::true
		sta playerHit, x
		
		;let game know you landed
		lda #bool::true
		sta playerOnGround, x
	:

	;==end player physics==
	rts

	
;==================
PlayerTestOverlap:
	;saves true/false to A
	
	;left
	lda playerX
	cmp PlayerRight + 1
	bcc :+
		lda #bool::false
		rts
	:
	
	;right
	lda PlayerRight
	cmp playerX + 1
	bcs :+
		lda #bool::false
		rts
	:
	
	;up
	lda playerY
	cmp PlayerBottom + 1
	bcc :+
		lda #bool::false
		rts
	:
	
	;down
	lda PlayerBottom
	cmp playerY + 1
	bcs :+
		lda #bool::false
		rts
	:
	
	;overlap
	lda #bool::true
	rts
	
	
;============
PlayerDraw:
	;note: make sure to rest the draw pointer index at new frame
		
	;gamestart
	lda gameStartTimer
	beq :++ ;skip if zero
		lda playerSpriteId, x
		cmp #spriteStartStart
		bne :+
			;break if already doing it
			jmp PlayerDrawAnimationLogicStart
		:
		
		;load new sprite data
		lda #spriteStartStart
		sta spriteWinStart, x
		sta playerSpriteState, x
		
		lda #spriteStartSEnd
		sta playerSpriteIdEnd, x
		
		lda #spriteStartSTimer
		sta playerSpriteTimerStart, x
		sta playerSpriteTimer, x
		jmp PlayerDrawAnimationLogicStart
	:
	
	;dead
	lda playerDeadTimer, x
	beq :++ ;skip if zero
		lda playerSpriteId, x
		cmp #spriteDieStart
		bne :+
			;break if already doing it
			jmp PlayerDrawAnimationLogicStart
		:
		
		;load new sprite data
		lda #spriteDieStart
		sta playerSpriteId, x
		sta playerSpriteState, x
		
		lda #spriteDieEnd
		sta playerSpriteIdEnd, x
		
		lda #spriteDieTimer
		sta playerSpriteTimerStart, x
		sta playerSpriteTimer, x
		jmp PlayerDrawAnimationLogicStart
	:
	
	;game end
	lda gameEndTImer
	beq :+++ ;skip if zero
		lda playerDieCount, x
		cmp #playerMaxDeaths
		bne :+
			lda #spriteLooseStart
			sta spriteWinStart, x
			sta playerSpriteState, x
			
			lda #spriteLooseEnd
			sta playerSpriteIdEnd, x
			
			lda #spriteLooseTimer
			sta playerSpriteTimerStart, x
			sta playerSpriteTimer, x
			jmp :++
		:
			lda #spriteWinStart
			sta spriteWinStart, x
			sta playerSpriteState, x
			
			lda #spriteWinEnd
			sta playerSpriteIdEnd, x
			
			lda #spriteWinTimer
			sta playerSpriteTimerStart, x
			sta playerSpriteTimer, x
		:
		jmp PlayerDrawAnimationLogicStart
	:
	
	;attacking
	lda playerAttackingTimer, x
	beq :++ ;skip if zero
		lda playerSpriteId, x
		cmp #spriteAttackStart
		bne :+
			;break if already doing it
			jmp PlayerDrawAnimationLogicStart
		:
		
		;load new sprite data
		lda #spriteAttackStart
		sta playerSpriteId, x
		sta playerSpriteState, x
		
		lda #spriteAttackEnd
		sta playerSpriteIdEnd, x
		
		lda #spriteAttackTimer
		sta playerSpriteTimerStart, x
		sta playerSpriteTimer, x
		jmp PlayerDrawAnimationLogicStart
	:
	
	;blocking
	lda playerBlockTimer, x
	beq :++ ;skip if zero
		lda playerSpriteId, x
		cmp #spriteBlockStart
		bne :+
			;break if already doing it
			jmp PlayerDrawAnimationLogicStart
		:
		
		;load new sprite data
		lda #spriteBlockStart
		sta playerSpriteId, x
		sta playerSpriteState, x
		
		lda #spriteBlockEnd
		sta playerSpriteIdEnd, x
		
		lda #spriteBlockTimer
		sta playerSpriteTimerStart, x
		sta playerSpriteTimer, x
		jmp PlayerDrawAnimationLogicStart
	:
	
	
	;skip up and down if on ground
	lda playerOnGround, x
	bne PlayerDrawSkipUpDown

	;up
	lda upSpeed, x
	beq :+ ;skip if zero
		lda playerSpriteId, x
		cmp #spriteJumpStart
		beq PlayerDrawAnimationLogicStart
		
		;load new sprite data
		lda #spriteJumpStart
		sta playerSpriteId, x
		sta playerSpriteState, x
		
		lda #spriteJumpEnd
		sta playerSpriteIdEnd, x
		
		lda #spriteJumpTimer
		sta playerSpriteTimerStart, x
		sta playerSpriteTimer, x
		jmp PlayerDrawAnimationLogicStart
	:

	;going down
	lda downSpeed, x
	beq :+ ;skip if zero
		lda playerSpriteId, x
		cmp #spriteFallStart
		beq PlayerDrawAnimationLogicStart

		;load new sprite data
		lda #spriteFallStart
		sta playerSpriteId, x
		sta playerSpriteState, x
		
		lda #spriteFallEnd
		sta playerSpriteIdEnd, x
		
		lda #spriteFallTimer
		sta playerSpriteTimerStart, x
		sta playerSpriteTimer, x
		jmp PlayerDrawAnimationLogicStart
	:
PlayerDrawSkipUpDown:
	
	;walking left/right
	lda leftSpeed, x
	ora rightSpeed, x
	beq :+
		lda playerSpriteId, x
		cmp #spriteWalkStart
		beq PlayerDrawAnimationLogicStart
		
		;load new sprite data
		lda #spriteWalkStart
		sta playerSpriteId, x
		sta playerSpriteState, x
		
		lda #spriteWalkEnd
		sta playerSpriteIdEnd, x
		
		lda #spriteWalkTimer
		sta playerSpriteTimerStart, x
		sta playerSpriteTimer, x
		jmp PlayerDrawAnimationLogicStart
	:
	
	
	;Idle animation
	;
	;break if already doing it
	lda playerSpriteId, x
	cmp #spriteIdleStart
	beq PlayerDrawAnimationLogicStart
	
	;break if not on ground
	lda playerOnGround, x
	beq PlayerDrawAnimationLogicStart
	
	;load new sprite data
	lda #spriteIdleStart
	sta playerSpriteId, x
	sta playerSpriteState, x
	
	lda #spriteIdleEnd
	sta playerSpriteIdEnd, x
	
	lda #spriteIdleTimer
	sta playerSpriteTimerStart, x
	sta playerSpriteTimer, x
	
PlayerDrawAnimationLogicStart:
	;animation logic
	lda playerSpriteTimer, x
	bne :++ ;skip if timer is not done yet
		;reset timer
		lda playerSpriteTimerStart, x
		sta playerSpriteTimer, x
		
		;inc player sprite
		lda playerSpriteState, x
		clc
		adc #playerWidthTiles
		sta playerSpriteState, x
		
		;check if we need to loop sprite
		cmp playerSpriteIdEnd, x
		bcc :+ ;skip if we have not past the end
			lda playerSpriteId, x
			sta playerSpriteState, x  ;reset sprite id to start
		:
	:


	;
	;draw sprite
	;
	;dont draw if hidden
	lda playerSpriteHide, x
	beq :+
		rts
	:
	
	;load sprite index
	ldy spritePointer
	
	;start y axis loop
	lda #playerHeightTiles
	sta yAxisOAM
	
	;reset sprite offset 
	lda playerY, x
	sta yOffsetOAM
	
	;starting tile of sprite
	lda playerFacing, x
	bne :+
		lda playerSpriteState, x
		clc
		adc #playerWidthTiles - 1
		jmp :++
	:
	;else
		lda playerSpriteState, x
	:
	sta tileOffset
	
	;
	;cache values for loop
	;
	;pallet data cache
	lda playerFacing, x
	bne :+
		;flipped
		lda #%01000000 ;for attributes below
		jmp :++
	:
	;else
		lda #%00000000 ;for attributes below
	:
	;get pallet data for player
	ora playerPalletId, x 
	;pallet dead for dead
	pha
	lda playerDeadTimer, x
	beq :+
		pla
		ora #playerPalletDead
		pha
	:
	pla
	sta tempPlayerPallet
	
	;cahce tile offset add x
	lda playerFacing, x
	bne :+
		;flipped
		lda #255
		jmp :++
	:
	;else
		lda #1
	:
	sta tileOffsetAddTmpX
	
PlayerDrawLoopY:
	;======x axis=======
	
	;reset x
	lda playerX, x
	sta xOffsetOAM
	
	;x axis loop
	lda #playerWidthTiles
	sta xAxisOAM
	
PlayerDrawLoopX:
	;======x axis=======
	
	;==set sprite OAM==
	;y
	lda yOffsetOAM
	sec
	sbc nmiScreenShakeState
	sta (spritePointer), y
	
	;tile
	lda tileOffset
	iny
	sta (spritePointer), y
	;move to next tile
	clc
	adc tileOffsetAddTmpX
	sta tileOffset
	
	;attribute
	lda tempPlayerPallet
	;save your sprite
	iny
	sta (spritePointer), y
	
	;x
	iny
	lda xOffsetOAM
	sta (spritePointer), y
	clc ;inc to next sprite pos
	adc #nesSpriteWidth
	sta xOffsetOAM
	
	;move sprite pointer to next sprite
	iny
	
	;==end sprite OAM==
	
	;loop upkeep x
	dec xAxisOAM
	bne PlayerDrawLoopX
	;======x axis end=======
	
	;move sprite offset on y
	lda yOffsetOAM
	clc
	adc #nesSpriteHeight
	sta yOffsetOAM
	
	;move tileOffset a y amount
	lda playerFacing, x
	bne :+
		;if flipped
		lda tileOffset
		clc
		adc #spriteRowSize + playerWidthTiles
		jmp :++
	:
	;else, if normal
		lda tileOffset
		clc
		adc #spriteRowSize - playerWidthTiles
	:
	sta tileOffset
	
	;loop upkeep y
	dec yAxisOAM
	bne PlayerDrawLoopY
	;======y axis end=======

	;save sprite index
	sty spritePointer
	
	;end
	rts

	
;=====
PlayerEndStep:
	;last code to run befor drawing the player
	
	;
	;get knock back when blocking
	;
	;test hit flag
	lda playerHit, x
	cmp #bool::true
	bne PlayerEndKnockbackExit
	
	;if blocking
	lda playerBlockTimer, x
	beq PlayerEndKnockbackExit

	;and not in knockback
	lda playerInKnockback, x
	cmp #bool::false
	bne PlayerEndKnockbackExit
	
	;get knocked back
	;select other player id
	txa
	eor #1
	tay
	;knockback me depending on other players facing direction
	lda playerFacing, y
	cmp #dirs::left
	beq :+
		lda #attackKnockBackAmount
		sta rightSpeed, x
		jmp :++
	:
	;else knockback right
	lda #attackKnockBackAmount
	sta leftSpeed, x
	:
	;stun player
	lda #gettingHitStunTimer
	sta playerMoveCooldown, x
	
	;set knockback
	lda #bool::true
	sta playerInKnockback, x
PlayerEndKnockbackExit:
	
	;
	;getting hit
	;
	;test hit flag
	lda playerHit, x
	beq PlayerEndStepGettingHitExit

	;reset playerHit
	lda bool::false
	sta playerHit, x
	
	;cant get hit if invincible
	lda playerInvincibilityTimer, x
	bne PlayerEndStepGettingHitExit
	
	;cant die if dead
	lda playerDeadTimer, x
	bne PlayerEndStepGettingHitExit
	
	;set dead
	lda #deadTimerAmount
	sta playerDeadTimer, x
	sta playerMoveCooldown, x
	
	;flinging player in air
	lda #deadJumpSpeed
	sta upSpeed, x
	
	;shake screen
	lda #screenShakeTimerAmount
	sta screenShakeTimer
	
	;inc death count
	inc playerDieCount, x
	lda playerDieCount, x
	cmp #playerMaxDeaths
	bcc :+ ;if you die too much
		;cap win count
		lda #playerMaxDeaths
		sta playerDieCount, x
		;start end game timer
		lda #gameStartEndTimerAmount
		sta gameEndTImer
		;make everyone invincible
		sta playerInvincibilityTimer
		sta playerInvincibilityTimer + 1
		;stun everyone also
		sta playerMoveCooldown
		sta playerMoveCooldown + 1
	:
	
PlayerEndStepGettingHitExit:
	
	;
	;blink when invincible
	;
	lda playerInvincibilityTimer, x
	beq :+++
		lda blinkState
		and #blinkTest
		bne :+
			lda #hideSprite
			jmp :++
		:
			lda #showSprite
		:
		sta playerSpriteHide, x
		jmp :++
	:
	;else keep sprite shown
		lda #showSprite
		sta playerSpriteHide, x
	:
	
	
	;
	;dec player timers
	;
	lda playerMoveCooldown, x
	beq :+
		dec playerMoveCooldown, x
		jmp :++
	:
	;else reset knockback
		lda #bool::false
		sta playerInKnockback, x
	:
	
	lda playerAttackCooldown, x
	beq :+
		dec playerAttackCooldown, x
	:
	
	lda playerAttackingTimer, x
	beq :+
		dec playerAttackingTimer, x
	:
	
	lda playerInvincibilityTimer, x
	beq :+
		dec playerInvincibilityTimer, x
	:
	
	lda playerBlockTimer, x
	beq :+
		dec playerBlockTimer, x
	:

	lda playerBlockCooldown, x
	beq :+
		dec playerBlockCooldown, x
	:
	
	lda playerSpriteTimer, x
	beq :+
		dec playerSpriteTimer, x
	:
	
	;
	;dieing
	;
	;dec timer
	lda playerDeadTimer, x
	beq :+
		dec playerDeadTimer, x
	:
	
	;reset players near end of dieing
	lda playerDeadTimer, x
	cmp #1
	bne PlayerEndStepEndDeath
	
	;give player a change to recover
	lda #invincibleAfterDeathTimer
	sta playerInvincibilityTimer, x
	
	;reset speed
	lda #0
	sta leftSpeed, x
	sta rightSpeed, x
	sta upSpeed, x
	sta downSpeed, x
	sta playerOnGround, x
	
	;right/bottom
	lda #playerHeightPixels + StartingPlayerY - 1
	sta PlayerBottom, x
	
	;reset y
	lda #StartingPlayerY
	sta playerY, x
	
	;reset player x
	cpx #playerId::player1
	bne :+ ;player 1
		lda #player1StartingX + playerWidthPixels - 1
		sta PlayerRight, x
		
		lda #player1StartingX
		sta playerX, x
		
		jmp :++
	: ;else player 2
		lda #player2StartingX + playerWidthPixels - 1
		sta PlayerRight, x
	
		lda #player2StartingX
		sta playerX, x
	:
PlayerEndStepEndDeath:
	
	;===end of playerend===
	rts
	
	
;===========
DrawPlayerScore:	
	;set up tile index into a tmp var
	lda playerDieCount, x
	clc
	adc #spriteScoreStart
	sta tmpVarZp
	
	;y index
	lda #2
	sta yAxisOAM
	
	;load next free sprite
	ldy spritePointer
	
	;cach blink state
	lda blinkState
	and #blinkTest
	bne :+
		lda #spriteScoreChangeFame0
		jmp :++
	:
		lda #spriteScoreChangeFame1
	:
	sta tileOffset ;using tileOffset as a temp
	
	;cache color
	lda #%00000000
	ora playerPalletId, x
	sta tempPlayerPallet

DrawPlayerScoreLoop:
	;set score y
	lda yAxisOAM
	cmp #1
	bne :+
		lda #spriteScoreY + nesSpriteHeight
		jmp :++
	:
		lda #spriteScoreY
	:
	;store y
	sta (spritePointer), y
	
	;set tile
	;play blink if dead
	iny
	lda playerDeadTimer, x
	beq :+
		lda tileOffset ;what tile to use when blinking
		jmp :++
	:
		lda tmpVarZp
	:
	sta (spritePointer), y
	clc ;set up index for next sprite on y axis
	adc #spriteRowSize
	sta tmpVarZp
	
	;attributes
	lda tempPlayerPallet
	iny
	sta (spritePointer), y 
	
	;x
	cpx #playerId::player1
	bne :+
		lda #player1StartingX
		jmp :++
	:
		lda #player2StartingX + playerWidthPixels
	:
	iny
	sta (spritePointer), y 
	
	
	;end of loop
	iny
	dec yAxisOAM
	bne DrawPlayerScoreLoop
	
	;end
	sty spritePointer
	rts
	
;==============
DrawPlayerShadow:

	;only on map PitPlat
	lda gameState
	cmp #gameStateEnum::PitPlat
	beq :+
		rts
	:

	;load sprite index
	ldy spritePointer
	
	;y mirroring players y
	lda platfromData + platfromDataEnm::platsGroundY
	sec
	sbc playerY, x
	lsr
	lsr
	lsr
	clc
	adc platfromData + platfromDataEnm::platsGroundY
	clc
	adc #nesSpriteHeight * 2
	sta (spritePointer), y
	
	;tile
	;cach blink state
	lda #spriteScoreChangeFame0
	iny
	sta (spritePointer), y 
	
	;attributes
	lda blinkState
	and #%11000000
	ora playerPalletId, x
	iny
	sta (spritePointer), y 
	
	;x
	lda playerX, x
	clc
	adc #nesSpriteWidth
	iny
	sta (spritePointer), y 
	
	;exit
	iny
	sty spritePointer
	rts
	
;=======
DrawStartGameTimer:
	;skip if timer is passed
	lda gameStartTimer
	bne :+
		rts
	:
	dec gameStartTimer

	;decode timer to seconds
	;3
	cmp #60 * 2
	bcc :+
		lda #3
		jmp DrawStartGameTimerDecodeExit
	:
	;2
	cmp #60
	bcc :+
		lda #2
		jmp DrawStartGameTimerDecodeExit
	:
	;1
	lda #1
	;blastoff!
DrawStartGameTimerDecodeExit:
	
	;set up tile index into a tmp var
	clc
	adc #spriteScoreStart
	sta tmpVarZp
	
	;y index
	lda #2
	sta yAxisOAM
	
	;load next free sprite
	ldy spritePointer

DrawStartGameTimerLoop:
	;set y
	lda yAxisOAM
	cmp #1
	bne :+
		lda #gameStartCountdownY + nesSpriteHeight
		jmp :++
	:
		lda #gameStartCountdownY
	:
	;store y
	sta (spritePointer), y
	
	;set tile
	lda tmpVarZp
	iny
	sta (spritePointer), y
	clc
	adc #spriteRowSize
	sta tmpVarZp
	
	;attributes
	and #%00000011
	iny
	sta (spritePointer), y 
	
	;x
	lda #gameStartCountdownX
	iny
	sta (spritePointer), y 
	
	;end of loop
	iny
	dec yAxisOAM
	beq :+
		jmp DrawStartGameTimerLoop
	:
	
	;end
	sty spritePointer
	rts

	
	
;====
LoadLevelData:
	;make sure the lable "gameState" is set
	
	;ThePits
	lda gameState
	cmp #gameStateEnum::ThePits
	bne :++ ;skip loading this map if it does not match
	
	;setup zppointerto look at ppu data to copy
	lda #<thePitsMapPPU
	sta tmpPointer
	lda #>thePitsMapPPU
	sta tmpPointer + 1
	
	;copy platfrom data
	ldy #0
	:
		lda thePitsMapPlatformData, y
		sta platfromData, y
		iny
		cpy #platfromDataEnm::platSizeBytes
		bne :-
	
	jmp LoadLevelBreakDecodeMapId
	:

	;TooTallTower
	lda gameState
	cmp #gameStateEnum::TooTallTower
	bne :++ ;skip loading this map if it does not match
	
	;setup zppointerto look at ppu data to copy
	lda #<TooTallTowerPPU
	sta tmpPointer
	lda #>TooTallTowerPPU
	sta tmpPointer + 1
	
	;copy platfrom data
	ldy #0
	:
		lda TooTallTowerMapPlatformData, y
		sta platfromData, y
		iny
		cpy #platfromDataEnm::platSizeBytes
		bne :-
	
	jmp LoadLevelBreakDecodeMapId
	:
	
	;PitPlat
	lda gameState
	cmp #gameStateEnum::PitPlat
	bne :++ ;skip loading this map if it does not match
	
	;setup zppointerto look at ppu data to copy
	lda #<PitPlatPPU
	sta tmpPointer
	lda #>PitPlatPPU
	sta tmpPointer + 1
	
	;copy platfrom data
	ldy #0
	:
		lda PitPlatMapPlatformData, y
		sta platfromData, y
		iny
		cpy #platfromDataEnm::platSizeBytes
		bne :-
	
	jmp LoadLevelBreakDecodeMapId
	:
	
	;Title screen
	lda gameState
	cmp #gameStateEnum::TitleScreen
	bne :++ ;skip loading this map if it does not match
	
	;setup zppointerto look at ppu data to copy
	lda #<TitleScreenPPU
	sta tmpPointer
	lda #>TitleScreenPPU
	sta tmpPointer + 1
	
	;copy platfrom data
	ldy #0
	:
		lda TitleScreenMapPlatformData, y
		sta platfromData, y
		iny
		cpy #platfromDataEnm::platSizeBytes
		bne :-
	
	jmp LoadLevelBreakDecodeMapId
	:
	

LoadLevelBreakDecodeMapId:
	;wait till its safe to draw
	jsr ppu_off
	
	;set horizontal nametable increment
	lda #%10001000
	sta $2000
	
	;start at start of nametable 0
	lda #$20
	sta $2006

	lda #$00
	sta $2006
		
	;copying level data from rom to vram
	;setup the pointer
	;http://forums.nesdev.com/viewtopic.php?t=7430
		
	;clear the index
	ldy #$00

LoadLevelCopyByte:
	;load
	lda (tmpPointer), y
	
	cmp #ppuEscapeLevelData
	beq LoadLevelDone ;break if found escape
	
	;tile level data has a offset of 1, so move it back 1
	sec
	sbc #1
	sta $2007 ;send to ppu

	;move on to the next byte
	iny
	bne LoadLevelCopyByte

	;move the pointer 256 bytes ahead
	inc tmpPointer+1
	jmp LoadLevelCopyByte

LoadLevelDone:
	rts	
	
;==============
NextSinTableBob:
	;returns next sin on a
	sty rngTmp
	inc sinTableIndex
	ldy sinTableIndex
	lda sinTableBob, y
	ldy rngTmp
	rts
	

;============
Prng:
	;returns in A
	stx rngTmp
	inc rngIndex
	ldx rngIndex
	lda rngTable, x
	ldx rngTmp
	rts
	
;==========
DrawCursor:
	;load next sprite index
	ldy spritePointer
	
	;set y
	jsr NextSinTableBob
	clc
	adc cursorY
	sta (spritePointer), y
	
	;set tile index
	lda #cursorSpriteIndex
	iny
	sta (spritePointer), y
	
	;set attributes
	lda #%00000000
	iny
	sta (spritePointer), y 
	
	;set x
	lda #cursorX
	iny
	sta (spritePointer), y 
	
	;exit
	iny
	rts

	
;==move main menu cursor==
CursorControls:
	;move cursor to option
	ldy cursorIndex
	lda cursorYPos, y
	cmp cursorY
	beq CursorControlsMoveExit

	bcc :+
		inc cursorY
		inc cursorY
		jmp :++
	:
		dec cursorY
		dec cursorY
	:
	rts
CursorControlsMoveExit:

	;reuse a player timer for key delay
	lda playerDeadTimer
	beq :+
		dec playerDeadTimer
		rts
	:

	;up
	lda playerPad
	ora playerPad + 1 ;or player 2
	and #PAD_U
	beq :+
		dec cursorIndex
		lda #cursorPauseAmount
		sta playerDeadTimer
	:
	
	;down
	lda playerPad
	ora playerPad + 1
	and #PAD_D
	beq :+
		inc cursorIndex
		lda #cursorPauseAmount
		sta playerDeadTimer
	:

	;keep cursor in range
	lda cursorIndex
	cmp #$FF
	bne :+
		lda #cursorEnum::sizeMax - 1
	:
	cmp #cursorEnum::sizeMax
	bne :+
		lda #0
	:
	sta cursorIndex
	
	;if they select a map start it
	lda playerPad
	ora playerPad + 1
	and #PAD_SELECT + PAD_B + PAD_START
	beq CursorControlsLoadMapExit

	;decode cursor index
	lda cursorIndex
	cmp #cursorEnum::TotallyTall
	bne :+
		lda #gameStateEnum::TooTallTower
		jmp CursorControlsLoadMap
	:
	cmp #cursorEnum::PitPlat
	bne :+
		lda #gameStateEnum::PitPlat
		jmp CursorControlsLoadMap
	:
	cmp #cursorEnum::BombBasement
	bne :+
		lda #gameStateEnum::ThePits
	:
CursorControlsLoadMap:
		;load level data
		sta gameState
		jsr LoadLevelData
		
		;hide menu graphics
		jsr ResetSprites
		
		;set player data
		jsr SetPlayerDefaultValues
		
		;pause player at start
		lda #gameStartEndTimerAmount
		sta playerMoveCooldown
		sta playerMoveCooldown + 1
		;set start game timer
		sta gameStartTimer
		
		;set pause key state, so you dont pause once you enter the game
		ldx #pauseKeyStateEnum::pauseKeyDown
		stx pauseKeyState
CursorControlsLoadMapExit:
	rts

;============
ResetSprites:		
	;kills a and y
	;place all sprites offscreen at Y=255
	lda #$FF
	ldy #0
	:
		sta oam, y
		iny
		iny
		iny
		iny
		bne :-
	rts

	
;=========
ReturnToMainMenu:
	;unpause
	lda #0
	sta inPause

	;set timer to ignore input
	lda #cursorPauseNewScreen
	sta playerDeadTimer

	;hide sprites
	jsr ResetSprites

	;rese go back to menu
	lda #gameStateEnum::TitleScreen
	sta gameState
	jsr LoadLevelData
	jmp mainLoop
	
	
	
;
;=== RO DATA ===
;
	
;== CHR ROM ==
.segment "TILES"
.incbin "background.chr"
.incbin "sprite.chr"


;==pallets==
.segment "RODATA"
rom_palette:
;bg
.byte $0F,$30,$15,$11 ; main colors, white blue and red
.byte $0F,$2D,$2D,$0F ; gray scale bg
.byte $0F,$01,$11,$38 ; water
.byte $0F,$17,$28,$39 ; towerColor
;sprites
.byte $0F,$38,$25,$15 ; player1
.byte $0F,$38,$21,$15 ; player2
.byte $0F,$01,$11,$38 ; not in use
.byte $0F,$38,$16,$27 ; player dead


;===sin lookup table===
sinTableBob: ;mutipled by a offset of 3
.byte 3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2

;===friction lookup==
frictionTable:
.byte 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12

;===rng===
rngTable:
.byte 37, 49, 63, 108, 206, 229, 68, 32, 185, 75, 15, 34, 196, 121, 93, 251, 127, 46, 115, 78, 193, 77, 198, 73, 119, 217, 173, 190, 67, 107, 34, 62, 111, 58, 84, 72, 118, 42, 20, 174, 213, 108, 137, 111, 39, 7, 221, 214, 144, 172, 210, 27, 119, 137, 196, 184, 206, 77, 38, 17, 48, 152, 60, 102, 194, 27, 54, 178, 156, 141, 220, 57, 32, 218, 139, 216, 158, 192, 42, 58, 97, 196, 135, 132, 87, 118, 173, 174, 72, 109, 205, 1, 58, 58, 165, 226, 214, 140, 26, 141, 60, 193, 220, 178, 42, 208, 202, 100, 239, 140, 111, 16, 232, 215, 200, 31, 57, 137, 197, 252, 93, 67, 1, 197, 166, 55, 13, 167, 17, 18, 200, 68, 175, 121, 108, 128, 158, 201, 207, 192, 63, 202, 80, 214, 145, 115, 30, 17, 23, 137, 70, 149, 100, 22, 129, 156, 4, 99, 18, 162, 121, 234, 173, 155, 143, 30, 9, 41, 52, 233, 61, 146, 99, 234, 129, 182, 200, 221, 96, 209, 248, 131, 209, 19, 95, 230, 151, 62, 241, 196, 126, 16, 148, 175, 26, 160, 126, 23, 199, 107, 180, 185, 49, 0, 153, 60, 112, 77, 14, 122, 50, 124, 31, 93, 150, 70, 121, 158, 204, 111, 203, 224, 215, 40, 72, 63, 18, 161, 145, 253, 140, 147, 92, 60, 134, 65, 84, 209, 43, 15, 93, 251, 110, 227, 24, 211, 46, 41, 171, 33, 165, 188, 80, 213, 148


;===menu cursor locations===
cursorYPos:
.byte 82 ;TotallyTall
.byte 114 ;PitPlat
.byte 146 ;BombBasement

;==level data==
;all values are offset by 1

;
;The Pits Map
;
thePitsMapPPU:
;nametable tile map
.byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,97,97,97,97,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,97,97,97,97,1,1,1,1,142,143,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,97,97,97,97,1,1,174,175,158,159,1,1,1,1,1,1,1,1,81,80,77,74,68,70,1,84,85,66,85,70,1,1,97,97,97,97,1,1,176,158,158,14,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,97,97,97,97,1,1,15,158,158,14,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,17,1,1,1,1,1,1,15,158,158,14,1,1,133,133,133,133,133,133,133,133,133,133,133,133,133,133,133,133,133,133,133,1,1,1,1,1,1,1,15,158,158,14,129,130,131,132,166,167,131,132,166,167,131,132,166,167,131,132,166,167,131,132,166,135,1,1,1,1,1,22,185,19,158,14,145,167,131,167,131,132,166,162,162,132,166,167,131,132,166,167,131,132,166,167,131,147,1,150,150,1,1,1,201,1,158,14,161,167,131,167,131,132,1,1,1,132,166,167,131,132,166,167,131,132,166,167,131,167,135,150,150,150,150,1,1,174,158,14,166,167,131,167,131,132,1,1,1,1,162,167,131,1,1,162,131,1,1,1,147,132,163,152,150,150,1,1,1,1,158,14,166,167,131,167,131,132,162,1,1,1,1,146,162,1,1,1,1,1,1,1,161,132,167,146,135,149,148,149,170,175,158,14,166,167,1,167,131,132,166,1,1,1,1,1,1,1,1,1,1,1,1,1,1,132,162,167,131,146,131,167,15,158,158,14,166,167,1,1,131,162,166,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,167,131,132,166,167,15,158,158,14,166,162,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,147,132,166,167,15,158,158,14,166,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,132,166,167,15,158,158,14,166,1,1,1,1,1,1,1,1,1,1,1,189,190,191,192,1,1,1,1,1,1,1,1,1,1,166,167,15,158,158,14,166,163,1,1,1,1,1,1,1,1,1,1,205,206,207,208,1,1,1,1,1,1,1,1,1,1,163,167,15,158,158,14,166,1,1,1,1,1,1,1,1,1,1,1,221,222,223,224,1,1,1,1,1,1,1,1,1,1,1,167,15,158,158,14,163,1,1,1,1,1,1,1,1,1,1,1,237,238,239,240,1,1,1,1,1,1,1,1,1,1,1,167,15,158,158,14,1,1,1,1,1,1,1,1,1,1,1,1,253,254,199,200,1,1,1,1,1,1,1,1,1,1,146,167,15,158,158,14,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,147,166,167,15,158,158,14,162,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,131,132,15,158,158,14,166,162,149,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,164,161,131,132,15,158,158,30,10,10,10,10,10,10,10,10,10,4,4,4,4,144,160,8,8,8,8,11,11,11,11,11,11,11,11,11,31,158,158,158,158,158,158,158,158,158,158,158,158,158,158,158,158,158,158,158,158,158,158,158,158,158,158,158,158,158,158,158,158,158,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1

;nametable attribute 
.byte $01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01
.byte $45,$56,$56,$56,$56,$56,$56,$01
.byte $45,$56,$56,$56,$56,$56,$56,$12
.byte $45,$56,$56,$00,$00,$56,$56,$12
.byte $45,$56,$56,$00,$00,$56,$56,$12
.byte $05,$06,$06,$06,$06,$06,$06,$02
.byte $01,$01,$01,$01,$01,$01,$01,$01

.byte ppuEscapeLevelData ;end for ppu copy

;write platfrom data
thePitsMapPlatformData:

.byte 16 ;Wall left
.byte 240 ;Wall right
.byte bool::false ;t/f for no walls

.byte 0 ;plats y
.byte 0, 0 ;plat left x start / end
.byte 0, 0 ;plat right x start / end

.byte 206 ;main ground plat y
.byte 16, 240 ;ground ends in x


;
;TooTallTower Map
;
TooTallTowerPPU:
;nametable
.byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,3,4,4,4,4,5,1,1,1,1,1,1,6,7,8,8,8,8,9,1,1,1,1,1,1,1,1,1,1,1,1,18,19,24,20,20,20,21,1,1,1,1,1,1,22,23,24,20,24,24,25,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,149,133,133,148,149,133,133,133,133,133,133,133,164,165,133,164,1,1,1,1,1,1,1,1,1,1,1,17,1,1,1,134,161,132,166,167,163,166,167,166,167,166,167,147,167,132,166,167,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,134,161,132,166,167,132,166,167,166,167,166,167,166,167,132,166,167,1,1,1,174,175,142,143,1,1,1,1,1,1,1,1,1,129,162,166,167,132,166,167,166,167,166,167,166,167,132,166,167,1,1,1,176,158,158,159,1,155,154,155,155,154,155,155,155,161,132,166,167,132,166,167,166,167,166,167,166,167,132,166,167,154,155,154,155,155,154,155,155,155,154,154,155,154,155,154,155,161,132,166,167,132,166,167,166,167,166,167,166,167,132,166,167,154,154,155,154,155,154,155,154,155,138,139,155,139,139,155,140,161,132,166,167,132,166,167,166,167,166,167,166,167,132,166,167,141,139,141,155,155,155,140,155,139,155,138,141,155,140,139,140,161,132,166,167,132,166,167,166,167,166,167,166,167,132,166,167,141,138,140,139,155,138,139,155,140,139,140,141,140,138,140,155,161,132,166,167,132,166,167,166,167,166,167,166,167,132,166,167,155,138,140,141,155,155,140,155,138,157,138,138,157,139,157,139,161,132,166,167,132,166,167,166,167,166,167,166,167,132,166,167,157,138,139,139,157,157,138,139,157,157,139,139,139,157,139,157,161,132,166,167,132,166,167,166,167,166,167,166,167,132,166,167,157,157,139,138,139,139,157,157,138,139,157,157,157,157,157,139,161,132,166,167,132,166,167,166,167,166,167,166,167,132,166,167,157,139,138,157,157,139,139,139,157,157,157,157,157,157,157,157,161,132,166,167,132,166,167,131,167,131,167,131,167,132,166,167,157,157,157,157,157,157,157,157,27,27,27,27,27,27,27,27,161,132,166,167,132,166,167,131,167,131,167,131,167,132,166,167,27,27,27,27,27,27,27,27

;nametable attribute
;rmb to add 1 offset
.byte $01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$56,$00,$00,$00,$00,$01,$01
.byte $AB,$AB,$00,$00,$00,$00,$AB,$AB
.byte $AB,$AB,$00,$00,$00,$00,$AB,$AB
.byte $AB,$AB,$00,$00,$00,$00,$AB,$AB

.byte ppuEscapeLevelData ;end for ppu copy

;write platfrom data
TooTallTowerMapPlatformData:

.byte 0 ;Wall left
.byte 255 ;Wall right
.byte bool::true ;t/f for no walls

.byte 94 ;plats y
.byte 51, 100 ;plat left x start / end
.byte 155, 204 ;plat right x start / end

.byte 133 ;main ground plat y
.byte 64, 191 ;ground ends in x


;
;PitPlat Map
;
PitPlatPPU:
;nametable
.byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,169,170,171,172,1,1,1,1,150,150,1,150,150,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,185,186,187,188,1,1,150,150,150,150,150,150,150,150,150,1,1,1,142,143,1,1,1,1,1,1,1,1,1,1,1,1,201,202,203,204,150,150,150,150,150,150,150,150,150,150,1,1,174,175,158,159,1,1,1,1,1,1,1,1,1,1,1,1,217,218,219,220,150,150,1,150,150,150,1,1,1,1,1,1,176,158,158,32,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,16,158,158,14,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,15,158,158,14,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,15,158,158,14,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,15,158,158,14,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,15,158,158,14,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,15,158,158,14,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,15,158,158,14,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,15,158,158,14,1,1,1,2,3,4,4,4,4,4,5,1,1,1,1,1,1,6,7,8,8,8,8,8,9,1,1,1,15,158,158,14,1,1,1,18,19,20,20,24,24,20,21,1,1,1,1,1,1,22,23,24,20,20,24,24,25,1,1,1,15,158,158,14,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,150,150,150,150,150,150,150,150,1,1,1,15,158,158,14,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,150,150,150,150,150,150,1,1,1,1,1,15,158,158,14,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,15,158,158,14,133,133,133,133,164,149,133,133,133,133,133,133,133,133,133,133,133,133,133,133,133,133,133,133,133,133,133,148,15,158,158,30,10,10,10,10,10,10,10,10,10,10,144,158,158,158,158,158,158,160,11,11,11,11,11,11,11,11,11,11,31,158,156,156,156,156,156,156,156,156,156,156,156,156,156,156,156,156,156,156,156,156,156,156,156,156,156,156,156,156,156,156,156,156,139,138,138,138,157,139,157,157,139,138,138,138,157,157,157,138,138,139,138,157,157,157,157,157,157,141,157,157,139,157,157,157,141,157,157,157,157,157,139,138,138,138,157,157,157,139,138,157,157,157,157,157,139,138,138,157,157,157,157,157,157,140,138,138,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27

;nametable attribute
;rmb to add 1 offset
.byte $01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01
.byte $A1,$A1,$A1,$A1,$A1,$A1,$A1,$A1
.byte $AB,$AB,$AB,$AB,$AB,$AB,$AB,$AB

.byte ppuEscapeLevelData ;end for ppu copy

;write platfrom data
PitPlatMapPlatformData:

.byte 16 ;Wall left
.byte 240 ;Wall right
.byte bool::false ;t/f for no walls

.byte 142 ;plats y
.byte 43, 100 ;plat left x start / end
.byte 155, 204 ;plat right x start / end

.byte 190 ;main ground plat y
.byte 0, 240 ;ground ends in x


;
;titleScreen Map
;
TitleScreenPPU:
;nametable
.byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,133,133,133,133,133,133,133,133,133,133,133,133,133,133,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,137,137,137,137,137,137,137,137,137,137,137,137,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,225,226,227,228,229,230,231,232,233,234,235,236,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,241,242,243,244,245,246,247,248,249,250,251,252,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,133,133,133,133,133,133,133,133,133,133,133,133,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,137,137,137,137,137,137,137,137,137,137,137,137,137,137,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,177,178,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,193,194,1,85,112,117,98,109,109,122,1,85,98,109,109,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,179,180,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,195,196,1,81,106,117,1,81,109,98,117,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,181,182,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,197,198,1,67,112,110,99,1,67,98,116,102,110,102,111,117,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,67,122,1,84,109,102,102,113,106,111,104,67,118,115,115,106,117,112,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1

;nametable attribute
;rmb to add 1 offset
.byte $01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01

.byte ppuEscapeLevelData ;end for ppu copy

;write platfrom data
TitleScreenMapPlatformData:

.byte 0 ;Wall left
.byte 0 ;Wall right
.byte bool::true ;t/f for no walls

.byte 0 ;plats y
.byte 0, 0 ;plat left x start / end
.byte 0, 0 ;plat right x start / end

.byte 0 ;main ground plat y
.byte 0, 0 ;ground ends in x



; credit for example.s
; Brad Smith (rainwarrior), 4/06/2014
; http://rainwarrior.ca
;
; end of file
;

