;----------------------------
; Constants Regeion
;----------------------------
kTileXCount = 16
kTileYCount = 12
kLevelSizeMax = kTileXCount*kTileYCount
kSprBase = 64
kSprites .block
	fish = kSprBase+80
	spiderLeft = kSprBase+72
	spiderRight = kSprBase+74
	springNormal = kSprBase+40
	springCompress = kSprBase+41
	springExpand = kSprBase+42
	springFull = kSprBase+43
	springFall = kSprBase+44
.bend
kSpiderValues .block
	yFallDelta = 1
	rightStartWiggle = 255-32-14 ; 32 pixels but compenstating for the sprite width
	rightStartFall = 255-16-14 ; 16 pixels
	leftStartWiggle = 32+14
	leftStartFall = 16+14
	pauseEndFallFrames = 32
	riseDelayTime = 4
.bend

kVectors .block
	charBase = $4000
	spr0ID = charBase+1016
	spr1ID = charBase+1017
	spr2ID = charBase+1018
	spr3ID = charBase+1019
	spr4ID = charBase+1020
	spr5ID = charBase+1021
	spr6ID = charBase+1022
	spr7ID = charBase+1023	
.bend
kBounds .block
	screenMinX = 24
	screenMinY = 50
	screenMaxX = 24 + ( kTileXCount * 8 )
	screenMaxY = 50 + ( kTileYCount * 8 )
.bend
kTimers .block
	dissBlocksValue = $8
	jumpUpValue = $38 ; 3.5 tile
	jumpUpSpringValue = $48 ; 4.5 tiles
	floatTimer = $50 
	DoorAnimeRate = 10
.bend
kEntity .block
	heli = $00
	spring = $01
	word = $02
	bat = $03
	ghost = $04
	spider = $05
	fish = $06
	circler = $07
	maxEntities = 31
.bend
kFishLimits .block
	startTwo = 250-21-(8*6) ; 165
	maxY = 255-8
.bend
kDirections .block
	right = 0
	up = 1
	left = 2
	down = 3
.bend
kPlayerParams .block
	jumpStartDelta = 255-1
	jumpDeltaAccum = 19
	jumpDeltaAccumFloat = 2
	maxFallSpeed = 8
.bend
kJumpIndexs .block
	normal = 0
	floaty = 1
.bend
kPlayerAnimsIndex .block
	standLeft = 0
	standRight = 1
	standWalkLeft = 2
	standWalkRight = 3
.bend		
kSBC .block ; kStatusBorderChars
	M	= 205
	TL	= 203+3
	T	= 204+3
	TR	= 206+3
	L	= 205+3
	R	= 207+3
	BL	= 250
	B	= 251
	BR	= 252
	Col = 15
	QWAKT = 208+3
	QWAKB = 214+3
	Score = 220+3
	High = 226+3
	QwakP = 232
	X = 204
	Flower = 236
.bend
kRaster .block
	bottomRaster = 241
.bend
mplex .block 
	kMaxSpr = $1f
.bend
kBlocks .block
	back = 0	
	dropShadow = 2	
	bottomRight = 3
	leftShadow = 5
	topLeftCorner = 35
.bend

.include "qwak_structs.asm"

;----------------------------
; ZP Regeion
;----------------------------
*= $02 
mplexZP .dstruct sMplexZP
LevelTileMapPtrLo .byte ?
LevelTileMapPtrHi .byte ?
LevelKeyListPtrLo .byte ?
LevelKeyListPtrHi .byte ?

CollCharX	.byte ?
CollCharY	.byte ?
CollTileX	.byte ?
CollTileY	.byte ?
CollTLX		.byte ?
CollTLY		.byte ?

EntityDataPointer	.word ?
CurrentEntity		.byte ?
EntNum				.byte ?
CollisionResult		.byte ?
Pointer1			.word ?
Pointer2			.word ?
Pointer3			.word ?
Pointer4			.word ?
playerTempCol		.byte ?
ZPTemp				.byte ?
ZPTemp2				.byte ?
ZPTemp3				.byte ?
ZPTemp4				.byte ?
ZPTemp5				.byte ?
TempX				.byte ?
ActiveTileIndex		.byte ?
ActiveTile			.byte ?

.cerror * > $FF, "Too many ZP variables"
;----------------------------
; Variables
;----------------------------

variables = $0200
* = $0200
joyLeft	 .byte ?
joyRight .byte ?
joyUp	 .byte ?
joyDown	 .byte ?
joyFire	 .byte ?
GameData .dstruct sGameData
LevelData .dstruct sLevelData
PlayerData .dstruct sPlayerData
TICK_DOWN_START = *
TickDowns .dstruct sTimerTickDowns
TICK_DOWN_END = *

playerTile1			.byte ?
playerTile2			.byte ?
playerMidTile		.byte ?
playerMidBelowTile	.byte ?
playerMidBelowOtherTile .byte ?

playerTile1X		.byte ?
playerTile2X		.byte ?
playerMidTileX		.byte ?
playerMidBelowTileX .byte ?
playerMidBelowOtherTileX .byte ?

playerTile1Y		.byte ?
playerTile2Y		.byte ?
playerMidTileY		.byte ?
playerMidBelowTileY .byte ?
playerMidBelowOtherTileY .byte ?

EntityData .dstruct sEntityData

checkSpriteToCharData .dstruct sCSTCCParams 
CollideSpriteToCheck .byte ?
CollideSpriteBoxIndex .byte ?
CollideCharTLI .byte ?
CollideCharTLC .byte ?
CollideCharTRI .byte ?
CollideCharTRC .byte ?
CollideCharBLI .byte ?
CollideCharBLC .byte ?
CollideCharBRI .byte ?
CollideCharBRC .byte ?
CollideInternalSprTLX .byte ?  ; these 4 MUST be in the same order as below
CollideInternalSprBRX .byte ?
CollideInternalSprTLY .byte ?
CollideInternalSprBRY .byte ?
CollideInternalTTLX .byte ?
CollideInternalTBRX .byte ?
CollideInternalTTLY .byte ?
CollideInternalTBRY .byte ?
DidClipX			.byte ? ; this is if the add X with MSB function did clip the Y
;.warn "Size of variables = ", *-variables

.cerror * > $400, "Too many variables"

* = $0400
tileMapTemp .fill 240

.cerror * > $500, "Too many level data"

;----------------------------
; Unpacked Code section
;----------------------------

* = $c000
mplexBuffer .dstruct sMplexBuffer 
irqBuffers .fill irqLength * 8

.cerror * > $D000, "Too much Unpacked Code"

*= $0801 ; 00 0C 08 0A 00 9E 20 32 30 36 34 00 00
	.word (+), 2005 ;pointer, line number
	.null $9e, ^start;will be sys 4096
+	.word 0	 ;basic line end
	
*= $0810
start
		jsr setirq ; clear all interupts and put out own in
		lda $dd02
		ora #2
		sta $dd02
		lda $dd00
		and #252
		ora #2
		sta $dd00
		lda #%00000010
		sta $d018
		lda #%00011000
		sta $d016
		lda #9
		sta $d021
		lda #0
		sta $d022
		sta $d020
		lda #1
		sta $d023
		lda #kSprBase
		sta mplexBuffer.sprp
		lda #1
		sta $d015
		lda #255
		sta $d01c
		lda #7
		sta mplexBuffer.sprc
		;lda #%00010000
		;sta $d011
		jsr copyStuff
		jsr buildBackAndShadowChars
		jsr BuildDisolveChars
		jsr unpackSprites
RESET
		; clear Mplex 
		ldx #$00	  ;init x with 00
-		txa			  ;transfer x to a to create index vals
		sta mplexZP.sort,x	;store it in the sort table
		lda #255
		sta mplexBuffer.ypos,x	; disbale all sprites
		inx			  
		cpx # mPlex.kMaxSpr+1	 ;have we reached 32 yet?
		bne -
		ldx #0
		lda #0
-		sta variables,x		; clear all the variables
		sta tileMapTemp,x	; clear the tile map and after it so collisions is 00
		inx
		bne -
		jsr emptyCRAM
		jsr convertLevelToTileMap
		jsr addShadowsToMap
		ldx # <tileMapTemp
		ldy # >tileMapTemp
		jsr plotTileMap
		jsr plotStatusArea
		jsr resetPlayerData 
		jsr setPlayerToSpawnPoint
		jsr unpackEntityBytes
		jsr setEntitySprites
		; plot bottom row of screen
		ldx #39
		lda #1
		sta mplexZP.lsbtod
-		sta kVectors.charBase + ( 24*40 ),x
		dex
		bpl -
		lda #5
		sta GameData.lives
		jsr pltLives
		; main loop
-		lda mplexZP.lsbtod	
		beq -	
		dec mplexZP.lsbtod	
;	inc $d020
;	inc $d020
		jsr buildSpriteCollisionTable
		lda #0
		jsr DidIHitSomething
		ora PlayerData.dead
		;lda PlayerData.dead
		beq +
		lda PlayerData.hasShield
		bne +
		dec GameData.lives
		lda #0
		sta PlayerData.dead
		jsr pltLives		
		jsr setPlayerToSpawnPoint		
		jmp -		
+		jsr updateTickdowns
		jsr joyToPlayerDelta
		jsr checkSpriteToCharCollision
;		jsr checkQwakOnDoor
;		jsr checkOnDissTile
		lda checkSpriteToCharData.xDeltaCheck
		beq _addY
		;make sure x reg is 0, and call addXWithMSBAndClip
		ldx #0
		jsr addXWithMSBAndClip
_addY		
;		lda checkSpriteToCharData.yDeltaCheck
;		bmi +
;		cmp #$8
;		bcc +
;		lda #$8
;		sta checkSpriteToCharData.yDeltaCheck
+		lda mplexBuffer.ypos
		clc
		adc checkSpriteToCharData.yDeltaCheck
		sta mplexBuffer.ypos
		jsr updatePlayerAnim
		jsr updateEntities
		jsr animateDoor
;	dec $d020
;	dec $d020	
	
		jmp -
		
addXWithMSBAndClip		
	stx ZPTemp2
	lda mplexBuffer.xmsb,x
	sta ZPTemp3
	lda #0
	sta DidClipX
	lda mplexBuffer.xpos,x
	clc
	adc checkSpriteToCharData.xDeltaCheck
	sta ZPTemp
	; xdelta +ve if this is +ve but original was -ve we have gone over
	lda checkSpriteToCharData.xDeltaCheck
	bmi _subbedX
	lda mplexBuffer.xpos,x
	bpl _loadX 
	; so last pos in negative >80
	lda ZPTemp
	bmi _storeX
	; new pos is positive 0-80
	lda #0			; enable MSB
	sta mplexBuffer.xmsb,x
	sta ZPTemp3
	jmp _storeX
_subbedX
	; xdelta -ve if this is -ve but original was +ve we have gone over
	lda mplexBuffer.xpos,x
	bmi _loadX
	; last post is positive >80
	lda ZPTemp
	bpl _storeX		
	lda #1			; clear MSB
	sta mplexBuffer.xmsb,x
	sta ZPTemp3
	lda #255
	jmp _storeX
_loadX
	lda ZPTemp
_storeX		
	ldx ZPTemp3
	beq _XClipInMSB
	cmp #24
	bcs _storeX2
	inc DidClipX
	lda #24
	bne _storeX2
_XClipInMSB	
	cmp #8	
	bcc _storeX2	
	inc DidClipX	
	lda #8		
_storeX2
	ldx ZPTemp2
	sta mplexBuffer.xpos,x		
	rts		
			
		
joyToPlayerDelta
		jsr scanJoystick
		lda #0
		sta checkSpriteToCharData.xDeltaCheck
		sta checkSpriteToCharData.yDeltaCheck
; Check Left and Right
		lda joyLeft
		beq _cr
		lda #$ff
		sta checkSpriteToCharData.xDeltaCheck
		;ldx PlayerData.movingLR
		;bne _cu
		lda #1
		sta PlayerData.movingLR
		lda #0
		jsr changePlayerDir
		jmp _cu
_cr		lda joyRight
		beq _cu
		lda #1		
		sta checkSpriteToCharData.xDeltaCheck
		;ldx PlayerData.movingLR
		;bne _cu
		sta PlayerData.movingLR
		jsr changePlayerDir		
; Check Up and Down		
_cu		lda joyLeft
		ora joyRight
		bne _noChangeLR
		; joy l r = not movement
		lda PlayerData.movingLR
		beq _noChangeLR
		; and we were mopving
		lda #0
		sta PlayerData.movingLR	  ; then stop moveing LR
		lda PlayerData.facingRight ; change anim to still version
		jsr changePlayerDir
		
_noChangeLR		
		lda joyUp
		beq _noUp
		lda PlayerData.hasJumped	; have we already jumped
		beq _startJump				; no then jump
		lda PlayerData.isFalling	; have we started falling
		bne _jumpFall				; yes handle the fall case then
		lda PlayerData.onGround		; or we on the ground and thus we have not let go since last jump
		bne _cb						; if on gound then don't move more
		; pressing up whilst we jump
		ldx # kJumpIndexs.normal
		jsr incPlayerYDeltaAndReturn
		bne _upe					; still able to jump
		lda #1
		sta PlayerData.isFalling	; start the fall
_upe	sta checkSpriteToCharData.yDeltaCheck
		rts		
_startJump
		lda PlayerData.onGround		; have we trided to jump while in mid air
		beq _cb						; ignore it
		lda #1
		sta PlayerData.hasJumped	; we are jumping
		lda #0
		sta PlayerData.isFalling	; not falling
		sta PlayerData.onGround		; not on the ground
		sta PlayerData.yDeltaAccum	; set the Y jump accleration
		lda # kTimers.floatTimer	; reset the float timer
		sta PlayerData.floatTimer
		lda # kPlayerParams.jumpStartDelta	; set the other half of jump accleration
		sta PlayerData.yDeltaAccum + 1
		jmp _upe					; start jumping
_jumpFall	
		lda PlayerData.canFloat		; if don't have the spring, nothing to do		
		beq _cb
		lda PlayerData.floatTimer	; time left of float clock
		beq _cb 
		dec PlayerData.floatTimer	
		ldx # kJumpIndexs.floaty
		bne _incD
			
_noUp	lda PlayerData.onGround
		bne _ong 
		lda PlayerData.hasJumped
		beq _ong
		lda PlayerData.yDeltaAccum+1
		bpl _cb
		cmp #$ff
		beq _cb
		lda #$80	
		;sta PlayerData.hasJumped	
		sta PlayerData.yDeltaAccum	
		lda #$FF
		sta PlayerData.yDeltaAccum+1
_cb		lda PlayerData.onGround		; are we on the ground	
		bne _downOne
		ldx # kJumpIndexs.normal
_incD	jsr incPlayerYDeltaAndReturn	; no, we are in gravity so fall
		sta checkSpriteToCharData.yDeltaCheck
		rts
_ong	lda #0
		sta PlayerData.hasJumped
_downOne
		lda #1
		sta checkSpriteToCharData.yDeltaCheck
		rts

changePlayerDir
		sta PlayerData.facingRight
		tax
		lda PlayerData.movingLR
		beq _still
		inx	 ; THIS IS BAD AND A HACK
		inx
_still	jsr setPlayerAnimeTo
		rts
	
incPlayerYDeltaAndReturn
		lda PlayerData.yDeltaAccum
		clc
		adc PlayerJumpLUT,x
		sta PlayerData.yDeltaAccum
		lda PlayerData.yDeltaAccum + 1
		adc #0
		bmi +
		cmp # kPlayerParams.maxFallSpeed
		bcc +
		lda # kPlayerParams.maxFallSpeed
+		sta PlayerData.yDeltaAccum + 1
		rts

setPlayerAnimeTo
		cpx PlayerData.currAnim
		beq _dontchange
		stx PlayerData.currAnim
		lda PlayerSprLUT,x
		sta PlayerData.baseSprite
		sta mplexBuffer.sprp
		lda PlayerFrameCountLUT,x
		sta PlayerData.frameCount
		lda PlayerAnimTimer,x
		sta PlayerData.frameTimer
		sta TickDowns.playerAnim
		lda #0
		sta PlayerData.frameOffset
_dontchange		
		rts
		
updatePlayerAnim
		lda PlayerData.frameCount
		cmp #2
		bcc _skip
		lda TickDowns.playerAnim
		beq _itTime
_skip	rts
_itTime
		lda PlayerData.frameOffset
		clc
		adc #1
		cmp PlayerData.frameCount
		bcc _store
		lda #0
_store	sta PlayerData.frameOffset
		clc
		adc PlayerData.baseSprite
		sta mplexBuffer.sprp
		lda PlayerData.frameTimer
		sta TickDowns.playerAnim
		rts 

PlayerJumpLUT .byte kPlayerParams.jumpDeltaAccum, kPlayerParams.jumpDeltaAccumFloat
						; Left	Right  Walk L	Walk R
PlayerSprLUT		.byte $44  ,$40		,$4C	,$48
PlayerFrameCountLUT .byte 1	   ,1		,4		,4
PlayerAnimTimer		.byte 255  ,255		,8		,8

emptyCRAM
		ldx #00
		lda #0
-		sta $d800,x
		sta $d900,x
		sta $da00,x
		sta $db00,x
		sta $dc00,x
		dex
		bne -
		rts
		
convertLevelToTileMap
		lda #0
		sta LevelData.numKeys
		sta LevelData.totalKeys
		lda #<tileMapTemp
		sta Pointer1
		sta LevelTileMapPtrLo
		lda #>tileMapTemp
		sta Pointer1+1
		sta LevelTileMapPtrHi
		lda #<fileTileMap
		sta Pointer2
		lda #>fileTileMap
		sta Pointer2+1
; read level pointers
		ldy #0
		sty ActiveTileIndex
		lda (Pointer2),y
		clc
		adc Pointer2
		sta LevelKeyListPtrLo
		iny
		lda (Pointer2),y
		adc Pointer2+1
		sta LevelKeyListPtrHi
		iny
		lda (Pointer2),y
		clc
		adc Pointer2
		sta EntityDataPointer
		iny
		lda (Pointer2),y
		adc Pointer2+1
		sta EntityDataPointer+1
		clc
		lda Pointer2
		adc #4
		sta Pointer2
		lda Pointer2+1
		adc #0
		sta Pointer2+1
		
		lda #12
		pha
_row	ldy #0
_loop	; read in 16 bytes		
		lda (Pointer2),y		
		cmp # kTiles.player
		beq _playerPos	
		cmp # kTiles.exit	
		beq _exitPos	
		cmp # kTiles.key1
		beq _key
		cmp # kTiles.key2
		beq _key
		cmp # kTiles.key3
		beq _key
		cmp # kTiles.key4
		beq _key
		cmp # kTiles.diss
		beq _dissBlock
		; covert and then push out		
_cont	;tax		
		;lda toolToTileLUT,x		
		sta (Pointer1),y		
		inc ActiveTileIndex	
		iny		
		cpy #16		
		bne _loop		
		clc		
		lda Pointer2	
		adc #16		
		sta Pointer2	
		lda Pointer2+1	
		adc #0		
		sta Pointer2+1	
		clc		
		lda Pointer1	
		adc #16		
		sta Pointer1	
		lda Pointer1+1	
		adc #0		
		sta Pointer1+1	
		pla 
		sec 
		sbc #1	
		cmp #0	
		pha 
		bne _row	
		pla 
		rts 
_playerPos
		sty LevelData.playerX
		pla
		sta ZPTemp
		pha
		lda #12
		sec
		sbc ZPTemp
		sta LevelData.playerY
		lda # kTiles.back
		jmp _cont
_key	inc LevelData.numKeys
		inc LevelData.totalKeys
		jmp _cont
_dissBlock
		lda # kTiles.diss
		jmp _cont		
_exitPos
		lda ActiveTileIndex
		sta LevelData.exitIndex
		lda # kTiles.exit
		jmp _cont

addShadowsToMap				
	ldy #0	
	sty TempX	
-	ldy TempX
	jsr tileIsSafeToChange
	bcc _next
	jsr calcBCDEforTileY
_next	
	inc TempX
	lda TempX
	cmp #kLevelSizeMax
	bne -
	rts
	
tileIsWall						
	lda tileMapTemp,y						
	beq	_no					
	cmp #kTiles.wall4+1						
	bcc _yes						
	cmp #kTiles.diss						
	bne _no												
	clc						
_yes						
	rts ; carry is clear						
_no sec						
	rts ; carry is set						
						
tileIsSafeToChange					
	lda tileMapTemp,y					
	beq _yes ; 0 is safe					
	cmp #kTiles.underHangStart					
	bcs _yes					
	rts ; carry is clear					
_yes					
	sec					
	rts					
;  BCD
;  EA
;   H
; A is tile we are testing
;  BCDE H
;  1110   = under hang
;  1100   = under hang right end
;  0110   = under hanr left  end
;  0001 0 = left wall top end
;  1001   = left wall
;  1000   = 35
;  11X1   = top left
;  0XX1 1 = bottom left					
calcBCDEforTileY				
	sty ZPTemp				
	sty ZPTemp2				
	tya			
	and #15			
	bne _canDoLeft			
	lda #$80		; can'r do left on Negative				
	bne +			
_canDoLeft			
	lda #0			
+	sta ZPTemp4			
	lda ZPTemp			
	and #15			
	cmp #15 			
	bne _canDoRight			
	lda #$40		 ; can't do right on Overflow	
	sta ZPTemp4			
_canDoRight			
END_LEFT_RIGHT_CHECK	
	lda #1+2+4 ; first 3 are empty ( it is inverted later)				
	sta ZPTemp3
	ldy ZPTemp	 	
	cpy #kTileXCount				
	bcc _doneFirstRow ; if it is the first row than ALL of above is not solid						
	lda #0						
	sta ZPTemp3						
	tya					
	sec					
	sbc #kTileXCount+1 ; so get -1x,-1y					
	sta ZPTemp2
	tay
	bit ZPTemp4 ; test to see if we can do right
	bmi _noB	; no then skip B					
	jsr tileIsWall					
	rol ZPTemp3					
	jmp _testC				
_noB					
	sec			; if there is no B then make it clear	
	rol ZPTemp3 				
_testC		
	iny 					
	jsr tileIsWall				
	rol ZPTemp3				
	iny				
	bit ZPTemp4			
	bvs _noRight			
	jsr tileIsWall			
	rol ZPTemp3			
	jmp _doneFirstRow			
_noRight				
	sec ; make it as 1 so it gets 0 later		
	rol ZPTemp3		
_doneFirstRow				
	bit ZPTemp4				
	bmi _noE ; check negative flag				
	ldy ZPTemp				
	dey				
	jsr tileIsWall				
	rol ZPTemp3				
	jmp DoIndexCheck			
_noE			
	sec ; make it 1 so it gets 0 later			
	rol ZPTemp3			
DoIndexCheck			
	lda ZPTemp3		
	eor #$0F ;
	tay 		
BCDEYVALUECHECK
	lda BCDELUT,y		
	bmi _checkH	
_writeMap	
	ldy ZPTemp		
	sta tileMapTemp,y		
	rts		
_checkH				
	lda ZPTemp		
	clc		
	adc #kTileXCount		
	tay		
	jsr tileIsWall		
	bcs _HNotWall		
	lda #kTiles.back		
_HNotWall			
	lda #kTiles.sideShadow					
	jmp _writeMap			
				
BCDELUT	.byte $00							; 0000
		.byte kTiles.sideShadow				; 0001	
		.byte $00							; 0010
		.byte kTiles.sideShadow				; 0011
		.byte kTiles.underHangStart			; 0100			
		.byte kTiles.topLeftCorner			; 0101
		.byte kTiles.underHangStart			; 0110		
		.byte kTiles.sideShadow				; 0111
		.byte kTiles.shadowOpenCorner		; 1000				
		.byte kTiles.middlesideShadow		; 1001		
		.byte $00							; 1010
		.byte kTiles.sideShadow				; 1011			
		.byte kTiles.underHang				; 1100				
		.byte kTiles.shadowOpenCorner		; 1101				
		.byte kTiles.underHang				; 1110				
		.byte kTiles.topLeftCorner			; 1111				
								
		; back,wall,wall1,wall2,wall3,wall4,spike,flower,fruit,key1,key2,key3,key4,shield,spring,potion,egg,exit,player,diss		
toolToTileLUT 	.byte 000
				.byte 007,007,007,007,007
				.byte 019,026,031
				.byte 020,020,020,020
				.byte 027,028,029,030
				.byte 021,117
				.byte 006
				.byte 008,009,010,011,012
				.byte 013,014,015,016,017
				.byte 018
				.byte 001,002,003,004,005
				.byte 033		
kTiles .block
	back = 0
	
	wall = 1
	wall1 = 2
	wall2 = 3
	wall3 = 4	
	wall4 = 5
	
	spike = 6
	flower = 7
	fruit = 8
	
	key1 = 9	
	key2 = 10
	key3 = 11
	key4 = 12
	
	shield = 13
	spring = 14
	potion = 15
	egg = 16
	
	exit = 17
	player = 18
	
	pipe = 19
	diss = 20
	dissNoColide = 30
	
	underHangStart = 31
	underHang = 32
	shadowOpenCorner = 33
	sideShadow = 34
	middlesideShadow = 35
	topLeftCorner = 36
.bend
kDoorClosed = 21
kDoorOpen = 25

kKeyToWallDelta = kTiles.key1 - kTiles.wall1

plotTileMap
		stx Pointer1
		sty Pointer1+1
		lda # kDoorClosed
		sta LevelData.exitFrame
		lda # <kVectors.charBase
		sta Pointer2
		lda # >kVectors.charBase
		sta Pointer2+1
		lda #00
		sta Pointer3
		lda #$d8
		sta Pointer3+1
		lda #12 ; num rows
		pha
_pltY	ldy #00 ; num cols
		tya
		pha
_pltX	lda (Pointer1),y ; tile num
		tax
		lda toolToTileLUT,x ; convert map to actual tile
		jsr renderTile		
		clc
		lda Pointer2
		adc #2
		sta Pointer2
		lda Pointer2+1
		adc #0
		sta Pointer2+1
		clc
		lda Pointer3
		adc #2
		sta Pointer3
		lda Pointer3+1
		adc #0
		sta Pointer3+1
		pla
		clc
		adc #1
		pha
		tay
		cpy #20 ; #16
		bne _pltX
		pla
		clc
		lda Pointer1
		adc #16 ; 16
		sta Pointer1
		lda Pointer1+1
		adc #00
		sta Pointer1+1
		pla
		sec
		sbc #1
		beq _exit
		pha
		clc
		lda Pointer2
		adc #40 ;48
		sta Pointer2
		lda Pointer2+1
		adc #0
		sta Pointer2+1
		clc
		lda Pointer3
		adc #40 ;48
		sta Pointer3
		lda Pointer3+1
		adc #0
		sta Pointer3+1
		jmp _pltY
_exit	rts

; a = tile num, Pointer2 = Screen, Pointer 3 = CRAM
renderTile 
		sta Pointer4 
		lda #0	
		sta Pointer4+1
		asl Pointer4 
		rol Pointer4+1 
		asl Pointer4	 ; tile num x 4
		rol Pointer4+1 
		; covert to tiles offset	
		clc 
		lda Pointer4
		adc # <fileTiles
		sta Pointer4
		lda Pointer4+1
		adc # >fileTiles
		sta Pointer4+1
		ldy #0
		lda (Pointer4),y
		tax
		sta (Pointer2),y
		lda fileCharCols,x
		sta (Pointer3),y
		ldy #1
		lda (Pointer4),y
		tax
		sta (Pointer2),y
		lda fileCharCols,x
		sta (Pointer3),y
		ldy #2
		lda (Pointer4),y
		tax
		ldy #40
		sta (Pointer2),y
		lda fileCharCols,x
		sta (Pointer3),y
		ldy #3
		lda (Pointer4),y
		tax
		ldy #41
		sta (Pointer2),y
		lda fileCharCols,x
		sta (Pointer3),y
		rts
		
scanJoystick
		lda #0
		sta joyDown
		sta joyLeft
		sta joyRight
		sta joyUp
		sta joyFire
		ldx #1
		lda $DC00
		lsr 
		bcc _joyUp
		lsr
		bcc _joyDown
_checkLR 
		lsr
		bcc _joyLeft
		lsr
		bcc _joyRight
_checkFire		
		lsr 
		bcs _joyEnd
		stx joyFire
_joyEnd rts
		
_joyUp	
		lsr ; skip down bit
		stx joyUp
		jmp _checkLR
		
_joyDown 
		stx joyDown
		jmp _checkLR
		
_joyLeft 
		stx joyLeft
		lsr ; skip right bit
		jmp _checkFire

_joyRight
		stx joyRight
		jmp _checkFire
	

CollisionBoxesX .byte 02,02,02 
CollisionBoxesW .byte 13,13,13 
CollisionBoxesY .byte 00,02,00 
CollisionBoxesH .byte 14,16,20 
	
newCollision
	ldx CollideSpriteToCheck
	ldy CollideSpriteBoxIndex
	; calc the final Xs
	lda mplexBuffer.xmsb,x
	bne _notMSB
	lda mplexBuffer.xpos,x
	clc
	adc CollisionBoxesX,y
	adc checkSpriteToCharData.xDeltaCheck
	adc #255-24
	bne _storeX
_notMSB
	lda mplexBuffer.xpos,x
	clc
	adc CollisionBoxesX,y
	adc checkSpriteToCharData.xDeltaCheck
	sec
	sbc # kBounds.screenMinX
_storeX
	sta CollideInternalSprTLX
	clc
	adc CollisionBoxesW,y
	sta CollideInternalSprBRX
	; calc the final Ys
	lda mplexBuffer.ypos,x
	clc
	adc CollisionBoxesY,y
	adc checkSpriteToCharData.yDeltaCheck
	sec
	sbc # kBounds.screenMinY
	jsr ClipY
	sta CollideInternalSprTLY
	clc
	adc CollisionBoxesH,y
	jsr ClipY
	sta CollideInternalSprBRY
	; calc the tile index
	ldx #1
-	lda CollideInternalSprTLX,x
	lsr a
	lsr a
	lsr a
	lsr a
	sta CollideInternalTTLX,x
	dex
	bpl -
	lda CollideInternalSprTLY
	and #$f0
	sta CollideInternalTTLY
	lda CollideInternalSprBRY
	and #$f0
	sta CollideInternalTBRY
	; covert the tile X,Y into a the index and pull Char
	lda CollideInternalTTLY
	ora CollideInternalTTLX
	sta CollideCharTLI
	tax
	lda tileMapTemp,x
	sta CollideCharTLC
	
	lda CollideInternalTTLY
	ora CollideInternalTBRX
	sta CollideCharTRI
	tax
	lda tileMapTemp,x
	sta CollideCharTRC
	
	lda CollideInternalTBRY
	ora CollideInternalTTLX
	sta CollideCharBLI
	tax
	lda tileMapTemp,x
	sta CollideCharBLC
	
	lda CollideInternalTBRY
	ora CollideInternalTBRX
	sta CollideCharBRI
	tax
	lda tileMapTemp,x
	sta CollideCharBRC
	rts
	
ClipY
	cmp # 225
	bcs _underflowY
	cmp # 200
	bcs _overflowY
	rts
_underflowY	
	clc	
	adc #200	
	bne _neitherY	
_overflowY		
	sec		
	sbc #200				
_neitherY
	rts

; {{{	
.comment	
CSTCCInteral
	ldx mplexBuffer.xpos
	ldy mplexBuffer.ypos
	jsr checkXYAgainstTile
	ldx playerTempCol
	sta playerTile1,x
	lda $f9
	lsr a
	lsr a
	lsr a
	lsr a	
	sta playerTile1Y,x
	lda $fa
	sta playerTile1X,x
	rts
.endc	
;}}}	
	
checkSpriteToCharCollision
	lda checkSpriteToCharData.yDeltaCheck
	sta checkSpriteToCharData.yDeltaBackup
	lda checkSpriteToCharData.xDeltaCheck
	sta checkSpriteToCharData.xDeltaBackup
	lda #0
	sta checkSpriteToCharData.xDeltaCheck
	sta CollideSpriteToCheck
	sta CollideSpriteBoxIndex
	jsr CSTCCY
	lda CollideCharBLI
	sta ActiveTileIndex
	lda CollideCharBLC
	sta ActiveTile	
	jsr checkOnDissTile
	lda CollideCharBLI
	cmp CollideCharBRI
	beq _otherIsSame
	lda CollideCharBRI
	sta ActiveTileIndex
	lda CollideCharBRC
	sta ActiveTile	
	jsr checkOnDissTile
_otherIsSame	
	lda checkSpriteToCharData.xDeltaBackup
	sta checkSpriteToCharData.xDeltaCheck
	lda #0
	sta checkSpriteToCharData.yDeltaCheck
	jsr CSTCCX
	lda checkSpriteToCharData.yDeltaBackup
	sta checkSpriteToCharData.yDeltaCheck
	ldx CollideCharTLI
	lda CollideCharTLC
	jsr checkActionTile
	lda CollideCharTRI
	cmp CollideCharTLI
	beq _skipTR
	tax
	lda CollideCharTRC
	jsr checkActionTile
_skipTR	
	lda CollideCharBLI
	cmp CollideCharTLI
	beq _skipBL
	tax
	lda CollideCharBLC
	jsr checkActionTile
_skipBL	
	lda CollideCharBRI
	cmp CollideCharTRI
	beq _skipBR
	tax
	lda CollideCharBRC
	jsr checkActionTile
_skipBR 
	rts
	
CSTCCY
	ldx #0
	stx ZPTemp
	stx ZPTemp2
	stx ZPTemp3
	stx ZPTemp4
	ldy #0
	jsr newCollision
	lda CollideCharTLC
	jsr checkSolidTile
	rol ZPTemp
	lda CollideCharTRC
	jsr checkSolidTile
	rol ZPTemp2
	lda CollideCharBLC
	jsr checkSolidTile
	rol ZPTemp3
	lda CollideCharBRC
	jsr checkSolidTile
	rol ZPTemp4
	lda checkSpriteToCharData.yDeltaCheck
	bpl _checkDown
	; check up
	lda ZPTemp
	ora ZPTemp2
	beq _exit	
	lda #1	
	sta PlayerData.isFalling	
	lda #0
	sta PlayerData.yDeltaAccum
	sta PlayerData.yDeltaAccum+1
	rts	
_onGround
	lda #1
	sta PlayerData.onGround
	sta PlayerData.yDeltaAccum
	lda #0
	sta PlayerData.isFalling
	sta PlayerData.yDeltaAccum + 1
	sta checkSpriteToCharData.yDeltaBackup
_noOnGround
	lda #$0
	sta checkSpriteToCharData.yDeltaCheck
	beq _exit
_checkDown
	lda ZPTemp3
	ora ZPTemp4
	bne _onGround
_exit 
	rts
	
CSTCCX
	ldx #0
	stx ZPTemp
	stx ZPTemp2
	stx ZPTemp3
	stx ZPTemp4
	ldy #0
	jsr newCollision
	lda CollideCharTLC
	jsr checkSolidTile
	rol ZPTemp
	lda CollideCharTRC
	jsr checkSolidTile
	rol ZPTemp2
	lda CollideCharBLC
	jsr checkSolidTile
	rol ZPTemp3
	lda CollideCharBRC
	jsr checkSolidTile
	rol ZPTemp4
_checkX
	lda checkSpriteToCharData.xDeltaCheck
	beq _exit
	bpl _checkRight
	; left
	lda ZPtemp
	ora ZPtemp3
	beq _exit
_noX
	lda #$0
	sta checkSpriteToCharData.xDeltaCheck
	beq _exit	
_checkRight
	lda ZPtemp2
	ora ZPtemp4
	bne _noX
_exit 
	rts
	

checkOnDissTile
	lda PlayerData.onGround
	bne _c
_exit	
	rts
	; get the tile below the player
_c	lda TickDowns.dissBlocks
	bne _exit
	lda ActiveTile
	cmp # kTiles.diss
	bcc _exit
	cmp # kTiles.diss+10
	bcs _exit
	lda #kTimers.dissBlocksValue
	sta TickDowns.dissBlocks
	ldx ActiveTileIndex
	inc tileMapTemp,x
	lda tileMapTemp,x
	cmp #kTiles.diss+9
	php
	jsr pltSingleTileNew
	plp
	bne _exit
	lda ActiveTileIndex
	sta Pointer4
	clc
	adc #16
	cmp #kLevelSizeMax
	bcs _exit
	sta ActiveTileIndex
	tay
	jsr tileIsSafeToChange
	bcc _exit
	jsr clearTileNew
	lda Pointer4
	sta ActiveTileIndex
	rts
	
checkActionTile
	sta ActiveTile ; for later
	stx ActiveTileIndex ; for later
	ldy #0
-	cmp TileFuncLookup,y
	beq _found
	iny
	cpy # size(TileFuncLookup)
	bne -
	rts

_found
	lda TileFuncLUTHi,y
	pha
	lda TileFuncLUTLo,y
	pha
	rts
	
TileFuncLookup .byte kTiles.fruit,kTiles.flower,kTiles.key1,kTiles.key2,kTiles.key3,kTiles.key4,kTiles.spike,kTiles.spring,kTiles.potion,kTiles.shield,kTiles.exit	
TileFuncLUTLo .byte <fruitFunc-1 ,<flowerFunc-1,<keyFunc-1 ,<keyFunc-1 ,<keyFunc-1 ,<keyFunc-1 ,<spikeFunc-1,<springFunc-1,<potionFunc-1,<shildFunction-1,<exitFunc-1
TileFuncLUTHi .byte >fruitFunc   ,>flowerFunc  ,>keyFunc   ,>keyFunc   ,>keyFunc   ,>keyFunc   ,>spikeFunc  ,>springFunc  ,>potionFunc  ,>shildFunction  ,>exitFunc

fruitFunc
	jsr clearTileNew
	lda #kScoreIndex.Fruit
	jsr giveScore
	rts 	
	
flowerFunc
	jsr clearTileNew
	lda #kScoreIndex.fruit
	jsr giveScore
	inc GameData.flowers
	lda GameData.flowers
	cmp #8
	bne _exit
	lda #0
	sta GameData.flowers
	jsr awardLife
_exit	
	jmp pltFlowers	

keyFunc
	jsr clearTileNew		; remove it
	lda #kScoreIndex.key
	jsr giveScore
	dec LevelData.numKeys
	lda ActiveTile				; 
	jsr countTempMapTile	; do we have any more of these keys still
	bne _done				; yes
	lda ActiveTile
	sec
	sbc # kKeyToWallDelta
	jsr removeAllTilesOf
_done	
	lda LevelData.numKeys
	cmp #0
	beq _changeDoor
	rts
_changeDoor
	lda #1
	sta GameData.exitOpen	
	rts	

spikeFunc	
	lda #1	
	sta PlayerData.dead 
	rts 
springFunc
	jsr clearTileNew
	lda #1
	sta PlayerData.hasSpring
	rts
potionFunc
	jsr clearTileNew
	lda #1
	sta PlayerData.canFloat
	rts
shildFunction
	jsr clearTileNew
	lda #1
	sta PlayerData.hasShield
	rts		
exitFunc	
	lda GameData.currLevel
	clc 
	adc #1
	and #3
	sta GameData.currLevel
	lda #0
	sta GameData.exitOpen
	pla
	pla ; pull off rts addr FIX ME when you get the STATE system
	jmp RESET			
; {{{				
.comment
	lda #0
	sta PlayerData.onGround
	;mid
	lda #kBounds.screenMinX-14+4
	sec 
	sbc checkSpriteToCharData.xDeltaCheck
	sta $fe ; x offset from sprite
	lda #kBounds.screenMinY-14
	sec 
	sbc checkSpriteToCharData.yDeltaCheck
	sta $fd ; y offset from sprite
	lda #2
	sta playerTempCol
	jsr CSTCCInteral
	jsr CSTCCInteral
	; mid below
	lda #kBounds.screenMinX-14+4
	sec 
	sbc checkSpriteToCharData.xDeltaCheck
	sta $fe ; x offset from sprite
	lda #kBounds.screenMinY-24
	sec 
	sbc checkSpriteToCharData.yDeltaCheck
	sta $fd ; y offset from sprite
	inc playerTempCol
	jsr CSTCCInteral
	lda PlayerData.facingRight
	beq _checkBelowLeft
	lda #kBounds.screenMinX-18+4
	sec 
	sbc checkSpriteToCharData.xDeltaCheck
	sta $fe ; x offset from sprite
	jmp _checkExtraBelow
_checkBelowLeft 
	lda #kBounds.screenMinX-10+4
	sec 
	sbc checkSpriteToCharData.xDeltaCheck
	sta $fe ; x offset from sprite
_checkExtraBelow
	lda #kBounds.screenMinY-24
	sec 
	sbc checkSpriteToCharData.yDeltaCheck
	sta $fd ; y offset from sprite
	inc playerTempCol
	jsr CSTCCInteral
	
	
; do movement checks	
	lda checkSpriteToCharData.yDeltaCheck
	beq _checkX
	bpl _checkDown
	jsr checkUpPoints
	lda playerTile1
	jsr checkSolidTile
	bcs _Y1
	lda playerTile2
	jsr checkSolidTile
	bcs _Y1
	bcc _checkX
_Y1
	lda #0
	sta checkSpriteToCharData.yDeltaCheck
	beq _checkX
_YFF
	lda #1
	sta PlayerData.onGround
	sta PlayerData.yDeltaAccum
	lda #0
	;sta PlayerData.hasJumped 
	sta PlayerData.isFalling
	sta PlayerData.yDeltaAccum + 1
_noOnGround
	lda #$0
	sta checkSpriteToCharData.yDeltaCheck
	beq _checkX
_checkDown
	jsr checkDownPoints
	lda playerTile1
	jsr checkSolidTile
	bcs _YFF
	lda playerTile2
	jsr checkSolidTile
	bcs _YFF
_checkX 
	lda checkSpriteToCharData.xDeltaCheck
	beq _exit
	bpl _checkRight
	;left
	jsr checkLeftPoints
	lda playerTile1
	jsr checkSolidTile
	bcs _X1
	lda playerTile2
	jsr checkSolidTile
	bcs _X1
	bcc _exit
_X1
	lda #1
	sta checkSpriteToCharData.xDeltaCheck
	bne _exit
_XFF
	lda #$FF
	sta checkSpriteToCharData.xDeltaCheck
	bne _exit
_checkRight
	jsr checkRightPoints
	lda playerTile1
	jsr checkSolidTile
	bcs _XFF
	lda playerTile2
	jsr checkSolidTile
	bcs _XFF
_exit	
	rts

;			 
CheckPointsX .byte kBounds.screenMinX-11+4,kBounds.screenMinX-17+4 ; up
			 .byte kBounds.screenMinX-18+4,kBounds.screenMinX-18+4 ; right
			 .byte kBounds.screenMinX-11+4,kBounds.screenMinX-17+4 ; down
			 .byte kBounds.screenMinX-10+8,kBounds.screenMinX-10+8 ; left
CheckPointsY .byte kBounds.screenMinY-06+2,kBounds.screenMinY-06+2 ; up
			 .byte kBounds.screenMinY-07+2,kBounds.screenMinY-20+5 ; right
			 .byte kBounds.screenMinY-21+5,kBounds.screenMinY-21+5 ; down
			 .byte kBounds.screenMinY-07+2,kBounds.screenMinY-20+5 ; left
			
checkPointsInternal
	stx ZPTemp
	lda CheckPointsX,x
	sec
	sbc checkSpriteToCharData.xDeltaCheck
	sta $fe
	lda CheckPointsY,x 
	sec 
	sbc checkSpriteToCharData.yDeltaCheck
	sta $fd
	lda #0
	sta playerTempCol
	jsr CSTCCInteral
	ldx ZPTemp
	inx
	lda CheckPointsX,x
	sec 
	sbc checkSpriteToCharData.xDeltaCheck
	sta $fe
	lda CheckPointsY,x
	sec 
	sbc checkSpriteToCharData.yDeltaCheck
	sta $fd
	inc playerTempCol
	jmp CSTCCInteral
	
checkUpPoints
	ldx #0
	jmp checkPointsInternal
checkRightPoints
	ldx #2
	jmp checkPointsInternal
checkDownPoints
	ldx #4
	jmp checkPointsInternal
checkLeftPoints
	ldx #6
	jmp checkPointsInternal

checkQwakOnDoor
	lda playerMidTileX
	sta CollTileX
	asl a
	sta CollTLX
	lda playerMidTileY
	sta CollTileY
	asl a
	sta CollTLY
	lda playerMidTile
	ldx #0
_checkCharObjects	
	cmp CharObjectMinLUT,x
	bne _next
	;cmp CharObjectMaxLUT,x
	;bcs _next
	lda CharObjectFuncLo,x
	sta _jmp+1
	lda CharObjectFuncHi,x
	sta _jmp+2
_jmp 
	jmp $0000	
_next	
	inx
	cpx #CharObjectMaxLUT-CharObjectMinLUT
	bne _checkCharObjects
	rts

exitFunc	
	lda GameData.currLevel
	clc 
	adc #1
	and #3
	sta GameData.currLevel
	lda #0
	sta GameData.exitOpen
	pla
	pla ; pull off rts addr
	jmp start
fruitFunc	
	jsr clearTile
	lda #kScoreIndex.Fruit
	jsr giveScore
	rts 
keyFunc
	lda #kScoreIndex.key
	jsr giveScore
	dec LevelData.numKeys
	jsr readTileMapTemp
	sta ZPTemp				; what key is this
	jsr clearTile			; remove it
	lda ZPTemp				; 
	jsr countTempMapTile	; do we have any more of these keys still
	bne _done				; yes
	lda ZPTemp
	sec
	sbc # kKeyToWallDelta
	jsr removeAllTilesOf
_done	
	lda LevelData.numKeys
	cmp #0
	beq _changeDoor
	rts
_changeDoor
;	lda CollTLX
;	pha
;	lda CollTLY
;	pha
	lda #1
	sta GameData.exitOpen
;	pla 
;	sta CollTLY
;	pla 
;	sta CollTLX 
	rts
flowerFunc
	jsr clearTile
	lda #kScoreIndex.flower
	jsr giveScore
	inc GameData.flowers
	lda GameData.flowers
	cmp #5
	bne _exit
	lda #0
	sta GameData.flowers
	jsr awardLife
_exit	
	jmp pltFlowers	
spikeFunc	
	lda #1	
	sta PlayerData.dead 
	rts 
springFunc
	jsr clearTile
	lda #1
	sta PlayerData.hasSpring
	rts
potionFunc
	jsr clearTile
	lda #1
	sta PlayerData.canFloat
	rts
shildFunction
	jsr clearTile
	lda #1
	sta PlayerData.hasShield
	rts
	
checkOnDissTile
	lda PlayerData.onGround
	bne _c
_exit	
	rts
	; get the tile below the player
_c	lda TickDowns.dissBlocks
	bne _exit
	lda playerMidBelowTile
	cmp # kTiles.diss
	bcc _next
	cmp # kTiles.diss+10
	bcs _next
	jmp _checkTile
_next
	lda playerMidBelowOtherTile
	cmp # kTiles.diss
	bcc _exit
	cmp # kTiles.diss+10
	bcs _exit
	lda playerMidBelowOtherTileY
	sta playerMidBelowTileY
	lda playerMidBelowOtherTileX
	sta playerMidBelowTileX
_checkTile	
	lda playerMidBelowTileY
	asl a
	asl a
	asl a
	asl a
	ora playerMidBelowTileX
	tax
	inc tileMapTemp,x
	lda tileMapTemp,x
	pha
	lda playerMidBelowTileY
	asl 
	sta CollTLY
	lda playerMidBelowTileX
	asl
	sta CollTLX
	pla
	jsr pltSingleTile
	lda #kTimers.dissBlocksValue
	sta TickDowns.dissBlocks
	rts
	

CharObjectMinLUT .byte kTiles.exit,kTiles.fruit,kTiles.key1,kTiles.key2,kTiles.key3,kTiles.key4,kTiles.flower,kTiles.spike,kTiles.spring,kTiles.potion,kTiles.shield
CharObjectMaxLUT ; .byte 168,77,49,57,44
CharObjectFuncLo .byte <exitFunc,<fruitFunc,<keyFunc,<keyFunc,<keyFunc,<keyFunc,<flowerFunc,<spikeFunc,<springFunc,<potionFunc,<shildFunction
CharObjectFuncHi .byte >exitFunc,>fruitFunc,>keyFunc,>keyFunc,>keyFunc,>keyFunc,>flowerFunc,>spikeFunc,>springFunc,>potionFunc,>shildFunction
.endc
;}}}

awardLife
	inc GameData.lives
	jmp pltLives
	
animateDoor
	lda GameData.exitOpen
	beq _exit
	lda TickDowns.doorAnim
	bne _exit
	lda #kTimers.DoorAnimeRate
	sta TickDowns.doorAnim
	lda LevelData.exitIndex
	sta ActiveTileIndex
	lda LevelData.exitFrame
	cmp #kDoorOpen
	beq _exit
	clc
	adc #1
	sta LevelData.exitFrame
	jmp pltSingleTileNoLookupNew ; skips below
_exit
	rts
	
countTempMapTile
	ldx # kLevelSizeMax-1
	ldy #0
_loop
	cmp tileMapTemp,x
	bne _skip
	iny
_skip
	dex
	cpx #$ff
	bne _loop
	tya
	rts
	
collTileXYtoIndex
	lda CollTileY
	asl a
	asl a
	asl a
	asl a
	ora CollTileX
	rts

readTileMapTemp 
	jsr collTileXYtoIndex
	tax
	lda tileMapTemp,x
	rts
	
clearTileNew
	ldy ActiveTileIndex
	lda # kTiles.back
	sta tileMapTemp,y
	jsr calcBCDEforTileY ; this sets it to be what it should be shadow wise
	jsr pltSingleTileNew
	ldy ActiveTileIndex
	lda tileMapTemp,y
pltSingleTileNew
	tax
	lda toolToTileLUT,x
pltSingleTileNoLookupNew
	pha
	lda ActiveTileIndex 
	jsr convertIndexToScreenAndCRAM
	pla
	jmp renderTile
	
.comment	
clearTile
	jsr collTileXYtoIndex
	tax
	lda # kTiles.back
	sta tileMapTemp,x
pltSingleTile	
	tax
	lda toolToTileLUT,x
pltSingleTileNoLookup
	pha
	ldx CollTLX
	ldy CollTLY
	jsr convertXYToScreen
	lda $fb
	sta $fe
	sta $f8
	lda $fc
	sta $ff
	eor # (>kVectors.charBase) ^ $d8
	sta $f9
	lda #<fileTiles
	sta $fa
	lda #>fileTiles
	sta $fb
	pla
	jsr renderTile
	rts
.endc		
		
removeAllTilesOf
	sta ZPTemp5
	ldx #0
	stx ActiveTileIndex
_loop
	lda tileMapTemp,x
	cmp ZPTemp5
	bne _next
	jsr clearTileNew
	lda ActiveTileIndex
	sta Pointer4
	inc ActiveTileIndex
	ldy ActiveTileIndex
	cpy #kLevelSizeMax
	beq _restoreACI
	jsr tileIsSafeToChange
	bcc _skipLeft
	jsr clearTileNew	
_skipLeft	
	lda ActiveTileIndex
	clc
	adc #15 ; it is + 1 already
	cmp #kLevelSizeMax
	beq _restoreACI
	sta ActiveTileIndex
	tay
	jsr tileIsSafeToChange
	bcc _restoreACI
	jsr clearTileNew
_restoreACI
	lda Pointer4
	sta ActiveTileIndex
_next	
	inc ActiveTileIndex
	ldx ActiveTileIndex
	cpx # kLevelSizeMax
	bne _loop
	rts	

giveScore
	asl a
	asl a
	asl a
	ora #5
	tay
	ldx #5
	clc 
_scLoop
	lda GameData.score,x	
	adc FruitScore,y	
	sta GameData.score,x	
	cmp #10
	bcc _ok
	sec
	sbc #10
	sta GameData.score,x
	sec
_ok dey
	dex 
	bpl _scLoop 
	jmp pltScore	 
	
kScoreIndex .block	
	fruit = 0	
	flower = 1	
	key = 2 
.bend	
	
FruitScore	.byte 0,0,0,1,0,0,15,15
FlowerScore .byte 0,0,0,5,0,0,15,15
KeyScore	.byte 0,0,0,2,5,0,15,15
	
.comment
convertXYToScreen
	stx TempX
	lda screenRowLUTLO,y
	clc
	adc TempX 
	sta $fb
	lda screenRowLUTHi,y
	adc #00
	sta $fc
	rts
.endc	
convertIndexToScreenAndCRAM
	sta TempX
	lsr a
	lsr a
	lsr a ; thus making the y / 16 * 2
	and #%00011110
	tay
	lda TempX
	and #$0F
	asl a
	sta TempX
	lda screenRowLUTLO,y
	clc
	adc TempX 
	sta Pointer2
	sta Pointer3
	lda screenRowLUTHi,y
	adc #00
	sta Pointer2+1
	eor # (>kVectors.charBase) ^ $d8
	sta Pointer3+1
	rts
	
convertSpriteXSingleByte
	lda mplexBuffer.xmsb,x
	beq _MSB
	lda mplexBuffer.xpos,x
	sec
	sbc #kBounds.screenMinX
	rts
_MSB
	lda mplexBuffer.xpos,x
	adc #255-kBounds.screenMinX
	rts
	
; carry set = not safe, clear = safe
checkSolidTile
	ldx GameData.exitOpen
	bne _skipDoorCheck
	cmp # kTiles.exit
	beq _notSafe
_skipDoorCheck
	cmp # kTiles.pipe
	beq _notSafe
	cmp # kTiles.dissNoColide
	beq _exitSafe
	cmp # kTiles.diss
	;bcs _notSafe
	bcs _checkNotShadow
	cmp # kTiles.wall
	bcc _exitSafe
	cmp # kTiles.spike
	bcs _exitSafe
_notsafe
	sec
	rts
_exitSafe
	clc
_exit
	rts
_checkNotShadow
	cmp #kTiles.dissNoColide
	bcc _notsafe
	bcs _exitSafe
	
plotStatusArea
	lda #<kVectors.charBase + 32 
	sta $fb
	sta $fd
	lda #>kVectors.charBase + 32 
	sta $fc
	eor # (>kVectors.charBase) ^ $d8
	sta $fe
	lda #0
	sta ZPTemp
_loop
	ldx ZPTemp
	lda statusLines,x
	asl a
	asl a
	asl a
	tax
	ldy #0
_lineLoop	
	lda statusLine0,x
	sta ($fb),y
	lda statusColour0,x
	sta ($fd),y
	inx
	iny
	cpy #8
	bcc _lineLoop
	clc
	lda $fb
	adc #40
	sta $fb
	sta $fd
	lda $fc
	adc #00
	sta $fc
	eor # (>kVectors.charBase) ^ $d8
	sta $fe
	lda ZPTemp
	clc
	adc #1
	cmp #24
	bcs _done
	sta ZPTemp
	bne _loop
_done
	jsr pltScore
	jsr pltHighScore
	jsr pltLives
	jmp pltFlowers
	
pltScore
	ldx #5
_l	lda GameData.score,x
	ora #240
	sta kVectors.charBase + 33 + (40*9),x
	lda #1
	sta $d800 + 33 + (40*9),x
	dex
	bpl _l
	rts
	
pltHighScore
	ldx #5
_l	lda GameData.high,x
	ora #240
	sta kVectors.charBase + 33 + (40*13),x
	lda #1
	sta $d800 + 33 + (40*13),x
	dex
	bpl _l
	rts
	
pltLives
	lda GameData.lives
	ora #240
	sta kVectors.charBase + 37 + (40*17)
	lda #1
	sta $d800 + 37 + (40*17)
	rts
	
pltFlowers
	lda GameData.flowers
	ora #240
	sta kVectors.charBase + 37 + (40*21)
	lda #1
	sta $d800 + 37 + (40*21)
	rts
	
setPlayerToSpawnPoint
	lda LevelData.playerX
	asl a
	asl a
	asl a
	asl a
	cmp #$F0
	beq _msbMode
	clc
	adc #24
	sta mplexBuffer.xpos
	sta mplexBuffer.xmsb
_Y	lda LevelData.playerY
	asl a
	asl a
	asl a
	asl a
	clc
	adc #50
	sta mplexBuffer.ypos
	rts
_msbMode
	lda #8
	sta mplexBuffer.xpos
	lda #0
	sta mplexBuffer.xmsb
	beq _Y
	
resetPlayerData
	lda #0
	sta PlayerData.dead
	rts
	
updateTickdowns
	ldx #TICK_DOWN_END - TICK_DOWN_START-1
_l	lda TickDowns,x
	beq _next
	dec TickDowns,x
_next
	dex
	bpl _l
	rts

; NumEnts followed by XXXXYYYY TTTT--DD
; X x tile
; Y y tile
; T type
; D initial direction
unpackEntityBytes
	ldy #0
	lda (EntityDataPointer),y
	sta $fb ; number of entities
	beq _e
	iny			; next byte
	ldx #0
	sta EntNum
_l	lda (EntityDataPointer),y
	and #$0f
	asl a
	asl a
	asl a
	asl a
	clc
	adc #50 
	sta mplexBuffer.ypos+1,x
	sta ZPTemp
	lda (EntityDataPointer),y
	and #$f0
	cmp #$f0
	bne _notMsb
	lda #0
	sta mplexBuffer.xmsb+1,x
	lda #8
	bne _storeX
_notMsb
	clc
	adc #24
_storeX
	sta mplexBuffer.xpos+1,x
	iny			; next byte	
	lda (EntityDataPointer),y
	lsr a
	lsr a
	lsr a
	lsr a
	sta EntityData.type,x
	lda ZPTemp
	sta EntityData.originalY,x
	lda #0
	sta EntityData.entState,x
	lda (EntityDataPointer),y
	and #3
	sta EntityData.direction,x	
	lda #1	
	sta EntityData.active,x 
	iny 		; next byte
	inx
	dec $fb
	lda $fb
	bne _l
_e	rts
	
setEntitySprites
	ldx # kEntity.maxEntities
_l	lda EntityData.active,x
	bne _active
_c	dex
	bpl _l
	rts
_active
	stx CurrentEntity
	lda EntityData.type,x
	tay
	lda EntitySpriteColours,y
	sta mplexBuffer.sprc+1,x
	jsr SetEntSpriteForDirection
	jmp _c
	
updateEntities
	ldx # kEntity.maxEntities-1
innerEntitiesLoop
	lda EntityData.active,x
	bne EntitiesActive
updateEntitiesLoop
	dex
	bpl innerEntitiesLoop
	rts
EntitiesActive
	stx CurrentEntity
	lda EntityData.type,x
	cmp #kEntity.fish
	bne +
	jmp entFishFunc
+	cmp #kEntity.spider
	bne +
	jmp entSpiderFunc	
+	cmp #kEntity.circler	
	bne +	
	jmp circlerFunc	
+	cmp #kEntity.spring
	bne +
	jmp springEntFunc
+	jsr updateEntAnimAndSetSprite
	lda CollFrameForEnt,y	
	sta CollideSpriteBoxIndex	
	inx
	stx CollideSpriteToCheck
	lda #<handleEntCollisionResult
	sta Pointer1
	lda #>handleEntCollisionResult
	sta Pointer1+1
	ldx CurrentEntity
	lda EntityData.direction,x	
	tax
	lda #0
	sta CollisionResult
	lda ENTDirectionCheckFuncLUTHi,x
	pha
	lda ENTDirectionCheckFuncLUTLo,x
	pha
	rts
		
;right, up, left ,down			
ENTDirectionCheckFuncLUTLo .byte <entRight-1,<entUp-1,<entLeft-1,<entDown-1	
ENTDirectionCheckFuncLUTHi .byte >entRight  ,>entUp  ,>entLeft  ,>entDown	
	
entRight	
	lda #1	
	sta checkSpriteToCharData.xDeltaCheck
	lda #0
	sta checkSpriteToCharData.yDeltaCheck
entRightNoDelta
	jsr newCollision
	lda CollideCharTRC
	jsr checkSolidTile
	rol CollisionResult
	lda CollideCharBRC
	jsr checkSolidTile
	rol CollisionResult
	jmp (Pointer1)
	
entUp	
	lda #$00	
	sta checkSpriteToCharData.xDeltaCheck
	lda #$ff
	sta checkSpriteToCharData.yDeltaCheck	
entUpNoDelta
	jsr newCollision
	lda CollideCharTLC
	jsr checkSolidTile
	rol CollisionResult
	lda CollideCharTRC
	jsr checkSolidTile
	rol CollisionResult
	jmp (Pointer1)
	
entLeft	
	lda #$ff	
	sta checkSpriteToCharData.xDeltaCheck
	lda #0
	sta checkSpriteToCharData.yDeltaCheck
entLeftNoDelta
	jsr newCollision
	lda CollideCharTLC
	jsr checkSolidTile
	rol CollisionResult
	lda CollideCharBLC
	jsr checkSolidTile
	rol CollisionResult
	jmp (Pointer1)
	
entDown	
	lda #$00	
	sta checkSpriteToCharData.xDeltaCheck
	lda #$01
	sta checkSpriteToCharData.yDeltaCheck
entDownNoDelta
	jsr newCollision
	lda CollideCharBLC
	jsr checkSolidTile
	rol CollisionResult
	lda CollideCharBRC
	jsr checkSolidTile
	rol CollisionResult
	jmp (Pointer1)	

entFishFunc
	dec EntityData.movTimer,x
	lda EntityData.movTimer,x
	bmi _next
	and #1
	bne _exit
	lda EntityData.entState,x
	beq _exit	
	jmp _keepGoing
_exit	
	jmp NextEnt
_next
	lda #4
	sta EntityData.movTimer,x

;	inc EntityData.entState,x
;{{{
.comment	
	lda EntityData.direction,x
	cmp # kDirections.up
	bne _fishDown
	;fish up
	lda #kSprBase+80
	;sta mplex.sprp+1,x ; will need to change to animation type
	sta EntityData.animBase,x
	lda mplexBuffer.ypos+1,x
	cmp # kFishLimits.startTwo
	bcc _fupNormal
	lda #0
	sta EntityData.entState,x
	jmp _moveFish
_fupNormal	
	lda EntityData.entState,x
	cmp #kSinJumpFall
	bcc _moveFish
	lda # kDirections.down
	sta EntityData.direction,x
	jmp _moveFish
_fishDown
	lda #kSprBase+84
	;sta mplex.sprp+1,x
	sta EntityData.animBase,x
	lda EntityData.entState,x
	cmp #kSinJumpMax
	bcc _checkMaxY
	dec EntityData.entState,x
_checkMaxY	
	lda mplexBuffer.ypos+1,x
	cmp # kFishLimits.maxY
	bcc _movefish
	lda # kDirections.up
	sta EntityData.direction,x
.endc
;}}}
_moveFish	
	lda EntityData.entState,x
	clc
	adc #1
	cmp #kSinJumpMax
	bne _storeDirect
	lda #kSinJumpMax-1
_storeDirect
	sta EntityData.entState,x
_keepGoing
	tay
	lda mplexBuffer.ypos+1,x
	clc
	adc SinJumpTable,y
	cmp # kFishLimits.maxY
	bcc _store
	lda #0
	sta EntityData.entState,x
	lda #32
	sta EntityData.movTimer,x
	lda #kFishLimits.maxY
	bcc _store
_store
	sta mplexBuffer.ypos+1,x
	lda EntityData.entState,x
	lsr a
	lsr a ; div 4
	cmp #8
	bcc _safe
	lda #7
_safe
	clc
	adc #kSprites.fish
	sta mplexBuffer.sprp+1,x
	jmp nextEnt

entSpiderFunc	
	lda EntityData.entState,x	
	tay	
	lda SpiderEntFuncLUTHi,y	
	pha	
	lda SpiderEntFuncLUTLo,y	
	pha		
	rts
	
SpiderEntFuncLUTLo .byte <spiderLookPlayer-1,<spiderFall-1,<spiderRise-1	
SpiderEntFuncLUTHi .byte >spiderLookPlayer  ,>spiderFall  ,>spiderRise	
	
spiderLookPlayer
	ldx #0
	stx ZPTemp2
	jsr convertSpriteXSingleByte
	sta ZPTemp
	ldx CurrentEntity
	inx
	jsr convertSpriteXSingleByte
	sbc ZPTemp
	sta ZPTemp
	bcs _left
	cmp #kSpiderValues.rightStartWiggle
	bcc +
	lda #1
	sta ZPTemp2
	lda ZPTemp
	cmp #kSpiderValues.rightStartFall
	bcc +
	lda #1
	ldx CurrentEntity
	sta EntityData.entState,x
+	lda #kSprites.spiderRight
_storeSprite
	ldx CurrentEntity
	sta EntityData.animBase,x
	lda ZPTemp2
	beq _noAnim
	jsr updateEntAnimAndSetSprite
	jmp nextEnt
_noAnim	
	ldx CurrentEntity
	lda EntityData.animBase,x
	sta mplexBuffer.sprp+1,x
	jmp nextEnt
_left
	cmp #kSpiderValues.leftStartWiggle
	bcs +
	lda #1
	sta ZPTemp2
	lda ZPTemp
	cmp #kSpiderValues.leftStartFall
	bcs +
	lda #1
	ldx CurrentEntity
	sta EntityData.entState,x
+	lda #kSprites.spiderLeft
	jmp _storeSprite	
		
spiderFall	
	jsr updateEntAnimAndSetSprite
	ldy #kEntity.spider
	lda CollFrameForEnt,y	
	sta CollideSpriteBoxIndex	
	ldx CurrentEntity	
	inx
	stx CollideSpriteToCheck
	lda #$00	
	sta checkSpriteToCharData.xDeltaCheck
	lda #kSpiderValues.yFallDelta
	sta checkSpriteToCharData.yDeltaCheck
	jsr newCollision
	lda CollideCharBLC
	jsr checkSolidTile
	bcc _noColide
	lda #2
	ldx CurrentEntity
	sta EntityData.entState,x
	lda #kSpiderValues.pauseEndFallFrames
	sta EntityData.movTimer,x
	jmp nextEnt
_noColide
	ldx CurrentEntity
	lda mplexBuffer.ypos+1,x
	clc
	adc #kSpiderValues.yFallDelta
	sta mplexBuffer.ypos+1,x
	jmp nextEnt
		
spiderRise	
	dec EntityData.movTimer,x
	lda EntityData.movTimer,x
	bpl +
	lda #kSpiderValues.riseDelayTime
	sta EntityData.movTimer,x
	lda mplexBuffer.ypos+1,x
	sec
	sbc #1
	sta mplexBuffer.ypos+1,x
	cmp EntityData.originalY,x
	bne +
	lda #0
	sta EntityData.entState,x
+	jmp nextEnt
		
circlerFunc
	lda EntityData.movTimer,x
	sec 
	sbc #1
	bmi _cirActive
	sta EntityData.movTimer,x
	jmp nextEnt
_cirActive
	lda #4
	sta EntityData.movTimer,x
	lda EntityData.entState,x
	ldy CurrentEntity
	tax
	lda mplexBuffer.xpos+1,y
	clc
	adc CircleJumpTableStart,x
	sta mplexBuffer.xpos+1,y
	lda mplexBuffer.ypos+1,y
	clc
	adc CircleJumpTableStart + ( CircleJumpTableCount / 4) + 1,x
	sta mplexBuffer.ypos+1,y
	ldx CurrentEntity
	lda EntityData.entState,x
	clc
	adc #1
	cmp # CircleJumpTableCount
	bne _cirStore
	lda #0
_cirStore
	sta EntityData.entState,x
	jsr updateEntAnimAndSetSprite
	jmp nextEnt		
			
springEntFunc
	lda EntityData.movTimer,x
	sec 
	sbc #1
	bmi _move
	sta EntityData.movTimer,x
	jmp nextEnt
_move
	lda #3
	sta EntityData.movTimer,x		
	; update Y component	
	lda EntityData.entState,x	
	sta ZPTemp
	tay	
	lda SinJumpTable,y	
	sta checkSpriteToCharData.yDeltaCheck	
	lda #$00	
	sta checkSpriteToCharData.xDeltaCheck	
	sta CollisionResult	
	lda #2 ; this might change per frame	
	sta CollideSpriteBoxIndex	
	inx ; current entity
	stx CollideSpriteToCheck	
	lda #<springEntYCollideEnd
	sta Pointer1
	lda #>springEntYCollideEnd
	sta Pointer1+1
	lda ZPTemp
	cmp #kSinJumpFall
	bcs _falling
	; rising
	lda #kSinJumpFall ; start falling
	sta ZPTemp2
	jmp entUpNoDelta
_falling
	lda #0				; start jumping again
	sta ZPTemp2
	jmp entDownNoDelta
springEntYCollideEnd
	lda CollisionResult
	bne _hit
	; didn't hit so carry on
	ldx CurrentEntity
	lda mplexBuffer.ypos+1,x
	clc
	adc checkSpriteToCharData.yDeltaCheck
	sta mplexBuffer.ypos+1,x
	lda EntityData.entState,x
	clc
	adc #1
	cmp #kSinJumpMax
	bcc _store
	lda #0
_store
	sta EntityData.entState,x
	jmp springEntHandleX
_hit
	lda ZPTemp2
	ldx CurrentEntity
	sta EntityData.entState,x
springEntHandleX
	lda #0
	sta checkSpriteToCharData.yDeltaCheck
	sta CollisionResult
	lda #<springEntXCollideEnd
	sta Pointer1
	lda #>springEntXCollideEnd
	sta Pointer1+1
	lda EntityData.direction,x
	sta ZPTemp
	clc
	adc #4
	tay
	lda SpringDirectionToDeltaLUT,y
	sta checkSpriteToCharData.xDeltaCheck
	bmi _left
	jmp entRightNoDelta
_left 
	jmp entLeftNoDelta	
springEntXCollideEnd		
	ldx CurrentEntity		
	lda ZPTemp	
	bmi springEntXLeft
	lda CollisionResult		
	beq _noCollideRight		
	lda #256-1		
	sta EntityData.direction,x 		 
	jmp nextEnt
_noCollideRight
	lda mplexBuffer.xpos+1,x
	clc
	adc checkSpriteToCharData.xDeltaCheck
	sta mplexBuffer.xpos+1,x
	lda EntityData.direction,x 	
	clc
	adc #1
	cmp #4
	bne _noClip
	lda #3
_noClip	
	sta EntityData.direction,x
	jmp nextEnt		
springEntXLeft	
	lda CollisionResult		
	beq _noCollideLeft		
	lda #0		
	sta EntityData.direction,x 		 
	jmp springEndAnimate
_noCollideLeft
	lda mplexBuffer.xpos+1,x
	clc
	adc checkSpriteToCharData.xDeltaCheck
	sta mplexBuffer.xpos+1,x
	lda EntityData.direction,x 	
	sec
	sbc #1
	cmp #256-5
	bne _noClip
	lda #256-4
_noClip	
	sta EntityData.direction,x
springEndAnimate
	ldx CurrentEntity
	lda EntityData.entState,x
	tay
	cmp #kSinJumpFall
	bcs _fall
	lda	SpringFrameFrameTable,y
	sta mplexBuffer.sprp+1,x
	jmp nextEnt
_fall
	lda #kSprites.springFall
	sta EntityData.animBase,x
	jsr updateEntAnimAndSetSprite
	jmp nextEnt

handleEntCollisionResult	
	ldx CurrentEntity	
	lda EntityData.type,x
	tay
	lda CollisionResult	
	eor CollisionResultEORForEnt,y	
	beq _addDeltas
	jsr setNextEntDir	
	jmp nextEnt	
_addDeltas	
	ldx CurrentEntity	
	lda mplexBuffer.ypos+1,x	
	clc	
	adc checkSpriteToCharData.yDeltaCheck	
	sta mplexBuffer.ypos+1,x	
	inx
	jsr addXWithMSBAndClip
	lda DidClipX
	beq _skipFlipDueToX
	lda mplexBuffer.xpos,x	; x was increased above
	sec	
	sbc checkSpriteToCharData.yDeltaCheck ; undo the move	
	sta mplexBuffer.xpos,x
	jsr setNextEntDir
_skipFlipDueToX	
nextEnt
	ldx CurrentEntity	
	jmp updateEntitiesLoop
	
setNextEntDir
	jsr getEntTableIndex
	lda NextDirectionLUT,y
	sta EntityData.direction,x
	ora ZPTemp ; add the ent type offset to it
	tay
	jmp setEntFrameForDir
	
setEntSpriteForDirection
	jsr getEntTableIndex
	jmp setEntFrameForDir
	
setEntFrameForDir
	lda BaseAnimeFrameForDir,y
	sta EntityData.animBase,x
	clc
	adc EntityData.animFrame,x
	sta mplexBuffer.sprp+1,x
	rts
getEntTableIndex
	ldx CurrentEntity
	lda EntityData.type,x
	asl a
	asl a
	sta ZPTemp
	ora EntityData.direction,x
	tay
	rts
	
updateEntAnimAndSetSprite	
	lda EntityData.type,x	
	tay	
	inc EntityData.animTimer,x
	lda EntityData.animTimer,x
	cmp AnimFrameTimerForEnt,y
	bne _notAnimUpdate
	lda #0
	sta EntityData.animTimer,x	
	inc EntityData.animFrame,x
	lda EntityData.animFrame,x
	cmp FrameCountForEnt,y
	bne _dontResetFrames
	lda #0
	sta EntityData.animFrame,x
_dontResetFrames	
	clc	
	adc EntityData.animBase,x
	sta mplexBuffer.sprp+1,x
_notAnimUpdate 		
	rts

FAKE_SPLIT	
; {{{
.comment
	lda EntityData.direction,x	
	tay 
	lda DirectionYLUT,y
	sta $fd
	lda DirectionXLUT,y
	sta $fe
	lda #0
	sta CollisionResult
	jsr CheckEntAgainstTile
	ldx CurrentEntity
	lda EntityData.type,x
	cmp # kEntity.spider ; this probably should be done BEFOR above checks
	beq _spider
	cmp # kEntity.fish
	beq _fish
	cmp # kEntity.spring
	beq _spring
	cmp # kEntity.circler
	beq _circler
	cmp # kEntity.bat
	bne _notBat
	;bat
	dec CollisionResult
	lda DirectionYLUT+3 ; down Y
	sta $fd
	jsr CheckEntAgainstTile
	jmp _notBat
_fish
	jmp _fishFunc
_spring
	jmp _springFunc
_circler 
	jmp _circlerFunc
_spider
	lda EntityData.entState,x
	beq _spiderCheckBelow
	cmp #1
	beq _notBat
_spiderDelay	
	cmp #4	
	beq _spiderReset	
	inc EntityData.entState,x	
	jmp _next
_spiderReset	
	lda #1	
	sta EntityData.entState,x	
	jmp _next	
_spiderCheckBelow	
	lda mplexBuffer.xpos+1,x
	sec
	sbc mplexBuffer.xpos
	clc
	adc #10
	cmp #20
	bcs _spiderNoMove
	inc EntityData.entState,x
_spiderNoMove	
	jmp _next	

_notBat 
	lda CurrentEntity
	clc
	adc #1 ; convert ent num to sprite num
	jsr didIHitSomething ; if hit something then returns 1
	ora CollisionResult 
	sta CollisionResult
	ldx CurrentEntity	
;	ldy EntIndexVIC
	lda EntityData.direction,x		
	beq _right	
	cmp #1	
	beq _up 
	cmp #2	
	beq _left	
_down	
	lda CollisionResult
	bne _dnotsafe
	ldx CurrentEntity
	inc mPlexBuffer.ypos+1,x 
	jmp _next		
_dnotsafe
	dec mPlexBuffer.ypos+1,x 
	jsr SetNextEntDir	
	jmp _next	
		
_right
	lda CollisionResult
	bne _rnotsafe
;	ldx EntIndexVIC ; what is in y
	lda mPlexBuffer.xpos+1,x
	clc
	adc #1
	bcc _rightStore ; didn't overflow
	lda #0
	sta mplexBuffer.xmsb+1,x
_rightStore
	tay
	lda mplexBuffer.xmsb+1,x
	bne _rightStore2
	cpy #8
	bcs _rnotsafe	
_rightStore2
	tya
	sta mPlexBuffer.xpos+1,x 
	jmp _next		
_rnotsafe
	dec mPlexBuffer.xpos+1,x 
	jsr SetNextEntDir
	jmp _next
	
_up
	lda CollisionResult
	bne _unotsafe
	ldx CurrentEntity 
	dec mPlexBuffer.ypos+1,x 
	;ldy CurrentEntity
	lda EntityData.type,x
	cmp # kEntity.spider
	beq _checkSpider
	jmp _next		
_unotsafe
	inc mPlexBuffer.ypos+1,x 
	jsr SetNextEntDir
	jmp _next

_checkSpider
	lda mplexBuffer.ypos+1,x
	sec
	sbc #4
	cmp EntityData.originalY,x
	bcs _nextSpider
	lda #3
	sta EntityData.direction,x
	lda #0
	sta EntityData.entState,x
	beq _next
_nextSpider
	ldx CurrentEntity
	inc EntityData.entState,x
	bne _next
	
_left
	lda CollisionResult
	bne _lnotsafe
	;ldx EntIndexVIC ; what is in y
	lda mPlexBuffer.xpos+1,x
	sec
	sbc #1
	bcs _storeLeft ; didn't underflow
	lda #1
	sta mPlexBuffer.xmsb+1,x
	lda #$FF
_storeLeft 
	tay
	lda mPlexBuffer.xmsb+1,x
	beq _storeLeft2
	cpy #25
	bcc _lnotsafe
_storeLeft2
	tya
	sta mPlexBuffer.xpos+1,x	 
	jmp _next		
_lnotsafe
	inc mPlexBuffer.xpos+1,x
	jsr SetNextEntDir
	jmp _next
	
_next	
	ldx CurrentEntity	
	jmp _c

_fishFunc
	dec EntityData.movTimer,x
	lda EntityData.movTimer,x
	bpl _next
	lda #4
	sta EntityData.movTimer,x
	
	inc EntityData.entState,x
	lda EntityData.direction,x
	cmp # kDirections.up
	bne _fishDown
	;fish up
	lda #kSprBase+80
	;sta mplex.sprp+1,x ; will need to change to animation type
	sta EntityData.animBase,x
	lda mplexBuffer.ypos+1,x
	cmp # kFishLimits.startTwo
	bcc _fupNormal
	lda #0
	sta EntityData.entState,x
	jmp _moveFish
_fupNormal	
	lda EntityData.entState,x
	cmp #kSinJumpFall
	bcc _moveFish
	lda # kDirections.down
	sta EntityData.direction,x
	jmp _moveFish
_fishDown
	lda #kSprBase+84
	;sta mplex.sprp+1,x
	sta EntityData.animBase,x
	lda EntityData.entState,x
	cmp #kSinJumpMax
	bcc _checkMaxY
	dec EntityData.entState,x
_checkMaxY	
	lda mplexBuffer.ypos+1,x
	cmp # kFishLimits.maxY
	bcc _movefish
	lda # kDirections.up
	sta EntityData.direction,x
_moveFish	
	lda EntityData.entState,x
	tay
	lda mplexBuffer.ypos+1,x
	clc
	adc SinJumpTable,y
	sta mplexBuffer.ypos+1,x
	jmp _next
	
; x = ent number
_springFunc
	lda EntityData.movTimer,x
	sec 
	sbc #1
	beq _move
	sta EntityData.movTimer,x
	jmp _next
_move
	lda #3
	sta EntityData.movTimer,x
; check down
	lda # 50-24 ; DirectionYLUT + kDirections.down
	sta $fd
	lda # 12 ;DirectionXLUT + kDirections.down
	sta $fe
	lda #0
	sta CollisionResult
	jsr CheckEntAgainstTile
	rts
	lda CollisionResult
	bne _resetJump
	lda DirectionYLUT + kDirections.up
	sta $fd
	lda DirectionXLUT + kDirections.up
	sta $fe
	jsr CheckEntAgainstTile
	lda CollisionResult
	bne _startFall
	ldx CurrentEntity
	lda EntityData.entState,x
	cmp # kSinJumpMax
	bcc _notOverFlow
	bcs _springStore
_resetJump
	ldx CurrentEntity
;	ldy EntIndexVIC
	lda EntityData.direction,x
	cmp #128
	bcs _springSpeedSkip
	clc
	adc #64
	sta EntityData.direction,x
_springSpeedSkip
	lda # 0
	beq _springStore
_startFall
	ldx CurrentEntity
	lda EntityData.entState,x
	cmp # kSinJumpFall
	bcs _notOverFlow
	lda # kSinJumpFall
	bne _springStore
_notOverFlow
	clc
	adc #1
_springStore
	sta EntityData.entState,x	
	ldy CurrentEntity
	tax
	lda mplexBuffer.ypos+1,y
	clc
	adc SinJumpTable,x
	sta mplexBuffer.ypos+1,y
	; check left/right
	ldx CurrentEntity
	lda EntityData.direction,x
	and #128+64 
	cmp #128
	beq _springMaxSpeed
	lda #1
	sta ZPTemp
	bne _springCheck
_springMaxSpeed
	lda #2
	sta ZPTemp
_springCheck	
	lda EntityData.direction,x
	and #15 ; mask of the upper bits 
	cmp # kDirections.left
	bne _springRight
	; check left
	lda DirectionYLUT + kDirections.left
	sta $fd
	lda DirectionXLUT + kDirections.left
	sta $fe
	lda #0
	sta CollisionResult
	jsr CheckEntAgainstTile
	lda CollisionResult 
	beq _moveLeft
	lda # kDirections.right + 64
	ldx CurrentEntity
	sta EntityData.direction,x
	jmp _moveRight
_springRight
	lda DirectionYLUT + kDirections.right
	sta $fd
	lda DirectionXLUT + kDirections.right
	sta $fe
	lda #0
	sta CollisionResult
	jsr CheckEntAgainstTile
	lda CollisionResult 
	beq _moveRight
	lda # kDirections.left + 64
	ldx CurrentEntity
	sta EntityData.direction,x
	jmp _moveLeft
_moveRight	
	ldx CurrentEntity
	lda mplexBuffer.xpos+1,x
	clc
	adc ZPTemp
	sta mplexBuffer.xpos+1,x
	jmp _next
_moveLeft
	ldx CurrentEntity
	lda mplexBuffer.xpos+1,x
	sec
	sbc ZPTemp
	sta mplexBuffer.xpos+1,x
	jmp _next
	
_circlerFunc
	lda EntityData.movTimer,x
	sec 
	sbc #1
	beq _cirActive
	sta EntityData.movTimer,x
	jmp _next
_cirActive
	lda #4
	sta EntityData.movTimer,x
	lda EntityData.entState,x
	ldy CurrentEntity
	tax
	lda mplexBuffer.xpos+1,y
	clc
	adc CircleJumpTableStart,x
	sta mplexBuffer.xpos+1,y
	lda mplexBuffer.ypos+1,y
	clc
	adc CircleJumpTableStart + ( CircleJumpTableCount / 4) + 1,x
	sta mplexBuffer.ypos+1,y
	ldx CurrentEntity
	lda EntityData.entState,x
	clc
	adc #1
	cmp # CircleJumpTableCount
	bne _cirStore
	lda #0
_cirStore
	sta EntityData.entState,x
	jmp _next

CheckEntAgainstTile
	lda mPlexBuffer.ypos+1,x
	tay
	lda mPlexBuffer.xpos+1,x
	tax 
checkXYAgainstTile
	tya
	sec 
	sbc $fd 
	and #$f0 ; clip to multiple of 16	
	sta $fb 
	sta $f9
	txa
	sec
	sbc $fe
	lsr a
	lsr a
	lsr a
	lsr a
	sta $fa
	clc
	adc $fb
	clc
	adc # <tileMapTemp
	sta $fb
	lda # >tileMapTemp
	adc #00
	sta $fc
	ldy #0
	lda ($fb),y
	jsr checkSolidTile
	bcs _not
_clear	
	rts
_not 
	inc CollisionResult ; fix me
	rts
	
.endc	
; }}}
	
SortIndexA .byte ?
SortIndexB .byte ?
CollisionTableIndex .byte ?
CollisionTableLo .fill 16
CollisionTableHi .fill 16
SortAY .byte ?
SortAMSB .byte ?
ActualIndexA .byte ?
AcutalIndexB .byte ?
buildSpriteCollisionTable
	lda #0	
	sta SortIndexA	
	sta CollisionTableIndex
	lda #1	
	sta SortIndexB	
_loopA	
	ldx SortIndexA	
	lda mplexZP.sort,x	
	sta ActualIndexA	
	tay	
	lda mplexBuffer.ypos,y	
	cmp #$ff	
	beq sortAllDone	
	sta SortAY	
_loopB
	ldx SortIndexB
	lda mplexZP.sort,x
	sta AcutalIndexB
	tay
	lda mplexBuffer.ypos,y
	cmp #$ff
	beq _doneB
	sec
	sbc SortAY
	clc
	cmp #14
	bcs _yNotInRange
	jsr yInRange
_yNotInRange
	inc SortIndexB
	bne _loopB
_doneB
	lda SortIndexA
	clc
	adc #1
	sta SortIndexA
	adc #1
	sta SortIndexB
	bne _loopA
sortAllDone
	rts
yInRange
	ldy ActualIndexA
	lda mplexBuffer.xpos,y
	sta SortAY
	ldy AcutalIndexB
	lda mplexBuffer.xpos,y
	sta SortAMSB
	lda SortAY
	cmp SortAMSB
	bcc _x2smaller
	; x1 is smaller
	lda SortAY
	sec
	sbc SortAMSB
	cmp #14
	bcs sortAllDone  ; just returns back to loops above
	bcc xInRange
_x2smaller	
	lda SortAMSB
	sec
	sbc SortAY
	cmp #14
	bcs sortAllDone  ; just returns back to loops above	
xInRange
	ldy CollisionTableIndex
	lda ActualIndexA
	sta CollisionTableLo,y
	lda AcutalIndexB
	sta CollisionTableHi,y
	iny
	sty CollisionTableIndex
	rts
	
didIHitSomething
	sta ZPTemp
	ldx CollisionTableIndex
	beq _no
	dex ; sub 1 to get first index
_l	lda CollisionTableLo,x
	cmp ZPTemp
	beq _did
	lda CollisionTableHi,x
	cmp ZPTemp
	beq _did
	dex
	bpl _l
_no
	lda #0
	rts
_did
	lda #1
	rts		
	
; multiplexor
setirq
	sei			 ;set interrupt disable
	lda #$1b
	sta $d011		 ;raster irq to 1st half of screen.
	lda # kRaster.bottomRaster
	sta $d012		 ;irq to happen at line #$fb
	lda #<irq0
	sta $fffe		 ;hardware irq vector low byte
	lda #>irq0
	sta $ffff		 ;hardware irq vector high byte
	lda #$1f
	sta $dc0d		 ;turn off all types of cia irq/nmi.
	sta $dd0d
	lda #$01
	sta $d01a		 ;turn on raster irq.
	lda #$35
	sta $01		 ;no basic or kernal
	lda $dc0d		 ;acknowledge any irq that has occured during setup.
	lda $dd0d
	inc $d019
	lda # <start
	sta $fffc
	lda # >start
	sta $fffd
	lda # <justRTI
	sta $fffa
	lda # >justRTI
	sta $fffb
	; build unrolled loop
	ldx #mplex.kMaxSpr -1
	stx ZPTemp
	lda #< mplexBuffer.unrolledCopyLoopDest
	sta Pointer1
	lda #> mplexBuffer.unrolledCopyLoopDest
	sta Pointer1+1
	ldy #27-1
-	lda unrolledCopyBase,y	
	sta tileMapTemp,y
	dey
	bpl -	
_copyLoop
	ldy #27-1
-	lda tileMapTemp,y	
	sta (Pointer1),y
	dey
	bpl -
	lda Pointer1
	clc
	adc #27
	sta Pointer1
	lda Pointer1+1
	adc #0
	sta Pointer1+1
	inc tileMapTemp+1
	inc tileMapTemp+6
	inc tileMapTemp+11
	inc tileMapTemp+16
	inc tileMapTemp+21
	inc tileMapTemp+26
	dec ZPTemp
	ldx ZPTemp
	bpl _copyLoop
	lda #$60
	sta mplexBuffer.unrolledCopyLoopDestEnd ; save the rts 
	
	; copy irq 1 to temp buffer
	lda #< irqBuffers
	sta Pointer1
	lda #> irqBuffers
	sta Pointer1+1
	ldy # irqLength -1
-	lda irqBase,y	
	sta tileMapTemp,y
	dey
	bpl -	
	ldx #7 ; we have 8 rasters to do
irqloop
	ldy #irqLength -1
-	lda tileMapTemp,y	
	sta (Pointer1),y
	dey
	bpl -
	lda Pointer1
	clc
	adc #irqLength
	sta Pointer1
	lda Pointer1+1
	adc #0
	sta Pointer1+1
	inc tileMapTemp+irqYVICOffset
	inc tileMapTemp+irqYVICOffset ; add 2 
	inc tileMapTemp+irqXVICOffset
	inc tileMapTemp+irqXVICOffset ; add 2 
	asl tileMapTemp+irqOrBitmaskOffset
	sec
	rol tileMapTemp+irqAndBitmaskOffset
	inc tileMapTemp+irqSpritePtrOffset
	inc tileMapTemp+irqSpriteColourOffset
	inc tileMapTemp+irqNextSpriteYOffset
	inc tileMapTemp+irqNextSpriteYOffset
	clc
	lda tileMapTemp+irqNextIrqLoOffset
	adc #irqLength
	sta tileMapTemp+irqNextIrqLoOffset
	lda tileMapTemp+irqNextIrqHiOffset
	adc #0
	sta tileMapTemp+irqNextIrqHiOffset
	dex
	bpl irqLoop
	; do the irq 8 patch
	ldx # irq8PatchLength
-	lda irq8Pathc,x
	sta irqBuffers + ( 7 *irqLength ) + (ibok1 - irqBase) - 9,x
	dex
	bpl - 
	
	cli			 ;clear interrupt disable
	rts			 ;return from subroutine
unrolledCopyBase
	ldy mplexZP.sort		;2
	lda mplexBuffer.ypos,y	;3 5
	sta mplexZP.ybuf		;2 7
	lda mplexBuffer.xpos,y	;3 10
	sta mplexZP.xbuf		;2 12
	lda mplexBuffer.xmsb,y	;3 15
	sta mplexZP.mbuf		;2 17
	lda mplexBuffer.sprc,y	;3 20
	sta mplexZP.cbuf		;2 22
	lda mplexBuffer.sprp,y	;3 25
	sta mplexZP.pbuf		;2 27
irqBase	
		pha			;save registers
		txa
		pha
		tya
		pha
		inc $d019		;acknowledge irq
+		ldx mplexZP.sptr		;get current sprite index
ibhlop1	lda mplexZP.ybuf,x	;get sprite y position
irqYVICOffset = *+1-irqBase
		sta $d001		;store sprite y postion.
		lda mplexZP.xbuf,x	;get sprite x position.
irqXVICOffset = *+1-irqBase
		sta $d000		;sta sprite x position.
		lda mplexZP.mbuf,x	;get sprite x position msb
		bne ibno1		;set msb register
		lda $d010
		ora #%00000001
irqOrBitmaskOffset = *-1-irqBase
		bne ibyes1
ibno1	lda $d010
		and #%11111110
irqAndBitmaskOffset = *-1-irqBase
ibyes1	sta $d010
		lda mplexZP.pbuf,x	;get sprite image pointer
irqSpritePtrOffset = *+1-irqBase
		sta kVectors.spr0ID		;store it screen.
		lda mplexZP.cbuf,x	;get sprite color
irqSpriteColourOffset = *+1-irqBase
		sta $d027		;store sprite color
		inx			;next sprite index
		cpx mplexZP.mnt		;lets go to next plot, if < then 8 yet.
		bcc irqBase + irqLength + ( ibhlop1-irqBase)
		cpx mplexZP.cnt		;no more sprites?
		bne ibok1
		jmp done		;no more sprites.

ibok1	stx mplexZP.sptr		;save sprite index
irqNextSpriteYOffset = *+1-irqBase
		lda $d003		;get last position of next sprite
		clc
		adc #$15		;add 21 lines
		cmp $d012		;we there yet?
		bcc irqBase + irqLength + ( ibhlop1-irqBase)		;yeah, so plot next sprite
		adc #$02		;no, so calculate next irq position (+3)
		sta $d012		;set it
irqNextIrqLoOffset = * +1-irqBase
		lda #<irqBuffers+irqLength		;irq for next sprite.
		sta $fffe
irqNextIrqHiOffset = * +1-irqBase
		lda #>irqBuffers+irqLength
		sta $ffff
		jmp eirq
irqLength = * -irqBase	
irq8Pathc
		bcc ibhlop9
		cpx mplexZP.cnt
		bne ibok8
		jmp done

ibok8		stx mplexZP.sptr
		lda $d001
		clc
		adc #$15
		cmp $d012
		bcc ibhlop9
		adc #$02
		sta $d012
		lda #<irqBuffers
		sta $fffe
		lda #>irqBuffers
		sta $ffff
		jmp eirq
ibhlop9 jmp irqBuffers + ( ibhlop1-irqBase)
irq8PatchLength = * - irq8Pathc
	
irq0
	pha		;use stack instead of zp to prevent bugs.
	txa
	pha
	tya
	pha  ;13
	inc $d019		 ;acknowledge irq 19
	lda #%01100011  ; turn the screen off 21
	ldx #0 ; 23
	ldy #3 ; 25
-	nop		; 27 41 52	
	dey		; 33 47 58
	bne -	; 39 50 60
	sta $d011
	stx $d027		; and the sprites
	stx $d028		; and the sprites
	stx $d029		; and the sprites
	stx $d02A		; and the sprites
	stx $d02B		; and the sprites
	stx $d02C		; and the sprites
	stx $d02D		; and the sprites
	stx $d02E		; and the sprites
	stx $d025
	stx $d026
;	inc $d021
slop  ldy mplexZP.sort+1,x	;main index sort algo
slep  lda mplexBuffer.ypos,y
	  ldy mplexZP.sort,x		;this sorter uses the previous frame as a prediction buffer.
	  cmp mplexBuffer.ypos,y		;as y position does not change much from frame to frame.
	  bcc swap				;otherwise, it is a simple swap sort.
	  inx					;our linked list (sort) is sorted in decending order, according
	  cpx #mplex.kMaxSpr-1	;to sprite y positions.
	  bne slop
	  beq end
swap 
	  lda mplexZP.sort+1,x
	  sta mplexZP.sort,x
	  sty mplexZP.sort+1,x
	  tay
	  dex
	  bpl slep
	  inx
	  beq slop
end
.comment
START_UNROLED = *
.for spr = 0, spr < mPlex.kMaxSpr, spr = spr + 1
	  ldy mplex.sort+spr		;re arrange frame buffers, into the raster buffers.
	  lda mplex.ypos,y			;this is unrolled for speed.
	  sta mplexZP.ybuf+spr		;this allows us to use only 1 index pointer for our sprite plotter.
	  lda mplex.xpos,y			;it is double buffered, to allow runtime code to calculate the sprite
	  sta mplexZP.xbuf+spr		;positions.
	  lda mplex.xmsb,y
	  sta mplexZP.mbuf+spr
	  lda mplex.sprc,y
	  sta mplexZP.cbuf+spr
	  lda mplex.sprp,y
	  sta mplexZP.pbuf+spr
.next
.warn "unroled length = ", *-START_UNROLED
.endc
		jsr mplexBuffer.unrolledCopyLoopDest
		ldx #$00	 ;find # of used sprites (you can remove sprites by
		stx mplexZP.sptr	 ;placing #$ff into the ypos buffer for the corresponding
maxc	lda mplexZP.ybuf,x   ;sprite. It will not be displayed by the raster routine.
		cmp #$ff
		beq maxs
		inx
		cpx mplex.kMaxSpr
		bne maxc
maxs	stx mplexZP.cnt		 ;max sprites this frame count.
		cpx #$07	 ;check if were doing more than 8
		bcc maxm	 ;if not, we want the plotter to stop after 1 irq.
		ldx #$07	 
maxm	stx mplexZP.mnt

		lda #$ff	;reset sprites to off screen.
		sta $d001	;prevents bugs.
		sta $d003
		sta $d005
		sta $d007
		sta $d009
		sta $d00b
		sta $d00d
		sta $d00f
		sta $d015

		inc mplexZP.lsbtod	 ;buffers are swapped, so we can do the next frame now.

		lda mplexZP.cnt ; if there are no sprite then keep this the irq
		beq +
		lda #<irqBuffers	 ;irq chain for raster code. prolly want a routine before
		sta $fffe	 ;this one, to turn the sprites back on ;)
		lda #>irqBuffers	 ;i.e. lda #$ff sta $d015
		sta $ffff
		lda #$28
		sta $d012
+		lda #%00011011  ; turn the screen on
		sta $d011
		lda #11
		sta $d025
		lda #1
		sta $d026
;	dec $d021
		jmp eirq
;{{{		
.comment		
irq1
		pha			;save registers
		txa
		pha
		tya
		pha
		inc $d019		;acknowledge irq
+		ldx mplexZP.sptr		;get current sprite index
hlop1	lda mplexZP.ybuf,x	;get sprite y position
		sta $d001		;store sprite y postion.
		lda mplexZP.xbuf,x	;get sprite x position.
		sta $d000		;sta sprite x position.
		lda mplexZP.mbuf,x	;get sprite x position msb
		bne no1		;set msb register
		lda $d010
		ora #%00000001
		bne yes1
no1		lda $d010
		and #%11111110
yes1	sta $d010
		lda mplexZP.pbuf,x	;get sprite image pointer
		sta kVectors.spr0ID		;store it screen.
		lda mplexZP.cbuf,x	;get sprite color
		sta $d027		;store sprite color
		inx			;next sprite index
		cpx mplexZP.mnt		;lets go to next plot, if < then 8 yet.
		bcc hlop2
		cpx mplexZP.cnt		;no more sprites?
		bne ok1
		jmp done		;no more sprites.
ok1		stx mplexZP.sptr		;save sprite index
		lda $d003		;get last position of next sprite
		clc
		adc #$15		;add 21 lines
		cmp $d012		;we there yet?
		bcc hlop2		;yeah, so plot next sprite
		adc #$02		;no, so calculate next irq position (+3)
		sta $d012		;set it
		lda #<irq2		;irq for next sprite.
		sta $fffe
		lda #>irq2
		sta $ffff
		jmp eirq

irq2
		pha			;and so on
		txa
		pha
		tya
		pha
		inc $d019
		ldx mplexZP.sptr
hlop2	lda mplexZP.ybuf,x
		sta $d003
		lda mplexZP.xbuf,x
		sta $d002
		lda mplexZP.mbuf,x
		bne no2
		lda $d010
		ora #%00000010
		bne yes2
no2		lda $d010
		and #%11111101
yes2	sta $d010
		lda mplexZP.pbuf,x
		sta kVectors.spr1ID		;store it screen.
		lda mplexZP.cbuf,x
		sta $d028
		inx
		cpx mplexZP.mnt
		bcc hlop3
		cpx mplexZP.cnt
		bne ok2
		jmp done

ok2		stx mplexZP.sptr
		lda $d005
		clc
		adc #$15
		cmp $d012
		bcc hlop3
		adc #$02
		sta $d012
		lda #<irq3
		sta $fffe
		lda #>irq3
		sta $ffff
		jmp eirq

irq3
		pha
		txa
		pha
		tya
		pha
		inc $d019
		ldx mplexZP.sptr
hlop3	lda mplexZP.ybuf,x
		sta $d005
		lda mplexZP.xbuf,x
		sta $d004
		lda mplexZP.mbuf,x
		bne no3
		lda $d010
		ora #%00000100
		bne yes3
no3		lda $d010
		and #%11111011
yes3	sta $d010
		lda mplexZP.pbuf,x
		sta kVectors.spr2ID		;store it screen.
		lda mplexZP.cbuf,x
		sta $d029
		inx
		cpx mplexZP.mnt
		bcc hlop4
		cpx mplexZP.cnt
		bne ok3
		jmp done

ok3		stx mplexZP.sptr
		lda $d007
		clc
		adc #$15
		cmp $d012
		bcc hlop4
		adc #$02
		sta $d012
		lda #<irq4
		sta $fffe
		lda #>irq4
		sta $ffff
		jmp eirq

irq4  
		pha
		txa
		pha
		tya
		pha
		inc $d019
		ldx mplexZP.sptr
hlop4	lda mplexZP.ybuf,x
		sta $d007
		lda mplexZP.xbuf,x
		sta $d006
		lda mplexZP.mbuf,x
		bne no4
		lda $d010
		ora #%00001000
		bne yes4
no4		lda $d010
		and #%11110111
yes4	sta $d010
		lda mplexZP.pbuf,x
		sta kVectors.spr3ID		;store it screen.
		lda mplexZP.cbuf,x
		sta $d02a
		inx
		cpx mplexZP.mnt
		bcc hlop5
		cpx mplexZP.cnt
		bne ok4
		jmp done

ok4		stx mplexZP.sptr
		lda $d009
		clc
		adc #$15
		cmp $d012
		bcc hlop5
		adc #$02
		sta $d012
		lda #<irq5
		sta $fffe
		lda #>irq5
		sta $ffff
		jmp eirq

irq5
		pha
		txa
		pha
		tya
		pha
		inc $d019
		ldx mplexZP.sptr
hlop5	lda mplexZP.ybuf,x
		sta $d009
		lda mplexZP.xbuf,x
		sta $d008
		lda mplexZP.mbuf,x
		bne no5
		lda $d010
		ora #%00010000
		bne yes5
no5		lda $d010
		and #%11101111
yes5	sta $d010
		lda mplexZP.pbuf,x
		sta kVectors.spr4ID		;store it screen.
		lda mplexZP.cbuf,x
		sta $d02b
		inx
		cpx mplexZP.mnt
		bcc hlop6
		cpx mplexZP.cnt
		bne ok5
		jmp done

ok5		stx mplexZP.sptr
		lda $d00b
		clc
		adc #$15
		cmp $d012
		bcc hlop6
		adc #$02
		sta $d012
		lda #<irq6
		sta $fffe
		lda #>irq6
		sta $ffff
		jmp eirq

irq6	pha
		txa
		pha
		tya
		pha
		inc $d019
		ldx mplexZP.sptr
hlop6	lda mplexZP.ybuf,x
		sta $d00b
		lda mplexZP.xbuf,x
		sta $d00a
		lda mplexZP.mbuf,x
		bne no6
		lda $d010
		ora #%00100000
		bne yes6
no6		lda $d010
		and #%11011111
yes6	sta $d010
		lda mplexZP.pbuf,x
		sta kVectors.spr5ID		;store it screen.
		lda mplexZP.cbuf,x
		sta $d02c
		inx
		cpx mplexZP.mnt
		bcc hlop7
		cpx mplexZP.cnt
		bne ok6
		jmp done

ok6		stx mplexZP.sptr
		lda $d00d
		clc
		adc #$15
		cmp $d012
		bcc hlop7
		adc #$02
		sta $d012
		lda #<irq7
		sta $fffe
		lda #>irq7
		sta $ffff
		jmp eirq

irq7	pha
		txa
		pha
		tya
		pha
		inc $d019
		ldx mplexZP.sptr
hlop7	lda mplexZP.ybuf,x
		sta $d00d
		lda mplexZP.xbuf,x
		sta $d00c
		lda mplexZP.mbuf,x
		bne no7
		lda $d010
		ora #%01000000
		bne yes7
no7		lda $d010
		and #%10111111
yes7	sta $d010
		lda mplexZP.pbuf,x
		sta kVectors.spr6ID		;store it screen.
		lda mplexZP.cbuf,x
		sta $d02d
		inx
		cpx mplexZP.mnt
		bcc hlop8
		cpx mplexZP.cnt
		bne ok7
		jmp done

ok7		stx mplexZP.sptr
		lda $d00f
		clc
		adc #$15
		cmp $d012
		bcc hlop8
		adc #$02
		sta $d012
		lda #<irq8
		sta $fffe
		lda #>irq8
		sta $ffff
		jmp eirq

irq8	pha
		txa
		pha
		tya
		pha
		inc $d019
		ldx mplexZP.sptr
hlop8 	lda mplexZP.ybuf,x
		sta $d00f
		lda mplexZP.xbuf,x
		sta $d00e
		lda mplexZP.mbuf,x
		bne no8
		lda $d010
		ora #%10000000
		bne yes8
no8	  	lda $d010
		and #%01111111
yes8	sta $d010
		lda mplexZP.pbuf,x
		sta kVectors.spr7ID		;store it screen.
		lda mplexZP.cbuf,x
		sta $d02e
		inx
		cpx mplexZP.mnt
		bcc hlop9
		cpx mplexZP.cnt
		bne ok8
		jmp done

ok8		stx mplexZP.sptr
		lda $d001
		clc
		adc #$15
		cmp $d012
		bcc hlop9
		adc #$02
		sta $d012
		lda #<irq1
		sta $fffe
		lda #>irq1
		sta $ffff
		jmp eirq
hlop9 	jmp hlop1
.endc
;}}}

done 	lda #<irq0
		sta $fffe
		lda #>irq0
		sta $ffff
		lda # kRaster.bottomRaster
		sta $d012
eirq	pla
		tay
		pla
		tax
		pla
justRTI	rti
	

buildBackAndShadowChars
		lda #0
		sta ZPTemp
_loop
		; make a clean copy
		ldx #31
-		lda BackChars1,x
		sta fileChars,x
		sta tileMapTemp,x
		dex 
		bpl -
		; build the char offset
		ldx ZPTemp
		lda ShadowTileLUT,x
		bmi _next
		asl a
		asl a
		asl a
		sta ZPTemp2
		; build the pat offset
		lda ShadowTilePatLUT,x
		asl a
		asl a
		sta ZPTemp3
		; or first 2 bytes 4 times to upper half
		ldx ZPTemp2
		ldy ZPTemp3
		jsr or2Bytes2Shadow
		; or second 2 bytes 4 times to lower half
		iny
		iny
		jsr or2Bytes2Shadow		
		; copy char to dest
		lda ZPTemp
		asl a
		asl a
		asl a
		tax
		ldy ZPTemp2
		lda #7
		sta ZPTemp3
-		lda tileMapTemp,y
		sta fileChars+(4*8),x
		iny
		inx
		dec ZPTemp3
		bpl -
		; next char set
_next
		inc ZPTemp
		lda ZPTemp
		cmp #size(ShadowTileLUT)
		bne _loop
		rts
				

or2Bytes2Shadow
		lda #0
		sta ZPTemp4
-		lda tileMapTemp,x
		ora ShadowMaskBytes,y
		sta tileMapTemp,x
		inx
		lda tileMapTemp,x
		ora ShadowMaskBytes+1,y
		sta tileMapTemp,x
		inx
		lda ZPTemp4
		clc
		adc #2
		sta ZPTemp4
		cmp #4
		bne -
		rts			

BuildDisolveChars
		ldx #31
-		lda BlockChars1,x
		sta tileMapTemp,x
		sta tileMapTemp+32,x
		dex
		bpl -
		lda #0
		sta Pointer1
disolveANDORChar 		
		ldx Pointer1
		lda DisolveSourceCharOffsetLUT,x
		sta ZPTemp ; source char
		lda DisolveDestCharOffsetLUT,x
		sta ZPTemp2 ; dest char
		lda DisolveANDORROffsetLUT,x
		tay 
		clc
		adc #8
		sta ZPTemp4
-		ldx ZPTemp
		lda tileMapTemp,x
		and DisolveBlocksANDLUT,y
		ora DisolveBlocksORLUT,y
		ldx ZPTemp2
		sta tileMapTemp,x
		inc ZPTemp
		inc ZPTemp2
		iny
		cpy ZPTemp4
		bne -		
		inc Pointer1
		lda Pointer1
		cmp #size(DisolveANDORROffsetLUT)
		bne disolveANDORChar
		ldx #0
-		lda tileMapTemp,x
		sta fileChars+(16*8),x
		inx
		cpx #24*8
		bne -
		ldx #(8*4)-1
-		lda EmptyDisolveChars,x
		sta fileChars+(12*8),x
		dex
		bpl -
		rts		
			
unpackSprites	
		lda #<fileSprites	
		sta Pointer1	
		lda #>fileSprites	
		sta Pointer1+1	
		lda #<PackedSprites	
		sta Pointer2	
		lda #>PackedSprites	
		sta Pointer2+1	
		lda #0; (15*8)+4;size(SpriteUnpackTBL)
		sta ZPTemp3
		lda #3
		sta Pointer3
		lda #%1000  ; this if the last byte for patchup
		sta Pointer4
spriteUnpackLook
		ldx ZPTemp3  
		lda SpriteUnpackTBL,x
		cmp #0
		beq copyFullSprite
		cmp #1
		beq copy16x16Sprite
		bne copy16x21Sprite
nextSpriteUnpack
		lda ZPTemp3
		cmp #(15*8)/4
		beq _endPatchUp
		dec Pointer3
		bpl spriteUnpackLook
		lda #3
		sta Pointer3
		inc ZPTemp3
		lda ZPTemp3
		cmp #(15*8)/4
		bne spriteUnpackLook
_endPatchUp		
		lda Pointer4		
		beq _exit		
		lsr Pointer4		
		bcc copyFullSprite		
		bcs copy16x16Sprite		
_exit	rts		
			
advancePonterXByZPTemp	
		clc	
		lda Pointer1,x	
		adc ZPTemp	
		sta Pointer1,x	
		lda Pointer1+1,x	
		adc #0	
		sta Pointer1+1,x	
		rts	
			
copyFullSprite	
		ldy #64	
		sty ZPTemp
-		lda (Pointer2),y	
		sta (Pointer1),y	
		dey	
		bpl -	
		ldx #0
		jsr advancePonterXByZPTemp ; shift source and dest by 64 bytes
		ldx #2
		jsr advancePonterXByZPTemp
		jmp nextSpriteUnpack	
		
copy16x16Sprite
		lda #16
		sta ZPTemp2
		lda #((21-16)*3)+1
		sta ZPTemp4
sprite2To3Unpack
-		ldy #0
		lda (Pointer2),y
		sta (Pointer1),y
		iny
		lda (Pointer2),y
		sta (Pointer1),y
		lda #3
		sta ZPTemp
		ldx #0
		jsr advancePonterXByZPTemp
		dec ZPTemp
		ldx #2
		jsr advancePonterXByZPTemp
		dec ZPTemp2
		bne -
		ldx #0
		lda ZPTemp4
		sta ZPTemp
		jsr advancePonterXByZPTemp
		jmp nextSpriteUnpack

copy16x21Sprite			
		lda #21
		sta ZPTemp2			
		lda #1			
		sta ZPTemp4			
		jmp sprite2To3Unpack		
			
copyStuff	
		ldx #size(CopyDestLoLUT)-1	
-		lda CopyDestLoLUT,x	
		sta Pointer1	
		lda CopyDestHiLUT,x	
		sta Pointer1+1	
		lda CopySrcLoLUT,x
		sta Pointer2	
		lda CopySrcHiLUT,x	
		sta Pointer2+1	
		lda CopyBytes,x	
		tay
-		lda (Pointer2),y	
		sta (Pointer1),y	
		dey	
		bne -	
		dex	
		bpl --	
		rts	
			
CopyDestLoLUT .byte <fileChars+(44*8),<fileChars+(44*8)    ,<fileChars+(193*8),<fileChars+(193*8)    ,<fileChars+(12*8) ,<fileChars+(40*8)	
CopyDestHiLUT .byte	>fileChars+(44*8),(>fileChars+(44*8))+1,>fileChars+(193*8),(>fileChars+(193*8))+1,>fileChars+(12*8) ,>fileChars+(40*8)
CopySrcLoLUT  .byte	<LowerFixedChars ,<LowerFixedChars     ,<UpperFixedChars  ,<UpperFixedChars      ,<EmptyDisolveChars,<AppleChars
CopySrcHiLUT  .byte >LowerFixedChars ,(>LowerFixedChars)+1 ,>UpperFixedChars  ,(>UpperFixedChars)+1  ,>EmptyDisolveChars,>AppleChars	
CopyBytes	  .byte 000          ,size(LowerFixedChars)-255,000          ,size(UpperFixedChars)-255  ,32				,32
				
ShadowTileLUT    .byte 0,1,0,0,0,0,2,0		
ShadowTilePatLUT .byte 0,1,5,1,6,3,4,4		
ShadowMaskBytes
		.byte %10101111 ; pat 0
		.byte %01011111
		.byte %10101010
		.byte %01010101
		
		.byte %11111111 ; pat 1
		.byte %11111111
		.byte %10101010
		.byte %01010101
		
		.byte %11111010 ; pat 2
		.byte %11110101
		.byte %10101010
		.byte %01010101
		
		.byte %10100000 ; pat 3
		.byte %01010000
		.byte %11111010
		.byte %11110101
		
		.byte %11111010 ; pat 4
		.byte %11110101
		.byte %11111010
		.byte %11110101
		
		.byte %11111111 ; pat 5
		.byte %11111111
		.byte %11111010
		.byte %11110101
		
		.byte %11111010 ; pat 6
		.byte %11110101
		.byte %10101010
		.byte %01010101
		
DisolveBlocksORLUT
		.byte 0,0,0,0,0,0,0,0
		.byte %00110011
		.byte %11001100
		.byte %00000000
		.byte %01000100
		.byte %00010001
		.byte %01010101
		.byte %01010101
		.byte %01010101
		.byte %01010101
		.byte %01010101
		.byte %01010101
		.byte %01010101
	
DisolveBlocksANDLUT
		.byte 255,255,255,255,255,255,255,255
		.byte %00110011
		.byte %11001100
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte %00000000
		.byte 0,0,0,0,0,0,0

		
DisolveSourceCharOffsetLUT	.byte 02*8,03*8,08*8,09*8,10*8,12*8, 00*8,01*8,14*8,15*8,16*8,17*8,18*8,19*8,20*8,21*8
DisolveDestCharOffsetLUT	.byte 08*8,09*8,10*8,11*8,12*8,13*8, 14*8,15*8,16*8,17*8,18*8,19*8,20*8,21*8,22*8,23*8
DisolveANDORROffsetLUT		.byte 3   ,3   ,5   ,5   ,7   ,8   , 0   ,0   ,0   ,0   ,2   ,2   ,4   ,4   ,6   ,6
EntitySpriteColours		.byte 4,15,10,14,15,5,3,14
EntitySpriteStartFrame	.byte kSprBase+32,kSprBase+40,kSprBase+48,kSprBase+56,kSprBase+64,kSprBase+72,kSprBase+80,kSprBase+88

SpriteUnpackTBL
.byte 1,1
.byte 1,1
.byte 1,0
.byte 2,1
.byte 1,1
.byte 2,2
.byte 1,1
.byte 1,1
.byte 1,1
.byte 1,1
.byte 1,1
.byte 1,1
.byte 0,0
.byte 0,0
.byte 0,0
;.byte 0,0,0,1

IndexToORLUT	.byte 1,2,4,8,16,32,64,128
IndexToANDLUT	.byte 254,253,251,247,239,223,191,127
; 0 = right, 1 = up, 2 = left, 3 = down
;DirectionXLUT	.byte 6,	24-12,	25,		24-12
DirectionXLUT	.byte 8,	12,		24,	 	12
DirectionYLUT	.byte 50-8,	50,		50-8,	50-16 ; raw sprite Y offsets
NextDirectionLUT
.byte 3,3,1,1 ; heli
.byte 0,0,0,0 ; spring
.byte 2,2,0,0 ; worm
.byte 2,2,0,0 ; bat
.byte 3,0,1,2 ; ghost
.byte 3,3,1,1 ; spider
.byte 0,0,0,0 ; fish - not used
.byte 0,0,0,0 ; flying thing - not used
BaseAnimeFrameForDir
.byte kSprBase+32,kSprBase+32,kSprBase+32,kSprBase+32 ; heli
.byte kSprBase+40,kSprBase+40,kSprBase+40,kSprBase+40 ; spring
.byte kSprBase+52,kSprBase+52,kSprBase+48,kSprBase+48 ; worm
.byte kSprBase+60,kSprBase+60,kSprBase+56,kSprBase+56 ; bat
.byte kSprBase+68,kSprBase+68,kSprBase+64,kSprBase+64 ; ghost
.byte kSprBase+72,kSprBase+72,kSprBase+72,kSprBase+72 ; spider
.byte kSprBase+80,kSprBase+80,kSprBase+84,kSprBase+84 ; fish 
.byte kSprBase+92,kSprBase+92,kSprBase+88,kSprBase+88 ; flying thing 
FrameCountForEnt
.byte 008,004,004,004,004,002,004,004
CollFrameForEnt
.byte 000,000,000,001,000,000,000,000
CollisionResultEORForEnt
.byte 000,000,000,001,000,000,000,000
AnimFrameTimerForEnt
.byte 008,002,008,008,008,008,001,002
SpringDirectionToDeltaLUT
.char -2,-1,-1,-1,01,01,01,02 
SinJumpTable
.char -8, -6, -5, -4, -5, -3
.char -4, -3, -2, -3, -1, -2, -1, 0, -1, -1, 0 
kSinJumpFall = * - SinJumpTable 
.char  1,  2,  1,  3,  2,  3,  4  
.char  3,  5,  4,  5,  6,  5, 6,  6,  7, 8, 8 
kSinJumpMax = * - SinJumpTable - 1
SpringFrameFrameTable
.byte kSprites.springCompress,kSprites.springCompress,kSprites.springCompress
.byte kSprites.springNormal,kSprites.springNormal,kSprites.springNormal
.byte kSprites.springExpand,kSprites.springExpand,kSprites.springExpand
.byte kSprites.springFull,kSprites.springFull,kSprites.springFull
.byte kSprites.springFull,kSprites.springExpand,kSprites.springExpand,kSprites.springNormal,kSprites.springNormal

CircleJumpTableStart 
.char  5, 5, 5, 5, 4, 4, 4, 3, 2, 2, 1, 1, 0,-1,-1,-2,-2,-3,-4,-4,-4,-5,-5,-5,-5
.char -5,-5,-5,-4,-4,-4,-3,-3,-2,-1,-1, 0, 1, 1, 2, 3, 3, 4, 4, 4, 5, 5, 5
CircleJumpTableCount = * - CircleJumpTableStart	  
.char  5, 5, 5, 5, 4, 4, 4, 3, 2, 2, 1, 1, 0

statusLine0 .byte kSBC.TL,kSBC.T,kSBC.T,kSBC.T,kSBC.T,kSBC.T,kSBC.T,kSBC.TR	
statusLine1 .byte kSBC.L ,kSBC.M,kSBC.M,kSBC.M,kSBC.M,kSBC.M,kSBC.M,kSBC.R	
statusLine2 .byte kSBC.BL,kSBC.B,kSBC.B,kSBC.B,kSBC.B,kSBC.B,kSBC.B,kSBC.BR	
statusLine3 .byte kSBC.L ,kSBC.QWAKT,kSBC.QWAKT+1,kSBC.QWAKT+2,kSBC.QWAKT+3,kSBC.QWAKT+4,kSBC.QWAKT+5,kSBC.R	
statusLine4 .byte kSBC.L ,kSBC.QWAKB,kSBC.QWAKB+1,kSBC.QWAKB+2,kSBC.QWAKB+3,kSBC.QWAKB+4,kSBC.QWAKB+5,kSBC.R	
statusLine5 .byte kSBC.L ,kSBC.Score,kSBC.Score+1,kSBC.Score+2,kSBC.Score+3,kSBC.Score+4,kSBC.Score+5,kSBC.R
statusLine6 .byte kSBC.L ,kSBC.M,kSBC.High,kSBC.High+1,kSBC.High+2,kSBC.High,kSBC.M,kSBC.R
statusLine7 .byte kSBC.L ,kSBC.M,kSBC.QwakP,kSBC.QwakP+1,kSBC.M,kSBC.M,kSBC.M,kSBC.R	
statusLine8 .byte kSBC.L ,kSBC.M,kSBC.QwakP+2,kSBC.QwakP+3,kSBC.X,kSBC.M,kSBC.M,kSBC.R
statusLine9 .byte kSBC.L ,kSBC.M,kSBC.Flower,kSBC.Flower+1,kSBC.M,kSBC.M,kSBC.M,kSBC.R	
statusLine10 .byte kSBC.L ,kSBC.M,kSBC.Flower+2,kSBC.Flower+3,kSBC.X,kSBC.M,kSBC.M,kSBC.R

statusColour0 .byte kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col
statusColour1 .byte kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col
statusColour2 .byte kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col
statusColour3 .byte kSBC.Col,1,1,1,1,1,1,kSBC.Col
statusColour4 .byte kSBC.Col,3,3,3,3,3,3,kSBC.Col
statusColour5 .byte kSBC.Col,3,3,3,3,3,3,kSBC.Col
statusColour6 .byte kSBC.Col,kSBC.Col,3,3,3,3,kSBC.Col,kSBC.Col
statusColour7 .byte kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col
statusColour8 .byte kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col,0,kSBC.Col,kSBC.Col,kSBC.Col
statusColour9 .byte kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col,kSBC.Col
statusColour10 .byte kSBC.Col,kSBC.Col,13,13,0,kSBC.Col,kSBC.Col,kSBC.Col

statusLines .byte 0,1,3,4,1,2,0,1,5,1,1,1,6,1,1,1,7,8,1,1,9,10,1,2	
	
screenRowLUTLO		
.for ue = kVectors.charBase, ue < kVectors.charBase + $400, ue = ue + 40
.byte <ue
.next
screenRowLUTHi	
.for ue = kVectors.charBase, ue < kVectors.charBase + $400, ue = ue + 40
.byte >ue
.next

BackChars1 .binary "back_chars_1.raw"
BackChars2 .binary "back_chars_1.raw"
BackChars3 .binary "back_chars_1.raw"
BackChars4 .binary "back_chars_1.raw"
BlockChars1 .binary "wall1_chars.raw"
BlockChars2 .binary "wall2_chars.raw"
BlockChars3 .binary "wall3_chars.raw"
BlockChars4 .binary "wall4_chars.raw"
AppleChars .binary "apple_chars.raw"
ExclimationsChars .binary "excelmation_chars.raw"
CherryChars .binary "cherry_chars.raw"
SphereChars .binary "sphere_chars.raw"
LowerFixedChars .binary "fixed_section_chars.raw"
UpperFixedChars .binary "top_fixed_chars.raw"
EmptyDisolveChars .binary "empty_disolve_chars.raw"

PackedSprites .binary "sprites_small.bin"

fileCharCols ;		
.binary "testattribs.raw"	
fileTiles ;		
.binary "tiledefs.raw",0,32*4
* = fileTiles + (33*4)
.byte 06,05,10,03


*= $4000
fileScreen ;
;*= $4400
;fileCharCols ;		
;.binary "testattribs.raw"	
;*= $4500
;fileTiles ;		
;.binary "tiledefs.raw",32*4	; needs to be 80 bytes
;* = $4500 + (33*4)
;.byte 06,05,10,03
*= $4800
fileChars ;
;.fill 88+(24*8)
;.binary "testchars.raw",88+(24*8)
*= $5000
fileSprites ;
;.binary "sprites.bin"		
* = $7000		
fileTileMap; 
.binary "testmap.raw"

	