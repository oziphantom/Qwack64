;----------------------------
; TEST DEFINES
;----------------------------

.if BDD=1

PAT .macro 
	.byte $02
.endm
PXT .macro 
	.byte $12
.endm
PYT .macro 
	.byte $22
.endm
TTA .macro 
	.byte $32
.endm
TTX .macro 
	.byte $42
.endm
TTY .macro 
	.byte $52
.endm

INR .macro 
	.byte $62
.endm

.else

PAT .macro
	;
.endm
PXT .macro 
	;
.endm
PYT .macro 
	;
.endm
TTA .macro 
	;
.endm
TTX .macro 
	;
.endm
TTY .macro 
	;
.endm
INR .macro 
	;
.endm

.endif

;----------------------------
; FONT encoding
;----------------------------
;.comment
.enc qwak ;define an ascii->petscii encoding
.cdef "@@", 128
.cdef "AZ", 129
.cdef "[[", 155
.cdef "££", 156
.cdef "]]", 157
.cdef "^^", 158
.cdef "||", 159;->
.cdef "  ", 160
.cdef "!!", 161
.cdef "``", 162;"
.cdef "##", 163
.cdef "~~", 164 ;heart
.cdef "%%", 165
.cdef "&&", 166
.cdef "''", 167
.cdef "((", 168
.cdef "))", 169
.cdef "**", 170
.cdef "++", 171
.cdef ",,", 172
.cdef "--", 173
.cdef "..", 174
.cdef "//", 175
.cdef "09", 176
.cdef "::", 186
.cdef ";;", 187
.cdef "<<", 188
.cdef "==", 189
.cdef ">>", 190
.cdef "??", 191
;.endc
;----------------------------
; Constants Regeion
;----------------------------
kTileXCount = 16
kTileYCount = 12
kLevelSizeMax = kTileXCount*kTileYCount
kSprBase = 128-8
kBulletSpriteOffset = 1
kEntsSpriteOffset = 2
kBulletCollisionbox = 1
kSprites .block
	fish = kSprBase+80
	spiderLeft = kSprBase+72
	spiderRight = kSprBase+74
	springNormal = kSprBase+40
	springCompress = kSprBase+41
	springExpand = kSprBase+42
	springFull = kSprBase+43
	springFall = kSprBase+44
	bubbles = kSprBase+124
	bulletSprite = kSprBase+120
	Q = kSprBase+116
	W = kSprBase+117
	A = kSprBase+118
	K = kSprBase+119
	splat = kSprBase+123
.bend
kSpiderValues .block
	yFallDelta = 2
	rightStartWiggle = 255-32-14 ; 32 pixels but compenstating for the sprite width
	rightStartFall = 255-16-14 ; 16 pixels
	leftStartWiggle = 32+14
	leftStartFall = 16+14
	pauseEndFallFrames = 32
	riseDelayTime = 3
.bend
kVectors .block
	charBase = $C000
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
	spawnBubble = 30
.bend
kEntity .block
	heli = $00
	spring = $01
	worm = $02
	bat = $03
	ghost = $04
	spider = $05
	fish = $06
	circler = $07
	bear = $08
	octopuss = $09
	bearBody = $0A	
	octopussBody = $0B
	bubble = $0C
	bossDummy = $0D
	maxEntities = 27
	maxBubbleMakers = 8
	maxNumBubblesPerMaker = 2
.bend
kBoss .block
	hitPoints = 7
	hitPointsOctopuss = 9
	deathAnimTime = 25
	normal = 0
	dead = 1
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
	jumpDeltaAccumFloat = 4
	maxFallSpeed = 4
.bend
kJumpIndexs .block
	normal = 0
	floaty = 1
.bend
kPlayerAnimsIndex .block
	standRight = 0
	standLeft = 1
	standWalkRight = 2
	standWalkLeft = 3
	jumpRight = 4
	jumpLeft = 5
	flapRight = 6
	flapLeft = 7
	dead = 8
	exit = 9 
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
	exit = 22
	wall = 7
.bend
kPlayerState .block
	appear = 0
	normal = 1
	flap = 2
	jump = 3
	exit = 4
	dead = 5
.bend
kPlayerStateExit .block
	waitForAnimation = 0
.bend
kPlayerStateDeath .block
	animate = 0
.bend
kIntermission .block
	firstExit = kTileXCount*5
	secondExit = (kTileXCount*6)-1
.bend
kHideScreen .block
	hide = %11101111
	show = %11111111
.bend
.include "qwak_structs.asm"

;----------------------------
; ZP Regeion
;----------------------------


*= $00 
ZP_START ;@@RAM
.byte ?,? ; 00.01 
mplexZP .dstruct sMplexZP
;LevelTileMapPtrLo .byte ?
;LevelTileMapPtrHi .byte ?
;LevelKeyListPtrLo .byte ?
;LevelKeyListPtrHi .byte ?

;CollCharX	.byte ?
;CollCharY	.byte ?
;CollTileX	.byte ?
;CollTileY	.byte ?
;CollTLX		.byte ?
;CollTLY		.byte ?

EntityDataPointer	.word ?
CurrentEntity		.byte ?
CollidedEntity		.byte ?
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
TestingSprX1		.byte ?
TestingSprX2		.byte ?
TestingSprY1		.byte ?
TestingSprY2		.byte ?
GameStatePointer	.word ?

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
DidClipX			.byte ?  ; this is if the add X with MSB function did clip the Y
HideScreen			.byte ?
ZP_END ; @@ENDRAM

.cerror * > $FF, "Too many ZP variables"

;----------------------------
; Dumy Stack Regeion
;----------------------------
* = $100
STACK_START ; @@RAM
	.fill 256
STACK_END  ; @@ENDRAM


;----------------------------
; Variables
;----------------------------

* = $0200
variables = $0200 ;@@RAM
joyLeft	 .byte ?
joyRight .byte ?
joyUp	 .byte ?
joyDown	 .byte ?
joyFire	 .byte ?
oldJoyLeft	.byte ?
oldJoyRight .byte ?
oldJoyUp	.byte ?
oldJoyDown	.byte ?
oldJoyFire	.byte ?
joyUpStart  .byte ?
joyUpStop	.byte ?
joyFireEvent .byte ?
GameData .dstruct sGameData
LevelData .dstruct sLevelData
PlayerData .dstruct sPlayerData
TICK_DOWN_START = *
TickDowns .dstruct sTimerTickDowns
TICK_DOWN_END = *

;playerTile1			.byte ?
;playerTile2			.byte ?
;playerMidTile		.byte ?
;playerMidBelowTile	.byte ?
;playerMidBelowOtherTile .byte ?

;playerTile1X		.byte ?
;playerTile2X		.byte ?
;playerMidTileX		.byte ?
;playerMidBelowTileX .byte ?
;playerMidBelowOtherTileX .byte ?

;playerTile1Y		.byte ?
;playerTile2Y		.byte ?
;playerMidTileY		.byte ?
;playerMidBelowTileY .byte ?
;playerMidBelowOtherTileY .byte ?

EntityData .dstruct sEntityData


;.warn "Size of variables = ", *-variables
VARIABLES_END ;@@ENDRAM
.cerror * > $400, "Too many variables"


; ----- @Map Temp Store@ -----

* = $0400
tileMapTemp .fill 240 ;@@RAM

.cerror * > $500, "Too much level data"
MAP_TEMP_STORE_END ; @@ENDRAM
;----------------------------
; Unpacked Code section
;----------------------------

* = $6000
mplexBuffer     .dstruct sMplexBuffer ;@@RAM
mplexCodeBuffer .dstruct sMplexCodeBuffers ;@@ENDRAM @@RAMEX
irqBuffers .fill irqLength * 8 
irqBuffEnd ;@@ENDRAMEX 

.cerror * > $7000, "Too much Unpacked Code"
;---------------------------
; MACROS
;---------------------------

mCallFunctionTable .macro
	lda \1.hi,\2
	pha
	lda \1.lo,\2
	pha
	.INR
	rts
.endm

mMakeFunctionTable .macro	
lo .byte <(\@)-1	
hi .byte >(\@)-1	
.endm	

mMakeTable .macro
lo .byte <(\@)
hi .byte >(\@)
.endm

mConvertXToEntSpriteX .macro
	inx
	inx
.endm

mRestoreEntSpriteX .macro
	dex
	dex
.endm

.enc screen
*= $0801 ; 00 0C 08 0A 00 9E 20 32 30 36 34 00 00
CODE_START ;@@ROM
	.word (+), 2005 ;pointer, line number
	.null $9e, ^start;will be sys 4096
+	.word 0	 ;basic line end
	
.enc qwak	
	
*= $0810
start
		jsr setirq ; clear all interupts and put out own in
		lda #kHideScreen.hide
		sta HideScreen
		lda $dd02
		ora #3
		sta $dd02
		lda $dd00
		and #252
		;ora #3 0
		sta $dd00
		lda #%00000010
		sta $d018
		;lda #%00010000
		;sta $d011
		jsr copyStuff
		
		ldx #0
		lda #0
-		sta variables,x		; clear all the variables
		sta variables+$100,x
		sta tileMapTemp,x	; clear the tile map and after it so collisions is 00
		inx
		bne -
		
		jsr unpackSprites
		
		lda #<titleScreenLoop
		sta GameStatePointer
		lda #>titleScreenLoop
		sta GameStatePointer+1
	
		; this needs to be done first time outside the loop
		jsr buildBackAndShadowChars
		jsr BuildDisolveChars
		jsr copyFruitChars
		
		lda #1
		sta GameData.high
		lda #$C0
		sta GameData.musicMode
.if BDD=0		
		; main loop
MAINLOOP

-		lda mplexZP.lsbtod	
		beq -	
		dec mplexZP.lsbtod	
.else	
MAINLOOP
		inc $600	
.endif		
		jmp (GameStatePointer)	
;----------------------------			
; GAME LOOP			
;----------------------------			
GAMELOOP	
		jsr updateTickdowns
		ldx PlayerData.state
		.mCallFunctionTable PlayerCodeLUT,x
PlayerCodeLUT .mMakeFunctionTable PlayerAppear,PlayerNormal,PlayerNormal,PlayerNormal,PlayerExit,PlayerDead

PlayerAppear
		jsr convertLevelToTileMap
		jsr addShadowsToMap
		ldx # <tileMapTemp
		ldy # >tileMapTemp
		jsr plotTileMap
		jsr resetPlayerData 
		jsr setPlayerToSpawnPoint
		jsr unpackEntityBytes
		jsr setEntitySprites
		lda #1
		sta mplexZP.lsbtod
		lda #kPlayerState.normal
		sta PlayerData.state
		lda #0
		sta GameData.exitOpen
		lda #kHideScreen.show
		sta HideScreen
		jmp MAINLOOP
		
PlayerNormal
		jsr BuildEntCollisionTable
		jsr collidePlayerAgainstRest
		stx CollidedEntity
		lda PlayerData.hitBubbleNum
		sta ZPTemp2
		lda #0
		sta PlayerData.hitBubbleNum
		rol a ; pull is carry set
		sta ZPTemp
		beq _noSpriteCollision
		ldx CollidedEntity
		lda EntityData.type,x
		jsr isTypeBossBounceDetect
		bcs _bossBounce
		cpx EntityData.pipeBubbleStart
		bcc _normalEnt
		; so it was a bubble
		lda PlayerData.onGround
		ora PlayerData.isFalling
		beq _skipDeath
_acceptBubble		
		ldx CollidedEntity	
		lda mplexBuffer.ypos+kEntsSpriteOffset,x	
		cmp mplexBuffer.ypos	
		bcc _skipDeath	
		stx PlayerData.hitBubbleNum
		cmp ZPTemp2
		beq _skipDeath
		jsr enterOnGround	
_skipDeath	
		lda #0
		jmp _noSpriteCollision
_bossBounce
		lda PlayerData.hasShield
		beq _normalEnt
		ldx CollidedEntity
		jsr hurtBoss
		inc PlayerData.forceJump
		jmp _skipDeathCheck
_normalEnt
		lda ZPTemp
_noSpriteCollision
		ora PlayerData.dead
		beq _skipDeathCheck
		lda PlayerData.hasShield
		bne _skipDeathCheck
		dec GameData.lives		
		jsr pltLives		
		lda #kPlayerState.dead		
		sta PlayerData.state		
		sta PlayerData.minorState	
		jmp MAINLOOP		
_skipDeathCheck		
		lda #0
		sta PlayerData.dead
		jsr joyToPlayerDelta
		jsr checkSpriteToCharCollision
		; level skip
.if CRT == 0 
		lda joyUp
		and joyDown
		beq +
		lda #kPlayerState.exit
		sta PlayerData.state
		lda #0
		sta PlayerData.minorState
		jmp MAINLOOP
+		
.endif
		lda checkSpriteToCharData.xDeltaCheck
		beq _addY
		;make sure x reg is 0, and call addXWithMSBAndClip
		ldx #0
		jsr addXWithMSBAndClip
_addY		
+		lda mplexBuffer.ypos
		clc
		adc checkSpriteToCharData.yDeltaCheck
		sta mplexBuffer.ypos
		jsr updatePlayerAnim
		lda PlayerData.hasShield
		beq _noShield
		lda TickDowns.shieldFlashTimer
		bne _noShield
		lda mplexBuffer.sprc
		eor #7^14
		sta mplexBuffer.sprc
		lda TickDowns.shieldFlashTimerSpeedUp
		bne +
		lda #35
		sta TickDowns.shieldFlashTimerSpeedUp
		dec PlayerData.baseFlashTimeDelta
+		lda PlayerData.baseFlashTimeDelta
		sta TickDowns.shieldFlashTimer
		
_noShield
		jmp EndOfGameLoop

PlayerExit		
		lda PlayerData.minorState
		cmp #kPlayerState.exit
		bne _waitForAnimation
		; we have to set up the exit animation
		lda #kPlayerAnimsIndex.exit
		jsr setPlayerAnimeTo
		lda #kPlayerStateExit.waitForAnimation
		sta PlayerData.minorState
		lda PlayerData.exitAtIndex
		jsr setPlayerToIndexA
		jsr clearSheildState
_exit	jmp EndOfGameLoop		
_waitForAnimation			
		jsr updatePlayerAnim			
		bcc _exit
		lda #<INTERLOOP
		sta GameStatePointer
		lda #>INTERLOOP
		sta GameStatePointer+1
		lda #0
		sta PlayerData.state
		jsr disableAllEntSprites
		jmp MAINLOOP
		
incLevelGraphicSet		
		lda LevelData.levelGraphicsSet
		clc
		adc #1
		and #3
		sta LevelData.levelGraphicsSet
		rts
		
PlayerDead
		lda PlayerData.minorState
		cmp #kPlayerState.dead
		bne _waitForAnimation
		ldx #kSFX.hurt
		jsr playSFX
		; we have to set up the exit animation
		lda #kPlayerAnimsIndex.dead
		jsr setPlayerAnimeTo
		lda #kPlayerStateDeath.animate
		sta PlayerData.minorState
		jsr removePickups
_exit	jmp EndOfGameLoop		
_waitForAnimation			
		dec mplexBuffer.ypos
		jsr updatePlayerAnim			
		bcc _exit
;		jsr setPlayerToSpawnPoint
;		jsr enterOnGround
		lda GameData.lives
		beq _gameOver
		lda #kPlayerState.appear
		sta PlayerData.state
		lda #0
		sta PlayerData.dead
		jmp EndOfGameLoop
_gameOver
		lda #0
		sta PlayerData.state
		lda #<gameOverLoop
		sta GameStatePointer
		lda #>gameOverLoop
		sta GameStatePointer+1
		jmp MAINLOOP
		
EndOfGameLoop
		lda joyFireEvent              ; if    1 1 0 0
		eor PlayerData.bulletActive   ; and   0 1 0 1
		and joyFireEvent			  ; still 1 0 0 0
		beq _noBulletStart
		jsr startBullet
_noBulletStart
		jsr updateBullet
		jsr updateEntities
		jsr updateBubbles
		jsr animateDoor
		jmp MAINLOOP
		
		
;----------------------------			
; Intermission LOOP			
;----------------------------		
INTERLOOP
	jsr updateTickdowns
	ldx PlayerData.state
	mCallFunctionTable InterFuncLUT,x
InterFuncLUT mMakeFunctionTable interSetUp,interMovePlayer,interEnterDoor,interNextLevel
	
interSetUp	
	jsr PlotTransitionScreenAndMakeNextChars ; also set player index,exit index
	jsr setPlayerToSpawnPoint
	inc PlayerData.state
	lda #1
	sta PlayerData.movingLR
	sta PlayerData.onGround
	sta checkSpriteToCharData.xDeltaCheck
	sta GameData.exitOpen
	lda #$FF
	sta LevelData.exitIndex+1
	lda #0
	jsr changePlayerDir
	jsr setAnimateDoorToClose
	inc GameData.currLevel
	jsr deactivateAllEntities
	jsr removePickups
	jmp MAINLOOP

interMovePlayer	
	ldx #0
	jsr addXWithMSBAndClip	
	jsr updatePlayerAnim
	jsr animateDoor
	lda mplexBuffer.xpos
	cmp #8
	bne +
	inc PlayerData.state	
	lda #kPlayerAnimsIndex.exit
	jsr setPlayerAnimeTo
	jmp MAINLOOP
+	cmp #(11*16)+kBounds.screenMinX	
	bne +	
	jsr setAnimateDoorToOpen		
	lda #kIntermission.secondExit
	sta LevelData.exitIndex
	lda #kSFX.door
	jsr playSFX
+	jmp MAINLOOP
		
interEnterDoor	
	jsr updatePlayerAnim
	bcc _exit
	lda GameData.currLevel
	ldx #size(BossLevels)-1
-	cmp BossLevels,x
	beq _bossLevel
	dex
	bpl -
	and #1
	clc
	adc #1
	.byte $2c ; BIT XXXXX
_bossLevel
	lda #3
	jsr playMusic
	lda #kPlayerState.appear
	sta PlayerData.state
	lda #<GAMELOOP
	sta GameStatePointer
	lda #>GAMELOOP
	sta GameStatePointer+1
_exit
	jmp MAINLOOP
	
interNextLevel
	jmp MAINLOOP
	
;----------------------------			
; GAME OVER LOOP		
;----------------------------	
gameOverLoop
	ldx PlayerData.state
	mCallFunctionTable GameOverFuncLUT,x
GameOverFuncLUT mMakeFunctionTable GOSetup,GOWaitForFire

GoSetup
	; print string
	lda # kStrings.gameOver
	ldx # (4*16)+6 
	jsr plotStringAAtIndexX
	inc PlayerData.state
	; remove sprites
	jsr disableAllEntSprites
	lda #4
	jsr playMusic
	; check to see if this is the new high score
	ldx #0
_l	lda GameData.score,x
	cmp GameData.high,x
	beq _next
	bcs _higher
	jmp _clearScore
_next
	inx
	cpx #size(sGameData.score)
	bne _l
	; go to GOSetup
_clearScore
	ldx #size(sGameData.score) -1
	lda #0 
_l3	sta GameData.score,x
	dex
	bpl _l3
	jmp MAINLOOP
_higher
	ldx #size(sGameData.score) -1
_l2	lda GameData.score,x
	sta GameData.high,x
	dex
	bpl _l2
	jsr pltHighScore
	jmp _clearScore
	
GOWaitForFire
	;wait for fire
	jsr scanJoystick
	lda joyFire
	beq _exit
		; got to Title Screen State
	lda #kPlayerState.appear
	sta PlayerData.state
.if CRT==0	
	lda #<GAMELOOP
	sta GameStatePointer
	lda #>GAMELOOP
	sta GameStatePointer+1
.else
	lda #<titleScreenLoop
	sta GameStatePointer
	lda #>titleScreenLoop
	sta GameStatePointer+1
.endif
_exit	
	jmp MAINLOOP

;----------------------------			
; Title Screen Loop		
;----------------------------		
titleScreenLoop
	ldx PlayerData.state
	mCallFunctionTable TitleScreenLoopFuncLUT,x
TitleScreenLoopFuncLUT mMakeFunctionTable TSSetup,TSWaitForFire,TSStartGame	
	
TSSetup
	jsr emptyCRAM
	jsr emptyScreen
	jsr clearAllSprites
	lda #0
	sta $d020
	sta $d021 
	lda #TitleScreenData.index-TitleScreenData.string-1
	sta ZPTemp
_l	ldy ZPTemp
	lda TitleScreenData.index,y
	tax
	lda TitleScreenData.string,y
	jsr plotStringAAtIndexX
	dec ZPTemp
	bpl _l
	ldx #3
_l2	lda TitleScreenData.spriteY
	sta mplexBuffer.ypos,x
	lda TitleScreenData.spriteX,x
	sta mplexBuffer.xpos,x
	lda TitleScreenData.spriteDef,x
	sta mplexBuffer.sprp,x
	lda TitleScreenData.spriteCol,x
	sta mplexBuffer.sprc,x
	lda #1
	sta mplexBuffer.xmsb,x
	dex
	bpl _l2
	lda #$FF
	sta $D01C
	lda #%00001000
	sta $d016
	inc PlayerData.state
	lda #0
	sta ZPTemp2
	jsr playMusic
	lda #kHideScreen.show
	sta HideScreen
	jmp MAINLOOP
	
TSWaitForFire		
	jsr scanJoystick	
	jsr updateTickdowns	
	lda TickDowns.playerAnim	
	bne _noScroll	
	ldx ZPTemp2	
	lda GameData.musicMode
	clc
	rol a
	rol a
	rol a
	tay
	lda TitleScreenData.menuOffsetsEnd,y
	sta ZPTemp
	lda TitleScreenData.menuOffsetsStart,y
	tay		
-	lda TitleScreenData.spriteCol,x	
	sta $d800+(11*80),y
	txa	
	clc	
	adc #1	
	and #3	
	tax	
	iny	
	cpy ZPTemp	
	bne -	
	stx ZPTemp2
	lda #4
	sta TickDowns.playerAnim
	lda joyRight
	beq _notLeft
	lda GameData.musicMode
	sec
	sbc #64
_saveNoMode
	and #128+64
	sta GameData.musicMode
	lda #1
	ldy #38
-	sta $d800+(11*80),y
	dey
	bpl -
	bit GameData.musicMode
	bpl _startMusic
	lda #0
	.byte $2c
_startMusic
	lda #5
	jsr SID
	jmp _noScroll
_notLeft	
	lda joyLeft	
	beq _noScroll	
	lda GameData.musicMode
	clc
	adc #64	
	jmp _saveNoMode
_noScroll	
	lda joyFire
	beq _exit	
	lda oldJoyFire
	bne _exit
	inc PlayerData.state	
	lda #kHideScreen.hide
	sta HideScreen
_exit	
	jmp MAINLOOP	
	
TSStartGame
	lda #9
	sta $d021
	lda #0
	sta $d022
	sta $d020
	sta LevelData.levelGraphicsSet
	lda #1
	sta $d023	
	sta $d015
	lda #%00011000
	sta $d016
	lda #kSprBase
	sta mplexBuffer.sprp
	lda #255
	sta $d01c
	lda #7
	sta mplexBuffer.sprc
	lda #<GAMELOOP
	sta GameStatePointer
	lda #>GAMELOOP
	sta GameStatePointer+1
RESET		
	jsr clearPlayerStuct
	jsr clearAllSprites
	jsr emptyCRAM	
	jsr plotStatusArea
	; plot bottom row of screen
	ldx #39
	lda #1
-	sta kVectors.charBase + ( 24*40 ),x
	dex
	bpl -	
	lda #5
	sta GameData.lives
	jsr pltLives
	lda #<GAMELOOP
	sta GameStatePointer
	lda #>GAMELOOP
	sta GameStatePointer+1
	lda #kPlayerState.appear
	sta PlayerData.state
	lda #0
	sta GameData.currLevel	
	lda #1
	jsr playMusic
	jmp MAINLOOP	
	
	
;----------------------------			
; SUBS	
;----------------------------		
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
		sta PlayerData.movingLR
		lda joyLeft
		ora joyRight
		beq _noLR
		lda joyLeft
		bne _left
		lda PlayerData.slowMove
		beq +
		lda #1
		.byte $2c ; bit
+		lda #2
		sta checkSpriteToCharData.xDeltaCheck
		lda joyRight
		and oldJoyLeft
		beq _fullSpeedRight ; we were already going left
		lda PlayerData.onGround
		bne _clearSpeedRight
		lda #1
		.byte $2c
_clearSpeedRight
		lda #0
		sta PlayerData.slowMove
_fullSpeedRight
		lda #1
		sta PlayerData.movingLR
		lda #0
		jsr changePlayerDir
		jmp _endLR
_left	lda PlayerData.slowMove
		beq +
		lda #255
		.byte $2c ; bit
+		lda #254
		sta checkSpriteToCharData.xDeltaCheck
		lda joyLeft
		and oldJoyRight
		beq _fullSpeedLeft ; we were already going left
		lda PlayerData.onGround
		bne _clearSpeedLeft
		lda #1
		.byte $2c
_clearSpeedLeft
		lda #0
		sta PlayerData.slowMove
_fullSpeedLeft
		lda #1
		sta PlayerData.movingLR
		lda #1
		jsr changePlayerDir
		jmp _endLR
		
_noLR	lda #$80	
		sta PlayerData.startedJumpLR
		lda #0
		sta PlayerData.slowMove
_endLR
		lda PlayerData.movingLR
		bne +
		lda PlayerData.facingRight
		jsr changePlayerDir
+		lda PlayerData.onGround
		and joyUpStart
		ora PlayerData.forceJump
		bne StartJump
		lda PlayerData.onGround
		bne OnGround
		lda PlayerData.yDeltaAccum + 1
		bpl falling
		lda #0
		sta PlayerData.isFalling
		lda PlayerData.hasJumped ; if this is 1
		eor PlayerData.isFalling   ; and so is this, it will make it 0, other wise still 1
		and joyUpStop 			 ; and the player has let go
		bne AbortJump
;		lda PlayerData.onGround
;		bne OnGround
		; we are in air then
normalJumpUpdate
		ldx # kJumpIndexs.normal
customJumpUpdate
		jsr incPlayerYDeltaAndReturn
		lda PlayerData.yDeltaAccum + 1
		sta checkSpriteToCharData.yDeltaCheck
		rts
falling
		lda #1
		sta PlayerData.isFalling
		lda PlayerData.canFloat
		beq normalJumpUpdate
		jmp handleFall
OnGround
		lda #kPlayerState.normal
		sta PlayerData.state
		lda PlayerData.hitBubbleNum
		beq _skip
		lda #255
		.byte $2c
_skip
		lda #1
		sta checkSpriteToCharData.yDeltaCheck
		jsr changePlayerAnimForCurrentDir
_exit
		rts
AbortJump		
		lda #$80	
		sta PlayerData.yDeltaAccum	
		lda #$FF
		sta PlayerData.yDeltaAccum+1		
		rts		
StartJump
		lda #1
		sta PlayerData.hasJumped	; we are jumping
		lda #kPlayerState.jump
		sta PlayerData.state
		lda #0
		sta PlayerData.isFalling	; not falling
		sta PlayerData.onGround		; not on the ground
		sta PlayerData.yDeltaAccum	; set the Y jump accleration
		sta PlayerData.forceJump
		lda # kTimers.floatTimer	; reset the float timer
		sta PlayerData.floatTimer
		lda # kPlayerParams.jumpStartDelta	; set the other half of jump accleration
		sta PlayerData.yDeltaAccum + 1
		sta checkSpriteToCharData.yDeltaCheck
		jsr changePlayerAnimForCurrentDir
		ldx #kSFX.jump
		jsr playSFX
		rts	
handleFall
		lda PlayerData.state
		cmp #kPlayerState.jump
		bne _didntJustStartFalling
		lda joyUp ; if we just start falling, and joy is up and we have spring float
		beq _didntJustStartFalling
		lda #kPlayerState.flap
		sta PlayerData.state
		jmp _dontStopFloat
_didntJustStartFalling
		lda PlayerData.state
		cmp #kPlayerState.flap
		bne _checkUpStart
		lda joyUpStop
		beq _dontStopFloat
		lda #kPlayerState.jump
		sta PlayerData.state
		jmp normalJumpUpdate
_dontStopFloat
		lda PlayerData.floatTimer
		bpl +
		jmp normalJumpUpdate
+		dec PlayerData.floatTimer
		ldx #kJumpIndexs.floaty
		jmp customJumpUpdate
_checkUpStart		
		lda joyUpStart		
		bne +
		jmp normalJumpUpdate		
+		lda #kPlayerState.flap		
		sta PlayerData.state		
		ldx #kJumpIndexs.floaty
		jmp customJumpUpdate			
				
enterOnGround
		lda #kPlayerState.normal
		sta PlayerData.state
		lda #1
		sta PlayerData.onGround
		sta PlayerData.yDeltaAccum
		lda #0
		sta PlayerData.hasJumped
		sta PlayerData.isFalling		
		sta PlayerData.yDeltaAccum + 1
		sta PlayerData.slowMove
		lda PlayerData.facingRight		
		jsr changePlayerDir
		rts

changePlayerDir
		sta PlayerData.facingRight
changePlayerAnimForCurrentDir
		lda PlayerData.state
		cmp #kPlayerState.flap
		bne _notFlap
		lda #kPlayerAnimsIndex.flapRight
		bne _still
_notFlap
		lda PlayerData.onGround
		bne _onGround
		lda #kPlayerAnimsIndex.jumpRight
		bne _still
_onGround
		lda PlayerData.movingLR
		beq _notMoving
		lda #kPlayerAnimsIndex.standWalkRight
		bne _still
_notMoving
		lda #kPlayerAnimsIndex.standRight		
_still	clc
		adc PlayerData.facingRight
		jsr setPlayerAnimeTo
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
		cmp PlayerData.currAnim
		beq _dontchange
		sta PlayerData.currAnim
		tax
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
		
; returns carry clear if anim did not loop		
; carry is set if it did		
updatePlayerAnim
		lda PlayerData.frameCount
		cmp #2
		bcc _skip
		lda TickDowns.playerAnim
		beq _itTime
		clc
_skip	rts
_itTime
		lda PlayerData.frameOffset
		clc
		adc #1
		cmp PlayerData.frameCount
		bcc _store
		lda #0
_store	sta PlayerData.frameOffset
		php ; if we overflowed c will be set, else clear
		clc
		adc PlayerData.baseSprite
		sta mplexBuffer.sprp
		lda PlayerData.frameTimer
		sta TickDowns.playerAnim
		plp ; restore carry state
		rts 

PlayerJumpLUT .byte kPlayerParams.jumpDeltaAccum, kPlayerParams.jumpDeltaAccumFloat
						; Left	Right  Walk L	Walk R
PlayerSprLUT		.byte kSprBase,kSprBase+04,kSprBase+08,kSprBase+12,kSprBase+16,kSprBase+18,kSprBase+20,kSprBase+22,kSprBase+24,kSprBase+28
PlayerFrameCountLUT .byte 1	   	  ,1		  ,4		  ,4		  ,2		  ,2		  ,2		  ,2		  ,4		  ,4
PlayerAnimTimer		.byte 255     ,255		  ,8		  ,8		  ,8		  ,8		  ,8		  ,8		  ,8		  ,8

clearPlayerStuct
	ldx #size(sPlayerData)-1
	lda #0
-	sta PlayerData,x
	dex
	bpl -
	rts

removePickups	
	lda #0	
	sta PlayerData.hasShield
	sta PlayerData.canFloat
	sta PlayerData.hasSpring
	sta PlayerData.bulletActive
	sta PlayerData.bulletEgg	
	rts	

startBullet
	lda #1
	sta PlayerData.bulletActive
	ldx #kSFX.bubble
	jsr playSFX
	lda #0
	sta PlayerData.bulletUD
	sta PlayerData.bulletBurst
	lda PlayerData.facingRight
	sta PlayerData.bulletLR
	lda #200
	sta TickDowns.bulletLifeTimer	
	lda mplexBuffer.xpos
	sta mplexBuffer.xpos+kBulletSpriteOffset
	lda mplexBuffer.ypos
	sec
	sbc #3
	sta mplexBuffer.ypos+kBulletSpriteOffset
	lda mplexBuffer.xmsb
	sta mplexBuffer.xmsb+kBulletSpriteOffset
	lda PlayerData.bulletEgg
	beq _normal
	lda #kSprites.bulletSprite+7
	sta mplexBuffer.sprp+kBulletSpriteOffset
	lda #2
	sta mplexBuffer.sprc+kBulletSpriteOffset
	rts
_normal
	lda #kSprites.bulletSprite
	sta mplexBuffer.sprp+kBulletSpriteOffset
	lda #5
	sta mplexBuffer.sprc+kBulletSpriteOffset
	rts
	
updateBullet
	lda PlayerData.bulletActive
	beq bulletExit
	lda TickDowns.bulletLifeTimer
	bne bulletNotDead
removeBullet
	lda #0
	sta PlayerData.bulletActive
	lda PlayerData.bulletEgg
	beq +
	dec PlayerData.bulletEgg
+	lda #255
	sta mplexBuffer.ypos+kBulletSpriteOffset
bulletExit
	rts
burstBullet
	lda #kSprites.bulletSprite+3
	sta mplexBuffer.sprp+kBulletSpriteOffset
	lda #16
	sta TickDowns.bulletLifeTimer
	lda #1
	sta PlayerData.bulletBurst
	rts
bulletNotDead	
	lda PlayerData.bulletBurst
	bne bulletExit
	lda PlayerData.bulletEgg
	bne _bulletFull
	lda mplexBuffer.sprp+kBulletSpriteOffset
	cmp #kSprites.bulletSprite+2
	beq _bulletFull
	lda TickDowns.bulletLifeTimer
	and #$07
	bne _bulletFull
	inc mplexBuffer.sprp+kBulletSpriteOffset
_bulletFull	
	lda #kBulletCollisionbox
	sta CollideSpriteBoxIndex
	lda #kBulletSpriteOffset
	sta CollideSpriteToCheck
	lda #<UpdateBulletEndYColl
	sta Pointer1
	lda #>UpdateBulletEndYColl
	sta Pointer1+1
	lda #0
	sta CollisionResult
	ldy #0
	lda PlayerData.bulletUD
	beq +
	jmp entDown
+	jmp entUp
UpdateBulletEndYColl
	lda CollisionResult	
	beq _updateY
	lda PlayerData.bulletUD
	eor #1
	sta PlayerData.bulletUD
	bpl _checkX
_updateY
	lda mplexBuffer.ypos+kBulletSpriteOffset	
	clc	
	adc checkSpriteToCharData.yDeltaCheck
	sta mplexBuffer.ypos+kBulletSpriteOffset	
_checkX	
	lda #<UpdateBulletEndXColl
	sta Pointer1
	lda #>UpdateBulletEndXColl
	sta Pointer1+1
	lda #$00
	sta CollisionResult
	ldy #0
	lda PlayerData.bulletLR
	bne +
	jmp entRight
+	jmp entLeft
	; do some more collision checking here
UpdateBulletEndXColl	
	lda CollisionResult	
	beq _updateX
	lda PlayerData.bulletLR
	eor #1
	sta PlayerData.bulletLR
	bpl _checkEnts
_updateX
	ldx #kBulletSpriteOffset	
	jsr addXWithMSBAndClip	
	lda DidClipX	
	beq _checkEnts	
	lda PlayerData.bulletLR
	eor #1
	sta PlayerData.bulletLR	
_checkEnts	
	jsr collideBulletAgainstRest	
	bcc _exit2 ; didn't hit one	
	lda EntityData.type,x
	jsr isTypeBoss
	bcs _boss
;	cmp #kEntity.bear
;	bcc _notBoss
;	cmp #kEntity.octopussBody+1
;	bcc _boss
;	cmp #kEntity.bossDummy
;	beq _boss
_notBoss
	lda #0
	sta EntityData.active,x
	lda #1
	sta EntityData.speed,x
	lda #255
	sta mplexBuffer.ypos+kEntsSpriteOffset,x
	sta EntityData.entState,x
	sta EntityData.movTimer,x
	jmp burstBullet
_exit2
	rts
_boss
	lda PlayerData.bulletEgg
	beq _exit2 ; only accept eggs for the boss
	lda EntityData.type,x
	jsr isTypeBossBounceDetect
	bcs _found
	dex
	jmp _boss
_found
	jsr hurtBoss
	jmp burstBullet

; carry clear not boss, set boss
isTypeBoss	
	cmp #kEntity.bear
	bcc _notBoss
	cmp #kEntity.octopussBody+1
	bcc _boss
	cmp #kEntity.bossDummy
	beq _boss	
_notBoss
	clc
	.byte $24
_boss
	sec
	rts

isTypeBossBounceDetect	
	cmp #kEntity.bear	
	beq _yes	
	cmp #kEntity.octopuss	
	beq _yes	
	clc	
	.byte $24	
_yes	
	sec	
	rts	
	
hurtBoss	
	lda EntityData.entState,x
	cmp #kBoss.dead
	beq _exit
	lda EntityData.movTimer+1,x
	bne _exit
	dec EntityData.active,x
	lda EntityData.active,x
	cmp #1
	beq _killedBoss
	; we need to flash them so the player knows they did something
	lda #01
	jsr setBossSpriteToColour
	lda #16
	sta EntityData.movTimer+1,x ; store the flash timer in the 2nd sprite
_exit
	rts
_killedBoss
	; well just killed the boss
	lda #kBoss.dead
	sta EntityData.entState,x
	lda #kSprites.splat
	sta mplexBuffer.sprp+kEntsSpriteOffset,x
	sta mplexBuffer.sprp+kEntsSpriteOffset+1,x
	sta mplexBuffer.sprp+kEntsSpriteOffset+2,x
	sta mplexBuffer.sprp+kEntsSpriteOffset+3,x
	lda #kBoss.deathAnimTime
	sta EntityData.movTimer,x
	stx ZPTemp
	lda #kScoreIndex.boss
	jsr giveScore
	ldx ZPTemp
	rts

clearAllSprites
	ldx #0
clearSpritesInternal
-	txa			  ;transfer x to a to create index vals
	sta mplexZP.sort,x	;store it in the sort table
	lda #255
	sta mplexBuffer.ypos,x	; disbale all sprites
	inx			  
	cpx # mPlex.kMaxSpr+1	 ;have we reached 32 yet?
	bne -
	rts
	
disableAllEntSprites
	lda #255
	ldx #mplex.kMaxSpr
-	sta mplexBuffer.ypos,x
	dex
	bne -
	rts
	
emptyCRAM
		ldx #00
		lda #0
-		sta $d800,x
		sta $d900,x
		sta $da00,x
		sta $db00,x
	;	sta $dc00,x
		dex
		bne -
		rts
		
emptyScreen
		ldx #00
		lda #" "
-		sta kVectors.charBase,x
		sta kVectors.charBase+$100,x
		sta kVectors.charBase+$200,x
		sta kVectors.charBase+$300,x
	;	sta $dc00,x
		dex
		bne -
		rts		
			
convertLevelToTileMap
		lda #0
		sta LevelData.numKeys
		sta LevelData.totalKeys
		sta EntityData.numPipes
		sta EntityData.lastPipeUsed
		lda #$FF
		sta LevelData.exitIndex
		sta LevelData.exitIndex+1
		lda #<tileMapTemp
		sta Pointer1
		;sta LevelTileMapPtrLo
		lda #>tileMapTemp
		sta Pointer1+1
		;sta LevelTileMapPtrHi
		ldx GameData.currLevel
		lda LevelTableLo,x
		sta Pointer2
		lda LevelTableHi,x
		sta Pointer2+1
; read level pointers
		ldy #0
		sty ActiveTileIndex
		;lda (Pointer2),y
		;clc
		;adc Pointer2
		;sta LevelKeyListPtrLo
		iny
		;lda (Pointer2),y
		;adc Pointer2+1
		;sta LevelKeyListPtrHi
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
		cmp # kTiles.pipe
		beq _pipe
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
		lda ActiveTileIndex
		sta LevelData.playerIndex
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
		ldx LevelData.exitIndex
		cpx #$FF
		bne _2nd
		sta LevelData.exitIndex
		jmp +
_2nd	sta LevelData.exitIndex+1		
+		lda # kTiles.exit
		jmp _cont
_pipe
		ldx EntityData.numPipes
		lda ActiveTileIndex
		sec
		sbc #16
		sta EntityData.pipeIndex,x
		inx
		stx EntityData.numPipes
		lda # kTiles.pipe
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
		.byte kTiles.shadowOpenCorner		; 1010
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
		lda #kTileYCount ; num rows
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
		sta Pointer3
		lda Pointer2+1
		adc #0
		sta Pointer2+1
		eor # (>kVectors.charBase) ^ $d8
		sta Pointer3+1
		pla
		clc
		adc #1
		pha
		tay
		cpy #kTileXCount
		bne _pltX
		pla
		clc
		lda Pointer1
		adc #kTileXCount
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
		adc #48 ;48
		sta Pointer2
		sta Pointer3
		lda Pointer2+1
		adc #0
		sta Pointer2+1
		eor # (>kVectors.charBase) ^ $d8
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
		ldx #4
-		lda joyLeft,x
		sta oldJoyLeft,x
		lda #0
		sta joyLeft,x
		dex
		bpl -
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
_joyEnd lda oldJoyUp
		eor joyUp
		and joyUp
		sta joyUpStart
		lda joyUp
		eor OldJoyUp
		and OldJoyUp
		sta joyUpStop
		lda oldJoyFire
		eor joyFire
		and joyFire
		sta joyFireEvent
		rts
		
_joyUp	
		stx joyUp
		lsr ; skip down bit
		bcc _joyDown
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
	

CollisionBoxesX .byte 02,02,02,04,00,$e8,12,04 ; $e8 = -24
CollisionBoxesW .byte 13,13,13,16,48, 48,01,16 
CollisionBoxesY .byte 02,02,00,04,00, 12,12,01 
CollisionBoxesH .byte 12,16,20,16,12, 30,01,08 


convertXSingleByteEntX	
	lda mplexBuffer.xmsb,x
	bne _notMSB
	lda mplexBuffer.xpos,x
	clc	
	adc #255-24-1
	rts
_notMSB
	lda mplexBuffer.xpos,x
	sec
	sbc # kBounds.screenMinX
	rts
	
newCollision
	ldx CollideSpriteToCheck
	ldy CollideSpriteBoxIndex
	; calc the final Xs
	jsr convertXSingleByteEntX
	clc
	adc CollisionBoxesX,y
	adc checkSpriteToCharData.xDeltaCheck
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
	cmp #208
	bcs +
	rts ; 0 - 192 = safe 192-208 = shared 16 off screen
+	cmp #240
	bcc _bottomOfScreen
	; top of screen
	lda #193
	rts
_bottomOfScreen
	lda #208
	rts
	
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
	cmp CollideCharBLI
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
	; abort jump
	lda PlayerData.hitBubbleNum
	beq _startFall
	lda #0
	sta checkSpriteToCharData.yDeltaBackup
	rts
_startFall
	lda #1	
	sta PlayerData.isFalling	
	lda #2
	sta PlayerData.yDeltaAccum
	sta PlayerData.yDeltaAccum+1
	rts	
_onGround
	lda #0
	sta checkSpriteToCharData.yDeltaBackup
	jsr enterOnGround
_noOnGround
	lda #$0
	sta checkSpriteToCharData.yDeltaCheck
	beq _exit
_checkDown
	lda PlayerData.hitBubbleNum
	bne _exit
	lda ZPTemp3
	ora ZPTemp4
	bne _onGround
	ldx PlayerData.onGround
	lda #0
	sta PlayerData.onGround
	cpx #1
	beq _startFall
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
CheckForShadowPlots
	ldx #1
	jsr _checkRemoveTile
	ldx #16
	jsr _checkRemoveTile
	ldx #17
	jmp _checkRemoveTile
	
_checkRemoveTile	
	stx ZPTemp	
	lda ActiveTileIndex
	pha
	clc
	adc ZPTemp
	cmp #kLevelSizeMax
	bcs _exit2
	sta ActiveTileIndex
	tay
	jsr tileIsSafeToChange
	bcc _exit2
	jsr clearTileNew
_exit2
	pla
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
	
TileFuncLookup .byte kTiles.fruit,kTiles.flower,kTiles.key1,kTiles.key2,kTiles.key3,kTiles.key4,kTiles.spike,kTiles.spring,kTiles.potion,kTiles.shield,kTiles.exit,kTiles.egg	
TileFuncLUTLo .byte <fruitFunc-1 ,<flowerFunc-1,<keyFunc-1 ,<keyFunc-1 ,<keyFunc-1 ,<keyFunc-1 ,<spikeFunc-1,<springFunc-1,<potionFunc-1,<shildFunction-1,<exitFunc-1,<eggFunc-1
TileFuncLUTHi .byte >fruitFunc-1 ,>flowerFunc-1,>keyFunc-1 ,>keyFunc-1 ,>keyFunc-1 ,>keyFunc-1 ,>spikeFunc-1,>springFunc-1,>potionFunc-1,>shildFunction-1,>exitFunc-1,>eggFunc-1

fruitFunc
	jsr clearTileNew
	lda #kScoreIndex.Fruit
	jsr giveScore
	ldx #kSFX.collect
	jsr playSFX
	rts 	
	
flowerFunc
	jsr clearTileNew
	lda #kScoreIndex.fruit
	jsr giveScore
	ldx #kSFX.flower
	jsr playSFX
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
	ldx #kSFX.collect
	jsr playSFX
	rts
_changeDoor
	lda #1
	sta GameData.exitOpen	
	ldx #kSFX.door
	jsr playSFX
	rts	

spikeFunc	
	lda #1	
	sta PlayerData.dead 
	rts
	 
springFunc
	jsr clearTileNew
	ldx #kSFX.powerup
	jsr playSFX
	lda #1
	sta PlayerData.hasSpring
	sta PlayerData.canFloat
	rts
	
potionFunc
	jsr clearTileNew
	ldx #0	
	stx ActiveTileIndex	
_loop	
	lda tileMapTemp,x	
	cmp #kTiles.spike	
	bne _next	
	lda #kTiles.fruit	
	sta tileMapTemp,x
	jsr pltSingleTileNew	
_next	
	lda ActiveTileIndex	
	clc	
	adc #1	
	sta ActiveTileIndex
	tax
	cmp #kLevelSizeMax	
	bne _loop	
	ldx #kSFX.powerup
	jsr playSFX
	rts
	
shildFunction
	jsr clearTileNew
	lda #1
	sta PlayerData.hasShield
	ldx #kSFX.powerup
	jsr playSFX
	lda # <endShieldNMI
	sta $FFFA
	lda # >endShieldNMI
	sta $FFFB
	lda #$FF	
	sta $DD04		
	lda #$FF	
	sta $DD05		
	lda #$97 ; about 10 seconds 	
	sta $DD06		
	lda #0
	sta $DD07
	lda #$82	; make it fire an NMI on Timer B underflow
	sta $DD0D	
	lda $DD0D   ; ack any NMI
	lda #$91
	sta $DD0E	
	lda #%01011001
	sta $DD0F	
	lda #14
	sta mplexBuffer.sprc
	lda #50
	sta TickDowns.shieldFlashTimerSpeedUp
	lda #16
	sta PlayerData.baseFlashTimeDelta
	rts		

endShieldNMI	
	pha
	jsr clearSheildState
	pla
	rti
	
clearSheildState
	lda # <justRTI
	sta $FFFA
	lda # >justRTI
	sta $FFFB	
	lda #0
	sta PlayerData.hasShield
	sta $DD0D
	lda #7
	sta mplexBuffer.sprc
	lda $DD0D	
	rts
	
	
exitFunc	
	lda GameData.exitOpen
	beq _notOpen
	lda #0
	sta GameData.exitOpen
	lda ActiveTileIndex
	sta PlayerData.exitAtIndex
	lda #kPlayerState.exit
	sta PlayerData.state
	sta PlayerData.minorState
_notOpen
	rts

eggFunc
	jsr clearTileNew
	inc PlayerData.bulletEgg
	ldx #kSFX.powerup
	jsr playSFX
	rts

awardLife
	inc GameData.lives
	jmp pltLives
	
animateDoor
	lda GameData.exitOpen
	beq aDexit
	lda TickDowns.doorAnim
	bne aDexit
	lda #kTimers.DoorAnimeRate
	sta TickDowns.doorAnim
	lda LevelData.exitIndex
	sta ActiveTileIndex
	jsr animateInternal
	lda LevelData.exitIndex+1
	cmp #$ff
	beq aDexit
	sta ActiveTileIndex
	jmp animateInternal
aDexit 
	rts	
		
animateInternal
	lda LevelData.exitFrame
animateDoorCmp ; @@ENDROM @@RAMEX
	cmp #kDoorOpen
	beq aDexit
animateDoorCLC ; @@ENDRAMEX @@ROM
	clc
animateDoorCLCEND
	adc #1
	sta LevelData.exitFrame
	jmp pltSingleTileNoLookupNew ; skips below
	
setAnimateDoorToOpen
	lda #kDoorClosed
	sta LevelData.exitFrame
	lda #kDoorOpen
	sta animateDoorCmp+1
	lda #$18 ; CLC
	sta animateDoorCLC
	lda #$69 ; ADC #
	sta animateDoorCLC+1
	rts
	
setAnimateDoorToClose
	lda #kDoorOpen
	sta LevelData.exitFrame
	lda #kDoorClosed
	sta animateDoorCmp+1
	lda #$38 ; SEC
	sta animateDoorCLC
	lda #$E9 ; SBC #
	sta animateDoorCLC+1
	rts
	
playSFX
	bit GameData.musicMode
	bvc audioExit
	lda SNDTBL.hi,x
	tay
	lda SNDTBL.lo,x
	ldx #14
	;lda #<effect        ;Start address of sound effect data
    ;ldy #>effect
    ;ldx #channel        ;0, 7 or 14 for channels 1-3
    jmp SID+6
audioExit
	rts
	
playMusic
	bit GameData.musicMode
	bpl audioExit
	jmp SID
	
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
	
.comment
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
.endc 	
clearTileNew
	ldy ActiveTileIndex
	lda # kTiles.back
	sta tileMapTemp,y
	jsr calcBCDEforTileY ; this sets it to be what it should be shadow wise
;	jsr pltSingleTileNew
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
	
removeAllTilesOf
	sta ZPTemp5
	ldx #0
	stx ActiveTileIndex
	stx TestingSprX1
_loop
	lda tileMapTemp,x
	cmp ZPTemp5
	bne _next
	jsr clearTileNew
;	lda ActiveTileIndex
;	sta Pointer4
;	inc ActiveTileIndex
;	ldy ActiveTileIndex
;	cpy #kLevelSizeMax
;	beq _next
;	jsr tileIsSafeToChange
;	bcc _skipLeft
;	jsr clearTileNew	
;_skipLeft	
;	lda ActiveTileIndex
;	clc
;	adc #15 ; it is + 1 already
;	cmp #kLevelSizeMax
;	beq _restoreACI
;	sta ActiveTileIndex
;	tay
;	jsr tileIsSafeToChange
;	bcc _restoreACI
;	jsr clearTileNew
;_restoreACI
;	lda Pointer4
;	sta ActiveTileIndex
	jsr CheckForShadowPlots
_next	
	inc TestingSprX1
	ldx TestingSprX1
	stx ActiveTileIndex
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
	boss = 3
.bend	
	
FruitScore	.byte 0,0,0,1,0,0,15,15
FlowerScore .byte 0,0,0,5,0,0,15,15
KeyScore	.byte 0,0,0,2,5,0,15,15
BossScore	.byte 0,1,0,0,0,0,15,15

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
	
; returns Y into ZPTemp
convertIndexToEntSpriteXY	
	sta ZPTemp3
	and #$f0
	clc
	adc #50 
	sta mplexBuffer.ypos+kEntsSpriteOffset,x
	sta ZPTemp
	lda ZPTemp3
	and #$0f
	asl a
	asl a
	asl a
	asl a
	cmp #$f0
	bne _notMsb
	lda #0
	sta mplexBuffer.xmsb+kEntsSpriteOffset,x
	lda #8
	bne _storeX
_notMsb
	pha
	lda #$ff
	sta mplexBuffer.xmsb+kEntsSpriteOffset,x
	pla
	clc
	adc #24
_storeX
	sta mplexBuffer.xpos+kEntsSpriteOffset,x
	rts
.comment		
convertSpriteXSingleByte
	lda mplexBuffer.xmsb,x
	beq _MSB
	lda mplexBuffer.xpos,x
	sec
	sbc #kBounds.screenMinX
	rts
_MSB
	lda mplexBuffer.xpos,x
	adc #255-(kBounds.screenMinX+1)
	rts
.endc	
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
	lda LevelData.playerIndex
setPlayerToIndexA	
	pha
	asl a
	asl a
	asl a
	asl a
	cmp #$F0
	beq _msbMode
	clc
	adc #kBounds.screenMinX
	sta mplexBuffer.xpos
	sta mplexBuffer.xmsb
_Y	pla
	and #$F0
	clc
	adc #kBounds.screenMinY
	sta mplexBuffer.ypos
	ldx #kBulletSpriteOffset
	lda #kSprites.bulletSprite
	sta mplexBuffer.sprp,x
	lda #2
	sta mplexBuffer.sprc,x
	lda #255
	sta mplexBuffer.ypos,x
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
	tya
	ldx #kEntity.maxEntities
-	sta EntityData.animBase,x
	sta EntityData.animFrame,x
	sta EntityData.entState,x
	dex
	bpl -
	lda (EntityDataPointer),y
	sta ZPTemp2 ; number of entities
	sta EntityData.number
	beq _e
	iny			; next byte
	ldx #0
	sta EntNum
_l  lda (EntityDataPointer),y
	jsr convertIndexToEntSpriteXY
	iny			; next byte	
	lda (EntityDataPointer),y
	lsr a
	lsr a
	lsr a
	lsr a
	sta EntityData.type,x
	cmp #kEntity.Bear
	bne +
	jmp _BossBear
+	cmp #kEntity.Octopuss
	bne +
	jmp _BossOctopuss	
+	lda ZPTemp
	sta EntityData.originalY,x
	lda #0
	sta EntityData.entState,x
	sta EntityData.speed,x
	lda (EntityDataPointer),y
	and #3
	sta EntityData.direction,x	
	lda #1	
	sta EntityData.active,x 
_nextEnt
	iny 		; next byte
	inx
	dec ZPTemp2
	lda ZPTemp2
	bne _l
_e	
	ldx EntityData.number ; keep number
	stx EntityData.pipeBubbleStart
	lda EntityData.numPipes
	beq _noPipes
	.cerror kEntity.maxNumBubblesPerMaker != 2, "need to change code so it handles new mul"
	asl a ; times two
	clc ; probably not needed as num pipes must be below 128
	adc EntityData.number
	sta EntityData.number ; add the bubble ents
_setupBubbleLoop
	lda #kEntity.bubble
	sta EntityData.type,x
	lda #0
	sta EntityData.entState,x
	sta EntityData.direction,x
	sta EntityData.active,x
	inx
	cpx EntityData.number
	bne _setupBubbleLoop
_noPipes
	rts
_BossBear	
	lda #kEntity.bear
	sta EntityData.type,x
	lda #kEntity.bearBody
	sta EntityData.type+1,x
	lda #kEntity.bossDummy
	sta EntityData.type+2,x
	sta EntityData.type+3,x
	lda #kBoss.hitPoints
	sta EntityData.active,x
_sharedBoss
	lda #kEntity.bossDummy
	sta EntityData.type+2,x
	sta EntityData.type+3,x
	lda EntityData.number
	clc
	adc #3 ; insert 3 more ents for the rest of the boss
	sta EntityData.number
	lda #1
	sta EntityData.active+1,x
	sta EntityData.active+2,x
	sta EntityData.active+3,x
	txa
	sta EntityData.entState+1,x
	sta EntityData.entState+2,x
	sta EntityData.entState+3,x
	lda mplexBuffer.xmsb+kEntsSpriteOffset,x
	sta mplexBuffer.xmsb+kEntsSpriteOffset+1,x
	sta mplexBuffer.xmsb+kEntsSpriteOffset+2,x
	sta mplexBuffer.xmsb+kEntsSpriteOffset+3,x
	lda mplexBuffer.ypos+kEntsSpriteOffset,x
	sec
	sbc #9
	sta mplexBuffer.ypos+kEntsSpriteOffset,x
	sta mplexBuffer.ypos+kEntsSpriteOffset+1,x
	clc
	adc #21
	sta mplexBuffer.ypos+kEntsSpriteOffset+2,x
	sta mplexBuffer.ypos+kEntsSpriteOffset+3,x
	lda mplexBuffer.xpos+kEntsSpriteOffset,x
	sec
	sbc #8
	sta mplexBuffer.xpos+kEntsSpriteOffset,x
	sta mplexBuffer.xpos+kEntsSpriteOffset+2,x
	clc
	adc #24
	sta mplexBuffer.xpos+kEntsSpriteOffset+1,x
	sta mplexBuffer.xpos+kEntsSpriteOffset+3,x
	lda #0
	sta EntityData.entState,x
	sta EntityData.speed,x
	lda (EntityDataPointer),y
	and #3
	sta EntityData.direction,x	
	lda #25
	sta EntityData.movTimer,x
	sta EntityData.movTimer+1,x
	inx
	inx
	inx
	; x is now + 3 so when nextEnt is called it will be +4	
	jmp _nextEnt
_BossOctopuss	
	lda #kEntity.octopuss
	sta EntityData.type,x
	lda #kEntity.octopussBody
	sta EntityData.type+1,x
	lda #kBoss.hitPointsOctopuss
	sta EntityData.active,x
	jmp _sharedBoss	
		
setEntitySprites
	ldx EntityData.number
	beq _exit
_active
	.PXT
	stx CurrentEntity
	lda EntityData.type,x
	cmp #kEntity.bear
	beq _bossBear
	cmp #kEntity.bearBody
	beq _nextEnt
	cmp #kEntity.octopuss
	beq _bossOctopuss
	cmp #kEntity.octopussBody
	beq _nextEnt
	tay
	lda EntitySpriteColours,y
	sta mplexBuffer.sprc+kEntsSpriteOffset,x
	jsr setEntSpriteForDirection
	.TTX
_nextEnt
	dex
	bpl _active
_exit
	lda EntityData.numPipes
	beq _exit2
	ldx EntityData.pipeBubbleStart
	lda #$ff
_loop	
	sta mplexBuffer.ypos+kEntsSpriteOffset,x
	inx
	cpx #kEntity.maxEntities
	bne _loop
_exit2
	rts
	
_bossBear
	jsr setBossToCorrectColour
	lda #kSprBase+96
	sta mplexBuffer.sprp+kEntsSpriteOffset,x
	sta EntityData.animBase,x
	lda #kSprBase+99
	sta mplexBuffer.sprp+kEntsSpriteOffset+1,x
	sta EntityData.animBase+1,x
	lda #kSprBase+102
	sta mplexBuffer.sprp+kEntsSpriteOffset+2,x
	sta EntityData.animBase+2,x
	lda #kSprBase+104
	sta mplexBuffer.sprp+kEntsSpriteOffset+3,x
	sta EntityData.animBase+3,x
	bne _nextEnt
	
_bossOctopuss
	jsr setBossToCorrectColour
	lda #kSprBase+106
	sta mplexBuffer.sprp+kEntsSpriteOffset,x
	sta EntityData.animBase,x
	lda #kSprBase+109
	sta mplexBuffer.sprp+kEntsSpriteOffset+1,x
	sta EntityData.animBase+1,x
	lda #kSprBase+112
	sta mplexBuffer.sprp+kEntsSpriteOffset+2,x
	sta EntityData.animBase+2,x
	lda #kSprBase+114
	sta mplexBuffer.sprp+kEntsSpriteOffset+3,x
	sta EntityData.animBase+3,x
	bne _nextEnt
	
setBossToCorrectColour
	lda EntityData.type,x
	tay
	lda EntitySpriteColours,y
setBossSpriteToColour	
	sta mplexBuffer.sprc+kEntsSpriteOffset,x
	sta mplexBuffer.sprc+kEntsSpriteOffset+1,x
	sta mplexBuffer.sprc+kEntsSpriteOffset+2,x
	sta mplexBuffer.sprc+kEntsSpriteOffset+3,x
	rts
	
deactivateAllEntities	
	ldx #kEntity.maxEntities-1	
	lda #0	
-	sta EntityData.active,x	
	dex
	bpl -
	rts
		

; build hte collision data for each ent first
BuildEntCollisionTable
	ldx # kEntity.maxEntities-1
innerEntitiesLoopColl
	lda EntityData.active,x
	beq updateEntitiesLoopColl
	.PXT
	jsr MakeMinMaxXYForX	
	.TTX	
updateEntitiesLoopColl
	dex
	bpl innerEntitiesLoopColl
	rts
	

addYDeltaEnt
	ldx CurrentEntity	
	lda mplexBuffer.ypos+kEntsSpriteOffset,x	
	clc	
	adc checkSpriteToCharData.yDeltaCheck	
	sta mplexBuffer.ypos+kEntsSpriteOffset,x
	rts
	
updateEntities
	ldx # kEntity.maxEntities-1
innerEntitiesLoop
	.PXT
	lda EntityData.active,x
	bne EntitiesActive
	lda EntityData.entState,x
	bpl updateEntitiesLoop
	dec EntityData.movTimer,x
	lda EntityData.movTimer,x
	bne updateEntitiesLoop
	lda EntityData.originalY,x
	sta mplexBuffer.yPos+kEntsSpriteOffset,x
	lda #0
	sta EntityData.entState,x
	lda #1
	sta EntityData.active,x
updateEntitiesLoop
	.TTX
	dex
	bpl innerEntitiesLoop
	rts
EntitiesActive
	stx CurrentEntity
	lda EntityData.type,x
	tay	
	.mCallFunctionTable EntUpdateFuncLUT,y		
EntUpdateFuncLUT .mMakeFunctionTable entNormalMovement,springEntFunc,EntNormalMovement,entBat,entGhostFunc,entSpiderFunc,entFishFunc,circlerFunc,entBoss,entBoss,nextEnt,nextEnt,entBubble,nextEnt

entNormalMovement	
+	jsr updateEntAnimAndSetSprite
	lda CollFrameForEnt,y	
	sta CollideSpriteBoxIndex	
	.TTX
	.mConvertXToEntSpriteX
	stx CollideSpriteToCheck
	lda #<handleEntCollisionResult
	sta Pointer1
	lda #>handleEntCollisionResult
	sta Pointer1+1
	ldx CurrentEntity
	lda EntityData.speed,x
	tay
	lda EntityData.direction,x	
	tax
	lda #0
	sta CollisionResult
	.mCallFunctionTable ENTDirectionCheckFuncLUT,x
ENTDirectionCheckFuncLUT .mMakeFunctionTable entRight,entUp,entLeft,entDown
;right, up, left ,down			
;ENTDirectionCheckFuncLUTLo .byte <entRight-1,<entUp-1,<entLeft-1,<entDown-1	
;ENTDirectionCheckFuncLUTHi .byte >entRight-1,>entUp-1,>entLeft-1,>entDown-1	
	
entPositiveTBL   .byte 002,004
entPositiveTBLUD .byte 001,002
entNegativeTBL   .byte 254,252
entNegativeTBLUD .byte 255,254

entRight	
	lda entPositiveTBL,y	
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
	lda entNegativeTBLUD,y
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
	lda entNegativeTBL,y	
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
	lda entPositiveTBLUD,y
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
	lda mplexBuffer.ypos+kEntsSpriteOffset,x
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
	sta mplexBuffer.ypos+kEntsSpriteOffset,x
	lda EntityData.entState,x
	lsr a
	lsr a ; div 4
	cmp #8
	bcc _safe
	lda #7
_safe
	clc
	adc #kSprites.fish
	sta mplexBuffer.sprp+kEntsSpriteOffset,x
	jmp nextEnt

entSpiderFunc	
	lda EntityData.entState,x	
	tay	
	.mCallFunctionTable SpiderEntFuncLUT,y
SpiderEntFuncLUT .mMakeFunctionTable spiderLookPlayer,spiderFall,spiderRise 
	
spiderLookPlayer
	ldx #0
	stx ZPTemp2
	jsr convertXSingleByteEntX
	sta ZPTemp
	ldx CurrentEntity
	.mConvertXToEntSpriteX
	jsr convertXSingleByteEntX
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
	sta mplexBuffer.sprp+kEntsSpriteOffset,x
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
	.mConvertXToEntSpriteX
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
;	ldx CurrentEntity
;	lda mplexBuffer.ypos+kEntsSpriteOffset,x
;	clc
;	adc #kSpiderValues.yFallDelta
;	sta mplexBuffer.ypos+kEntsSpriteOffset,x
	jsr addYDeltaEnt
	jmp nextEnt
		
spiderRise	
	dec EntityData.movTimer,x
	lda EntityData.movTimer,x
	bpl +
	lda #kSpiderValues.riseDelayTime
	sta EntityData.movTimer,x
	lda mplexBuffer.ypos+kEntsSpriteOffset,x
	sec
	sbc #1
	sta mplexBuffer.ypos+kEntsSpriteOffset,x
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
	lda mplexBuffer.xpos+kEntsSpriteOffset,y
	clc
	adc CircleJumpTableStart,x
	sta mplexBuffer.xpos+kEntsSpriteOffset,y
	lda mplexBuffer.ypos+kEntsSpriteOffset,y
	clc
	adc CircleJumpTableStart + ( CircleJumpTableCount / 4) + 1,x
	sta mplexBuffer.ypos+kEntsSpriteOffset,y
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
	.mConvertXToEntSpriteX ; current entity
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
	jsr collideEntAgainstRest
	bcs _hit
	; didn't hit so carry on
	ldx CurrentEntity
	lda mplexBuffer.ypos+kEntsSpriteOffset,x
	clc
	adc checkSpriteToCharData.yDeltaCheck
	sta mplexBuffer.ypos+kEntsSpriteOffset,x
	lda EntityData.entState,x
	clc
	adc #1
	cmp #kSinJumpMax
	bcc _store
	lda #kSinJumpMax-1
_store
	sta EntityData.entState,x
	jmp springEntHandleX
_hit
	ldx CurrentEntity
	lda ZPTemp2
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
_hit	
	lda #256-1		
	ldx CurrentEntity	
	sta EntityData.direction,x 		 
	jmp springEndAnimate
_noCollideRight
	jsr collideEntAgainstRest
	bcs _hit
	ldx CurrentEntity
	.mConvertXToEntSpriteX
	jsr addXWithMSBAndClip
	.mRestoreEntSpriteX
	lda DidClipX
	beq _noclip
	lda #256-1
	bmi _store
_noclip
	lda EntityData.direction,x 	
	clc
	adc #1
	and #3
_store
	sta EntityData.direction,x	
	jmp springEndAnimate		
springEntXLeft	
	lda CollisionResult		
	beq _noCollideLeft		
_hit		
	lda #1		
	ldx CurrentEntity
	sta EntityData.direction,x 		 
	jmp springEndAnimate
_noCollideLeft
	jsr collideEntAgainstRest
	bcs _hit
	ldx CurrentEntity
	.mConvertXToEntSpriteX
	jsr addXWithMSBAndClip
	.mRestoreEntSpriteX
	lda DidClipX
	beq _noclip2
	lda #1
	bpl _store2
_noClip2	
	lda EntityData.direction,x 	
	sec
	sbc #1
	cmp #256-5
	bne _store2
	lda #256-4	
_store2	
	sta EntityData.direction,x
springEndAnimate
	ldx CurrentEntity
	lda EntityData.entState,x
	tay
	cmp #kSinJumpFall
	bcs _fall
	lda	SpringFrameFrameTable,y
	sta mplexBuffer.sprp+kEntsSpriteOffset,x
	jmp nextEnt
_fall
	lda #kSprites.springFall
	sta EntityData.animBase,x
	jsr updateEntAnimAndSetSprite
	jmp nextEnt

entGhostFunc
	lda #<entGhostXResults
	sta Pointer1
	lda #>entGhostXResults
	sta Pointer1+1
	.mConvertXToEntSpriteX
	stx CollideSpriteToCheck
	ldx CurrentEntity
	lda EntityData.speed,x
	tay
	lda EntityData.direction,x
	cmp #4
	bcc +
	lda #0
	sta EntityData.direction,x
	; 0 00= UpRight	
	; 1 01= UpLeft	
+	and #1
	beq ghostLeft
; ghostRight
	jmp entRight
ghostLeft
	jmp entLeft
entGhostXResults
	ldx CurrentEntity
	lda CollisionResult	
	beq _addXDelta
_toggleX
	ldx CurrentEntity	
	lda EntityData.direction,x
	eor #1
	sta EntityData.direction,x
	jsr setEntSpriteForDirection
	jmp entGhostCheckY
_addXDelta
	jsr collideEntAgainstRest
	bcs _togglex	
	ldx CurrentEntity
	.mConvertXToEntSpriteX
	jsr addXWithMSBAndClip
	lda DidClipX
	bne _toggleX
entGhostCheckY
	lda #<entGhostYResults
	sta Pointer1
	lda #>entGhostYResults
	sta Pointer1+1
	ldx CurrentEntity
	lda EntityData.speed,x
	tay
	lda EntityData.direction,x
	and #2
; 2 10= DownRight	
; 3 11= DownLeft	
	bne _down	
	; up	
	jmp entUp	
_down	
	jmp entDown	
entGhostYResults
	ldx CurrentEntity
	lda CollisionResult	
	beq _entGhostCheckSprites
_toggleY
	ldx CurrentEntity	
	lda EntityData.direction,x
	eor #2
	sta EntityData.direction,x
_entHitAndGoNext	
	jmp nextEnt
_entGhostCheckSprites	
	jsr collideEntAgainstRest
	bcs _toggleY	
	jsr addYDeltaEnt
;	ldx CurrentEntity	
;	lda mplexBuffer.ypos+kEntsSpriteOffset,x	
;	clc	
;	adc checkSpriteToCharData.yDeltaCheck	
;	sta mplexBuffer.ypos+kEntsSpriteOffset,x	
	jsr updateEntAnimAndSetSprite
	jmp nextEnt	

entBat
	; we check to see if we can fall down
	lda CollFrameForEnt+kEntity.bat ; this might change per frame	
	sta CollideSpriteBoxIndex	
	.mConvertXToEntSpriteX ; current entity
	stx CollideSpriteToCheck	
	lda #<entBatYResults
	sta Pointer1
	lda #>entBatYResults
	sta Pointer1+1
	;lda EntityData.speed,x
	;tay
	ldy #1 ; fall fast
	jmp entDown
entBatYResults
	ldx CurrentEntity
	lda CollisionResult	
	bne _dontFall
	; yes update Y
	jsr addYDeltaEnt
_dontFall
	; jump to normal left right update
	jmp entNormalMovement
	
handleEntCollisionResult	
	ldx CurrentEntity	
;	lda EntityData.type,x
;	tay
	lda CollisionResult	
;	eor CollisionResultEORForEnt,y	
	beq _addDeltas
_entHitAndGoNext
	.TTX
	jsr setNextEntDir	
	jmp nextEnt	
_addDeltas	
	jsr collideEntAgainstRest
	bcs _entHitAndGoNext
	jsr addYDeltaEnt ; will set X to current Ent	
	.mConvertXToEntSpriteX
	jsr addXWithMSBAndClip
	lda DidClipX
	beq _skipFlipDueToX
	lda mplexBuffer.xpos,x	; x was increased above
	sec	
	sbc checkSpriteToCharData.xDeltaCheck ; undo the move	
	sta mplexBuffer.xpos,x
	jsr setNextEntDir
_skipFlipDueToX	
nextEnt
	ldx CurrentEntity	
	jmp updateEntitiesLoop

entBubble 
	lda mplexBuffer.ypos+kEntsSpriteOffset,x
	sec
	sbc #1
	cmp #kBounds.screenMinY - 24
	bne _safe
	lda #0
	sta EntityData.active,x
	lda #$FF
_safe
	sta mplexBuffer.ypos+kEntsSpriteOffset,x
;	cpx PlayerData.hitBubbleNum
;	bne _noPlayer
;	dec mplexBuffer.ypos
_noPlayer
	jsr updateEntAnimAndSetSprite
	jmp nextEnt ; for now	
		
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
	sta mplexBuffer.sprp+kEntsSpriteOffset,x
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
	sta mplexBuffer.sprp+kEntsSpriteOffset,x
_notAnimUpdate 		
	rts
	

updateBubbles
	ldx EntityData.numPipes
	beq _exit
	lda TickDowns.bubbleTimer
	bne _exit
	ldx EntityData.pipeBubbleStart
_findFreeEnt
	lda EntityData.active,x
	beq _foundOne
	inx
	cpx EntityData.number
	bne _findFreeEnt
	beq _exit
_foundOne
	stx ZPTemp2 ; bubble ent number
	lda #1
	sta EntityData.active,x
	ldy EntityData.lastPipeUsed
	lda EntityData.pipeIndex,y
	jsr convertIndexToEntSpriteXY
	lda mplexBuffer.xpos+kEntsSpriteOffset,x
	sec
	sbc #4
	sta mplexBuffer.xpos+kEntsSpriteOffset,x
	lda #kTimers.spawnBubble
	sta TickDowns.bubbleTimer
	lda EntityData.lastPipeUsed
	clc
	adc #1
	cmp EntityData.numPipes
	bne _store
	lda #0
_store
	sta EntityData.lastPipeUsed
	ldx #kSFX.ebubble
	jsr playSFX
_exit
	rts 

entBoss
	lda EntityData.entState,x
	tay
	.mCallFunctionTable BossLUT,y
BossLut .mMakeFunctionTable BossNormal,BossDeath

BossNormal
	lda EntityData.movTimer+1,x
	beq _notFlash
	dec EntityData.movTimer+1,x
	bne _notFlash
	jsr setBossToCorrectColour
_notFlash
	jsr AnimateUpperHalfBoss
	dec EntityData.movTimer,x
	beq _doneMove
	lda EntityData.movTimer,x
	cmp #16
	bcs _noMove
	jsr AnimateLowerHalfBoss
	lda EntityData.direction,x
	bne _left
	dec mplexBuffer.xpos+kEntsSpriteOffset,x
	dec mplexBuffer.xpos+kEntsSpriteOffset+2,x
	dec mplexBuffer.xpos+kEntsSpriteOffset+1,x
	dec mplexBuffer.xpos+kEntsSpriteOffset+3,x
	lda mplexBuffer.xpos+kEntsSpriteOffset,x
	cmp #kBounds.screenMinX
	bne _noClipMinX
_toggleDir
	lda EntityData.direction,x
	eor #2 ; switch from 0 & 2
	sta EntityData.direction,x
_noClipMinX
	jmp _noMove
_left
	inc mplexBuffer.xpos+kEntsSpriteOffset,x
	inc mplexBuffer.xpos+kEntsSpriteOffset+2,x
	inc mplexBuffer.xpos+kEntsSpriteOffset+1,x
	inc mplexBuffer.xpos+kEntsSpriteOffset+3,x
	lda mplexBuffer.xpos+kEntsSpriteOffset+1,x
	cmp #$ff
	beq _toggleDir
	jmp _noMove
_doneMove
	lda EntityData.type,x
	sec
	sbc #kEntity.bear
	tay
	lda BossMoveTimerLut,y
	sta EntityData.movTimer,x
_noMove
	jmp nextEnt

BossMoveTimerLut .byte 32,24	
	
BossDeath
	dec mplexBuffer.ypos+kEntsSpriteOffset,x
	dec mplexBuffer.ypos+kEntsSpriteOffset+1,x
	inc mplexBuffer.ypos+kEntsSpriteOffset+2,x
	inc mplexBuffer.ypos+kEntsSpriteOffset+3,x
	dec mplexBuffer.xpos+kEntsSpriteOffset,x
	dec mplexBuffer.xpos+kEntsSpriteOffset+2,x
	inc mplexBuffer.xpos+kEntsSpriteOffset+1,x
	inc mplexBuffer.xpos+kEntsSpriteOffset+3,x
	dec EntityData.movTimer,x
	bne _exit
	lda #0
	sta EntityData.active,x
	sta EntityData.active+1,x
	sta EntityData.active+2,x
	sta EntityData.active+3,x
	lda #$ff
	sta mplexBuffer.ypos+kEntsSpriteOffset,x
	sta mplexBuffer.ypos+kEntsSpriteOffset+1,x
	sta mplexBuffer.ypos+kEntsSpriteOffset+2,x
	sta mplexBuffer.ypos+kEntsSpriteOffset+3,x
_exit
	jmp nextEnt
	
AnimateLowerHalfBoss
	lda EntityData.animTimer,x
	beq _anim
	dec EntityData.animTimer,x
_exit
	rts
_anim
	lda EntityData.animFrame+2,x
	eor #1
	sta EntityData.animFrame+2,x
	sta ZPTemp
	lda EntityData.animBase+2,x
	clc
	adc ZPTemp
	sta mplexBuffer.sprp+kEntsSpriteOffset+2,x
	lda EntityData.animBase+3,x
	clc
	adc ZPTemp
	sta mplexBuffer.sprp+kEntsSpriteOffset+3,x
	lda #4
	sta EntityData.animTimer,x
	rts

;<<<<<32--0--32>>>>>	
AnimateUpperHalfBoss
	stx ZPTemp
	.mConvertXToEntSpriteX
	jsr convertXSingleByteEntX
	sta ZPTemp2
	ldx #0 ; player
	jsr convertXSingleByteEntX
	sta ZPTemp3
	cmp ZPTemp2
	bcc _playerLeft
	sbc ZPTemp2 ; carry is already set
	cmp #32
	bcc _under
	lda #2
	.byte $2c ; bit XXXX
_under
	lda #1
	.byte $2c
_playerLeft
	lda #0	
	sta ZPTemp4
	ldx ZPTemp
	sta EntityData.animFrame,x
	clc	
	adc EntityData.animBase,x	
	sta mplexBuffer.sprp+kEntsSpriteOffset,x	
	lda ZPTemp4	
	clc	
	adc EntityData.animBase+1,x	
	sta mplexBuffer.sprp+kEntsSpriteOffset+1,x	
	rts
		
collideBulletAgainstRest
	ldy #3
	ldx #1
	bne collideAgainstRestEntry
collidePlayerAgainstRest
	ldx #0
	ldy #0
collideAgainstRestEntry
;;	jsr makeMinMaxYForPlayer
	lda mplexBuffer.ypos,x
	clc
	adc CollisionBoxesY,y
	sta Pointer3
	sta TestingSprY1
	clc
	adc CollisionBoxesH,y
	sta Pointer3+1
	sta TestingSprY2
;;	jsr makeMinMaxXForPlayer
	;ldx #0
	jsr convertXSingleByteEntX
	clc
	adc CollisionBoxesX,y
	sta TestingSprX1
	clc
	adc CollisionBoxesW,y
	sta TestingSprX2
	lda #$FF
	sta CurrentEntity ; so we don't skip any
	jmp collideAgainstEntPlayerEntry
collideEntAgainstRest
	; start at the mplex y + 1 and check to see if the Y is in Range
	; to do this we need to know which collsiion box the ent we are is using
	; and the one that the other is using
	; a hit is if my x1 <= y2 && y1 <= x2
	; where x1 = my Ent Y, x2 = my Ent Y+Height 
	; y1 = Other Ent Y, y2 = other Ent Y+Height
	;dec $d020
	ldx CurrentEntity
	ldy #0
	lda EntityData.collisionX1,x
	clc
	adc checkSpriteToCharData.xDeltaCheck
	sta TestingSprX1 ; cache X
	lda EntityData.collisionX2,x
	clc
	adc checkSpriteToCharData.xDeltaCheck
	sta TestingSprX2
	lda EntityData.collisionY1,x
	clc
	adc checkSpriteToCharData.yDeltaCheck
	sta TestingSprY1
	lda EntityData.collisionY2,x
	clc
	adc checkSpriteToCharData.yDeltaCheck
	sta TestingSprY2
	;inc $d020
collideAgainstEntPlayerEntry
	;dec $d020
	ldy #2 ; other slot
	ldx #0
-	cpx CurrentEntity
	beq Ent_Ent_Coll_skipSelf
	lda EntityData.active,x
	beq Ent_Ent_Coll_skipSelf
	bmi Ent_Ent_Coll_skipSelf ; if there active is 0 or - don't collide
	lda #0	
	sta ZPTemp	
	lda TestingSprY1
	cmp EntityData.collisionY2,x
	jsr doMinMaxBitTest
	lda EntityData.collisionY1,x
	cmp TestingSprY2
	jsr doMinMaxBitTest
	lda ZPTemp	
	and #3	
	beq hitY	
Ent_Ent_Coll_skipSelf	
	inx	
	cpx EntityData.number	
	bne -
	;inc $d020
	clc
	rts
	
hitY ; now we need to do the same thing but for the X	
	lda #0	
	sta ZPTemp	
	lda TestingSprX1
	cmp EntityData.collisionX2,x
	jsr doMinMaxBitTest
	lda EntityData.collisionX1,x
	cmp TestingSprX2
	jsr doMinMaxBitTest
	lda ZPTemp
	and #3
	beq hitX
	jmp Ent_Ent_Coll_skipSelf
hitX
;	inc $d020
	sec
	rts

MakeMinMaxXYForX
	lda EntityData.type,x
	tay
	lda CollFrameForEnt,y
	tay
	.PXT
	.PYT
	.mConvertXToEntSpriteX ; convert to all sprites not just ents
	jsr convertXSingleByteEntX
	.mRestoreEntSpriteX ; convert back
	.TTX
	.TTY
	clc
	adc CollisionBoxesX,y
	sta EntityData.collisionX1,x
	clc
	adc CollisionBoxesW,y
	sta EntityData.collisionX2,x
	lda mplexBuffer.ypos+kEntsSpriteOffset,x
	clc
	adc CollisionBoxesY,y
	sta EntityData.collisionY1,x
	clc
	adc CollisionBoxesH,y
	sta EntityData.collisionY2,x
	rts

doMinMaxBitTest
	beq _secPass		
	bcc _secPass		
	bcs _secFail		
_secPass
	clc					
_secFail
	rol ZPTemp			
	rts					
				
plotStringAAtIndexX			
	pha			
	txa			
	jsr convertIndexToScreenAndCRAM ; screen to Pointer2, CRAM to Pointer3			
	pla			
	tax			
	lda StringTableLUTLO,x			
	sta Pointer1			
	lda StringTableLUTHI,x			
	sta Pointer1+1			
	ldy #0			
_l	lda (Pointer1),y			
	cmp #$ff			
	beq _done			
	sta (Pointer2),y			
	lda #1			
	sta (Pointer3),y			
	iny			
	bne _l			
_done			
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
	lda #< mplexCodeBuffer.unrolledCopyLoopDest
	sta Pointer1
	lda #> mplexCodeBuffer.unrolledCopyLoopDest
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
	sta mplexCodeBuffer.unrolledCopyLoopDestEnd ; save the rts 
	
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
	
	lda $dc0d		 ;acknowledge any irq that has occured during setup.
	lda $dd0d
	inc $d019
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
		jsr mplexCodeBuffer.unrolledCopyLoopDest
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
		and HideScreen
		sta $d011
		lda #11
		sta $d025
		lda #1
		sta $d026
		lda GameData.musicMode
		beq +
		jsr SID+3
+		jmp eirq

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
		ldx LevelData.levelGraphicsSet
		lda BackCharsLUT,x
		tax
		stx ZPTemp5
		lda #0
		sta ZPTemp
_loop
		; make a clean copy
		ldx ZPTemp5
		ldy #31
-		lda BackChars1,x
		sta fileChars,y
		sta tileMapTemp,y
		dex 
		dey
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
		ldx LevelData.levelGraphicsSet
		lda BackCharsLUT,x
		tax
		stx ZPTemp5
		ldy #31
-		lda BlockChars1,x
		sta tileMapTemp,y
		sta tileMapTemp+32,y
		dex
		dey
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
		; patch the colours
		ldx LevelData.levelGraphicsSet
		lda WallColourLUT,x
		ldy #23
-		sta fileCharCols+16,y
		dey
		bpl -
		rts		
			

copyFruitChars
		ldx LevelData.levelGraphicsSet
		lda BackCharsLUT,x
		tax
		ldy #31
-		lda AppleChars,x
		sta fileChars+(40*8),y
		dex
		dey
		bpl -
		lda LevelData.LevelGraphicsSet
		asl a
		asl a ; x4
		tax
		ldy #0
-		lda FruitColourLut,x
		sta fileCharCols+40,y
		inx
		iny
		cpy #4
		bcc -
		rts

unpackSprites	
		sei
		lda #$34
		sta $01 ; hide IO
		lda #>fileSprites
		sta _loop+2		
		lda #0		
_loop2	ldx #0		
_loop	sta fileSprites,x
		dex
		bne _loop
		inc _loop+2
		ldx _loop+2
		cpx #>fileSprites+$2000
		bne _loop2
		 
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
		cmp #((15*8)/4)+1
		beq _endPatchUp
		dec Pointer3
		bpl spriteUnpackLook
		lda #3
		sta Pointer3
		inc ZPTemp3
		lda ZPTemp3
		cmp #((15*8)/4)+1
		bne spriteUnpackLook
_endPatchUp		
		lda Pointer4		
		beq _exit		
		lsr Pointer4		
		bcc copyFullSprite		
		bcs copy16x16Sprite		
_exit	lda #$35 ; bring IO back
		sta $01
		cli
		rts		
			
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
		cpy #$ff		
		bne -	
		dex	
		bpl --	
		rts	


PlotTransitionScreenAndMakeNextChars			
		jsr ClearMapScreenToSolidBlack	
		ldy #31
-		lda fileChars+(16*8),y
		sta fileChars+(84*8),y
		dey
		bpl -
		ldy #3
-		lda fileCharCols+16,y
		sta fileCharCols+84,y
		dey
		bpl - 
		lda #kIntermission.firstExit	
		sta ActiveTileIndex	
		sta LevelData.playerIndex
		sta LevelData.exitIndex
		lda #kDoorOpen
		jsr pltSingleTileNoLookupNew	
		lda #kIntermission.secondExit	
		sta ActiveTileIndex	
		lda #kDoorClosed	
		jsr pltSingleTileNoLookupNew				
		ldx #(kTileXCount/2)-1	
_firstLoop	
		txa	
		pha	
		inc ActiveTileIndex
		lda #32
		jsr pltSingleTileNoLookupNew
		pla
		tax
		dex
		bpl _firstLoop				
					
		jsr incLevelGraphicSet				
		jsr buildBackAndShadowChars
		jsr BuildDisolveChars
		jsr copyFruitChars			
					
		ldx #(kTileXCount/2)-1	
_secondLoop	
		txa	
		pha	
		inc ActiveTileIndex
		lda #kBlocks.wall
		jsr pltSingleTileNoLookupNew
		pla
		tax
		dex
		bpl _secondLoop
			
		rts		
				
ClearMapScreenToSolidBlack			
		ldx #kTileYCount*2			
		lda #<kVectors.charBase ; should be 0			
		sta Pointer1			
		sta Pointer2			
		lda #>kVectors.charBase			
		sta Pointer1+1			
		lda #$D8			
		sta Pointer2+1			
_yloop			
		ldy #(kTileXCount*2)-1			
_xloop			
		lda #192			
		sta (Pointer1),y					
		lda #0					
		sta (Pointer2),y					
		dey					
		bpl _xloop					
		lda Pointer1				
		clc				
		adc #40				
		sta Pointer1				
		sta Pointer2			
		lda Pointer1+1				
		adc #00				
		sta Pointer1+1				
		eor # (>kVectors.charBase) ^ $d8				
		sta Pointer2+1				
		dex					
		bpl _yloop					
		rts					
							
CopyDestLoLUT .byte <fileChars+(44*8),<fileChars+(44*8)    ,<fileChars+(192*8),<fileChars+(192*8)    ,<fileChars+(12*8) ,<fileChars+(40*8),<fileChars+$400  ,<fileChars+$500
CopyDestHiLUT .byte	>fileChars+(44*8),(>fileChars+(44*8))+1,>fileChars+(192*8),(>fileChars+(192*8))+1,>fileChars+(12*8) ,>fileChars+(40*8),>fileChars+$400  ,>fileChars+$500
CopySrcLoLUT  .byte	<LowerFixedChars ,<LowerFixedChars     ,<UpperFixedChars  ,<UpperFixedChars      ,<EmptyDisolveChars,<AppleChars	  ,<fileFont   		,<fileFont+$100	 
CopySrcHiLUT  .byte >LowerFixedChars ,(>LowerFixedChars)+1 ,>UpperFixedChars  ,(>UpperFixedChars)+1  ,>EmptyDisolveChars,>AppleChars	  ,>fileFont   		,>fileFont+$100 
CopyBytes	  .byte 255          ,size(LowerFixedChars)-255,255          ,size(UpperFixedChars)-255  ,32				,32				  ,255				,255
				
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

EntitySpriteColours		.byte 4,15,10,14,15,5,3,14,10,14,10,14,5
EntitySpriteStartFrame	.byte kSprBase+32,kSprBase+40,kSprBase+48,kSprBase+56,kSprBase+64,kSprBase+72,kSprBase+80,kSprBase+96,kSprBase+97,kSprBase+106,kSprBase+107,kSprBase+88

BackCharsLUT .byte 32-1,64-1,96-1,128-1

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
.byte 1
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
.byte kSprBase+64,kSprBase+68,kSprBase+64,kSprBase+68 ; ghost
.byte kSprBase+72,kSprBase+72,kSprBase+72,kSprBase+72 ; spider
.byte kSprBase+80,kSprBase+80,kSprBase+84,kSprBase+84 ; fish 
.byte kSprBase+92,kSprBase+92,kSprBase+88,kSprBase+88 ; flying thing 
.byte 0,0,0,0 ; bear
.byte 0,0,0,0 ; other bear
.byte 0,0,0,0 ; octopus
.byte 0,0,0,0 ; other octopus
.byte kSprBase+124,kSprBase+124,kSprBase+124,kSprBase+124 ; bubble
FrameCountForEnt
.byte 008,004,004,004,004,002,004,004,002,002,002,002,003,000
CollFrameForEnt
.byte 000,000,000,000,000,000,000,000,004,004,005,005,007,006
;CollisionResultEORForEnt
;.byte 000,000,000,001,000,000,000,000,000,000,000,000,000,000
AnimFrameTimerForEnt
.byte 008,002,008,008,008,008,001,002,004,004,004,004,012,004
SpringDirectionToDeltaLUT
.char -2,-1,-1,-1,01,01,01,02 
SinJumpTable
.char -5, -5, -4, -4, -5, -3
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

TitleScreenData .block
string .byte kStrings.original,kStrings.c64port,kStrings.program,kStrings.art,kStrings.saul
	   .byte kStrings.music,kStrings.saul,kStrings.specialThanks,kStrings.soci,kStrings.martinPiper
	   .byte kStrings.both,kStrings.music,kStrings.sfx,kStrings.none
index  .byte 3*16+1			  ,4*16+2		   ,5*16+5			,6*16+5		,6*16+8
	   .byte 7*16+5		   ,7*16+8			,8*16+5				  ,9*16+8       ,10*16+6
	   .byte 11*16+2,11*16+6,11*16+10,11*16+14
	
spriteY 	.byte 60
spriteX 	.byte 101+15,136+15,171+15,206+15
spriteDef 	.byte kSprites.Q,kSprites.W,kSprites.A,kSprites.K
spriteCol	.byte 7,13,14,10

menuOffsetsStart	.byte 26,18,11,3
menuOffsetsEnd		.byte 33,25,18,10
.bend

kStrings .block 
gameOver = 0
original = 1
c64port = 2
program = 3
art = 4
music = 5
specialThanks = 6
soci = 7
martinPiper = 8
saul = 9
sfx = 10
none = 11
both = 12
.bend

StringTableLUTLo .byte <GAMEOVER,<ORIGINAL,<C64PORT,<PROGRAM,<ART,<MUSIC,<SPECIALTHANKS,<SOCI,<MARTINPIPER,<SAUL,<SFX,<NONE,<BOTH
StringTableLUTHi .byte >GAMEOVER,>ORIGINAL,>C64PORT,>PROGRAM,>ART,>MUSIC,>SPECIALTHANKS,>SOCI,>MARTINPIPER,>SAUL,>SFX,>NONE,>BOTH

GAMEOVER 		.text "GAME OVER"
		 		.byte $FF
ORIGINAL 		.text "ORIGINAL CONCEPT : JAMIE WOODHOUSE"		
				.byte $FF
C64PORT  		.text "PORTED TO THE COMMODORE 64 BY"		
				.byte $FF		
PROGRAM	 		.text "CODE  : OZIPHANTOM"		
				.byte $FF		
ART		 		.text "ART"		
				.byte $FF		
SAUL			.text ": SAUL CROSS"		
				.byte $FF		
MUSIC 	 		.text "MUSIC"		
				.byte $FF		
SFX				.text "SFX"		
				.byte $FF		
NONE			.text "NONE"		
				.byte $FF		
BOTH			.text "BOTH"		
				.byte $FF		
SPECIALTHANKS 	.text "SPECIAL THANKS TO"		
				.byte $FF		
SOCI		  	.text "SOCI"		
				.byte $FF		
MARTINPIPER	  	.text "MARTIN PIPER"		
				.byte $FF			
						
BossLevels 		.byte 4,4+5,4+10,4+15,4+20,4+25

kSFX .block
flower = 0
door =1
collect = 2
bubble = 3
ebubble = 4
powerup = 5
jump = 6
hurt = 7
.bend

SNDTBL mMakeTable SND_FLOWER,SND_DOOR,SND_COLLECT,SND_BUBBLE,SND_EBUBBLE,SND_POWER_UP,SND_JUMP,SND_HURT



BackChars1 .binary "back_chars_1.raw"
BackChars2 .binary "back_chars_2.raw"
BackChars3 .binary "back_chars_3.raw"
BackChars4 .binary "back_chars_4.raw"
BlockChars1 .binary "wall1_chars.raw"
BlockChars2 .binary "wall2_chars.raw"
BlockChars3 .binary "wall3_chars.raw"
BlockChars4 .binary "wall4_chars.raw"
AppleChars .binary "apple_chars.raw"
ExclimationsChars .binary "excelmation_chars.raw"
CherryChars .binary "cherry_chars.raw"
aSphereChars .binary "sphere_chars.raw"
LowerFixedChars .binary "fixed_section_chars.raw"
UpperFixedChars .binary "top_fixed_chars.raw"
EmptyDisolveChars .binary "empty_disolve_chars.raw"
WallColourLUT .byte 3+8,4+8,5+8,7+8
FruitColourLut
.byte 5+8,5+8,5+8,5+8 ; apple
.byte 7+8,7+8,2+8,2+8 ; !
.byte 5+8,5+8,2+8,2+8 ; cherry
.byte 3+8,3+8,4+8,4+8 ; sphere
PackedSprites .binary "sprites_small.bin"

fileCharCols ;@@ENDROM ; @@RAM		
.binary "testattribs.raw"	
fileTiles ;		
.binary "tiledefs.raw",0,32*4
.byte 84,85,86,87
* = fileTiles + (33*4)
.byte 06,05,10,03
fileFont ;
.binary "font.raw"
FILE_END 

*= $C000
VIC_START 
fileScreen ;
;*= $4400
;fileCharCols ;		
;.binary "testattribs.raw"	
;*= $4500
;fileTiles ;		
;.binary "tiledefs.raw",32*4	; needs to be 80 bytes
;* = $4500 + (33*4)
;.byte 06,05,10,03
*= $C800
fileChars ;
;.fill 88+(24*8)
;.binary "testchars.raw",88+(24*8)
*= $E000-(8*64)
fileSprites ;
;.binary "sprites.bin"		
* = $77C0	
LevelTableLo	
.byte <fileTileMap,<Level02,<Level03,<Level04,<Level05,<Level06,<Level07,<Level08,<Level09,<Level10,<Level11,<Level12,<Level13,<Level14,<Level15,<Level16,<Level17,<Level18,<Level19,<Level20,<Level21,<Level22,<Level23,<Level24,<Level25,<Level26,<Level27,<Level28,<Level29,<Level30	
LevelTableHi	
.byte >fileTileMap,>Level02,>Level03,>Level04,>Level05,>Level06,>Level07,>Level08,>Level09,>Level10,>Level11,>Level12,>Level13,>Level14,>Level15,>Level16,>Level17,>Level18,>Level19,>Level20,>Level21,>Level22,>Level23,>Level24,>Level25,>Level26,>Level27,>Level28,>Level29,>Level30	
* = $7800		
VIC_END ; @@ENDRAM	
fileTileMap; 
.binary "levels/01.bin"
Level02 .binary "levels/02.bin"
Level03 .binary "levels/03.bin"
Level04 .binary "levels/04.bin"
Level05 .binary "levels/04boss01.bin"
Level06 .binary "levels/05.bin"
Level07 .binary "levels/06.bin"
Level08 .binary "levels/07.bin"
Level09 .binary "levels/08.bin"
Level10 .binary "levels/08boss02.bin"
Level11 .binary "levels/09.bin"
Level12 .binary "levels/10.bin"
Level13 .binary "levels/11.bin"
Level14 .binary "levels/12.bin"
Level15 .binary "levels/12boss03.bin"
Level16 .binary "levels/13.bin"
Level17 .binary "levels/14.bin"
Level18 .binary "levels/15.bin"
Level19 .binary "levels/16.bin"
Level20 .binary "levels/16boss04.bin"
Level21 .binary "levels/17.bin"
Level22 .binary "levels/18.bin"
Level23 .binary "levels/19.bin"
Level24 .binary "levels/20.bin"
Level25 .binary "levels/20boss05.bin"
Level26 .binary "levels/21.bin"
Level27 .binary "levels/22.bin"
Level28 .binary "levels/23.bin"
Level29 .binary "levels/24.bin"
Level30 .binary "levels/24boss06.bin"

* = $a000
SID .binary "QWAK.sid",126
; SFX
SND_FLOWER		.binary "qwak_flower.snd"
SND_DOOR		.binary "qwak_door.snd"
SND_COLLECT		.binary "qwak_collect.snd"
SND_BUBBLE		.binary "qwak_bubble.snd"
SND_EBUBBLE		.binary "enemy_bubble.snd"
SND_POWER_UP	.binary "qwak_power_up.snd"
SND_JUMP		.binary "qwak_jump.snd"
SND_HURT		.binary "qwak_hurt.snd"
; to pack
; exomizer.exe sfx sys -o qwak_ex.prg qwak.prg
	