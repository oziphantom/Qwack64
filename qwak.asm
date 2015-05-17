.include "qwak_structs.asm"

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

kTileXCount = 16
kTileYCount = 12

kBounds .block
	screenMinX = 24
	screenMinY = 50
	screenMaxX = 24 + ( kTileXCount * 8 )
	screenMaxY = 50 + ( kTileYCount * 8 )
.bend

kTimers .block
	dissBlocksValue = $10
	jumpUpValue = $38 ; 3.5 tile
	jumpUpSpringValue = $48 ; 4.5 tiles
	floatTimer = $50 
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
.bend

kLevelSizeMax = kTileXCount*kTileYCount

kFishLimits .block
	startTwo = 250-21-(8*6) ; 165
	maxY = 255-8-8
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

; kStatusBorderChars		
kSBC .block
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

playerTempCol	= $d2
ZPTemp			= $d3
TempX			= $d4

mplex .block
kMaxSpr = $1f
sort = $02
ybuf = $22		;sprite y position raster buffer
xbuf = $42		;sprite x position raster buffer
mbuf = $62		;sprite x msb raster buffer
cbuf = $82		;sprite color raster buffer
pbuf = $a2		;sprite pointer raster buffer
sptr = $c2		;sprite pointer for raster
cnt  = $c3
mnt  = $c4
lsbtod = $c5
ypos = $c000	;sprite y position frame buffer
xpos = $c020	;sprite x position frame buffer
xmsb = $c040	;sprite x msb frame buffer
sprc = $c060	;sprite color frame buffer
sprp = $c080	;sprite pointer frame buffer
.bend

LevelTileMapPtrLo = $d5
LevelTileMapPtrHi = $d6
LevelKeyListPtrLo = $d7
LevelKeyListPtrHi = $d8

CollCharX	= $d9
CollCharY	= $da
CollTileX	= $db
CollTileY	= $dc
CollTLX		= $dd
CollTLY		= $de

EntityDataPointer	= $e0
CurrentEntity		= $e2
EntIndexVIC			= $e3
EntNum				= $e4
CollisionResult		= $e5

tileMapTemp = $c100 ; .fill 240

variables = $0300
* = $0300

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

.cerror * > $400, "Too many variables"

;.warn "Size of variables = ", $400-*



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
		lda #64
		sta mplex.sprp
		lda #1
		sta $d015
		lda #255
		sta $d01c
		lda #11
		sta $d025
		lda #1
		sta $d026
		lda #7
		sta mplex.sprc
		;lda #%00010000
		;sta $d011
		; clear Mplex 
		ldx #$00	  ;init x with 00
-		txa			  ;transfer x to a to create index vals
		sta mplex.sort,x	;store it in the sort table
		lda #255
		sta mplex.ypos,x	; disbale all sprites
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
-		sta kVectors.charBase + ( 24*40 ),x
		dex
		bpl -
		; main loop
-		lda mplex.lsbtod	
		beq -	
		dec mplex.lsbtod	
;	inc $d020
;	inc $d020
		
		lda PlayerData.dead
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
		jsr checkQwakOnDoor
		jsr checkOnDissTile
		lda mplex.xpos
		clc
		adc checkSpriteToCharData.xDeltaCheck
		sta mplex.xpos
		lda mplex.ypos
		clc
		adc checkSpriteToCharData.yDeltaCheck
		sta mplex.ypos
		jsr updatePlayerAnim
		jsr updateEntities
;	dec $d020
;	dec $d020	
	
		jmp -
		
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
		and PlayerData.hasJumped	
		beq _cb 
		lda #0	
		sta PlayerData.hasJumped	
_cb		lda PlayerData.onGround		; are we on the ground	
		bne _ong
		ldx # kJumpIndexs.normal
_incD	jsr incPlayerYDeltaAndReturn	; no, we are in gravity so fall
		sta checkSpriteToCharData.yDeltaCheck
		rts
_ong	lda #1
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
		sta PlayerData.yDeltaAccum + 1
		rts
		
setPlayerAnimeTo
		cpx PlayerData.currAnim
		beq _dontchange
		stx PlayerData.currAnim
		lda PlayerSprLUT,x
		sta PlayerData.baseSprite
		sta mplex.sprp
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
		sta mplex.sprp
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
		sta $fc
		sta LevelTileMapPtrLo
		lda #>tileMapTemp
		sta $fd
		sta LevelTileMapPtrHi
		lda #<fileTileMap
		sta $fe
		lda #>fileTileMap
		sta $ff
; read level pointers
		ldy #0
		lda ($fe),y
		clc
		adc $fe
		sta LevelKeyListPtrLo
		iny
		lda ($fe),y
		adc $ff
		sta LevelKeyListPtrHi
		iny
		lda ($fe),y
		clc
		adc $fe
		sta EntityDataPointer
		iny
		lda ($fe),y
		adc $ff
		sta EntityDataPointer+1
		clc
		lda $fe
		adc #4
		sta $fe
		lda $ff
		adc #0
		sta $ff
		
		lda #12
		pha
_row	ldy #0
_loop	; read in 16 bytes		
		lda ($fe),y		
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
		sta ($fc),y		
		iny		
		cpy #16		
		bne _loop		
		clc		
		lda $fe		
		adc #16		
		sta $fe		
		lda $ff		
		adc #0		
		sta $ff		
		clc		
		lda $fc		
		adc #16		
		sta $fc		
		lda $fd		
		adc #0		
		sta $fd		
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
		;lda #33
		;sta ($fe),y
		lda # kTiles.diss
		jmp _cont		
_exitPos
		sty LevelData.exitX
		asl LevelData.exitX
		pla
		sta ZPTemp
		pha
		lda #12
		sec
		sbc ZPTemp
		asl a
		sta LevelData.exitY
		lda # kTiles.exit
		jmp _cont

addShadowsToMap				
		lda #<tileMapTemp
		sta $fe
		lda #>tileMapTemp
		sta $ff
		lda #12
		pha
		ldx #0
_row	ldy #0
_loop	; read in 16 bytes		
		lda ($fe),y					
		cmp toolToTileLUT+1				
		beq _checkit				
		cmp toolToTileLUT+2			
		beq _checkit			
_done	iny				
		cpy #16				
		bne _loop				
		clc		
		lda $fe		
		adc #16		
		sta $fe		
		lda $ff		
		adc #0		
		sta $ff		
		inx 
		pla 
		sec 
		sbc #1	
		cmp #0	
		pha 
		bne _row	
		pla 
		rts					
_checkit cpx #11				
		beq _skipBelowCheck				
		tya				
		pha				
		clc				
		adc #16				
		tay				
		lda ($fe),y				
		cmp #4				
		bne _skipBelowCheckpla				
		lda #6				
		sta ($fe),y				
_skipBelowCheckpla						
		pla						
		tay						
_skipBelowCheck				
		cpy #15 ; last column		
		beq _done		
		iny		
		lda ($fe),y		
		cmp #4 ; pure empty		
		bne _checkBottom		
		lda #9		
		sta ($fe),y		
_done2	dey		
		jmp _done		
_checkBottom				
		cmp #6				
		bne _done2				
		lda #10				
		sta ($fe),y				
		jmp _done2				
						
		; back,wall,wall1,wall2,wall3,wall4,spike,flower,fruit,key1,key2,key3,key4,shield,spring,potion,egg,exit,player,diss		
toolToTileLUT .byte 0,7,7,7,7,7,19,26,31,20,20,20,20,27,28,29,30,21,117,6,8,9,10,11,12,13,14,15,16,17,18		
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
.bend

kDoorOpen = 25

kKeyToWallDelta = kTiles.key1 - kTiles.wall1

plotTileMap
		stx $fc
		sty $fd
		lda # <kVectors.charBase
		sta $fe
		lda # >kVectors.charBase
		sta $ff
		lda #00
		sta $f8
		lda #$d8
		sta $f9
		lda #12 ; num rows
		pha
_pltY	ldy #00 ; num cols
		tya
		pha
_pltX	lda ($fc),y ; tile num
		tax
		lda toolToTileLUT,x ; convert map to actual tile
		jsr renderTile		
		clc
		lda $fe
		adc #2
		sta $fe
		lda $ff
		adc #0
		sta $ff
		clc
		lda $f8
		adc #2
		sta $f8
		lda $f9
		adc #0
		sta $f9
		pla
		clc
		adc #1
		pha
		tay
		cpy #20 ; #16
		bne _pltX
		pla
		clc
		lda $fc
		adc #16 ; 16
		sta $fc
		lda $fd
		adc #00
		sta $fd
		pla
		sec
		sbc #1
		beq _exit
		pha
		clc
		lda $fe
		adc #40 ;48
		sta $fe
		lda $ff
		adc #0
		sta $ff
		clc
		lda $f8
		adc #40 ;48
		sta $f8
		lda $f9
		adc #0
		sta $f9
		jmp _pltY
_exit	rts

; a = tile num fa,fb = tile set, fe,ff = screen, f8,f9 = d800
renderTile 
		sta $fa 
		lda #0	
		sta $fb 
		asl $fa 
		rol $fb 
		asl $fa	 ; tile num x 4
		rol $fb 
		; covert to tiles offset	
		clc 
		lda $fa
		adc # <fileTiles
		sta $fa
		lda $fb
		adc # >fileTiles
		sta $fb
		ldy #0
		lda ($fa),y
		tax
		sta ($fe),y
		lda fileCharCols,x
		sta ($f8),y
		ldy #1
		lda ($fa),y
		tax
		sta ($fe),y
		lda fileCharCols,x
		sta ($f8),y
		ldy #2
		lda ($fa),y
		tax
		ldy #40
		sta ($fe),y
		lda fileCharCols,x
		sta ($f8),y
		ldy #3
		lda ($fa),y
		tax
		ldy #41
		sta ($fe),y
		lda fileCharCols,x
		sta ($f8),y
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
	
CSTCCInteral
	ldx mplex.xpos
	ldy mplex.ypos
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
	
checkSpriteToCharCollision
	lda #0
	sta PlayerData.onGround
	;mid
	lda #kBounds.screenMinX-14
	sec 
	sbc checkSpriteToCharData.xDeltaCheck
	sta $fe
	lda #kBounds.screenMinY-14
	sec 
	sbc checkSpriteToCharData.yDeltaCheck
	sta $fd
	lda #2
	sta playerTempCol
	jsr CSTCCInteral
	jsr CSTCCInteral
	; mid below
	lda #kBounds.screenMinX-14
	sec 
	sbc checkSpriteToCharData.xDeltaCheck
	sta $fe
	lda #kBounds.screenMinY-24
	sec 
	sbc checkSpriteToCharData.yDeltaCheck
	sta $fd
	inc playerTempCol
	jsr CSTCCInteral
	lda PlayerData.facingRight
	beq _checkBelowLeft
	lda #kBounds.screenMinX-18
	sec 
	sbc checkSpriteToCharData.xDeltaCheck
	sta $fe
	jmp _checkExtraBelow
_checkBelowLeft 
	lda #kBounds.screenMinX-10
	sec 
	sbc checkSpriteToCharData.xDeltaCheck
	sta $fe
_checkExtraBelow
	lda #kBounds.screenMinY-24
	sec 
	sbc checkSpriteToCharData.yDeltaCheck
	sta $fd
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
;	sta PlayerData.hasJumped 
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

checkUpPoints
	lda #kBounds.screenMinX-11
	sec 
	sbc checkSpriteToCharData.xDeltaCheck
	sta $fe
	lda #kBounds.screenMinY-06
	sec 
	sbc checkSpriteToCharData.yDeltaCheck
	sta $fd
	lda #0
	sta playerTempCol
	jsr CSTCCInteral
	lda #kBounds.screenMinX-17
	sec 
	sbc checkSpriteToCharData.xDeltaCheck
	sta $fe
	lda #kBounds.screenMinY-06
	sec 
	sbc checkSpriteToCharData.yDeltaCheck
	sta $fd
	inc playerTempCol
	jmp CSTCCInteral
	
checkRightPoints
	lda #kBounds.screenMinX-18
	sec 
	sbc checkSpriteToCharData.xDeltaCheck
	sta $fe
	lda #kBounds.screenMinY-07
	sec 
	sbc checkSpriteToCharData.yDeltaCheck
	sta $fd
	lda #0
	sta playerTempCol
	jsr CSTCCInteral
	lda #kBounds.screenMinX-18
	sec 
	sbc checkSpriteToCharData.xDeltaCheck
	sta $fe
	lda #kBounds.screenMinY-20
	sec 
	sbc checkSpriteToCharData.yDeltaCheck
	sta $fd
	inc playerTempCol
	jmp CSTCCInteral
	
checkDownPoints
	lda #kBounds.screenMinX-11
	sec 
	sbc checkSpriteToCharData.xDeltaCheck
	sta $fe
	lda #kBounds.screenMinY-21
	sec 
	sbc checkSpriteToCharData.yDeltaCheck
	sta $fd
	lda #0
	sta playerTempCol
	jsr CSTCCInteral
	lda #kBounds.screenMinX-17
	sec 
	sbc checkSpriteToCharData.xDeltaCheck
	sta $fe
	lda #kBounds.screenMinY-21
	sec 
	sbc checkSpriteToCharData.yDeltaCheck
	sta $fd
	inc playerTempCol
	jmp CSTCCInteral
	
checkLeftPoints
	lda #kBounds.screenMinX-10
	sec 
	sbc checkSpriteToCharData.xDeltaCheck
	sta $fe
	lda #kBounds.screenMinY-07
	sec 
	sbc checkSpriteToCharData.yDeltaCheck
	sta $fd
	lda #0
	sta playerTempCol
	jsr CSTCCInteral
	lda #kBounds.screenMinX-10
	sec 
	sbc checkSpriteToCharData.xDeltaCheck
	sta $fe
	lda #kBounds.screenMinY-20
	sec 
	sbc checkSpriteToCharData.yDeltaCheck
	sta $fd
	inc playerTempCol
	jmp CSTCCInteral
	
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
	lda CollTLX
	pha
	lda CollTLY
	pha
	lda LevelData.exitX
	sta CollTLX
	lda LevelData.exitY
	sta CollTLY
	lda #kDoorOpen
	jsr pltSingleTileNoLookup
	lda #1
	sta GameData.exitOpen
	pla 
	sta CollTLY
	pla 
	sta CollTLX 
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
	
awardLife
	inc GameData.lives
	jmp pltLives

CharObjectMinLUT .byte kTiles.exit,kTiles.fruit,kTiles.key1,kTiles.key2,kTiles.key3,kTiles.key4,kTiles.flower,kTiles.spike,kTiles.spring,kTiles.potion,kTiles.shield
CharObjectMaxLUT ; .byte 168,77,49,57,44
CharObjectFuncLo .byte <exitFunc,<fruitFunc,<keyFunc,<keyFunc,<keyFunc,<keyFunc,<flowerFunc,<spikeFunc,<springFunc,<potionFunc,<shildFunction
CharObjectFuncHi .byte >exitFunc,>fruitFunc,>keyFunc,>keyFunc,>keyFunc,>keyFunc,>flowerFunc,>spikeFunc,>springFunc,>potionFunc,>shildFunction

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
	
removeAllTilesOf
	sta ZPTemp
	lda CollTileX
	pha
	lda CollTLX
	pha
	lda CollTileY
	pha
	lda CollTLY
	pha
	ldx #0
	stx CollTileX
	stx CollTLX
	stx CollTileY
	stx CollTLY
	stx TempX
_loop
	ldx TempX
	lda tileMapTemp,x
	cmp ZPTemp
	bne _skip
	jsr clearTile
_skip
	inc CollTileX
	inc CollTLX
	inc CollTLX
	lda CollTileX
	cmp # kTileXCount
	bcc _next
	lda #0
	sta CollTileX
	sta CollTLX
	inc CollTileY
	inc CollTLY
	inc CollTLY
_next	
	inc TempX
	ldx TempX
	inx
	cpx # kLevelSizeMax
	bne _loop
	pla
	sta CollTLY
	pla
	sta CollTileY
	pla
	sta CollTLX
	pla 
	sta CollTileX
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
	bcs _notSafe
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
	clc
	adc #20
	sta mplex.xpos
	lda LevelData.playerY
	asl a
	asl a
	asl a
	asl a
	clc
	adc #44
	sta mplex.ypos
	rts
	
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
	adc #50-5 
	sta mplex.ypos+1,x
	sta ZPTemp
	lda (EntityDataPointer),y
	and #$f0
	clc
	adc #24 - 4
	sta mplex.xpos+1,x
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
	ldx #6
_l	lda EntityData.active,x
	bne _active
_c	dex
	bpl _l
	rts
_active
	lda EntityData.type,x
	tay
	lda EntitySpriteStartFrame,y
	sta mplex.sprp+1,x
	lda EntitySpriteColours,y
	sta mplex.sprc+1,x
	jmp _c
	
updateEntities
	ldx #6
_l	lda EntityData.active,x
	bne _active
_c	dex
	bpl _l
	rts
_active
	stx CurrentEntity
	txa 
	asl a	
	tay		
	sty EntIndexVIC 
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
	lda mplex.xpos+1,x
	sec
	sbc mplex.xpos
	clc
	adc #10
	cmp #20
	bcs _spiderNoMove
	inc EntityData.entState,x
_spiderNoMove	
	jmp _next	

_notBat 
	ldx CurrentEntity	
	ldy EntIndexVIC
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
	inc mPlex.ypos+1,x 
	jmp _next		
_dnotsafe
	jsr SetNextEntDir	
	jmp _next	
		
_right
	lda CollisionResult
	bne _rnotsafe
;	ldx EntIndexVIC ; what is in y
	inc mPlex.xpos+1,x 
	jmp _next		
_rnotsafe
	jsr SetNextEntDir
	jmp _next
	
_up
	lda CollisionResult
	bne _unotsafe
	ldx CurrentEntity 
	dec mPlex.ypos+1,x 
	;ldy CurrentEntity
	lda EntityData.type,x
	cmp # kEntity.spider
	beq _checkSpider
	jmp _next		
_unotsafe
	jsr SetNextEntDir
	jmp _next

_checkSpider
	lda mplex.ypos+1,x
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
	dec mPlex.xpos+1,x 
	jmp _next		
_lnotsafe
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
	lda #64+80
	sta mplex.sprp+1,x ; will need to change to animation type
	lda mplex.ypos+1,x
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
	lda #64+84
	sta mplex.sprp+1,x
	lda EntityData.entState,x
	cmp #kSinJumpMax
	bcc _checkMaxY
	dec EntityData.entState,x
_checkMaxY	
	lda mplex.ypos+1,x
	cmp # kFishLimits.maxY
	bcc _movefish
	lda # kDirections.up
	sta EntityData.direction,x
_moveFish	
	lda EntityData.entState,x
	tay
	lda mplex.ypos+1,x
	clc
	adc SinJumpTable,y
	sta mplex.ypos+1,x
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
	lda DirectionYLUT + kDirections.down
	sta $fd
	lda DirectionXLUT + kDirections.down
	sta $fe
	lda #0
	sta CollisionResult
	jsr CheckEntAgainstTile
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
	ldy EntIndexVIC
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
	lda mplex.ypos+1,y
	clc
	adc SinJumpTable,x
	sta mplex.ypos+1,y
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
	lda mplex.xpos+1,x
	clc
	adc ZPTemp
	sta mplex.xpos+1,x
	jmp _next
_moveLeft
	ldx CurrentEntity
	lda mplex.xpos+1,x
	sec
	sbc ZPTemp
	sta mplex.xpos+1,x
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
	lda mplex.xpos+1,y
	clc
	adc CircleJumpTableStart,x
	sta mplex.xpos+1,y
	lda mplex.ypos+1,y
	clc
	adc CircleJumpTableStart + ( CircleJumpTableCount / 4) + 1,x
	sta mplex.ypos+1,y
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
	lda mPlex.ypos+1,x
	tay
	lda mPlex.xpos+1,x
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
	
SetNextEntDir
	ldx CurrentEntity
	lda EntityData.type,x
	asl a
	asl a
	asl a
	ora EntityData.direction,x
	tay
	lda NextDirectionLUT,y
	sta EntityData.direction,x
	rts
	
; multiplexor
setirq
	  sei			 ;set interrupt disable
	  lda #$1b
	  sta $d011		 ;raster irq to 1st half of screen.
	  lda #$fb
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
		lda $dc0d
	  lda $dd0d
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
	  cli			 ;clear interrupt disable
	  rts			 ;return from subroutine

irq0
	  pha			 ;use stack instead of zp to prevent bugs.
	  txa
	  pha
	  tya
	  pha
	  inc $d019		 ;acknowledge irq
;	inc $d020
	  ldx #$03		 ;wait a few cycles
l1	  dex
	  bpl l1
	  inx
	  stx $d015				;sprites off = more raster time in top/bottom border

slop  ldy mplex.sort+1,x	;main index sort algo
slep  lda mplex.ypos,y
	  ldy mplex.sort,x		;this sorter uses the previous frame as a prediction buffer.
	  cmp mplex.ypos,y		;as y position does not change much from frame to frame.
	  bcc swap				;otherwise, it is a simple swap sort.
	  inx					;our linked list (sort) is sorted in decending order, according
	  cpx #mplex.kMaxSpr-1	;to sprite y positions.
	  bne slop
	  beq end
swap 
	  lda mplex.sort+1,x
	  sta mplex.sort,x
	  sty mplex.sort+1,x
	  tay
	  dex
	  bpl slep
	  inx
	  beq slop
end

.for spr = 0, spr < mPlex.kMaxSpr, spr = spr + 1
	  ldy mplex.sort+spr		;re arrange frame buffers, into the raster buffers.
	  lda mplex.ypos,y			;this is unrolled for speed.
	  sta mplex.ybuf+spr		;this allows us to use only 1 index pointer for our sprite plotter.
	  lda mplex.xpos,y			;it is double buffered, to allow runtime code to calculate the sprite
	  sta mplex.xbuf+spr		;positions.
	  lda mplex.xmsb,y
	  sta mplex.mbuf+spr
	  lda mplex.sprc,y
	  sta mplex.cbuf+spr
	  lda mplex.sprp,y
	  sta mplex.pbuf+spr
.next

		ldx #$00	 ;find # of used sprites (you can remove sprites by
		stx mplex.sptr	 ;placing #$ff into the ypos buffer for the corresponding
maxc	lda mplex.ybuf,x   ;sprite. It will not be displayed by the raster routine.
		cmp #$ff
		beq maxs
		inx
		cpx mplex.kMaxSpr
		bne maxc
maxs	stx mplex.cnt		 ;max sprites this frame count.
		cpx #$07	 ;check if were doing more than 8
		bcc maxm	 ;if not, we want the plotter to stop after 1 irq.
		ldx #$07	 
maxm	stx mplex.mnt

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

		inc mplex.lsbtod	 ;buffers are swapped, so we can do the next frame now.

		lda #<irq1	 ;irq chain for raster code. prolly want a routine before
		sta $fffe	 ;this one, to turn the sprites back on ;)
		lda #>irq1	 ;i.e. lda #$ff sta $d015
		sta $ffff
		lda #$28
		sta $d012
;	dec $d020
		jmp eirq
		
irq1
		pha			;save registers
		txa
		pha
		tya
		pha
		inc $d019		;acknowledge irq
		lda mplex.cnt
		bne +
		jmp done			; don't have any sprites to render exit
+		ldx mplex.sptr		;get current sprite index
hlop1	lda mplex.ybuf,x	;get sprite y position
		sta $d001		;store sprite y postion.
		lda mplex.xbuf,x	;get sprite x position.
		sta $d000		;sta sprite x position.
		lda mplex.mbuf,x	;get sprite x position msb
		bne no1		;set msb register
		lda $d010
		ora #%00000001
		bne yes1
no1		lda $d010
		and #%11111110
yes1	sta $d010
		lda mplex.pbuf,x	;get sprite image pointer
		sta kVectors.spr0ID		;store it screen.
		lda mplex.cbuf,x	;get sprite color
		sta $d027		;store sprite color
		inx			;next sprite index
		cpx mplex.mnt		;lets go to next plot, if < then 8 yet.
		bcc hlop2
		cpx mplex.cnt		;no more sprites?
		bne ok1
		jmp done		;no more sprites.

ok1		stx mplex.sptr		;save sprite index
		lda $d003		;get last position of next sprite
		clc
		adc #$15		;add 21 lines
		cmp $d012		;we there yet?
		bcc hlop2		;yeah, so plot next sprite
		adc #$02		;no, so calculate next irq position (+3)
		sta $d012		;set it
		lda #<irq2	;irq for next sprite.
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
		ldx mplex.sptr
hlop2	lda mplex.ybuf,x
		sta $d003
		lda mplex.xbuf,x
		sta $d002
		lda mplex.mbuf,x
		bne no2
		lda $d010
		ora #%00000010
		bne yes2
no2		lda $d010
		and #%11111101
yes2	sta $d010
		lda mplex.pbuf,x
		sta kVectors.spr1ID		;store it screen.
		lda mplex.cbuf,x
		sta $d028
		inx
		cpx mplex.mnt
		bcc hlop3
		cpx mplex.cnt
		bne ok2
		jmp done

ok2		stx mplex.sptr
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
		ldx mplex.sptr
hlop3	lda mplex.ybuf,x
		sta $d005
		lda mplex.xbuf,x
		sta $d004
		lda mplex.mbuf,x
		bne no3
		lda $d010
		ora #%00000100
		bne yes3
no3		lda $d010
		and #%11111011
yes3	sta $d010
		lda mplex.pbuf,x
		sta kVectors.spr2ID		;store it screen.
		lda mplex.cbuf,x
		sta $d029
		inx
		cpx mplex.mnt
		bcc hlop4
		cpx mplex.cnt
		bne ok3
		jmp done

ok3		stx mplex.sptr
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
		ldx mplex.sptr
hlop4	lda mplex.ybuf,x
		sta $d007
		lda mplex.xbuf,x
		sta $d006
		lda mplex.mbuf,x
		bne no4
		lda $d010
		ora #%00001000
		bne yes4
no4		lda $d010
		and #%11110111
yes4	sta $d010
		lda mplex.pbuf,x
		sta kVectors.spr3ID		;store it screen.
		lda mplex.cbuf,x
		sta $d02a
		inx
		cpx mplex.mnt
		bcc hlop5
		cpx mplex.cnt
		bne ok4
		jmp done

ok4		stx mplex.sptr
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
		ldx mplex.sptr
hlop5	lda mplex.ybuf,x
		sta $d009
		lda mplex.xbuf,x
		sta $d008
		lda mplex.mbuf,x
		bne no5
		lda $d010
		ora #%00010000
		bne yes5
no5		lda $d010
		and #%11101111
yes5	sta $d010
		lda mplex.pbuf,x
		sta kVectors.spr4ID		;store it screen.
		lda mplex.cbuf,x
		sta $d02b
		inx
		cpx mplex.mnt
		bcc hlop6
		cpx mplex.cnt
		bne ok5
		jmp done

ok5		stx mplex.sptr
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
		ldx mplex.sptr
hlop6	lda mplex.ybuf,x
		sta $d00b
		lda mplex.xbuf,x
		sta $d00a
		lda mplex.mbuf,x
		bne no6
		lda $d010
		ora #%00100000
		bne yes6
no6		lda $d010
		and #%11011111
yes6	sta $d010
		lda mplex.pbuf,x
		sta kVectors.spr5ID		;store it screen.
		lda mplex.cbuf,x
		sta $d02c
		inx
		cpx mplex.mnt
		bcc hlop7
		cpx mplex.cnt
		bne ok6
		jmp done

ok6		stx mplex.sptr
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
		ldx mplex.sptr
hlop7	lda mplex.ybuf,x
		sta $d00d
		lda mplex.xbuf,x
		sta $d00c
		lda mplex.mbuf,x
		bne no7
		lda $d010
		ora #%01000000
		bne yes7
no7		lda $d010
		and #%10111111
yes7	sta $d010
		lda mplex.pbuf,x
		sta kVectors.spr6ID		;store it screen.
		lda mplex.cbuf,x
		sta $d02d
		inx
		cpx mplex.mnt
		bcc hlop8
		cpx mplex.cnt
		bne ok7
		jmp done

ok7		stx mplex.sptr
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
		ldx mplex.sptr
hlop8 	lda mplex.ybuf,x
		sta $d00f
		lda mplex.xbuf,x
		sta $d00e
		lda mplex.mbuf,x
		bne no8
		lda $d010
		ora #%10000000
		bne yes8
no8	  	lda $d010
		and #%01111111
yes8	sta $d010
		lda mplex.pbuf,x
		sta kVectors.spr7ID		;store it screen.
		lda mplex.cbuf,x
		sta $d02e
		inx
		cpx mplex.mnt
		bcc hlop9
		cpx mplex.cnt
		bne ok8
		jmp done

ok8		stx mplex.sptr
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

done 	lda #<irq0
		sta $fffe
		lda #>irq0
		sta $ffff
		lda #$fb
		sta $d012
eirq	pla
		tay
		pla
		tax
		pla
justRTI	rti
	
EntitySpriteColours		.byte 4,15,10,14,15,5,3,14
EntitySpriteStartFrame	.byte 64+32,64+40,64+48,64+56,64+64,64+72,64+80,64+88

IndexToORLUT	.byte 1,2,4,8,16,32,64,128
IndexToANDLUT	.byte 254,253,251,247,239,223,191,127
; 0 = right, 1 = up, 2 = left, 3 = down
DirectionXLUT	.byte 6,	24-12,	25,		24-12
DirectionYLUT	.byte 50-13,48,		50-13,	50-22 ; raw sprite Y offsets
NextDirectionLUT
.byte 3,3,1,1,0,0,0,0 ; heli
.byte 0,0,0,0,0,0,0,0 ; spring
.byte 2,2,0,0,0,0,0,0 ; worm
.byte 2,2,0,0,0,0,0,0 ; bat
.byte 3,0,1,2,0,0,0,0 ; ghost
.byte 3,3,1,1,0,0,0,0 ; spider
.byte 0,0,0,0,0,0,0,0 ; fish - not used
.byte 0,0,0,0,0,0,0,0 ; flying thing - not used

SinJumpTable
.char -8, -6, -5, -4, -5, -3
.char -4, -3, -2, -3, -1, -2, -1, 0, -1, -1, 0 
kSinJumpFall = * - SinJumpTable 
.char  1,  2,  1,  3,  2,  3,  4  
.char  3,  5,  4,  5,  6,  5, 6,  6,  7, 8, 8 
kSinJumpMax = * - SinJumpTable - 1

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

*= $4000
fileScreen ;
*= $4400
fileCharCols ;		
.binary "testattribs.raw"	
*= $4500
fileTiles ;		
.binary "tiledefs.raw"	; needs to be 80 bytes
*= $4800
fileChars ;
.binary "testchars.raw"
*= $5000
fileSprites ;
.binary "sprites.bin"		
* = $7000		
fileTileMap; 
.binary "testmap.raw"
fileEntityTest
.byte $3,$d9,$40,$D2,$20,$22,$03
	