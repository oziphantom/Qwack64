kVectors .block
	charBase = $0400
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
	minY = 250-21-(8*10)
	startTwo = 250-21-(8*9) ; 165
	startThree = 250-21-(8*7) ; 189
	maxY = 251
.bend

kDirections .block
	right = 0
	up = 1
	left = 2
	down = 3
.bend

sGameData .struct 
lives .byte 3
flowers .byte 0
score .byte 0,0,0,0,0,0
high .byte 0,0,0,0,0,0
currLevel .byte 0
exitOpen .byte 0
.ends

sLevelData .struct
numKeys .byte 0
totalKeys .byte 0
playerX .byte 0
playerY .byte 0
exitX .byte 0
exitY .byte 0
.ends

sTimerTickDowns .struct
dissBlocks .byte 0
playerJump .byte 0
.ends

sPlayerData .struct
dead .byte 0
hasShield .byte 0
canFloat .byte 0
hasSpring .byte 0
onGround .byte 0
hasJumped .byte 0
isFalling .byte 0
floatTimer .byte 0
facingRight .byte 0
.ends

sEntityData .struct
type 		.byte 0,0,0,0,0,0,0
direction 	.byte 0,0,0,0,0,0,0
active		.byte 0,0,0,0,0,0,0
movTimer	.byte 0,0,0,0,0,0,0
animTimer	.byte 0,0,0,0,0,0,0
originalY	.byte 0,0,0,0,0,0,0
entState	.byte 0,0,0,0,0,0,0
.ends

playerTempCol 	= $9
ZPTemp			= $10
TempX			= $11

LevelTileMapPtrLo = $30
LevelTileMapPtrHi = $31
LevelKeyListPtrLo = $32
LevelKeyListPtrHi = $33

CollCharX 	= $40
CollCharY 	= $41
CollTileX 	= $42
CollTileY 	= $43
CollTLX		= $44
CollTLY		= $45

EntityDataPointer 	= $50
CurrentEntity 		= $52



*= $0801 ; 00 0C 08 0A 00 9E 20 32 30 36 34 00 00
	.word (+), 2005 ;pointer, line number
	.null $9e, ^start;will be sys 4096
+	.word 0	 ;basic line end
	
*= $0810
start
		lda #%00011110
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
		lda #128
		sta kVectors.spr0ID
		lda #1
		sta $d015
		lda #255
		sta $d01c
		lda #100
		sta $d000
		sta $d001
		lda #11
		sta $d025
		lda #1
		sta $d026
		lda #7
		sta $d027
		;lda #%00010000
		;sta $d011
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
		ldx #39
		lda #1
-		sta kVectors.charBase + ( 24*40 ),x
		dex
		bpl -
		
-		lda $d012	
		bne -	
		lda $d011	
		cmp #80	
		beq -	
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
		; gravity
		lda PlayerData.hasJumped
		bne _checkNoInput
		lda checkSpriteToCharData.yDeltaCheck
		bmi _nograv
_grav	lda #1
		sta checkSpriteToCharData.yDeltaCheck
		jmp _nograv
_checkNoInput		
		lda joyUp
		ora joyDown
		beq _grav
_nograv	jsr checkSpriteToCharCollision
		jsr checkQwakOnDoor
		jsr checkOnDissTile
		lda $d000
		clc
		adc checkSpriteToCharData.xDeltaCheck
		sta $d000
		lda $d001
		clc
		adc checkSpriteToCharData.yDeltaCheck
		sta $d001
		jsr updateEntities
		jmp -
		
joyToPlayerDelta
		jsr scanJoystick
		lda #0
		sta checkSpriteToCharData.xDeltaCheck
		sta checkSpriteToCharData.yDeltaCheck
		lda joyLeft
		beq _cr
		lda #0
		jsr changePlayerDir
		lda #$ff
		sta checkSpriteToCharData.xDeltaCheck
		jmp _cu
_cr		lda joyRight
		beq _cu
		lda #1		
		sta checkSpriteToCharData.xDeltaCheck
		jsr changePlayerDir		
_cu		lda joyUp
		beq _cb
		lda PlayerData.hasJumped 
		beq _startJump
		lda PlayerData.isFalling
		bne _jumpFall
		lda TickDowns.playerJump
		bne _upe ; still able to jump
		lda #1
		sta PlayerData.isFalling
_upe	lda #$ff
		sta checkSpriteToCharData.yDeltaCheck
		rts		
_startJump
		lda PlayerData.onGround
		beq _fall
		lda #1
		sta PlayerData.hasJumped 
		lda #0
		sta PlayerData.isFalling
		sta PlayerData.onGround
		ldx PlayerData.hasSpring
		lda PlayerJumpLUT,x
		sta TickDowns.playerJump
		lda # kTimers.floatTimer
		sta PlayerData.floatTimer
		jmp _upe
_jumpFall	
		lda PlayerData.canFloat
		beq _fall
		lda PlayerData.floatTimer	
		beq _fall	
		dec PlayerData.floatTimer	
		lda #0	
		sta checkSpriteToCharData.yDeltaCheck	
		rts	
			
_cb		lda joyDown
		beq _exit
_fall	
		lda #01
		sta checkSpriteToCharData.yDeltaCheck
_exit	rts

changePlayerDir
		sta PlayerData.facingRight
		tax
		lda PlayerSprLUT,x
		sta kVectors.spr0ID
		rts
		
PlayerJumpLUT .byte kTimers.jumpUpValue, kTimers.jumpUpSpringValue
PlayerSprLUT  .byte $84,$80

joyLeft  .byte 0
joyRight .byte 0
joyUp 	 .byte 0
joyDown  .byte 0
joyFire	 .byte 0

GameData .dstruct sGameData
LevelData .dstruct sLevelData
PlayerData .dstruct sPlayerData
TICK_DOWN_START = *
TickDowns .dstruct sTimerTickDowns
TICK_DOWN_END = *

levelNum .byte 0

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
		sta $03
		pha
		lda #12
		sec
		sbc $03
		sta LevelData.playerY
		lda # kTiles.back
		jmp _cont
_key	inc LevelData.numKeys
		inc LevelData.totalKeys
		jmp _cont
_dissBlock
		lda #33
		sta ($fe),y
		lda # kTiles.diss
		jmp _cont		
_exitPos
		sty LevelData.exitX
		asl LevelData.exitX
		pla
		sta $03
		pha
		lda #12
		sec
		sbc $03
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
		cpy #15	; last column		
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
toolToTileLUT .byte 4,32,32,32,32,32,80,87,93,81,81,81,81,88,89,90,91,82,117,33,34,35,36,37,38,39,40,41,42,43		
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
	diss = 19
	dissNoColide = 29
.bend

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
		sta	$fa	
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
_joyEnd	rts
		
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
		
sCSTCCParams .struct 	
sprNum .byte 0
sprWindowX .byte 3 ; pixel
sprWindowW .byte 2 ; chars
sprWindowY .byte 5 ; pixel
sprWindowH .byte 2 ; chars
xDeltaCheck .byte 0 ; pixels
yDeltaCheck .byte 0 ; pixels
.ends

checkSpriteToCharData .dstruct sCSTCCParams	
playerTile1 		.byte 0
playerTile2 		.byte 0
playerMidTile 		.byte 0
playerMidBelowTile 	.byte 0
playerMidBelowOtherTile .byte 0

playerTile1X 		.byte 0
playerTile2X 		.byte 0
playerMidTileX 		.byte 0
playerMidBelowTileX .byte 0
playerMidBelowOtherTileX .byte 0

playerTile1Y 		.byte 0
playerTile2Y 		.byte 0
playerMidTileY 		.byte 0
playerMidBelowTileY	.byte 0
playerMidBelowOtherTileY .byte 0
	
CSTCCInteral
	ldx $d000
	ldy $d001
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
	lda #0
	sta PlayerData.hasJumped 
	sta PlayerData.isFalling
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
	lda levelNum
	clc 
	adc #1
	and #3
	sta levelNum
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
	lda #86
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
	clc
	adc #$d4
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
_ok	dey
	dex	
	bpl _scLoop	
	jmp pltScore	 
	
kScoreIndex .block 	
	fruit = 0	
	flower = 1	
	key = 2	
.bend	
	
FruitScore 	.byte 0,0,0,1,0,0,15,15
FlowerScore .byte 0,0,0,5,0,0,15,15
KeyScore 	.byte 0,0,0,2,5,0,15,15
	
convertXYToScreen
	stx _off+1
	lda screenRowLUTLO,y
	clc
_off adc #00 
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
		
kStatusBorderChars .block
	M	= 191
	TL 	= 203
	T 	= 204
	TR 	= 206
	L	= 205
	R	= 207
	BL  = 229
	B	= 230
	BR	= 231
	Col = 15
	QWAKT = 208
	QWAKB = 214
	Score = 220
	High = 226
.bend
	
plotStatusArea
; fill area
	lda #<kVectors.charBase + 32 
	sta $fb
	lda #>kVectors.charBase + 32 
	sta $fc
	lda #kStatusBorderChars.M
	jsr _fillarea
	lda #<$d800 + 32 
	sta $fb
	lda #>$d800 + 32 
	sta $fc
	lda #kStatusBorderChars.Col
	jsr _fillarea
; plot side lines
	lda #<kVectors.charBase + 32 + 40
	sta $fb
	lda #>kVectors.charBase + 32 + 40
	sta $fc
	lda #<kVectors.charBase + 39 + 40
	sta $fd
	lda #>kVectors.charBase + 39 + 40
	sta $fe
	ldx #22
	ldy #0
_lop2
	lda # kStatusBorderChars.L
	sta ($fb),y
	lda # kStatusBorderChars.R
	sta ($fd),y
	clc
	lda $fb
	adc #40
	sta $fb
	lda $fc
	adc #00
	sta $fc
	clc
	lda $fd
	adc #40
	sta $fd
	lda $fe
	adc #00
	sta $fe
	dex
	bpl _lop2 
; plot corners
	lda # kStatusBorderChars.TL
	sta kVectors.charBase + 32
	sta kVectors.charBase + 32 + (6*40)
	lda # kStatusBorderChars.TR
	sta kVectors.charBase + 39
	sta kVectors.charBase + 39 + (6*40)
	lda # kStatusBorderChars.BL
	sta kVectors.charBase + 32 + (5*40)
	sta kVectors.charBase + 32 + (23*40)
	lda # kStatusBorderChars.BR
	sta kVectors.charBase + 39 + (5*40)
	sta kVectors.charBase + 39 + (23*40)
; plot tops and bottoms
	ldx #6
_lop
	lda #kStatusBorderChars.T
	sta kVectors.charBase + 32,x
	sta kVectors.charBase + 32 + (6*40),x
	lda #kStatusBorderChars.B
	sta kVectors.charBase + 32 + (5*40),x
	sta kVectors.charBase + 32 + (23*40),x
	dex
	bne _lop
; plot QWAK	
	lda #<kVectors.charBase + 33 + 80	
	sta $fb	
	lda #>kVectors.charBase + 33 + 80	
	sta $fc	
	lda #<$d800 + 33 + 80	
	sta $fd	
	lda #>$d800 + 33 + 80	
	sta $fe		
	ldy #5
	ldx #1
	lda #kStatusBorderChars.QWAKT + 5	
	jsr _plotText
	lda #<kVectors.charBase + 33 + 120	
	sta $fb	
	;lda #>kVectors.charBase + 33 + 120	
	;sta $fc	
	lda #<$d800 + 33 + 120	
	sta $fd	
	;lda #>$d800 + 33 + 80	
	;sta $fe	
	ldy #5
	ldx #3
	lda #kStatusBorderChars.QWAKB + 5	
	jsr _plotText
; plot score
	lda #<kVectors.charBase + 33 + (40*8)	
	sta $fb	
	lda #>kVectors.charBase + 33 + (40*8)	
	sta $fc	
	lda #<$d800 + 33 + (40*8)	
	sta $fd	
	lda #>$d800 + 33 + (40*8)	
	sta $fe		
	ldy #5
	ldx #3
	lda #kStatusBorderChars.Score + 5	
	jsr _plotText
; plot high	
	lda #kStatusBorderChars.High
	sta kVectors.charBase + 34 + (40*12)
	sta kVectors.charBase + 37 + (40*12)
	lda #kStatusBorderChars.High+1
	sta kVectors.charBase + 35 + (40*12)
	lda #kStatusBorderChars.High+2
	sta kVectors.charBase + 36 + (40*12)
	lda #3
	sta $d800 + 34 + (40*12)
	sta $d800 + 35 + (40*12)
	sta $d800 + 36 + (40*12)
	sta $d800 + 37 + (40*12)
; a = tile num fa,fb = tile set, fe,ff = screen, f8,f9 = d800
	lda #<fileTiles
	sta $fa
	lda #>fileTiles
	sta $fb
	lda #< kVectors.charBase + 34 + (40*16)
	sta $fe
	lda #> kVectors.charBase + 34 + (40*16)
	sta $ff
	lda #< $d800 + 34 + (40*16)
	sta $f8
	lda #> $d800 + 34 + (40*16)
	sta $f9
	lda #117
	jsr renderTile 
	lda #< kVectors.charBase + 34 + (40*20)
	sta $fe
	lda #> kVectors.charBase + 34 + (40*20)
	sta $ff
	lda #< $d800 + 34 + (40*20)
	sta $f8
	lda #> $d800 + 34 + (40*20)
	sta $f9
	lda #118
	jsr renderTile 
	lda #190 ; x
	sta kVectors.charBase + 36 + (40*17)
	sta kVectors.charBase + 36 + (40*21)
	lda #0
	sta $d800 + 36 + (40*17)
	sta $d800 + 36 + (40*21)
	jsr pltScore
	jsr pltHighScore
	jsr pltLives
	jsr pltFlowers
_exit	
	rts
	
_fillarea
	ldx #23
_row
	ldy #7
_rowloop
	sta ($fb),y
	dey
	bpl _rowloop
	dex
	bmi _exit
	pha
	clc
	lda $fb
	adc #40
	sta $fb
	lda $fc
	adc #00
	sta $fc
	pla
	jmp _row
_plotText
_qt	sta ($fb),y
	pha
	txa
	sta ($fd),y
	pla
	sec
	sbc #1
	dey
	bpl _qt
	rts
	
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
	sta $d000
	lda LevelData.playerY
	asl a
	asl a
	asl a
	asl a
	clc
	adc #44
	sta $d001
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
	
screenRowLUTLO		
.for ue = kVectors.charBase, ue < kVectors.charBase + $400, ue = ue + 40
.byte <ue
.next
screenRowLUTHi	
.for ue = kVectors.charBase, ue < kVectors.charBase + $400, ue = ue + 40
.byte >ue
.next

levelMapsLo
.byte <fileTileMap+20,<fileTileMap+260+20,<fileTileMap+(260*2)+20,<fileTileMap+(260*3)+20
levelMapsHi
.byte >fileTileMap+20,>fileTileMap+260+20,>fileTileMap+(260*2)+20,>fileTileMap+(260*3)+20

tileMapTemp
.fill 240

EntityData .dstruct sEntityData

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
	iny
	ldx #0
_l	lda (EntityDataPointer),y
	and #$0f
	asl a
	asl a
	asl a
	asl a
	clc
	adc #50-5 
	sta $d003,x
	sta $6
	lda (EntityDataPointer),y
	and #$f0
	clc
	adc #24 - 4
	sta $d002,x
	iny
	txa
	lsr a
	tax
	lda (EntityDataPointer),y
	lsr a
	lsr a
	lsr a
	lsr a
	sta EntityData.type,x
	lda $6
	sta EntityData.originalY,x
	lda #0
	sta EntityData.entState,x
	lda (EntityDataPointer),y
	and #3
	sta EntityData.direction,x	
	lda #1	
	sta EntityData.active,x	
	iny
	txa
	asl a
	tax
	inx
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
	sta kVectors.spr1ID,x
	lda EntitySpriteColours,y
	sta $d028,x
	lda $d015
	ora IndexToORLUT+1,x ; offset the spr num by 1 as ent 0 is spr 1,
	sta $d015
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
	stx $6	
	sty $7	
	lda EntityData.direction,x	
	tax	
	lda DirectionYLUT,x
	sta $fd
	lda DirectionXLUT,x
	sta $fe
;	ldy $7
	lda #0
	sta $8
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
	dec $8
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
	ldy $7
	lda $d002,y
	sec
	sbc $d000
	clc
	adc #10
	cmp #20
	bcs _spiderNoMove
	inc EntityData.entState,x
_spiderNoMove	
	jmp _next	

_notBat	
	ldx $6	
	ldy $7	
	lda EntityData.direction,x		
	beq _right	
	cmp #1	
	beq _up	
	cmp #2	
	beq _left	
_down	
	lda $8
	bne _dnotsafe
	ldx $7 ; what is in y
	inc $d003,x	
	jmp _next		
_dnotsafe
	jsr SetNextEntDir	
	jmp _next	
		
_right
	lda $8
	bne _rnotsafe
	ldx $7 ; what is in y
	inc $d002,x	
	jmp _next		
_rnotsafe
	jsr SetNextEntDir
	jmp _next
	
_up
	lda $8
	bne _unotsafe
	ldx $7 ; what is in y
	dec $d003,x	
	ldy CurrentEntity
	lda EntityData.type,y
	cmp # kEntity.spider
	beq _checkSpider
	jmp _next		
_unotsafe
	jsr SetNextEntDir
	jmp _next

_checkSpider
	lda $d003,x
	sec
	sbc #4
	cmp EntityData.originalY,y
	bcs _nextSpider
	lda #3
	sta EntityData.direction,y
	lda #0
	sta EntityData.entState,y
	beq _next
_nextSpider
	ldx CurrentEntity
	inc EntityData.entState,x
	bne _next
	
_left
	lda $8
	bne _lnotsafe
	ldx $7 ; what is in y
	dec $d002,x	
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
	lda $d003,y
	cmp # kFishLimits.startTwo
	bcs _fNext
	ldy #4
	jmp _fishFound
_fNext
	cmp #kFishLimits.startThree
	bcs _fLast
	ldy #2
	jmp _fishFound
_fLast
	ldy #0
_fishFound	
;	sty $fc ; fish delta	
	tya
	ldy $7 ; restore y to ent num x2
	sta EntityData.movTimer,x
	lda EntityData.direction,x
	cmp #1
	bne _fishDown
	lda #128+80
	sta kVectors.spr1ID,x
	lda $d003,y
	sec
	sbc #1
	sta $d003,y
	cmp # kFishLimits.minY
	bcs _next
	lda #3
	sta EntityData.direction,x
	jmp _next
	
_fishDown
	lda #128+84
	sta kVectors.spr1ID,x
	lda $d003,y
	clc
	adc #1
	sta $d003,y
	cmp # kFishLimits.maxY
	bcc _next
	lda #1
	sta EntityData.direction,x
	jmp _next

; x = ent number
; y = ent number * 2
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
	sta $8
	jsr CheckEntAgainstTile
	lda $8
	bne _resetJump
	lda DirectionYLUT + kDirections.up
	sta $fd
	lda DirectionXLUT + kDirections.up
	sta $fe
	jsr CheckEntAgainstTile
	lda $8
	bne _startFall
	lda EntityData.entState,x
	cmp # SinJumpMax
	bcc _notOverFlow
	bcs _springStore
_resetJump
	ldx $6
	ldy $7
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
	ldx $6
	ldy $7
	lda EntityData.entState,x
	cmp # SinJumpFall
	bcs _notOverFlow
	lda # SinJumpFall
	bne _springStore
_notOverFlow
	clc
	adc #1
_springStore
	sta EntityData.entState,x	
	tax
	lda	$d003,y
	clc
	adc SinJumpTable,x
	sta $d003,y
	; check left/right
	ldx $6
	lda EntityData.direction,x
	and #128+64 
	cmp #128
	beq _springMaxSpeed
	lda #1
	sta $9
	bne _springCheck
_springMaxSpeed
	lda #2
	sta $9
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
	sta $8
	jsr CheckEntAgainstTile
	lda $8 
	beq _moveLeft
	lda # kDirections.right + 64
	ldx $6
	sta EntityData.direction,x
	jmp _moveRight
_springRight
	lda DirectionYLUT + kDirections.right
	sta $fd
	lda DirectionXLUT + kDirections.right
	sta $fe
	lda #0
	sta $8
	jsr CheckEntAgainstTile
	lda $8 
	beq _moveRight
	lda # kDirections.left + 64
	ldx $6
	sta EntityData.direction,x
	jmp _moveLeft
_moveRight	
	ldy $7
	lda $d002,y
	clc
	adc $9
	sta $d002,y
	jmp _next
_moveLeft
	ldy $7
	lda $d002,y
	sec
	sbc $9
	sta $d002,y
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
	tax
	lda $d002,y
	clc
	adc CircleJumpTableStart,x
	sta $d002,y
	lda $d003,y
	clc
	adc CircleJumpTableStart + ( CircleJumpTableCount / 4) + 1,x
	sta $d003,y
	ldx $6
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
	lda $d002,y	
	tax	
	lda $d003,y	
	tay	
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
	;cmp #11
	;bcc _clear ; fix me
	;cmp #43
	;bne _not
_clear	
	rts
_not 
	inc $8 ; fix me
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
	
EntitySpriteColours 	.byte 4,15,10,14,15,5,3,14
EntitySpriteStartFrame 	.byte 128+32,128+40,128+48,128+56,128+64,128+72,128+80,128+88

IndexToORLUT 	.byte 1,2,4,8,16,32,64,128
IndexToANDLUT 	.byte 254,253,251,247,239,223,191,127
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
SinJumpFall = * - SinJumpTable 
.char  1,  2,  1,  3,  2,  3,  4  
.char  3,  5,  4,  5,  6,  5, 6,  6,  7, 8, 8 
SinJumpMax = * - SinJumpTable - 1

;CircleJumpTableStart 
;.char -1,-2,-2,-3,-3,-4,-5
;.char 5,4,3,3,2,2,1
;CircleJumpTableCount = * - CircleJumpTableStart 
;.char -1,-2,-2,-3,-3,-4,-5

CircleJumpTableStart 
.char  5, 5, 5, 5, 4, 4, 4, 3, 2, 2, 1, 1, 0,-1,-1,-2,-2,-3,-4,-4,-4,-5,-5,-5,-5
.char -5,-5,-5,-4,-4,-4,-3,-3,-2,-1,-1, 0, 1, 1, 2, 3, 3, 4, 4, 4, 5, 5, 5
CircleJumpTableCount = * - CircleJumpTableStart   
.char  5, 5, 5, 5, 4, 4, 4, 3, 2, 2, 1, 1, 0

 
*= $2000
fileSprites ;
.binary "sprites.bin"		
* = $3800
fileChars ;
.binary "testchars.raw"
* = $4000
fileCharCols ;		
.binary "testattribs.raw"		
* = $4100	
fileTiles ;		
.binary "tiledefs.raw"	
* = $4300		
fileTileMap; 
.binary "testmap.raw"
fileEntityTest
.byte $3,$d9,$40,$D2,$20,$22,$03
	