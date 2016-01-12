sGameData .struct 
lives .byte ?
flowers .byte ?
score .byte ?,?,?,?,?,?
high .byte ?,?,?,?,?,?
currLevel .byte ?
exitOpen .byte ?
.ends

sLevelData .struct
numKeys .byte ?
totalKeys .byte ?
playerX .byte ?
playerY .byte ?
exitIndex .byte ?
exitFrame .byte ?
.ends

sTimerTickDowns .struct
dissBlocks 	.byte ?
playerAnim 	.byte ?
doorAnim	.byte ?
.ends

sPlayerData .struct
dead .byte ?
hasShield .byte ?
canFloat .byte ?
hasSpring .byte ?
onGround .byte ?
hasJumped .byte ?
isFalling .byte ?
floatTimer .byte ?
facingRight .byte ?
yDeltaAccum .word ?
baseSprite .byte ?
frameOffset .byte ?
frameCount .byte ?
frameTimer .byte ?
movingLR .byte ?
currAnim .byte ?
.ends

sEntityData .struct
type		.fill 31 ; .byte ?,?,?,?,?,?,?
direction	.fill 31 ;.byte ?,?,?,?,?,?,?
active		.fill 31 ;.byte ?,?,?,?,?,?,?
movTimer	.fill 31 ;.byte ?,?,?,?,?,?,?
animTimer	.fill 31 ;.byte ?,?,?,?,?,?,?
animBase 	.fill 31
animFrame	.fill 31
originalY	.fill 31 ;.byte ?,?,?,?,?,?,?
entState	.fill 31 ;.byte ?,?,?,?,?,?,?
.ends

sCSTCCParams .struct	
xDeltaCheck .byte ? ; pixels
yDeltaCheck .byte ?  ; pixels
xDeltaBackup .byte ? ; pixels
yDeltaBackup .byte ? ; pixels
.ends

sMplexZP .struct
sort .fill mplex.kMaxSpr+1
ybuf .fill mplex.kMaxSpr+1	;sprite y position raster buffer
xbuf .fill mplex.kMaxSpr+1	;sprite x position raster buffer
mbuf .fill mplex.kMaxSpr+1	;sprite x msb raster buffer
cbuf .fill mplex.kMaxSpr+1	;sprite color raster buffer
pbuf .fill mplex.kMaxSpr+1	;sprite pointer raster buffer
sptr .byte ?				;sprite pointer for raster
cnt  .byte ?	
mnt  .byte ?	
lsbtod .byte ?
.ends

sMplexBuffer .struct
ypos .fill mplex.kMaxSpr+1		;sprite y position frame buffer
xpos .fill mplex.kMaxSpr+1		;sprite x position frame buffer
xmsb .fill mplex.kMaxSpr+1		;sprite x msb frame buffer
sprc .fill mplex.kMaxSpr+1		;sprite color frame buffer
sprp .fill mplex.kMaxSpr+1		;sprite pointer frame buffer
unrolledCopyLoopDest .fill 837 	;this is where the unrolled loop sits
unrolledCopyLoopDestEnd .byte ? ;837 = size + 1 for rts
.ends
