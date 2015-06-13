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
exitX .byte ?
exitY .byte ?
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
yDeltaCheck .byte ? ; pixels
.ends