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
.ends

sTimerTickDowns .struct
dissBlocks .byte ?
playerAnim .byte ?
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
type		.fill 7 ; .byte ?,?,?,?,?,?,?
direction	.fill 7 ;.byte ?,?,?,?,?,?,?
active		.fill 7 ;.byte ?,?,?,?,?,?,?
movTimer	.fill 7 ;.byte ?,?,?,?,?,?,?
animTimer	.fill 7 ;.byte ?,?,?,?,?,?,?
originalY	.fill 7 ;.byte ?,?,?,?,?,?,?
entState	.fill 7 ;.byte ?,?,?,?,?,?,?
.ends

sCSTCCParams .struct	
xDeltaCheck .byte ? ; pixels
yDeltaCheck .byte ? ; pixels
.ends