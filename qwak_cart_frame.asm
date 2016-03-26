*= $8000
.word launcher
.word launcher
.byte $c3,$c2,$cd,$38,$30 ; CMB80
launcher
	stx $d016
	jsr $fda3 ; irq
	jsr $fd50 ; init ram
	jsr $fd15 ; init i/o
	jsr $ff5b ; init video
	lda #$00
	sta $D020
	sta $D021
	ldx #$fb
	txs
	sei
;set up starting code outside of cartridge-area
move_starter
  ldx #(starter_end-starter_start)
loop1
  lda starter_start,x
  sta $100,x
  dex
  bpl loop1
  jmp $100
;---------------------------------
starter_start	
.logical $100
  ldx #$40 ;64 pages = 256 * 64 = 16384 Bytes
  ldy #0
loop
src
  lda exomized_data,y
dst
  sta $801,y
  iny
  bne loop
  inc src+2
  inc dst+2
  dex
  bpl loop
  lda #$37 ;cart is always on instead of BASIC unless it can be switched off via software
  sta $01
  jmp $80d ;for exomizer, i.e.
.here
starter_end
exomized_data
	.binary "qwak_ex.prg",2
	.align $3FFF,0
	