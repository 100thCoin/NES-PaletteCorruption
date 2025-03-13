
	
	
	;;;; HEADER AND COMPILER STUFF ;;;;
	.inesprg 1  ; 2 banks
	.ineschr 1  ; 
	.inesmap 0  ; mapper 0 = NROM
	.inesmir 1  ; background mirroring, vertical

	;;;; CONSTANTS ;;;;

CpuPpuAlignment = $80
CHRFill = $81
RunningTest = $82
MainMenuCurrentSelection = $83
MenuSelectedByte = $84

MainMenuSelections = $9D
MainV_HI = $9D
MainV_LO = $9E
MainCol = $9F
MainPal = $A0
FrameCounter = $F0


	;;;; ASSEMBLY CODE ;;;;
	.org $8000
	

	.org $9000

RESET:
	sei
	cld
	ldx	#%01000000
	stx	$4017
	ldx	#$ff
	txs
	inx
	stx	$2000
	stx	$2001
	stx	$4010

.Vwait	bit	$2002
	bpl	.Vwait

	ldx	#$00
	txa
.clrmem
	sta	<$00,x
	STA $100,X
	STA $200,X
	STA $300,X
	STA $400,X
	STA $500,X
	STA $600,X
	STA $700,X
	inx
	bne	.clrmem

.Vwait2	bit	$2002
	bpl	.Vwait2

	JSR DetermineClockAlignment

	; Initialize the menu stuff.
	
	LDA #$3C
	STA <MainV_HI
	LDA #00
	STA <MainV_LO

ExitTest:

	jsr	loadBG

	; set up some timers for the test
	lda	#$00
	sta	<$01
	lda	#$43
	sta	<$02
	lda	#$02
	
	JSR SetUpMenu
StartTest:

	; Synchronize to PPU and enable NMI
	jsr	init_nmi_sync

.loop	jsr	wait_nmi
	jmp	.loop

	.org $9800

	; Menu Loop
NotRunningTestCode:

	jsr getKey
	
	; handle pressing buttons.
	
	LDA <$21
	AND #$10
	BEQ MenuDoNotStartTest
	LDA #$01
	INC RunningTest
	PLA
	PLA
	PLA
	JMP PrepTest
MenuDoNotStartTest:

	LDA <$21
	BPL MenuNotPressingA
	LDA <MenuSelectedByte
	EOR #01
	STA <MenuSelectedByte
	JSR ToggleSelectedMode
MenuNotPressingA

	LDA <MenuSelectedByte
	BEQ MenuNotInSelectedMode
	JSR MenuDoSelected
	JMP MenuPostMenuStuff
MenuNotInSelectedMode
	JSR MenuNotSelected
	
MenuPostMenuStuff:
	LDA $2002
	LDA #$20
	STA $2006
	LDA #$00
	STA $2006
	LDA #$80
	STA $2000
	LDA #$08
	STA $2001

	PLA
	RTI



	.org	$9200

NMI:
	pha
	
	lda <RunningTest		;+3
	bne RunTest				;+3
	jmp NotRunningTestCode
RunTest:
	
	; Do this sometime before you DMA sprites
	jsr	begin_nmi_sync

	; DMA then enable sprites. Instructions before
	; STA $4014 (excluding begin_nmi_sync) must take
	; an even number of cycles. The only required
	; instruction here is STA $4014.
	bit	<$ff		; to make cycle count even
	lda	#$00
	sta	$2003
	lda	#02
	sta	$4014
	lda	#%00011110
	sta	$2001

	; Our instructions up to this point MUST total
	; 1715 cycles, so we'll burn the rest in a loop.
	
	; if we're running the test, we need to burn a specific number of cycles.
	; I added the conditional "if we're in test mode" branch, so now I need to count.
	; I need to burn exactly 58 cycles. I added 6 already, so 52 to go.
	; 4*13 = 52, so here's 13 4 cycle LDAs
	LDA $0000
	LDA $0001
	LDA $0002
	LDA $0003
	LDA $0004
	LDA $0005
	LDA $0006
	LDA $0007
	LDA $0008
	LDA $0009
	LDA $0010
	LDA $0011
	LDA $0012
	
	lda	#28
	sec
.nmi_1	
	pha
	lda	#9
.nmi_2 	
	sbc	#1
	bne	.nmi_2
	pla
	sbc	#1
	bne	.nmi_1

	jsr	end_nmi_sync

	; We're now synchronized exactly to 2286 cycles
	; after beginning of frame.

;------------------------------



	jsr	waitScan

	lda	#%00000000
	sta	$2001

	NOP
	NOP
	NOP
	NOP
	NOP
	NOP

	ldx	#$3f
	stx	$2006
	ldx	#$00
	stx	$2006
.loop
	ldy	#133
.loop2	dey
	bne	.loop2
	lda	$2007
	sta $300,x
	inx
	cpx	#$10
	bne	.loop


	;jsr getKey
	;JSR SetUpVRegister

	; cool test.
	LDA <FrameCounter
	BEQ EndTest
	DEC <FrameCounter
	JSR SetUpVRegister
	PLA
	RTI
	
EndTest:
	dec <RunningTest ; set this back to 0
	
	PLA
	PLA
	PLA
	PLA

	; disable the NMI
	LDA #0
	STA $2000

	JMP ExitTest
	.org	$9400

waitScan:
	lda	<$01
	beq	.end
	tax
.loop	ldy	#21
.sub	dey
	bne	.sub
	bit	<$ff
	dex
	bne	.loop
.end
waitCycle:
	lda	<$02
.loop	cmp	#9
	bcc	.sub
	sbc	#9
	bcs	.loop
.sub	lsr	A
	bcs	.sub2
	adc	#255
.sub2	beq	.sub3
	bcc	.end
	lsr	A
	beq	.end
.sub3	bcc	.end
	bne	.end
.end	rts

;-------------------------------------------------------------------------------

getKey:
	lda	<$20
	pha
	lda	#$01
	sta	<$20
	sta	$4016
	lsr	A
	sta	$4016
.loop	lda	$4016
	lsr	A
	rol	<$20
	bcc	.loop
	pla
	eor	<$20
	and	<$20
	sta	<$21
	eor	<$22
	sta	<$22
	rts

;-------------------------------------------------------------------------------

loadPal:
	ldx	#$3f
	stx	$2006
	ldx	#$00
	stx	$2006
.loop	lda	$400,x
	sta	$2007
	inx
	cpx	#$20
	bne	.loop
	rts

loadBG:
	lda	#0
	sta	<$fe
	lda	#0
	sta	<$ff
	ldy	#$24
	sty	$2006
	ldy	#$00
	sty	$2006
	ldx	#$08
.loop	lda	#$ff
	sta	$2007
	iny
	bne	.loop
	inc	<$ff
	dex
	bne	.loop

	rts

	.org $9900
; Blargg's NMI sync:
; Copy/Pasted from Blargg's nmi_sync.s, and made minor changes for use in the nesasm assembler
; Allows precise PPU synchronization in NMI handler, without
; having to cycle-time code outside NMI handler.

nmi_sync_count = $00

; Initializes synchronization and enables NMI
; Preserved: X, Y
; Time: 15 frames average, 28 frames max
init_nmi_sync:
	; Disable interrupts and rendering
	sei
	lda #0
	sta $2000
	sta $2001
	
	; Coarse synchronize
	bit $2002
init_nmi_sync_1:
	bit $2002
	bpl init_nmi_sync_1
	
	; Synchronize to odd CPU cycle
	sta $4014

	; Fine synchronize
	lda #3
init_nmi_sync_2:
	sta <nmi_sync_count
	bit $2002
	bit $2002
	php
	eor #$02
	nop
	nop
	plp
	bpl init_nmi_sync_2

	; Delay one frame
init_nmi_sync_3:
	bit $2002
	bpl init_nmi_sync_3
	
	; Enable rendering long enough for frame to
	; be shortened if it's a short one, but not long
	; enough that background will get displayed.
	lda #$08
	sta $2001
	
	; Can reduce delay by up to 5 and this still works,
	; so there's a good margin.
	; delay 2377
	lda #216
init_nmi_sync_4:
	nop
	nop
	sec
	sbc #1
	bne init_nmi_sync_4
	
	sta $2001
	
	lda <nmi_sync_count
	
	; Wait for this and next frame to finish.
	; If this frame was short, loop ends. If it was
	; long, loop runs for a third frame.
init_nmi_sync_5:
	bit $2002
	bit $2002
	php
	eor #$02
	sta <nmi_sync_count
	nop
	nop
	plp
	bpl init_nmi_sync_5
	
	; Enable NMI
	lda #$80
	sta $2000
	
	rts



; Waits until NMI occurs.
; Preserved: A, X, Y
wait_nmi:
	pha
	
	; Reset high/low flag so NMI can depend on it
	bit $2002
	
	; NMI must not occur during taken branch, so we
	; only use branch to get out of loop.
	lda <nmi_sync_count
wait_nmi_1:
	cmp <nmi_sync_count
	bne wait_nmi_2
	jmp wait_nmi_1
wait_nmi_2:
	pla
	rts


; Must be called in NMI handler, before sprite DMA.
; Preserved: X, Y
begin_nmi_sync:
	lda <nmi_sync_count
	and #$02
	beq begin_nmi_sync_1
begin_nmi_sync_1:
	rts


; Must be called after sprite DMA. Instructions before this
; must total 1715 (NTSC)/6900 (PAL) cycles, treating
; JSR begin_nmi_sync and STA $4014 as taking 10 cycles total) 
; Next instruction will begin 2286 (NTSC)/7471 (PAL) cycles
; after the cycle that the frame began in.
; Preserved: X, Y
end_nmi_sync:
	lda <nmi_sync_count
	inc <nmi_sync_count
	and #$02
	bne end_nmi_sync_1
end_nmi_sync_1:
	lda $2002
	bmi end_nmi_sync_2
end_nmi_sync_2:
	bmi end_nmi_sync_3
end_nmi_sync_3:
	rts


; Keeps track of synchronization on frames where no
; synchronization is needed (where begin_nmi_sync/end_nmi_sync
; aren't called).
; Preserved: A, X, Y
track_nmi_sync:
	inc <nmi_sync_count
	rts

DetermineClockAlignment:
	; For my own convenience, I'd like to know the cpu-ppu clock alignment.
	; This test has varying results depending on the console revision.
	; my console is NES-CPU-10 
	; featuring an RP2A03G cpu, and an RP2C02G-0 ppu with a SN74LS139N multiplexer connecting the address bus.

	; anyway, this test works by running a series of read-modify-write instructions to address $2007
	
	; Clear the nametable:
	; reset the latch
	BIT $2002
	; set VRAM Address to $2000
	LDA #$20
	STA $2006
	LDA #$00
	STA $2006
	; set every byte to 0
	LDX #$10
	LDY #$00
ResetVRAMLoop:
	; store 0 in VRAM
	STA $2007	
	; loop 0x1000 times
	DEY
	BNE ResetVRAMLoop
	DEX 
	BNE ResetVRAMLoop 
	
	; with VRAM set up, prep the buffer with the test values I like to use.
	LDX #$55
	JSR PrepBufferWithX
	; The buffer now holds 55
	LDX #$21
	LDY #$10
	JSR SetAddressToXY ; VRAM Address = $2110
	LDX #0
	INC $2007,X	; this makes 3 writes to VRAM.
	; depending on the value at address $2113, we can determine the alignment.
	LDX #$21
	LDY #$13
	JSR ReadFromAddressXXYY ; read from 2113	
	
	CMP #0
	BNE TestForAlignment1
	; test for other proof of alignment 0
	LDX #$21
	LDY #$56
	JSR ReadFromAddressXXYY ; read from 2156
	CMP #$56
	BNE FailedToDetermineAlignment
	LDA #0
	STA <CpuPpuAlignment
	RTS
TestForAlignment1:
	CMP #$56
	BNE TestForAlignment2
	; test for other proof of alignment 1
	LDX #$21
	LDY #$56
	JSR ReadFromAddressXXYY ; read from 2156
	CMP #$56
	BNE FailedToDetermineAlignment
	LDA #1
	STA <CpuPpuAlignment
	RTS
TestForAlignment2:
	CMP #$11
	BNE TestForAlignment3
	; test for other proof of alignment 2
	LDX #$21
	LDY #$11
	JSR ReadFromAddressXXYY ; read from 2111
	CMP #$11
	BNE FailedToDetermineAlignment
	LDA #2
	STA <CpuPpuAlignment
	RTS
TestForAlignment3:
	CMP #$01
	BNE FailedToDetermineAlignment
	; test for other proof of alignment 3
	LDX #$21
	LDY #$01
	JSR ReadFromAddressXXYY ; read from 2156
	CMP #$01
	BNE FailedToDetermineAlignment
	LDA #3
	STA <CpuPpuAlignment
	RTS
FailedToDetermineAlignment:
	LDA #$27 ; this is the CHR data for "?"
	STA <CpuPpuAlignment
AlignmentDetermined:
	RTS
	
	PrepBufferWithX:
	; stores X into VRAM buffer. Overwrites A register with previous buffer contents. (garbage)

	; This writes X to VRAM $2000
	; Reads from VRAM $2000
	; Then resets VRAM $2000 back to a value of zero.

	; reset the latch
	BIT $2002
	; set VRAM Address to $2000
	LDA #$20
	STA $2006
	LDA #$00
	STA $2006
	; store X in VRAM $2000
	STX $2007
	; set VRAM Address back to $2000
	LDA #$20
	STA $2006
	LDA #$00
	STA $2006
	; read the value from VRAM into buffer
	LDA $2007
	PHA
	; set VRAM Address back to $2000
	LDA #$20
	STA $2006
	LDA #$00
	STA $2006
	LDA #0
	; Reset the value of address $2000
	STA $2007
	PLA
	; return
	RTS
	
SetAddressToXY:
	; store X then Y to address $2006 to set the VRAM Address.
	; this helps make my code a bit denser
	
	; reset the latch
	BIT $2002
	; store X and Y to $2006
	STX $2006
	STY $2006
	RTS
	
ReadFromAddressXXYY:
	JSR SetAddressToXY
	; read garbage
	LDA $2007
	; read from buffer
	LDA $2007
	RTS

SetUpMenu:
	;;;;;;;;;;;;;;;;;;;;;;;;;
	; set up the main menu. ;
	;;;;;;;;;;;;;;;;;;;;;;;;;
	
	LDA $2002
	
	
	; the PrintOnScreen function updates the return address, so these bytes get skipped.
	JSR PrintOnScreen
	.word $2066	; PPU Address
	.byte 19	; Length
	; 100th Coins Palette
	.byte 1,0,0,$1D,$11,$24,$C,$18,$12,$17,$2C,$24,$19,$A,$15,$E,$1D,$1D,$E	

	JSR PrintOnScreen
	.word $2086 ; PPU Address
	.byte 19	; Length
	; Corruption Test ROM
	.byte $c,$18,$1B,$1b,$1E,$19,$1D,$12,$18,$17,$24,$1D,$E,$1C,$1D,$24,$1B,$18,$16

	JSR PrintOnScreen
	.word $20C7 ; PPU Address
	.byte 15	; Length
	; Clock Alignment
	.byte $C,$15,$18,$C,$14,$24,$A,$15,$12,$10,$17,$16,$E,$17,$1D

	LDA $2007; INC V
	LDA <CpuPpuAlignment
	STA $2007
	
	JSR PrintOnScreen
	.word $2147 ; PPU Address
	.byte 17	; Length
	; V Register: $3C00
	.byte $1F,$24,$1B,$E,$10,$12,$1C,$1D,$E,$1B,$28,$24,$35,$03,$c,0,0

	JSR PrintOnScreen
	.word $2187 ; PPU Address
	.byte 14	; Length
	; Test Color: 0
	.byte $1D,$E,$1C,$1D,$24,$C,$18,$15,$18,$1B,$28,$24,$0,$0

	JSR PrintOnScreen
	.word $21E2 ; PPU Address
	.byte 25	; Length
	; Palette:        Results:
	.byte $19,$A,$15,$E,$1D,$1D,$E,$28,$24,$24,$24,$24,$24,$24,$24,$24,$24,$1B,$E,$1C,$1E,$15,$1D,$1C,$28

	LDA <MainMenuCurrentSelection
	PHA
	LDX #0
	STX <MainMenuCurrentSelection
.PrintLoop
	JSR QuickUpdateNoGlowText
	INC <MainMenuCurrentSelection
	INX
	CPX #19
	BNE .PrintLoop

	; now print the results of the most recent test.
	
PrintLoopResults:
	LDA $300-19, X ; Load the results color palette at this index
	STA <MainMenuSelections,X
	CMP <MainPal-19, X ; compare with the original palette
	BEQ MainMenuResultsNoGlow
	; they are different!
	JSR QuickUpdateGlowText
	JMP MainMenuResultsContinue
MainMenuResultsNoGlow:
	JSR QuickUpdateNoGlowText
MainMenuResultsContinue:
	INC <MainMenuCurrentSelection
	INX
	CPX #35
	BNE PrintLoopResults

	; restore the current selection, and highlight it

	PLA
	STA <MainMenuCurrentSelection
	LDA <MenuSelectedByte
	BEQ MainMenuDoHighlight
	JSR QuickUpdateGlowText
	JMP DoPal	
MainMenuDoHighlight:
	JSR QuickUpdateHighlightText
	JMP DoPal	

DefaultPal:

	.db	$0F,$27,$27,$30,$0F,$27,$27,$30,$0F,$27,$27,$30,$0F,$27,$27,$30
	.db	$0F,$30,$30,$30,$0F,$30,$30,$30,$0F,$30,$30,$30,$0F,$30,$30,$30	
DoPal:
	LDX #0
.PalLoop
	LDA DefaultPal,x
	STA $300,x
	STA $400,x
	INX
	CPX #$20
	BNE .PalLoop
	
	JSR	loadPal

	LDX #$20
	LDY #$00
	JSR SetAddressToXY

	RTS
	
	
	
	
	
PrintOnScreen:

	PLA
	STA <$40
	PLA
	STA <$41
	; the return address has been pulled.

	; the next 2 bytes form the ppu address
	LDY #2
	LDA $2002
	LDA [$40],Y
	STA $2006
	DEY
	LDA [$40],Y
	STA $2006	
	; the next byte is the length
	LDY #3
	LDA [$40],Y
	STA <$42
	LDX #0
	INY
.printLoop
	LDA [$40],Y
	STA $2007
	INY
	INX
	CPX <$42
	BNE .printLoop
	; Let's fix the return address
	DEY
	TYA
	CLC
	ADC <$40
	STA <$40
	BCC PrintFixReturnHighByte
	INC <$41
PrintFixReturnHighByte:
	LDA <$41
	PHA
	LDA <$40
	PHA
	RTS


WriteByteHighlight:
	PHA
	AND #$F0
	LSR A
	LSR A
	LSR A
	LSR A
	ORA #$80
	STA $2007
	PLA
	AND #$0F
	ORA #$80
	STA $2007
	RTS	

WriteByteNoHighlight:
	PHA
	AND #$F0
	LSR A
	LSR A
	LSR A
	LSR A
	STA $2007
	PLA
	AND #$0F
	STA $2007
	RTS	

WriteByteGlow:
	PHA
	AND #$F0
	LSR A
	LSR A
	LSR A
	LSR A
	ORA #$40
	STA $2007
	PLA
	AND #$0F
	ORA #$40
	STA $2007
	RTS	
	
ToggleSelectedMode:
	; a = MenuSelectedByte
	BEQ ToggleSelectedOff
	; turn on
QuickUpdateGlowText:
	LDA <MainMenuCurrentSelection
	ASL A
	TAX
	LDA MenuOptionsAddressLUT, X
	TAY
	INX
	LDA MenuOptionsAddressLUT, X
	BIT $2002
	STA $2006
	STY $2006
	LDX <MainMenuCurrentSelection
	LDA <MainMenuSelections, X
	JSR WriteByteGlow	
	RTS
ToggleSelectedOff:
QuickUpdateHighlightText:
	; turn off
	LDA <MainMenuCurrentSelection
	ASL A
	TAX
	LDA MenuOptionsAddressLUT, X
	TAY
	INX
	LDA MenuOptionsAddressLUT, X
	BIT $2002
	STA $2006
	STY $2006
	LDX <MainMenuCurrentSelection
	LDA <MainMenuSelections, X
	JSR WriteByteHighlight	
	RTS

QuickUpdateNoGlowText:
	LDA <MainMenuCurrentSelection
	ASL A
	TAX
	LDA MenuOptionsAddressLUT, X
	TAY
	INX
	LDA MenuOptionsAddressLUT, X
	BIT $2002
	STA $2006
	STY $2006
	LDX <MainMenuCurrentSelection
	LDA <MainMenuSelections, X
	JSR WriteByteNoHighlight	
	RTS
	
MenuDoSelected:
	LDA <$20
	AND #$08
	BEQ MenuDoSelected_NotUp
	LDX <MainMenuCurrentSelection
	INC <MainMenuSelections, X
	LDA <MainMenuSelections, X
	AND MenuOptionsBitwiseANDmasksLUT, X
	STA <MainMenuSelections, X
	JMP QuickUpdateGlowText ; includes RTS

MenuDoSelected_NotUp:
	LDA <$20
	AND #$04
	BEQ MenuDoSelected_NotDown
	LDX <MainMenuCurrentSelection
	DEC <MainMenuSelections, X
	LDA <MainMenuSelections, X
	AND MenuOptionsBitwiseANDmasksLUT, X
	STA <MainMenuSelections, X
	JMP QuickUpdateGlowText ; includes RTS

MenuDoSelected_NotDown:	
	LDA <$21
	AND #$01
	BEQ MenuDoSelected_NotRight
	LDX <MainMenuCurrentSelection
	INC <MainMenuSelections, X
	LDA <MainMenuSelections, X
	AND MenuOptionsBitwiseANDmasksLUT, X
	STA <MainMenuSelections, X
	JMP QuickUpdateGlowText ; includes RTS

MenuDoSelected_NotRight:

	LDA <$21
	AND #$02
	BEQ MenuDoSelected_NotLeft
	LDX <MainMenuCurrentSelection
	DEC <MainMenuSelections, X
	LDA <MainMenuSelections, X
	AND MenuOptionsBitwiseANDmasksLUT, X
	STA <MainMenuSelections, X
	JMP QuickUpdateGlowText ; includes RTS

MenuDoSelected_NotLeft:
	RTS
MenuNotSelected:
	LDA $21
	AND #$01
	BEQ MenuNotSelectedNoRight
	JSR QuickUpdateNoGlowText
	LDX <MainMenuCurrentSelection
	LDA MenuOptionsChangePressRight, X
	STA <MainMenuCurrentSelection
	JMP QuickUpdateHighlightText ; includes RTS
MenuNotSelectedNoRight:
	LDA $21
	AND #$02
	BEQ MenuNotSelectedNoLeft
	JSR QuickUpdateNoGlowText
	LDX <MainMenuCurrentSelection
	LDA MenuOptionsChangePressLeft, X
	STA <MainMenuCurrentSelection
	JMP QuickUpdateHighlightText ; includes RTS
MenuNotSelectedNoLeft:
	LDA $21
	AND #$04
	BEQ MenuNotSelectedNoDown
	JSR QuickUpdateNoGlowText
	LDX <MainMenuCurrentSelection
	LDA MenuOptionsChangePressDown, X
	STA <MainMenuCurrentSelection
	JMP QuickUpdateHighlightText ; includes RTS
MenuNotSelectedNoDown:
	LDA $21
	AND #$08
	BEQ MenuNotSelectedNoUp
	JSR QuickUpdateNoGlowText
	LDX <MainMenuCurrentSelection
	LDA MenuOptionsChangePressUp, X
	STA <MainMenuCurrentSelection
	JMP QuickUpdateHighlightText ; includes RTS
MenuNotSelectedNoUp:
	RTS


MenuOptionsAddressLUT:
	.word $2154, $2156
	.word $2193
	.word $2222, $2225, $2228, $222B
	.word $2262, $2265, $2268, $226B
	.word $22A2, $22A5, $22A8, $22AB
	.word $22E2, $22E5, $22E8, $22EB
	
	.word $2233, $2236, $2239, $223C
	.word $2273, $2276, $2279, $227C
	.word $22B3, $22B6, $22B9, $22BC
	.word $22F3, $22F6, $22F9, $22FC

MenuOptionsBitwiseANDmasksLUT:
	.byte $3F, $FF
	.byte $03
	.byte $3F, $3F, $3F, $3F
	.byte $3F, $3F, $3F, $3F
	.byte $3F, $3F, $3F, $3F
	.byte $3F, $3F, $3F, $3F

MenuOptionsChangePressRight:
	.byte $01, $01
	.byte $02
	.byte $04, $05, $06, $06
	.byte $08, $09, $0A, $0A
	.byte $0C, $0D, $0E, $0E
	.byte $10, $11, $12, $12
	
MenuOptionsChangePressLeft:
	.byte $00, $00
	.byte $02
	.byte $03, $03, $04, $05
	.byte $07, $07, $08, $09
	.byte $0B, $0B, $0C, $0D
	.byte $0F, $0F, $10, $11
	
MenuOptionsChangePressDown:
	.byte $02, $02
	.byte $03
	.byte $07, $08, $09, $0A
	.byte $0B, $0C, $0D, $0E
	.byte $0F, $10, $11, $12
	.byte $0F, $10, $11, $12
	
MenuOptionsChangePressUp:
	.byte $00, $01
	.byte $00
	.byte $02, $02, $02, $02
	.byte $03, $04, $05, $06
	.byte $07, $08, $09, $0A
	.byte $0B, $0C, $0D, $0E
	
	
PrepTest:
	LDA #0
	STA $2000 ; disable NMI
	LDA #0
	STA $2001 ; disable rendering
	LDX #0
	LDY #$10
	LDA #20
	STA $2006
	TYA
	STA $2006
	LDA #$FF
.Loop
	STA $2007
	DEX
	BNE .Loop
	DEY
	BNE .Loop
	; okay, the entire nametable $2000 - $2FFF is now overwritten with "FF"
	; let's overwrite the target address.
	LDA $2002
	JSR SetUpVRegister2
	LDA <MainCol
	CLC
	ADC #$FB ; A = FB, FC, FD, or FE. (Color 0, 1, 2, or 3)
	STA $2007 ; Overwrite the target address with a solid square of the target color.


	; Set up the color palette from the colors the user picked out.
	ldx #0
.PalLoop
	lda MainPal,x
	sta $300,x
	sta $310,x
	sta $400,x ; values copied into palette
	sta $410,x ; use the same values for the sprites.
	inx
	cpx #$10
	bne .PalLoop
	jsr	loadPal
	
	; set up the v register
	;JSR SetUpVRegister
	; Due to how blargg's NMI sync works, there must be a few cycles of rendering.
	; It also inconveniently lands on a cycle that would result in corruption if the VRAM address is >= $3C00
	; in order to prevent this from false-positives, let's set the V register to 0000
	LDA #0
	STA $2006
	STA $2006
	; this is a single frame in which the test is pointless.
	; the V register is fixed every frame of the test, so it's no big deal.
	
	LDA #08
	STA <FrameCounter
	JMP StartTest
	
SetUpVRegister:
	BIT $2002
	;TODO: Find a "magic pixel" for every scanline value 0 - 7.
	LDA <MainV_HI
	AND #$F0
	LSR A
	LSR A
	LSR A
	LSR A
	; A = a value from 0 to 7.
	; This value is the scanline we need to disable rendering on.
	; The tricky part is now finding an applicable pixel on a scanline with the desired index
	; But that means we're not going to be able to disable rendering from the upper left corner.
	; In other words, the value written to $2006 won't be equal to the desired value of the V register.
	TAX
	

	lda	#$00	; instead of an immediate value, load from a LUT with X offset
	sta	<$01
	lda	#$43	; instead of an immediate value, load from a LUT with X offset
	sta	<$02
	; This is scheduled to disable rendering on dots 0 and 1
	; Due to how the final cycles in a scanline have already read the first few bytes from the nametable...
	; The V register will be 2 greater than the T register.
	; So we need to set up the T register to be 2 less than the desired V register.
	LDA <MainV_LO
	SEC
	SBC #5
	PHA	; store the low byte - 2
	LDA <MainV_LO
	AND #$1F	; To check for NT boundary crossing, we check
	SEC
	SBC #5
	; If subtracting 2 would cross a page boundary, we need to run the reverse logic of the coarse X increment
	BCS SetV_NoBoundary
	; Okay, so the coarse x increment goes V &= $FFE0; V ^= $400
	; in other words, the low byte gets the EOR #04 treatment, and the low byte needs to add $20	
	LDA <MainV_HI
	EOR #$04
	STA $2006
	PLA
	CLC
	ADC #$20
	STA $2006
	RTS	
SetV_NoBoundary:
	; No baundary crossed
	LDA <MainV_HI
	STA $2006
	PLA
	STA $2006
	RTS

	
	;this is where the tile is updated to change the color of the test
SetUpVRegister2:
	BIT $2002
	LDA <MainV_LO
	SEC
	SBC #2
	PHA	; store the low byte - 2
	LDA <MainV_LO
	AND #$1F	; To check for NT boundary crossing, we check
	SEC
	SBC #2
	; If subtracting 2 would cross a page boundary, we need to run the reverse logic of the coarse X increment
	BCS SetV_NoBoundary2
	; Okay, so the coarse x increment goes V &= $FFE0; V ^= $400
	; in other words, the low byte gets the EOR #04 treatment, and the low byte needs to add $20	
	LDA <MainV_HI
	EOR #$04
	AND #$2F	; Don't write to the palette. Write to the nametable.
	STA $2006
	PLA
	CLC
	ADC #$20
	STA $2006
	RTS	
SetV_NoBoundary2:
	; No baundary crossed
	LDA <MainV_HI
	AND #$2F	; Don't write to the palette. Write to the nametable.
	STA $2006
	PLA
	STA $2006
	RTS

	.bank 1
	.org $BFFA	; Interrupt vectors go here:
	.word NMI ; NMI
	.word RESET ; Reset
	.word $9000 ; IRQ

	;;;; MORE COMPILER STUFF, ADDING THE PATTERN DATA ;;;;

	.incchr "Sprites.pcx"
	.incchr "Tiles.pcx"