;***************************************************************************
;*
;* Title: PWM_100_levels_intr.asm
;* Author: Brian Kim
;* Version: 1.0
;* Last updated: 11/30/2023
;* Target: AVR128DB48
;* 
;* DESCRIPTION
;* Reads a 3 digit value from the 16 value keypad and uses this value to determine 
;* the duty cycle of a PWM output. The entered value is displayed on the 
;* LCD screen from the previous lab. The duty cycle can be set to any value 
;* from 0% to 100% in steps of 1%. Values above 100% are not valid. Instead of 
;* polling from keypress, IRQ from pushbutton is connected to PE0.
;*
;* VERSION HISTORY
;* 1.0 Original version
;***************************************************************************

;***  DATA Segment  **************************************************   
.dseg
dsp_buff_1:   .byte 16		;16 byte array for display line 1
dsp_buff_2:   .byte 16		;16 byte array for display line 2
dsp_buff_3:   .byte 16		;16 byte array for display line 3

;***  CODE Segment  **************************************************  
.CSEG

; interrupt vector table, with several 'safety' stubs
rjmp RESET      ;Reset/Cold start vector
reti            ;External Intr0 vector
reti            ;External Intr1 vector

;interrupt vector table
.org PORTE_PORT_vect
	jmp porte_ISR				;vector for all PORTE pin changes IRQs

;**********************************************************************
;************* M A I N   A P P L I C A T I O N   C O D E  *************
;**********************************************************************

RESET:
start:
	ldi r16, 0xFF			;loads r16 with all 1s
	out VPORTD_DIR, r16		;PORTD - all pins configured as outputs. VPORTD drives LED bargraph
	out VPORTD_OUT, r16		;turns off LEDS

	cbi VPORTB_DIR, 5		;PB5 set as input from flip flop status bit (Q)
	sbi VPORTB_DIR, 4		;PB4 set as output for flip flop clear. CLR when read DCBA data

    sbi VPORTA_DIR, 7		; set PA7 = output.                   
    sbi VPORTA_OUT, 7		; set /SS of DOG LCD = 1 (Deselected)

    rcall init_lcd_dog    ; init display, using SPI serial interface
    rcall clr_dsp_buffs   ; clear all three SRAM memory buffer lines

	rcall update_lcd_dog		;display data in memory buffer on LCD

   	ldi r16, 0x00
	out VPORTC_DIR, r16		;all PORTC pins configured as input for keypress

	;Configure I/O port
	cbi VPORTE_DIR, 0			;PE0 input - gets output from DAV from keypad encoder
	;Configure interrupt
	lds r16, PORTE_PIN0CTRL		;set ISC for PE0 to pos. edge
	ori r16, 0x02
	sts PORTE_PIN0CTRL, r16

	sei							;enable global interrupts

display:
	;load_line_1 into dbuff1:
	ldi  XH, high(dsp_buff_1)  ; X pointer to line 1 memory buffer
	ldi  XL, low(dsp_buff_1)   ;Points to first byte
	;DutyCycle: 000%
    ldi r16, 'D'
	st X+, r16
	ldi r16, 'u'
	st X+, r16
   	ldi r16, 't'
	st X+, r16
	ldi r16, 'y'
	st X+, r16
	ldi r16, 'C'
	st X+, r16
   	ldi r16, 'y'
	st X+, r16
	ldi r16, 'c'
	st X+, r16
   	ldi r16, 'l'
	st X+, r16
	ldi r16, 'e'
	st X+, r16
	ldi r16, ':'
	st X+, r16
	ldi r16, ' '
	st X+, r16
	ldi r16, '0'
	st X+, r16
	ldi r16, '0'
	st X+, r16
	ldi r16, '0'
	st X+, r16
	ldi r16, '%'
	st X+, r16
	ldi r16, ' '
	st X, r16

; Read keypad value before generating one period of PWM waveform
; Each loop back to label polling is one period
polling:  
wait_for_1:					;FF is set when a key is pressed
;	sbis VPORTB_IN, 5
;	rjmp wait_for_1			;1 is generated in response to a keypress
	lds r16, PORTE_INTFLAGS		;check for PE0 IRQ flag set
	sbrc r16, 0			
	rjmp polling

porte_ISR:
	cli							;clear global interrupt enable, I = 0
	push r16					;save r16 then SREG, note I = 0
	in r16, CPU_SREG			
	push r16

	;Determine which pins of PORTE have IRQs
	lds r16, PORTE_INTFLAGS		;check for PE0 IRQ flag set
	sbrc r16, 0
	rcall PB_sub				;execute subroutine for PE0

	pop r16						;restore SREG then r16
	out CPU_SREG, r16			;note I in SREG now = 0
	pop r16
	sei							;SREG I = 1
	reti						;return from PORTE pin change ISR
	;Note: reti does not set I on an AVR128DB48

;Subroutine called by porte_ISR
PB_sub:						;PE0's task to be done
	in r16, VPORTC_IN		;read switch value, gives high count
	mov r19, r16			;copy r16 into r19. r19 is used for DCBA bits
	andi r19, 0xF0			;masks first four r19 bits (PC7 - PC4) for the DCBA MM74C922 output
	lsr r19					;shift r19 4 times to line up with table lookup bits
	lsr r19
	lsr r19
	lsr r19
	rcall bcd_ASCII

	cpi r19, 'C'			;if 'C' (enter) is pressed, enters a valid duty cycle
	breq enter_DC
	cpi r19, 'A'			;if 'A' (clear) is pressed, clear current setting
	breq clear_setting1
	
	rcall char_shift_left	;keypress detected, shift characters left
	st X, r19				;store next setting character

	ldi r16, PORT_INT0_bm		;clear IRQ flag for PE0
	sts PORTE_INTFLAGS, r16
;display
	rcall update_lcd_dog	;display data in memory buffer on LCD
;loop polling
	rjmp polling
	

;***************************** char_shift_left *****************************;
char_shift_left:
	ldi r21, 2
	ldi XH, high (dsp_buff_1 + 12) ; Load XH and XL as a pointer to 1st setting digit
    ldi XL, low (dsp_buff_1 + 12)  
	ldi YH, high (dsp_buff_1 + 13)	; Load into byte after X
	ldi YL, low (dsp_buff_1 + 13)
shift_loop:
	ld r22, Y+
	st X+, r22
	dec r21
	brne shift_loop
	rcall update_lcd_dog
	ret

;***************************** clear_setting1 *****************************;
;Account for relative branch out of reach errors
clear_setting1:
	rjmp clear_setting

display1:
	rjmp display
;***************************** enter_DC *****************************;
enter_DC:
bcd2binary:
	ldi XH, high (dsp_buff_1 + 12) ; Load XH and XL as a pointer to 1st setting digit
    ldi XL, low (dsp_buff_1 + 12)  
	;Process Hundreds Place
    ld r23, X					; Load hundreds digit
    subi r23, '0'            ; Convert ASCII to number
    ldi r20, 100
    mul r23, r20             ; Multiply by 100
    mov r23, r0              ; Store result in r23

    ;Process Tens Place
    ld r24, X+               ; Load tens digit
    subi r24, '0'            ; Convert ASCII to number
    ldi r20, 10
    mul r24, r20             ; Multiply by 10
    mov r24, r0              ; Store result in r19

    ; Combine Hundreds and Tens
    add r23, r24            ; Add hundreds and tens, result in r19
    ; Process Ones Place
    ld r25, X               ; Load ones digit
    subi r25, '0'           ; Convert ASCII to number
    add r23, r25            ; Add ones to combined hundreds and tens
	cpi r25, 101			; If concatenated total is larger than 100, invalid input
	brsh invalid
    mov r20, r25
    add r25, r20            ;multiplying it itself therefore its times 2 
    lsr r20                 ;logical shift right r20 = r20/2
    add r25, r20            ;r19 = r19(doubled) + r20(r19 halved)
							;therefore r19 has the duty cycle value * 2.5            
							;2.5 is 256 * 0.01 and rounded
    mov r20, r25
	rjmp again

invalid:
	;display invalid input message
	;Invalid Cycle  
    ldi r16, 'I'
	st X+, r16
	ldi r16, 'n'
	st X+, r16
   	ldi r16, 'v'
	st X+, r16
	ldi r16, 'a'
	st X+, r16
	ldi r16, 'l'
	st X+, r16
   	ldi r16, 'i'
	st X+, r16
	ldi r16, 'd'
	st X+, r16
   	ldi r16, ' '
	st X+, r16
	ldi r16, 'C'
	st X+, r16
	ldi r16, 'y'
	st X+, r16
	ldi r16, 'c'
	st X+, r16
	ldi r16, 'l'
	st X+, r16
	ldi r16, 'e'
	st X+, r16
	ldi r16, ' '
	st X+, r16
	ldi r16, ' '
	st X+, r16
	ldi r16, ' '
	st X, r16

	rcall update_lcd_dog

	;delay for around 4 seconds
	ldi r22, 255
	ldi r23, 255
	rcall v_delay
	rcall v_delay
	rcall v_delay
	rcall v_delay

	ldi r16, PORT_INT0_bm		;clear IRQ flag for PE0
	sts PORTE_INTFLAGS, r16
	rjmp display1

again:
	ldi r17, 255			;load period
	sub r17, r20			;compute low count
	cpi r20, 0				;is high count = 0?
	breq pwm_low			;if yes, skip to low
	sbi VPORTD_OUT, 0		;set PD0 - LED ON

; Generating high time of PWM period
pwm_high:
	dec r20					;decrement high counter - put BP here
	nop						;delay longer
	nop
	brne pwm_high			;loop back to pwm_high if count is not 0
	cpi r17, 0				;is low = 0?
	breq polling1			;if yes, skip to next period of PWM
	cbi VPORTD_OUT, 0		;set PD0 - LED OFF

; Generating low time of PWM period
pwm_low:
	dec r17					;decrement low counter -  put BP here
	nop						;delay longer
	nop
	brne pwm_low			;loop back to pwm_low if count is not 0
	rjmp polling1				;generate next period of PWM signal

;***************************** polling1 *****************************;
;Account for relative branch out of reach errors
polling1:
	rjmp polling

;***************************** clear_setting *****************************;
clear_setting:
	ldi XH, high (dsp_buff_1 + 12) ; Load XH and XL as a pointer to nth
    ldi XL, low (dsp_buff_1 + 12)  ; byte of buffer for line 1.
	ldi r16, '0'
	st X+, r16			; load message into buffer(s)
	ldi r16, '0'
	st X+, r16			; load message into buffer(s)
	ldi r16, '0'
	st X+, r16			; load message into buffer(s).
	ldi r16, PORT_INT0_bm		;clear IRQ flag for PE0
	sts PORTE_INTFLAGS, r16
	rcall update_lcd_dog
	rjmp polling

;---------------------------- SUBROUTINES ----------------------------
	
;=======================================
.include "lcd_dog_asm_driver_avr128.inc"  ; LCD DOG init/update procedures.
;=======================================

;*******************************************************************
;NAME:      clr_dsp_buffs
;FUNCTION:  Initializes dsp_buffers 1, 2, and 3 with blanks (0x20)
;ASSUMES:   Three CONTIGUOUS 16-byte dram based buffers named
;           dsp_buff_1, dsp_buff_2, dsp_buff_3.
;RETURNS:   nothing.
;MODIFIES:  r25,r26, Z-ptr
;CALLS:     none
;CALLED BY: main application and diagnostics
;********************************************************************
clr_dsp_buffs:
     ldi R25, 48               ; load total length of both buffer.
     ldi R26, ' '              ; load blank/space into R26.
     ldi ZH, high (dsp_buff_1) ; Load ZH and ZL as a pointer to 1st
     ldi ZL, low (dsp_buff_1)  ; byte of buffer for line 1.
   
    ;set DDRAM address to 1st position of first line.
store_bytes:
     st  Z+, R26       ; store ' ' into 1st/next buffer byte and
                       ; auto inc ptr to next location.
     dec  R25          ; 
     brne store_bytes  ; cont until r25=0, all bytes written.
     ret

;*******************************************************************
;NAME:      bcd_ASCII
;FUNCTION:  Converts keypad binary value into ASCII value to be displayed
;ASSUMES:   
;
;RETURNS:   nothing.
;MODIFIES:  r19
;CALLS:     none
;CALLED BY: main application and diagnostics
;********************************************************************
;Table lookup code for keypad
bcd_ASCII:
	andi r19, $0F			;mask out 4 ms bits
	cpi r19, 16				;result 0 - F?
	brlo lookup				;if yes, look up keycode value
	clc						;if no, clr carry, zero result, and return
	ldi r19, 0
	ret
lookup:
	ldi ZH, high (segtable * 2)		;set Z to point to start of table
	ldi ZL, low (segtable * 2)		
	ldi r18, $00					;add offset to Z pointer
	add ZL, r19						
	adc ZH, r18
	lpm r19, Z						;load byte from table pointed to by Z
	sec								;set carry to indicate valid result
	ret

;Table of segment values to display digits 0 - F (Converted to ASCII)
segtable: .db '1', '2', '3', 'F', '4', '5', '6', 'E', '7', '8', '9', 'D', 'A', '0', 'B', 'C'


;***** END OF FILE ******

