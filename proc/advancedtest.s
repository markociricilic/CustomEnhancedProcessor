DEPTH 4096

.define STACK       0x10
.define HEX_ADDRESS 0x20
.define SW_ADDRESS  0x30

_start:     mvt     sp, #STACK         // initialize sp to bottom of memory 
            mvt     r1, #HEX_ADDRESS 
            mvt     r2, #SW_ADDRESS 
            mv      r0, #SEG7

MAIN:       ld      r0, [r0]
            st      r0, [r1]

            add     r1, #1
            ld      r0, [r0]
            st      r0, [r1]

LOOP_END:   b       MAIN              // loops until it completes the 7-segment display       

// causes a delay
DELAY: push r1
// the delay loop below works well for DESim. Use a longer delay if running on a 
// DE1-SoC board
       mvt  r1, #0x04       // r1 <- 2^10 = 1024
WAIT:  sub  r1, #1
       bne  WAIT
       pop  r1
       mv   pc, lr

SEG7:  .word 0b00111111       // '0'
       .word 0b00000110       // '1'
       .word 0b01011011       // '2'
       .word 0b01001111       // '3'
       .word 0b01100110       // '4'
       .word 0b01101101       // '5'
       .word 0b01111101       // '6'
       .word 0b00000111       // '7'
       .word 0b01111111       // '8'
       .word 0b01100111       // '9'