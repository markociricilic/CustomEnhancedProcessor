DEPTH 4096

.define LED_ADDRESS 0x10
.define HEX_ADDRESS 0x20
.define SW_ADDRESS  0x0

HEX_BIT0:    .word   0b0000001
HEX_BIT1:    .word   0b1000000
HEX_BIT2:    .word   0b1000000
HEX_BIT3:    .word   0b1000000

_start:     mvt     r1, #HEX_ADDRESS 
            mvt     r2, #0 
            mvt     sp, #0x10         // initialize sp to bottom of memory 

// MAIN only sets up the first bit in each display
MAIN:       mv      r0, #HEX_BIT0     // copying the least significant bit into r0 for LSL
            ld      r0, [r0]          // loading the value in address r0 to r0
            st      r0, [r1]          // saving the value r0 into the HEX ADDRESS   

            add     r1, #1            // add 1 to the HEX ADDRESS to get to next display
            mv      r0, #HEX_BIT1     // copying the most significant bit into r0 for LSR
            ld      r0, [r0]
            st      r0, [r1]             

            add     r1, #1            // add 1 to the HEX ADDRESS to get to next display
            mv      r0, #HEX_BIT2     // copying the most significant bit into r0 for ASR
            ld      r0, [r0]          
            st      r0, [r1]            
                                     
            add     r1, #1            // add 1 to the HEX ADDRESS to get to next display
            mv      r0, #HEX_BIT3
            ld      r0, [r0]      
            st      r0, [r1]            

LOOP:       mvt     r1, #HEX_ADDRESS   // initialize r1 to be the base HEX ADDRESS again [HEX0]

            mv      r0, #HEX_BIT0       // copying the least significant bit into r0 for LSL
            ld      r2, [r0]            // loading the least significant bit into r0
            lsl     r2, #1              // performs a LSL operation by 1 bit
            st      r2, [r1]            // stores the shifted bit back into the HEX ADDRESS to be displayed
            st      r2, [r0]            // saves the shifted bit in the HEX back into r0 to be repeated 
            bl      DELAY               // going through a delay loop for each bit, when done it executes ALU operation

            mv      r0, #HEX_BIT1
            add     r1, #1              // add 1 to the HEX ADDRESS to get to next display
            ld      r2, [r0]
            lsr     r2, #1            
            st      r2, [r1]          
            st      r2, [r0]         
            bl      DELAY  

            mv      r0, #HEX_BIT2  
            add     r1, #1
            ld      r2, [r0]
            asr     r2, #1         
            st      r2, [r1]         
            st      r2, [r0]  
            bl      DELAY

            mv      r0, #HEX_BIT3
            add     r1, #1
            ld      r2, [r0]
            ror     r2, #1         
            st      r2, [r1]    
            st      r2, [r0]  
            bl      DELAY

LOOP_END:   b       LOOP              // loops until it completes the 7-segment display       

DELAY:      push    r0
            mv      r0, #255

WAIT:       sub     r0, #1
            cmp     r0, #0  
            bne     WAIT        // keep branching until r1 is equal to 0
            pop     r0
            mv      pc, lr