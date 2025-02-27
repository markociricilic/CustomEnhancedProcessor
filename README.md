# Enhanced 16-bit ‘ARM-like’ Processor

## Overview 
I designed my own 16-bit, 8 register processor in Verilog based on the ARM architecture family. I added subroutine and stack functionality, along with connections to external devices (I/O devices).

I used the hardware simulation software, ModelSim, to debug and validate the design by checking the clock cycles to confirm that the instructions being executed are what I expect.

Additionally, I wrote my own testbenches to test the instructions the processor should support according to my design.

| Operations Supported | Function performed                              |
|----------------------|-------------------------------------------------|
| mv rX, Op2          | rX ← Op2                                         |
| mvt rX, #D          | rX ← D                                           |
| add rX, Op2         | rX ← rX + Op2                                    |
| sub rX, Op2         | rX ← rX - Op2                                    |
| ld rX, [rY]         | rX ← [rY]                                        |
| st rX, [rY]         | [rY] ← rX                                        |
| and rX, Op2         | rX ← rX & Op2                                    |
| b{cond} Label       | if (cond), pc ← Label                            |
| bl Label            | r6 ← pc, pc ← Label                              |
| push rX             | sp ← sp - 1, [sp] ← rX                           |
| pop rX              | rX ← [sp], sp ← sp + 1                           |
| cmp rX, Op2         | performs rX - Op2, sets flags                    |
| lsl rX, Op2         | rX ← rX << Op2                                   |
| lsr rX, Op2         | rX ← rX >> Op2                                   |
| asr rX, Op2         | rX ← rX >>> Op2                                  |
| ror rX, Op2         | rX ← rX <<>> Op2                                 |

The b{cond} Label instruction supports the following conditions:

| Operations Supported | Condition                       |
|----------------------|---------------------------------|
| b Label             | branch always                    |
| beq Label           | branch if equal (to zero)        |
| bne Label           | branch if not equal (to zero)    |
| bcc Label           | branch if carry clear            |
| bcs Label           | branch if carry set              |
| bpl Label           | branch if positive result        |
| bmi Label           | branch if negative result        |

For an overview of the project concept, hardware details, and photos of the setup, please visit
[Enhanced 16-bit ‘ARM-like’ Processor]([https://markociricilic.com/projects/wireless-communication](https://markociricilic.com/projects/processor/).
