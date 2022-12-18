.data
.balign 4
buf: .ds 4 // memory buffer for IO
.balign 4
heap: .space 1000000, 0
.text
.global _start
.align 4
_divide:
udiv x2, x0, x1
msub x3, x2, x1, x0
ret
_output_char:
mov x0, #1
adrp x1, buf@page
add x1, x1, buf@pageoff
ldr x8, [sp]
str x8, [x1]
mov x2, #1
mov x16, #4
svc #0x80
ret
_output_num:
ldr x0, [sp]
str x30, [sp, #-16]! // push x30 onto stack
_int_to_ascii:
cmp x0, #0
b.ge _int_to_ascii_pos
mov x1, #45 // ascii for '-'
str x1, [sp, #-16]!
mov x27, x0
bl _output_char
ldr x28, [sp], #16
mov x0, x27
mov x27, #0
negs x0, x0
_int_to_ascii_pos:
// expects x0 = int
cmp x0, #10
b.lt _int_to_ascii_br
mov x1, #10             // if x0 >= 10
bl _divide
mov x0, x2
add x3, x3, #48
str x3, [sp, #-16]!
add x27, x27, #1
bl _int_to_ascii_pos
_int_to_ascii_br:      // if x0 < 10
add x8, x0, #48         // add ascii representation of x0 to stack
str x8, [sp, #-16]!
add x27, x27, #1
// print the stack x27 times
_print_stack:
cmp x27, #0             // if x27 <= 0
b.le _done_ascii
bl _output_char          // print top of stack
ldr x28, [sp], #16
sub x27, x27, #1
bl _print_stack
_done_ascii:
ldr x30, [sp], #16 // load linked address back to x30
ret
_slide:
// expects x0 = number of things to slide off
ldr x1, [sp], #16
mov x29, x30
slide_loop:
cmp x0, #0
b.le slide
ldr x28, [sp], #16
sub x0, x0, #1
bl slide_loop
slide:
str x1, [sp, #-16]!
mov x30, x29
ret
_input_num:
mov x22, x30            // store address
mov x0, #0              // fd = stdin
adrp x26, buf@page
add x26, x26, buf@pageoff // x26 is address of buf
mov x1, x26
mov x2, #11             // count = 11 (since max int is 2147483647)
mov x16, #3             // unix read system call
svc #0x80               // call kernel
// now buf contains the value read in by the system call
// now x0 contains how many bytes were read (including the \n)
sub x0, x0, #1
mov x25, x0             // x25 = num bytes read
mov x24, x0             // x24 = num bytes read
mov x23, #0             // x23 = where the number will be stored
ldrb w1, [x26, x25]     // w1 = last character read
cmp x1, #10             // check if x1 = '\n'
bne _no_nl
sub x25, x25, #1        // ignore the '\n' if it appears
_no_nl:
cmp x25, #0
blt _calculate_number   // number is on stack, we now need to calculate it
ldrb w0, [x26, x25]
sub x0, x0, #48        // convert ascii
str x0, [sp, #-16]!
sub x25, x25, #1 
bl _no_nl
_calculate_number:
cmp x24, #0
beq _done_calculation
ldr x0, [sp], #16       // push top of stack to x0
mov x1, #10
madd x23, x23, x1, x0
sub x24, x24, #1
bl _calculate_number
_done_calculation:
ldr x1, [sp], #16       // push address from top of stack
adrp x2, heap@page
add x2, x2, heap@pageoff
str x23, [x2, x1]       // store number in location given by top of the stack
mov x30, x22
ret
_start:
// push
mov x0, #258
str x0, [sp, #-16]!
// push
mov x0, #30258
str x0, [sp, #-16]!
// push
mov x0, #0
str x0, [sp, #-16]!
// store
ldr x0, [sp], #16 // value
ldr x1, [sp], #16 // address
adrp x2, heap@page
add x2, x2, heap@pageoff
str x0, [x2, x1]
// push
mov x0, #72
str x0, [sp, #-16]!
// outputChar
mov x0, #1
adrp x1, buf@page
add x1, x1, buf@pageoff
ldr x8, [sp]
str x8, [x1]
mov x2, #1
mov x16, #4
svc #0x80
// push
mov x0, #101
str x0, [sp, #-16]!
// outputChar
mov x0, #1
adrp x1, buf@page
add x1, x1, buf@pageoff
ldr x8, [sp]
str x8, [x1]
mov x2, #1
mov x16, #4
svc #0x80
// push
mov x0, #108
str x0, [sp, #-16]!
// dup
ldr x0, [sp], #16
str x0, [sp, #-16]!
str x0, [sp, #-16]!
// outputChar
mov x0, #1
adrp x1, buf@page
add x1, x1, buf@pageoff
ldr x8, [sp]
str x8, [x1]
mov x2, #1
mov x16, #4
svc #0x80
// outputChar
mov x0, #1
adrp x1, buf@page
add x1, x1, buf@pageoff
ldr x8, [sp]
str x8, [x1]
mov x2, #1
mov x16, #4
svc #0x80
// push
mov x0, #111
str x0, [sp, #-16]!
// outputChar
mov x0, #1
adrp x1, buf@page
add x1, x1, buf@pageoff
ldr x8, [sp]
str x8, [x1]
mov x2, #1
mov x16, #4
svc #0x80
// push
mov x0, #32
str x0, [sp, #-16]!
// outputChar
mov x0, #1
adrp x1, buf@page
add x1, x1, buf@pageoff
ldr x8, [sp]
str x8, [x1]
mov x2, #1
mov x16, #4
svc #0x80
// push
mov x0, #87
str x0, [sp, #-16]!
// outputChar
mov x0, #1
adrp x1, buf@page
add x1, x1, buf@pageoff
ldr x8, [sp]
str x8, [x1]
mov x2, #1
mov x16, #4
svc #0x80
// push
mov x0, #111
str x0, [sp, #-16]!
// outputChar
mov x0, #1
adrp x1, buf@page
add x1, x1, buf@pageoff
ldr x8, [sp]
str x8, [x1]
mov x2, #1
mov x16, #4
svc #0x80
// push
mov x0, #114
str x0, [sp, #-16]!
// outputChar
mov x0, #1
adrp x1, buf@page
add x1, x1, buf@pageoff
ldr x8, [sp]
str x8, [x1]
mov x2, #1
mov x16, #4
svc #0x80
// push
mov x0, #108
str x0, [sp, #-16]!
// outputChar
mov x0, #1
adrp x1, buf@page
add x1, x1, buf@pageoff
ldr x8, [sp]
str x8, [x1]
mov x2, #1
mov x16, #4
svc #0x80
// push
mov x0, #100
str x0, [sp, #-16]!
// outputChar
mov x0, #1
adrp x1, buf@page
add x1, x1, buf@pageoff
ldr x8, [sp]
str x8, [x1]
mov x2, #1
mov x16, #4
svc #0x80
// push
mov x0, #33
str x0, [sp, #-16]!
// outputChar
mov x0, #1
adrp x1, buf@page
add x1, x1, buf@pageoff
ldr x8, [sp]
str x8, [x1]
mov x2, #1
mov x16, #4
svc #0x80
// push
mov x0, #10
str x0, [sp, #-16]!
// outputChar
mov x0, #1
adrp x1, buf@page
add x1, x1, buf@pageoff
ldr x8, [sp]
str x8, [x1]
mov x2, #1
mov x16, #4
svc #0x80
// discard
ldr x0, [sp], #16
end:
mov x0, #0
mov x16, #1
svc #0x80