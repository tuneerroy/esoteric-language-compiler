.data
.balign 4
buf: .space 20, 0
.balign 4
array: .space 30000, 0
.text
.global _start
.balign 16
_output_char:
mov x0, #1
adrp x1, buf@page
add x1, x1, buf@pageoff
ldr x8, [sp], #0
str x8, [x1]
mov x2, #1
mov x16, #4
svc #0x80
ret
_input_char:
mov x0, #1
adrp x1, buf@page
add x1, x1, buf@pageoff
mov x26, x1
mov x2, #1
mov x16, #3
svc #0x80
ldr x0, [x26], #0
ldr x1, [sp], #0
adrp x2, array@page
add x2, x2, array@pageoff
str x0, [x2, x1]
ret
_start:
adrp x29, array@page
add x29, x29, array@pageoff
mov x0, #0
str x0, [sp, #-16]!
// Input
bl _input_char
// While
while1:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend1
// Output
ldr x0, [sp], #0
ldrb w1, [x29, x0]
str x1, [sp, #-16]!
bl _output_char
ldr x0, [sp], #16
// Input
bl _input_char
b while1
whileend1:
mov x0, #0
mov x16, #1
svc #0x80