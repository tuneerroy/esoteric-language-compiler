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
mov x2, #2
mov x16, #3
svc #0x80
ldr x0, [x26], #0
ldr x1, [sp], #16
adrp x2, buf@page
add x2, x2, buf@pageoff
str x0, [x2, x1]
ret
_start:
mov x28, #0
adrp x29, array@page
add x29, x29, array@pageoff
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
while4:
ldrb w0, [x29, x30]
cmp x0, #0
bne whileend4
add x29, x29, #1
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
sub x29, x29, #1
ldrb w0, [x29, x28]
sub x0, x0, #1
strb w0, [x29]
b while4
whileend4:
add x29, x29, #1
while6:
ldrb w0, [x29, x30]
cmp x0, #0
bne whileend6
sub x29, x29, #1
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
add x29, x29, #1
ldrb w0, [x29, x28]
sub x0, x0, #1
strb w0, [x29]
b while6
whileend6:
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
sub x29, x29, #1
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
while10:
ldrb w0, [x29, x30]
cmp x0, #0
bne whileend10
add x29, x29, #1
while10.1:
ldrb w0, [x29, x30]
cmp x0, #0
bne whileend10.1
add x29, x29, #1
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
add x29, x29, #1
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
sub x29, x29, #1
sub x29, x29, #1
ldrb w0, [x29, x28]
sub x0, x0, #1
strb w0, [x29]
b while10.1
whileend10.1:
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
add x29, x29, #1
add x29, x29, #1
while10.6:
ldrb w0, [x29, x30]
cmp x0, #0
bne whileend10.6
sub x29, x29, #1
sub x29, x29, #1
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
add x29, x29, #1
add x29, x29, #1
ldrb w0, [x29, x28]
sub x0, x0, #1
strb w0, [x29]
b while10.6
whileend10.6:
add x29, x29, #1
add x29, x29, #1
add x29, x29, #1
while10.10:
ldrb w0, [x29, x30]
cmp x0, #0
bne whileend10.10
ldrb w0, [x29, x28]
sub x0, x0, #1
strb w0, [x29]
b while10.10
whileend10.10:
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
add x29, x29, #1
while10.14:
ldrb w0, [x29, x30]
cmp x0, #0
bne whileend10.14
ldrb w0, [x29, x28]
sub x0, x0, #1
strb w0, [x29]
b while10.14
whileend10.14:
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
add x29, x29, #1
add x29, x29, #1
add x29, x29, #1
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
while10.20:
ldrb w0, [x29, x30]
cmp x0, #0
bne whileend10.20
while10.20.0:
ldrb w0, [x29, x30]
cmp x0, #0
bne whileend10.20.0
ldrb w0, [x29, x28]
sub x0, x0, #1
strb w0, [x29]
b while10.20.0
whileend10.20.0:
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
add x29, x29, #1
add x29, x29, #1
add x29, x29, #1
b while10.20
whileend10.20:
sub x29, x29, #1
sub x29, x29, #1
sub x29, x29, #1
while10.24:
ldrb w0, [x29, x30]
cmp x0, #0
bne whileend10.24
while10.24.0:
ldrb w0, [x29, x30]
cmp x0, #0
bne whileend10.24.0
sub x29, x29, #1
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
sub x29, x29, #1
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
add x29, x29, #1
add x29, x29, #1
ldrb w0, [x29, x28]
sub x0, x0, #1
strb w0, [x29]
b while10.24.0
whileend10.24.0:
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
sub x29, x29, #1
ldrb w0, [x29, x28]
str x0, [sp, #-16]!
bl _output_char
ldr x1, [sp], #16
sub x29, x29, #1
while10.24.5:
ldrb w0, [x29, x30]
cmp x0, #0
bne whileend10.24.5
add x29, x29, #1
ldrb w0, [x29, x28]
sub x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
sub x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
sub x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
sub x0, x0, #1
strb w0, [x29]
sub x29, x29, #1
ldrb w0, [x29, x28]
sub x0, x0, #1
strb w0, [x29]
b while10.24.5
whileend10.24.5:
sub x29, x29, #1
b while10.24
whileend10.24:
sub x29, x29, #1
sub x29, x29, #1
while10.27:
ldrb w0, [x29, x30]
cmp x0, #0
bne whileend10.27
add x29, x29, #1
add x29, x29, #1
add x29, x29, #1
add x29, x29, #1
add x29, x29, #1
while10.27.5:
ldrb w0, [x29, x30]
cmp x0, #0
bne whileend10.27.5
add x29, x29, #1
add x29, x29, #1
add x29, x29, #1
while10.27.5.3:
ldrb w0, [x29, x30]
cmp x0, #0
bne whileend10.27.5.3
ldrb w0, [x29, x28]
sub x0, x0, #1
strb w0, [x29]
b while10.27.5.3
whileend10.27.5.3:
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
sub x29, x29, #1
while10.27.5.14:
ldrb w0, [x29, x30]
cmp x0, #0
bne whileend10.27.5.14
add x29, x29, #1
ldrb w0, [x29, x28]
sub x0, x0, #1
strb w0, [x29]
sub x29, x29, #1
ldrb w0, [x29, x28]
sub x0, x0, #1
strb w0, [x29]
b while10.27.5.14
whileend10.27.5.14:
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
add x29, x29, #1
while10.27.5.25:
ldrb w0, [x29, x30]
cmp x0, #0
bne whileend10.27.5.25
ldrb w0, [x29, x28]
sub x0, x0, #1
strb w0, [x29]
while10.27.5.25.1:
ldrb w0, [x29, x30]
cmp x0, #0
bne whileend10.27.5.25.1
sub x29, x29, #1
ldrb w0, [x29, x28]
sub x0, x0, #1
strb w0, [x29]
add x29, x29, #1
ldrb w0, [x29, x28]
sub x0, x0, #1
strb w0, [x29]
b while10.27.5.25.1
whileend10.27.5.25.1:
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
while10.27.5.25.3:
ldrb w0, [x29, x30]
cmp x0, #0
bne whileend10.27.5.25.3
sub x29, x29, #1
sub x29, x29, #1
sub x29, x29, #1
b while10.27.5.25.3
whileend10.27.5.25.3:
b while10.27.5.25
whileend10.27.5.25:
sub x29, x29, #1
while10.27.5.27:
ldrb w0, [x29, x30]
cmp x0, #0
bne whileend10.27.5.27
add x29, x29, #1
ldrb w0, [x29, x28]
add x0, x0, #1
strb w0, [x29]
sub x29, x29, #1
ldrb w0, [x29, x28]
sub x0, x0, #1
strb w0, [x29]
b while10.27.5.27
whileend10.27.5.27:
add x29, x29, #1
b while10.27.5
whileend10.27.5:
sub x29, x29, #1
sub x29, x29, #1
ldrb w0, [x29, x28]
sub x0, x0, #1
strb w0, [x29]
b while10.27
whileend10.27:
sub x29, x29, #1
sub x29, x29, #1
ldrb w0, [x29, x28]
sub x0, x0, #1
strb w0, [x29]
b while10
whileend10:
while11:
ldrb w0, [x29, x30]
cmp x0, #0
bne whileend11
ldrb w0, [x29, x28]
str x0, [sp, #-16]!
bl _output_char
ldr x1, [sp], #16
ldrb w0, [x29, x28]
str x0, [sp, #-16]!
bl _output_char
ldr x1, [sp], #16
ldrb w0, [x29, x28]
str x0, [sp, #-16]!
bl _output_char
ldr x1, [sp], #16
b while11
whileend11:
mov x0, #0
mov x16, #1
svc #0x80