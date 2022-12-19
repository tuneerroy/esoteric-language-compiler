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
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// While
while8:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8
// While
while8.0:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// While
while8.0.1:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.1
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
b while8.0.1
whileend8.0.1:
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// While
while8.0.3:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.3
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
b while8.0.3
whileend8.0.3:
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// While
while8.0.6:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.6
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
b while8.0.6
whileend8.0.6:
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// While
while8.0.9:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9
// While
while8.0.9.0:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.0
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// While
while8.0.9.0.3:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.0.3
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
b while8.0.9.0.3
whileend8.0.9.0.3:
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
b while8.0.9.0
whileend8.0.9.0:
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// While
while8.0.9.2:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.2
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
b while8.0.9.2
whileend8.0.9.2:
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// While
while8.0.9.5:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.5
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// While
while8.0.9.5.1:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.5.1
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
b while8.0.9.5.1
whileend8.0.9.5.1:
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// While
while8.0.9.5.8:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.5.8
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
b while8.0.9.5.8
whileend8.0.9.5.8:
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// Output
ldr x0, [sp], #0
ldrb w1, [x29, x0]
str x1, [sp, #-16]!
bl _output_char
ldr x0, [sp], #16
// While
while8.0.9.5.12:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.5.12
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
b while8.0.9.5.12
whileend8.0.9.5.12:
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// While
while8.0.9.5.15:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.5.15
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// While
while8.0.9.5.15.3:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.5.15.3
// While
while8.0.9.5.15.3.0:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.5.15.3.0
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
b while8.0.9.5.15.3.0
whileend8.0.9.5.15.3.0:
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
b while8.0.9.5.15.3
whileend8.0.9.5.15.3:
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// While
while8.0.9.5.15.8:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.5.15.8
// While
while8.0.9.5.15.8.0:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.5.15.8.0
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
b while8.0.9.5.15.8.0
whileend8.0.9.5.15.8.0:
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
b while8.0.9.5.15.8
whileend8.0.9.5.15.8:
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
b while8.0.9.5.15
whileend8.0.9.5.15:
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// While
while8.0.9.5.19:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.5.19
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// While
while8.0.9.5.19.3:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.5.19.3
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
b while8.0.9.5.19.3
whileend8.0.9.5.19.3:
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// While
while8.0.9.5.19.15:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.5.19.15
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// While
while8.0.9.5.19.15.1:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.5.19.15.1
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
b while8.0.9.5.19.15.1
whileend8.0.9.5.19.15.1:
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// While
while8.0.9.5.19.15.5:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.5.19.15.5
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// While
while8.0.9.5.19.15.5.2:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.5.19.15.5.2
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
b while8.0.9.5.19.15.5.2
whileend8.0.9.5.19.15.5.2:
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// While
while8.0.9.5.19.15.5.9:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.5.19.15.5.9
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
b while8.0.9.5.19.15.5.9
whileend8.0.9.5.19.15.5.9:
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// While
while8.0.9.5.19.15.5.11:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.5.19.15.5.11
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
b while8.0.9.5.19.15.5.11
whileend8.0.9.5.19.15.5.11:
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
b while8.0.9.5.19.15.5
whileend8.0.9.5.19.15.5:
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
b while8.0.9.5.19.15
whileend8.0.9.5.19.15:
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// While
while8.0.9.5.19.17:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.5.19.17
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
b while8.0.9.5.19.17
whileend8.0.9.5.19.17:
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// While
while8.0.9.5.19.22:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.5.19.22
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
b while8.0.9.5.19.22
whileend8.0.9.5.19.22:
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
b while8.0.9.5.19
whileend8.0.9.5.19:
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// While
while8.0.9.5.24:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.5.24
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// While
while8.0.9.5.24.1:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.5.24.1
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
b while8.0.9.5.24.1
whileend8.0.9.5.24.1:
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
b while8.0.9.5.24
whileend8.0.9.5.24:
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// While
while8.0.9.5.26:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.5.26
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
b while8.0.9.5.26
whileend8.0.9.5.26:
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
b while8.0.9.5
whileend8.0.9.5:
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// While
while8.0.9.9:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.9
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// While
while8.0.9.9.1:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.9.1
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
b while8.0.9.9.1
whileend8.0.9.9.1:
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// While
while8.0.9.9.5:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.9.5
// While
while8.0.9.9.5.0:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.9.5.0
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
b while8.0.9.9.5.0
whileend8.0.9.9.5.0:
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
b while8.0.9.9.5
whileend8.0.9.9.5:
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// While
while8.0.9.9.14:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.9.14
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
b while8.0.9.9.14
whileend8.0.9.9.14:
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// While
while8.0.9.9.18:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.9.18
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
b while8.0.9.9.18
whileend8.0.9.9.18:
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// While
while8.0.9.9.21:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend8.0.9.9.21
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
b while8.0.9.9.21
whileend8.0.9.9.21:
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
b while8.0.9.9
whileend8.0.9.9:
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
b while8.0.9
whileend8.0.9:
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
b while8.0
whileend8.0:
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// Output
ldr x0, [sp], #0
ldrb w1, [x29, x0]
str x1, [sp, #-16]!
bl _output_char
ldr x0, [sp], #16
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
b while8
whileend8:
mov x0, #0
mov x16, #1
svc #0x80