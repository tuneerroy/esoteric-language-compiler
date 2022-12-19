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
while4:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend4
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
b while4
whileend4:
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// While
while6:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend6
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
b while6
whileend6:
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
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// While
while10:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend10
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// While
while10.1:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend10.1
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
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
b while10.1
whileend10.1:
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
// While
while10.6:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend10.6
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
b while10.6
whileend10.6:
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
while10.10:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend10.10
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
b while10.10
whileend10.10:
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
// While
while10.14:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend10.14
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
b while10.14
whileend10.14:
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
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// While
while10.20:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend10.20
// While
while10.20.0:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend10.20.0
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
b while10.20.0
whileend10.20.0:
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
b while10.20
whileend10.20:
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
while10.24:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend10.24
// While
while10.24.0:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend10.24.0
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
b while10.24.0
whileend10.24.0:
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
// Output
ldr x0, [sp], #0
ldrb w1, [x29, x0]
str x1, [sp, #-16]!
bl _output_char
ldr x0, [sp], #16
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// While
while10.24.5:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend10.24.5
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
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
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
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
b while10.24.5
whileend10.24.5:
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
b while10.24
whileend10.24:
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// While
while10.27:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend10.27
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
// While
while10.27.5:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend10.27.5
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
while10.27.5.3:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend10.27.5.3
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
b while10.27.5.3
whileend10.27.5.3:
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
while10.27.5.14:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend10.27.5.14
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
b while10.27.5.14
whileend10.27.5.14:
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
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
// While
while10.27.5.25:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend10.27.5.25
// DecrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
sub x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// While
while10.27.5.25.1:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend10.27.5.25.1
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
b while10.27.5.25.1
whileend10.27.5.25.1:
// IncrByte
ldr x0, [sp], #0
ldrb w1, [x29, x0]
add x1, x1, #1
add x0, x0, x29
strb w1, [x0]
// While
while10.27.5.25.3:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend10.27.5.25.3
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
b while10.27.5.25.3
whileend10.27.5.25.3:
b while10.27.5.25
whileend10.27.5.25:
// DecrPtr
ldr x0, [sp], #16
sub x0, x0, #1
str x0, [sp, #-16]!
// While
while10.27.5.27:
ldr x0, [sp], #0
ldrb w1, [x29, x0]
cmp x1, #0
beq whileend10.27.5.27
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
b while10.27.5.27
whileend10.27.5.27:
// IncrPtr
ldr x0, [sp], #16
add x0, x0, #1
str x0, [sp, #-16]!
b while10.27.5
whileend10.27.5:
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
b while10.27
whileend10.27:
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
b while10
whileend10:
mov x0, #0
mov x16, #1
svc #0x80