.data
.balign 4
buf: .space 20, 0
.balign 4
heap: .space 10000000, 0
.text
.global _start
.balign 16
_divide:
sdiv x2, x0, x1
msub x3, x2, x1, x0
ret
_output_char:
mov x0, #1
adrp x1, buf@page
add x1, x1, buf@pageoff
ldr x8, [sp], #16
str x8, [x1]
mov x2, #1
mov x16, #4
svc #0x80
ret
_output_num:
ldr x0, [sp], #16
str x30, [sp, #-16]!
_int_to_ascii:
cmp x0, #0
bge _int_to_ascii_pos
mov x1, #45
str x1, [sp, #-16]!
mov x27, x0
bl _output_char
mov x0, x27
mov x27, #0
negs x0, x0
_int_to_ascii_pos:
cmp x0, #10
blt _int_to_ascii_br
mov x1, #10
bl _divide
mov x0, x2
add x3, x3, #48
str x3, [sp, #-16]!
add x27, x27, #1
bl _int_to_ascii_pos
_int_to_ascii_br:
add x8, x0, #48
str x8, [sp, #-16]!
add x27, x27, #1
_print_stack:
cmp x27, #0
ble _done_ascii
bl _output_char
sub x27, x27, #1
bl _print_stack
_done_ascii:
ldr x30, [sp], #16
ret
_slide:
ldr x1, [sp], #16
mov x29, x30
slide_loop:
cmp x0, #0
ble slide
ldr x28, [sp], #16
sub x0, x0, #1
bl slide_loop
slide:
str x1, [sp, #-16]!
mov x30, x29
ret
_input_num:
mov x21, #1
mov x22, x30
mov x0, #0
adrp x26, buf@page
add x26, x26, buf@pageoff
mov x1, x26
mov x2, #11
mov x16, #3
svc #0x80
mov x6, #0
ldrb w5, [x26, x6]
cmp x5, #45
bne _nonneg
add x26, x26, #1
sub x0, x0, #1
mov x21, #-1
_nonneg:
sub x0, x0, #1
mov x25, x0
mov x24, x0
mov x23, #0
ldrb w1, [x26, x25]
cmp x1, #10
bne _no_nl
sub x25, x25, #1
_no_nl:
cmp x25, #0
blt _calculate_number
ldrb w0, [x26, x25]
sub x0, x0, #48
str x0, [sp, #-16]!
sub x25, x25, #1
bl _no_nl
_calculate_number:
cmp x24, #0
beq _done_calculation
ldr x0, [sp], #16
mov x1, #10
madd x23, x23, x1, x0
sub x24, x24, #1
bl _calculate_number
_done_calculation:
mul x23, x23, x21
ldr x1, [sp], #16
adrp x2, heap@page
add x2, x2, heap@pageoff
mov x3, #8
mul x1, x1, x3
str x23, [x2, x1]
mov x30, x22
ret
_start:
// push
mov x0, #43
str x0, [sp, #-16]!
// push
mov x0, #6
str x0, [sp, #-16]!
// push
mov x0, #75
str x0, [sp, #-16]!
// swap
ldr x0, [sp], #16
ldr x1, [sp], #16
str x0, [sp, #-16]!
str x1, [sp, #-16]!
// push
mov x0, #83
str x0, [sp, #-16]!
// push
mov x0, #38
str x0, [sp, #-16]!
// outputNum
bl _output_num
// push
mov x0, #15
str x0, [sp, #-16]!
// push
mov x0, #88
str x0, [sp, #-16]!
// store
ldr x0, [sp], #16
ldr x1, [sp], #16
mov x3, #8
adrp x2, heap@page
add x2, x2, heap@pageoff
mul x1, x1, x3
str x0, [x2, x1]
// push
mov x0, #33
str x0, [sp, #-16]!
// push
mov x0, #85
str x0, [sp, #-16]!
// outputNum
bl _output_num
// outputNum
bl _output_num
// store
ldr x0, [sp], #16
ldr x1, [sp], #16
mov x3, #8
adrp x2, heap@page
add x2, x2, heap@pageoff
mul x1, x1, x3
str x0, [x2, x1]
// retrieve
ldr x1, [sp], #16
mov x3, #8
mul x1, x1, x3
adrp x2, heap@page
add x2, x2, heap@pageoff
ldr x0, [x2, x1]
str x0, [sp, #-16]!
// retrieve
ldr x1, [sp], #16
mov x3, #8
mul x1, x1, x3
adrp x2, heap@page
add x2, x2, heap@pageoff
ldr x0, [x2, x1]
str x0, [sp, #-16]!
// push
mov x0, #60
str x0, [sp, #-16]!
// push
mov x0, #66
str x0, [sp, #-16]!
// outputNum
bl _output_num
// swap
ldr x0, [sp], #16
ldr x1, [sp], #16
str x0, [sp, #-16]!
str x1, [sp, #-16]!
// outputNum
bl _output_num
// push
mov x0, #72
str x0, [sp, #-16]!
// outputNum
bl _output_num
// push
mov x0, #25
str x0, [sp, #-16]!
// push
mov x0, #67
str x0, [sp, #-16]!
// sub
ldr x0, [sp], #16
ldr x1, [sp], #16
sub x2, x1, x0
str x2, [sp, #-16]!
// outputNum
bl _output_num
// push
mov x0, #28
str x0, [sp, #-16]!
// dup
ldr x0, [sp], #16
str x0, [sp, #-16]!
str x0, [sp, #-16]!
// outputNum
bl _output_num
// outputNum
bl _output_num
// discard
ldr x0, [sp], #16
// outputNum
bl _output_num
// push
mov x0, #9
str x0, [sp, #-16]!
// push
mov x0, #92
str x0, [sp, #-16]!
// push
mov x0, #5
str x0, [sp, #-16]!
// push
mov x0, #2
str x0, [sp, #-16]!
// outputNum
bl _output_num
// outputNum
bl _output_num
// push
mov x0, #70
str x0, [sp, #-16]!
// push
mov x0, #74
str x0, [sp, #-16]!
// push
mov x0, #58
str x0, [sp, #-16]!
// store
ldr x0, [sp], #16
ldr x1, [sp], #16
mov x3, #8
adrp x2, heap@page
add x2, x2, heap@pageoff
mul x1, x1, x3
str x0, [x2, x1]
// outputNum
bl _output_num
// push
mov x0, #64
str x0, [sp, #-16]!
// push
mov x0, #41
str x0, [sp, #-16]!
// push
mov x0, #28
str x0, [sp, #-16]!
// push
mov x0, #91
str x0, [sp, #-16]!
// push
mov x0, #56
str x0, [sp, #-16]!
// retrieve
ldr x1, [sp], #16
mov x3, #8
mul x1, x1, x3
adrp x2, heap@page
add x2, x2, heap@pageoff
ldr x0, [x2, x1]
str x0, [sp, #-16]!
// outputNum
bl _output_num
// push
mov x0, #40
str x0, [sp, #-16]!
// push
mov x0, #43
str x0, [sp, #-16]!
// outputNum
bl _output_num
// outputNum
bl _output_num
// swap
ldr x0, [sp], #16
ldr x1, [sp], #16
str x0, [sp, #-16]!
str x1, [sp, #-16]!
// outputNum
bl _output_num
// push
mov x0, #80
str x0, [sp, #-16]!
// push
mov x0, #48
str x0, [sp, #-16]!
// push
mov x0, #46
str x0, [sp, #-16]!
// push
mov x0, #99
str x0, [sp, #-16]!
// push
mov x0, #30
str x0, [sp, #-16]!
// slide
mov x0, #3
bl _slide
// push
mov x0, #91
str x0, [sp, #-16]!
// push
mov x0, #85
str x0, [sp, #-16]!
// push
mov x0, #55
str x0, [sp, #-16]!
// push
mov x0, #88
str x0, [sp, #-16]!
// push
mov x0, #46
str x0, [sp, #-16]!
// outputNum
bl _output_num
// store
ldr x0, [sp], #16
ldr x1, [sp], #16
mov x3, #8
adrp x2, heap@page
add x2, x2, heap@pageoff
mul x1, x1, x3
str x0, [x2, x1]
// push
mov x0, #6
str x0, [sp, #-16]!
// push
mov x0, #23
str x0, [sp, #-16]!
// push
mov x0, #41
str x0, [sp, #-16]!
// copy
ldr x0, [sp, #0]
str x0, [sp, #-16]!
// outputNum
bl _output_num
// slide
mov x0, #0
bl _slide
// push
mov x0, #85
str x0, [sp, #-16]!
// push
mov x0, #66
str x0, [sp, #-16]!
// outputNum
bl _output_num
// store
ldr x0, [sp], #16
ldr x1, [sp], #16
mov x3, #8
adrp x2, heap@page
add x2, x2, heap@pageoff
mul x1, x1, x3
str x0, [x2, x1]
// push
mov x0, #61
str x0, [sp, #-16]!
// outputNum
bl _output_num
// push
mov x0, #91
str x0, [sp, #-16]!
// push
mov x0, #40
str x0, [sp, #-16]!
// retrieve
ldr x1, [sp], #16
mov x3, #8
mul x1, x1, x3
adrp x2, heap@page
add x2, x2, heap@pageoff
ldr x0, [x2, x1]
str x0, [sp, #-16]!
// push
mov x0, #76
str x0, [sp, #-16]!
// retrieve
ldr x1, [sp], #16
mov x3, #8
mul x1, x1, x3
adrp x2, heap@page
add x2, x2, heap@pageoff
ldr x0, [x2, x1]
str x0, [sp, #-16]!
// push
mov x0, #17
str x0, [sp, #-16]!
// store
ldr x0, [sp], #16
ldr x1, [sp], #16
mov x3, #8
adrp x2, heap@page
add x2, x2, heap@pageoff
mul x1, x1, x3
str x0, [x2, x1]
// copy
ldr x0, [sp, #16]
str x0, [sp, #-16]!
// push
mov x0, #50
str x0, [sp, #-16]!
// push
mov x0, #78
str x0, [sp, #-16]!
// outputNum
bl _output_num
// retrieve
ldr x1, [sp], #16
mov x3, #8
mul x1, x1, x3
adrp x2, heap@page
add x2, x2, heap@pageoff
ldr x0, [x2, x1]
str x0, [sp, #-16]!
// outputNum
bl _output_num
// retrieve
ldr x1, [sp], #16
mov x3, #8
mul x1, x1, x3
adrp x2, heap@page
add x2, x2, heap@pageoff
ldr x0, [x2, x1]
str x0, [sp, #-16]!
// push
mov x0, #1
str x0, [sp, #-16]!
// push
mov x0, #47
str x0, [sp, #-16]!
// push
mov x0, #65
str x0, [sp, #-16]!
// push
mov x0, #0
str x0, [sp, #-16]!
// outputNum
bl _output_num
// outputNum
bl _output_num
// outputNum
bl _output_num
// outputNum
bl _output_num
// push
mov x0, #66
str x0, [sp, #-16]!
// outputNum
bl _output_num
// outputNum
bl _output_num
// push
mov x0, #67
str x0, [sp, #-16]!
// push
mov x0, #36
str x0, [sp, #-16]!
// outputNum
bl _output_num
// store
ldr x0, [sp], #16
ldr x1, [sp], #16
mov x3, #8
adrp x2, heap@page
add x2, x2, heap@pageoff
mul x1, x1, x3
str x0, [x2, x1]
// store
ldr x0, [sp], #16
ldr x1, [sp], #16
mov x3, #8
adrp x2, heap@page
add x2, x2, heap@pageoff
mul x1, x1, x3
str x0, [x2, x1]
// push
mov x0, #74
str x0, [sp, #-16]!
// push
mov x0, #93
str x0, [sp, #-16]!
// store
ldr x0, [sp], #16
ldr x1, [sp], #16
mov x3, #8
adrp x2, heap@page
add x2, x2, heap@pageoff
mul x1, x1, x3
str x0, [x2, x1]
// outputNum
bl _output_num
// store
ldr x0, [sp], #16
ldr x1, [sp], #16
mov x3, #8
adrp x2, heap@page
add x2, x2, heap@pageoff
mul x1, x1, x3
str x0, [x2, x1]
// push
mov x0, #44
str x0, [sp, #-16]!
// push
mov x0, #54
str x0, [sp, #-16]!
// outputNum
bl _output_num
// push
mov x0, #2
str x0, [sp, #-16]!
// push
mov x0, #79
str x0, [sp, #-16]!
// outputNum
bl _output_num
// push
mov x0, #94
str x0, [sp, #-16]!
// store
ldr x0, [sp], #16
ldr x1, [sp], #16
mov x3, #8
adrp x2, heap@page
add x2, x2, heap@pageoff
mul x1, x1, x3
str x0, [x2, x1]
// outputNum
bl _output_num
// outputNum
bl _output_num
// push
mov x0, #21
str x0, [sp, #-16]!
// push
mov x0, #99
str x0, [sp, #-16]!
// push
mov x0, #5
str x0, [sp, #-16]!
// push
mov x0, #74
str x0, [sp, #-16]!
// retrieve
ldr x1, [sp], #16
mov x3, #8
mul x1, x1, x3
adrp x2, heap@page
add x2, x2, heap@pageoff
ldr x0, [x2, x1]
str x0, [sp, #-16]!
// outputNum
bl _output_num
// push
mov x0, #0
str x0, [sp, #-16]!
// push
mov x0, #15
str x0, [sp, #-16]!
// push
mov x0, #27
str x0, [sp, #-16]!
// outputNum
bl _output_num
// push
mov x0, #6
str x0, [sp, #-16]!
// end
b end
end:
mov x0, #0
mov x16, #1
svc #0x80