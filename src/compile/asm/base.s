.intel_syntax noprefix

.global main
.global _main

.section .text

main:
    call _main
    mov rdi, rax  # move the return value into the first argument for the syscall
    mov rax, 0x3C # move the exit syscall number into rax
    syscall

_main:
# Start of generated assembly
# TODO for now just return always 0
mov rax, 0
ret
