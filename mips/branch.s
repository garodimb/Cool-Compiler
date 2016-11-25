.data

msg0: .asciiz "This is l0\n"
msg1: .asciiz "This is l1\n"

.text
.globl main

main:
	b loc0
	loc0:
		la $a0, msg0
		li $v0, 4
		syscall
		j end
	loc1:
		la $a0, msg1
		li $v0, 4
		syscall
		j end
	end:	
		li $v0, 10
		syscall
	
	
	
