# This program adds two numbers taken from
# user and prints it on console
# Run this program with QtSpim

.data				# Text section
.align 2
read_msg: 	.asciiz "Enter number: "
print_msg: 	.asciiz	"Result: "
print_exit:	.asciiz "\nThank you!"
str_const0:
	.word	0
	.word	6
	.word	
#	.word	int_const1
	.ascii	"Main"
	.byte	0	
	.align	2
	.word	-1
	
int_const0:
	.word	1

.text					# Code section
.globl main
.globl exit
.globl read_num
.globl print_num

main:
	jal		read_num	# Read number 1
	move 	$t0, $v0	
	jal		read_num	# Read number 2
	move	$t1, $v0
	addu	$t2, $t0, $t1	# Add number
	jal 	print_num		# Print number
	jal exit 				# Exit program

exit:					# Procedure to exit program
	li $v0, 4
	la $a0, print_exit	# Print exit message
	syscall
	li $v0, 10			# System call for exit
	syscall

read_num:				# Start of read_num procedure
	li $v0, 4
	la $a0, read_msg
	syscall
	li $v0, 5			# System call for reading integet
	syscall
	jr $ra 				# Return from this procedure

print_num:				# Start of print_num procedure
	li $v0, 4			# System call for printing string
	la $a0, print_msg	# Loading string address to be printed
	syscall
	move $a0, $t2		# Number to be printed is always passed in t2
	li $v0, 1			# System call for printing integer
	syscall
	jr $ra 				# Return from this procedure
