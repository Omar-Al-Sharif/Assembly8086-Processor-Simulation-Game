
player1Registers label byte
axPlayer1 dw 0
bxPlayer1 dw 0
cxPlayer1 dw 0
dxPlayer1 dw 0
siPlayer1 dw 0
diPlayer1 dw 0
spPlayer1 dw 0
bpPlayer1 dw 0
memoryPlayer1 db 16 dup(0)
operation     DB         'MOV [AX],BX','$'
instruction   DB         5 DUP('$')
operands      DB         20 dup('$')
operand1     DB          2 DUP('$')
operand2     DB          2 DUP('$') 
sizeOfInstruction  DW      ? 
Operand db '[CX]'
hasError db 0h
hasBrack db 0h
isNum db 0h
isChar db 0h
Counter db ?
validAddressing db 0h

player1Name db 11,?, 11 dup('$')

player2Name db 11,?, 11 dup('$')

player1Score db ?

player2Score db ?

instruction db 30,?, 30 dup('$')


