.model small 
.stack 64
.data
player1Registers label byte
axPlayer1 dw 0
bxPlayer1 dw 0
cxPlayer1 dw 0
dxPlayer1 dw 0
siPlayer1 dw 0
diPlayer1 dw 0
spPlayer1 dw 0
bpPlayer1 dw 0
;player1Registers + 15
memoryPlayer1 db 16 dup(0)
offsetOperand1 dw ?
offsetOperand2 dw ?
operation     DB         'MOV [5],BX','$'
instruction   DB         5 DUP('$')
operands      DB         20 dup('$')
operand1     DB          5 DUP('$')
operand2     DB          5 DUP('$')  
sizeOfOperand1     DW      ?
sizeOfOperand2     DW      ?
sizeOfInstruction  DW      ? 
Operand db '[CX]'
hexOperand1 dw 0
hasError db 0h
hasBrack db 0h
isNum db 0h
isChar db 0h
operandsCount db ?
validAddressing db 0h
validMem db 0h
Size db 4h
AsciiToHexTable db 30h,31h,32h,33h,34h,35h,36h,37h,38h,39h,41h,42h,43h,44h,45h,46h
.code  
main proc far
mov ax,@data
mov ds,ax
mov es,ax

Call Seperate
mov al,3h
mov memoryPlayer1+5,al
mov bx, offset operand1
Call GetOperandType

hlt
main endp

;*********************
CheckBrackets proc
mov ch,0h
mov cl,Size
mov bx, offset operand1
CheckBracket:
cmp byte ptr[bx], 5Bh
jz HasBrackets 
inc bx ;Where to store results?
Loop CheckBracket
ret 
HasBrackets:
inc hasBrack
ret
CheckBrackets endp

;*********************
IsNumber proc
mov al,0h
mov isNum,al
cmp byte ptr[bx], 29h
ja CheckLessThan
ret
CheckLessThan:
cmp byte ptr[bx], 40h
jb Number:
ret
Number:
inc isNum
ret
IsNumber endp
;*********************           

IsCharacter proc

cmp byte ptr[bx], 40h
ja CheckBelow
ret
CheckBelow:
cmp byte ptr [bx], 5Bh
jb Character:
ret
Character:
inc isChar
ret
IsCharacter endp

ValidateAddressing proc
cmp [bx], 'SI'
je SiPointedContents ;Get contents of memory that si points to
cmp [bx], 'DI'
je DiPointedContents
cmp [bx], 'BX'
je BxPointedContents
;if not one of the previous it may be num
Call IsNumber
mov al,isNum
cmp al,1 
je ValidMemoryRange
ret
SiPointedContents:
mov di,[player1Registers+8] 
mov si,offset memoryPlayer1
Add di,si
mov offsetOperand1, di
inc validAddressing
ret
DiPointedContents:
mov di,[player1Registers+10]
mov si,offset memoryPlayer1
Add di,si
mov offsetOperand1, di
inc validAddressing
ret
BxPointedContents:
mov di,[player1Registers+2]
mov si,offset memoryPlayer1      
Add di,si
mov offsetOperand1, di
inc validAddressing
ret 
ValidMemoryRange:
;Modify size before calling
mov ax,sizeOfOperand1
sub ax,2
mov sizeOfOperand1,ax

call RemoveBrackets
call AsciiToHex
call ValidateMemRange
mov al, validMem
cmp al,1h
je MemoryContents
inc hasError
ret

MemoryContents:

mov bx,hexOperand1
mov cx,[bx]
Add cl,memoryPlayer1
mov offsetOperand1,cx
inc validAddressing
ret

ValidateAddressing endp

RemoveBrackets proc
mov ah,0
RmBrackets:
mov al,[bx]
cmp al,']'
je Removed
push ax
Loop RmBrackets
Removed:
ret
RemoveBrackets endp

AsciiToHex proc 
mov cx,sizeOfOperand1
mov si, offset operand1 
SearchDigits:
mov di,offset AsciiToHexTable
mov al,[si]
push cx
mov cx,10h
repne scasb
jne InvalidNum
mov dh,30h 
mov dl,10h
sub dl,cl
cmp dl,0Bh
jb FirstHalf
mov dh,37h
FirstHalf:
sub [si],dh
pop cx
inc si
loop SearchDigits
;Combine the digits
mov cx,sizeOfOperand1
mov si, offset operand1
mov dx,cx
dec dx
add si,dx
;mov dx,000Fh 
mov al,4h
mov dl,0h
PushDigits:
mov al,4h

mul dl 
push cx
mov cl,al
mov bl,[si]
mov bh,0h
shl bx,cl
inc dl
dec si
pop cx
push bx
Loop PushDigits


mov cx,sizeOfOperand1
mov di,0h
Combine:
pop ax
add di,ax
Loop Combine
mov hexOperand1,di
ret
InvalidNum:
inc hasError
ret
AsciiToHex endp

ValidateMemRange proc
mov ax,hexOperand1
cmp hexOperand1,0h
ja UpperRange
inc hasError
ret
UpperRange: 
cmp hexOperand1,0fh
jb ValidMemory
inc hasError
ret
ValidMemory:
inc validMem
ret
ValidateMemRange endp



;*****************************
;lea bx, operand1

GetOperandType proc
push bx
Call CheckBrackets
;Call ClearGarbage 
mov al,hasBrack
cmp al,1
je NumOrChar

;Immediate branching
;Register branching

NumOrChar:
pop bx
inc bx
Call IsCharacter
;Call ClearGarbage 
mov al,isChar
cmp al,1
je CheckAdMode ;Check Addressing mode
;if not char, it may be num
Call IsNumber
mov al,isNum
cmp al,1 
je CheckAdMode
inc hasError
ret

CheckAdMode: 
Call ValidateAddressing
mov al,validAddressing
cmp al,1
je Handled
inc hasError

Handled:
ret
GetOperandType endp
;****************************

ClearGarbage proc ;description

            XOR SI,SI
            XOR DI,DI
            XOR AX,AX
            XOR BX,BX
            XOR CX,CX
            XOR DX,DX

ClearGarbage ENDP

SaveOperands proc


ret
SaveOperands endp

Seperate proc
             ;------------------------------------------------------------------------------------------------------    
      ;-----here I seperated the operation into instruction+ operand1+operand2-------------
              XOR SI,SI
              XOR DI,DI
              XOR AX,AX
              XOR BX,BX
              XOR CX,CX
              XOR DX,DX

              MOV SI, OFFSET operation 
              MOV BP,SI
              MOV DI, OFFSET instruction
              MOV sizeOfInstruction,SI 
Prepare_instruction_Seperate:           ; here you are trying to check if you found space or not, if u don't, it will jump to instruction_seperate Loop
              CMP [SI],20h              ; 20h= ASCII code of SPACE
              JNE instruction_seperate
              
              SUB SI,BP                 ;from the beginning of this line to the next JMP is shuffling between registers and variables to the size of the instruction
              MOV sizeOfInstruction,SI
              MOV SI,BP  
              ADD SI,sizeOfInstruction
              JMP Prepare_operand_seperation
   
instruction_seperate:                   ;This loop is responsible for the copy the chars of instruction into instruction variable
              MOV Al,[SI]   
              MOV [DI],Al
              INC DI
              INC SI     
              JMP Prepare_instruction_Seperate

Prepare_operand_seperation: 
              MOV DI,OFFSET operands
              INC SI
               
   label1:  
              CMP [SI],24H              ;24h= ASCII code of $
              JNE operand_seperation
              JE prepare_seperation_first_operand

Operand_seperation:
              MOV Al,[SI]
              MOV [DI],Al
              INC DI
              INC SI
              JMP label1  
      
prepare_seperation_first_operand:   ;this loop is to seperate the operands into operand1 and operand2
               MOV SI,OFFSET operands
               MOV DI,OFFSET OPERAND1
               MOV BP,SI
               MOV sizeOfOperand1,SI
   
   label2:    CMP [SI],','
              JNZ seperate_first_operand
              
              SUB SI,BP                 ;from the beginning of this line to the next JMP is shuffling between registers and variables to the size of the instruction
              MOV sizeOfOperand1,SI
              MOV SI,BP  
              ADD SI,sizeOfOperand1
              JMP prepare_seperation_second_operand

seperate_first_operand: 
              MOV AL,[SI]   
              MOV [DI],AL
              INC DI
              INC SI     
              JMP label2  
                                 
                                 
prepare_seperation_second_operand: 
               INC SI
               MOV DI,OFFSET OPERAND2 
               MOV BP,SI
               MOV sizeOfOperand2,SI
               
   label3:     CMP [SI],'$'
               JNE seperate_second_operand
               
               SUB SI,BP                 ;from the beginning of this line to the next JMP is shuffling between registers and variables to the size of the instruction
               MOV sizeOfOperand2,SI
               MOV SI,BP  
               ADD SI,sizeOfOperand2 
               JMP FINISHED_SEPERATION

seperate_second_operand:
              MOV AL,[SI]   
              MOV [DI],AL
              INC DI
              INC SI     
              JMP label3           
              
              
FINISHED_SEPERATION:  ;now you have the instruction in instruction variable, also you have operand1 and operand2            
              
Seperate endp


CountOperands proc
mov si, offset operand1
cmp byte ptr[si],'$'
jne IncCount
CheckSecond:
mov si, offset operand2
cmp byte ptr[si],'$'
je exit 
inc operandsCount
IncCount:
inc operandsCount
jmp CheckSecond
exit:
ret

CountOperands endp

end main  
;*
