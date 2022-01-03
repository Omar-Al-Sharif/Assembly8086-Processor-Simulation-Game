.model Large 
.stack 64
.data
all_Registers_stringPlayer1   DB  "AXBXCXDXSIDISPBPALAHBLBHCLCHDLDH"  ;this is a string that contain all the registers, this string is helpful in detecting the exsistance of registers
player1Registers label byte
axPlayer1 dw 0
bxPlayer1 dw 07h
cxPlayer1 dw 09h
dxPlayer1 dw 30h
siPlayer1 dw 0
diPlayer1 dw 0
spPlayer1 dw 0Fh
bpPlayer1 dw 0
;player1Registers + 15
memoryPlayer1 db 16 dup(0)
;ORG 20
   operand2Immediate  dw   ?
   operand1Immediate  dw   ?
   
   isOperand2Immediate  dw   ?
   isOperand1Immediate  dw   ?
offsetOperand1 dw ?
offsetOperand2 dw ?
offsetOperandN dw ?
operation     DB         'ADD DX,CX','$'
instruction   DB         5 DUP('$')
operands      DB         20 dup('$')
operand1     DB          5 DUP('$')
operand2     DB          5 DUP('$')  
operandN               DB     5 DUP('$')
ptrOperandN            DB     5 DUP('$')
emptyString            DB     5 DUP('$')
sizeOfOperand1     DW      ?
sizeOfOperand2     DW      ?
sizeOfOperandN         DW        ?
sizeOfInstruction  DW      ? 
Operand db '[CX]'
hexOperand1 dw 0
hexOperand2    dw 0 
hexOperandN    dw 0 
 
hasError db 0h
hasBrack db 0h
isNum db 0h
isChar db 0h
operandsCount db ?
validAddressing db 0h
validMem db 0h
isOperand1Register     DB        ?
   isOperand2Register     DB        ?
   isOperand1HasOffset    DB        ?
   isOperand2HasOffset    DB        0
   isOperandNHasOffset    DB        0
   isOperand18Bits        DB        ?
   isOperand28Bits        DB        ?
   isOperandN8Bits        DB        ?
   Counter                dw      0h
   isFirstCharLetterOperand1      DB            0
   isFirstCharLetterOperand2      DB            0
   isFirstCharNumberOperand1      DB            0
   isFirstCharNumberOperand2      DB            0
   isFirstCharLetterOperandN      DB            0
   isFirstCharNumberOperandN      DB            0
 CarryFlag                      DB            0
 
Size db 4h
AsciiToHexTable db 30h,31h,32h,33h,34h,35h,36h,37h,38h,39h,41h,42h,43h,44h,45h,46h

instructionFound    db   0h     ;This is a boolean variable
instructionOpcode   dw   ?

movInst  db 'MOV'   
addInst  db 'ADD'
adcInst  db 'ADC'
subInst  db 'SUB'
sbbInst  db 'SBB'
mulInst  db 'MUL'
;imulInst  db 'IMUL'
xorInst  db 'XOR'
andInst  db 'AND'
orInst   db 'OR'
notInst  db 'NOT'
clcInst  db 'CLC'
pushInst db 'PUSH'
popInst  db 'POP'
incInst  db 'INC'
decInst  db 'DEC'
nopInst  db 'NOP'
divInst  db 'DIV'
shlInst  db 'SHL'
shrInst  db 'SHR'
negInst  db 'NEG'
.code 


;------------------------------------------------------------------------------------------
;----------------------------------------MACROS--------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------

LoadString MACRO OriginalString,CopiedString,SizeOfOriginal,SizeOfCopied 

     
     
     MOV SI,OFFSET OriginalString 
     MOV DI,OFFSET CopiedString
     MOV CX,SizeOfOriginal
     
     REPE MOVSB 
     Push BX
     
     MOV BX,SizeOfOriginal
     MOV SizeOfCopied,BX
     pop bx
                                            
ENDM 

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------

COMPARE MACRO instruction,avaliable_instruction,sizeOfInstruction,opcode
     Local FOUND,UNFOUND,DONE1
     
     MOV SI,OFFSET avaliable_instruction
     MOV DI,OFFSET instruction
     MOV CX,sizeOfInstruction
     REPE CMPSB 
    
     CMP CX,0
     JE FOUND
     JNE UNFOUND

  FOUND: MOV AX,opcode   
         MOV instructionOpcode,AX
         MOV instructionFound,1
         JMP DONE1
         
  UNFOUND:MOV instructionFound,0 
    
  DONE1:
  
ENDM 




main proc far

mov ax,@data
mov ds,ax
mov es,ax
 
mov al,3h
mov memoryPlayer1+5,al
call OperandsProc
call ExecuteInstruction



hlt
main endp      

AssignInstructionOpcode proc
  XOR AX,AX
  XOR BX,BX
  XOR CX,CX
  XOR DX,DX 
  
  
  COMPARE instruction,movInst,sizeOfInstruction,00h
  CMP instructionFound,1
  JE  DONE2 
         
  COMPARE instruction,addInst,sizeOfInstruction,01h
  CMP instructionFound,1
  JE  DONE2  
  
  COMPARE instruction,adcInst,sizeOfInstruction,02h
  CMP instructionFound,1
  JE  DONE2  
  
  COMPARE instruction,subInst,sizeOfInstruction,03h
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,sbbInst,sizeOfInstruction,04h
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,mulInst,sizeOfInstruction,05h
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,andInst,sizeOfInstruction,06h
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,orInst,sizeOfInstruction,07h
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,xorInst,sizeOfInstruction,08h
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,divInst,sizeOfInstruction,09h
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,pushInst,sizeOfInstruction,0Ah
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,popInst,sizeOfInstruction,0Bh
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,incInst,sizeOfInstruction,0Ch
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,decInst,sizeOfInstruction,0Dh
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,nopInst,sizeOfInstruction,0Eh
  CMP instructionFound,1
  JE  DONE2
  
  COMPARE instruction,clcInst,sizeOfInstruction,0Fh
  CMP instructionFound,1
  JE  DONE2
  
  MOV hasError,1
    
  DONE2:
  ret
AssignInstructionOpcode endp
         
CountOperands proc
mov si, offset operand1
cmp byte ptr[si],'$'
jne IncCount
CheckSecond:
mov si, offset operand2
cmp byte ptr[si],'$'
je exit 
inc operandsCount
jmp exit
IncCount:
inc operandsCount
jmp CheckSecond
exit:
ret

CountOperands endp
         
      
AsciiToHex proc 
  mov cx,sizeOfOperandN
  mov si, bx 
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
  mov cx,sizeOfOperandN
  mov si, bx
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
  mov cx,sizeOfOperandN
  mov di,0h
Combine:
  pop ax
  add di,ax
  Loop Combine
  mov si,hexOperandN
  mov [si],di
ret
  InvalidNum:
  inc hasError
ret
AsciiToHex endp 

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
              JNZ Check_Dollar_Sign 
              jz CalculateSize
              
              Check_Dollar_Sign:
              CMP [SI],'$' 
              jz CalculateSize
              JNZ seperate_first_operand
              
              CalculateSize:
              SUB SI,BP                 ;from the beginning of this line to the next JMP is shuffling between registers and variables to the size of the instruction
              MOV sizeOfOperand1,SI
              MOV SI,BP  
              ADD SI,sizeOfOperand1
              
              CMP [SI],'$'
              JZ FINISHED_SEPERATION
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
ret           
              
Seperate endp




LoadOffsetNStatus proc
cmp cx,1h
je MoveHasOffset2
mov al,IsOperandNHasOffset
mov IsOperand2HasOffset,al
ret
MoveHasOffset2:
mov al,IsOperandNHasOffset
mov IsOperand1HasOffset,al    
ret    
LoadOffsetNStatus endp    


;*********************
CheckBrackets proc

mov cx,sizeOfOperandN
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
mov si,offsetOperandN
mov [si], di ;move offset operandN to offset operand1
inc isOperandNHasOffset
inc validAddressing
ret
DiPointedContents:
mov di,[player1Registers+10]
mov si,offset memoryPlayer1
Add di,si
mov si,offsetOperandN
mov [si], di ;move offset operandN to offset operand1
inc isOperandNHasOffset
inc validAddressing
ret
BxPointedContents:
mov di,[player1Registers+2]
mov si,offset memoryPlayer1      
Add di,si
mov si,offsetOperandN
mov [si], di ;move offset operandN to offset operand1
inc isOperandNHasOffset
inc validAddressing
ret 
ValidMemoryRange:
;Modify size before calling
mov ax,sizeOfOperandN
sub ax,2
mov sizeOfOperandN,ax
push bx
call RemoveBrackets
pop bx
push bx
call AsciiToHex
pop bx
call ValidateMemRange
mov al, validMem
cmp al,1h
je MemoryContents
inc hasError
ret

MemoryContents:

mov bx,hexOperandN
mov cx,[bx]
mov si, offset memoryPlayer1
Add cx,si  
mov si, offsetOperandN
mov [si],cx
inc isOperandNHasOffset
inc validAddressing
ret

ValidateAddressing endp

RemoveBrackets proc
mov cx,sizeOfOperandN
mov ah,0
mov si,bx
RmBrackets:
mov al,[bx]
cmp al,']'
je PopStackToOperand
push ax
inc bx
Loop RmBrackets
mov cx, sizeOfOperandN
Add cx,2h
mov bx,si
Call ClearString 
mov si,bx
dec si
mov cx,sizeOfOperandN
PopStackToOperand:
pop bx
xchg bh,bl
mov [si],bx
dec si
Loop PopStackToOperand
ret
RemoveBrackets endp

ClearString proc
mov cx,Counter
cmp cx,1h
je ClearOperand1
LoadString emptyString,operand2,5,5
ret
ClearOperand1:
LoadString emptyString,operand1,5,5
ret
ClearString endp


AssignOperandN proc
cmp cx,1h
je AssignOperandOne
AssignOperandTwo:
mov bx, offset operand2
;mov operandN, byte ptr [si]
mov si,sizeOfOperand2
mov sizeOfOperandN,si
mov si,offset hexOperand2
mov hexOperandN,si
mov si, offset offsetOperand2
mov offsetOperandN,si
ret
AssignOperandOne:
mov bx, offset operand1
;mov operandN,[si]
mov si,sizeOfOperand1
mov sizeOfOperandN,si
mov si,offset hexOperand1
mov hexOperandN,si
mov si, offset offsetOperand1
mov offsetOperandN,si
ret
AssignOperandN endp

ValidateMemRange proc
mov si,hexOperandN
mov ax,[si]
cmp ax,0h
ja UpperRange
inc hasError
ret
UpperRange: 
cmp ax,0fh
jb ValidMemory
inc hasError
ret
ValidMemory:
inc validMem
ret
ValidateMemRange endp

;Integrated Function
OperandsProc proc
call Seperate
call AssignInstructionOpcode
call CountOperands
mov cl,operandsCount
cmp cl,0h
je NoOperands
call GetOperandType
NoOperands:
ret
OperandsProc endp

;---------------------

DirectOrIndirect proc

mov ch,0h
mov cl, operandsCount

LoopAllOperands:
mov Counter,cx
push cx
call AssignOperandN
push bx
Call CheckBrackets 
mov al,hasBrack
cmp al,1
je NumOrChar
pop bx
jmp Handled
NumOrChar:
pop bx
inc bx
Call IsCharacter 
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
pop cx
ret
Handled:
pop cx 

call LoadOffsetNStatus
Loop LoopAllOperands
ret
DirectOrIndirect endp


;*****************************

GetOperandType proc

call DirectOrIndirect
;**************
;Immediate branching
;Register branching

Call RegisterOrImmediate

;***************
ret
GetOperandType endp
;****************************

ClearGarbage proc 

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





;------------------------------------------------------------------------------------------
;--------------------------------------PROCUDURES------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------

RegisterOrImmediate proc
     
               
     xor ax,ax
     xor bx,bx
     xor cx,cx
     xor dx,dx
     xor si,si
     xor di,di        
     ;----------------------------    
     CMP operandsCount,1
     JE Check_Modes_Of_Operand1
     JG Check_Modes_Of_Operand2    


 Check_Modes_Of_Operand2:
          
           CMP hasError,1
           je GiveErrorToPlayer 
           
           CMP isOperand2HasOffset,1
           JE  Check_Modes_Of_Operand1
          
           MOV isOperandNHasOffset,0
           LoadString emptyString,operandN,5,sizeOfOperandN
           LoadString operand2,operandN,sizeOfOperand2,sizeOfOperandN
           
           CALL Check_First_char
           CMP hasError,1 
           je GiveErrorToPlayer 
           
        
           MOV  AL,isFirstCharLetterOperandN
           MOV  isFirstCharLetterOperand2,AL
           MOV  AL,isFirstCharLetterOperandN
           MOV  isFirstCharLetterOperand2,AL 
           
           
           CMP isFirstCharLetterOperand2,1
           JE  Call_register_addressing_function2
           JMP Call_Immediate_addressing_function2
           
           
     Call_register_addressing_function2:
           CALL Check_Register_Addressing
           CMP hasError,1
           je GiveErrorToPlayer  
           MOV isOperand2Register,1
           MOV AX,offsetOperandN
           MOV offsetOperand2,AX
           MOV Bl,isOperandN8Bits
           MOV isOperand28Bits,Bl
           jmp Check_Modes_Of_Operand1
            
           
     Call_Immediate_addressing_function2:
           CALL AsciiToHex
           mov si,hexOperandN 
           MOV AX,[si]           
           CMP ax,0FFFFh
           JLE  The_operand_is_immediate
           MOV hasError,1       
           JMP GiveErrorToPlayer
                 
           
     The_operand_is_immediate:
                mov si,hexOperandN 
                MOV AX,[si]
                MOV Operand2Immediate,AX
                MOV AX, OFFSET Operand2Immediate                   
                MOV offsetOperand2,AX
                MOV isOperand2Immediate,1
                           
          
           ;CHECK FIRST CHAR
           ;if it is letter --> call the Register_Addressing_Function
           ;if it can't find offset-->give error
           ;if it is number ---> call the Imidate_Addressing_function              

 Check_Modes_Of_Operand1:
           CMP hasError,1
           je GiveErrorToPlayer 
           
           CMP isOperand1HasOffset,1
           JE  Finsihed_checking_for_Addressing_mode
           
           MOV isOperandNHasOffset,1
           LoadString emptyString,operandN,5,sizeOfOperandN
           LoadString operand1,operandN,sizeOfOperand1,sizeOfOperandN
           
           CALL Check_First_char
           CMP hasError,1
           je GiveErrorToPlayer 
           MOV  AL,isFirstCharLetterOperandN
           MOV  isFirstCharLetterOperand1,AL
           MOV  AL,isFirstCharLetterOperandN
           MOV  isFirstCharLetterOperand1,AL
           
           
           
           CMP isFirstCharLetterOperand1,1
           JE  Call_register_addressing_function1
           JMP Call_Immediate_addressing_function1
           
           
           Call_register_addressing_function1: 
                 CALL Check_Register_Addressing
                 CMP hasError,1
                 je GiveErrorToPlayer 
                 MOV isOperand1Register,1
                 MOV AX,offsetOperandN
                 MOV offsetOperand1,AX
                 MOV Bl,isOperandN8Bits
                 MOV isOperand18Bits,Bl
                 JMP Finsihed_checking_for_Addressing_mode
                 
     Call_Immediate_addressing_function1:
           CALL AsciiToHex 
           mov si,hexOperandN 
           MOV AX,[si]
           CMP AX,0FFFFh
           JLE The_operand_is_immediate
           MOV hasError,1       
           JMP GiveErrorToPlayer
           
           
           
           
           
     The_operand1_is_immediate: 
                mov si,hexOperandN 
                MOV AX,[si]
                MOV Operand1Immediate,AX
                MOV AX, OFFSET Operand1Immediate                   
                MOV offsetOperand1,AX
                MOV isOperand1Immediate,1
      
                
                
               
            ;CHECK FIRST CHAR
           ;if it is letter --> call the Register_Addressing_Function
              ;if it can't find offset-->give error
           ;if it is number ---> call the Imidate_Addressing_function
              ;give error 
          
 GiveErrorToPlayer: 
                             ;CALL ERROR FUNCTION
              ret             ;JMP END TURN

 Finsihed_checking_for_Addressing_mode: 
                  CMP operandsCount,2
                  JE Check_Register_mismatch
                  ret
                                         
        Check_Register_mismatch: CMP isOperand1Register,1
                                 JE Check_operand2_for_mismatch                                         
                                 ret
                   
        Check_operand2_for_mismatch: CMP isOperand2Register,1
                                     JE  CHECK_TYPE_OF_REGISTERS
                                     ret
                                
        CHECK_TYPE_OF_REGISTERS:     MOV AL,isOperand28Bits
                                     CMP isOperand18Bits,AL
                                     CMP AL,0H
                                     JNE  ERROR_MISMATCH
                                     JE FINISHEDCHECK
                                     
                                            
        ERROR_MISMATCH: MOV hasError,1
                        je GiveErrorToPlayer                                                        
        
        FINISHEDCHECK: 
        ret 
 RegisterOrImmediate endp




Check_First_char PROC NEAR 
     
     PUSH SI
     PUSH AX
     MOV SI,OFFSET OperandN 
     
     MOV AL,[SI]
  check_if_letter:  CMP AL,'A'  ;IF GREATER THAN OR A GO TO CHECK IF LESS THAN Z
                   JE First_char_letter
                   JG Check_less_than_or_equal_Z
                   JL  Check_if_number
    
    
  Check_less_than_or_equal_Z: CMP AL,'Z'
                             JLE First_char_letter
                             JMP Make_hasError_equal_1
 
  Check_if_number:  CMP AL,'0'
                   JE First_char_number
                   JG Check_less_than_or_equal_9
                   JL  Make_hasError_equal_1
                   
  Check_less_than_or_equal_9: CMP AL,'9'
                             JLE First_char_number
                             JMP Make_hasError_equal_1
                                
  First_char_number: MOV isFirstCharNumberOperandN,1  
                    JMP FirstCharIsKnown
 
  First_char_letter: MOV isFirstCharLetterOperandN,1
                    JMP FirstCharIsKnown
                   
  Make_hasError_equal_1: MOV hasError,1
 
  FirstCharIsKnown:    
     POP AX
     POP SI
     
RET    
Check_First_char ENDP
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------


Check_Register_Addressing  PROC NEAR
       
       XOR AX,AX
       XOR BX,BX
       XOR CX,CX
       XOR DX,DX
       
       
       
       CMP sizeOfOperandN,2
       JNE This_isnt_register  
       MOV DI,OFFSET all_Registers_stringPlayer1
       MOV AL,operandN
       MOV AH, operandN[1]
       MOV CX,16
       REPNE SCASW 
       
       CMP CX,8
       JL  It_is_8bits
       
       MOV isOperandN8bits,0
       
       CMP CX,15
       JNE NEXT14
       
       MOV AX,OFFSET axPlayer1
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
       
     
    
NEXT14:
       CMP CX,14
       JNE NEXT13
        
       MOV AX,OFFSET bxPlayer1
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
       
       
NEXT13:       
       CMP CX,13
       JNE NEXT12
        
       MOV AX,OFFSET cxPlayer1
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
       
       
NEXT12: 
       CMP CX,12
       JNE NEXT11
        
       MOV AX,OFFSET dxPlayer1
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
       
       
NEXT11:
       CMP CX,11
       JNE NEXT10
        
       MOV AX,OFFSET siPlayer1
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
NEXT10:
       CMP CX,10
       JNE NEXT9
        
       MOV AX,OFFSET diPlayer1
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
         
         
NEXT9:
       CMP CX,9
       JNE NEXT8
        
       MOV AX,OFFSET spPlayer1
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
       

NEXT8:
       CMP CX,8
       JNE It_is_8bits
        
       MOV AX,OFFSET bpPlayer1
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
It_is_8bits:       
       
       MOV isOperandN8bits,1
       
       CMP CX,7
       JNE NEXT6
       
       MOV AX,OFFSET axPlayer1
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
       
     
    
NEXT6:
        CMP CX,6
        JNE NEXT5
        
       MOV AX,OFFSET axPlayer1[1]
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
       
       
NEXT5:       
       CMP CX,5
       JNE NEXT4
        
       MOV AX,OFFSET axPlayer1[2]
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
       
       
NEXT4: 
       CMP CX,4
       JNE NEXT3
        
       MOV AX,OFFSET axPlayer1[3]
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
       
       
NEXT3:
       CMP CX,3
       JNE NEXT2
        
       MOV AX,OFFSET axPlayer1[4]
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
NEXT2:
       CMP CX,2
       JNE NEXT1
        
       MOV AX,OFFSET axPlayer1[5]
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       JMP OffsetIsFound
       
         
         
NEXT1:
       CMP CX,1
       JNE NEXT0
        
       MOV AX,OFFSET axPlayer1[6]
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
        
NEXT0: 
       CMP CX,1
       JNE This_isnt_register
        
       MOV AX,OFFSET axPlayer1[7]
       MOV offsetOperandN,AX
       MOV isOperandNHasOffset,1
       
       

OffsetIsFound: JMP Register_Addressing_mode_is_finished     
       
       
This_isnt_register: MOV hasError,1    
       
       

Register_Addressing_mode_is_finished:    
    
RET
Check_Register_Addressing ENDP   

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------

ExecuteInstruction PROC  
    
 CMP hasError,1
 JE GiveErrorToThePlayer   
    
    
 CMP instructionOpcode,00h
  JE CALL_ExecuteMov    
    
 CMP instructionOpcode,01h
  JE CALL_ExecuteAdd 
 
 CMP instructionOpcode,02h
  JE CALL_ExecuteAdc
 
 CMP instructionOpcode,03h
  JE CALL_ExecuteSub
 
 CMP instructionOpcode,04h
  JE CALL_ExecuteSbb
 
 CMP instructionOpcode,05h
  JE CALL_ExecuteMul
 
 CMP instructionOpcode,06h
  JE CALL_ExecuteAnd
 
 CMP instructionOpcode,07h
  JE CALL_ExecuteOr
 
 CMP instructionOpcode,08h
  JE CALL_ExecuteXor
 
 CMP instructionOpcode,09h
  JE CALL_ExecuteDiv
 
 CMP instructionOpcode,0Ah
  JE CALL_ExecutePush
 
 CMP instructionOpcode,0Bh
  JE CALL_ExecutePop
 
 CMP instructionOpcode,0Ch
  JE CALL_ExecuteInc
 
 CMP instructionOpcode,0Dh
  JE CALL_ExecuteDec
 
 CMP instructionOpcode,0Eh
  JE CALL_ExecuteNop
 
 CMP instructionOpcode,0Fh
  JE CALL_ExecuteClc  
    
 
 CALL_ExecuteMov:
          CALL ExecuteMov 
          RET
 CALL_ExecuteAdd:
          CALL ExecuteAdd
          RET
 CALL_ExecuteAdc:
          CALL ExecuteAdc
          RET
 CALL_ExecuteSub:
          CALL ExecuteSub
          RET
 CALL_ExecuteSbb:
          CALL ExecuteSbb
          RET
 CALL_ExecuteMul:
          CALL ExecuteMul
          RET
 CALL_ExecuteAnd:
          CALL ExecuteAnd
          RET
 CALL_ExecuteOr:
          CALL ExecuteOr
          RET
 CALL_ExecuteXor:
          CALL ExecuteXor
          RET
 CALL_ExecuteDiv:
          CALL ExecuteDiv
          RET
 CALL_ExecutePush:
          CMP isOperand18Bits,1
          JE GiveErrorToThePlayer
          CALL ExecutePush
          RET
 CALL_ExecutePop:
          CALL ExecutePop
          RET
 CALL_ExecuteInc:
          CALL ExecuteInc
          RET
 CALL_ExecuteDec:
          CALL ExecuteDec
          RET
 CALL_ExecuteNop:
          NOP 
          RET
 CALL_ExecuteClc:
          MOV carryFlag,0
          RET

GiveErrorToThePlayer: 
         MOV hasError,1
                    
ret    
ExecuteInstruction ENDP 



;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;---------------------------------MOV FUNCTION---------------------------------------------

ExecuteMov PROC
    
    CMP isOperand18Bits,1
    JE OneByte1
    
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BX,[SI]
    MOV [DI],BX
    ret
    
  OneByte1:
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BL,[SI]
    MOV [DI],BL
        
ret    
ExecuteMov ENDP    
    
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;-------------------------------- ADD FUNCTION --------------------------------------------
    
ExecuteAdd PROC
    CMP isOperand18Bits,1
    JE OneByte2
    
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BX,[SI]
    ADD [DI],BX
    JC set_carry1
    MOV carryFlag,0
    ret
    
  OneByte2:
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BL,[SI]
    ADD [DI],BL
    JC set_carry1
    MOV carryFlag,0 
    ret
    
  set_carry1: MOV carryFlag,1  
        
ret
ExecuteAdd ENDP    

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;---------------------------------- ADC FUNCTION ------------------------------------------

ExecuteAdc PROC
    
    CMP isOperand18Bits,1
    JE OneByte3
    
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BX,[SI]
    ADD BL,carryFlag
    ADD [DI],BX
    JC set_carry2
    MOV carryFlag,0
    ret
    
  OneByte3:
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BL,[SI]
    ADD BL,carryFlag
    ADD [DI],BL
    JC set_carry2
    MOV carryFlag,0 
    ret
    
  set_carry2: MOV carryFlag,1  
        
ret
ExecuteAdc ENDP


;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;--------------------------------- SUB FUNCTION --------------------------------------------

ExecuteSub PROC
    CMP isOperand18Bits,1
    JE OneByte4
    
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BX,[SI]
    SUB [DI],BX
    JC set_carry3
    MOV carryFlag,0
    ret
    
  OneByte4:
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BL,[SI]
    SUB [DI],BL
    JC set_carry3
    MOV carryFlag,0 
    ret
    
  set_carry3: MOV carryFlag,1  
        
ret
ExecuteSub ENDP

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;-------------------------------- SBB FUNCTION -----------------------------------------

ExecuteSbb PROC
    CMP isOperand18Bits,1
    JE OneByte5
    
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BX,[SI]
    ADD BL,carryFlag
    SUB [DI],BX
    JC set_carry4
    MOV carryFlag,0
    ret
    
  OneByte5:
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BL,[SI]
    ADD BL,carryFlag
    SUB [DI],BL
    JC set_carry4
    MOV carryFlag,0 
    ret
    
  set_carry4: MOV carryFlag,1  
        
ret    
ExecuteSbb ENDP

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;--------------------------------- AND FUNCTION -------------------------------------------

ExecuteAnd PROC
    CMP isOperand18Bits,1
    JE OneByte6
    
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BX,[SI]
    AND [DI],BX
    ret
    
  OneByte6:
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BL,[SI]
    AND [DI],BL
        
ret
ExecuteAnd ENDP

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;--------------------------------- OR FUNCTION --------------------------------------------



ExecuteOr PROC
    CMP isOperand18Bits,1
    JE OneByte7
    
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BX,[SI]
    OR [DI],BX
    ret
    
  OneByte7:
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BL,[SI]
    OR [DI],BL
    
    
ret    
ExecuteOr ENDP


;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------- XOR FUNCTION ---------------------------------------



ExecuteXor PROC
    CMP isOperand18Bits,1
    JE OneByte8
    
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BX,[SI]
    OR [DI],BX
    ret
    
  OneByte8:
    MOV SI,offsetOperand2
    MOV DI,offsetOperand1
    MOV BL,[SI]
    OR [DI],BL
    
    
ret    
ExecuteXor ENDP

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------ PUSH FUNCTION ---------------------------------------

                        ;si =offset memeory+sp 
ExecutePush PROC

     MOV BP,OFFSET memoryPlayer1      
     SUB spPlayer1,1
     MOV DI,spPlayer1
     
     MOV BX,offsetOperand1
     add di,bp
     MOV SI ,di
     MOV AX,[BX]
     MOV [SI],AX
     
     
ret
ExecutePush ENDP

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------ POP FUNCTION ----------------------------------------

ExecutePop PROC
   
     MOV SI,OFFSET memoryPlayer1 
     MOV DI,spPlayer1
     ADD DI,SI
     MOV AX,[DI]
     
     MOV [DI],0000H    
     MOV BX,offsetOperand1
     MOV [BX],AX
     ADD spPlayer1,2
        
ret
ExecutePop ENDP

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------ INC FUNCTION ----------------------------------------

ExecuteInc PROC
    MOV BX,offsetOperand1
    INC [BX]
        
ret
ExecuteInc ENDP



;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------ DEC FUNCTION ----------------------------------------
      
ExecuteDec PROC
    
    MOV BX,offsetOperand1
    DEC [BX]
    
ret
ExecuteDec ENDP

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------ MUL FUNCTION ----------------------------------------

ExecuteMul PROC
    
    CMP isOperand18Bits,1
    JE BYTExBYTE
    
    MOV AX,axPlayer1
    MOV BX,offsetOperand1
    MOV CX,[BX]
    MUL CX
    MOV axPlayer1,AX
    MOV dxPlayer1,DX
    ret
      
 BYTExBYTE:
    MOV AX,axPlayer1
    MOV SI,offsetOperand1
    MOV DL,[SI]
    MUL DL
    MOV axPlayer1,AX
              
    
ret
ExecuteMul ENDP

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------ DIV FUNCTION ----------------------------------------

ExecuteDiv PROC
    
    CMP isOperand18Bits,1
    JE BYTEdivBYTE
    
    MOV AX,axPlayer1
    MOV DX,dxPlayer1
    MOV BX,offsetOperand1
    MOV CX,[BX]
    DIV CX
    MOV axPlayer1,AX
    MOV dxPlayer1,DX
    ret
      
 BYTEdivBYTE:
    MOV AX,axPlayer1
    MOV BX,offsetOperand1
    MOV DL,[BX]
    DIV DL
    MOV axPlayer1,AX
              

ret
ExecuteDiv ENDP

;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------

end main  
;*