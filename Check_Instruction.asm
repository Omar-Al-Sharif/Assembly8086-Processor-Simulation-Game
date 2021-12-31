.MODEL SMALL 
.STACK 64
.DATA 
hasError            db   0
instructionFound    db   0h     ;This is a boolean variable
instructionOpcode   dw   ?
instruction db  "MOV"
sizeOfInstruction  dw  3
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
;xchgInst db 'XCHG'



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



.CODE 
MAIN PROC FAR 
  MOV AX,@DATA 
  MOV DS,AX
  MOV ES,AX
  
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
  
  COMPARE instruction,notInst,sizeOfInstruction,09h
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
    
  HLT   
MAIN ENDP 
END MAIN 


          
          
          
          
          
          
          
          
          
          
          
          
          





          