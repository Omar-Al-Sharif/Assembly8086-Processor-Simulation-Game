.MODEL SMALL 
.STACK 64
.DATA 
operation     DB         'MOV AX,BX','$'
instruction   DB         5 DUP('$')
operands      DB         20 dup('$')
operand1     DB          5 DUP('$')
operand2     DB          5 DUP('$') 
sizeOfInstruction  DW      ? 
sizeOfOperand1     DW      ?
sizeOfOperand2     DW      ?



.CODE 
MAIN PROC FAR 
   MOV AX,@DATA
   MOV DS,AX 
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
              
              
              
                   
                   
                   

MAIN ENDP 
END MAIN
