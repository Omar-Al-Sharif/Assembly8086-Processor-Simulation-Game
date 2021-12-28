Pass MACRO corX,corY,passedString,sizeOfString
   
    MOV SI, OFFSET passedString
	MOV DH,corY
	MOV DL,corX
	MOV CX,sizeOfString
	
ENDM