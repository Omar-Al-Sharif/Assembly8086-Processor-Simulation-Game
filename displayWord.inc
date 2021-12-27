
Display MACRO corX,corY,string,numberByte,COLOR,ASCII_TABLE 
   local back 
   ;This is a macro to be used in displaying word in VGA mode
   ;; corX--> cordinate of x of the starting point of typing
   ;; corY-->coordinate of y of the starting point of typing   
   ;; string--> the string I want to type it
   ;;numberByte--> the size of the string
   ;;COLOR-->to select the color of the printed word or byte
   ;;ASCII_TABLE for XLAT instruction
   
    MOV SI,OFFSET STRING
    MOV CL,4 
    MOV DH,corY
    MOV DL,corX
    MOV CL,4
    MOV DI, numberByte    
    
    INC SI  ;; this is to start by the first of 
    MOV AL,[SI]
  BACK:
    ;the next section for trying to isolate the nipple to print it only 
	 sub ax,ax
	MOV AL,[SI]
	ROl Ax,CL
	ROR AL,CL
	PUSH AX
	MOV AL,AH
	MOV BX,OFFSET ASCII_TABLE
	XLAT
	
	;NOW LET'S SET THE CURSOR 
	
	
	MOV AH,2  ;; INT 10/AH=2 FOR SETTIING THE CURSOR
	INT 10H
	
	;THIS FOR TYPING THE NIPPLE
	
	MOV BL,COLOR  ;; BL FOR THE COLOR 
	MOV BH,0  ;; BH FOR THE PAGE NUMBER
	MOV AH,0EH  ;; INT 10/AH=0EH FOR WRITING IN VGA MODE 
    INT 10H
    
	;INCREMENT THE VALUES FOR THE NEXT ITERATIONS
	
	INC DL 
	MOV AH,2  ;; INT 10/AH=2 FOR SETTIING THE CURSOR
	INT 10H
	
	
	;NOW WE WILL PRINT THE SECOND NIPPLE
	POP AX
	XLAT
	
	MOV BL,COLOR  ;; BL FOR THE COLOR 
	MOV BH,0  ;; BH FOR THE PAGE NUMBER
	MOV AH,0EH  ;; INT 10/AH=0EH FOR WRITING IN VGA MODE 
    INT 10H 
	
	;THE NEXT PART IS TO ORGANISE THE LOOP 
	INC DL
	DEC DI
	DEC SI
	
	CMP DI,0
	JNE BACK 
	
   
  ENDM