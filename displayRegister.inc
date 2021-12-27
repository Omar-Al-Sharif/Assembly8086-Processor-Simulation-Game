
displayRegister MACRO corX,corY,string, sizeOfString,color
    local again
   ;This macro is to display the names of registers 
   ;; corX--> cordinate of x of the starting point of typing
   ;; corY-->coordinate of y of the starting point of typing   
   ;; string--> the string I want to type it
   ;;color-->to select the color of the printed word or byte

  MOV DS,AX 
  MOV SI, OFFSET string
	MOV DH,corY
	MOV DL,corX
	MOV CX,sizeOfString
	
 again:
	MOV AH,2  ;; INT 10/AH=2 FOR SETTIING THE CURSOR
	INT 10H
	
	MOV AL,[SI]   ;;the ascii of the letter for al bec. the letter to print is stored in AL
    MOV BL,color  ;; BL FOR THE COLOR 
	MOV BH,0     ;; BH FOR THE PAGE NUMBER
	MOV AH,0EH  ;; INT 10/AH=0EH FOR WRITING IN VGA MODE 
    INT 10H
    
    INC DL   ;;increment the cordinates of x for the cursor
	INC SI   ;;go to the next letter 
    LOOP again      
    
 
ENDM



