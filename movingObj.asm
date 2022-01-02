.model large
.stack 64
.data
	
	
	;width1 dw 140h ;width (320 pixel)
	;height1 dw 	0c8h  ;height (200 pixel)
	time db 0 ;var to check if time changed
	ballX dw 0ah
	ballY dw 0ah
	ballSize dw 04h 
	arrowX dw  9ah
	arrowY dw  9ah 
	arrowSize dw 04h ;width
	arrowL dw 0ch ;length
	fireX dw 9ah
	fireY dw 9ah
	;fire dw 9ah
	;fireSize dw 04h
	ballSpeed equ 05h ;X velocity of the ball
.code

	main proc far
		mov ax,@data
		mov ds,ax
		
		mov ah,00h
		mov al,13h
		int 10h
		
		;call clearScreen
		;mov ah,0bh
		;mov bh,00h
		;mov bl,00h
		;int 10h
		
		move:
			mov  ah, 2ch
			int  21h
			
			cmp dl,time
			je move
			
			mov time,dl
			;inc ballX
			
			call clearScreen
			
			inc ballX
			call drawBall
			
			;call moveArrow
			call arrow
			call drawFire
			call moveArrow
			call moveFire
			call throw

			;call clearScreen
			jmp move
		ret

	main endp
	

drawBall proc near

	mov cx,ballX  ; initial position
	mov dx,ballY
	
	drawHorizBall:
		mov ah,0ch     ; pixel
		mov al,0fh  
		mov bh,00h
		int 10h
		
		inc cx    ;horiz
		mov ax,cx
		sub ax,ballX
		cmp ax,ballSize
		jng drawHorizBall
		
		mov cx,ballX  ;cx goes back to initial column
		inc dx
		mov ax,dx
		sub ax,ballY   ;vertical
		cmp ax,ballSize
		jng drawHorizBall
		
	ret	

drawBall endp


arrow proc near

		;xor cx,cx
		;xor dx,dx

		mov cx,arrowX  ; initial position
		mov dx,arrowY
	
	 drawHorizArr:
		mov ah,0ch     ; pixel
		mov al,0fh  
		mov bh,00h
		int 10h
		
		inc cx    ;horiz
		mov ax,cx
		sub ax,arrowX
		cmp ax,arrowSize
		jng drawHorizArr
		
		mov cx,arrowX  ;cx goes back to initial column
		inc dx
		mov ax,dx
		sub ax,arrowY   ;vertical
		cmp ax,arrowL
		jng drawHorizArr
		
		
	ret		

arrow endp

moveArrow proc near

		mov ah,01h			;check key press
		int 16h 
		je noMotion
		
		mov ah,00h			;check which key is pressed (al -> ascii char)
		int 16h
		
		cmp al,61h	;if a or A move left
		je left
		cmp al,41h
		je left
		
		; if d or D move right
		cmp al,44h
		je right
		cmp al,64h
		je right
		jmp noMotion
		
		right:
			inc arrowX
		ret	
		
		left:
			dec arrowX
		ret
		
		noMotion:
		
	ret
moveArrow endp

drawFire proc near

	mov cx,fireX  ; initial position
	mov dx,fireY
	
	drawFire1:
		mov ah,0ch     ; pixel
		mov al,0fh  
		mov bh,00h
		int 10h
		
		;inc cx    ;horiz
		;mov ax,cx
		;sub ax,fireX
		;cmp ax,fireSize
		;jng drawFire1
		
		;mov cx,fireX  ;cx goes back to initial column
		;inc dx
		;mov ax,dx
		;sub ax,fireY   ;vertical
		;cmp ax,fireSize
		;jng drawFire1
		
	ret	

drawFire endp

moveFire proc near

		mov ah,01h			;check key press
		int 16h 
		je noMotion1
		
		mov ah,00h			;check which key is pressed (al -> ascii char)
		int 16h
		
		cmp al,61h	;if a or A move left
		je left1
		cmp al,41h
		je left1
		
		; if d or D move right
		cmp al,44h
		je right1
		cmp al,64h
		je right1
		jmp noMotion1
		
		right1:
			inc fireX
		ret	
		
		left1:
			dec fireX
		ret
		
		noMotion1:
		
	ret
moveFire endp

throw proc near 

		mov ah,01h			;check key press
		int 16h 
		je noMotion2
		
		mov ah,00h			;check which key is pressed (al -> ascii char)
		int 16h
		
		cmp al,20h
		je up
		jmp noMotion2
		
		up:
			
			mov  ah, 2ch
			int  21h
			
			cmp dl,time
			je noMotion2
			;test1:
			
			mov time,dl
			
			
			call clearScreen
			inc fireY
			cmp fireY,0c8h
			je noMotion2
		
		noMotion2:
	ret
	
throw endp


clearScreen proc near

		mov ah,00h
		mov al,13h
		int 10h
		
	ret
clearScreen endp
end main
