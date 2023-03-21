.model small
.stack 64

.data
VALUE db 'H'
axPlayer1 dw 31h
bxPlayer1 dw 32h
cxPlayer1 dw 33h
dxPlayer1 dw 34h

.code

Main proc far
mov ax,@data
mov ds,ax
call SendGuiElements

Main endp

SendGuiElements proc 
;Set Divisor Latch Access Bit
mov dx,3fbh             ; Line Control Register
mov al,10000000b        ;Set Divisor Latch Access Bit
out dx,al                ;Out it
;Set LSB byte of the Baud Rate Divisor Latch register.
mov dx,3f8h            
mov al,0ch            
out dx,al
;Set MSB byte of the Baud Rate Divisor Latch register.
mov dx,3f9h
mov al,00h
out dx,al
;Set port configuration
mov dx,3fbh
mov al,00011011b
;0:Access to Receiver buffer, Transmitter buffer
;0:Set Break disabled
;011:Even Parity
;0:One Stop Bit
;11:8bits
out dx,al

;Sending a value
mov cx,08h
mov si, offset axPlayer1
;Check that Transmitter Holding Register is Empty
SendLoop:
        mov dx , 3FDH        ; Line Status Register
AGAIN:  In al , dx             ;Read Line Status
        and al , 00100000b
        JZ AGAIN                               ;Not empty

;If empty put the VALUE in Transmit data register
        mov dx , 3F8H        ; Transmit data register
        mov al,[si]
        inc si
        
        out dx , al
 Loop SendLoop       

; Put the byte you want to send in location 3f8h
ret
SendGuiElements endp

End main