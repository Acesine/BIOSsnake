printString:
	pusha			;
	mov ah, 0x0e		;
	.loop:
	mov al, [ds:si]		;
	cmp al, 0		;
	je .return		;
	int 0x10		;
	inc si			;
	jmp .loop		;
	.return:
	popa			;
	ret			;

;; digit stored in cl;
printDigit:
    pusha;
    mov ah, 0x0e;
    mov al, '0';
    add al, cl;
    int 0x10;
    popa;
    ret;

;; positive integer stored in cx
printInt:
    pusha;
    cmp cx, 0;
    jne .next;
    call printDigit;
    popa;
    ret;

    .next:
    mov bx, 10;
    .loop:
    cmp cx, 0;
    je .return;
    dec bx;
    mov ax, cx;
    mov cx, 10;
    mov dx, 0;
    div cx;
    mov cl, '0';
    add cl, dl;
    mov byte[TMP+bx], cl;
    mov cx, ax;
    jmp .loop
    .return:
    mov si, TMP;
    add si, bx;
    call printString;
    popa;
    ret;

TMP db '0000000000',0;
