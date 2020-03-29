;----------------------------------------------------------------------------
;dOS4GB Dos extender ver 4.00  Raw/XMS/VCPI/DPMI
;
;Copyright (c) 1997  Thessaloniki - Greece
;                    By George Papaioannou (a.k.a Shadow Dancer)
;
;
;
;
;
;þ You can use this code freely in any way you want but the only thing that
;  I ask, is to credit me & spread this package unmodified.
;
;  email:xxxxxxxxxx@rocketmail.com
;  ICQ #11405902
;
;----------------------------------------------------------------------------
.386p

RAW             =       0
XMS             =       1
VCPI            =       2
DPMI            =       3



STACK16_SIZE    =       400h            ; stack size for Real Mode
STACK32_SIZE    =       400h            ; stack size for Protected Mode
LowMemLimit     =       64              ; Minimum Base Mem in KB
LowExtMem       =       64              ; Minimum Extended Mem in 1K chunks
                                        ; meaning 64*1024 = 64KB at least


segment_descriptor struc
  seg_length0_15	dw	?	; low word of the segment length
  base_addr0_15		dw	?	; low word of base address
  base_addr16_23	db	?	; low byte of high word of base addr.
  flags			db	?	; segment type and misc. flags
  access		db	?	; highest nibble of segment length
  					; and access flags
  base_addr24_31	db	?	; highest byte of base address
segment_descriptor ends

interrupt_descriptor struc
  offset0_15		dw	?	; low word of handler offset
  selector0_15		dw	?	; segment selector
  zero_byte		db	0	; unused in this descriptor format
  flags			db	?	; flag-byte
  offset16_31		dw	?	; high-word of handler offset
interrupt_descriptor ends




CallBackStruc struc

_edi        label   dword
_di         dw      0, 0
_esi        label   dword
_si         dw      0, 0
_ebp        label   dword
_bp         dw      0, 0
_esp        dd      0          ; this DWORD is a 'fake' esp (see POPAD)
_ebx        label   dword
_bx         label   word
_bl         db      0
_bh         db      0, 0, 0
_edx        label   dword
_dx         label   word
_dl         db      0
_dh         db      0, 0, 0
_ecx        label   dword
_cx         label   word
_cl         db      0
_ch         db      0, 0, 0
_eax        label   dword
_ax         label   word
_al         db      0
_ah         db      0, 0, 0
_Flgs       dw      0
_es         dw      0
_ds         dw      0
_fs         dw      0
_gs         dw      0
_ip         dw      0

_cs         dw      0
_sp         dw      0
_ss         dw      0


CallBackStruc ends





.errnz STACK16_SIZE gt 0fffh        ; error if stack greater than 64k
.errnz STACK32_SIZE gt 0fffh        ; error if stack greater than 64k






Code16  segment public para use16    ; Real 16-bit code
Code16  ends
Code32  segment public para use32    ; Protected 32-bit code
Code32  ends

StackSeg segment Stack use16  ; my stack
     dw STACK16_SIZE dup (?)
EndOfStack Label word
StackSeg ends

Code32end segment para public use16  ; Marker segment for calculations.
Endofprg Label
Code32end ends



;=========================================================================
;Code Segment 16-Bit.
;=========================================================================

Code16 Segment public para use16
       assume cs:code16,ds:code16,ss:StackSeg
       org 0


;------------------------------------------------------------------------
;This is my own stack.This is IN my 16bit code segment.

;stack16 db      STACK16_SIZE dup (?)    ; 16-bit Real Mode stack

;stack16_end   label  word


;_________________________________________________________________________
;Messages come here ? Hmmm
;_________________________________________________________________________
errmsg0         db      1fh,'þ This Dos Extender requires at least a i386 processor.',0
errmsg1         db      1fh,'þ Not enough low memory. Please free some mem by removing TSRs & Device drivers.',0
errmsg2         db      1fh,'þ System is already in V86 mode.',0
errmsg3         db      1fh,'þ Not enough extended memory.',0
errmsg4         db      1fh,'þ Address line A20 can not be switched on.',0
errmsg5         db      1fh,'þ Extended memory allocation failure.',0
errmsg6         db      1fh,'þ VCPI PICs mapping error (INT 30h is already reserved).',0
errmsg7         db      1fh,'þ VCPI error. Page Table overflow error.',0
errmsg8         db      1fh,'þ DPMI server is 16-bit. This extender supports only 32-bit DPMI servers.',0
errmsg9         db      1fh,'þ Can not initialize protected mode via DPMI server.',0
errmsg10        db      'þ Can not modify the DPMI descriptors.',13,10,'$'
errmsg11        db      'þ Not enough low memory under DPMI.',13,10,'$'
errmsg12        db      'þ DPMI does not respond to memory info request.',13,10,'$'
errmsg13        db      'þ Not enough XMS memory under DPMI.',13,10,'$'
errmsg14        db      'þ Any attempt to allocate XMS memory under DPMI is failed.',13,10,'$'

testing         db      'Just it came thru. Testing Msg.',13,10,'$'
DPMI_ok         db      'þ DPMI initialization completed.',13,10,'$'


NoVGAmsg        db      'þ A VGA card required to run the extender. Sorry !! $',10,13
DispLogo        db      4fh,'dOS4GB DosExtender ver 4.00  (C)opyright 1997 by G. Papaioannou (Shadow_Dancer) ',0

DPMImsg         db      0fh,'þ DPMI host found.',0
VCPImsg         db      0fh,'þ VCPI host found.',0
XMSmsg          db      0fh,'þ XMS host found.',0
RAWmsg          db      0fh,'þ RAW mode enabled.',0


Hex_table       db      '0123456789ABCDEF'
RegMask         db      'eax=                 ebx=                 ecx=                 edx=',10,13
                db      'edi=                 esi=                 ebp=                 eip=',10,13
                db      'cs =                 flg=                 err=',10,13,'$'


IDT_Real	dw	3ffh,0,0   ; Real Mode IDT
Old_Int15h      dd      ?
xms_handle      dw      ?          ; The XMS handler
xms_addr        dd      ?          ; The XMS interrupt service address

Host_system     db      ?          ; A flag that shows what kind of host
                                   ; we are running of (RAW,XMS,VCPI,DPMI)
PSP_seg         dw      ?          ; PSP segment

PIC_master      db      ?
PIC_slave       db      ?

;--- DPMI stuff
DPMI_REQ_handle      dd     ?
DPMI_REQ_size        dd     ?
DPMI_REQ_base        dd     ?

DPMI_enter_pmode dd     ?          ; Address of proc to call to enter to PMode
DPMI_DosEnvSel   dw     ?          ; store DPMI ENVIRONMENT selector
DPMI_PSPSel      dw     ?          ; store PSP selector
Code32Sel        dw     ?
Data32Sel        dw     ?
Core32Sel        dw     ?
Code16Sel        dw     ?
Data16Sel        dw     ?
linear_code32End dd     ?
code32_descriptor16 segment_descriptor <0ffffh,0,0,9ah,0cfh,0> ; 4GB 32-bit code
data32_descriptor16 segment_descriptor <0ffffh,0,0,92h,0cfh,0> ; 4GB 32-bit data
core32_descriptor16 segment_descriptor <0ffffh,0,0,92h,0cfh,0> ; 4GB 32-bit core


CallBack16        CallBackStruc   <0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>


;---------------------------------------------------------------------------
;Procedure Write_rm
;Displays a zero terminated message in real mode at x=0,y=0
;The first byte of the message is the attribute
;In       ds:si message
;--------------------------------------------------------------------------
Write_rm proc
	push	ax dx si di es
	mov	ax,0B800h
	mov	es,ax

        mov     di,0

	mov	ah,[si]		        ; get attribute byte
        inc     si
WriteLoop:
	mov	al,ds:[si]
	or	al,al			; end of string?
	jz	LoopEnd
	inc	si
	mov	es:[di],ax
	inc	di
	inc	di
	jmp	WriteLoop
LoopEnd:
	pop	es di si dx ax
	ret

Write_rm endp



;----------------------------------------------------------------------------
;Procedure Write_rm_spaces
;Displays a single painted row (filled with spaces)
;In:      dh  :Y Position
;         dl  :Attribute
;----------------------------------------------------------------------------
Write_rm_spaces proc
	push	ax dx di es

        mov     ax,0B800h
	mov	es,ax
	movzx	di,dh		        ; get Y position
	imul	di,160
	mov	ah,dl        		; get attribute byte
        mov     al,32
        mov     cx,80
        rep     stosw

	pop	es di dx ax
        ret
Write_rm_spaces endp


;----------------------------------------------------------------------------
;Procedure Write_rm_Y
;Displays a single line filled with the ds:si message
;In:      dh  :Y Position
;       ds:si :Message
;----------------------------------------------------------------------------
Write_rm_Y proc
	push	ax dx di es

        cld
        mov     ax,0B800h
        mov     es,ax
        movzx   di,dh                   ; get Y position
	imul	di,160

	mov	ah,[si]        		; get attribute byte
        inc     si
lop:    lodsb
        or      al,al
        jz      finitoMsg
        stosw
        jmp     lop

finitoMsg:
	pop	es di dx ax
        ret
Write_rm_Y endp



;---------------------------------------------------------------------------
;EXIT WITH AN ERROR
;If any fatal error will occur such as not a 386 processor presented then
;the program falls here and stop the execution while is printing an error
;message
;___________________________________________________________________________
Exit16_err:
        mov     dx,001fh
        call    write_rm_spaces
        call    write_rm   ;Display a message
QuitToDos:
        mov ax,4c00h     ;quit to dos
        int 21h

;__________________________________________________________________________
;Procedure check_processor
;It checks to see what kind of processor is that.
;A processor is 386 or above if flag bits 4-7 are remain unchanged
;while pushing and popping from the stack.
;__________________________________________________________________________
Check_processor Proc
        cli

        pushf         ;push the original flags
        xor ah,ah     ;check for i386 by testing the flags.
        push ax
        popf
        pushf
        pop ax
        and ah,0f0h       ;in i386 the flag bits 4-7 remains unchanged.
        cmp ah,0f0h       ;if ah=0f0h then isn't a 386
        je short No386
        mov ah,0f0h       ;check again with a non-zero value
        push ax
        popf
        pushf
        pop ax
        and ah,0f0h
        jz short no386  ;if ah bits 4-7<>0f then not a 386
        popf

        sti
        ret
No386:
        sti
        mov si,offset errmsg0
        jmp short Exit16_err
Check_processor endp

;-------------------------------------------------------------------------
;Check to see if a VGA Card is presented
;-------------------------------------------------------------------------
Check_vga proc
      mov   ax,1a00h
      int   10h
      cmp   al,1ah
      jnz   NoVGA
      ret
NoVGA:
      mov   ah,09h
      mov   dx,Offset NoVGAmsg
      int   21h
      jmp   QuitToDos
Check_vga endp

;-------------------------------------------------------------------------
;Procedure Check_v86
;Check to see if is in v86 mode already
;------------------------------------------------------------------------
Check_V86 Proc Near
        smsw ax    ;store machine status word (cr0)
        test al,1  ;Check bit 0 to see if we are already in PM
        mov si,offset errmsg2
        jnz short Exit16_Err
        ret
Check_V86 endp

;-----------------------------------------------------------------------
;Procedure TestA20
;Test to see if line A20 is enabled
;Returns  :ZF = 1 A20 has already enabled
;          ZF = 0 A20 is disabled
;-----------------------------------------------------------------------
TestA20 Proc Near
        mov al,fs:[0]   ;take a sample byte from 0:0
        mov ah,al       ;make a copy to ah
        not al          ;reverse al
        mov gs:[10h],al  ;store the traced byte.IF A20=enable no wrap will
                         ;occure else the internal address will become 0:0
                         ;'cos FFFFh:10h=(FFFFh*16)+10h= 100000 linear addr
                         ;IF A20=disabled this address is out of range
        cmp ah,fs:[0]    ;compare the result.if are the same zf=1
        mov fs:[0],ah    ;restore the last value
        ret
TestA20 endp

;----------------------------------------------------------------------
;Procedure EnableA20
;It just enables the A20 line.If any error will occur then the program
;exist with an error message.
;-----------------------------------------------------------------------
EnableA20 Proc Near

        xor ax,ax
        mov fs,ax               ;fs=0
        dec ax
        mov gs,ax               ;gs=FFFF
        call testA20            ;do the test.if zf=1 A20 already enabled
        jz A20_Enabled

        ;==================================================================
        ;PS/2 reflects the A20 port in 92h in contrast with an AT compatible
        ;==================================================================
        cli
        in al,92h                       ;PS/2 A20 enable
        or al,2
        jmp short $+2                   ;wait for a little until the internal
        jmp short $+2                   ;circuits updated
        jmp short $+2
        out 92h,al
        call testA20                    ;test again.
        jz A20_Enabled

        ;==================================================================
        ;AT compatibles reflects the A20 port in the keyboard controller
        ;
        ;þ Commands in Keyboard are accessed thru port 60h
        ;þ While the corresponding data must be send to port 64h
        ;==================================================================

        call Keyb_InBuffer     ;check to see if keyb is free to receive data
        jnz  TestA20
        jz   A20_Enabled
        mov al,0d1h            ;Write output port. Next byte written to 60h
        out 64h,al             ;will be written to the 804x output port.
                               ;Bit 1 of port 60h controls A20
        call Keyb_InBuffer     ;check to see if keyb is free to receive data
        jnz  TestA20           ;if yes then try to enable A20
        jz   A20_Enabled
        mov al,0dfh            ;enable A20
        out 60h,al
        call Keyb_InBuffer
        jnz  TestA20
        jz   A20_Enabled

        sti
        mov si,offset errmsg4
        jmp Exit16_err

Keyb_InBuffer:
        mov ecx,20000h
@Loop1:
        jmp short $+2       ;some delay here
        jmp short $+2
        jmp short $+2
        in al,64h
        test al,2           ;Test to see if keyboard Input buffer is full
        loopnz @Loop1       ;If yes we can write nothing so loop again
A20_Enabled:
        ret

EnableA20 endp

;--------------------------------------------------------------------------
;Procedure DisableCtrl_B
;Redirects the Ctrl-Break interrupt to a null interrupt.
;--------------------------------------------------------------------------
DisableCtrl_B proc
        xor ax,ax                       ; Disable CTRL+BREAK
        mov es,ax
        mov eax,es:[1Bh*4]
        mov word ptr es:[1bh*4],Offset NullInt    ;Null Int
        mov word ptr es:[1bh*4+2],cs

        ret

NullInt:
        iret

DisableCtrl_B endp



;-------------------------------------------------------------------------
;Procedure PrepareToExit
;Recover the system to the initial settings ie. Restore Int handler etc.
;-------------------------------------------------------------------------

PrepareToExit proc

        ;------ Restore Old Int15h

        push    ds
        mov     ax,2515h
        mov     ds,word ptr [Old_Int15h+2]
        mov     dx,word ptr [Old_Int15h]
        int     21h
        pop     ds

        cmp     host_system,XMS    ; if XMS host DO something more.
        jnz     short finito

        mov     dx,xms_handle
        mov     ah,0dh             ; unlock the allocated xms block
        call    xms_addr
        mov     ah,0ah             ; and finally release it.
        call    xms_addr

finito:

        ret
PrepareToExit endp

;-------------------------------------------------------------------------
;Procedure Init_Raw_Mode
;-------------------------------------------------------------------------
Init_Raw_Mode proc
        mov     host_system,RAW

        call    EnableA20

        mov     ah,88h    ;Get Extended memory
        int     15h       ;Ax=Number of 1024KB above the 1MB Dos real limit

        cmp     ax,LOWExtMem  ;If it's below than the minimum then quit
        jae     LowExt_Ok

        sti
        mov     si,offset errmsg3
        jmp     Exit16_err

LowExt_Ok:
        mov     cx,code32
        mov     es,cx

        movzx   eax,ax
        shl     eax,10     ;mul * 1024
        mov     es:[XMS_MemAvail],eax           ;For info purposes

        mov     ebx,100000h
        mov     es:[XMS_LowMem],ebx
        add     ebx,eax
        mov     es:[XMS_TopMem],ebx            ;For Internal use

        ret
Init_Raw_Mode endp


;----------------------------------------------------------------------
;Init proc for all hosts. It just replaces int15h and then it fills up the
;base mem variables.
;----------------------------------------------------------------------
InitSystem proc near

        cli

        ;---- Install My Own Int15h

        mov     ax,3515h
        int     21h                        ;get int 15h
        mov     word ptr cs:[old_int15h+2],es
        mov     word ptr cs:[old_int15h],bx

        push    ds
        push    cs
        pop     ds
        mov     ax,2515h
        mov     dx,offset My_int15h
        int     21h                        ;set int 15h
        pop     ds


        ret
InitSystem endp

;-----------------------------------------------------------------------------
;Procedure FindLowMem
;-----------------------------------------------------------------------------
        ;----- Prepare the memory below the 1MB limit
FindLowMem proc near
        mov     ax,code32
        mov     fs,ax

        int     12h
        movzx   eax,ax
        cmp     eax,639
        jb      Not_enough_mem


        shl     eax,10    ;mul * 1024
        mov     fs:[Base_TopMem],eax
        xor     ebx,ebx
        mov     bx,Code32end
        shl     ebx,4
        add     ebx,15       ;15 more bytes just to be sure
        sub     eax,ebx
        mov     fs:[Base_LowMem],eax

        shr    eax,10        ; div by 1024
        cmp    ax,LOWMemLimit
        jb     Not_enough_mem

        ;---- Shrink DOS memory
        xor    eax,eax
        xor    ebx,ebx
        mov    ax,[psp_seg]
        shl    eax,4
        mov    bx,Code32End
        shl    ebx,4
        sub    ebx,eax
        shr    bx,4
        inc    bx
        mov    es,[psp_seg]
        mov    ah,4Ah
        int    21h
        jc     Not_enough_mem

        jmp    LowBase_ok

Not_enough_mem:

        sti
        pop     ax           ;Extract cs:ip (at this point they aren't used)
        pop     ax
        mov     si,offset errmsg1
        jmp     Exit16_err


LowBase_Ok:

        ret

FindLowMem endp



;--------------------------------------------------------------------------
;Procedure My_Int15h
;Interrupt handler for the int 15h just to inform propertly the user of how
;much memory is currently available.
;--------------------------------------------------------------------------
My_Int15h proc far
        cmp     ah,88h             ;Memory avail requested ?
        jnz     Call_Old_Int15h    ;If not jmp to the original handler

        push    es
        push    eax

        mov     ax,code32
        mov     es,ax

        mov     eax,es:[XMS_TopMem]
        sub     eax,es:[XMS_LowMem]

        pop     eax
        pop     es

        iret
Call_Old_Int15h:
        jmp     [old_int15h]

My_Int15h endp


;-----------------------------------------------------------------------
;Initialize XMS Mode.
;-----------------------------------------------------------------------
Init_XMS_Mode proc near
        mov     host_system,XMS

        mov     ax,4310h                    ; get XMS driver address
        int     2fh
        mov     word ptr xms_addr[0],bx
        mov     word ptr xms_addr[2],es


        mov     ax,code32
        mov     es,ax


        mov     ah,3                        ; XMS enable A20
        call    xms_addr
        or      ax,ax
        mov     si,offset errmsg4
        jz      Exit16_err

        mov     ah,8                        ; get max free extended memory
        call    xms_addr                    ; sub 64k 'cos in the returned
        sub     ax,64                       ; mem doesn't included the
        jnc     short xms_cont              ; HMA (even if doesn't used)
        xor     ax,ax
xms_cont:
        cmp     ax,LowExtMem
        mov     si,offset errmsg3
        jb      Exit16_err
        mov     dx,ax
        movzx   ecx,ax                      ; keep ecx for later use
        shl     ecx,10                      ; ax=xms mem in kb * 1024

        mov     es:[XMS_MemAvail],ecx

        mov     ah,9                        ; try to alloc the returned
        call    xms_addr                    ; amount of extended memory
        mov     si,offset errmsg5
        or      ax,ax
        jz      Exit16_err

        mov     xms_handle,dx               ; save the xms handle
        mov     ah,0ch                      ; try to lock the allocated
        call    xms_addr                    ; block of memory.

        mov     si,offset errmsg5

        or      ax,ax                       ; error ?
        jz      Exit16_err

        shrd    eax,edx,16                  ;dx:bx =32bit locked addr
        mov     ax,bx                       ;mov the above 32bit addr to eax
        mov     es:[XMS_LowMem],eax         ;Addresses are reletive with
        add     eax,ecx                     ;zero and not with the
        mov     es:[XMS_TopMem],eax         ;code32.


        ret
Init_XMS_Mode endp

;-------------------------------------------------------------------------
;Procedure Check_VCPI
;Check to see if a VCPI manager is currently running.
;Returns: CF=1 if VCPI not presents
;-------------------------------------------------------------------------
Check_VCPI Proc Near
        xor   ax,ax               ;do a double check
        mov   gs,ax
        mov   ax,gs:[67h*4]
        or    ax,gs:[(67h*4)+2]    ;if int vect seg:ofs = 0 then no VCPI exists
        jz    short No_VCPI

        mov   ax,0de00h   ;INT 67h (EMS interrupt)
                        ;AX = DE00h     VCPI Presence Detection
        int   67h         ;if ah<>0 VCPI is not present
        or    ah,ah
        clc
        jz    short Yes_VCPI
No_VCPI:
        stc
Yes_VCPI:
        ret
Check_VCPI endp

;-------------------------------------------------------------------------
;Initialize the VCPI host
;No registers preserved
;-------------------------------------------------------------------------


Init_VCPI_Mode:
        mov     host_system,VCPI

        mov     ax,0de0ah                   ; get PIC mappings
        int     67h
        mov     PIC_master,bl
        mov     PIC_slave,cl

        cmp     bl,cl                       ;Mapped as equal ?
        mov     si,offset errmsg5
        je      Exit16_err                  ;IF yes exit.

        cmp     bl,30h                      ; Do not allow PIC mapping
        je      Exit16_err                  ; starting for int 30h
                                            ; we just need this & next int
        cmp     cl,30h                      ; for our purposes. This is
        je      Exit16_err                  ; the true for both PICs

        ;--- adjust fs to point to my code32 seg.
        mov     ax,code32
        mov     fs,ax


	mov     eax,large code32	; adjust switch GDT & IDT
	shl     eax,4
	add     fs:[vcpi_gdt],eax
	add     fs:[vcpi_idt],eax

        ;--- align data mem on the next 4kb page.

        mov     ebx,fs:[Base_LowMem]     ; align data area on 4kb page
        lea     ecx,[ebx+0fffh]          ; Base_LowMem addr + 4095
        and     ecx,0fffff000h           ; zero 12 LSB
        mov     fs:[Base_LowMem],ecx     ; That addr is aligned.
        cmp     ecx,fs:Base_TopMem
        mov     si,offset errmsg1
        jz      Exit16_err


        ;--- set up page directory and page tables.

	mov     eax,fs:[Base_LowMem]	; setup page dir. & tables seg.
	shr     eax,4
	mov     es,ax
	mov     fs:[vcpi_dir_seg],ax
        mov     es,ax

	mov     eax,fs:[Base_LowMem]	; allocate page directory
	add     eax,4096
	cmp     eax,fs:[Base_TopMem]
        mov     si,offset errmsg1
        jz      Exit16_err
	mov     fs:[Base_LowMem],eax


        movzx   ebx,fs:[vcpi_PageTables]
        shl     ebx,12                    ; PageTables * 4096 (shl 12)
        add     eax,ebx                   ; add to base mem
        cmp     eax,fs:[Base_TopMem]
        mov     si,offset errmsg1
        jz      Exit16_err
	mov     fs:[Base_TopMem],eax

        ;--- initialize page dir & 1st page table
        cld
	xor     di,di			; clear page dir. & 1st table
        mov     cx,1000h                ; 4096x2
	mov     eax,0
	repz    stosw

        ;--- prepare to call the init interrupt.

	mov     di,4096			  ; DI--> 1st page table
	mov     esi,offset fs:[GDTtask_descriptor] ; DS MUST point to my Code32 seg.
                                          ; & si--> 3 descrip. tables in
                                          ; my GDT
        push    ds
        mov     ax,code32
        mov     ds,ax

	mov     ax,0DE01h
	int     067h                    ; init VCPI API.
        pop     ds
	mov     fs:[vcpi_call],ebx	;(and VCPI PM call address)


	movzx   eax,di			; setup heap base
	sub     eax,4096                ;the first 4096 bytes is the Directory
                                        ;Page table
	shl     eax,10                  ;each entry is 4 bytes long.
                                        ;So, mul x1024 == (di/4)*4096
	mov     fs:[XMS_LowMem],eax

	mov     fs:[vcpi_page_base],di 	;allocate new pages from here

        ;--- Start allocating all the available memory

Alloc_Next_Page:
	mov     ax,0DE04h               ;alloc one 4KB page
	int     067h
	cmp     ah,0
	jnz     Last_Page

	and     edx,0FFFFF000h          ;for backward compatibility both the
	                                ;server and the client must make the
	                                ;12 LSBs of the physical addr (EDX)
	                                ;zero
        or      edx,000000007h          ;set page as user/writeable/present
        mov     es:[di],edx
        add     di,4          ; max limit of di = 65536-4096
                              ; STOP if allocated 64k-4k of mapping ram
                              ; (equivalent to 64-4-4 ==  56 megabytes
                              ; due to the first 4k block used by the page dir
                              ; and to the last 4M used for phys mapping)
        jnc     Alloc_Next_Page
Last_page:
        mov     fs:[vcpi_page_top],di
        mov     si,di                   ; use si as a counter for la8ter use

	movzx   ecx,di			; setup heap top
	sub     ecx,4096                ; remove the page directory (first 4096)

        mov     ebx,ecx                 ; Test to see if the allocated page
        shr     ebx,12                  ; tables are more that we before
        cmp     bx,fs:[vcpi_PageTables] ; allocated.If so error.
        ja      VCPI_Err_PagesOverflow

	shl     ecx,10                  ; mul x1024
	mov     fs:[XMS_TopMem],ecx
	mov     eax,code32
	shl     eax,4
	add     fs:[XMS_LowMem],eax
	add     fs:[XMS_TopMem],eax

	movzx   ebx,fs:[vcpi_dir_seg]	; setup page dir. address (CR3)
	shl     ebx,4                   ; linear address.
	mov     fs:[vcpi_cr3],eax

        cld
	xor     di,di			; setup page directory
Next_table:
	add     ebx,4096                ; 4096+i = linear addr of each page
	and     eax,0FFFFF000h          ; table (if we have more than one)
	or      eax,000000007h
	stosd                           ; es:[di+i] =page directory
        sub     si,4096                 ; every page table = 4096 bytes long
	ja      Next_table


        ;--- TSS SETUP

        mov     eax,68h                ; TSS is 68h bytes wide
        add     eax,fs:[Base_LowMem]   ; allocate space for TSS and IDT
	cmp     eax,fs:[Base_TopMem]
        mov     si,offset errmsg1
        jz      Exit16_err
	mov     fs:[Base_LowMem],eax

        mov     ebx,code32
        add     eax,ebx        ; get linear address of TSS

        ;--- set Task Switch Segment (TSS) descriptor base address
        or      fs:[GDTtask_descriptor.Base_Addr0_15],ax
        shr     eax,8
        or       fs:[GDTtask_descriptor.Base_Addr16_23],ah

        mov     ebx,eax
        shr     eax,4
        mov     es,ax
        and     ebx,0Fh                ;  es:bx = TSS start

        ;--- first of all, clear everything
        mov     di,bx    ; clear TSS and IO bitmap
        xor     eax,eax  ;
        mov     ecx,1Ah  ; TSS size in dwords
        rep     stosd    ; T-bit is cleared into this loop

        ;--- No I/O permission map
        mov     byte ptr es:[bx+66h],68h ; 68h == end of tss

        ;--- set cr3 in TSS
        mov     eax,fs:[vcpi_cr3]
        mov     es:[bx+1Ch],eax ; set CR3 in TSS


        ;--- ENTER 16BIT PROTECTED MODE
        mov     ax,0de0ch
        mov     esi,offset fs:VCPI_Data_struct   ; system data for mode switch
        int     67h                    ; GO!


        mov     dl,host_system

        ;----  JUMP TO CODE32, Bye from here !!!!

	db	0eah           ;opcode for far jump (to set CS correctly)
        dw      low start32,08h   ;08h=code32_idx







        ;--- error modules
VCPI_Err_PagesOverflow:
        cli

        mov     es,fs:[vcpi_dir_seg]
        mov     si,fs:[vcpi_page_base]
        mov     cx,fs:[vcpi_page_top]
        sub     cx,si
        jz      Pages_Cleaned

Clean_Next_Page:
        mov     edx,es:[si]
        mov     ax,0de05h
        int     67h

        add     si,4

        sub     cx,4
        jnz     Clean_Next_Page
Pages_Cleaned:
        mov     si,offset errmsg6
        jmp     Exit16_err


;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;Initialize the DPMI
;--------------------------------------------------------------------------
Init_DPMI:
        mov     host_system,DPMI

        mov     word ptr DPMI_enter_pmode[0],di
        mov     word ptr DPMI_enter_pmode[2],es
        ;--- save PSP env. space
        mov     ax,[psp_seg]
        mov     es,ax
        push    word ptr es:[2ch]

        ;--- we currently support only 32bit DPMI servers
        mov     cx,si                    ; preserve si, we need it later.
        mov     si,offset errmsg8
        test    bl,1                     ; si = paragraphs for DPMI host private data
        jz      Exit16_err

        ;--- fs points to my code32 segment
        mov     dx,code32
        mov     fs,dx
        xor     edx,edx
        mov     dx,code32End
        shl     edx,4
        mov     linear_code32end,edx

        movzx   eax,cx                 ; paragraphs needed for DPMI private data
        inc     eax                    ; alloc one more paragraph just to be sure
        shl     eax,4
        add     eax,fs:[Base_LowMem]
        cmp     eax,fs:[Base_TopMem]
        ja      Set_Carry
        xchg    eax,fs:[Base_LowMem]
        clc
        jmp     Next_Part
Set_Carry:
        stc

Next_part:

        mov     si,offset errmsg1
        jc      Exit16_err
        shr     eax,4
        mov     es,ax                  ; es:0000 = base of DPMI private data
        cli
        mov     ax,0001h               ; This is a 32bit application
                                       ; so turn on 32bit register interface

        ;--- Now get in Protected mode via the DPMI server.
        mov     si,offset errmsg9
        call    dword ptr cs:DPMI_enter_pmode
        jc      Exit16_err


	;--- now in 16 bit protected mode.
        ; (we are into code16 for code and code32 for data )
        ; PSP:[2ch] now is a SELECTOR to the environment space
        ; cs,ds,ss are now SELECTORS equivalent to their
        ; previous real-mode segment values.
        ; es contains the PSP selector
        ; fs,gs are set to zero

        cli
        pop     ax             ; swap environment segment  with equivalent selector
        xchg    ax,es:[2ch]    ; now pspa+2ch == environment REAL segment
                               ; we use this trick because VCPI
                               ; startup code doesn't set up selectors for PSP
                               ; so we "standardize" on real-mode segments
                               ; instead of pmode selectors


        mov    ds:[DPMI_DosEnvSel],ax     ; store DPMI ENVIRONMENT selector

        mov    ds:[DPMI_PSPSel],es        ; store PSP selector


        push    ds                    ; no more need for PSP segment
        pop     es                    ; now ES == DS

	;--- must ask DPMI for valid selectors
	mov     ax,0003   ; get selector increment value
	int     31h
	mov     bx,ax



        ;--- allocate some descriptors
	mov     ax,0000
        mov     cx,3      ; cx = number of selector to be allocated in LDT
	int     31h
        mov     ds:callback16._edx,offset ds:errmsg10
        jc      DPMI_Exit16_err


	;--- initialize and store selectors
                                        ; bx= selector increment
                                        ; ax = first selector value
        mov     si,ax
        mov     ds:[Code32Sel],ax
        add     ax,bx
        mov     ds:[Data32Sel],ax
        add     ax,bx
        mov     ds:[Core32Sel],ax


        ;--- set descriptors access rights
        mov     dx,cs
        cmp     dx,dx   ; set zero flag so we will know if LAR set it to zero
        lar     dx,dx
        jnz     LAR_err
        and     dh,060h ; access rights AND CPL 3
                        ; (just in case we find a DPMI server running
                        ;  in more powerful cpu levels :) )
        jmp short DPMI_Continue1
LAR_err:
        ;Failed loading access rights
        mov     dh,060h ; set CPL=3 lowest level.

DPMI_Continue1:
        clc ; clear carry
            ; to be sure only the following int 31h will modify it

        ;--- link the descriptors to their selector into LDT
        mov     callback16._edx,offset ds:errmsg10
        mov     ax,000ch
        mov     bx,ds:[Code32Sel]
        mov     edi,offset ds:code32_descriptor16
        or      byte ptr [edi+5],dh            ; change CPL
	int     31h
        jc      DPMI_Exit16_err

        mov     ds:callback16._edx,offset ds:errmsg10
        mov     ax,000ch
        mov     bx,ds:[Data32Sel]
        mov     edi,offset ds:data32_descriptor16
        or      byte ptr [edi+5],dh            ; change CPL
	int     31h
        jc      DPMI_Exit16_err

        mov     ds:callback16._edx,offset ds:errmsg10
        mov     ax,000ch
        mov     bx,ds:[Core32Sel]
        mov     edi,offset ds:core32_descriptor16
        or      byte ptr [edi+5],dh
	int     31h
        jc      DPMI_Exit16_err

        ;--- Set Selectors
        mov     ax,ds
        mov     Data16Sel,ax
        mov     ax,cs
        mov     Code16Sel,ax

        mov     ax,Data32Sel
        mov     fs,ax    ; ES,FS = Data32 (alias for Code32)
        mov     es,ax
        mov     ax,Core32Sel
        mov     gs,ax    ; GS base = zero

        mov     ax,Code32Sel
        mov     fs:Code32_idx,ax
        mov     ax,Data32Sel
        mov     fs:Data32_idx,ax
        mov     ax,Core32Sel
        mov     fs:Core32_idx,ax
        mov     ax,Code16Sel
        mov     fs:Code16_idx,ax
        mov     ax,Data16Sel
        mov     fs:Data16_idx,ax

        ;--- check to see if we have enough low memory (at least 48 bytes)
        mov     edi,fs:[Base_LowMem]
        mov     eax,fs:[Base_TopMem]
	sub     eax,edi
        cmp     eax,48         ; minimum space needed for extended info
        mov     callback16._edx,offset ds:errmsg11
        jb      DPMI_Exit16_err


        mov    fs:[XMS_LowMem],0  ; better assume the worst
        mov    fs:[XMS_TopMem],1
        mov    eax,LowExtMem
        or     eax,eax
        jnz    Get_DPMI_info
        jmp    DPMI_Passed_ok  ; if no ext. mem required,
                               ; then jump without any tests.

        ;--- try to find the max available page under DPMI
Get_DPMI_info:
        mov    es,Data32Sel
        mov    ax,0500h             ; ax     = 0500 == GET DPMI INFO 0.9
        int    31h                  ; es:edi = info block  (48 bytes wide)
                                    ; remember edi was set on Base_LowMem
        mov    callback16._edx,offset ds:errmsg12
        jc     DPMI_Exit16_err

        mov    edx,es:[edi+08h]     ; largest available lockable page number
        cmp    edx,-1               ; (1page = 4K)
        je     short DPMIdefault_alloc
        or     edx,edx              ; zero bytes available ?
        jz     short DPMIdefault_alloc
        shl    edx,12 ; allocate lockable pages
        jmp    short DPMI_available_alloc

DPMIdefault_alloc:
        mov    callback16._edx,offset ds:testing
        jmp    dpmi_exit16_err
                                     ; Try in blind to alloc only
                                     ; the absolutely necessery XMS memory.
        mov    edx,(LowExtMem*1024)  ; minimum space in Kbyte

DPMI_available_alloc:
        mov    callback16._edx,offset ds:errmsg13
        cmp    edx,( LowExtMem * 1024 )
        jb     DPMI_Exit16_err

        mov    fs:[XMS_MemAvail],edx
        jmp    DPMI_passed_ok    ; no need for extended memory

Try_under_DPMI:

        push   edx
	mov    cx,dx                         ; in:
        shld   ebx,edx,16                    ; ax = 0501
	mov    ax,0501h                      ; bx:cx = ext. memory needed
	int    31h                           ; out:
        jnc    Ok_worked                     ; CARRY CLEAR == NO ERRORS and ...
                                             ; bx:cx = linear address allocated
                                             ; si:di = memory block handle

        ; IF cs:eip comes here then something
        ; wrong is happening.

        pop    edx
        mov    callback16._edx,offset ds:errmsg14
        cmp    edx,(LowExtMem*1024)
        jbe    DPMI_Exit16_err
        sub    edx,4096             ; try one page less, and see if it works

        jmp short Try_under_DPMI   ;try again ... Who knows!

OK_worked:
        pop    edx               ;get requested memory size back
        shl    esi,16
        mov    si,di
        mov    ds:[DPMI_REQ_Handle],esi
        pushad
        mov    di,dx
        shr    edx,16
        mov    si,dx
        mov    ax,0600h          ; lock the requested mem size
        int    31h

        popad
        mov    ds:[DPMI_REQ_size],edx

        ;--- set XMS limits
        shl    ebx,16
        mov    bx,cx             ;  ebx = memory block linear address
        mov    ds:[DPMI_REQ_base],ebx
        sub    ebx,[linear_Code32End]
        mov    fs:[XMS_LowMem],ebx

        add    ebx,edx
        mov    fs:[XMS_TopMem],ebx

DPMI_passed_ok:

        mov     dl,host_system

        ;----  JUMP TO CODE32, Bye from here !!!!

;        db      0eah           ;opcode for far jump (to set CS correctly)
;        dw      low start32,08h    ;08h=code32_idx
         mov     ds:callback._edx,offset ds:DPMI_ok
         jmp     dpmi_exit16_err


;--------------------------------------------------------------------------
; DPMI 16 bit TERMINATION ROUTINE
;--------------------------------------------------------------------------


DPMI_Exit16_err:
        ;--- return to text mode
        mov     ds:callback16._ds,code16
        mov     ds:callback16._ah,0
        mov     ds:callback16._al,03h

        mov     ax,0300h
        mov     bx,0010h
        xor     cx,cx
        mov     edi,offset ds:callback16
        push    ds
        pop     es
        int     31h


        ;--- DPMI Exit with error message
        mov     ds:callback16._ds,code16
        mov     ds:callback16._ah,9

        mov     ax,0300h
        mov     bx,0021h
        xor     cx,cx
        mov     edi,offset ds:callback16
        push    ds
        pop     es
        int     31h


DPMI_Exit16:                              ; DPMI exit to real mode
        mov     es,DPMI_PSPSel            ; restore env selector
        mov     ax,DPMI_DosEnvSel
	mov     es:[2ch],ax

        sti

        ;--- unlock the memory we tried to lock a startup
        mov     esi,ds:[DPMI_REQ_size]
        mov     ax,0601h
        mov     ebx,ds:[DPMI_REQ_base]
        mov     di,si
        mov     cx,bx
        shr     esi,16
        shr     ebx,16
        int     31h


        ;--- release XMS mem block
        mov     esi,ds:[DPMI_REQ_handle]
        mov     ax,0502h
        mov     di,si
        shr     esi,16
        int     31h


        ;--- free the selectors
        mov     ax,0001h
        mov     bx,core32Sel
        int     31h

        mov     ax,0001h
        mov     bx,Code32Sel
        int     31h

        mov     ax,0001h
        mov     bx,Data32Sel
        int     31h


        ;--- back to OS
        mov ax,4C00h
        int 21h




;--------------------------------------------------------------------------
;DumpRegisters for debug purposes
;--------------------------------------------------------------------------
DumpRegisters:
        mov     ah,2
        mov     bh,0
        mov     dx,0100h
        int     10h

        mov     dx,Offset RegMask
        mov     ah,9
        int     21h

        mov     ax,code32
        mov     fs,ax

        mov     dx,0105h
        mov     esi,fs:callback._eax
        call    dispNext

        mov     dx,011Ah
        mov     esi,fs:callback._ebx
        call    dispNext

        mov     dx,012Fh
        mov     esi,fs:callback._ecx
        call    dispNext

        mov     dx,0144h
        mov     esi,fs:callback._edx
        call    dispNext

        mov     dx,0205h
        mov     esi,fs:callback._edi
        call    dispNext

        mov     dx,021Ah
        mov     esi,fs:callback._esi
        call    dispNext

        mov     dx,022Fh
        mov     esi,fs:callback._ebp
        call    dispNext

        mov     dx,0244h
        movzx   esi,fs:callback._ip
        call    dispNext

        mov     dx,0305h
        movzx   esi,fs:callback._cs
        call    dispNext

        mov     dx,031Ah
        movzx   esi,fs:callback._flgs
        call    dispNext

        mov     dx,032Fh
        mov     esi,fs:callback._esp
        call    dispNext

        mov     ah,2        ;Free lines
        mov     bh,0
        mov     dx,0500h
        int     10h


        jmp     finish

DispNext:
        mov     ah,2         ;Set X,Y position
        xor     bh,bh
        int     10h

        mov     dx,8
Repeat4:
        rol     esi,4
        mov     bx,si
        and     bx,0fh
        mov     al,byte ptr Offset [Hex_table+bx]
        mov     ah,0eh         ;Display the character
        xor     bx,bx
        int     10h


        dec     dx
        jnz     Repeat4

        mov     al,'h'
        mov     ah,0eh       ;Display the character
        xor     bx,bx
        int     10h


        ret


;======================= s t a r t  16 =============================

start16:
        mov     ax,cs
        mov     ds,ax
        mov     ds:[psp_seg],es   ;preserve PSP
        mov     es,ax
        ;------ Adjust my stack
        cld
	cli			; better disable interrupts while setting
        mov     ax,StackSeg
	mov	ss,ax		; SS and SP
        ;mov     sp,offset stack16_end ;initialize my own stack

        mov     ax,3                  ;text mode 3
        int     10h

        call    check_vga             ;If not a vga card presented shut down

        ;------ Display my logo

        mov     dx,002fh
        call    write_rm_spaces

        mov     si,offset DispLogo
        call    write_rm

        ;------ Delay
        mov     cx,100
@OneCycle:
        mov     DX,3DAh
@Wait:  in      AL,DX
        test    AL,08h
        jz      @Wait
@Retr:  in      AL,DX
        test    AL,08h
        jnz     @Retr
        loop    @OneCycle



        ;------ Do checks

        call    DisableCtrl_B   ; Disable Ctrl-Break
        call    check_processor ; check if we are running on at least a 80386



        ;----- CHECK PM MANAGERS

        ;/////////////////////////////////////////////////////////////////////////
        ; Start checking a DPMI manager then a VCPI the XMS and last run RAW mode
        ;/////////////////////////////////////////////////////////////////////////
        mov ax,1687h                    ; check for DPMI
        int 2fh
        or ax,ax                        ;If ax=0 then we are in DPMI
        jz DPMI_start                   ;The function may return non-zero value
                                        ;even a manager like as DPMI is running in the
                                        ;case where we are in V86 mode.

        call check_VCPI                  ; check for VCPI
        jnc VCPI_start                   ; If cf=0 then VCPI presents

        call check_V86                   ; check for V86 mode

        mov ax,4300h                     ; check for XMS
        int 2fh
        cmp al,80h
        je XMS_start

        jmp RAW_start                     ; Here is the native fast raw mode



;-------------------------------------------------------------------------
;InitTables
;Init GDT,LDT base address,limits and stuffs like that
;-------------------------------------------------------------------------
InitTables proc near
        mov     ax,Code32
        mov     ds,ax

        xor     eax,eax
	mov	ax,code16	; get code segment into AX
	shl	eax,4		; make a physical address
        mov     ds:code16_descriptor.base_addr0_15,ax ; store it in the dscr
        mov     ds:data16_descriptor.base_addr0_15,ax
	shr	eax,8
        mov     ds:code16_descriptor.base_addr16_23,ah
        mov     ds:data16_descriptor.base_addr16_23,ah

        xor     eax,eax
	mov	ax,code32	; get 32-bit code segment into AX
	shl	eax,4		; make a physical address
        mov     ds:code32_descriptor.base_addr0_15,ax ; store it in the dscr
        mov     ds:data32_descriptor.base_addr0_15,ax
	shr	eax,8
        mov     ds:code32_descriptor.base_addr16_23,ah
        mov     ds:data32_descriptor.base_addr16_23,ah

        xor     eax,eax
	mov	ax,code32	; get 32-bit code segment into AX
	shl	eax,4		; make a physical address
        add     eax,offset ds:dummy_descriptor ; calculate physical address of GDT
        mov     dword ptr ds:[gdt_start+2],eax

        xor     eax,eax
	mov	ax,code32	; get 32-bit code segment into AX
	shl	eax,4		; make a physical address
        add     eax,offset ds:interrupt_0  ; calculate physical address of IDT
        mov     dword ptr ds:[idt_start+2],eax

        ret
InitTables endp

;--------------------------------------------------------------------------
;Switch to protected mode in XMS/RAW mode
;--------------------------------------------------------------------------
Switch_2_PM:
        in      al,70h	  ;Disable NMI via CMOS
        mov     ah,al
        or      al,80h
        out     70h,al
        and     ah,80h
        mov     ch,ah
        in      al,71h


        lgdt   fword ptr ds:global_descriptor_table    ; load GDT register
        lidt   fword ptr ds:interrupt_descriptor_table ; load IDT register


	mov	eax,cr0		; get CR0 into EAX
	or	al,1		; set Protected Mode bit
	mov	cr0,eax		; after this we are in Protected Mode!


        in      al,70h		;Enable NMI via CMOS
	and     al,7fh
        or      al,ch
        out     70h,al
        in      al,71h

        mov     dl,host_system


	db	0eah           ;opcode for far jump (to set CS correctly)
        dw      low start32,08h


;        ======= an alternative solution to jump to seg32 ==============
;	 db	0eah           ;opcode for far jump (to set CS correctly)
;        dw      $+4,code16_idx


;        push    dword ptr code32_idx
;        push    offset start32
;        db      66h,0cbh         ;32 bit RETF

;------------------------------------------------------------------------
;Exit16
;------------------------------------------------------------------------
exit16:
        cli
        pushf
	mov	eax,cr0		; get CR0 into EAX
	and	al,0FEh 	; clear Protected Mode bit
	mov	cr0,eax		; after this we are back in Real Mode!

	db	0eah
	dw	offset flush_queue,code16


flush_queue:
        popf
        lidt    fword ptr cs:IDT_Real   ;load old int vectorz

	mov	ax,code16 	; restore important registers
        mov     ds,ax
	;mov	sp,offset stack16_end
	mov	ds,ax
	mov	es,ax
        mov     gs,ax
        mov     fs,ax
        mov     ax,StackSeg
        mov     ss,ax
        mov     si,0            ; Don't use xor si,si 'cos CF becomes 0
        mov     di,0


        jc      DumpRegisters   ;If exception or error Carry=1 dump regs

Finish:
	sti			; enable interrupts
        call    PrepareToExit   ;Restore touched ints

        ;--- Back to OS
	mov	ax,4c00h	; everything is okay, exit with exit-code 0
	int	21h		; bye...






        ;------- Init Raw/Xms/VCPI/DPMI


DPMI_Start:

           ;--- we need current regs so save them
           pusha
           push  es
           ;--- print dpmi logo
           mov   si,offset DPMImsg
           mov   dh,2
           call  Write_rm_y

           ;--- Prepare in 16bit code seg the Code32 & Data32 descriptors
           xor     eax,eax
           mov     ax,code32    ; get 32-bit code segment into AX
           shl     eax,4        ; make a physical address
           mov     ds:code32_descriptor16.base_addr0_15,ax ; store it in the dscr
           mov     ds:data32_descriptor16.base_addr0_15,ax
           shr     eax,8
           mov     ds:code32_descriptor16.base_addr16_23,ah
           mov     ds:data32_descriptor16.base_addr16_23,ah

           ;--- adjust low mem & release unused mem
           call  FindLowMem

           pop    es
           popa
           jmp    Init_DPMI

VCPI_Start:
           mov   si,offset VCPImsg
           mov   dh,2
           call  Write_rm_y

           call  FindLowMem
           call  InitSystem
           jmp   Init_VCPI_Mode

XMS_Start:
           mov   si,offset XMSmsg
           mov   dh,2
           call  Write_rm_y


           call  Init_XMS_mode
           call  FindLowMem
           call  InitSystem
           call  InitTables
           jmp   Switch_2_PM

Raw_Start:
           mov   si,offset RAWmsg
           mov   dh,2
           call  Write_rm_y


           call  Init_Raw_Mode
           call  FindLowMem
           call  InitSystem
           call  InitTables
           jmp   switch_2_PM




;-------------------------------------------------------------------------
;SimulateRInt
;Simulates a Real Mode Interrupt
;-------------------------------------------------------------------------

SimulateRInt:
        cli
	mov	eax,cr0		; get CR0 into EAX
	and	al,0FEh 	; clear Protected Mode bit
	mov	cr0,eax		; after this we are back in Real Mode!

	db	0eah
	dw	offset RInt_flush_queue,code16


RInt_flush_queue:
	mov	ax,code16 	; restore important registers
        mov     ds,ax
        mov     ax,StackSeg
	mov	ss,ax
	;mov	sp,offset stack16_end


        lidt    fword ptr ds:idt_real   ;load old int vectorz

        mov     ax,code32
        mov     fs,ax

        mov     eax,fs:[callback._eax]
        mov     ebx,fs:[callback._ebx]
        mov     ecx,fs:[callback._ecx]
        mov     edx,fs:[callback._edx]

        mov     ebp,fs:[callback._ebp]

        mov     esi,fs:[callback._esi]
        mov     edi,fs:[callback._edi]

        push    fs:[callback._es]
        pop     es
        push    fs:[callback._ds]
        pop     ds


            db      0cdh         ;opcode for   "int RMIntNum"
RMIntNum    db      ?

        push    ds
        push    ax

        mov    ax,code16
        mov    ds,ax

        pop    ax
        mov    fs:[callback._eax],eax
        mov    fs:[callback._ebx],ebx
        mov    fs:[callback._ecx],ecx
        mov    fs:[callback._edx],edx

        mov    fs:[callback._ebp],ebp

        mov    fs:[callback._esi],esi
        mov    fs:[callback._edi],edi

        mov    fs:[callback._es],es
        pop    ax
        mov    fs:[callback._ds],ax
        pushf
        pop    fs:[callback._flgs]


        lgdt   fword ptr fs:global_descriptor_table    ; load GDT register
        lidt   fword ptr fs:interrupt_descriptor_table ; load IDT register


	mov	eax,cr0		; get CR0 into EAX
	or	al,1		; set Protected Mode bit
	mov	cr0,eax		; after this we are in Protected Mode!

        db	0eah            ;opcode for far jump (to set CS correctly)
        dw      $+4,20h         ;20h=code16_idx


        push    dword ptr 08h       ;08h=code32_idx
        push    offset BackFromRInt
        db      66h,0cbh         ;32 bit RETF



Code16 ends







;------------------------------------------------------------------------
;------------------------------------------------------------------------
;                          * * * C O D E  3 2 * * *
;------------------------------------------------------------------------
;------------------------------------------------------------------------

code32 Segment public para use32	; this segment contains all 32-bit

       assume cs:code32, ds:code32, es:code32 ; code and data stuff



;===== 32 Bit entry point ======
start32:

         mov ax,4c00h
         int 21h
         jmp  dpmi_shutdown
        cli
        ;------ here we start in Protected Mode
        mov     ax,core32_idx
        mov     gs,ax
        mov     ax,data32_idx           ; 32bit data selector
        mov     ds,ax
	mov	es,ax
        mov     fs,ax

        mov     ss,ax                   ;use my own stack
        mov     esp,offset stack32_end

        ;------- clear NT in flags, or first iret will attempt to switch task
	pushfd
	mov	ebp, esp
	and	WORD PTR [ebp+1], 08Fh	;40h = NT bit , 30h = IOPL bits
	popfd

        ;--- dx=host_system
        mov    host_system32,dl
        cmp    dl,DPMI
        je     user_execution

        cmp    dl,VCPI
        je     user_execution

        ;----- Prepare Ints

        call   SetInterrupts

        ;---- display messages

        call    clrscr_pm

        mov     dx,001Fh
        call    write_pm_spaces

        mov     esi,Offset InitPM          ; Print a message
        mov     dx,0
        call    write_pm


        ;--- from that point control is send to the client
User_execution:
        ;jmp     _main            ;** START OF THE CLIENT PROGRAM ***

         jmp     Close_PM_session

        ;---- Prepare to come back
Close_PM_session:
        clc                         ;Clear carry flag means don't dump registers
Exception_ShutDown:

        cmp     host_system32,DPMI
        je      DPMI_shutdown
        cmp     host_system32,VCPI
        je      VCPI_shutdown

        ;--- XMS & RAW shutdown
	mov	ax, data16_idx      ;Back to real world !!!
	mov	ds, ax
	mov	es, ax
	db	0EAh
	dw	exit16, 0, 20h  ;PMode ends at this point.Back to real

        ;--- DPMI shutdown
DPMI_shutdown:

	mov	ax, data16_idx      ;Back to real world !!!
	mov	ds, ax
	mov	es, ax
	db	0EAh
        dw      DPMI_Exit16, 0, 20h  ;PMode ends at this point.Back to real

        ;--- VCPI shutdown
VCPI_shutdown:





;========================================================================
_Main:
        int      0

        jmp      Close_pm_session


;========================================================================


;----------------------------------------------------------------------------
;                        PROCEDURES IN PMODE
;----------------------------------------------------------------------------
;----------------------------------------------------------------------------
;Procedure SetInterrupts
;Guess ? Just call this (important) from the Start32 entry point.
;----------------------------------------------------------------------------
SetInterrupts proc
          mov    esi,Offset IDT
          mov    edi,Offset InterruptVector
          mov    cx,MaxInts
NextInt:  mov    bx,word ptr ds:[esi]
          mov    ds:[edi],bx
          add    esi,4
          add    edi,8
          dec    cx
          jnz    NextInt
          ret
SetInterrupts endp


;-----------------------------------------------------------------------------
;Procedure Write_pm
;Displays a zero terminated message in pmode
;In:	DS:ESI - pointer to format string,   byte0 = attrib byte mov al,[esi]
;          dh  - y location
;          dl  - x location
;-----------------------------------------------------------------------------
Write_Pm proc
	push	ax dx esi edi es

	mov	ax,core32_idx		; in protected mode, we have to use
					; core32 0:0 memory to address the
                                        ; screen
	mov	es,ax
	movzx	edi,dh		        ; get Y position
	imul	edi,160
        movzx   ax,dl
	add	di,ax          		; add X position
	add	di,ax
	add	edi,0b8000h		; physical address of text screen
	mov	ah,[esi]		; get attribute byte
        inc     esi
write_loop:
	mov	al,[esi]
	or	al,al			; end of string?
	jz	loop_end
	inc	esi
	mov	es:[edi],ax
	inc	edi
	inc	edi
	jmp	write_loop
loop_end:
	pop	es edi esi dx ax
	ret
Write_Pm endp

;----------------------------------------------------------------------------
;Procedure Write_pm_spaces
;Displays a single painted row (filled with spaces)
;In:      dh  :Y Position
;         dl  :Attribute
;----------------------------------------------------------------------------
Write_pm_spaces proc
	push	ax cx dx edi es

	mov	ax,core32_idx		; in protected mode, we have to use
					; core32 0:0 memory to address the
        cld                             ; screen
	mov	es,ax
	movzx	edi,dh		        ; get Y position
	imul	edi,160
	add	edi,0b8000h		; physical address of text screen
	mov	ah,dl        		; get attribute byte
        mov     al,32
        mov     ecx,80
        rep     stosw

	pop	es edi dx cx ax
        ret
Write_pm_spaces endp

;--------------------------------------------------------------------------
;Procedure Clrscr_pm
;Clears the screen
;---------------------------------------------------------------------------
Clrscr_pm proc

        push    eax ecx edi es

        mov     ax,Core32_idx
        mov     es,ax

        cld
        mov     eax,07200720h
        mov     ecx,1000
        mov     edi,0B8000h
        rep     stosd

        pop    es edi ecx eax

        ret
Clrscr_pm endp

;----------------------------------------------------------------------------
;Procedure AllocGlobalMem
;Allocates memory chunks in extended
;In        Eax = Size required
;Out       cf  = 1 not enough mem. EAX = undefined
;          cf  = 0 mem allocated and EAX = linear address
;----------------------------------------------------------------------------
AllocGlobalMem proc

        stc
        add     eax,XMS_LowMem
        cmp     eax,XMS_TopMem
        ja      short er1
        xchg    eax,XMS_LowMem
        clc
er1:
        ret
AllocGlobalMem endp

;----------------------------------------------------------------------------
;Procedure AllocBaseMem
;Allocates memory chunks in conventional mem  < 640KB
;In        Eax = Size required
;Out       cf  = 1 not enough mem. EAX = undefined
;          cf  = 0 mem allocated and EAX = linear address
;----------------------------------------------------------------------------
AllocBaseMem proc

        stc
        add     eax,Base_LowMem
        cmp     eax,Base_TopMem
        ja      short er2
        xchg    eax,Base_LowMem
        clc
er2:
        ret
AllocBaseMem endp

;----------------------------------------------------------------------------
;Procedure FreeGlobalMem
;De_allocates previously allocated memory chunk in extended
;In          Eax = Size to be released
;Out         Nothing
;----------------------------------------------------------------------------
FreeGlobalMem proc

       sub      XMS_LowMem,eax
       cmp      XMS_LowMem,100000h
       jnb      continue1

       mov      XMS_LowMem,100000h
continue1:
       ret
FreeGlobalMem endp

;---------------------------------------------------------------------------
;Procedure FreeBaseMem
;De_allocates previously allocated memory chunk in main (base)
;In          Eax = Size to be released
;Out         Nothing
;----------------------------------------------------------------------------
FreeBaseMem proc

       push     ebx

       sub      Base_LowMem,eax
       mov      ebx,Base_LowMem
       cmp      Base_LowMemCopy,ebx
       jnb      continue2

       mov      eax,Base_LowMemCopy
       mov      Base_LowMem,eax
continue2:
       pop      ebx
       ret

Base_LowMemCopy dd ?

FreeBaseMem endp


;---------------------------------------------------------------------------
;Procedure XMS_MaxAvail
;Max free mem block in extended
;In           nothing
;Out          eax = number of bytes free
;---------------------------------------------------------------------------
XMS_MaxAvail proc

        mov eax,XMS_TopMem
        sub eax,XMS_LowMem
        ret
XMS_MaxAvail endp

;---------------------------------------------------------------------------
;Procedure Base_MaxAvail
;Max free mem block in extended
;In           nothing
;Out          eax = number of bytes free
;---------------------------------------------------------------------------
Base_MaxAvail proc

        mov eax,Base_TopMem
        sub eax,Base_LowMem
        ret
Base_MaxAvail endp




;----------------------------------------------------------------------------
;                              VARIABLES
;----------------------------------------------------------------------------

code32_idx	dw	08h		; offset of 32-bit code segment in GDT
data32_idx	dw	10h		; offset of 32-bit data segment in GDT
core32_idx	dw	18h		; offset of 32-bit core segment in GDT
code16_idx	dw	20h		; offset of 16-bit code segment in GDT
data16_idx	dw	28h		; offset of 16-bit data segment in GDT
TSS_idx         dw      30h             ; TSS selector in GDT
VCPI_idx        dw      38h             ; VCPI selectors in GDT


host_system32   db      ?


Stack32		db	STACK32_SIZE dup (?)	; 32-bit stack

stack32_end  label   dword



;----------------------------------------------------------------------
;                          * * *  G D T * * *
;----------------------------------------------------------------------
global_descriptor_table label fword     ; here begins the GDT

gdt_start         dw                 gdt_size,0,0            ; val for GDT reg

dummy_descriptor  segment_descriptor <0,0,0,0,0,0>
code32_descriptor segment_descriptor <0ffffh,0,0,9ah,0cfh,0> ; 4GB 32-bit code
data32_descriptor segment_descriptor <0ffffh,0,0,92h,0cfh,0> ; 4GB 32-bit data
core32_descriptor segment_descriptor <0ffffh,0,0,92h,0cfh,0> ; 4GB 32-bit core
code16_descriptor segment_descriptor <0ffffh,0,0,9ah,0,0>    ; 64k 16-bit code
data16_descriptor segment_descriptor <0ffffh,0,0,92h,0,0>    ; 64k 16-bit data
GDTtask_descriptor segment_descriptor <0067h,0,0,89h,0,0>    ; TSS limit without i/o bitmap
GDTvcpi_descriptor segment_descriptor  3 dup( <0,0,0,0,0,0> ) ; VCPI descript.

gdt_size=$-(offset dummy_descriptor)    ;Compute the size


;----------------------------------------------------------------------
;                          * * *  I D T * * *
;----------------------------------------------------------------------
interrupt_descriptor_table label fword  ; here begins the IDT
                idt_start	dw idt_size,0,0


InterruptVector Label
interrupt_0	interrupt_descriptor	<0,08h,0,8eh,0>
interrupt_1	interrupt_descriptor	<0,08h,0,8eh,0>
interrupt_2	interrupt_descriptor	<0,08h,0,8eh,0>
interrupt_3	interrupt_descriptor	<0,08h,0,8eh,0>
interrupt_4	interrupt_descriptor	<0,08h,0,8eh,0>
interrupt_5	interrupt_descriptor	<0,08h,0,8eh,0>
interrupt_6	interrupt_descriptor	<0,08h,0,8eh,0>
interrupt_7	interrupt_descriptor	<0,08h,0,8eh,0>
interrupt_8	interrupt_descriptor	<0,08h,0,8eh,0>
interrupt_9	interrupt_descriptor	<0,08h,0,8eh,0>
interrupt_10	interrupt_descriptor	<0,08h,0,8eh,0>
interrupt_11	interrupt_descriptor	<0,08h,0,8eh,0>
interrupt_12	interrupt_descriptor	<0,08h,0,8eh,0>
interrupt_13	interrupt_descriptor	<0,08h,0,8eh,0>
interrupt_14	interrupt_descriptor	<0,08h,0,8eh,0>
interrupt_15	interrupt_descriptor	<0,08h,0,8eh,0>
interrupt_16	interrupt_descriptor	<0,08h,0,8eh,0>
interrupt_17	interrupt_descriptor	<0,08h,0,8eh,0>
Int_Reserved    interrupt_descriptor   14 dup (<0,08h,0,8eh,0>)
Interrupt_32    interrupt_descriptor    <0,08h,0,8eh,0>
InterruptVectorEnd Label

idt_size=$-(offset interrupt_0)    ;Compute the size
MaxInts=((offset InterruptVectorEnd) - (offset interrupt_0) ) / 8   ;Compute the number of ints


IDT                 dd  offset _int_0,  offset _int_1,  offset _int_2,  offset _int_3
                    dd  offset _int_4,  offset _int_5,  offset _int_6,  offset _int_7
                    dd  offset _int_8,  offset _int_9,  offset _int_10, offset _int_11
                    dd  offset _int_12, offset _int_13, offset _int_14, offset _int_15
                    dd  offset _int_16, offset _int_17, offset _ReservedInts
                    dd  offset _ReservedInts, offset _ReservedInts, offset _ReservedInts
                    dd  offset _ReservedInts, offset _ReservedInts, offset _ReservedInts
                    dd  offset _ReservedInts, offset _ReservedInts, offset _ReservedInts
                    dd  offset _ReservedInts, offset _ReservedInts, offset _ReservedInts
                    dd  offset _ReservedInts, offset _Int_32


;----------------------- General ------------------------------------

_int0msg            db   1fh,'þ Divide by zero (int 0)',0
_int1msg            db   1fh,'þ Debug exception (int 1)',0
_int2msg            db   1fh,'þ Non maskble interrupt (int 2)',0
_int3msg            db   1fh,'þ Breakpoint (int 3)',0
_int4msg            db   1fh,'þ Overflow error (int 4)',0
_int5msg            db   1fh,'þ Bounds check error (int 5)',0
_int6msg            db   1fh,'þ Invalid code (int 6)',0
_int7msg            db   1fh,'þ Device not available (int 7)',0
_int8msg            db   1fh,'þ Double fault (int 8)',0
_int9msg            db   1fh,'þ 80486+ = reserved, 80386- = co-processor segment overrun (int 9)',0
_int10msg           db   1fh,'þ Invalid TSS (int 10)',0
_int11msg           db   1fh,'þ Segment not present (int 11)',0
_int12msg           db   1fh,'þ Stack exception (int 12)',0
_int13msg           db   1fh,'þ General protection fault (int 13)',0
_int14msg           db   1fh,'þ Page fault (int 14)',0
_int15msg           db   1fh,'þ Reserved (int 15)',0
_int16msg           db   1fh,'þ FPU error (int 16)',0
_int17msg           db   1fh,'þ Alignment check error (int 17)',0
_intReservedmsg     db   1fh,'þ Unexpected interrupt',0



XMS_MemAvail    dd      ?
XMS_LowMem      dd      ?
XMS_TopMem      dd      ?
Base_LowMem     dd      ?
Base_TopMem     dd      ?
CallBack        CallBackStruc   <?,?,?,?,?,?,?,?,?,?,?,?,?,?,?>
SaveInt_temp    db      ?

InitPM		db	1fh,'þ Protected mode initialized',0

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³ VCPI DATA								       ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
vcpi_pageTables dw 4

VCPI_Data_struct Label
align 4
                                        ; \
vcpi_cr3	dd 0                    ;   \
vcpi_gdt	dd offset vcpi_gdt_ptr  ;    \
vcpi_idt	dd offset vcpi_idt_ptr  ;      \____   Struct of VCPI call
vcpi_ldt	dd 0                    ;      /       ax=DE0CH,int 67h
vcpi_task	dw 30h   ;TSS_idx       ;     /
vcpi_jump	dd start32              ;    /
		dw 08h   ;code32_idx    ;   /

vcpi_dir_seg	dw 0
vcpi_page_base	dw 0
vcpi_page_top	dw 0

vcpi_call	dd 0

vcpi_gdt_ptr      dw gdt_size                 ; limit for selectors
                  dd offset dummy_descriptor  ; 32bit GDT address

vcpi_idt_ptr      dw idt_size
                  dd offset InterruptVector

;ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;³ DPMI DATA								       ³
;ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ


;---------------------------------------------------------------------------
;                        INTERRUPT ROURINES
;---------------------------------------------------------------------------


;==================================
;int 32 emulate real dos interrupt
;input   :al   = int num
;         callback structure
;output  :callback structure
;==================================
_Int_32:
        push    ds
        push    es
        push    gs
        push    fs
        pushfd

        mov     bx,Data16_Idx       ;Set the Real Mode Int number
        mov     es,bx
;        mov     es:[RMIntNum],al

        mov     CallBack._esp,esp


        mov     ds, bx
        mov     es, bx


        ;---- Go to Code16 and execute the Int xx

        db      0EAh
        dw      SimulateRInt, 0, 20h     ;code16_idx


        ;---- Back from real mode here
BackFromRInt:
        cli
        mov     ax,Data32_idx
        mov     ss,ax                   ;use my own stack
;        mov     esp,ss:CallBack._esp

        popfd
        pop     fs
        pop     gs
        pop     es
        pop     ds

        iretd

;==================================
;Reserved interrupts goes here
;System Shutdown
;==================================
_ReservedInts:
      pushad
      call      clrscr_pm
      mov       dx,001fh
      call      write_pm_spaces
      mov       dx,0
      mov       esi,Offset _intReservedmsg
      call      write_pm

      jmp       Handle_Exception


      iretd

;=======================
;ints 0 thru 17
;=======================
_Int_0:
      pushad            ;For debug purposes

      mov       al,10h             ;Int10 subF ax=3 ==> Textmode
      mov       callback._eax,3
      int       20h

      call      clrscr_pm
      mov       dx,001fh
      call      write_pm_spaces
      mov       dx,0
      mov       esi,Offset _int0msg
      call      write_pm

      jmp       Handle_Exception

_Int_1:
      pushad            ;For debug purposes
      call      clrscr_pm
      mov       dx,001fh
      call      write_pm_spaces
      mov       dx,0
      mov       esi,Offset _int1msg
      call      write_pm

      jmp       Handle_Exception

_Int_2:
      pushad            ;For debug purposes
      call      clrscr_pm
      mov       dx,001fh
      call      write_pm_spaces
      mov       dx,0
      mov       esi,Offset _int2msg
      call      write_pm

      jmp       Handle_Exception

_Int_3:
      pushad            ;For debug purposes
      call      clrscr_pm
      mov       dx,001fh
      call      write_pm_spaces
      mov       dx,0
      mov       esi,Offset _int3msg
      call      write_pm

      jmp       Handle_Exception

_Int_4:
      pushad            ;For debug purposes
      call      clrscr_pm
      mov       dx,001fh
      call      write_pm_spaces
      mov       dx,0
      mov       esi,Offset _int4msg
      call      write_pm

      jmp       Handle_Exception

_Int_5:
      pushad            ;For debug purposes
      call      clrscr_pm
      mov       dx,001fh
      call      write_pm_spaces
      mov       dx,0
      mov       esi,Offset _int5msg
      call      write_pm

      jmp       Handle_Exception

_Int_6:
      pushad            ;For debug purposes
      call      clrscr_pm
      mov       dx,001fh
      call      write_pm_spaces
      mov       dx,0
      mov       esi,Offset _int6msg
      call      write_pm

      jmp       Handle_Exception

_Int_7:
      pushad            ;For debug purposes
      call     clrscr_pm
      mov      dx,001fh
      call     write_pm_spaces
      mov      dx,0
      mov      esi,Offset _int7msg
      call     write_pm

      jmp       Handle_Exception

_Int_8:
      pushad            ;For my debug purposes

      mov      al,00001010b   ;PIC,OCW3 two low bits (0-1)=Read Interrupt
                              ;request register on next read at port 0020h
      out      20h,al
      in       al,20h
      and      al,1          ;IRQ 0 pending ?
      jnz      _IRQ_0


      xor       bx,bx
      push      es
      mov       bp,sp        ;check to see if the last executed
      mov       bl,[bp+32]   ;command was "int xx"
      mov       ax,data32_idx
      mov       es,ax
      mov       byte ptr es:[SaveInt_temp],8
      mov       al,byte ptr es:[bx-2]
      pop       es
      cmp       al,0cdh      ;is al=int opcode ?
      jz        Prepare_User_int


      call     clrscr_pm
      mov      dx,001fh
      call     write_pm_spaces
      mov      dx,0
      mov      esi,Offset _int8msg
      call     write_pm

      jmp       Handle_Exception

_Int_9:
      pushad            ;For debug purposes

      mov      al,00001010b   ;PIC,OCW3 two low bits (0-1)=Read Interrupt
                              ;request register on next read at port 0020h
      out      20h,al
      in       al,20h
      and      al,2          ;IRQ 1 pending ?
      jnz      _IRQ_1


      xor       bx,bx
      push      es
      mov       bp,sp        ;check to see if the last executed
      mov       bl,[bp+32]   ;command was "int xx"
      mov       ax,data32_idx
      mov       es,ax
      mov       byte ptr es:[SaveInt_temp],9
      mov       al,byte ptr es:[bx-2]
      pop       es
      cmp       al,0cdh      ;is al=int opcode ?
      jz        Prepare_User_int


      call     clrscr_pm
      mov      dx,001fh
      call     write_pm_spaces
      mov      dx,0
      mov      esi,Offset _int9msg
      call     write_pm

      jmp       Handle_Exception

_Int_10:
      pushad            ;For debug purposes

      mov      al,00001010b   ;PIC,OCW3 two low bits (0-1)=Read Interrupt
                              ;request register on next read at port 0020h
      out      20h,al
      in       al,20h
      and      al,4          ;IRQ 2 pending ?
      jnz      _IRQ_2


      xor       bx,bx
      push      es
      mov       bp,sp        ;check to see if the last executed
      mov       bl,[bp+32]   ;command was "int xx"
      mov       ax,data32_idx
      mov       es,ax
      mov       byte ptr es:[SaveInt_temp],10
      mov       al,byte ptr es:[bx-2]
      pop       es
      cmp       al,0cdh      ;is al=int opcode ?
      jz        Prepare_User_int


      call     clrscr_pm
      mov      dx,001fh
      call     write_pm_spaces
      mov      dx,0
      mov      esi,Offset _int10msg
      call     write_pm

      jmp       Handle_Exception

_Int_11:
      pushad            ;For debug purposes

      mov      al,00001010b   ;PIC,OCW3 two low bits (0-1)=Read Interrupt
                              ;request register on next read at port 0020h
      out      20h,al
      in       al,20h
      and      al,8          ;IRQ 3 pending ?
      jnz      _IRQ_3


      xor       bx,bx
      push      es
      mov       bp,sp        ;check to see if the last executed
      mov       bl,[bp+32]   ;command was "int xx"
      mov       ax,data32_idx
      mov       es,ax
      mov       byte ptr es:[SaveInt_temp],11
      mov       al,byte ptr es:[bx-2]
      pop       es
      cmp       al,0cdh      ;is al=int opcode ?
      jz        Prepare_User_int


      call    clrscr_pm
      mov     dx,001fh
      call    write_pm_spaces
      mov     dx,0
      mov     esi,Offset _int11msg
      call    write_pm

      jmp       Handle_Exception

_Int_12:
      pushad            ;For debug purposes

      mov      al,00001010b   ;PIC,OCW3 two low bits (0-1)=Read Interrupt
                              ;request register on next read at port 0020h
      out      20h,al
      in       al,20h
      and      al,16          ;IRQ 4 pending ?
      jnz      _IRQ_4

      xor       bx,bx
      push      es
      mov       bp,sp        ;check to see if the last executed
      mov       bl,[bp+32]   ;command was "int xx"
      mov       ax,data32_idx
      mov       es,ax
      mov       byte ptr es:[SaveInt_temp],12
      mov       al,byte ptr es:[bx-2]
      pop       es
      cmp       al,0cdh      ;is al=int opcode ?
      jz        Prepare_User_int



      call    clrscr_pm
      mov     dx,001fh
      call    write_pm_spaces
      mov     dx,0
      mov     esi,Offset _int12msg
      call    write_pm

      jmp       Handle_Exception

_Int_13:
      pushad            ;For debug purposes

      mov      al,00001010b   ;PIC,OCW3 two low bits (0-1)=Read Interrupt
                              ;request register on next read at port 0020h
      out      20h,al
      in       al,20h
      and      al,32          ;IRQ 5 pending ?
      jnz      _IRQ_5

      xor       bx,bx
      push      es
      mov       bp,sp        ;check to see if the last executed
      mov       bl,[bp+32]   ;command was "int xx"
      mov       ax,data32_idx
      mov       es,ax
      mov       byte ptr es:[SaveInt_temp],13
      mov       al,byte ptr es:[bx-2]
      pop       es
      cmp       al,0cdh      ;is al=int opcode ?
      jz        Prepare_User_int



      call    clrscr_pm
      mov     dx,001fh
      call    write_pm_spaces
      mov     dx,0
      mov     esi,Offset _int13msg
      call    write_pm

      jmp       Handle_Exception

_Int_14:
      pushad            ;For debug purposes

      mov      al,00001010b   ;PIC,OCW3 two low bits (0-1)=Read Interrupt
                              ;request register on next read at port 0020h
      out      20h,al
      in       al,20h
      and      al,64          ;IRQ 6 pending ?
      jnz      _IRQ_6

      xor       bx,bx
      push      es
      mov       bp,sp        ;check to see if the last executed
      mov       bl,[bp+32]   ;command was "int xx"
      mov       ax,data32_idx
      mov       es,ax
      mov       byte ptr es:[SaveInt_temp],14
      mov       al,byte ptr es:[bx-2]
      pop       es
      cmp       al,0cdh      ;is al=int opcode ?
      jz        Prepare_User_int


      call    clrscr_pm
      mov     dx,001fh
      call    write_pm_spaces
      mov     dx,0
      mov     esi,Offset _int14msg
      call    write_pm

      jmp       Handle_Exception

_Int_15:
      pushad            ;For debug purposes

      mov      al,00001010b   ;PIC,OCW3 two low bits (0-1)=Read Interrupt
                              ;request register on next read at port 0020h
      out      20h,al
      in       al,20h
      and      al,128          ;IRQ 7 pending ?
      jnz      _IRQ_7

      xor       bx,bx
      push      es
      mov       bp,sp        ;check to see if the last executed
      mov       bl,[bp+32]   ;command was "int xx"
      mov       ax,data32_idx
      mov       es,ax
      mov       byte ptr es:[SaveInt_temp],15
      mov       al,byte ptr es:[bx-2]
      pop       es
      cmp       al,0cdh      ;is al=int opcode ?
      jz        Prepare_User_int



      call    clrscr_pm
      mov     dx,001fh
      call    write_pm_spaces
      mov     dx,0
      mov     esi,Offset _int15msg
      call    write_pm

      jmp       Handle_Exception

_Int_16:
      pushad            ;For debug purposes
      call    clrscr_pm
      mov     dx,001fh
      call    write_pm_spaces
      mov     dx,0
      mov     esi,Offset _int16msg
      call    write_pm

      jmp       Handle_Exception

_Int_17:
      pushad            ;For debug purposes
      call    clrscr_pm
      mov     dx,001fh
      call    write_pm_spaces
      mov     dx,0
      mov     esi,Offset _int17msg
      call    write_pm

      jmp       Handle_Exception


Handle_Exception:
      cld

      mov     ax,Data32_idx
      mov     ds,ax
      mov     es,ax
      mov     fs,ax
      mov     gs,ax


      mov     al,0ffh    ;Disable PIC controllers
      out     21h,al     ;PIC1
      out     0A1h,al    ;PIC2

      ;---- Back To Real Mode Dump Regs and exit

      mov     callback._esp,0
      pop     callback._edi
      pop     callback._esi
      pop     callback._ebp
      pop     callback._ebx
      pop     callback._edx
      pop     callback._ecx
      pop     callback._eax

      mov     eax,Offset Stack32_end
      sub     eax,esp
      cmp     eax,12
      je      @NoErrorCode

      pop     eax
      mov     callback._esp,eax        ;Here is the Error Code



@NoErrorCode:

;      pop     callback._eip

      pop     eax
      mov     callback._ip,ax
      pop     eax
      mov     callback._cs,ax
      pop     callback._flgs

      mov     al,00h     ;Enable PIC controllers
      out     21h,al     ;PIC1
      out     0A1h,al    ;PIC2


      stc                            ;When carry flag set ---> Dump registers
      jmp     exception_shutdown



 ;------------- handlers for IRQs 8-15 -----------------

_int_112: ;(int 70h & IRQ 8)
      pushad            ;For my debug purposes

      mov      al,00001010b   ;PIC,OCW3 two low bits (0-1)=Read Interrupt
                              ;request register on next read at port 00a0h
      out      0A0h,al
      in       al,0A0h
      and      al,1          ;IRQ 8 pending ?
      jnz      _IRQ_8

      push      es
      mov       byte ptr es:[SaveInt_temp],70h
      pop       es
      jz        Prepare_User_int

_int_113: ;(int 71h & IRQ 9)
      pushad            ;For my debug purposes

      mov      al,00001010b   ;PIC,OCW3 two low bits (0-1)=Read Interrupt
                              ;request register on next read at port 00a0h
      out      0A0h,al
      in       al,0A0h
      and      al,2          ;IRQ 9 pending ?
      jnz      _IRQ_9

      push      es
      mov       byte ptr es:[SaveInt_temp],71h
      pop       es
      jz        Prepare_User_int

_int_114: ;(int 72h & IRQ 10)
      pushad            ;For my debug purposes

      mov      al,00001010b   ;PIC,OCW3 two low bits (0-1)=Read Interrupt
                              ;request register on next read at port 00a0h
      out      0A0h,al
      in       al,0A0h
      and      al,4          ;IRQ 10 pending ?
      jnz      _IRQ_10

      push      es
      mov       byte ptr es:[SaveInt_temp],72h
      pop       es
      jz        Prepare_User_int


_int_115: ;(int 73h & IRQ 11)
      pushad            ;For my debug purposes

      mov      al,00001010b   ;PIC,OCW3 two low bits (0-1)=Read Interrupt
                              ;request register on next read at port 00a0h
      out      0A0h,al
      in       al,0A0h
      and      al,8          ;IRQ 11 pending ?
      jnz      _IRQ_11

      push      es
      mov       byte ptr es:[SaveInt_temp],73h
      pop       es
      jz        Prepare_User_int

_int_116: ;(int 74h & IRQ 12)
      pushad            ;For my debug purposes

      mov      al,00001010b   ;PIC,OCW3 two low bits (0-1)=Read Interrupt
                              ;request register on next read at port 00a0h
      out      0A0h,al
      in       al,0A0h
      and      al,16          ;IRQ 12 pending ?
      jnz      _IRQ_12

      push      es
      mov       byte ptr es:[SaveInt_temp],74h
      pop       es
      jz        Prepare_User_int

_int_117: ;(int 75h & IRQ 13)
      pushad            ;For my debug purposes

      mov      al,00001010b   ;PIC,OCW3 two low bits (0-1)=Read Interrupt
                              ;request register on next read at port 00a0h
      out      0A0h,al
      in       al,0A0h
      and      al,32          ;IRQ 13 pending ?
      jnz      _IRQ_13

      push      es
      mov       byte ptr es:[SaveInt_temp],75h
      pop       es
      jz        Prepare_User_int

_int_118: ;(int 76h & IRQ 14)
      pushad            ;For my debug purposes

      mov      al,00001010b   ;PIC,OCW3 two low bits (0-1)=Read Interrupt
                              ;request register on next read at port 00a0h
      out      0A0h,al
      in       al,0A0h
      and      al,64          ;IRQ 14 pending ?
      jnz      _IRQ_14

      push      es
      mov       byte ptr es:[SaveInt_temp],76h
      pop       es
      jz        Prepare_User_int

_int_119: ;(int 77h & IRQ 15)
      pushad            ;For my debug purposes

      mov      al,00001010b   ;PIC,OCW3 two low bits (0-1)=Read Interrupt
                              ;request register on next read at port 00a0h
      out      0A0h,al
      in       al,0A0h
      and      al,128          ;IRQ 15 pending ?
      jnz      _IRQ_15

      push      es
      mov       byte ptr es:[SaveInt_temp],77h
      pop       es
      jz        Prepare_User_int


;==========================================================
;if not an exception or IRQ it's sure that the user called
;this interrupt.All we have to do is to simulate this INT
;with the normal way (int 20h -or 32 in decimal- )
;==========================================================
Prepare_User_int:
      ;we don't know exactly which regs we must send
      ;so prepare all of them and let the actual interrrupt
      ;to decide

      popa
      mov     callback._eax,eax
      mov     callback._ebx,ebx
      mov     callback._ecx,ecx
      mov     callback._edx,edx

      pushf
      pop     callback._flgs

      mov     callback._edi,edi
      mov     callback._esi,esi

      mov     callback._esp,esp
      mov     callback._ebp,ebp

      mov     callback._es,es
      mov     callback._ds,ds
      mov     callback._gs,gs
      mov     callback._fs,fs


      push    es
      mov     ax,data32_idx
      mov     es,ax
      mov     al,es:[SaveInt_temp]
      pop     es

      int     20h             ;simulate real int

      ;restore the interrupts

      mov     eax,callback._eax
      mov     ebx,callback._ebx
      mov     ecx,callback._ecx
      mov     edx,callback._edx

      push    callback._flgs
      popf                     ;don't care but what the hell?

      mov     edi,callback._edi
      mov     esi,callback._esi

      mov     esp,callback._esp
      mov     ebp,callback._ebp

      mov     es,callback._es
      mov     ds,callback._ds
      mov     gs,callback._gs
      mov     fs,callback._fs

      iretd


      ;--------------- IRQs Handlers ---------------
      ;
      ;IRQs normally redirected to their default
      ;real mode handlers.

_IRQ_0:
      push    ax
      mov     ax,8
      jmp     irq86

_IRQ_1:
      push    ax
      mov     ax,9
      jmp     irq86
_IRQ_2:
      push    ax
      mov     ax,10
      jmp     irq86
_IRQ_3:
      push    ax
      mov     ax,11
      jmp     irq86
_IRQ_4:
      push    ax
      mov     ax,12
      jmp     irq86
_IRQ_5:
      push    ax
      mov     ax,13
      jmp     irq86
_IRQ_6:
      push    ax
      mov     ax,14
      jmp     irq86
_IRQ_7:
      push    ax
      mov     ax,15
      jmp     irq86
_IRQ_8:
      push    ax
      mov     ax,70h
      jmp     irq86
_IRQ_9:
      push    ax
      mov     ax,71h
      jmp     irq86

_IRQ_10:
      push    ax
      mov     ax,72h
      jmp     irq86
_IRQ_11:
      push    ax
      mov     ax,73h
      jmp     irq86
_IRQ_12:
      push    ax
      mov     ax,74h
      jmp     irq86
_IRQ_13:
      push    ax
      mov     ax,75h
      jmp     irq86
_IRQ_14:
      push    ax
      mov     ax,76h
      jmp     irq86
_IRQ_15:
      push    ax
      mov     ax,77h
IRQ86:
      int     20h
      pop     ax
      iretd


code32 ends

Code32end segment para public use16

;-----  Here goes nothing. I use it just as a marker.

Code32end ends

       end start16





;                                                 G.Papaioannou
;                                                     20/1/98



