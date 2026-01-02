; Assembly code that demonstrates how pnut's single pass compiler works.
; This example implements mutually recursive functions odd() and even() using
; indirect jumps via a global offset table that is initialized at startup.
; The main function calls even(12) and returns the result as the exit code.
; To compile: nasm -f bin -o a.out doc/single-pass.s
; To run:     chmod +x a.out; ./a.out; echo $?

BITS 32
  org     0x08048000
ehdr:                                     ; Elf32_Ehdr
  db      0x7F, "ELF", 1, 1, 1, 0         ;   e_ident
times 8 db      0
  dw      2                               ;   e_type
  dw      3                               ;   e_machine
  dd      1                               ;   e_version
  dd      _start                          ;   e_entry
  dd      phdr - $$                       ;   e_phoff
  dd      0                               ;   e_shoff
  dd      0                               ;   e_flags
  dw      ehdrsize                        ;   e_ehsize
  dw      phdrsize                        ;   e_phentsize
  dw      1                               ;   e_phnum
  dw      0                               ;   e_shentsize
  dw      0                               ;   e_shnum
  dw      0                               ;   e_shstrndx

ehdrsize      equ     $ - ehdr            ; Compute header size

phdr:                                     ; Elf32_Phdr
  dd      1                         ;   p_type
  dd      0                         ;   p_offset
  dd      $$                        ;   p_vaddr
  dd      $$                        ;   p_paddr
  dd      filesize                  ;   p_filesz
  dd      filesize                  ;   p_memsz
  dd      5                         ;   p_flags
  dd      0x1000                    ;   p_align

phdrsize      equ     $ - phdr            ; Compute program header size

; Malloc some memory for the forward jump table and global variables
allocate_globals:
  push    ebx               ; Save address of global variables table
  mov     eax, 192
  mov     ebx, 0
  mov     ecx, 1024
  mov     edx, 0x3
  mov     esi, 0x22
  mov     edi, -1
  mov     ebp, 0
  int     0x80
  pop     ebx
  ret

_start:
  call    allocate_globals  ; Allocate space for global variables.
  mov     ebx, eax          ; Store the address of globals in ebx
  jmp     setup_odd_jte     ; Jump to first initialization code

; odd(int n) { return n == 0 ? 0 : even(n - 1); }
odd:
  mov     eax, [esp + 4]    ; Get arg from the stack
  cmp     eax, 0            ; Compare to 0
  je      .odd_true         ; If 0, goto odd_true
  dec     eax               ; Otherwise, decrement then call even
  push    eax
  ; call    even              ; Compute even(n - 1), result is in eax
  ; Load even's address from forward jump table
  mov     edx, [ebx + 4]    ; Load even jump table entry
  call    edx               ; Call even function
  add     esp, 4            ; Remove call params
  ret                       ; Return

.odd_true:
  mov     eax, 0            ; 0 is not odd
  ret

setup_odd_jte:              ; Setup odd forward jump table entry
  call    .get_eip
.get_eip:
  pop     edx               ; Get EIP = start of pop instruction
  add     edx, (odd - $)    ; Add address offset of odd function
  add     edx, 1            ; Adjust for instruction size
  mov     [ebx+0], edx      ; Initialize odd jump table entry
  jmp     setup_even_jte    ; Continue with next setup code

; even(int n) { return n == 0 ? 1 : odd(n - 1); }
even:
  mov     eax, [esp + 4]    ; Get arg from the stack
  cmp     eax, 0            ; Compare to 0
  je      .even_true        ; If 0, goto .even_true
  dec     eax               ; Otherwise, decrement then call even
  push    eax
  ; No need to use odd's forward jump entry, since we know the address of the function
  call    odd               ; Compute even(n-1), result is in eax
  add     esp, 4            ; Remove call params
  ret                       ; Return

.even_true:
  mov     eax, 1            ; 0 is even
  ret

setup_even_jte:             ; Setup even forward jump table entry
  call    .get_eip
.get_eip:
  pop     edx               ; Get EIP = start of pop instruction
  add     edx, (even - $)   ; Add address offset of even function
  add     edx, 1            ; Adjust for instruction size
  mov     [ebx+4], edx      ; Initialize even jump table entry
  jmp     setup3            ; Continue with next setup code

; int main() { return even(12); }
main:
  push    12
  call    even
  mov     ebx, eax          ; exit code = even(12)
  mov     eax, 1            ; exit syscall
  int     0x80

setup3:                     ; No more setup, run the main function
  call    main              ; Call the main function


filesize      equ     $ - $$
