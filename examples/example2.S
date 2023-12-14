; I/O example - simple echo
;
; pseudocode:
; do {                 // read
;   x = in()
;   out(x)
; while(x - 0x0A != 0)

read:  in
       dup
       out
       push 0x0A
       sub
       jne &read ; loop until the input != new line (0x0A)
       halt
