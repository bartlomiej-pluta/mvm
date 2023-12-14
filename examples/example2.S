; I/O example - simple echo

read:  in
       dup
       out
       push 0x0A
       sub       
       jne &read ; loop until the input != new line (0x0A)
       halt
