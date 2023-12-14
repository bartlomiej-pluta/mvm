; Evaluate 2^10 - loop variant

push 2
push 10
call &pow
clr 2
halt

pow:   lda 1      ; base
       lda 0      ; exp
       push 1     ; acc

                  ;               | Stack:
.loop: ldl 1      ; if exp == 0   | exp
       je &.done  ; then return   | exp
       pop        ;               |
                  ;               | 
       ldl 2      ; Evaluate      | acc
       ldl 0      ; next power    | acc      base
       mul        ;               | acc*base
       stl 2      ;               | 
                  ;               |
       ldl 1      ; Decrement exp | exp
       push 1     ;               | exp      1
       sub        ;               | exp-1
       stl 1      ;               |
       jmp &.loop ;               |

.done: ldl 2      ;               | ...      acc
       ret        ;               | acc
