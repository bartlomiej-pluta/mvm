; Evaluate 2^10 - recursive variant

push 2          ; base
push 10         ; exp
call &pow
clr 2
halt

pow:   lda 1     ; base
       lda 0     ; exp

       ldl 1     ; push exp to top
       je &.edge ; the edge case: if exp == 0  then return 1
       pop       ; pop exp

                 ; | Stack:
       ldl 0     ; | base
       ldl 1     ; | base               exp
       push 1    ; | base               exp           1
       sub       ; | base               exp-1
       call &pow ; | base               exp-1         base^(exp-1)]
       clr 1     ; | base               base^(exp-1)
       mul       ; | base*base^(exp-1)
       ret       ; | base*base^(exp-1)
       
.edge: pop    
       push 1    ; return 1
       ret      
