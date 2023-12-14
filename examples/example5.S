; The N-th element of Fibbonaci sequence - recursive variant
;
; pseudocode:
; fibb(n) {
;   if (n == 0) {
;     return 1                    // .done0
;   }
;
;   if ((n-1) == 0) {
;     return 1                    // .done1
;   }
;
;   return fibb(n-2) + fibb(n-1)
; }

push 6
call &fibb
clr 1
halt

fibb:   lda 0       ; n                  | Stack:
        ldl 0       ; n == 0 -> return 1 | n
        je &.done0  ;                    | n
        pop         ;                    |
        ldl 0       ; n == 1 -> return 1 | n
        push 1      ;                    | n   1
        sub         ;                    | n-1
        je &.done1  ;                    | n-1
        dup         ; Evaluate fibb      | n-1 n-1
        push 1      ;                    | n-1 n-1           1
        sub         ;                    | n-1 n-2
        call &fibb  ;                    | n-1 n-2           f(n-2)
        clr 1       ;                    | n-1 f(n-2)
        over        ;                    | n-1 f(n-2)        n-1
        call &fibb  ;                    | n-1 f(n-2)        n-1     f(n-1)
        clr 1       ;                    | n-1 f(n-2)        f(n-1)
        add         ;                    | n-1 f(n-2)+f(n-1)
        ret

.done1: pop
        push 1
        ret

.done0: pop
        push 1
        ret
