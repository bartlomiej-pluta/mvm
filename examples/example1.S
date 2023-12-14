; Evaluate 2*3+5
;
; pseudocode:
; sum(prd(2, 3), 5)
;
; sum(a, b) {
;   return a+b
; }
;
; prd(a, b) {
;   return a*b;
; }

main: push 2
      push 3
      call &prd
      clr 2
      push 5
      call &sum
      clr 2
      halt

sum:  lda 0
      lda 1
      add
      ret

prd:  lda 0
      lda 1
      mul
      ret
