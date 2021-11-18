# MVM
## MVM Architecture
List of available instructions:
| Op. code | Op. mnemo  | Description                                                                    |
|:--------:|:-----------|:-------------------------------------------------------------------------------|
| ``0x00`` | ``NOP``    | No operation                                                                   |
| ``0x01`` | ``HALT``   | Stop the program execution                                                     |
| ``0x02`` | ``PUSH x`` | Push number to stack                                                           |
| ``0x03`` | ``POP``    | Pop number from a stack                                                        |
| ``0x04`` | ``DUP``    | Duplicate the top stack element                                                |
| ``0x05`` | ``SWAP``   | Swap first and second stack elements                                           |
| ``0x06`` | ``ADD``    | Pop 1st and 2nd stack elements, add them and push to stack                     |
| ``0x07`` | ``SUB``    | Pop 1st and 2nd stack elements, subtract them and push to stack                |
| ``0x08`` | ``MUL``    | Pop 1st and 2nd stack elements, multiply them and push to stack                |
| ``0x09`` | ``DIV``    | Pop 1st and 2nd stack elements, divide them and push to stack                  |
| ``0x0A`` | ``NEG``    | Negate the top stack element                                                   |
| ``0x0B`` | ``NOT``    | Pop top stack element. If it's equal to ``0`` then push ``1``, else push ``0`` |
| ``0x0C`` | ``CALL x`` | Jump to the ``x`` address and push new stack frame                             |
| ``0x0D`` | ``RET``    | Return to the caller and pop the stack frame                                   |
| ``0x0E`` | ``JMP x``  | Perform the unconditional jump to ``x`` address                                |
| ``0x0F`` | ``JE x``   | Jump to ``x`` **if** top element ``== 0``                                      |
| ``0x10`` | ``JNE x``  | Jump to ``x`` **if** top element ``!= 0``                                      |
| ``0x11`` | ``JG x``   | Jump to ``x`` **if** top element ``> 0 ``                                      |
| ``0x12`` | ``JL x``   | Jump to ``x`` **if** top element ``< 0 ``                                      |
| ``0x13`` | ``JGE x``  | Jump to ``x`` **if** top element ``>= 0``                                      |
| ``0x14`` | ``JLE x``  | Jump to ``x`` **if** top element ``<= 0``                                      |
| ``0x15`` | ``LDA x``  | Push local variable to stack                                                   |
| ``0x16`` | ``IN``     | Read input from stdin                                                          |
| ``0x17`` | ``OUT``    | Put top stack value to stdout as char                                          |
| ``0x18`` | ``CLR x``  | Wipe out ``x`` values before the top value from the stack                      |
| ``0x19`` | ``OVER``   | Duplicate and push the second value from the top                               |
| ``0x1A`` | ``LDL x``  | Lift the ``x`` from the _fp_ variable to the top of the stack                  |
| ``0x1B`` | ``STL x``  | Store the top stack value under the ``x`` from the _fp_ variable               |

## Example
### Example 1
The `2*3+5` formula written as the MVM assembly code:
```asm
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
```
The result of execution with the `debug = True` flag:
```
VM {_pc = 0, _fp = -1, _stack = fromList [], _halt = False, _debug = True}
0: Push 2
VM {_pc = 2, _fp = -1, _stack = fromList [2], _halt = False, _debug = True}
2: Push 3
VM {_pc = 4, _fp = -1, _stack = fromList [3,2], _halt = False, _debug = True}
4: Call 21
VM {_pc = 21, _fp = 2, _stack = fromList [6,-1,3,2], _halt = False, _debug = True}
21: Ld 0
VM {_pc = 23, _fp = 2, _stack = fromList [3,6,-1,3,2], _halt = False, _debug = True}
23: Ld 1
VM {_pc = 25, _fp = 2, _stack = fromList [2,3,6,-1,3,2], _halt = False, _debug = True}
25: Mul 
VM {_pc = 26, _fp = 2, _stack = fromList [6,6,-1,3,2], _halt = False, _debug = True}
26: Ret 
VM {_pc = 6, _fp = -1, _stack = fromList [6,3,2], _halt = False, _debug = True}
6: Clr 2
VM {_pc = 8, _fp = -1, _stack = fromList [6], _halt = False, _debug = True}
8: Push 5
VM {_pc = 10, _fp = -1, _stack = fromList [5,6], _halt = False, _debug = True}
10: Call 15
VM {_pc = 15, _fp = 2, _stack = fromList [12,-1,5,6], _halt = False, _debug = True}
15: Ld 0
VM {_pc = 17, _fp = 2, _stack = fromList [5,12,-1,5,6], _halt = False, _debug = True}
17: Ld 1
VM {_pc = 19, _fp = 2, _stack = fromList [6,5,12,-1,5,6], _halt = False, _debug = True}
19: Add 
VM {_pc = 20, _fp = 2, _stack = fromList [11,12,-1,5,6], _halt = False, _debug = True}
20: Ret 
VM {_pc = 12, _fp = -1, _stack = fromList [11,5,6], _halt = False, _debug = True}
12: Clr 2
VM {_pc = 14, _fp = -1, _stack = fromList [11], _halt = False, _debug = True}
14: Halt 


Done:
VM {_pc = 14, _fp = -1, _stack = fromList [11], _halt = True, _debug = True}

```
### Example 2
The base I/O example - simple echo:
```asm
read:  in
       dup
       out
       push 0x0A
       sub       
       jne &read ; loop until the input != new line (0x0A)
       halt
```
The execution for the input string `Hello, world!`:
```
Hello, world!


Done:
VM {_pc = 8, _fp = -1, _stack = fromList [], _halt = True, _debug = False}
```
### Example 3
The power (`2^10`) computation - loop variant
```asm
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
```
The result of execution:
```
Done:
VM {_pc = 8, _fp = -1, _stack = fromList [1024], _halt = True, _debug = False}
```
### Example 4
The power (`2^10`) computation - recursive variant
```asm
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
```
The result of execution:
```
Done:
VM {_pc = 8, _fp = -1, _stack = fromList [1024], _halt = True, _debug = False}
```
### Example 5
The `N`-th element of Fibbonaci sequence - recursive variant
```asm
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
```
The result of execution:
```
Done:
VM {_pc = 6, _fp = -1, _stack = fromList [13], _halt = True, _debug = False}
```