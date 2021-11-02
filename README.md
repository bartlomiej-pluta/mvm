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
| ``0x15`` | ``LD x``   | Push local variable to stack                                                   |