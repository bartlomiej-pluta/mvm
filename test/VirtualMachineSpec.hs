module VirtualMachineSpec where

import Test.Hspec
import qualified Data.Sequence as S


import VirtualMachine.VM (VM(..), empty)
import Runner (run, exec)

done :: [Int] -> Int -> Int -> Either String VM
done stack pc fp = return $ empty { _stack = S.fromList stack, _pc = pc, _fp = fp, _halt = True }

spec :: Spec
spec = do
  describe "nop" $ do
    it "does do nothing" $ do
      let input = " nop  \n\
                  \ halt "
      let expected = done [] 1 (-1)
      actual <- run input
      actual `shouldBe` expected
  
  describe "halt" $ do
    it "shutdowns the VM" $ do
      let input = "halt"
      let expected = done [] 0 (-1)
      actual <- run input
      actual `shouldBe` expected      
    it "throws an error if halt instruction is not encountered and program exits" $ do
      let input = " nop \n\
                  \ nop "
      let expected = Left "PC (=2) exceeds program size (=2)"
      actual <- run input
      actual `shouldBe` expected        

  describe "push" $ do
    it "supports push" $ do
      let input = " push 1 \n\
                  \ halt   "
      let expected = done [1] 2 (-1)
      actual <- run input
      actual `shouldBe` expected
    it "supports pushing in the correct order" $ do
      let input = " push 1 \n\
                  \ push 2 \n\
                  \ push 3 \n\
                  \ push 4 \n\
                  \ halt   "
      let expected = done [4, 3, 2, 1] 8 (-1)
      actual <- run input
      actual `shouldBe` expected   
  
  describe "pop" $ do
    it "supports pop" $ do
      let input = " push 1 \n\
                  \ pop    \n\
                  \ halt   "
      let expected = done [] 3 (-1)
      actual <- run input
      actual `shouldBe` expected
    it "supports poping in correct order" $ do
      let input = " push 1 \n\
                  \ push 2 \n\
                  \ push 3 \n\
                  \ push 4 \n\
                  \ pop    \n\
                  \ pop    \n\
                  \ halt   "
      let expected = done [2, 1] 10 (-1)
      actual <- run input
      actual `shouldBe` expected
    it "raises error if empty stack" $ do
      let input = " pop  \n\
                  \ halt "
      let expected = Left "Attempt to pop from empty stack: tried to pop 1 elements, got 0"
      actual <- run input
      actual `shouldBe` expected     

  describe "dup" $ do
    it "duplicates the top stack value" $ do
      let input = " push 1 \n\
                  \ push 2 \n\
                  \ push 3 \n\
                  \ dup    \n\
                  \ halt   "
      let expected = done [3, 3, 2, 1] 7 (-1)
      actual <- run input
      actual `shouldBe` expected  
    it "raises error if empty stack" $ do
      let input = " dup  \n\
                  \ halt "
      let expected = Left "Attempt to pop from empty stack: tried to pop 1 elements, got 0"
      actual <- run input
      actual `shouldBe` expected       

  describe "swap" $ do
    it "swaps the top 2 stack values" $ do
      let input = " push 1 \n\
                  \ push 2 \n\
                  \ push 3 \n\
                  \ push 4 \n\
                  \ push 5 \n\
                  \ swap   \n\
                  \ halt   "
      let expected = done [4, 5, 3, 2, 1] 11 (-1)
      actual <- run input
      actual `shouldBe` expected  
    it "raises error if empty stack" $ do
      let input = " swap \n\
                  \ halt "
      let expected = Left "Attempt to pop from empty stack: tried to pop 2 elements, got 0"
      actual <- run input
      actual `shouldBe` expected           
    it "raises error if stack is not big enough" $ do
      let input = " push 5 \n\
                  \ swap   \n\
                  \ halt   "
      let expected = Left "Attempt to pop from empty stack: tried to pop 2 elements, got 1"
      actual <- run input
      actual `shouldBe` expected      

  describe "add" $ do
    it "sums the top 2 stack values" $ do
      let input = " push 7 \n\
                  \ push 4 \n\
                  \ add    \n\
                  \ halt   "
      let expected = done [7+4] 5 (-1)
      actual <- run input
      actual `shouldBe` expected        
    it "can be composed" $ do
      let input = " push 4  \n\
                  \ push 7  \n\
                  \ push 10 \n\
                  \ push 3  \n\
                  \ add     \n\
                  \ add     \n\
                  \ add     \n\
                  \ halt    "
      let expected = done [4+(7+(10+3))] 11 (-1)
      actual <- run input
      actual `shouldBe` expected      
    it "raises error if empty stack" $ do
      let input = " add  \n\
                  \ halt "
      let expected = Left "Attempt to pop from empty stack: tried to pop 2 elements, got 0"
      actual <- run input
      actual `shouldBe` expected           
    it "raises error if stack is not big enough" $ do
      let input = " push 5\n\
                  \ add   \n\
                  \ halt  "
      let expected = Left "Attempt to pop from empty stack: tried to pop 2 elements, got 1"
      actual <- run input
      actual `shouldBe` expected        

  describe "sub" $ do
    it "substracts the top 2 stack values" $ do
      let input = " push 7 \n\
                  \ push 4 \n\
                  \ sub    \n\
                  \ halt   "
      let expected = done [7-4] 5 (-1)
      actual <- run input
      actual `shouldBe` expected        
    it "can be composed" $ do
      let input = " push 4  \n\
                  \ push 7  \n\
                  \ push 10 \n\
                  \ push 3  \n\
                  \ sub     \n\
                  \ sub     \n\
                  \ sub     \n\
                  \ halt    "
      let expected = done [4-(7-(10-3))] 11 (-1)
      actual <- run input
      actual `shouldBe` expected      
    it "raises error if empty stack" $ do
      let input = " sub  \n\
                  \ halt "
      let expected = Left "Attempt to pop from empty stack: tried to pop 2 elements, got 0"
      actual <- run input
      actual `shouldBe` expected           
    it "raises error if stack is not big enough" $ do
      let input = " push 5 \n\
                  \ sub    \n\
                  \ halt   "
      let expected = Left "Attempt to pop from empty stack: tried to pop 2 elements, got 1"
      actual <- run input
      actual `shouldBe` expected   

  describe "mul" $ do
    it "multiplies the top 2 stack values" $ do
      let input = " push 7 \n\
                  \ push 4 \n\
                  \ mul    \n\
                  \ halt   "
      let expected = done [7*4] 5 (-1)
      actual <- run input
      actual `shouldBe` expected        
    it "can be composed" $ do
      let input = " push 4  \n\
                  \ push 7  \n\
                  \ push 10 \n\
                  \ push 3  \n\
                  \ mul     \n\
                  \ mul     \n\
                  \ mul     \n\
                  \ halt    "
      let expected = done [4*(7*(10*3))] 11 (-1)
      actual <- run input
      actual `shouldBe` expected      
    it "raises error if empty stack" $ do
      let input = " mul  \n\
                  \ halt "
      let expected = Left "Attempt to pop from empty stack: tried to pop 2 elements, got 0"
      actual <- run input
      actual `shouldBe` expected           
    it "raises error if stack is not big enough" $ do
      let input = " push 5 \n\
                  \ mul    \n\
                  \ halt   "
      let expected = Left "Attempt to pop from empty stack: tried to pop 2 elements, got 1"
      actual <- run input
      actual `shouldBe` expected    

  describe "div" $ do
    it "divides the top 2 stack values" $ do
      let input = " push 7 \n\
                  \ push 4 \n\
                  \ div    \n\
                  \ halt   "
      let expected = done [7 `div` 4] 5 (-1)
      actual <- run input
      actual `shouldBe` expected   
    it "can be composed" $ do
      let input = " push 4  \n\
                  \ push 7  \n\
                  \ push 10 \n\
                  \ push 3  \n\
                  \ div     \n\
                  \ div     \n\
                  \ div     \n\
                  \ halt    "
      let expected = done [4 `div` (7 `div` (10 `div` 3))] 11 (-1)
      actual <- run input
      actual `shouldBe` expected      
    it "raises error if empty stack" $ do
      let input = " div  \n\
                  \ halt "
      let expected = Left "Attempt to pop from empty stack: tried to pop 2 elements, got 0"
      actual <- run input
      actual `shouldBe` expected           
    it "raises error if stack is not big enough" $ do
      let input = " push 5 \n\
                  \ div    \n\
                  \ halt   "
      let expected = Left "Attempt to pop from empty stack: tried to pop 2 elements, got 1"
      actual <- run input
      actual `shouldBe` expected  

  describe "neg" $ do
    it "negates the top stack value" $ do
      let input = " push 4 \n\
                  \ neg    \n\
                  \ halt   "
      let expected = done [-4] 3 (-1)
      actual <- run input
      actual `shouldBe` expected   
    it "the od number of 'neg' instructions should negate the value" $ do
      let input = " push 4 \n\
                  \ neg    \n\
                  \ neg    \n\
                  \ neg    \n\
                  \ neg    \n\
                  \ neg    \n\
                  \ neg    \n\
                  \ halt   "
      let expected = done [4] 8 (-1)
      actual <- run input
      actual `shouldBe` expected         
    it "the even number of 'neg' instructions should not change the value" $ do
      let input = " push 4 \n\
                  \ neg    \n\
                  \ neg    \n\
                  \ neg    \n\
                  \ neg    \n\
                  \ neg    \n\                  
                  \ halt   "
      let expected = done [-4] 7 (-1)
      actual <- run input
      actual `shouldBe` expected   
    it "raises error if empty stack" $ do
      let input = " neg  \n\
                  \ halt "
      let expected = Left "Attempt to pop from empty stack: tried to pop 1 elements, got 0"
      actual <- run input
      actual `shouldBe` expected         

  describe "not" $ do
    it "negates the 'true' value" $ do
      let input = " push 1 \n\
                  \ not    \n\
                  \ halt   "
      let expected = done [0] 3 (-1)
      actual <- run input
      actual `shouldBe` expected   
    it "negates the 'false' value" $ do
      let input = " push 0 \n\
                  \ not    \n\
                  \ halt   "
      let expected = done [1] 3 (-1)
      actual <- run input
      actual `shouldBe` expected         
    it "negates any non-zero value to false" $ do
      let input = " push 129 \n\
                  \ not      \n\
                  \ halt     "
      let expected = done [0] 3 (-1)
      actual <- run input
      actual `shouldBe` expected          
    it "the od number of 'neg' instructions should negate the logic value" $ do
      let input = " push 1 \n\
                  \ not    \n\
                  \ not    \n\
                  \ not    \n\
                  \ not    \n\
                  \ not    \n\
                  \ not    \n\
                  \ halt   "
      let expected = done [1] 8 (-1)
      actual <- run input
      actual `shouldBe` expected         
    it "the even number of 'neg' instructions should not change the value" $ do
      let input = " push 1 \n\
                  \ not    \n\
                  \ not    \n\
                  \ not    \n\
                  \ not    \n\
                  \ not    \n\                  
                  \ halt   "
      let expected = done [0] 7 (-1)
      actual <- run input
      actual `shouldBe` expected   
    it "raises error if empty stack" $ do
      let input = " not  \n\
                  \ halt "
      let expected = Left "Attempt to pop from empty stack: tried to pop 1 elements, got 0"
      actual <- run input
      actual `shouldBe` expected            

  describe "jmp" $ do
    it "changes the program counter register" $ do
      let input = " nop   \n\
                  \ jmp 4 \n\
                  \ halt  \n\
                  \ nop   \n\
                  \ nop   \n\
                  \ jmp 3 "
      let expected = done [] 3 (-1)
      actual <- run input
      actual `shouldBe` expected        
    it "skips instructions after jump" $ do
      let input = " nop    \n\
                  \ jmp 11 \n\
                  \ push 1 \n\
                  \ push 2 \n\
                  \ push 3 \n\
                  \ push 4 \n\
                  \ halt   "
      let expected = done [] 11 (-1)
      actual <- run input
      actual `shouldBe` expected  
    it "supports jumping by labels" $ do
      let input = " nop            \n\
                  \ jmp &end       \n\
                  \ stop:          \n\
                  \ halt           \n\
                  \ push 1         \n\
                  \ push 2         \n\
                  \ push 3         \n\
                  \ push 4         \n\
                  \ end: jmp &stop "
      let expected = done [] 3 (-1)
      actual <- run input
      actual `shouldBe` expected  

  describe "je" $ do
    it "jumps if top stack value == 0" $ do
      let input = " push 0     \n\
                  \ je &jumped \n\
                  \ push 0     \n\
                  \ halt       \n\
                  \ jumped:    \n\
                  \ push 1     \n\
                  \ halt       "
      let expected = done [1] 9 (-1)
      actual <- run input
      actual `shouldBe` expected  
    it "proceeds if top stack value != 0" $ do
      let input = " push 1     \n\
                  \ je &jumped \n\
                  \ push 0     \n\
                  \ halt       \n\
                  \ jumped:    \n\
                  \ push 1     \n\
                  \ halt       "
      let expected = done [0] 6 (-1)
      actual <- run input
      actual `shouldBe` expected   
    it "raises error if empty stack" $ do
      let input = " je 0 \n\
                  \ halt "
      let expected = Left "Attempt to pop from empty stack: tried to pop 1 elements, got 0"
      actual <- run input
      actual `shouldBe` expected  
             
  describe "jne" $ do
    it "jumps if top stack value != 0" $ do
      let input = " push 4      \n\
                  \ jne &jumped \n\
                  \ push 0      \n\
                  \ halt        \n\
                  \ jumped:     \n\
                  \ push 1      \n\
                  \ halt        "
      let expected = done [1] 9 (-1)
      actual <- run input
      actual `shouldBe` expected  
    it "proceeds if top stack value == 0" $ do
      let input = " push 0      \n\
                  \ jne &jumped \n\
                  \ push 0      \n\
                  \ halt        \n\
                  \ jumped:     \n\
                  \ push 1      \n\
                  \ halt        "
      let expected = done [0] 6 (-1)
      actual <- run input
      actual `shouldBe` expected   
    it "raises error if empty stack" $ do
      let input = " jne 0 \n\
                  \ halt  "
      let expected = Left "Attempt to pop from empty stack: tried to pop 1 elements, got 0"
      actual <- run input
      actual `shouldBe` expected  

  describe "jg" $ do
    it "jumps if top stack value > 0" $ do
      let input = " push 1     \n\
                  \ jg &jumped \n\
                  \ push 0     \n\
                  \ halt       \n\
                  \ jumped:    \n\
                  \ push 1     \n\
                  \ halt       "
      let expected = done [1] 9 (-1)
      actual <- run input
      actual `shouldBe` expected  
    it "proceeds if top stack value == 0" $ do
      let input = " push 0     \n\
                  \ jg &jumped \n\
                  \ push 0     \n\
                  \ halt       \n\
                  \ jumped:    \n\
                  \ push 1     \n\
                  \ halt       "
      let expected = done [0] 6 (-1)
      actual <- run input
      actual `shouldBe` expected   
    it "proceeds if top stack value < 0" $ do
      let input = " push 1     \n\
                  \ neg        \n\
                  \ jg &jumped \n\
                  \ push 0     \n\
                  \ halt       \n\
                  \ jumped:    \n\
                  \ push 1     \n\
                  \ halt       "
      let expected = done [0] 7 (-1)
      actual <- run input
      actual `shouldBe` expected   
    it "raises error if empty stack" $ do
      let input = " jg 0 \n\
                  \ halt "
      let expected = Left "Attempt to pop from empty stack: tried to pop 1 elements, got 0"
      actual <- run input
      actual `shouldBe` expected    
  
  describe "jl" $ do
    it "proceeds if top stack value > 0" $ do
      let input = " push 1     \n\
                  \ jl &jumped \n\
                  \ push 0     \n\
                  \ halt       \n\
                  \ jumped:    \n\
                  \ push 1     \n\
                  \ halt       "
      let expected = done [0] 6 (-1)
      actual <- run input
      actual `shouldBe` expected  
    it "proceeds if top stack value == 0" $ do
      let input = " push 0     \n\
                  \ jl &jumped \n\
                  \ push 0     \n\
                  \ halt       \n\
                  \ jumped:    \n\
                  \ push 1     \n\
                  \ halt       "
      let expected = done [0] 6 (-1)
      actual <- run input
      actual `shouldBe` expected   
    it "jumps if top stack value < 0" $ do
      let input = " push 1     \n\
                  \ neg        \n\
                  \ jl &jumped \n\
                  \ push 0     \n\
                  \ halt       \n\
                  \ jumped:    \n\
                  \ push 1     \n\
                  \ halt       "
      let expected = done [1] 10 (-1)
      actual <- run input
      actual `shouldBe` expected   
    it "raises error if empty stack" $ do
      let input = " jl 0 \n\
                  \ halt "
      let expected = Left "Attempt to pop from empty stack: tried to pop 1 elements, got 0"
      actual <- run input
      actual `shouldBe` expected 
         
  describe "jge" $ do
    it "jumps if top stack value > 0" $ do
      let input = " push 1      \n\
                  \ jge &jumped \n\
                  \ push 0      \n\
                  \ halt        \n\
                  \ jumped:     \n\
                  \ push 1      \n\
                  \ halt        "
      let expected = done [1] 9 (-1)
      actual <- run input
      actual `shouldBe` expected  
    it "jumps if top stack value == 0" $ do
      let input = " push 0      \n\
                  \ jge &jumped \n\
                  \ push 0      \n\
                  \ halt        \n\
                  \ jumped:     \n\
                  \ push 1      \n\
                  \ halt        "
      let expected = done [1] 9 (-1)
      actual <- run input
      actual `shouldBe` expected   
    it "proceeds if top stack value < 0" $ do
      let input = " push 1      \n\
                  \ neg         \n\
                  \ jge &jumped \n\
                  \ push 0      \n\
                  \ halt        \n\
                  \ jumped:     \n\
                  \ push 1      \n\
                  \ halt        "
      let expected = done [0] 7 (-1)
      actual <- run input
      actual `shouldBe` expected   
    it "raises error if empty stack" $ do
      let input = " jge 0 \n\
                  \ halt  "
      let expected = Left "Attempt to pop from empty stack: tried to pop 1 elements, got 0"
      actual <- run input
      actual `shouldBe` expected

  describe "jle" $ do
    it "proceeds if top stack value > 0" $ do
      let input = " push 1      \n\
                  \ jle &jumped \n\
                  \ push 0      \n\
                  \ halt        \n\
                  \ jumped:     \n\
                  \ push 1      \n\
                  \ halt        "
      let expected = done [0] 6 (-1)
      actual <- run input
      actual `shouldBe` expected  
    it "jumps if top stack value == 0" $ do
      let input = " push 0      \n\
                  \ jle &jumped \n\
                  \ push 0      \n\
                  \ halt        \n\
                  \ jumped:     \n\
                  \ push 1      \n\
                  \ halt        "
      let expected = done [1] 9 (-1)
      actual <- run input
      actual `shouldBe` expected   
    it "jumps if top stack value < 0" $ do
      let input = " push 1      \n\
                  \ neg         \n\
                  \ jle &jumped \n\
                  \ push 0      \n\
                  \ halt        \n\
                  \ jumped:     \n\
                  \ push 1      \n\
                  \ halt        "
      let expected = done [1] 10 (-1)
      actual <- run input
      actual `shouldBe` expected   
    it "raises error if empty stack" $ do
      let input = " jle 0 \n\
                  \ halt  "
      let expected = Left "Attempt to pop from empty stack: tried to pop 1 elements, got 0"
      actual <- run input
      actual `shouldBe` expected 

  describe "call" $ do
    it "pushes return address and previous frame pointer to the stack" $ do
      let input = " nop       \n\
                  \ nop       \n\
                  \ call &fun \n\
                  \ nop       \n\
                  \ halt      \n\
                  \ fun: halt "
      --                       ┌────┬─ fp points to the beginning of active stack frame
      -- stack indexes:    1   0    │
      --                       │    │
      let expected = done [4, -1] 6 0
      --                   ├───┤
      --                   │   │
      --                   │   └─ previous fp
      --                   └───── return address (points to the next instruction after call)      
      actual <- run input
      actual `shouldBe` expected 
    it "pushes stack frames in correct order for multiple calls" $ do
      let input = "    nop     \n\
                  \    call &f \n\
                  \    halt    \n\
                  \ f: nop     \n\
                  \    push 1  \n\
                  \    nop     \n\
                  \    call &g \n\
                  \    halt    \n\
                  \ g: nop     \n\
                  \    call &h \n\
                  \ h: halt    "
      --                       ┌──────────────────────┬─ fp points to the beginning of active stack frame
      -- stack indexes:     6  5   4  3  2  1   0     │
      --                       │                      │
      let expected = done [14, 3, 10, 0, 1, 3, -1] 14 5
      --                    ├──┤   ├──┤  │  ├───┤
      --                    │  │   │  │  │  │   │
      --                    │  │   │  │  │  └───┴─ call &f   
      --                    │  │   │  │  └──────── push 1  
      --                    │  │   └──┴─────────── call &g     
      --                    └──┴────────────────── call &h
      actual <- run input
      actual `shouldBe` expected 
  
  describe "ret" $ do
    it "unwinds the stack frame and jumps to the caller" $ do
      let input = "    nop     \n\
                  \    call &f \n\
                  \    halt    \n\
                  \ f: nop     \n\
                  \    ret     \n"
      let expected = done [] 3 (-1)
      actual <- run input
      actual `shouldBe` expected 
    it "leaves the top stack value as a 'return' value from the function" $ do
      let input = "    nop     \n\
                  \    call &f \n\
                  \    halt    \n\
                  \ f: nop     \n\
                  \    push 4  \n\
                  \    ret     \n"
      --                   ┌──── this is the top stack value at the time 'ret' instruction being invoked
      let expected = done [4] 3 (-1)
      actual <- run input
      actual `shouldBe` expected 
    it "unwinds the stack frames in the correct order for multiple calls" $ do
      let input = "    nop     \n\
                  \    call &f \n\
                  \    halt    \n\
                  \ f: nop     \n\
                  \    call &g \n\
                  \    push 2  \n\
                  \    mul     \n\                  
                  \    ret     \n\
                  \ g: nop     \n\
                  \    call &h \n\
                  \    push 4  \n\
                  \    add     \n\
                  \    ret     \n\
                  \ h: push 6  \n\
                  \    ret     \n"
      --                    ┌─────   h: _ -> 6      │ = 6
      --                    │        g: _ -> h + 4  │ = 10 = 6 + 4
      --                    │        f: _ -> h * 2  │ = 20 = 10 * 2
      let expected = done [20] 3 (-1)
      --                           └───── fp = -1 means the root program: there are no stack frames on the stack
      actual <- run input
      actual `shouldBe` expected 
    it "raises error if there is no previous fp on the stack (stack is empty)" $ do
      let vm = empty { _stack = S.fromList [], _fp = 0 }
      let input = " ret \n\
                  \ halt  "
      let expected = Left "Cannot determine previous frame pointer (fp)"      
      actual <- exec vm input
      actual `shouldBe` expected  
    it "raises error if there is no return address on the stack (stack size is 1)" $ do
      let vm = empty { _stack = S.fromList [-1], _fp = 0 }
      let input = " ret    \n\
                  \ halt   "
      let expected = Left "Cannot determine return address"      
      actual <- exec vm input
      actual `shouldBe` expected      

  describe "ld" $ do
    it "should lift the function argument to the stack top" $ do
      let input = "    push 14 \n\
                  \    call &f \n\
                  \    nop     \n\
                  \ f: ld 0    \n\
                  \    halt"
      let expected = done [14, 4, -1, 14] 7 1
      actual <- run input
      actual `shouldBe` expected  
    it "should lift multiple function arguments to the stack top" $ do
      let input = "    push 14 \n\
                  \    push 11 \n\
                  \    push 4  \n\
                  \    call &f \n\
                  \    nop     \n\
                  \ f: ld 2    \n\
                  \    ld 1    \n\
                  \    ld 0    \n\
                  \    halt"
      let expected = done [4, 11, 14, 8, -1, 4, 11, 14] 15 3
      actual <- run input
      actual `shouldBe` expected  
    it "can be combined with 'call' and 'ret' to define a function" $ do
      let input = "      push 14   \n\
                  \      push 11   \n\
                  \      call &sum \n\
                  \      halt      \n\
                  \ sum: ld 1      \n\
                  \      ld 0      \n\
                  \      add       \n\
                  \      ret       "
      let expected = done [25, 11, 14] 6 (-1)
      actual <- run input
      actual `shouldBe` expected  
    it "raises error if stack is empty" $ do      
      let input = " ld 0  \n\
                  \ halt  "
      let expected = Left "Index 0 out of stack bounds"
      let vm = empty { _stack = S.fromList [], _fp = 0 }
      actual <- exec vm input            
      actual `shouldBe` expected  
    it "raises error if stack contains only previous fp" $ do
      let vm = empty { _stack = S.fromList [-1], _fp = 0 }
      let input = " ld 0    \n\
                  \ halt   "
      let expected = Left "Index 0 out of stack bounds"      
      actual <- exec vm input
      actual `shouldBe` expected    
    it "raises error if stack contains only previous fp and return address" $ do
      let vm = empty { _stack = S.fromList [2, -1], _fp = 0 }
      let input = " ld 0    \n\
                  \ halt   "
      let expected = Left "Index 0 out of stack bounds"      
      actual <- exec vm input
      actual `shouldBe` expected 
    it "loads the first (0) argument if stack contains only previous fp, return address and single argument" $ do
      let vm = empty { _stack = S.fromList [2, -1, 3], _fp = 1 }
      let input = " ld 0    \n\
                  \ halt   "
      let expected = done [3, 2, -1, 3] 2 1      
      actual <- exec vm input
      actual `shouldBe` expected 
    it "raises error when accessint second (1) argument if stack contains only previous fp, return address and single argument" $ do
      let vm = empty { _stack = S.fromList [2, -1, 3], _fp = 1 }
      let input = " ld 1    \n\
                  \ halt   "
      let expected = Left "Index 1 out of stack bounds"      
      actual <- exec vm input
      actual `shouldBe` expected 
    it "loads the 11th argument if it exists" $ do
      --                                         ┌─────────────────────────────────────────────────┬── fp points the beginning of active stack frame
      --                                         │                                                 │
      -- stack indexes:                  14 13  12  11  10  9  8  7  6  5  4  3  2  1  0           │ 
      --                                         │                                                 │
      let vm = empty { _stack = S.fromList [ 2, -1, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 ], _fp = 12 }
      --                                         │                                     │
      -- argument indexes (ld):                  └─ 0   1   2  3  4  5  6  7  8  9  10 11
      --                                                                               └───── ld 11 results in pushing 0 on to the top of the stack        
      let input = " ld 11    \n\
                  \ halt   "
      let expected = done [0, 2, -1, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0] 2 12    
      actual <- exec vm input
      actual `shouldBe` expected 

  describe "clr" $ do
    it "wipes 4 values before the top of the stack" $ do
      let input = " push 1 \n\
                  \ push 2 \n\
                  \ push 3 \n\
                  \ push 4 \n\
                  \ push 5 \n\
                  \ clr 4  \n\
                  \ halt   "
      let expected = done [5] 12 (-1)
      actual <- run input
      actual `shouldBe` expected 
    it "can be combined with 'call' and 'ret' to define a function and perform a clean by caller" $ do
      let input = "      push 14   \n\
                  \      push 11   \n\
                  \      call &sum \n\
                  \      clr 2     \n\
                  \      halt      \n\
                  \ sum: ld 1      \n\
                  \      ld 0      \n\
                  \      add       \n\
                  \      ret       "
      let expected = done [25] 8 (-1)
      actual <- run input
      actual `shouldBe` expected  