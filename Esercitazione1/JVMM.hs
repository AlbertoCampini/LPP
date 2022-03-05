module Esercitazione1.JVMM where

data Instruction
  = PUSH Value
  | LOAD Var
  | STORE Var
  | OP (Value -> Value -> Value)
  | IF (Value -> Value -> Bool) Code
  | RETURN
  | DUP 
  | NOP
  | SWAP
  | POP

type Value = Int
type Var = Int
type Stack = [Value]
type Frame = [Value]
type Code = [Instruction]

load :: Var -> Frame -> Value
load _ [] = 0
load 0 (v : _) = v
load n (_ : vs) = load (n - 1) vs

store :: Var -> Value -> Frame -> Frame
store 0 v []       = [v]
store 0 v (_ : vs) = v : vs
store n v []       = 0 : store (n - 1) v []
store n v (w : vs) = w : store (n - 1) v vs

run :: Code -> Frame -> Value
run = aux []
  where
    aux :: Stack -> Code -> Frame -> Value
    aux (v : [])     (RETURN : [])  _  = v
    aux vs           (PUSH v : is)  fr = aux (v : vs) is fr
    aux vs           (LOAD x : is)  fr = aux (load x fr : vs) is fr
    aux (v : vs)     (STORE x : is) fr = aux vs is (store x v fr)
    aux (w : v : vs) (OP f : is)    fr = aux (f v w : vs) is fr
    aux (w : v : vs) (IF p is : _)  fr | p v w = aux vs is fr
    aux (_ : _ : vs) (IF _ _ : is)  fr = aux vs is fr
    aux (v : vs) (DUP: is) fr = aux (v : v : vs) is fr 
    aux (v : w : vs) (SWAP : is) fr = aux (w : v : vs) is fr
    aux (_ : vs) (POP : is) fr = aux vs is fr
    aux vs (NOP : is) fr = aux vs is fr
