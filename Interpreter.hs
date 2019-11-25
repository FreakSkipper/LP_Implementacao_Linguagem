module Interpreter where

import AbsLE
import Prelude hiding (lookup)

type Contexto = [(Ident, Valor)]

data Valor = ValorInt {
               i :: Integer
             }
            |
             ValorFun {
               f :: Function
             }
            |
             ValorStr {
               s :: String
             }
            | ValorBool {
               b :: Bool
             }

instance Show Valor where
    show (ValorBool b) = show b
    show (ValorInt i) = show i
    show (ValorStr s) = s
    show (ValorFun (Fun nf decls _)) = show (nf) ++ "[" ++ show (decls) ++ "]"


executeP :: Program -> Contexto
executeP (Prog fs) = execute (atualizaContexto [] fs) (SBlock (mainStm fs))
    where mainStm ((Fun (Ident "main") params stms):xs) = stms
          mainStm (_:xs) = mainStm xs


execute :: Contexto -> Stm -> Contexto
execute ctx stm = case stm of
    SAss    id exp          -> update ctx id (eval ctx exp)
    -- Def    id exp          -> update ctx id (eval ctx exp)
    SBlock  []              -> ctx
    SBlock  (x:xs)          -> execute (execute ctx x) (SBlock xs)
    SWhile  exp x           -> if (i (eval ctx exp) /= 0) then 
                                execute (execute ctx x) (SWhile exp x)
                            else ctx
    SReturn exp             -> update ctx (Ident "return") (eval ctx exp)
    SIf     exp stmI stmE   ->  if (i (eval ctx exp) /= 0)
                                    then execute ctx stmI       -- eu n preciso alterar o stm?
                                else execute ctx stmE


eval :: Contexto -> Exp -> Valor
eval ctx x = case x of
    ECon exp0 exp  -> ValorStr ( s (eval ctx exp0) ++ s (eval ctx exp ))
    ELet exp0 exp  -> ValorInt ( i (eval (atualizaCDef ctx exp0) exp))
    EAdd exp0 exp  -> ValorInt ( i (eval ctx exp0) + i (eval ctx exp))
    ESub exp0 exp  -> ValorInt ( i (eval ctx exp0) - i (eval ctx exp))
    EMul exp0 exp  -> ValorInt ( i (eval ctx exp0) * i (eval ctx exp))
    EDiv exp0 exp  -> ValorInt ( i (eval ctx exp0) `div` i (eval ctx exp))
    EPow exp0 exp  -> ValorInt ( i (eval ctx exp0) ^ i (eval ctx exp))
    EOr  exp0 exp  -> ValorBool ( b (eval ctx exp0) || b (eval ctx exp))
    EAnd exp0 exp  -> ValorBool ( b (eval ctx exp0) && b (eval ctx exp))
    ENot exp0      -> ValorBool ( not ( b (eval ctx exp0)))
    EStr s         -> ValorStr s
    ETrue          -> ValorBool True
    EFalse         -> ValorBool False
    EInt n         -> ValorInt n
    EVar id        -> lookup0 ctx id
    Call id lexp   -> lookup0 (execute (paramBindings++contextFunctions) (SBlock stms)) (Ident "return")
                        where ValorFun (Fun _ decls stms) = lookup0 ctx id
                              paramBindings = zip decls (map (eval ctx) lexp)
                              contextFunctions = filter (\(i,v) -> case v of 
                                                                        ValorFun _ -> True 
                                                                        _ -> False
                                                        ) ctx 


lookup0 :: Contexto -> Ident -> Valor
lookup0 (ctx:ctxs) s
    | (fst ctx) == s = (snd ctx)
    | otherwise = lookup0 ctxs s


update :: Contexto -> Ident -> Valor -> Contexto
update [] s nv = [(s, nv)]
update (ctx:ctxs) s nv
    | (fst ctx) == s = (s,nv):ctxs
    | otherwise = ctx:(update ctxs s nv)


atualizaContexto :: Contexto -> [Function] -> Contexto
atualizaContexto ctx [] = ctx
atualizaContexto ctx (f@(Fun id params stms):fs)= atualizaContexto (update ctx id (ValorFun f)) fs


atualizaCDef :: Contexto -> [Def] -> Contexto
atualizaCDef ctx [] = ctx
atualizaCDef ctx ((Def id exp):ds)= atualizaCDef (update ctx id (eval ctx exp)) ds