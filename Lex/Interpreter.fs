module Interpreter
open AST
open Reader
open GlobalVariable
open Parser

(*
let test = [
    IF (("우리 정글 딜량", 15),[
        IFNOT (("상대 미드 딜량", 10), [

        ]); 
        ASSIGN (ASSIGN_VALUE ("상대 정글 딜량", 20));
        IF(("우리 킬 다해도", 5),[
            ASSIGN (ASSIGN_VALUE ("우리 정글 딜량", 20));
            ASSIGN (ASSIGN_VALUE ("상대 정글 딜량", 20));
            ASSIGN (ASSIGN_VARIABLE ("시발", "우리 정글 딜량"))]); 
        BREAK; 
        GOTO 15]);
   RETURN "시발"
];;

 #load "D:\\Project\\LolChatLang\\Lex\\Reader.fs";
 #load "D:\\Project\\LolChatLang\\Lex\\AST.fs";
 #load "D:\\Project\\LolChatLang\\Lex\\GlobalVariable.fs";
 #load "D:\\Project\\LolChatLang\\Lex\\Parser.fs";
 open AST;
 open GlobalVariable;
 open Parser;
 open Reader;;

 (*
 let code = loadCode "C:\\Users\\dldnj\\Desktop\\새 폴더\\condition.txt"
 
 let code = loadCode "C:\\Users\\dldnj\\Desktop\\새 폴더\\ccc.txt"
 *)
 
 let cc = (parse code)
 interpreteAst cc
 finalReturnValue
*)

let mutable finalReturnValue = 0
let mutable resetInterprete = false

let interpreteAst (ast: Ast list) = 
    resetInterprete  <- false
    let rec interprete (ast: Ast list) = 
        match ast with
        | [] -> ()
        | (IF (cond, ast)) :: rest -> 
            printfn "IF %A" cond
            let (var, value) = cond
            if getVariable(var) = value then
                interprete ast 
                interprete rest
            else
                interprete rest 
        | (IFNOT (cond, ast)) :: rest -> 
            printfn "IF NOT %A" cond
            let (var, value) = cond
            if getVariable(var) <> value then
                interprete ast 
                interprete rest
            else
                interprete rest 
        | (ASSIGN (ASSIGN_VALUE (var, value))) :: rest -> 
            printfn "ASSIGNE %A %A" var value
           
            setVariable var value
            interprete rest 
        | (ASSIGN (ASSIGN_VARIABLE (var, value))) :: rest -> 
            printfn "ASSIGNE %A %A" var value
            
            let realvalue = getVariable value
            setVariable var realvalue
            interprete rest
        | (BREAK) :: rest -> 
            printfn "BREAK"
            ()
        | (GOTO (line)) :: rest -> 
            printfn "GOTO"
            resetInterprete  <- true
            ()
        | (RETURN (var)) :: rest -> 
            printfn "RETURN"
            resetInterprete  <- true
            finalReturnValue <- getVariable var
        | _ :: rest -> 
            interprete rest
    interprete ast 