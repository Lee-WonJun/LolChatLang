module Parser

open AST
open GlobalVariable
open Reader

let checkStringStartAndEnd (startWith : string list)  (endWith: string list) (line:string)=
    let checkStartWith = startWith |> List.exists (fun x -> line.StartsWith(x))
    let checkEndWith = endWith |> List.exists (fun x -> line.EndsWith(x))
    checkStartWith && checkEndWith
    
let findVariable(line: string) : VARIABLE option =
    globalVariables |> List.filter (fun x -> line.Contains(x)) |> List.tryHead

let findNamespace(line:string) : VARIABLE option =
    globalNamespace |> List.filter (fun x -> line.Contains(x)) |> List.tryHead
    
let equalConditionParse (line: string) : AST.EQUAL_CONDITION option =
    findVariable line 
    |> Option.map (fun x -> line.Replace(x, "")|> int |> fun y -> (x, y))

let returnParse (line: string) =
    let variable = findVariable line 
    let check = checkStringStartAndEnd [""] ["니네는 지는게 맞다"] line
    if check then
        try
            variable |> Option.map (fun x -> RETURN(x))
        with
            | _ -> None
    else
        None

let consoleOutParse (line:string) = 
    let variableOut = checkStringStartAndEnd [""] ["보소"; "봐라"] line
    let reflectionOut = checkStringStartAndEnd ["@ㅐ미"; "@ㅐ비"] [""] line
    
    if variableOut then
        findVariable line |> Option.map (fun x -> CONSOLEOUT(VARIABLE(x)))
    elif reflectionOut then
        let reflectionType = if line.StartsWith("@ㅐ미") then REFLECTION_TYPE.NAMESPACE else REFLECTION_TYPE.HISTORY
        findVariable line |> Option.map (fun x -> CONSOLEOUT(REFLECTION(reflectionType,x)))
    else
        None
   

let operatorParse (line:string) =
    let deathCheck = checkStringStartAndEnd [""] ["솔킬 따이네"] line
    if deathCheck then
        line.Replace("솔킬 따이네", "") + "데스" |> findVariable |> Option.map (fun x -> OPERATOR(DEATH, x))
    else
        None

let assignParse (line: string) : AST.Ast option =
    if checkStringStartAndEnd [""] ["실화냐?"] line then
        try
            let variable = findVariable line 
            let number = line.Replace("실화냐?", "").Replace(variable.Value,"") |> int 
            Ast.ASSIGN(ASSIGN_VALUE(variable.Value, number)) |> Some
        with
            | _ -> None
    else if checkStringStartAndEnd [""] ["시발"] line then
        let variable = findVariable (line.Replace("시발",""))
    
        variable |> Option.map (fun v -> Ast.ASSIGN(ASSIGN_VARIABLE("시발", v)))
    else
        None
       
    
let gotoAstParse(line: string) =
    if checkStringStartAndEnd ["ㅅㄱ 던짐"] ["서렌 ㄱㄱ"] line then 
        try
            let number = line.Replace("ㅅㄱ 던짐", "").Replace("서렌 ㄱㄱ","") |> int
            Some (GOTO number)
        with
            | _ -> None
    else if checkStringStartAndEnd [""] ["년아 라인 쳐 가라고"] line then 
        try
            let number = line.Replace("년아 라인 쳐 가라고", "") |> int
            let allowNumbers = [18;28]
            if allowNumbers |> List.contains number then
                Some (GOTO number)
            else
                None
        with
            | _ -> None
        else
        None

    
let ifAstParse (line: string)  = 
    if line = "답이없네" then
        Some BREAK
    else 
        let isIf = checkStringStartAndEnd  ["근데 진짜";"근데 아니";"아니 근데"] ["실화냐?"] line
        if isIf then
             let condition = line.Replace("근데 진짜", "").Replace("근데 아니", "").Replace("아니 근데", "").Replace("실화냐?", "")
             equalConditionParse condition
             |> Option.map (fun x -> if line.StartsWith("근데 진짜") then IF (x, []) else IFNOT (x, []))
         else 
             None


let parse (input: Code list) : Ast list =
    let rec parse' (input: Code list) (depth: int) (ast: Ast list) : Ast list =
        match input with
        | [] -> ast
        | head :: rest when head.Depth = depth ->
            let depthCause =  rest |> List.takeWhile (fun x -> x.Depth >= depth)
            let lazyParseCause = fun () -> parse' depthCause (depth + 1) []
            let parseOrder = [ifAstParse;assignParse;gotoAstParse;returnParse;operatorParse;consoleOutParse]
            let passedParse = parseOrder |> List.tryPick (fun x -> x head.Line) |> Option.defaultValue NOTHING
            
            match passedParse with
            | IF (condition, _) -> parse' rest depth (ast @ [IF (condition, lazyParseCause())])
            | IFNOT (condition, _) -> parse' rest depth (ast @ [IFNOT (condition, lazyParseCause())])
            | NOTHING -> parse' rest depth ast
            | _ -> parse' rest depth (ast @ [passedParse])
            
        | head :: rest when head.Depth > depth -> parse' rest depth ast
        | _ -> ast
                
    parse' input 0 []

 (*
 
 #load "D:\\Project\\LolChatLang\\Lex\\Reader.fs";
 #load "D:\\Project\\LolChatLang\\Lex\\AST.fs";
 #load "D:\\Project\\LolChatLang\\Lex\\GlobalVariable.fs";
 #load "D:\\Project\\LolChatLang\\Lex\\Parser.fs";
 open AST;
 open GlobalVariable;
 open Parser;
 open Reader;;
 *)
 
