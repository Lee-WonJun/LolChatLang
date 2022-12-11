module GlobalVariable

open System.Collections.Generic

//X::Y::Z 구성은 다음과같습니다.

//X = 우리, 상대
//Y = 탑,정글, 미드, 서폿, 봇
//Z = 딜량, CS, 레벨, 킬, 데스, 어시


let namespaces = ["우리";"상대"]
let subnamespaces = ["탑";"정글";"미드";"서폿";"봇"]
let variables = ["딜량";"CS";"레벨";"킬";"데스";"어시"]
let specialVariables = ["시발"]

let combinations = 
     [for ns in namespaces do
        for subns in subnamespaces do
            for variable in variables do
                ns + " " + subns + " " + variable]

let sumProperties =
     [for ns in namespaces do
        for variable in variables do
            ns + " " + variable + " 다해도"]
                
let substractProperties =
     [for ns in namespaces do
        for subns in subnamespaces do
            for variable in variables do
                ns + " " + subns + " " + variable + "차이"]

let globalNamespace = 
    [for ns in namespaces do
        for subns in subnamespaces do
                ns + " " + subns]

// append order : long -> short
let globalVariables = 
    sumProperties @ substractProperties @ combinations @ specialVariables
    
let toDictionary (map : Map<_, _>) : Dictionary<_, _> = Dictionary(map)

let variableHistory = 
    [for x in combinations@specialVariables do x, [0]] |> Map.ofList |> toDictionary
             
let setVariable (variable: string) (value: int) =
    variableHistory.[variable] <- value :: variableHistory.[variable]
    
let rec getVariable (variable: string) =
    // variable in combinations then use variableHistory else calc
    if List.contains variable (combinations@specialVariables) then
        variableHistory.[variable] |> List.head
        
    else if List.contains variable substractProperties then
        let s = variable.Replace("차이","")
        let operand1 = s.Split(" ")
        let ns = operand1.[0]
        let subns = operand1.[1]
        let var = operand1.[2]
        let inversNs = if ns = "우리" then "상대" else "우리"
        let operand2 = inversNs + " " + subns + " " + var
        getVariable s - getVariable operand2
        
    else if List.contains variable sumProperties then
        let s = variable.Split(" ")
        let ns = s.[0]
        let variable = s.[1]
        let filter = [for subns in subnamespaces do ns + " " + subns + " " + variable]
        combinations |> List.filter (fun x -> List.contains x filter) |> List.map (fun x -> variableHistory.[x] |> List.head) |> List.sum
        
    else
        0

let getNamespace (variable: string) =
    let s = variable.Split(" ")
    let v = s.[2]

    if List.exists (fun x -> x = v) variables then
       variable.Replace(" ","::")
    else
        ""
    
    
let getVariableHistory (variable: string) =
    variableHistory.[variable]
    



