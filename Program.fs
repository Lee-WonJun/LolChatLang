// For more information see https://aka.ms/fsharp-console-apps
open Interpreter

let args = System.Environment.GetCommandLineArgs()
let path = args.[1]
let result = run path
exit result