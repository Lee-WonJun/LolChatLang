﻿module AST


type VARIABLE = string

type LINE_NUMBER = int

type VALUE = int

type EQUAL_CONDITION = VARIABLE * VALUE

type OPERATOR_TYPE = DEATH

type REFLECTION_TYPE = 
  | NAMESPACE
  | HISTORY

type REFLECTION_DATA = REFLECTION_TYPE * VARIABLE

type CONSOLEOUT_TYPE =
  | VARIABLE of VARIABLE
  | REFLECTION of REFLECTION_DATA 
  
type ASSIGN_TYPE = ASSIGN_VALUE of VARIABLE * VALUE | ASSIGN_VARIABLE of VARIABLE * VARIABLE
type Ast = 
  | IF of EQUAL_CONDITION * Ast list
  | IFNOT of EQUAL_CONDITION * Ast list
  | BREAK
  | GOTO of LINE_NUMBER
  | RETURN of VARIABLE
  | ASSIGN of ASSIGN_TYPE
  | OPERATOR of OPERATOR_TYPE * VARIABLE
  | CONSOLEOUT of CONSOLEOUT_TYPE
  | NOTHING