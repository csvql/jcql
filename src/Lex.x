 {
module Lex where
import Data.Char
import Data.Bool
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

@identifier = $alpha [a-zA-Z0-9_]*
@int     = [1-9] $digit*

@compare = [\>\<][\=]?

@escape_char = [\\] [\" \' \\]
@single_str = [\'] ( [^\'] | @escape_char )* [\']
@double_str = [\"] ( [^\"] | @escape_char )* [\"]

tokens :-

<0> "//".*     ;
<0> $white+    ;
<0> => { \p s -> TArrow p }
<0> =|==|\-|\+|\/|\^|\%|@compare|not|and|or { \p s -> TOperator (map toLower s) p }
<0> true { \p _ -> TBool True p}
<0> false { \p _ -> TBool False p}

<0> import|take|join|inner|cross|on|case|when|then|else|end { \p s -> TKeyword s p }

<0> \(|\)|\[|\]  { mkBracket }
<0> \.    { \p _ -> TDot p }
<0> \,    { \p _ -> TComma p }
<0> @single_str|@double_str { \p s -> TString (strip_quotes s) p}
<0> @int                    { \p s -> TInt (read s) p }
<0> @identifier             { \p s -> TIdentifier s p }


{
strip_quotes = tail . init

mkBracket p s  = TBracket (map toUpper s) p

str (TDot _)        = "."
str (TComma _)      = ","
str (TString s _)     = "\"" ++ s ++ "\""
str (TIdentifier c _) = c
str (TKeyword c _) = c
str (TBracket c _) = c
str (TInt i _)        = show i

data Token = 
  TIdentifier String AlexPosn
  | TDot AlexPosn
  | TComma AlexPosn
  | TKeyword String AlexPosn
  | TOperator String AlexPosn
  | TBracket String AlexPosn
  | TString String AlexPosn
  | TInt Int AlexPosn
  | TBool Bool AlexPosn
  | TArrow AlexPosn
  deriving (Eq,Show)
}