 {
module Lex where
import Data.Char
import Data.Bool
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

@identifier = $alpha [a-zA-Z0-9_]*
@int     = $digit+

@compare = [\>\<][\=]?

@escape_char = [\\] [\" \' \\]
@single_str = [\'] ( [^\'] | @escape_char )* [\']
@double_str = [\"] ( [^\"] | @escape_char )* [\"]

tokens :-

<0> "//".*     ;
<0> $white+    ;
<0> =|\-|\+|\/|\^|\%|@compare|not|and|or { \p s -> TOperator (map toLower s) p }
<0> true { \p _ -> TBool True p}
<0> false { \p _ -> TBool False p}

<0> import|take|join|inner|cross|on|case|when|then|else|end|where|select { \p s -> TKeyword s p }

<0> \(|\)|\[|\]  { mkBracket }
<0> \.    { \p _ -> TDot p }
<0> \,    { \p _ -> TComma p }
<0> @single_str|@double_str { \p s -> TString (strip_quotes s) p}
<0> @int                    { \p s -> TInt (read s) p }
<0> @identifier             { \p s -> TIdentifier s p }
<0> \*             { \p _ -> TAsterisk p }


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
str (TAsterisk _) = "*"

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
  | TAsterisk AlexPosn
  deriving (Eq,Show)

showErr s (AlexPn _ l c) = "'"++s++"' at line "++show l++", column "++show c

tokenPosn :: Token -> String
tokenPosn (TIdentifier id p) = showErr id p
tokenPosn (TDot p) = showErr "." p
tokenPosn (TComma p) = showErr "," p
tokenPosn (TKeyword k p) = showErr k p
tokenPosn (TOperator op p) = showErr op p
tokenPosn (TBracket b p) = showErr b p
tokenPosn (TString s p) = showErr s p
tokenPosn (TInt i p) = showErr (show i) p
tokenPosn (TBool b p) = showErr (show b) p
tokenPosn (TAsterisk p) = showErr "*" p
}