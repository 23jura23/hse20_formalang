{
module Lex where
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z\_]		-- alphabetic characters

tokens :-

  $white+				        ;
  \:\-					        { \p _ -> Turnstile p }
  \,                            { \p _ -> Comma p }
  \;                            { \p _ -> Semicolon p }                     
  \.					        { \p _ -> Dot p }
  \(                            { \p _ -> Lb p }
  \)                            { \p _ -> Rb p }
  $alpha [$alpha $digit]*		{ \p s -> Word s p }
  \(\*                          { \p _ -> Lc p }
  \*\)                          { \p _ -> Rc p }
  [\x00-\x10ffff]               { \p s -> Error s p }
{
-- Each action has type :: String -> Token

-- The token type:
data Token =
    Turnstile    AlexPosn |
    Comma        AlexPosn |
    Semicolon    AlexPosn |
    Dot          AlexPosn |
    Lb           AlexPosn |
    Rb           AlexPosn |
    Lc           AlexPosn |
    Rc           AlexPosn |
    Word  String AlexPosn |
    Error String AlexPosn
	deriving (Eq,Show)
}
