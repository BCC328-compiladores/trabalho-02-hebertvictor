{-  ---------------------------------
    @file       src/Frontend/Token.hs
    @details    Defines the token structure for the frontend.
-}
{-# LANGUAGE InstanceSigs #-} -- for which doesn't allow by default...
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Frontend.Token where
import Frontend.Pretty


newtype SrcPos = SrcPos (Int, Int) 
    deriving (Eq, Ord, Show, Read)

-- Token type.
data Token = Token {
    pos :: SrcPos,
    lexeme :: Lexeme
} deriving (Eq, Ord, Show, Read)


data Lexeme =
    -- General Keywords.
    -- @TODO: kw and types may be encapsulated, for which the parser is what may actually need them.
    T_Func |
    T_Struct |
    T_Let |
    T_Return |
    T_If |
    T_Elif |
    T_Else |
    T_For |
    T_While |
    T_Forall |
    T_New |
    
    T_Delete | -- <- will we need this?　とりあえず、やっているけど。。。

    T_Lambda | 
    T_Captures |
    T_LambdaReturn |
    T_Eval |

    -- IO.
    T_IO_Print |
    T_IO_Scan |
    T_IO_RC |

    -- Types. 
    T_TypeVoid |
    T_TypeBool |
    T_TypeInt |
    T_TypeFloat |
    T_TypeString |

    -- Identifier.
    T_Identifier String |

    -- Literal types.
    T_Integral  Integer |
    T_Floating  Double |
    T_String    String |
    T_Bool      Bool |

    -- Enclosure.
    T_LParenthesis |
    T_RParenthesis |
    T_LBracket |
    T_RBracket |
    T_LBrace | -- curly brackets.
    T_RBrace |
    T_LAngle |
    T_RAngle |

    -- Binary operators.    
    T_Plus |
    T_Minus |
    T_Times |
    T_Divide |
    T_Divide2 |
    T_Mod |
    T_Power |
    T_ExpEq |
    T_ExpNeq |
    T_ExpGeq |
    T_ExpLeq |
    T_ExpAnd |
    T_ExpOr |
    T_ExpXor |

    -- Unary operators.
    T_ExpNot |
    T_Incr | -- ++
    T_Decr | -- --

    -- Other resources.
    T_Dot |
    T_Comma |
    T_Colon |
    T_Semicolon |
    T_Equal |
    T_LArrow |
    T_RArrow |

    -- for control...
    T_EOF
    deriving (Eq, Ord, Show, Read)


-- personalized show...
lexeme_show :: Lexeme -> String
lexeme_show T_Func          = "kw func"
lexeme_show T_Struct        = "kw struct"
lexeme_show T_Let           = "kw let"
lexeme_show T_Return        = "kw return"
lexeme_show T_If            = "kw if"
lexeme_show T_Elif          = "kw elif"
lexeme_show T_Else          = "kw else"
lexeme_show T_For           = "kw for"
lexeme_show T_While         = "kw while"
lexeme_show T_Forall        = "kw forall"
lexeme_show T_New           = "kw new"
lexeme_show T_Delete        = "kw delete"

lexeme_show T_TypeVoid      = "kw void"
lexeme_show T_TypeBool      = "kw bool"
lexeme_show T_TypeInt       = "kw int"
lexeme_show T_TypeFloat     = "kw float"
lexeme_show T_TypeString    = "kw string"

lexeme_show (T_Identifier string) = "id \"" ++ string ++ "\""

lexeme_show (T_Integral  x) = "i" ++ show x
lexeme_show (T_Floating  x) = "f" ++ show x
lexeme_show (T_String    x) = "s" ++ show x
lexeme_show (T_Bool      x) = "b" ++ show x

lexeme_show T_LParenthesis  = "enc. \"(\""
lexeme_show T_RParenthesis  = "enc. \")\"" 
lexeme_show T_LBracket      = "enc. \"[\""
lexeme_show T_RBracket      = "enc. \"]\""
lexeme_show T_LBrace        = "enc. \"{\""
lexeme_show T_RBrace        = "enc. \"}\""
lexeme_show T_LAngle        = "enc. \"<\""
lexeme_show T_RAngle        = "enc. \">\""

lexeme_show T_Plus          = "+" 
lexeme_show T_Minus         = "-"
lexeme_show T_Times         = "*"
lexeme_show T_Divide        = "/"
lexeme_show T_Divide2       = "//"
lexeme_show T_Mod           = "%"
lexeme_show T_Power         = "**"
lexeme_show T_ExpEq         = "=="
lexeme_show T_ExpNeq        = "!="
lexeme_show T_ExpGeq        = ">="
lexeme_show T_ExpLeq        = "<="
lexeme_show T_ExpAnd        = "&&"
lexeme_show T_ExpOr         = "||"
lexeme_show T_ExpXor        = "^^"
lexeme_show T_ExpNot        = "!"
lexeme_show T_Incr          = "++"
lexeme_show T_Decr          = "--"

lexeme_show T_Dot           = "punctuation \".\""
lexeme_show T_Comma         = "punctuation \",\"" 
lexeme_show T_Colon         = "punctuation \":\""
lexeme_show T_Semicolon     = "punctuation \";\""
lexeme_show T_Equal         = "punctuation \"=\""
lexeme_show T_LArrow        = "punctuation \"<-\""
lexeme_show T_RArrow        = "punctuation \"->\""

lexeme_show T_IO_Print      = "PRINT"
lexeme_show T_IO_Scan       = "SCAN"
lexeme_show T_IO_RC         = "RC"

lexeme_show T_EOF = "EOF"

lexeme_show (T_Lambda) = "kw λ"
lexeme_show (T_Captures) = "lw captures"
lexeme_show (T_LambdaReturn) = "kw λ-return"
