{
{-  --------------------------------
    @file       src/Frontend/Lexer.x
    @details    ~
-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Frontend.Lexer ( lexer,
                        tokenize,
                        tokenize_plain,
                        runAlex,
                        alexMonadScan,
                        alexError,
                        to_either_error,
                        Alex) where

import Control.Monad
import Frontend.Token
import Frontend.Error
}

-- lexer's type...
%wrapper "monadUserState"



$digit      = 0-9 --
@integral   = $digit+ -- [\+\-]? -> it seems not to be ideal putting signness here for which it confuses with the expressions...
@floating   = ($digit+)? (\. $digit+) ( [eE] [\+\-]? ($digit+)? )?
$character  = [_a-zA-Z]
@identifier = [$character][$character$digit]*
@string     = \" ([^\"\\] | \\.)* \" -- `\\.` specifies to match any character. the LHS specifies to not include the `"` or scape `\`.


tokens :-
    -- Whitespace and line comments.
    <0> $white+ ;
    <0> "//" .* ;
    
    -- Multi-line comment.
    <0> "/*"                { comment_nest `andBegin` state_comment }
    <0> "*/"                { comment_close_error }
    <state_comment> "/*"    { comment_nest }
    <state_comment> "*/"    { comment_unnest }
    <state_comment> .       ;
    <state_comment> \n      ;

    -- Keywords.
    <0> "func"              { tokenize_simple T_Func }
    <0> "struct"            { tokenize_simple T_Struct }
    <0> "let"               { tokenize_simple T_Let }
    <0> "return"            { tokenize_simple T_Return }
    <0> "if"                { tokenize_simple T_If }
    <0> "elif"              { tokenize_simple T_Elif }
    <0> "else"              { tokenize_simple T_Else }
    <0> "for"               { tokenize_simple T_For }
    <0> "while"             { tokenize_simple T_While }
    <0> "forall"            { tokenize_simple T_Forall }
    <0> "new"               { tokenize_simple T_New }
    <0> "delete"            { tokenize_simple T_Delete }
    <0> "void"              { tokenize_simple T_TypeVoid }    
    <0> "bool"              { tokenize_simple T_TypeBool }    
    <0> "int"               { tokenize_simple T_TypeInt }    
    <0> "float"             { tokenize_simple T_TypeFloat }    
    <0> "string"            { tokenize_simple T_TypeString }

    <0> "@print"            { tokenize_simple T_IO_Print }
    <0> "@scan"             { tokenize_simple T_IO_Scan }
    <0> "@rc"                { tokenize_simple T_IO_RC }

    -- Enclosures.
    <0> "("                 { tokenize_simple T_LParenthesis }
    <0> ")"                 { tokenize_simple T_RParenthesis }
    <0> "["                 { tokenize_simple T_LBracket }
    <0> "]"                 { tokenize_simple T_RBracket }
    <0> "{"                 { tokenize_simple T_LBrace }
    <0> "}"                 { tokenize_simple T_RBrace }

    -- 
    <0> "<-"                { tokenize_simple T_LArrow }
    <0> "->"                { tokenize_simple T_RArrow }

    -- Operators.
    <0> "++"                { tokenize_simple T_Incr }
    <0> "+"                 { tokenize_simple T_Plus }

    <0> "--"                { tokenize_simple T_Decr }
    <0> "-"                 { tokenize_simple T_Minus }

    <0> "!="                { tokenize_simple T_ExpNeq }
    <0> "!"                 { tokenize_simple T_ExpNot }

    <0> "**"                { tokenize_simple T_Power }
    <0> "*"                 { tokenize_simple T_Times }

    <0> "//"                { tokenize_simple T_Divide2 }
    <0> "/"                 { tokenize_simple T_Divide }

    <0> "%"                 { tokenize_simple T_Mod }

    <0> "=="                { tokenize_simple T_ExpEq }

    <0> ">="                { tokenize_simple T_ExpGeq }
    <0> "<="                { tokenize_simple T_ExpLeq }

    <0> "&&"                { tokenize_simple T_ExpAnd }
    <0> "||"                { tokenize_simple T_ExpOr }
    <0> "^^"                { tokenize_simple T_ExpXor }
    
    -- can be either a enclosue or a operator...
    <0> "<"                 { tokenize_simple T_LAngle }
    <0> ">"                 { tokenize_simple T_RAngle }

    -- Punctuactions and whatnot.
    <0> "."                 { tokenize_simple T_Dot }
    <0> ","                 { tokenize_simple T_Comma }
    <0> ":"                 { tokenize_simple T_Colon }
    <0> ";"                 { tokenize_simple T_Semicolon }
    <0> "="                 { tokenize_simple T_Equal }

    -- Literals.
    <0> @integral           { tokenize_read1 T_Integral }
    <0> @floating           { tokenize_read1 T_Floating }
    <0> "true"              { tokenize_simple (T_Bool True) }
    <0> "false"             { tokenize_simple (T_Bool False) }
    <0> @string             { tokenize_read1 T_String }

    -- 
    <0> @identifier         { tokenize_id }

    -- otherwise.
    <0> .                   { unknown_error }


{   

-- User state~.
data AlexUserState = AlexUserState {
    nestLevel :: Int -- comment nesting level
}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 0

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())


-- definition of the EOF token
alexEOF :: Alex Token
alexEOF = do
    (pos, _, _, _) <- alexGetInput
    startCode <- alexGetStartCode

    when (startCode == state_comment) $ alexError $ show $ 
        Error { error_type = LexicalError, error_msg = "Unclosed comment.", error_pos = position pos }

    pure $ Token (position pos) T_EOF



------------------
-- Commentaries --
------------------

comment_nest :: AlexAction Token
comment_nest input len = do
    modify $ \s -> s{nestLevel = nestLevel s + 1}
    skip input len


-- undoes `comment_nest`.
comment_unnest :: AlexAction Token
comment_unnest input len = do
    s <- get
    let level = (nestLevel s) - 1
    put s{nestLevel = level}

    when (level == 0) $ alexSetStartCode 0

    skip input len


comment_close_error :: AlexInput -> Int -> Alex Token
comment_close_error (st, _, _, _) _ = alexError $ show $ 
    Error { error_type = LexicalError, error_msg = "Unexpected close comment.", error_pos = position st }




----------------------
-- Tokenizing input --
----------------------

position :: AlexPosn -> SrcPos
position (AlexPn _ x y) = SrcPos (x, y)


tokenize_simple :: Lexeme -> AlexAction Token
tokenize_simple lx (st, _, _, _) _ = 
    return $ Token (position st) lx


tokenize_read1 :: Read a => (a -> Lexeme) -> AlexAction Token
tokenize_read1 typec (st, _, _, str) len =
    return $ Token (position st) (typec $ read $ take len str)


tokenize_id (st, _, _, str) len =
    return $ Token (position st) (T_Identifier $ take len str)


unknown_error :: AlexInput -> Int -> Alex Token
unknown_error (st, _, _, input) _ = alexError $ show $
    Error { error_type = LexicalError, error_msg = "Unknown symbol: \"" ++ ([input !! 0]) ++ "\".", error_pos = position st }


-----------
-- Lexer --
-----------

to_either_error :: Either String a -> Either Error a
to_either_error (Left x)    = Left $ read x
to_either_error (Right x)   = Right $ x


-- Lexer main function. Returns a list of tokens from the input string.
tokenize :: String -> Either Error [Token]
tokenize s = to_either_error $ runAlex s go where
    go = do
        output <- alexMonadScan

        if lexeme output == T_EOF then
            pure [output]
        
        else 
            (output :) <$> go


lexer :: String -> Either Error [Token]
lexer = tokenize

-- just the lexemes...
tokenize_plain :: String -> [Lexeme]
tokenize_plain s = case tokenize s of
    Left _          -> []
    Right tokens    -> map lexeme tokens 


} -- END OF HASKELL.
