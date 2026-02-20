{-  ---------------------------------
    @file       src/Frontend/Parser.y
    @details    Happy parser specification.
-}

-- happy deixa utf8 n wtf

{
module Frontend.Parser (    parse_sl, 
                            parse_sl_exp, 
                            parse_sl_stat) where

-- [Token] -> IR
import Frontend.Token
import Frontend.IR
import Frontend.Error
import Frontend.Lexer hiding (lexer)
}

%name       parse_sl_alex       Program
%name       parse_sl_exp_alex   Expression
%name       parse_sl_stat_alex  Statement
%monad      { Alex }{ (>>=) } { return }
%tokentype  { Token }
%error      { parsing_error }
%lexer      { lexer } { Token _ T_EOF }
%expect 0 -- Raises an error whenever a shift/reduce or reduce/reduce conflict is found.


%token
    -- Terminal symbol mapping.
    IDENTIFIER      { Token id_pos (T_Identifier $$) }

    -- Literals.
    INTEGRAL        { Token literal_pos (T_Integral $$) }
    FLOATING        { Token literal_pos (T_Floating $$) }
    BOOLEAN         { Token literal_pos (T_Bool $$ ) }
    STRING          { Token literal_pos (T_String $$) }
    
    -- Keywords.
    FUNC            { Token _   (T_Func) }
    STRUCT          { Token _   (T_Struct) }
    LET             { Token _   (T_Let) }
    RETURN          { Token $$  (T_Return) }
    IF              { Token $$  T_If }
    ELIF            { Token $$  (T_Elif) }
    ELSE            { Token _   (T_Else) }
    FOR             { Token $$  (T_For) }
    WHILE           { Token $$  (T_While) }
    FORALL          { Token _   (T_Forall) }
    NEW             { Token _   (T_New) }
    DELETE          { Token _   (T_Delete) }
    
    LAMBDA          { Token $$  (T_Lambda) }
    CAPTURES        { Token $$  (T_Captures) }
    L_RETURN        { Token $$  (T_LambdaReturn) }
    EVAL            { Token $$  (T_Eval) }

    TYPE_VOID       { Token _ T_TypeVoid }
    TYPE_BOOL       { Token _ T_TypeBool }
    TYPE_INT        { Token _ T_TypeInt }
    TYPE_FLOAT      { Token _ T_TypeFloat }
    TYPE_STRING     { Token _ T_TypeString }

    KW_IO_PRINT     { Token $$ T_IO_Print }
    KW_IO_SCAN      { Token $$ T_IO_Scan }
    KW_IO_RC        { Token $$ T_IO_RC }

    EOF             { Token _ T_EOF }

    -- Operators.
    '++'            { Token _ T_Incr }
    '+'             { Token _ T_Plus }
    '--'            { Token _ T_Decr }
    '-'             { Token _ T_Minus }
    '!='            { Token _ T_ExpNeq }
    '!'             { Token _ T_ExpNot }
    '**'            { Token _ T_Power }
    '*'             { Token _ T_Times }
    '//'            { Token _ T_Divide2 }
    '/'             { Token _ T_Divide }
    '%'             { Token _ T_Mod }
    '=='            { Token _ T_ExpEq }
    '>='            { Token _ T_ExpGeq }
    '<='            { Token _ T_ExpLeq }
    '&&'            { Token _ T_ExpAnd }
    '||'            { Token _ T_ExpOr }
    '^^'            { Token _ T_ExpXor }

    -- Enclosures.
    '('             { Token _ T_LParenthesis }
    ')'             { Token _ T_RParenthesis }
    '['             { Token _ T_LBracket }
    ']'             { Token _ T_RBracket }
    '{'             { Token _ T_LBrace }
    '}'             { Token _ T_RBrace }

    -- can be either a enclosure or a operator...
    '<'             { Token _ T_LAngle }
    '>'             { Token _ T_RAngle }
    
    -- Punctuactions and whatnot.
    '.'             { Token $$ T_Dot } -- what is important to get from them is their position.
    ','             { Token $$ T_Comma }
    ':'             { Token $$ T_Colon }
    ';'             { Token $$ T_Semicolon }
    '='             { Token $$ T_Equal }
    '<-'            { Token $$ T_LArrow }
    '->'            { Token $$ T_RArrow }


-- Defining the operators associativity and precedence.
%right '||'
%right '&&'
%nonassoc '==' '!=' '<' '<=' '>' '>='
%left '+' '-'
%left '*' '/' '//' '%'
%right '**'
%nonassoc '++' '--'


%%
----------------------------------
-- General procedural structure --
----------------------------------

Program :: { IR_Program } -- `P`.
Program : -- the program is a list of statements.
    Statements { Program $1 }


Statements :: { [IR_Statement] } -- `[D]`.
Statements : -- listing the statements.
    { [] } |
    Statements Statement { $1 ++ [$2] } 


Statement :: { IR_Statement } -- `D`.
Statement : -- high-level definitions (function / structure).
    -- structure unique definition.
    STRUCT IDENTIFIER '{' FieldList '}'
    {   -- `St`.
        StructDef {
            symbol_name = $2,
            fields      = $4,
            symbol_pos  = id_pos
        }
    } |

    -- standard function definition.
    FUNC IDENTIFIER '(' ParameterList ')' OptionalTypeSpec2 '{' CommandList '}'
    {   -- `F`.
        FuncDef {
            symbol_name         = $2,
            function_rtype      = $6,
            function_parameters = $4,
            function_gtypes     = [],
            function_body       = $8,
            symbol_pos          = id_pos
        }
    } |

    -- generic function definition.
    FORALL GenericList '.' FUNC IDENTIFIER '(' ParameterList ')' OptionalTypeSpec2 '{' CommandList '}'
    {   -- `F`.
        FuncDef {
            symbol_name         = $5,
            function_rtype      = $9,
            function_parameters = $7,
            function_gtypes     = $2,
            function_body       = $11,
            symbol_pos          = id_pos
        }
    } 


FieldList :: { [IR_Var] } -- `\hat{V}`.
FieldList : -- list of variable declarations.
    { [] } |
    FieldList VariableDecl ';' { $1 ++ [$2] } 


ParameterList :: { [IR_Var] } -- `Pm`.
ParameterList : -- either explicit void or a listing.
    ParameterList2  { $1 } |
    TYPE_VOID       { [] } |
                    { [] }


ParameterList2 :: { [IR_Var] } -- `Pm'`.
ParameterList2 : -- usual parameter listing.
    OptionalVariableDecl                    { [$1] } |
    ParameterList2 ',' OptionalVariableDecl { $1 ++ [$3] }


GenericList :: { [ Identifier ] } -- `\hat{G}`.
GenericList : -- will be identifiers with no extra token in-between.
    Generic { [$1] } | -- at least one generic type.
    GenericList Generic { $1 ++ [$2] }


Generic :: { Identifier }
Generic :
    IDENTIFIER { $1 }


CommandList :: { [IR_LocatedCommand] } -- `\hat{C}`.
CommandList :
    { [] } | 
    CommandList Command { $1 ++ [$2] }


Command :: { IR_LocatedCommand } -- `C`.
Command :
    KW_IO_PRINT '<' '<' Expression '>' '>' ';'                          { LC (Print  $4)            $1 } |
    KW_IO_SCAN  '<' '<' Expression '>' '>' ';'                          { LC (Scan   $4)            $1 } |

    -- Assignment commands.
    LET VariableDecl ';'                                                { LC (VarDef $2 ExpNothing) $3 } |
    LET OptionalVariableDecl '=' Expression ';'                         { LC (VarDef $2 $4)         $5 } | 
    VariableAccess '=' Expression ';'                                   { LC (Assignment $1 $3)     $4 } | 

    -- Control flux commands. `Cf`.
    -- (diverges a little bit from the grammar on the document for simplicity in the AST translation...)
    IF '(' Expression ')' '{' CommandList '}' ELSE '{' CommandList '}'  { LC (If $3 $6 $10)         $1 } |
    IF '(' Expression ')' '{' CommandList '}' Elses                     { LC (If $3 $6 [$8])        $1 } |
    IF '(' Expression ')' '{' CommandList '}'                           { LC (If $3 $6 [])          $1 } |

    -- Repetition commands. `R`.
    WHILE '(' Expression ')' '{' CommandList '}'                        { LC (While $3 $6)          $1 } |
    
    -- @NOTES: 
    -- 1 - semicolon is already included in the command...
    -- 2 - `command` in the for is too generic.
    FOR '(' ForInitCommand ';' OptionalExpression ';' ForItCommand ')' '{' CommandList '}'  
                                                                        { LC (For $3 $5 $7 $10)     $1 } |

    -- Others
    RETURN OptionalExpression ';'                                       { LC (Return $2)            $1 } |
    Expression ';'                                                      { LC (CmdExpression $1)     $2 }


Elses :: { IR_LocatedCommand } -- `\bar{Cf}`.
Elses :
    ELIF '(' Expression ')' '{' CommandList '}' ELSE '{' CommandList '}'    { LC (If $3 $6 $10) $1 } |
    ELIF '(' Expression ')' '{' CommandList '}' Elses                       { LC (If $3 $6 [$8]) $1 } |
    ELIF '(' Expression ')' '{' CommandList '}'                             { LC (If $3 $6 [])  $1 }


ForInitCommand :: { IR_LocatedCommand }
ForInitCommand :
    VariableAccess '=' Expression           { LC (Assignment $1 $3) $2 } | -- pos on equal.
    LET VariableDecl '=' Expression         { LC (VarDef $2 $4)     $3 }  |
    OptionalExpression                      { LC (CmdExpression $1) (SrcPos (0, 0)) } -- @TODO

ForItCommand :: { IR_LocatedCommand }
ForItCommand :
    VariableAccess '=' Expression           { LC (Assignment $1 $3) $2 } | -- pos on equal.
    OptionalExpression                      { LC (CmdExpression $1) (SrcPos (0, 0)) } -- @TODO


-----------------
-- Expressions --
-----------------

-- Optional Expressions should be separeted to evade ambiguty and invalid expressions.
OptionalExpression :: { IR_Expression }
OptionalExpression :
    {- no expression... -}          { ExpNothing } |
    Expression                      { $1 }


Expression :: { IR_Expression }
Expression :
    -- implicit function call.
    EVAL Expression '<-' '(' ArgList ')'    { ExpFCall_Implicit $2 $5 } |

    -- literals.
    INTEGRAL                        { ExpLitInteger $1 } |
    FLOATING                        { ExpLitFloating $1 } |
    BOOLEAN                         { ExpLitBoolean $1 } |
    STRING                          { ExpLitString $1 } |

    -- arithmetic operators.
    Expression '+' Expression       { ExpSum $1 $3 } |
    Expression '-' Expression       { ExpSub $1 $3 } |
    Expression '*' Expression       { ExpMul $1 $3 } |
    Expression '/' Expression       { ExpDiv $1 $3 } |
    Expression '//' Expression      { ExpIntDiv $1 $3 } |
    Expression '**' Expression      { ExpPow $1 $3 } |
    Expression '%' Expression       { ExpMod $1 $3 } |

    -- relational operators.
    Expression '==' Expression      { ExpEq $1 $3 } |
    Expression '!=' Expression      { ExpNeq $1 $3 } |
    Expression '>' Expression       { ExpGt $1 $3 } |
    Expression '>=' Expression      { ExpGeq $1 $3 } |
    Expression '<' Expression       { ExpLt $1 $3 } |
    Expression '<=' Expression      { ExpLeq $1 $3 } |
    Expression '&&' Expression      { ExpAnd $1 $3 } |
    Expression '||' Expression      { ExpOr $1 $3 } |

    -- unary plus / minus (literal identifier).
    '+' Expression                  { $2 } | -- note: no need for an `ExpNegative` equivalent in the plus case.
    '-' Expression                  { ExpNegative $2 } |

    -- unary increment / decrement.
    '++' Expression                 { ExpLIncr $2 } |
    '--' Expression                 { ExpLDecr $2 } |
    Expression '++'                 { ExpRIncr $1 } |
    Expression '--'                 { ExpRDecr $1 } |
    
    -- explicit function call.
    IDENTIFIER '(' ArgList ')'      { ExpFCall $1 $3 } |
    
    -- lambda.
    LAMBDA ParameterList2 OptionalLambdaCapture OptionalLambdaReturn '{' CommandList '}'     { ExpLambda $4 $2 $3 $6 } |

    -- structures.
    IDENTIFIER '{' ArgList '}'      { ExpStructInstance $1 $3 } |
    
    -- variables and arrays.
    '[' ArgList ']'                 { ExpArrayInstancing $2 } |
    VariableAccess                  { ExpVariable $1 } |
    KW_IO_RC                        { ExpVariable $ VarAccess "@rc" VarAccessNothing } |
    
    -- malloc.
    NEW Type                        { ExpNew $2 } |
    
    -- enclosure.
    '(' Expression ')'              { $2 }


ArgList :: { [IR_Expression] }
ArgList :
    { [] } |
    ArgList2                        { $1 }


ArgList2 :: { [IR_Expression] }
ArgList2 :
    Expression                      { [$1] } |
    ArgList2 ',' Expression         { $1 ++ [$3] }


OptionalLambdaCapture :: { [Identifier] }
OptionalLambdaCapture : 
                        { [] } |
    CAPTURES ASDList    { $2 } -- for instance...


ASDList :: { [Identifier] }
ASDList :
    IDENTIFIER { [$1] } |
    ASDList ',' IDENTIFIER { $1 ++ [$3] }


OptionalLambdaReturn :: { IR_Type }
OptionalLambdaReturn :
    -- {- undefined type... -}     { TypeVoid } |
    L_RETURN Type               { $2 }



----------------------------
-- Variables and Literals --
----------------------------

VariableDecl :: { IR_Var } -- `V`.
VariableDecl :
    IDENTIFIER TypeSpec   { VarDecl $1 $2 }


OptionalVariableDecl :: { IR_Var } -- `V^*`.
OptionalVariableDecl :
    IDENTIFIER OptionalTypeSpec   { VarDecl $1 $2 }


TypeSpec :: { IR_Type } -- `T_S`.
TypeSpec :
    ':' Type { $2 }


OptionalTypeSpec :: { IR_Type } -- `T_S^*`.
OptionalTypeSpec :
    TypeSpec    { $1 } |
                { TypeVoid }


OptionalTypeSpec2 :: { IR_Type } -- `T_S^*`.
OptionalTypeSpec2 :
    TypeSpec    { $1 } |
                { NoType }


VariableAccess :: { IR_VarAccess }
VariableAccess :
    IDENTIFIER VariableAccess2                  { VarAccess $1 $2 }


VariableAccess2 :: { IR_VarAccess }
VariableAccess2 :
                                                { VarAccessNothing } |
    IDENTIFIER VariableAccess2                  { VarAccess $1 $2 } |
    '.' IDENTIFIER VariableAccess2              { VarAccess $2 $3 } |                    
    TypeIndex VariableAccess2                   { VarAccessIndex $1 $2 }



-----------
-- Types --
-----------

BaseType :: { IR_Type } -- `T_B`.
BaseType :
    TYPE_VOID                   { TypeVoid } |
    TYPE_INT                    { TypeInt } |
    TYPE_BOOL                   { TypeBool } |
    TYPE_FLOAT                  { TypeFloat } |
    TYPE_STRING                 { TypeString } |

    -- Generic types. (could be structures as well...)
    IDENTIFIER                  { TypeGeneric $1 }


Type :: { IR_Type }
Type : 
    BaseType                    { $1 } |
    
    -- Array types.
    BaseType TypeIndices        { TypeArray $1 $2 } |
    
    -- Function types.
    '(' TypeList ')' '->' Type  { TypeFunction $2 $5 }


TypeList :: { [IR_Type] }
TypeList :
    { [] } |
    TypeList2           { $1 }


TypeList2 :: { [IR_Type] }
TypeList2 :
    Type                { [$1] } |
    TypeList2 ',' Type   { $1 ++ [$3] }


TypeIndices :: { [IR_Expression] }
TypeIndices :
    '[' OptionalExpression ']'  { [$2] } |
    TypeIndices TypeIndex       { $1 ++ [$2] }


TypeIndex :: { IR_Expression }
TypeIndex :
    '[' Expression ']'     { $2 }



{ -- Haskell scape.
parsing_error (Token pos@(SrcPos _) lexeme) = alexError $ show
    Error { 
        error_type = SyntaxError, 
        error_msg = "Lexeme: " ++ lexeme_show lexeme ++ ".", 
        error_pos = pos
    }


-- Alex's lexer.
lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)


-- Main parsing function.
-- Run with alex on an input.
parse_sl_alex :: Alex IR_Program

-- Parses a simple SL expression.
-- Run with alex on an input.
parse_sl_exp_alex :: Alex IR_Expression


parse_sl_stat_alex :: Alex IR_Statement

-- Parses a SL program.
-- Returns an error or the AST.
parse_sl :: String -> Either Error IR_Program
parse_sl string = to_either_error $ runAlex string parse_sl_alex

parse_sl_exp :: String -> Either Error IR_Expression
parse_sl_exp string = to_either_error $ runAlex string parse_sl_exp_alex

parse_sl_stat :: String -> Either Error IR_Statement
parse_sl_stat string = to_either_error $ runAlex string parse_sl_stat_alex
}
