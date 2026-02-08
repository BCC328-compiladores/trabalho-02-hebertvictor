{-  ------------------------------
    @file       src/Frontend/IR.hs
    @details    Defines IR (Intemediate-Representation) for the analysis.
-}

{-# LANGUAGE InstanceSigs #-} -- for which doesn't allow by default...
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Frontend.IR where

import Frontend.Error
import Frontend.Pretty


-- specifies an identifier string.
type Identifier = String;


-- A program is a sequence of statements.
data IR_Program = Program [IR_Statement]
    deriving (Eq, Show, Read)


-- A statement is a higher-level language construct...
data IR_Statement =
    
    -- Defines a function.
    FuncDef { 
        symbol_name :: Identifier,
        function_rtype :: IR_Type,
        function_parameters :: [IR_Var],

        -- Generic types (on the forall function decl.).
        function_gtypes :: [Identifier],

        -- instructions themselves...
        function_body :: [IR_LocatedCommand],
        
        symbol_pos :: SrcPos
    } | 

    -- Defines a structure.
    StructDef {
        symbol_name :: Identifier,
        fields :: [IR_Var],

        symbol_pos :: SrcPos
    }
    
    deriving (Eq, Show, Read)


data IR_Command =

    VarDef IR_Var IR_Expression |                   -- Let x = E;
    Assignment IR_VarAccess IR_Expression |         -- x = ...
    Return IR_Expression |                          -- return E;

    -- Actually, for the control-flux, it can be packed into constructor.
    -- In that case, every if has an else; it can be empty or with more 'ifs' thought.
    If IR_Expression [IR_LocatedCommand] [IR_LocatedCommand] |
    
    -- In constrast with the grammar, for the repetition it may be better have them separately.
    While IR_Expression [IR_LocatedCommand] |
    For IR_LocatedCommand IR_Expression IR_Expression [IR_LocatedCommand] | -- @TODO For should be parsed into a while!

    CmdExpression IR_Expression |

    Print IR_Expression |
    Scan IR_Expression
    
    deriving (Eq, Show, Read)


data IR_LocatedCommand = LC {
        lc_cmd :: IR_Command,
        lc_pos :: SrcPos
    } deriving (Eq, Show, Read)


-- Represents a variable (assigned with the type).
data IR_Var = 
    -- declaring it.
    VarDecl Identifier IR_Type
    deriving (Eq, Show, Read)


data IR_VarAccess =
    -- accessing the variable, in a tree- (or rather list-) like structure...
    -- list, since the access is sequential...
    VarAccess Identifier IR_VarAccess           | -- `x.y`
    VarAccessIndex IR_Expression IR_VarAccess   | -- `x[E]`
    
    -- represents the tree's terminal...
    VarAccessNothing

    deriving (Eq, Show, Read)


-- @TODO more types constructors oh no. to factor with `Token.hs` defs...
data IR_Type = 
    -- value used for erroing in the semantical analysis...
    -- won't be used as a return of neither of the parser nor semantical analyzer...
    NoType |
    
    -- Standard Types
    TypeVoid    |
    TypeBool    |
    TypeInt     | 
    TypeFloat   |
    TypeString  |
    
    -- Array Types
    TypeArray IR_Type [IR_Expression] | -- @TODO <- list of integers rather?

    -- Functions.
    TypeFunction [IR_Type] IR_Type | -- parameter type -> RT.

    -- Generics
    TypeGeneric Identifier |

    -- @TODO やはり、あった方がいい!
    -- parsing doesn't get us that, it
    -- is actually get in the semantical analysis.
    TypeStruct Identifier

    deriving (Eq, Show, Read)


-- Non-strict equality between types (without the need to check the expressions.)
type_eq :: IR_Type -> IR_Type -> Bool
TypeVoid            `type_eq` TypeVoid              = True
TypeBool            `type_eq` TypeBool              = True
TypeInt             `type_eq` TypeInt               = True
TypeFloat           `type_eq` TypeFloat             = True
TypeString          `type_eq` TypeString            = True
(TypeGeneric a)     `type_eq` (TypeGeneric b)       = a == b
(TypeArray t exps)  `type_eq` (TypeArray t2 exps2)  = 
    t `type_eq` t2 && (length exps == length exps2)
(TypeFunction ts r) `type_eq` (TypeFunction ts2 r2) =
    r `type_eq` r2 && length ts == length ts2 && (and $ (\(a, b) -> a `type_eq` b) <$> (zip ts ts2))
(TypeStruct sname)  `type_eq` (TypeStruct sname2)   = sname == sname2
type_eq _ _ = False


-- diz se é exatamente genérico d'algo.
is_generic :: IR_Type -> Bool
is_generic (TypeGeneric _) = True
is_generic _ = False

-- diz se eventualmente o tipo tem algo genérico...
have_generic :: IR_Type -> Bool
have_generic (TypeGeneric _)        = True

-- recusion on the type tree.
have_generic (TypeArray t _)        = have_generic t
have_generic (TypeFunction ts tr)   = or (have_generic <$> ts) || have_generic tr
have_generic _                      = False




-- now, that is the most tedious part...
data IR_Expression = 
    ExpNothing | -- Empty expression~
    ExpVariable     IR_VarAccess |
    
    -- Literals.
    ExpLitInteger   Integer |
    ExpLitFloating  Double |
    ExpLitBoolean   Bool |
    ExpLitString    String |

    -- Arithimetic.
    ExpSum          IR_Expression IR_Expression |
    ExpSub          IR_Expression IR_Expression |
    ExpMul          IR_Expression IR_Expression |
    ExpDiv          IR_Expression IR_Expression |
    ExpIntDiv       IR_Expression IR_Expression |
    ExpMod          IR_Expression IR_Expression |
    ExpPow          IR_Expression IR_Expression |
    ExpNegative     IR_Expression |

    -- Logical.
    ExpAnd          IR_Expression IR_Expression |
    ExpOr           IR_Expression IR_Expression |
    
    -- Relational.
    ExpEq           IR_Expression IR_Expression |
    ExpNeq          IR_Expression IR_Expression |
    ExpGt           IR_Expression IR_Expression |
    ExpGeq          IR_Expression IR_Expression |
    ExpLt           IR_Expression IR_Expression |
    ExpLeq          IR_Expression IR_Expression |
    
    -- Increment / Decrement
    ExpLIncr        IR_Expression |
    ExpRIncr        IR_Expression |
    ExpLDecr        IR_Expression |
    ExpRDecr        IR_Expression |

    -- Function call.
    ExpFCall Identifier [IR_Expression] |
    ExpFCall_Implicit IR_Expression [IR_Expression] |
    
    -- Structure instancing.
    ExpStructInstance Identifier [IR_Expression] | -- `X { values... }`

    -- Array instancing.
    ExpArrayInstancing [IR_Expression] |

    -- Memory allocation.
    ExpNew IR_Type |

    -- Lambda.
    ExpLambda IR_Type [IR_Var] [Identifier] [IR_LocatedCommand]
    
    deriving (Eq, Show, Read)



---------------
-- Pretty IR --
---------------

newtype P_Identifier = P_Identifier Identifier
    deriving (Eq, Show)


instance Pretty P_Identifier where
    pretty :: P_Identifier -> PrettyContext ()
    pretty (P_Identifier str) = pc_tell str


instance Pretty IR_Program where
    pretty :: IR_Program -> PrettyContext ()
    pretty (Program statement_list) = 
        pc_list_wsep (pc_newline >> pc_newline) statement_list


instance Pretty IR_Statement where
    pretty :: IR_Statement -> PrettyContext ()
    pretty (FuncDef fname rtype param gtypes body _) = do
        case gtypes of
            [] -> pc_tell ""
            
            _ -> do
                pc_tell "forall "
                pc_list_wsep (pc_tell " ") $ map P_Identifier gtypes
                pc_tell " . "

        pc_tell "func "
        pc_tell fname

        pc_tell "("
        pc_list_wsep (pc_tell ", ") param -- pretty param
        pc_tell ")"

        pc_tell " : "
        pretty rtype
        pc_tell " {"

        pc_increment_identation
        pc_newline
        pc_list_wsep (pc_newline) body
        pc_decrement_identation
        pc_newline

        pc_tell "}"

    pretty (StructDef sname sfields _) = do
        pc_tell "struct "
        pc_tell sname
        pc_tell " {"

        pc_increment_identation
        pc_newline
        pc_list_wsep ((pc_tell ";") >> pc_newline) sfields
        pc_tell ";" -- lastone
        pc_decrement_identation
        pc_newline

        pc_tell "}"


instance Pretty IR_Command where
    pretty :: IR_Command -> PrettyContext ()
    pretty (VarDef var_decl expr) = do
        pc_tell "let "
        pretty var_decl

        case expr of
            ExpNothing -> pc_tell ""
            _ -> do 
                pc_tell " = "
                pretty expr
            
        pc_tell ";"

    pretty (Assignment var_access expr) = do
        pretty var_access
        pc_tell " = "
        pretty expr
        pc_tell ";"

    pretty (Return expr) = do
        pc_tell "return "
        pretty expr
        pc_tell ";"

    pretty (If cond if_cmd else_cmd) = do
        -- IF header.
        pc_tell "if ("
        pretty cond
        pc_tell ") {"
        pc_increment_identation
        pc_newline

        -- IF body.
        pc_list_wsep pc_newline if_cmd -- pretty if_cmd
        pc_decrement_identation
        pc_newline
        pc_tell "}"

        -- ELSE header.
        case else_cmd of
            [] -> pc_tell ""
            [LC (If _ _ _) _] -> do
                pc_tell " el"
                pretty $ else_cmd !! 0

            _ -> do
                pc_tell " else {"
                pc_increment_identation
                pc_newline
                pc_list_wsep pc_newline else_cmd -- pretty else_cmd
                pc_decrement_identation
                pc_newline
                pc_tell "}"
                pc_newline

    pretty (While cond cmds) = do
        pc_tell "while ("
        pretty cond
        pc_tell ") {"

        pc_increment_identation
        pc_newline

        pc_list_wsep (pc_newline) cmds

        pc_decrement_identation
        pc_newline
        pc_tell "}"
        pc_newline

    pretty (For ini cond incr cmds) = do
        pc_tell "for ("
        pretty ini
        pc_tell " "
        pretty cond
        pc_tell "; "
        pretty incr
        pc_tell ") {"

        pc_increment_identation
        pc_newline

        pc_list_wsep (pc_newline) cmds

        pc_decrement_identation
        pc_newline
        pc_tell "}"
        pc_newline

    pretty (CmdExpression expr) = do
        pretty expr
        pc_tell ";"

    pretty (Print expr) = do
        pc_tell "@print<<"
        pretty expr
        pc_tell ">>;"

    pretty (Scan expr) = do
        pc_tell "@scan<<"
        pretty expr
        pc_tell ">>;"


instance Pretty IR_Var where
    pretty :: IR_Var -> PrettyContext ()
    pretty (VarDecl identifier t) = do
        pc_tell identifier

        -- avoiding making the `void` explict when printing the variable type.
        case t of
            TypeVoid -> pc_tell ""
            _ -> do 
                pc_tell " : "
                pretty t


instance Pretty IR_LocatedCommand where
    pretty :: IR_LocatedCommand -> PrettyContext ()
    pretty (LC cmd _) = pretty cmd


instance Pretty IR_Type where
    pretty :: IR_Type -> PrettyContext ()
    pretty NoType           = pc_tell ""
    pretty TypeVoid         = pc_tell "void"
    pretty TypeBool         = pc_tell "bool"
    pretty TypeInt          = pc_tell "int"
    pretty TypeFloat        = pc_tell "float"
    pretty TypeString       = pc_tell "string"
    pretty (TypeGeneric x)  = pc_tell x
    pretty (TypeStruct x)   = pretty (TypeGeneric x) -- >> pc_tell "*"

    pretty (TypeArray base_type index_expressions) = do
        pretty base_type
        __pretty_index_expr index_expressions

        where
        __pretty_index_expr []              = pc_tell ""
        __pretty_index_expr [ExpNothing]    = pc_tell "[" >> pc_tell "]"
        __pretty_index_expr [x]             = pc_tell "[" >> pretty x >> pc_tell "]"
        __pretty_index_expr (x:xs)          = __pretty_index_expr [x] >> __pretty_index_expr xs

    pretty (TypeFunction pts rt) = do
        pc_tell "("
        pc_list_wsep (pc_tell ", ") pts

        pc_tell ") -> "
        pretty rt


pc_binop :: (Pretty t1, Pretty t2) => t1 -> t2 -> String -> PrettyContext ()
pc_binop exp1 exp2 op_str = do
    pretty exp1
    pc_tell $ " " ++ op_str ++ " "
    pretty exp2


instance Pretty IR_Expression where
    pretty :: IR_Expression -> PrettyContext ()
    pretty ExpNothing = pc_tell "NOTHING"

    pretty (ExpVariable variable) = pretty variable

    pretty (ExpLitInteger x) = pc_tell $ show x
    pretty (ExpLitFloating x) = pc_tell $ show x
    pretty (ExpLitBoolean False) = pc_tell "false"
    pretty (ExpLitBoolean True) = pc_tell "true"
    pretty (ExpLitString x) = pc_tell $ show x
    
    pretty (ExpSum exp1 exp2) = pc_binop exp1 exp2 "+"
    pretty (ExpSub exp1 exp2) = pc_binop exp1 exp2 "-"
    pretty (ExpMul exp1 exp2) = pc_binop exp1 exp2 "*"
    pretty (ExpDiv exp1 exp2) = pc_binop exp1 exp2 "/"
    pretty (ExpIntDiv exp1 exp2) = pc_binop exp1 exp2 "//"
    pretty (ExpMod exp1 exp2) = pc_binop exp1 exp2 "%"
    pretty (ExpPow exp1 exp2) = pc_binop exp1 exp2 "**"

    pretty (ExpNegative expr) = do
        pc_tell "- "
        pretty expr

    pretty (ExpAnd exp1 exp2) = pc_binop exp1 exp2 "&&"
    pretty (ExpOr exp1 exp2) = pc_binop exp1 exp2 "||"

    pretty (ExpEq exp1 exp2) = pc_binop exp1 exp2 "=="
    pretty (ExpNeq exp1 exp2) = pc_binop exp1 exp2 "!="
    pretty (ExpGt exp1 exp2) = pc_binop exp1 exp2 ">"
    pretty (ExpGeq exp1 exp2) = pc_binop exp1 exp2 ">="
    pretty (ExpLt exp1 exp2) = pc_binop exp1 exp2 "<"
    pretty (ExpLeq exp1 exp2) = pc_binop exp1 exp2 "<="

    pretty (ExpFCall fname args) = do
        pc_tell fname
        pc_tell "("
        pc_list_wsep (pc_tell ", ") args
        pc_tell ")"

    pretty (ExpStructInstance sname args) = do
        pc_tell sname
        pc_tell " { "
        pc_list_wsep (pc_tell ", ") args
        pc_tell " }"

    pretty (ExpArrayInstancing args) = do
        pc_tell "[ "
        pc_list_wsep (pc_tell ", ") args
        pc_tell " ]"

    pretty (ExpNew t) = do
        pc_tell "new "
        pretty t 

    pretty (ExpLIncr expr) = (pc_tell "++ ") >> pretty expr
    pretty (ExpLDecr expr) = (pc_tell "-- ") >> pretty expr
    pretty (ExpRIncr expr) = (pretty expr) >> pc_tell " ++"
    pretty (ExpRDecr expr) = (pretty expr) >> pc_tell " --"


instance Pretty IR_VarAccess where
    pretty :: IR_VarAccess -> PrettyContext ()
    pretty (VarAccess varname var_rest) = do
        pc_tell varname
        case var_rest of
            VarAccess _ _ -> do
                pc_tell "."

            _ -> do
                pc_tell ""

        pretty var_rest

    pretty (VarAccessIndex index_expression var_rest) = do
        pc_tell "["
        pretty index_expression
        pc_tell "]"

        case var_rest of 
            VarAccess _ _ -> do
                pc_tell "."
            _ -> 
                pc_tell ""

        pretty var_rest

    pretty VarAccessNothing = pc_tell ""

