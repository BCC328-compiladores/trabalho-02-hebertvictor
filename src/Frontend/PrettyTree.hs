{-  -------------------------------------
    @file       src/Frontend/PrettyTree.hs
    @details    ~
-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-} -- for which doesn't allow by default...
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Frontend.PrettyTree (pretty_sl_tree) where

import Frontend.Pretty
import Frontend.IR
import Frontend.Error()



newtype PrettyTree a = PrettyTree a

pretty_sl_tree :: IR_Program -> String
pretty_sl_tree ir = (pc_get_string . pretty) (PrettyTree ir)


instance Pretty (PrettyTree IR_Program) where
    pretty :: (PrettyTree IR_Program) -> PrettyContext ()
    pretty (PrettyTree (Program statement_list)) = do
        pc_tell "Program [ "
        pc_increment_identation >> pc_newline
        pc_list_wsep (pc_tell "," >> pc_newline >> pc_newline) (map PrettyTree statement_list)
        pc_decrement_identation >> pc_newline
        pc_tell "]"


instance Pretty (PrettyTree IR_Statement) where
    pretty :: (PrettyTree IR_Statement) -> PrettyContext ()
    pretty (PrettyTree (FuncDef fname rtype param gtypes body pos)) = do
        pc_tell "FuncDef {"
        pc_increment_identation >> pc_newline
        
        pc_tell ("symbol_name = " ++ fname)
        pc_newline

        pc_tell "function_rtype = "
        pretty $ PrettyTree rtype
        pc_newline

        pc_tell "function_parameters = [ "
        case param of
            [] -> pc_tell ""
            _ -> do
                pc_increment_identation >> pc_newline
                pc_list_wsep (pc_tell ", " >> pc_newline >> pc_newline) (map PrettyTree param)
                pc_decrement_identation >> pc_newline
        pc_tell "]"
        pc_newline

        pc_tell "function_gtypes = [ "
        case gtypes of 
            [] -> pc_tell ""
            _ -> do
                pc_increment_identation >> pc_newline
                pc_list_wsep (pc_tell ", " >> pc_newline) (map P_Identifier gtypes)
                pc_decrement_identation >> pc_newline
        pc_tell "]"
        pc_newline

        pc_tell "function_body = [ "
        case body of
            [] -> pc_tell ""
            _ -> do
                pc_increment_identation >> pc_newline
                pc_list_wsep (pc_tell ", " >> pc_newline >> pc_newline) (map PrettyTree body)
                pc_decrement_identation >> pc_newline
        pc_tell "]"

        pc_newline
        pc_tell "symbol_pos = "
        pretty pos

        pc_decrement_identation >> pc_newline
        pc_tell "}"

    pretty (PrettyTree (StructDef sname sfields pos)) = do
        pc_tell "StructDef { "
        pc_newline >> pc_increment_identation

        pc_tell ("symbol_name = " ++ sname)
        pc_newline >> pc_increment_identation

        pc_tell "fields = [ "
        pc_increment_identation
        pc_list_wsep (pc_tell ", " >> pc_newline) (map PrettyTree sfields)
        pc_decrement_identation
        pc_tell "]"

        pc_newline
        pc_tell "symbol_pos = "
        pretty pos

        pc_decrement_identation >> pc_newline
        pc_tell "}"


instance Pretty (PrettyTree IR_LocatedCommand) where
    pretty :: (PrettyTree IR_LocatedCommand) -> PrettyContext ()
    pretty (PrettyTree (LC cmd pos)) = do
        pretty $ PrettyTree cmd
        pc_newline
        pc_tell $ "Pos = "
        pretty pos
        


instance Pretty (PrettyTree IR_Command) where
    pretty :: (PrettyTree IR_Command) -> PrettyContext ()
    pretty (PrettyTree (VarDef var_decl expr)) = do
        pc_tell "VarDef "
        pc_increment_identation >> pc_newline

        --pc_tell "( "
        pretty $ PrettyTree var_decl
        pc_newline
        --pc_tell ")"
        
        pc_newline >> pc_newline
        --pc_tell "( "
        pretty $ PrettyTree expr
        --pc_tell ")"
        pc_decrement_identation

    pretty (PrettyTree (Assignment var_access expr)) = do
        pc_tell "Assignment "
        pc_increment_identation >> pc_newline
        pretty (PrettyTree var_access) >> pc_newline
        pc_newline
        pretty (PrettyTree expr)
        pc_decrement_identation

    pretty (PrettyTree (Return expr)) = do
        pc_tell "Return ("
        pc_increment_identation >> pc_newline
        pretty $ PrettyTree expr
        pc_decrement_identation
        pc_newline >> pc_tell ")"

    pretty (PrettyTree (If cond if_cmd else_cmd)) = do
        -- IF header.
        pc_tell "If ( "
        pc_increment_identation >> pc_newline
        pretty $ PrettyTree cond
        pc_decrement_identation >> pc_newline
        pc_tell ")"
        pc_newline >> pc_tell "[ "
        pc_increment_identation >> pc_newline

        -- IF body.
        pc_list_wsep (pc_tell ", " >> pc_newline) (map PrettyTree if_cmd) -- pretty if_cmd
        pc_decrement_identation >> pc_newline
        pc_tell "]"

        -- ELSE header.
        case else_cmd of
            [] -> pc_newline >> pc_tell "[]"

            _ -> do
                pc_newline
                pc_tell "[ "
                pc_increment_identation >> pc_newline
                pc_list_wsep (pc_tell ", " >> pc_newline) (map PrettyTree else_cmd) -- pretty else_cmd
                pc_decrement_identation >> pc_newline
                pc_tell "]"
        --pc_decrement_identation

    pretty (PrettyTree (While cond cmds)) = do
        pc_tell "While ( "
        pretty $ PrettyTree cond
        pc_tell ")" >> pc_newline >> pc_tell "[ "
        pc_increment_identation >> pc_newline

        pc_list_wsep (pc_tell ", " >> pc_newline) (map PrettyTree cmds)

        pc_decrement_identation >> pc_newline
        pc_tell "]"

    pretty (PrettyTree (For ini cond incr cmds)) = do
        pc_tell "For ( "
        pc_increment_identation >> pc_newline
        pretty $ PrettyTree ini
        pc_decrement_identation >> pc_newline
        pc_tell ")"

        pc_newline >> pc_tell "("
        pc_increment_identation >> pc_newline
        pretty $ PrettyTree cond
        pc_decrement_identation >> pc_newline
        pc_tell ") "

        pc_newline >> pc_tell "("
        pc_increment_identation >> pc_newline
        pretty $ PrettyTree incr
        pc_decrement_identation >> pc_newline
        pc_tell ")"

        pc_newline >> pc_tell "["
        pc_increment_identation >> pc_newline
        pc_list_wsep (pc_tell ", " >> pc_newline) (map PrettyTree cmds)
        pc_decrement_identation >> pc_newline
        pc_tell "]"

    pretty (PrettyTree (CmdExpression expr)) = do
        pc_tell "CmdExpression ( " 
        pc_increment_identation >> pc_newline
        pretty $ PrettyTree expr
        pc_decrement_identation >> pc_newline
        pc_tell ")"

    pretty (PrettyTree (Print expr)) = do
        pc_tell "Print ( "
        pc_increment_identation >> pc_newline
        pretty $ PrettyTree expr
        pc_decrement_identation >> pc_newline
        pc_tell ")"

    pretty (PrettyTree (Scan expr)) = do
        pc_tell "Scan ( "
        pc_increment_identation >> pc_newline
        pretty $ PrettyTree expr
        pc_decrement_identation >> pc_newline
        pc_tell ")"
        

instance Pretty (PrettyTree IR_Var) where
    pretty :: (PrettyTree IR_Var) -> PrettyContext ()
    pretty (PrettyTree (VarDecl identifier t)) = do
        pc_tell "VarDecl "
        pc_increment_identation >> pc_newline

        pc_tell ("\"" ++ identifier ++ "\"")

        pc_newline

        pretty $ PrettyTree t

        pc_decrement_identation


instance Pretty (PrettyTree IR_Type) where
    pretty :: (PrettyTree IR_Type) -> PrettyContext ()
    pretty (PrettyTree TypeVoid)         = pc_tell "TypeVoid"
    pretty (PrettyTree TypeBool)         = pc_tell "TypeBool"
    pretty (PrettyTree TypeInt)          = pc_tell "TypeInt"
    pretty (PrettyTree TypeFloat)        = pc_tell "TypeFloat"
    pretty (PrettyTree TypeString)       = pc_tell "TypeString"
    pretty (PrettyTree (TypeGeneric x))  = pc_tell ("(TypeGeneric \"" ++ x ++ "\")")

    pretty (PrettyTree (TypeArray base_type index_expressions)) = do
        pc_tell "TypeArray "
        pc_increment_identation >> pc_newline
        pretty $ PrettyTree base_type
        pc_newline
        __pretty_index_expr index_expressions
        pc_decrement_identation

        where
        __pretty_index_expr []              = pc_tell "[]"
        __pretty_index_expr [ExpNothing]    = pc_tell "[ ExpNothing ]"
        __pretty_index_expr [x]             = pc_tell "[ " >> pretty (PrettyTree x) >> pc_tell " ]"
        __pretty_index_expr exp_list          = do
            pc_tell "[ "
            pc_increment_identation >> pc_newline
            pc_list_wsep (pc_tell ", " >> pc_newline) (map PrettyTree exp_list)
            pc_decrement_identation >> pc_newline
            pc_tell "]"
            --__pretty_index_expr [x] >> pc_tell "," >> pc_newline >> __pretty_index_expr xs

    pretty (PrettyTree (TypeFunction pt rt)) = do
        pc_tell "TypeFunction "
        pc_increment_identation >> pc_newline
        pc_tell "("
        pc_list_wsep (pc_tell ", ") (map PrettyTree pt)
        pc_tell ")" >> pc_newline >> pc_tell "("
        pretty $ PrettyTree rt
        pc_tell ")"
        pc_decrement_identation >> pc_newline


pc_binop_pt :: (Pretty (PrettyTree a1), Pretty (PrettyTree a2)) => a1 -> a2 -> String -> PrettyContext ()
pc_binop_pt exp1 exp2 op_str = do
    pc_tell op_str
    pc_increment_identation >> pc_newline
    pretty (PrettyTree exp1)
    pc_newline >> pc_newline
    pretty (PrettyTree exp2)
    pc_decrement_identation

instance Pretty (PrettyTree IR_Expression) where
    pretty :: (PrettyTree IR_Expression) -> PrettyContext ()
    pretty (PrettyTree ExpNothing) = pc_tell "ExpNothing"

    pretty (PrettyTree (ExpVariable variable)) = pretty (PrettyTree variable)

    pretty (PrettyTree (ExpLitInteger x))       = pc_tell ("ExpLitInteger " ++ show x)
    pretty (PrettyTree (ExpLitFloating x))      = pc_tell ("ExpLitFloating " ++ show x)
    pretty (PrettyTree (ExpLitBoolean False))   = pc_tell "ExpLitBoolean False"
    pretty (PrettyTree (ExpLitBoolean True))    = pc_tell "ExpLitBoolean True"
    pretty (PrettyTree (ExpLitString x))        = pc_tell ("ExpLitString " ++ show x)
    
    pretty (PrettyTree (ExpSum exp1 exp2)) = pc_binop_pt exp1 exp2 "ExpSum "
    pretty (PrettyTree (ExpSub exp1 exp2)) = pc_binop_pt exp1 exp2 "ExpSub "
    pretty (PrettyTree (ExpMul exp1 exp2)) = pc_binop_pt exp1 exp2 "ExpMul "
    pretty (PrettyTree (ExpDiv exp1 exp2)) = pc_binop_pt exp1 exp2 "ExpDiv "

    pretty (PrettyTree (ExpIntDiv exp1 exp2)) = pc_binop_pt exp1 exp2 "ExpIntDiv "
    pretty (PrettyTree (ExpMod exp1 exp2)) = pc_binop_pt exp1 exp2 "ExpMod "
    pretty (PrettyTree (ExpPow exp1 exp2)) = pc_binop_pt exp1 exp2 "ExpPow "

    pretty (PrettyTree (ExpNegative expr))  = do 
        pc_tell "ExpNegative " 
        pc_increment_identation >> pc_newline
        pretty (PrettyTree expr)
        pc_decrement_identation >> pc_newline

    pretty (PrettyTree (ExpAnd exp1 exp2))  = pc_binop_pt exp1 exp2 "ExpAnd " 
    pretty (PrettyTree (ExpOr exp1 exp2))   = pc_binop_pt exp1 exp2 "ExpOr "

    pretty (PrettyTree (ExpEq exp1 exp2))   = pc_binop_pt exp1 exp2 "ExpEq "
    pretty (PrettyTree (ExpNeq exp1 exp2))  = pc_binop_pt exp1 exp2 "ExpNeq "
    pretty (PrettyTree (ExpGt exp1 exp2))   = pc_binop_pt exp1 exp2 "ExpGt "
    pretty (PrettyTree (ExpGeq exp1 exp2))  = pc_binop_pt exp1 exp2 "ExpGeq "
    pretty (PrettyTree (ExpLt exp1 exp2))   = pc_binop_pt exp1 exp2 "ExpLt "
    pretty (PrettyTree (ExpLeq exp1 exp2))  = pc_binop_pt exp1 exp2 "ExpLeq "

    pretty (PrettyTree (ExpFCall fname args)) = do
        pc_tell "ExpFCall "
        pc_increment_identation >> pc_newline
        pc_tell ("\"" ++ fname ++ "\"")
        pc_newline
        pc_tell "[ "
        case args of
            [] -> pc_tell " ]"

            [_] -> do 
                pc_list_wsep (pc_tell ", " >> pc_newline) (map PrettyTree args)
                pc_tell " ]"

            _ -> do
                pc_increment_identation >> pc_newline
                pc_list_wsep (pc_tell ", " >> pc_newline) (map PrettyTree args)
                pc_decrement_identation
                pc_newline >> pc_tell "]"
            
        pc_decrement_identation

    pretty (PrettyTree (ExpStructInstance sname args)) = do
        pc_tell "ExpStructInstance "
        pc_increment_identation >> pc_newline
        pc_tell ("\"" ++ sname ++ "\"")
        pc_newline
        pc_tell "[ "
        pc_list_wsep (pc_newline >> pc_tell ", ") (map PrettyTree args)
        pc_newline >> pc_tell "]"
        pc_decrement_identation

    pretty (PrettyTree (ExpArrayInstancing args)) = do
        pc_tell "ExpArrayInstancing "
        pc_increment_identation >> pc_newline
        pc_tell "[ "
        pc_list_wsep (pc_newline >> pc_tell ", ") (map PrettyTree args)
        pc_newline >> pc_tell "]"
        pc_decrement_identation

    pretty (PrettyTree (ExpNew t)) = do
        pc_tell "ExpNew " 
        pc_increment_identation >> pc_newline
        pretty (PrettyTree t)
        pc_decrement_identation


    pretty (PrettyTree (ExpLIncr expr)) = do
        pc_tell "ExpLIncr "
        pc_increment_identation >> pc_newline
        pretty (PrettyTree expr)
        pc_decrement_identation

    pretty (PrettyTree (ExpLDecr expr)) = do
        pc_tell "ExpLDecr "
        pc_increment_identation >> pc_newline
        pretty (PrettyTree expr)
        pc_decrement_identation

    pretty (PrettyTree (ExpRIncr expr)) = do
        pc_tell "ExpRIncr "
        pc_increment_identation >> pc_newline
        pretty (PrettyTree expr)
        pc_decrement_identation

    pretty (PrettyTree (ExpRDecr expr)) = do
        pc_tell "ExpRDecr "
        pc_increment_identation >> pc_newline
        pretty (PrettyTree expr)
        pc_decrement_identation



instance Pretty (PrettyTree IR_VarAccess) where
    pretty :: (PrettyTree IR_VarAccess) -> PrettyContext ()
    pretty (PrettyTree (VarAccess varname var_rest)) = do
        pc_tell "VarAccess "
        pc_increment_identation >> pc_newline
        pc_tell ("\"" ++ varname ++ "\"")
        pc_newline

        --pc_tell "( "
        pretty $ PrettyTree var_rest
        -- pc_tell ")"
        pc_decrement_identation

    pretty (PrettyTree (VarAccessIndex index_expression var_rest)) = do
        pc_tell "VarAccessIndex "
        pc_increment_identation >> pc_newline
        -- pc_tell "( "
        pretty $ PrettyTree index_expression

        pretty $ PrettyTree var_rest
        -- pc_tell ")"
        pc_decrement_identation

    pretty (PrettyTree VarAccessNothing) = pc_tell "VarAccessNothing"
