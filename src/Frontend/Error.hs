{-  ---------------------------------
    @file       src/Frontend/Error.hs
    @details    Defines the frontend error structure.
-}

{-# LANGUAGE InstanceSigs #-} -- for which doesn't allow by default...

module Frontend.Error ( ErrorType(..),
                        Error(..),
                        SrcPos(..)) where

import Frontend.Token
import Frontend.Pretty


data ErrorType = 
    LexicalError    |
    SyntaxError     |
    SemanticalError | 
    ExecutionError
    deriving (Eq, Show, Read)


data Error = 
    Error {
        error_type  :: ErrorType,
        error_msg   :: String,
        error_pos   :: SrcPos
    } | 
    MultipleErrors [Error]
    deriving (Eq, Show, Read)



-- Representing the error
-------------------------

instance Pretty SrcPos where
    pretty :: SrcPos -> PrettyContext ()
    pretty (SrcPos (line, col)) = do
        pc_tell $ "(" ++ show line ++ ", " ++ show col ++ ")"


instance Pretty ErrorType where
    pretty :: ErrorType -> PrettyContext ()
    pretty LexicalError     = pc_tell "Lexical error"
    pretty SyntaxError      = pc_tell "Syntax error"
    pretty SemanticalError  = pc_tell "Semantical error"
    pretty ExecutionError   = pc_tell "Runtime error"


instance Pretty Error where
    pretty :: Error -> PrettyContext ()
    pretty (Error t msg pos) = do
        pc_tell "=== "
        pc_tell $ show t

        pc_tell " at "
        pretty pos
        pc_tell "."

        pc_newline
        case msg of 
            ""  -> return ()
            _   -> do
                pc_tell " Details: "
                pc_tell msg

    pretty (MultipleErrors errors@((Error t _ _):_)) = do
        pc_tell "=== "
        pc_tell $ show t
        pc_tell "."
        pc_newline

        __pretty_error_list errors

        where
            __pretty_error_list []      = return ()
            __pretty_error_list ((Error _ msg pos):es)  = do
                pc_tell "\t- At " 
                pretty pos
                pc_tell "."

                case msg of 
                    ""  -> return ()
                    _   -> do
                        pc_tell " Details: "
                        pc_tell msg
                        
                pc_newline
                __pretty_error_list es
