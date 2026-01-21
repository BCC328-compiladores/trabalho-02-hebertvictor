{-  ------------------------------------------
    @file       src/Frontend/Value.hs
    @brief      Value and basic semantics for default data types.
-}

module Frontend.Value where

import Frontend.IR


------------------------------------------------------
-- Value and basic semantics for default data types --
------------------------------------------------------

-- Represents a lvalue of one of the default types,
-- in the program memory.
data Value =
    ValueInt        Integer |
    ValueFloat      Double  |
    ValueString     String  |
    ValueBool       Bool    |
    ValueArray      [Value] |
    ValueFunction   Identifier |
    ValueStruct     Identifier [Value] |
    ValueUnknown    -- represents nothing...
    deriving (Show, Eq)

-- Represents a rvalue to a variable.
-- Mainly needed to workaround the increment operations...
data Reference =
    ReferênciaNão |
    Referência IR_VarAccess
    deriving (Show)


-- Operators and whatnot

infixl 6 ||+||
infixl 6 ||-||

infixl 7 ||*||
infixl 7 ||/||
infixl 7 ||//||
infixl 7 ||%||

infixr 8 ||**||


(||!||) :: Value -> Value
(||!||) (ValueInt x)                    = ValueInt $ - x
(||!||) (ValueFloat x)                  = ValueFloat $ - x
(||!||) (ValueBool x)                   = ValueBool $ not x 
(||!||) _                               = undefined


(||+||) :: Value -> Value -> Value
(ValueInt x) ||+|| (ValueInt y)         = ValueInt (x + y)
(ValueFloat x) ||+|| (ValueFloat y)     = ValueFloat (x + y)
_ ||+|| _                               = undefined

(||-||) :: Value -> Value -> Value
x ||-|| y     = x ||+|| ((||!||) y)

(||*||) :: Value -> Value -> Value
(ValueInt x) ||*|| (ValueInt y)         = ValueInt (x * y)
(ValueFloat x) ||*|| (ValueFloat y)     = ValueFloat (x * y)
_ ||*|| _                               = undefined

(||/||) :: Value -> Value -> Value
(ValueInt x) ||/|| (ValueInt y)         = ValueInt (x `div` y)
(ValueFloat x) ||/|| (ValueFloat y)     = ValueFloat (x / y)
_ ||/|| _                               = undefined

(||//||) :: Value -> Value -> Value
(ValueInt x) ||//|| (ValueInt y)        = ValueInt (x `div` y)
_ ||//|| _                              = undefined

(||%||) :: Value -> Value -> Value
(ValueInt x) ||%|| (ValueInt y)         = ValueInt (x `mod` y)
_ ||%|| _                               = undefined

(||**||) :: Value -> Value -> Value
(ValueInt x) ||**|| (ValueInt y)        = ValueInt (x ^ y)
(ValueFloat x) ||**|| (ValueFloat y)    = ValueFloat (x ** y)
_ ||**|| _                              = undefined

(||&&||) :: Value -> Value -> Value
(ValueBool x) ||&&|| (ValueBool y)      = ValueBool (x && y)
_ ||&&|| _                              = undefined

(||||||) :: Value -> Value -> Value
(ValueBool x) |||||| (ValueBool y)      = ValueBool (x || y)
_ |||||| _                              = undefined

(||==||) :: Value -> Value -> Value
(ValueInt x) ||==|| (ValueInt y)        = ValueBool (x == y)
(ValueFloat x) ||==|| (ValueFloat y)    = ValueBool (x == y)
_ ||==|| _                              = undefined

(||!=||) :: Value -> Value -> Value
(ValueInt x) ||!=|| (ValueInt y)        = ValueBool (x /= y)
(ValueFloat x) ||!=|| (ValueFloat y)    = ValueBool (x /= y)
_ ||!=|| _                              = undefined

(||>||) :: Value -> Value -> Value
(ValueInt x) ||>|| (ValueInt y)         = ValueBool (x > y)
(ValueFloat x) ||>|| (ValueFloat y)     = ValueBool (x > y)
_ ||>|| _                               = undefined

(||>=||) :: Value -> Value -> Value
(ValueInt x) ||>=|| (ValueInt y)        = ValueBool (x >= y)
(ValueFloat x) ||>=|| (ValueFloat y)    = ValueBool (x >= y)
_ ||>=|| _                              = undefined

(||<||) :: Value -> Value -> Value
(ValueInt x) ||<|| (ValueInt y)         = ValueBool (x < y)
(ValueFloat x) ||<|| (ValueFloat y)     = ValueBool (x < y)
_ ||<|| _                               = undefined

(||<=||) :: Value -> Value -> Value
(ValueInt x) ||<=|| (ValueInt y)        = ValueBool (x <= y)
(ValueFloat x) ||<=|| (ValueFloat y)    = ValueBool (x <= y)
_ ||<=|| _                              = undefined



to_type :: Value -> IR_Type
to_type ValueUnknown        = TypeVoid
to_type (ValueInt _)        = TypeInt
to_type (ValueBool _)       = TypeBool
to_type (ValueFloat _)      = TypeFloat
to_type (ValueString _)     = TypeString
to_type _ = undefined


-- Evaluates the type of an expression.
constant_eval :: IR_Expression -> Maybe Value
constant_eval ExpNothing            = Nothing
constant_eval (ExpNegative e)       = (||!||) <$> constant_eval e
constant_eval (ExpLitInteger x)     = Just $ ValueInt x
constant_eval (ExpLitString x)      = Just $ ValueString x
constant_eval (ExpSum e1 e2)        = (||+||) <$> constant_eval e1 <*> constant_eval e2
constant_eval _                     = Nothing
