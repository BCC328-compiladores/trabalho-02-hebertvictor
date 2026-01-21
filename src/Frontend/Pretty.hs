{-  ----------------------------------
    @file       src/Frontend/Pretty.hs
    @details    Pretty prints the IR...
-}

{-# LANGUAGE InstanceSigs #-} -- for which doesn't allow by default...
{-# LANGUAGE TypeSynonymInstances #-} -- for instance of String...
{-# LANGUAGE FlexibleInstances #-} -- ~
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Frontend.Pretty (Pretty(..), 
                        PrettyContext,
                        pc_get_string,
                        pc_tell, 
                        pc_list_wsep,
                        pc_increment_identation,
                        pc_decrement_identation,
                        pc_newline,
                        pretty_sl) where

import Data.Map (Map)
import qualified Data.Map as Map



-- Pretty class: represents data that can be "pretty" printed.
-- Defines the `pretty` operation that, given the data, interprets it on the pretty context.
class Pretty t where
    pretty :: t -> PrettyContext ()


-----------------------
-- Pretty Context Monad.
data PrettyState = PrettyState {    
    identation_level :: Int
}


newtype PrettyContext t = PC {
    -- Context state function.
    pc_transition :: PrettyState -> (PrettyState, String, t)
}


pc_get_string :: PrettyContext () -> String
pc_get_string pc = let (_, out, _) = pc_transition pc (PrettyState 0) in out


pc_tell :: String -> PrettyContext ()
pc_tell str = PC $ \state -> (state, str, ())


instance Functor PrettyContext where
    fmap f (PC transition) = PC $ \state ->        
        let (state', s, x) = transition state
        in  (state', s, f x)


instance Applicative PrettyContext where
    pure :: a -> PrettyContext a
    pure x = PC $ \state -> (state, "", x)
    
    PC f_transition <*> PC x_transition = 
        PC $ \state ->
            let (state1, s1, f) = f_transition state
                (state2, s2, x) = x_transition state1
            in (state2, s1 ++ s2, f x)


instance Monad PrettyContext where
    PC transition >>= k =
        PC $ \state -> 
            let (state1, s1, x) = transition state
                PC transition2  = k x
                (state2, s2, y) = transition2 state1
            in (state2, s1 ++ s2, y)


pc_increment_identation :: PrettyContext ()
pc_increment_identation = PC $ \state ->
    let state' = state { identation_level = identation_level state + 1 } 
    in  (state', "", ())


pc_decrement_identation :: PrettyContext ()
pc_decrement_identation = PC $ \state -> 
    let state' = state { identation_level = identation_level state - 1 }
    in (state', "", ())


pc_newline :: PrettyContext ()
pc_newline = PC $ \state ->
    let idl = identation_level state
        tabbing = "\n" ++ (replicate idl '\t')
    in (state, tabbing, ())


{- melhor não...
instance Pretty a => Pretty [a] where
    pretty []       = pure ()
    pretty [x]      = pretty x
    pretty (x:xs)   = do
        pretty x
        pc_tell ", "
        pretty xs
-}      

pc_list_wsep :: Pretty t => PrettyContext a -> [t] -> PrettyContext ()
pc_list_wsep _ []               = pure ()
pc_list_wsep _ [x]              = pretty x
pc_list_wsep separator (x:xs)   = do
    pretty x
    _ <- separator
    pc_list_wsep separator xs


pretty_sl :: Pretty t => t -> String
pretty_sl str = (pc_get_string . pretty) str



-- ======================= --
-- Default types instances --
-- ======================= --

instance Pretty a => Pretty [a] where
    pretty xs = do
        pc_tell "["
        pc_list_wsep (pc_tell ", ") xs
        pc_tell "]"

instance (Pretty a, Pretty b) => Pretty (Either a b) where
    pretty :: Either a b -> PrettyContext ()
    pretty (Left x) = do
        pc_tell "LEFT:"
        pc_increment_identation
        pc_newline
        pretty x
        pc_decrement_identation

    pretty (Right y) = do
        pc_tell "RIGHT:"
        pc_increment_identation
        pc_newline
        pretty y
        pc_decrement_identation

instance (Pretty a, Pretty b) => Pretty (a, b) where
    pretty :: (a, b) -> PrettyContext ()
    pretty (x, y) = do
        pc_tell "("
        pretty x
        pc_tell ", "
        pretty y
        pc_tell ")"




instance {-# OVERLAPPING #-} Pretty String where
    pretty :: String -> PrettyContext ()
    pretty s = pc_tell s


instance (Pretty a, Pretty b) => Pretty (Map a b) where
    pretty :: Map a b -> PrettyContext ()
    pretty mm = do
        pc_tell "{"
        pc_increment_identation
        pc_newline
        __pretty_key_value (Map.toList mm)
        pc_decrement_identation
        pc_newline
        pc_tell "}"

        where
            __pretty_key_value [] = return $ ()
            __pretty_key_value ((key, value):xs) = do
                pretty key
                pc_tell " -> "
                pretty value
                pc_newline
                __pretty_key_value xs



    


