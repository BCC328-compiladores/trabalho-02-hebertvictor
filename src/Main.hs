{-  -----------------------
    @file       src/Main.hs
    @details    Driver.
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Main where
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.Directory (doesFileExist) -- Need to be added to the project requirements
import System.Process (callCommand)
import Control.Monad (when, unless)

import Frontend.Lexer
import Frontend.Parser
import Frontend.Pretty (pretty_sl)
import Frontend.PrettyTree (pretty_sl_tree)
import Frontend.Semantics (sl_verify)
import Interpreter.Interpreter

data CompilerOptions = Options {
    opt_lexer :: Bool,
    opt_parser :: Bool,
    opt_pretty :: Bool,
    opt_semantics :: Bool,
    opt_interpret :: Bool,
    opt_compile :: Bool
}

parse_options :: [String] -> CompilerOptions
parse_options args = Options {
    opt_lexer  = elem "-l" args || elem "--lexer" args,
    opt_parser = elem "-p" args || elem "--parser" args,
    opt_pretty = elem "-pt" args || elem "--pretty" args,
    opt_semantics = elem "-s" args || elem "--semantics" args,
    opt_interpret = elem "-i" args || elem "--interpret" args,
    opt_compile = elem "-c" args || elem "--compile" args
}


show_usage :: IO ()
show_usage = do
    program_name <- getProgName
    putStrLn ("Use: " ++ program_name ++ " <input_file> [options]\n")
    putStrLn "Options: "
    putStrLn "\t-l,  --lexer     - Lexical analysis and show tokes."
    putStrLn "\t-p,  --parser    - Syntax analysis and show IR."
    putStrLn "\t-pt, --pretty    - Syntax analysis and show textual version of the IR."
    putStrLn "\t-s,  --semantics - ..."
    putStrLn "\t-i,  --interpret - Runs the intepreter."
    putStrLn "\t-c,  --compile - ..."


to_parse_options :: CompilerOptions -> Bool
to_parse_options options = 
    opt_parser options || opt_pretty options || opt_interpret options || opt_semantics options || opt_compile options

to_verify_options :: CompilerOptions -> Bool
to_verify_options options =
    opt_semantics options || opt_compile options


change_extention :: String -> String -> String
change_extention filename new_extention =
    case break (== '.') (reverse filename) of
        (_, "") -> filename ++ "." ++ new_extention
        (_, r) -> reverse (drop 1 r) ++ "." ++ new_extention


main :: IO ()
main = do
    -- getting command line arguments.
    args <- getArgs

    case args of
        [] -> do
            putStrLn "[Error] Expected at least 1 argument."
            show_usage
            exitFailure

        (filepath : _) -> do
            file_exists <- doesFileExist filepath
            unless file_exists $ do
                putStrLn ("Error: File " ++ filepath ++ " dosen't Exists.")
                exitFailure

    let filepath = head args
    file_content <- readFile filepath

    let options = parse_options $ tail args

    -- Option Run Lexer
    when (opt_lexer options) $ do
        let tokens = lexer file_content
        case tokens of
            Left error_str ->
                putStrLn $ "Lexer error: " ++ pretty_sl error_str

            Right tk_list -> do
                putStrLn "Tokens:"
                mapM_ print tk_list -- Print's list elements one per line

    -- Executing the parser.
    let parsed = if to_parse_options options
                 then parse_sl file_content
                 else Right undefined


    case parsed of
        Left error_str -> putStrLn $ "Parser Error: " ++ pretty_sl error_str

        Right ir_program -> do
            -- Option Run Parser (-p)
            when (opt_parser options) $ putStrLn $ "Program IR:" ++ pretty_sl_tree ir_program

            -- Option Run Pretty (-pt)
            when (opt_pretty options) $ putStrLn $ "Program:" ++ pretty_sl ir_program                    

            -- Option Run Interpreter (-i)
            when (opt_interpret options) $ do
                result <- interpret ir_program
                case result of
                    Left err -> putStrLn $ pretty_sl err
                    Right (rv, state) -> putStrLn $ "RV: " ++ show rv ++ " Log: " ++ show (is_log state) ++ "."


            let verified_program =  if to_verify_options options
                                    then sl_verify ir_program
                                    else Right undefined

            -- Option Run Semantics (-s)
            when (opt_semantics options) $ do
                case verified_program of
                    Left err -> putStrLn $ pretty_sl err
                    Right (p', st) -> putStrLn $ pretty_sl st
                
            -- Option Compile (-c)
            when (opt_compile options) $ do
                case verified_program of
                    Left err -> putStrLn $ pretty_sl err

                    Right (p', _) -> do
                        -- traslated <- translate_program p'
                        -- pretty_wat <- pretty_sl traslated

                        -- Writing .wat file
                        -- let wat_filepath = change_extention filepath "wat"
                        -- writeFile wat_filepath pretty_wat

                        -- callCommand $ "wat2wasm " ++ wat_filepath

                        callCommand $ "wat2wasm data/sla.wat"

    exitSuccess
