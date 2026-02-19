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
import Control.Monad (when, unless)

--import Frontend.Token
import Frontend.Lexer
--import Frontend.IR
import Frontend.Parser
import Frontend.Pretty ( pretty_sl, Pretty (pretty) )
import Frontend.PrettyTree (pretty_sl_tree)
import Frontend.Semantics (sl_verify)
import Interpreter.Interpreter

data CompilerOptions = Options {
    opt_lexer :: Bool,
    opt_parser :: Bool,
    opt_pretty :: Bool,
    opt_semantics :: Bool,
    opt_interpret :: Bool
}

parse_options :: [String] -> CompilerOptions
parse_options args = Options {
    opt_lexer  = elem "-l" args || elem "--lexer" args,
    opt_parser = elem "-p" args || elem "--parser" args,
    opt_pretty = elem "-pt" args || elem "--pretty" args,
    opt_semantics = elem "-s" args || elem "--semantics" args,
    opt_interpret = elem "-i" args || elem "--interpret" args
}


show_usage :: IO ()
show_usage = do
    program_name <- getProgName
    putStrLn ("Use: " ++ program_name ++ " <input_file> [options]\n")
    putStrLn "Options: "
    putStrLn "\t-l,  --lexer     - Lexical analysis and show tokes."
    putStrLn "\t-p,  --parser    - Syntax analysis and show IR."
    putStrLn "\t-pt, --pretty    - Syntax analysis and show textual version of the IR."
    putStrLn "\t-i,  --interpret - Runs the intepreter."


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

    when (opt_lexer options) $ do
        let tokens = lexer file_content
        case tokens of
            Left error_str ->
                putStrLn $ "Lexer error: " ++ pretty_sl error_str

            Right tk_list -> do
                putStrLn "Tokens:"
                mapM_ print tk_list -- Print's list elements one per line


    let parsed = if opt_parser options || opt_pretty options || opt_interpret options || opt_semantics options
                 then parse_sl file_content
                 else Right undefined

    case parsed of
        Left error_str -> putStrLn $ "Parser Error: " ++ pretty_sl error_str

        Right ir_program -> do
            when (opt_parser options) $ do
                putStrLn "Program IR:"
                -- print ir 
                -- putStrLn "\n----------------------------------\n"
                putStrLn $ pretty_sl_tree ir_program

            when (opt_pretty options) $ do
                putStrLn "Program:"
                putStrLn $ pretty_sl ir_program

            when (opt_semantics options) $ do
                let verified = sl_verify ir_program

                case verified of
                    Left s -> putStrLn $ "Error: " ++ pretty_sl s
                    Right (p', _) -> putStrLn $ pretty_sl p'
                        
                
            when (opt_interpret options) $ do
                let verified = sl_verify ir_program

                case verified of 
                    Left s -> putStrLn $ "Error: " ++ pretty_sl s

                    Right (p', _) -> do 
                        result <- interpret p'
                        case result of
                            Left err -> putStrLn $ "Error: " ++ pretty_sl err
                            Right (rv, state) -> putStrLn $ "RV: " ++ show rv ++ " Log: " ++ show (is_log state) ++ "."

    exitSuccess
