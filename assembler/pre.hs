-- Write now this is just a bunch of rambling.  There is some code at the
-- bottom that actually compiles, but it doesn't do much of anything yet.

-- This is a preproccessor that combines all the files into a single,
-- simplified stream to be sent to some other program that wants to
-- manipulate assembler code more easily.

-- This reads an input file, handles #file and comment directives,
-- and ignores any insignificant text.  To simplify processing for the
-- next program in the stream, We also change the syntax from:
--
--     /; *[:word:] *( [:argument:])*[^;]*/
--
-- to:
--
--     /^([:word:]( [:argument:])*)|( *)$/
--
-- For the other program, parsing becomes as easy as:
--
--     parse fileString = map words $ lines fileString
--
-- The result of this transformation is sent to stdout.

-- Example Usage:
--
--     #!/bin/sh
--     out=$1
--     shift
--     for file in $*; do pre $file; done | asm > $out

-- Split the input file into a list of statements.
-- filter out comment statements
-- make a list of #file statements.
-- repeat this process for all file statements, producing a dictionary of
-- dict :: [(filename, [statement])]
--
-- Do this repeatedly until all #file statements have an entry in said
-- dictionary.

-- Produce a final list of statements resulting from substituting #file
-- directives, with their corresponsind statements.  For example:

--  [('file1', [a, b, '#file file2', c, d]),
--   ('file2', [e, f, g])]
--  ... produces ...
--  [a, b, e, f, g, c, d]

-- Finally, print these statements, one per line (with no ';'s).


-- -- The file-dictionary might look like:
-- --
-- --     [("in", [a, "#file yo", b])
-- --     ("foo", [c])]
-- --
-- -- Builds a dictionary of (filename -> statements) for all files
-- -- referenced from the main input file (and all files referenced from
-- -- those, etc)
-- build dictSoFar [] = dictSoFar
-- build dictSoFar (f:fs) = do
    -- input <- readFile f
    -- statements <- getStatements input
    -- build (file, statements):dictSoFar \
          -- (files ++ filter (NotIn dictSoFar) (includes statements))

-- main = do args <- getArgs
          -- dict <- build [] args
          -- put . joinlines $ fs (args !! 0) dict


-- flattenStatements files fileStmts statementsSoFar
-- fs dict stmts = reverse $ fs' dict stmts []
-- fs' dict (stmt:stmts) soFar = if include_statement? stmt
                              -- then fs' dict stmts $ fs' dict (lookup dict $ include_file stmt) Far
                              -- else fs' dict stmts stmt:soFar

-- split_statements string = parse!!!!

-- statements filename = do input <- read filename
                         -- split_statements input

import System.Environment
import Text.ParserCombinators.Parsec
import List

-- nestCommentD = char '{' >>= skipMany (noneOf "#") >>= string "#}" >>= return ()
-- underscoreComment = char '_' >>= junk >>= return ()
-- printD = string "Print " >>= junk >>= return ()
-- directive = do char '#' >>= printD <|> commentD <|> cpuD <|> pcD <|> declarD
-- directive = char ';' >> spaces >> char '#' >> return "#"

join = concat . intersperse " "
junk = many $ noneOf ";"
charTok c = char c >> spaces
possible p = (count 1 p) <|> (count 0 p)

usage = "pre input-file"
directive = charTok ';' >> charTok '#' >> many1 (letter <|> digit)
labell = do charTok ';'
            char '{'
            body <- many1 (noneOf ";}\n ")
            char '}'
            return ("{" ++ body ++ "}")

stmt = do charTok ';'
          str <- many1 upper
          spaces
          arg <- possible $ many1 $ noneOf " ;\n"
          return $ join $ str:arg

parseExprs = do junk
                result <- many (do x <- choice [ try stmt
                                               , try directive
                                               , labell ]
                                   junk
                                   return x)
                eof
                return result

readExprs input = case parse parseExprs "asm" input of
    Left err -> [show err]
    Right val -> val

-- This needs to do parsing, etc to correctly split an input program into a
-- list of statements.
-- split_into_statements i = ["stmt 1"] ++ (lines i) ++ ["stmt n"]
main = do args <- getArgs
          input <- readFile $ args !! 0
          -- putStr $ unlines $ split_into_statements input
          putStr $ unlines $ readExprs input
