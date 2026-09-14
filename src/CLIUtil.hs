module CLIUtil ( checkFlags, checkOpts, parseArgs, getOpt ) where

import Data.Char ( chr, ord )

possibleFlags :: [String]
possibleLFlags :: [String]
possibleOpts :: [String]
possibleLOpts :: [String]

-------- Configuration --------

-- Define short flags
possibleFlags = ["h"]
-- Define long flags
possibleLFlags = ["help", "version"]

-- Define short options
possibleOpts = ["p"]
-- Define long options
possibleLOpts = ["port"]

------- Generic Helpers -------

-- Removes all dashes from the beginning of a string (strips flags and longflags alike)
removeDashes :: String -> String
removeDashes ('-':cs)   = removeDashes cs
removeDashes str        = str

-- Reverses each list within a 4tuple
reverse4 :: ([a], [b], [c], [d]) -> ([a], [b], [c], [d])
reverse4 (l1, l2, l3, l4) = (reverse l1, reverse l2, reverse l3, reverse l4)

-- Converts a char to lowercase
toLower :: Char -> Char
toLower c
    | ord c >= ord 'A' && ord c <= ord 'Z' = chr $ (ord c) - ( (ord 'A') - (ord 'a') )
    | otherwise = c

-- Converts a string to lowercase
strToLower :: String -> String
strToLower = map toLower

-- Splits a string in half at the first occurence of the character
splitAtFirst :: Char -> String -> (String, String)
splitAtFirst delim str = helper delim str []
    where
        helper d s newstr = case s of
            (c:cs)
                | c == d    -> ((reverse newstr), cs)
                | otherwise -> helper d cs (c:newstr)
            [] -> ((reverse newstr), [])

----------- Helpers -----------

-- Returns true if the string is a flag
isFlag :: String -> Bool
isFlag "--"     = False
isFlag ('-':_)  = True
isFlag _        = False

-- Returns true if the string is a longflag that takes an argument
isOpt :: String -> Bool
isOpt ""   = False
isOpt str  =  str' `elem` possibleOpts || str' `elem` possibleLOpts
    where
        str' = removeDashes str


-- Expands flags like -abc into -a -b -c, as well as --opt=val into --opt val
-- Also converts long flags and long opts into lowercase
expandArgs :: [String] -> [String]
expandArgs [] = []
expandArgs (arg:args) = case arg of
        -- Stop expanding if you hit a double dash
        "--" -> (arg:args)
        -- Split options with equals signs
        ('-':'-':longflag) -> case (splitAtFirst '=' (strToLower longflag)) of
            (opt, [])       -> (('-':'-':opt):(expandArgs args))
            (opt, optarg)   -> (('-':'-':opt):optarg:(expandArgs args))
        -- Decompose flags
        ('-':flags) -> (map (\c -> ['-', c]) flags) ++ expandArgs args
        -- Expand nothing
        a -> a:(expandArgs args)
    

---------- Exported -----------

-- Returns false if there is an invalid flag
checkFlags :: [String] -> Bool
checkFlags [] = True
checkFlags (s:ss) = (s `elem` possibleFlags || s `elem` possibleLFlags) && checkFlags ss

-- Returns false if there is an invalid opt, or if the length of opts != length of optargs
checkOpts :: [String] -> [String] -> Bool
checkOpts opts optargs = (length opts == length optargs) && helper opts
    where
        -- Checks for validity
        helper []       = True
        helper (o:os)   = (o `elem` possibleOpts || o `elem` possibleLOpts) && helper os
        

-- Parse a list of strings into a 4tuple of strings containing:
--  List of arguments
--  List of flags
--  List of opts
--  List of optargs
parseArgs :: [String] -> ([String], [String], [String], [String])
parseArgs x = reverse4 (parseRawArgs x) where
    -- Lists will be reversed!
    parseRawArgs strs = helper (expandArgs strs) [] [] [] [] True
        where
            helper :: [String] -> [String] -> [String] -> [String] -> [String] -> Bool -> ([String], [String], [String], [String])
            helper args argv flags opts optargs doOpts = case args of
                (s1:s2:ss)
                    -- double dash signifies we should stop processing subsequent flags and treat them as normal strings
                    | doOpts && s1 == "--"  -> helper (s2:ss)   argv        flags                       opts                        optargs         False
                    -- Process opt (we do this first because opts and flags are the same to isFlag )
                    | doOpts && isOpt s1    -> helper ss        argv        flags                       ((removeDashes s1):opts)    (s2:optargs)    doOpts
                    -- Process flag
                    | doOpts && isFlag s1   -> helper (s2:ss)   argv        ((removeDashes s1):flags)   opts                        optargs         doOpts
                    -- Process normal argument
                    | otherwise             -> helper (s2:ss)   (s1:argv)   flags                       opts                        optargs         doOpts
                (s:[])
                    -- Note that opts in the last position will cause there to be an insufficient number of args in the arg list
                    -- Process flag
                    | doOpts && isFlag s    -> (argv,      ((removeDashes s):flags),    opts, optargs)
                    -- Process arg
                    | otherwise             -> ((s:argv),  flags,                       opts, optargs)
                [] -> (argv, flags, opts, optargs)

-- Get the argument from the desired option(s), with a fallback value
getOpt :: [String] -> String -> [String] -> [String] -> String
getOpt _ fallback [] _ = fallback
getOpt _ fallback _ [] = fallback
getOpt desiredOpts fallback (o:os) (oa:oas)
    | o `elem` desiredOpts  = oa
    | otherwise             = getOpt desiredOpts fallback os oas
