
module Main where

import Lexer
import Parser
import PreProcess

-- ---------------------------------------------------------------------
main :: IO ()
main = doParse

-- ---------------------------------------------------------------------
-- Call the emulated parser (emulating GHC side)

doParse :: IO ()
doParse =
    case unP parseModuleNoHaddock initParserState of
        PFailed pst -> putStrLn $ "PFailed: " ++ show pst
        POk pst out -> putStrLn $ "POk: " ++ show (pst, out)

-- | Creates a parse state
initParserState :: PState
initParserState =
    PState
        { errors = []
        , feed =
            [ ITa
            , ITppDefine "FOO"
            , ITppIfdef "FOO"
            , ITtrue
            , ITa
            , ITppElse
            , ITb
            , ITppEnd
            , ITb
            , ITeof
            ]
        , output = []
        , pp = initPpState
        }

-- | Creates a parse state
initParserStateFeed :: [Token] -> PState
initParserStateFeed feed =
    PState
        { errors = []
        , feed = feed
        , output = []
        , pp = initPpState
        }

-- =====================================================================
-- =====================================================================
-- =====================================================================
-- =====================================================================
-- ---------------------------------------------------------------------

-- Testing

testWithFeed :: [Token] -> IO [Token]
testWithFeed feed =
    case unP parseModuleNoHaddock (initParserStateFeed feed) of
        PFailed pst -> error $ "PFailed: " ++ show pst
        -- POk pst out -> putStrLn $ "POk: " ++ show (pst, out)
        -- POk pst _out -> return (reverse $ output pst)
        POk _pst out -> return (reverse out)

t1 :: IO ()
t1 = do
    res <-
        testWithFeed
            [ ITa
            , ITppDefine "FOO"
            , ITppIfdef "FOO"
            , ITa
            , ITppElse
            , ITb
            , ITppEnd
            , ITb
            , ITeof
            ]
    putStrLn $ "res=" ++ show res
    let expected =
            [ ITa
            , ITppIgnored [ITppDefine "FOO"]
            , ITppIgnored [ITppIfdef "FOO"]
            , ITa
            , ITppIgnored [ITppElse]
            , ITppIgnored [ITb]
            , ITppIgnored [ITppEnd]
            , ITb
            ]
    if res == expected
        then return ()
        else error $ "mismatch: expected\n " ++ show expected ++ "\ngot\n " ++ show res

t2 :: IO ()
t2 = do
    res <-
        testWithFeed
            [ ITa
            , ITppDefine "FOO"
            , ITppIfdef "BAR"
            , ITa
            , ITppElse
            , ITb
            , ITppEnd
            , ITb
            , ITeof
            ]
    putStrLn $ "res=" ++ show res
    let expected =
            [ ITa
            , ITppIgnored [ITppDefine "FOO"]
            , ITppIgnored [ITppIfdef "BAR"]
            , ITppIgnored [ITa]
            , ITppIgnored [ITppElse]
            , ITb
            , ITppIgnored [ITppEnd]
            , ITb
            ]

    if res == expected
        then return ()
        else error $ "mismatch: expected\n " ++ show expected ++ "\ngot\n " ++ show res
