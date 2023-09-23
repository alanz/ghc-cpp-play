{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Main where

import Control.Monad
import Debug.Trace (trace)
import GHC.Generics (prec)

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

-- | Creates a parse state from a 'ParserOpts' value
initParserState :: PState
initParserState =
    PState
        { errors = []
        , feed = [ITa, ITppIf, ITa, ITppEnd, ITb, ITeof]
        , output = []
        , pp = initPpState
        }

-- =====================================================================
-- Preprocessor layer.
-- Implement as a monad transformer stack, adding state.
-- Or there is already ParserState, maybe just leverage that.

-- Requirements
-- - Consume the tokens from `parseModuleNoHaddock`
-- - Process them for preprocessor directives, swallowing them until done
-- - Emit the stream as normal tokens, with unused and other PP
--   directives wraped in ITppIgnored

data PpState = PpState
    { defines :: ![String]
    }
    deriving (Show)

initPpState :: PpState
initPpState = PpState{defines = []}

ppLexer, ppLexerDbg :: Bool -> (Token -> P a) -> P a
-- Use this instead of 'lexer' in GHC.Parser to dump the tokens for debugging.
ppLexerDbg queueComments cont = ppLexer queueComments contDbg
  where
    contDbg tok = trace ("ptoken: " ++ show tok) (cont tok)

ppLexer _queueComments cont = do
    tok <- lexToken
    tok' <- case tok of
        ITppIf -> preprocessIf
        _ -> return tok
    cont tok'

-- Swallow tokens until ITppEnd
preprocessIf :: P Token
preprocessIf = go [ITppIf]
  where
    go acc = do
      tok <- lexToken
      case tok of
        ITppEnd -> return $ ITppIgnored (reverse $ ITppEnd:acc)
        _ -> go (tok:acc)

-- =====================================================================
-- ---------------------------------------------------------------------
-- =====================================================================
-- Lexer.x

-- | The parsing monad, isomorphic to @StateT PState Maybe@.
newtype P a = P {unP :: PState -> ParseResult a}

instance Functor P where
    fmap = liftM

instance Applicative P where
    pure = returnP
    (<*>) = ap

instance Monad P where
    (>>=) = thenP

returnP :: a -> P a
returnP a = a `seq` P (\s -> POk s a)

thenP :: P a -> (a -> P b) -> P b
(P m) `thenP` k = P $ \s ->
    case m s of
        POk s1 a -> unP (k a) s1
        PFailed s1 -> PFailed s1

-- ---------------------------------------------------------------------

-- | The result of running a parser.
newtype ParseResult a = PR (# (# PState, a #) | PState #)

-- The carried parsing state can be used to resume parsing.
pattern POk :: PState -> a -> ParseResult a
pattern POk s a = PR (# (# s, a #) | #)

-- The carried parsing state can be used to resume parsing. It is the state
-- right before failure, including the fatal parse error. 'getPsMessages' and
-- 'getPsErrorMessages' must return a non-empty bag of errors.
pattern PFailed :: PState -> ParseResult a
pattern PFailed s = PR (# | s #)

{-# COMPLETE POk, PFailed #-}

-- ---------------------------------------------------------------------

data PState = PState
    { feed :: ![Token]
    , errors :: ![String]
    , output :: ![Token]
    , pp :: !PpState
    }
    deriving (Show)

data Token
    = ITa
    | ITb
    | ITppIf
    | ITppEnd
    | ITppIgnored [Token]
    | ITeof
    deriving (Show)

lexer :: Bool -> (Token -> P a) -> P a
lexer _queueComments cont = do
    tok <- lexToken
    cont tok

lexToken :: P Token
lexToken =
    P $ \s ->
        let
            (tok, feed') = case feed s of
                (t : ts) -> (t, ts)
                [] -> (ITeof, [])
            s' = s{feed = feed'}
         in
            POk s' tok

-- Report a parse failure, giving the span of the previous token as
-- the location of the error.  This is the entry point for errors
-- detected during parsing.
srcParseFail :: P a
srcParseFail = P $ \s@PState{} ->
    unP (addFatalError "failed") s

addFatalError :: String -> P b
addFatalError err =
    addError err >> P PFailed

addError :: String -> P ()
addError err =
    P $ \s -> POk s{errors = err : errors s} ()

-- End Lexer.(x|hs)
-- =====================================================================
-- ---------------------------------------------------------------------
-- =====================================================================
-- From Parser.(y|hs)
-- Emulate the parser

parseModuleNoHaddock :: P Token
parseModuleNoHaddock = happySomeParser
  where
    -- happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (let {(HappyWrap35 x') = happyOut35 x} in x'))
    happySomeParser = (>>=) (happyParse 0) (\x -> return x)

happyParse :: Int -> P Token
happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

happyNewToken :: p1 -> p2 -> p3 -> P Token
happyNewToken action sts stk =
    -- lexer
    ppLexerDbg
        True
        ( \tk ->
            let cont i =
                    trace ("happyNewToken:tk=" ++ show tk)
                        $ happyDoAction i tk action sts stk
             in case tk of
                    ITeof -> happyDoAction 169 tk action sts stk
                    ITa -> cont 1
                    ITb -> cont 2
                    ITppIf -> cont 3
                    ITppEnd -> cont 4
                    ITppIgnored _ -> cont 5
                    -- _ -> happyError' (tk, [])
        )

happyDoAction :: Int -> Token -> p2 -> p3 -> p4 -> P Token
-- happyDoAction num tk action sts stk = P $ \s -> POk s tk
happyDoAction num tk action sts stk =
    case num of
        1 -> happyShift 2 num tk action sts stk
        2 -> happyShift 5 num tk action sts stk
        3 -> happyShift 5 num tk action sts stk
        4 -> happyShift 5 num tk action sts stk
        5 -> happyShift 5 num tk action sts stk
        50 -> happyAccept num tk action sts stk
        169 -> happyAccept num tk action sts stk
        i -> happyFail ["failing:" ++ show i] i tk action sts stk

-- happyAccept j tk st sts (HappyStk ans _) =
--         (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

happyAccept :: Int -> Token -> p2 -> p3 -> p4 -> P Token
happyAccept j tk st sts _ =
    trace ("happyAccept:" ++ show tk)
        $ return tk

-- happyReturn1 :: a -> P a
-- happyReturn1 = return

happyShift :: Int -> Int -> Token -> p2 -> p3 -> p4 -> P Token
happyShift new_state i tk st sts stk = do
    stash tk
    happyNewToken new_state sts stk

stash :: Token -> P ()
stash tk = P $ \s -> POk (s{output = tk : output s}) ()

happyFail :: [String] -> Int -> Token -> p2 -> p3 -> p4 -> P a
happyFail explist i tk old_st _ stk =
    trace ("failing" ++ show explist)
        $ happyError_ explist i tk

happyError_ :: [String] -> p -> Token -> P a
happyError_ explist _ tk = happyError' (tk, explist)

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

happyError' :: (Token, [String]) -> P a
happyError' tk = (\(tokens, explist) -> happyError) tk

happyError :: P a
happyError = srcParseFail

-- =====================================================================
-- ---------------------------------------------------------------------
