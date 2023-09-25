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
module Lexer where

import Control.Monad
import Data.Set qualified as Set
-- import Debug.Trace (trace)

-- ---------------------------------------------------------------------

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

data PpState = PpState
    { defines :: !(Set.Set String)
    , pushed_back :: !(Maybe Token)
    , context :: ![PpContext]
    , accepting :: !Bool
    }
    deriving (Show)

data PpContext = PpContextIf [Token]
    deriving (Show)

data Token
    = ITa
    | ITb
    | ITtrue
    | ITfalse
    | -- directives
      ITppIf
    | ITppElse
    | ITppEnd
    | ITppDefine String
    | ITppUndef String
    | -- conditionals
      ITppIfdef String
    | ITppIfndef String
    | ITppDefined String
    | -- Transfer to upper parser
      ITppIgnored [Token]
    | ITeof
    deriving (Show, Eq)

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
