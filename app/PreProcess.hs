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
module PreProcess where

import Data.Maybe
import Data.Set qualified as Set
import Debug.Trace (trace)

import Lexer

-- ---------------------------------------------------------------------

-- Preprocessor layer.
-- Implement as a monad transformer stack, adding state.
-- Or there is already ParserState, maybe just leverage that.

-- Requirements
-- - Consume the tokens from `parseModuleNoHaddock`
-- - Process them for preprocessor directives, swallowing them until done
-- - Emit the stream as normal tokens, with unused and other PP
--   directives wraped in ITppIgnored



initPpState :: PpState
initPpState = PpState{defines = Set.empty, pushed_back = Nothing, context = [], accepting = True}

ppLexer, ppLexerDbg :: Bool -> (Token -> P a) -> P a
-- Use this instead of 'lexer' in GHC.Parser to dump the tokens for debugging.
ppLexerDbg queueComments cont = ppLexer queueComments contDbg
  where
    contDbg tok = trace ("ptoken: " ++ show tok) (cont tok)
ppLexer _queueComments cont = do
    tok <- ppLexToken
    tok' <- case tok of
        ITppIf -> preprocessIf
        ITppDefine _ -> preprocessDefine tok
        ITppIfdef _ -> preprocessIfDef tok
        ITppElse -> preprocessElse
        ITppEnd -> preprocessEnd
        _ -> do
            accepting <- getAccepting
            if accepting
                then return tok
                else return (ITppIgnored [tok])
    cont tok'

-- Swallow tokens until ITppEnd
preprocessIf :: P Token
preprocessIf = go [ITppIf]
  where
    go acc = do
        tok <- ppLexToken
        case tok of
            ITppEnd -> return $ ITppIgnored (reverse $ ITppEnd : acc)
            _ -> go (tok : acc)

preprocessDefine :: Token -> P Token
preprocessDefine tok@(ITppDefine def) = do
    ppDefine def
    return (ITppIgnored [tok])
preprocessDefine tok = return tok

preprocessIfDef :: Token -> P Token
preprocessIfDef tok@(ITppIfdef def) = do
    defined <- ppIsDefined def
    if defined
        then do
            pushContext (PpContextIf [tok])
            setAccepting True
        else setAccepting False
    return (ITppIgnored [tok])
preprocessIfDef tok = return tok

preprocessElse :: P Token
preprocessElse = do
    accepting <- getAccepting
    setAccepting (not accepting)
    return (ITppIgnored [ITppElse])

preprocessEnd :: P Token
preprocessEnd = do
    -- TODO: nested context
    setAccepting True
    return (ITppIgnored [ITppEnd])

-- ---------------------------------------------------------------------
-- Preprocessor state functions

-- context stack start -----------------

pushContext :: PpContext -> P ()
pushContext new =
    P $ \s -> POk s{pp = (pp s){context = new : context (pp s)}} ()

setAccepting :: Bool -> P ()
setAccepting on =
    P $ \s -> POk s{pp = (pp s){accepting = on}} ()

getAccepting :: P Bool
getAccepting = P $ \s -> POk s (accepting (pp s))

-- context stack end -------------------

-- pushed_back token start --------------

pushBack :: Token -> P ()
pushBack tok = P $ \s ->
    if isJust (pushed_back (pp s))
        then
            PFailed
                $ s
                    { errors =
                        ("pushBack: " ++ show tok ++ ", we already have a token:" ++ show (pushed_back (pp s)))
                            : errors s
                    }
        else
            let
                ppVal = pp s
                pp' = ppVal{pushed_back = Just tok}
                s' = s{pp = pp'}
             in
                POk s' ()

-- | Destructive read of the pushed back token (if any)
getPushBack :: P (Maybe Token)
getPushBack = P $ \s ->
    POk s{pp = (pp s){pushed_back = Nothing}} (pushed_back (pp s))

-- | Get next token, which may be the pushed back one
ppLexToken :: P Token
ppLexToken = do
    mtok <- getPushBack
    maybe lexToken return mtok

-- pushed_back token end ----------------

-- definitions start --------------------

ppDefine :: String -> P ()
ppDefine def = P $ \s ->
    POk s{pp = (pp s){defines = Set.insert def (defines (pp s))}} ()

ppIsDefined :: String -> P Bool
ppIsDefined def = P $ \s ->
    POk s (Set.member def (defines (pp s)))

-- definitions end ----------------------


