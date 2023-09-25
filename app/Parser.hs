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
module Parser where

import Debug.Trace (trace)

import Lexer
import PreProcess

-- ---------------------------------------------------------------------

-- From Parser.(y|hs)
-- Emulate the parser

parseModuleNoHaddock :: P [Token]
parseModuleNoHaddock = happySomeParser
  where
    -- happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (let {(HappyWrap35 x') = happyOut35 x} in x'))
    happySomeParser :: P [Token]
    happySomeParser = (>>=) (happyParse 0) (\x -> return x)

happyParse :: Int -> P [Token]
happyParse start_state = happyNewToken start_state [] []

happyNewToken :: Int -> [Int] -> [Token] -> P [Token]
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
                    _ -> cont 5
                    -- _ -> happyError' (tk, [])
        )

happyDoAction :: Int -> Token -> Int -> [Int] -> [Token] -> P [Token]
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

happyAccept :: Int -> Token -> Int -> [Int] -> [Token] -> P [Token]
happyAccept _j tk _st _sts stk =
    trace ("happyAccept:" ++ show tk)
        $ return stk

happyShift :: Int -> Int -> Token -> Int -> [Int] -> [Token] -> P [Token]
happyShift new_state _i tk st sts stk = do
    -- stash tk
    happyNewToken new_state (st : sts) (tk : stk)

-- happyShift new_state i tk st sts stk =
--      happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- stash :: Token -> P ()
-- stash tk = P $ \s -> POk (s{output = tk : output s}) ()

happyFail :: [String] -> Int -> Token -> p2 -> p3 -> p4 -> P a
happyFail explist i tk _old_st _ _stk =
    trace ("failing" ++ show explist)
        $ happyError_ explist i tk

happyError_ :: [String] -> p -> Token -> P a
happyError_ explist _ tk = happyError' (tk, explist)

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

happyError' :: (Token, [String]) -> P a
happyError' tk = (\(_tokens, _explist) -> happyError) tk

happyError :: P a
happyError = srcParseFail
