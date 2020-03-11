module QC where

import Parser.Data.ParseTree
import Parser.Generator.Syntax

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Property

import Parser.Generator.Exception

instance Arbitrary ParseTree where
    arbitrary = sized pt

pt 0 = oneof [ return Eps
             , liftM Token ebnfToken ]
pt n = oneof [ liftM2 Rule (return $ "Syntax") (pt' n) ]

pt' 0 = pt 0
pt' n = oneof [ subSeq
              , subRule ]
    where
        subSeq  = liftM2 Seq half half
        subRule = liftM2 Rule ebnfRule half
        half    = pt' (n `div` 2)

ebnfRule = elements [ "SyntaxRule"
                    , "DefinitionsList"
                    , "SingleDefinition"
                    , "SyntacticTerm"
                    , "SyntacticFactor"
                    , "SyntacticPrimary"
                    , "OptionalSequence"
                    , "RepeatedSequence"
                    , "GroupedSequence"
                    , "EmptySequence"
                    , "MetaIdentifier"
                    , "TerminalString"
                    , "OptionalSequence"
                    , "RepeatedSequence"
                    , "GroupedSequence"
                    , "EmptySequence"
                    , "TerminalCharacter"
                    , "Letter"
                    , "DecimalDigit"
                    , "ConcatenateSymbol"
                    , "DefiningSymbol"
                    , "AlternativeSymbol"
                    , "EndCommentSymbol"
                    , "EndGroupSymbol"
                    , "EndOptionSymbol"
                    , "EndRepeatSymbol"
                    , "ExceptSymbol"
                    , "FirstQuoteSymbol"
                    , "RepetitionSymbol"
                    , "SecondQuoteSymbol"
                    , "SpecialSequenceSymbol"
                    , "StartCommentSymbol"
                    , "StartGroupSymbol"
                    , "StartOptionSymbol"
                    , "StartRepeatSymbol"
                    , "TerminatorSymbol"
                    , "OtherCharacter"
                    , "FirstTerminalCharacter"
                    , "SecondTerminalCharacter"
                    , "Integer"
                    , "MetaIdentifierCharacter"
                    , "SpaceCharacter"
                    , "NewLine"
                    , "CarriageReturn"
                    , "HorizontalTabulationCharacter"
                    , "VerticalTabulationCharacter"
                    , "FormFeed"
                    , "Optional"
                    , "Closure"
                    , "Repetition" ]

ebnfToken = listOf ebnfChar
    where
        ebnfChar = elements $  ['a' .. 'z']
                            <> ['A' .. 'Z']
                            <> ['0' .. '9']
                            <> ",=|/!*\")]}:-'*?([{;. +_%@&#$<>^`~\\"

simpleValidTree =
    ( Rule "Syntax"
        ( Seq
            ( Rule "SyntaxRule"
                ( Seq
                    ( Seq
                        ( Seq
                            ( Rule "MetaIdentifier" -- a
                                ( Seq
                                    ( Rule "Letter" ( Token "a" ) )
                                    ( Eps )
                                )
                            )
                            ( Rule "DefiningSymbol" ( Token "=" ) ) -- b
                        )
                        ( Rule "DefinitionsList"
                            ( Seq
                                ( Rule "SingleDefinition"
                                    ( Seq
                                        ( Rule "SyntacticTerm"
                                            ( Seq
                                                ( Rule "SyntacticFactor"
                                                    ( Seq
                                                        ( Eps )
                                                        ( Rule "SyntacticPrimary"
                                                            ( Rule "EmptySequence" ( Eps ) )
                                                        )
                                                    )
                                                )
                                                ( Eps )
                                            )
                                        )
                                        ( Eps )
                                    )
                                )
                                ( Eps )
                            )
                        )
                    )
                    ( Rule "TerminatorSymbol" ( Token ";" ) ) -- d
                )
            )
            ( Rule "Closure" Eps )
        )
    )

prop_failure = monadicIO $ do
    t <- pick (arbitrary :: Gen ParseTree)
    a <- run $ protect show (return $ gSyntax t)
    assert $ read a == GenFailure

prop_failure' = monadicIO $ do
    t <- pick $ elements [ simpleValidTree ]
    a <- run $ protect show (return $ gSyntax t)
    assert $ read a == GenFailure
