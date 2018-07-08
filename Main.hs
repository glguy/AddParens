{-|
Module      : Main
Description : Program for adding explicit layout and parentheses to an
              expression.
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com
-}

-- syb
import Data.Generics (everywhere, extT, mkT)
-- haskell-src-exts
import Language.Haskell.Exts (Exp(..), Type(..), Mode(OneLineMode), PPLayout(PPNoLayout),
                              Style(mode), PPHsMode(layout), ParseResult(..),
                              Pretty, SrcLoc(..), defaultMode, paren, parseExp,
                              prettyPrintStyleMode, style)
-- base
import Text.Printf (printf)

------------------------------------------------------------------------
-- Rewrite rule for adding parentheses around all applications ---------
------------------------------------------------------------------------

-- | Ensure that infix and prefix applications are surrounded by parentheses
-- and remove redundant parentheses. This rule only operates on the top-level
-- of a particular expression.
addParensExp1 :: Exp () -> Exp ()
addParensExp1 x@InfixApp{}        = paren x -- add parentheses to infix application
addParensExp1 x@App{}             = paren x -- add parentheses to prefix application
addParensExp1 (Paren _ x@Paren{}) = x       -- remove nested parentheses
addParensExp1 x                   = x       -- otherwise no transformation

-- | Apply same logic from 'addParensExp1' to types found in the expression.
addParensType1 :: Type () -> Type ()
addParensType1 x@TyInfix{}             = TyParen () x
addParensType1 x@TyFun{}               = TyParen () x
addParensType1 x@TyApp{}               = TyParen () x
addParensType1 (TyParen _ x@TyParen{}) = x
addParensType1 x                       = x

-- | Recursively apply 'addParensExp1' throughout a whole expression.
addParensExp :: Exp () -> Exp ()
addParensExp = everywhere (mkT addParensExp1 `extT` addParensType1)
               -- bottom up rewrite using syb

------------------------------------------------------------------------
-- Program logic -------------------------------------------------------
------------------------------------------------------------------------

-- | Transform a Haskell expression to have parentheses making precedence
-- more obvious.
explicitParens :: String -> String
explicitParens str =
  render $
  do e <- parseExp str
     let e' = () <$ e -- forget source position annotations
     return (addParensExp e')

-- | Pretty print a syntax element on a single line using explicit layout.
flatPrettyPrint :: Pretty a => a -> String
flatPrettyPrint =
  prettyPrintStyleMode
    style       { mode   = OneLineMode }
    defaultMode { layout = PPNoLayout  }

-- | Render successful parses and parser error messages
render :: Pretty a => ParseResult a -> String
render (ParseOk x) = flatPrettyPrint x
render (ParseFailed (SrcLoc _ line col) err) = printf "(%d:%d): %s" line col err

-- | Parse an expression from stdin and write the transformed version to stdout.
main :: IO ()
main = interact explicitParens
