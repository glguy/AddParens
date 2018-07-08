{-|
Module      : Main
Description : Program for adding explicit layout and parentheses to an
              expression.
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com
-}

-- syb
import Data.Generics (Data, everywhere, extT, mkT)
-- haskell-src-exts
import Language.Haskell.Exts (Exp(..), Type(..), Mode(OneLineMode), PPLayout(PPNoLayout),
                              Style(mode), PPHsMode(layout), ParseResult(..), Module,
                              Pretty, SrcLoc(..), SrcSpanInfo(..), Parseable(parse),
                              prettyPrintStyleMode, defaultMode, style)
-- base
import System.Environment (getArgs)
import Data.Proxy (Proxy(Proxy))
import Text.Printf (printf)

------------------------------------------------------------------------
-- Rewrite rule for adding parentheses around all applications ---------
------------------------------------------------------------------------

-- | Ensure that infix and prefix applications are surrounded by parentheses
-- and remove redundant parentheses. This rule only operates on the top-level
-- of a particular expression.
addParensExp1 :: Exp () -> Exp ()
addParensExp1 x@InfixApp{}        = Paren () x -- add parentheses to infix application
addParensExp1 x@App{}             = Paren () x -- add parentheses to prefix application
addParensExp1 (Paren _ x@Paren{}) = x          -- remove nested parentheses
addParensExp1 x                   = x          -- otherwise no transformation

-- | Apply same logic from 'addParensExp1' to types found in the expression.
addParensType1 :: Type () -> Type ()
addParensType1 x@TyInfix{}             = TyParen () x
addParensType1 x@TyFun{}               = TyParen () x
addParensType1 x@TyApp{}               = TyParen () x
addParensType1 (TyParen _ x@TyParen{}) = x
addParensType1 x                       = x

-- | Recursively parenthesize the expressions and types in a given value.
addParens :: Data d => d -> d
addParens = everywhere (mkT addParensExp1 `extT` addParensType1)
            -- bottom up rewrite using syb

------------------------------------------------------------------------
-- Program logic -------------------------------------------------------
------------------------------------------------------------------------

-- | Transform a Haskell expression to have parentheses making precedence
-- more obvious.
explicitParens ::
  Functor f                 => -- functor used to remove source locations
  Parseable (f SrcSpanInfo) => -- parsable with source locations
  Pretty    (f ())          => -- printable without source locations
  Data      (f ())          => -- supports generic traversals
  proxy f {- ^ proxy for specifying syntax elements to process -} ->
  String  {- ^ parser input string                             -} ->
  String  {- ^ rendered, parenthesized output                  -}
explicitParens pxy = render . fmap (addParens . forgetAnn pxy) . parse

-- | Forget the source location annotations. The extra proxy argument
-- is to help type inference along in the implementation of 'explicitParens'.
forgetAnn :: Functor f => proxy f -> f SrcSpanInfo -> f ()
forgetAnn _proxy x = () <$ x

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
-- Either @module@ or @type@ can optionally be specified as a command line argument
-- to pick an alternative parser.
main :: IO ()
main =
  do args <- getArgs
     case args of
       []         -> interact (explicitParens (Proxy :: Proxy Exp   ))
       ["module"] -> interact (explicitParens (Proxy :: Proxy Module))
       ["type"]   -> interact (explicitParens (Proxy :: Proxy Type  ))
       _          -> putStrLn "usage: AddParens [module|type]"
