{-|
Module      : Main
Description : Program for adding explicit layout and parentheses to an
              expression.
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com
-}

-- syb
import Data.Generics (Data, everywhere, ext1T)
-- haskell-src-exts
import Language.Haskell.Exts (Exp(..), Type(..), Mode(OneLineMode), PPLayout(PPNoLayout),
                              Style(mode), PPHsMode(layout), ParseResult(..), Module,
                              Pretty, SrcSpanInfo(..), Parseable(parseWithMode),
                              ParseMode(..), glasgowExts, ann,
                              defaultMode, defaultParseMode, prettyPrintStyleMode, style)
-- base
import System.Environment (getArgs)
import Data.Proxy (Proxy(Proxy))

------------------------------------------------------------------------
-- Rewrite rule for adding parentheses around all applications ---------
------------------------------------------------------------------------

-- | Ensure that infix and prefix applications are surrounded by parentheses
-- and remove redundant parentheses. This rule only operates on the top-level
-- of a particular expression.
addParensExp1 :: Exp a -> Exp a
addParensExp1 x@InfixApp{}        = Paren (ann x) x -- add parentheses to infix application
addParensExp1 x@App{}             = Paren (ann x) x -- add parentheses to prefix application
addParensExp1 (Paren _ x@Paren{}) = x               -- remove nested parentheses
addParensExp1 x                   = x               -- otherwise no transformation

-- | Apply same logic from 'addParensExp1' to types found in the expression.
--
-- Fixities aren't correctly supported for type operators, so this code doesn't
-- bother parenthesizing them.
-- <https://github.com/haskell-suite/haskell-src-exts/issues/47>
addParensType1 :: Type a -> Type a
addParensType1 x@TyFun{}               = TyParen (ann x) x
addParensType1 x@TyApp{}               = TyParen (ann x) x
addParensType1 (TyParen _ x@TyParen{}) = x
addParensType1 x                       = x

-- | Recursively parenthesize the expressions and types in a given value.
addParens :: Data d => d -> d
addParens = everywhere (id `ext1T` addParensExp1 `ext1T` addParensType1)
            -- bottom up rewrite using syb

------------------------------------------------------------------------
-- Program logic -------------------------------------------------------
------------------------------------------------------------------------

-- | Transform a Haskell expression to have parentheses making precedence
-- more obvious.
explicitParens ::
  Parseable (f SrcSpanInfo) =>
  Pretty    (f SrcSpanInfo) =>
  Data      (f SrcSpanInfo) =>
  proxy f {- ^ proxy for specifying syntax elements to process -} ->
  String  {- ^ parser input string                             -} ->
  String  {- ^ rendered, parenthesized output                  -}
explicitParens pxy = render . fmap (addParens . fixType pxy) . parse
  where
    -- Link parsed type to the proxy argument and specify SrcSpanInfo
    fixType :: proxy f -> f SrcSpanInfo -> f SrcSpanInfo
    fixType _ x = x

-- | Pretty print a syntax element on a single line using explicit layout.
flatPrettyPrint :: Pretty a => a -> String
flatPrettyPrint =
  prettyPrintStyleMode
    style       { mode   = OneLineMode }
    defaultMode { layout = PPNoLayout  }

-- | Parse using @stdin@ as the filename and with lots of GHC extensions enabled.
parse :: Parseable a => String -> ParseResult a
parse =
  parseWithMode
    defaultParseMode
      { parseFilename = "stdin"
      , extensions = glasgowExts }

-- | Render successful parses and parser error messages
render :: Pretty a => ParseResult a -> String
render (ParseOk x)           = flatPrettyPrint x
render (ParseFailed pos err) = flatPrettyPrint pos ++ " " ++ err

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
