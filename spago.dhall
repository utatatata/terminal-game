{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "ansi"
  , "arrays"
  , "bifunctors"
  , "control"
  , "datetime"
  , "effect"
  , "foldable-traversable"
  , "free"
  , "integers"
  , "js-date"
  , "js-timers"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-process"
  , "node-streams"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "psci-support"
  , "random"
  , "refs"
  , "spec"
  , "st"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
