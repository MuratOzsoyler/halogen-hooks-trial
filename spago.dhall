{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen-hooks-trial"
, dependencies =
  [ "console", "effect", "formatters", "halogen-hooks", "ordered-collections", "psci-support", "read", "tuples" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
