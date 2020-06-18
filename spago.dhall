{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen-hooks-trial"
, dependencies =
  [ "console", "effect", "halogen-hooks", "psci-support", "read", "tuples", "halogen-hooks" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
