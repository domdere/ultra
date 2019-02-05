workflow "Haskell Lint check" {
  on = "push"
  resolves = ["hlint"]
}

action "hlint" {
  uses = "domdere/haskell-lint-action@master"
}
