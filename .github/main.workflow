workflow "Haskell Lint check" {
  on = "push"
  resolves = ["domdere/haskell-lint-action"]
}

action "domdere/haskell-lint-action" {
  uses = "domdere/haskell-lint-action@41d79b3"
}
