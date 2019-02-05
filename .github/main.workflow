workflow "Comment Hooks" {
  on = "issue_comment"
  resolves = ["bump-submodules"]
}

action "bump-submodules" {
  uses = "domdere/git-submodule-action@master"
  secrets = ["GITHUB_TOKEN"]
}
