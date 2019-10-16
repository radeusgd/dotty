# Dotty Entropy
This is a mod of the [Dotty Compiler](https://github.com/lampepfl/dotty). It is intended for the developers to better manage the entropy – issues – generated in process of the development.

It currently supports the following capabilities:

- Better readable exception stack traces
- Issue minimization process logging – automatically save each step of issue minimization to a folder. See `-YlogToFile` in `ScalaSettings.scala`.

## Workflow
This branch must always be rebased on `lampepfl/dotty:master`. Whenever you have an issue you want to tackle with this mod:

1. Rebase the fix branch on this mod's branch
2. After you've done working with the mod, interactively rebase your fix back to `lampepfl/dotty:master` and drop all the mod's commits
