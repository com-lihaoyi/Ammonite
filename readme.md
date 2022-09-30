# Ammonite [![Build Status][travis-badge]][travis-link] [![Build (Windows)][appveyor-badge]][appveyor-link] [![Gitter Chat][gitter-badge]][gitter-link] [![Patreon][patreon-badge]][patreon-link]

[travis-badge]: https://travis-ci.org/lihaoyi/Ammonite.svg
[travis-link]: https://travis-ci.org/lihaoyi/Ammonite
[appveyor-badge]: https://ci.appveyor.com/api/projects/status/github/lihaoyi/Ammonite
[appveyor-link]: https://ci.appveyor.com/project/lihaoyi/ammonite
[gitter-badge]: https://badges.gitter.im/Join%20Chat.svg
[gitter-link]: https://gitter.im/lihaoyi/Ammonite?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge
[patreon-badge]: https://img.shields.io/badge/patreon-sponsor-ff69b4.svg
[patreon-link]: https://www.patreon.com/lihaoyi

This is where the code for the [Ammonite](https://ammonite.io) 
project lives:

- [Ammonite-REPL](https://ammonite.io), the improved Scala REPL

If you use Ammonite and like it, you will probably enjoy the following book by the Author:

- [*Hands-on Scala Programming*](https://www.handsonscala.com/)

*Hands-on Scala* has uses Ammonite extensively throughout the book, using
Ammonite to accomplish a lot of useful tasks: exploring third party libraries or
writing parallel web-crawlers in the REPL, and implementing entire programming
languages in Ammonite Scala Scripts. *Hands-on Scala* is a great way to level 
up your skills in Scala in general and Ammonite in particular.

If you want to learn more about Ammonite or how to use it, check out the links 
above, or ask on the [Gitter Channel](https://gitter.im/lihaoyi/Ammonite). The 
remainder of this document is developer-docs for people who want to work on the 
Ammonite source code itself.

If you are interested in living more "on the edge", we also publish artifacts 
and the doc-site every commit; the doc-site is available at

- https://ammonite.io/#UnstableVersions

And contains instructions on how to download the per-commit executable or 
depend on the per-commit maven artifact.

If you use Ammonite and like it, please support us by donating to our Patreon:

- [https://www.patreon.com/lihaoyi](https://www.patreon.com/lihaoyi)

# Developer Docs

The layout of the repository is roughly:

- `amm/` is [Ammonite](https://ammonite.io)'s core, REPL and script runner
- `terminal/` is the JLine re-implementation used by Ammonite-REPL to provide 
  syntax highlighting and multiline editing
- `readme/` is the source code for the [Documentation](https://ammonite.io/#Ammonite-Ops), 
  written in [Scalatex](https://www.lihaoyi.com/Scalatex/).
- `published/` is a synthetic project used for publishing, excluding the readme 
  and integration tests

For more detailed information, check out the internals documentation for 
high-level overviews of some interesting facets of the codebase

- [Internals Documentation](https://github.com/com-lihaoyi/Ammonite/tree/master/internals-docs)

## Common Commands

### Manual Testing

Although most features should be unit tested, it's still useful to fire up a REPL from the current codebase to see things work (or not). There are a variety of shells you can spin up for testing different things:

- `mill -i -w terminal[2.12.6].test.run` is useful for manual testing the
  terminal interaction; it basically contains a minimal echo-anything terminal,
  with multiline input based on the count of open- and closed-parentheses. This
  lets you test all terminal interactions without all the complexity of the
  Scala compiler, classloaders, etc. that comes in `repl/`
  
- `mill -i -w amm[2.12.6].run` brings up the Ammonite-REPL using the source code
  in the repository, and automatically restarts it on-exit if you have made a
  change to the code. Useful for manual testing both of `amm/` as well as
  `ops/`, since you can just `import ammonite.ops._` and start using them. Note
  that this does not bring in filesystem utilities like the `wd` variable, `cd!`
  command. You can also pass in the path to a `.sc` file to run it using
  Ammonite's script runner
  
- `mill -i -w integration[2.12.6].test.run` runs the trivial main method in the
  `integration` subproject, letting you manually test running Ammonite
  programmatically, whether through `run` or `debug`

- `mill -i amm[2.12.6].assembly` creates an assembly at
  `out/amm/2.12.6/assembly/dest/out.jar` that you can then use to test: start a
  REPL, run scripts, etc. in a standalone environment without being wrapped in
  Mill build tool

- `mill -i amm[2.12.6].launcher` creates a launcher script at
  `out/amm/2.12.6/launcher/dest/run` that can also be used to run Ammonite
  outside of the Mill build tool.

### Automated Testing

While working on an arbitrary `xyz` subproject, `mill -w xyz.test` runs tests after every change.
`./mill 'amm[2.13.8].test'` can be a bit slow because of the amount of code it compiles, so you may want to specify the test manually via `./mill 'amm[2.13.8].test path.to.test'`.

- `./mill -i 'amm[2.13.8].test'` tests the Ammonite Script-runner, without filesystem-shell integration.
- `./mill -i 'amm.repl[2.13.8].test'` tests the Ammonite-REPL.
- `./mill -i 'terminal[2.13.8].test'` tests the readline re-implementation: keyboard navigation, shortcuts, editing, without any filesystem/scala-repl logic
- `./mill -i 'integration[2.13.8].test'` kicks off the integration tests, which bundle `amm/` and `shell/` into their respective jars and invoke them as subprocesses. Somewhat slow, but exercises all the command-line-parsing stuff that the other unit tests do not exercise, and makes sure that everything works when run from `.jar`s instead of loose class-files
- `./mill -i 'sshd[2.13.8].test'` tests the remote Ammonite-REPL over sshd.

### Publishing

- Publishing is automatic, controlled by scripts in the `ci/` folder.

- Every commit that lands in master will publish a new
  [unstable version](https://ammonite.io/#UnstableVersions),
  that you can already use and download. This includes publishing the unstable version
  to maven central to the
  [snapshot-commit-uploads](https://github.com/com-lihaoyi/Ammonite/releases/tag/snapshot-commit-uploads)
  tag, and updating the documentation-site so it's
  [Unstable Version](https://ammonite.io/#UnstableVersions) download
  instructions to point to it, though the "main" download/install instructions
  in the doc-site will not be changed.

- Every commit that lands in master *with a tag* will re-publish a stable version
  to maven central and upload a new versioned release (using the tag as the
  version) and the doc-site is updated so the main download/install instructions
  point at the new published stable version.

In general, if you land a change in master, once CI completes (1-2hrs) you
should be able to download it via the
[Unstable Version](https://ammonite.io/#UnstableVersions)
instructions and make use of your changes standalone or in an SBT project.

Occasionally, the CI job building and publishing one of the above steps
flakes and fails, and needs to be re-run through the travis web interface.

## Issue Tags

I've started tagging open issues in the issue tracker to try and keep things neat. This is what the various tags mean:

Each issue should only have one of these:

- `bug`: this behavior clearly wrong, and needs to be fixed
- `enhancement`: something relatively speccable, that can be worked on, finished, and will make Ammonite better
- `wishlist`: could be totally awesome, but we're uncertain if it is worth doing at all, what it would look like, or if it will ever reach a "finished" state.

And possibly:

- `help wanted`: I don't have context, hardware, or for some other reason am unlikely to ever do this. But I know people out there care, so one of you should step up and fix it.

## Contribution Guidelines

- **All code PRs should come with**: a meaningful description, inline-comments for important things, unit tests (positive and negative), and a green build in [CI](https://travis-ci.org/lihaoyi/Ammonite)
- **Try to keep lines below 80 characters width**, with a hard limit of 100 characters.
- **PRs for features should generally come with *something* added to the [Documentation](https://ammonite.io)**, so people can discover that it exists
- **Be prepared to discuss/argue-for your changes if you want them merged**! You will probably need to refactor so your changes fit into the larger codebase
- **If your code is hard to unit test, and you don't want to unit test it, that's ok**. But be prepared to argue why that's the case!
- **It's entirely possible your changes won't be merged**, or will get ripped out later. This is also the case for my changes, as the Author!
- **Even a rejected/reverted PR is valuable**! It helps to explore the solution space, and know what works and what doesn't. For every line in the repo, at least three lines were tried, committed, and reverted/refactored, and more than 10 were tried without committing.
- **Feel free to send Proof-Of-Concept PRs** that you don't intend to get merged.

