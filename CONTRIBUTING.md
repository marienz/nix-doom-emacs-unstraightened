# How to Contribute

Contributions (code or otherwise) are much appreciated!

## Issues

You can help me help you by including steps to reproduce your problem. If you're
comfortable sharing the entire flake or Doom configuration triggering it, just
include a link to that in bug reports: pruning it down to just what's required
to reproduce the problem is appreciated but not required.

## Pull requests

This project aims to support everything Doom upstream does: if some Doom module
or feature is not usable from Unstraightened, that is generally a bug worth
fixing.

However, problems with specific packages not pulled in by Doom (added through
`package!` in your own configuration) are generally out of scope. If you cannot
find a way of fixing these without modifying Unstraightened, please file an
issue: I am generally happy to add hooks or features you can use from your own
configuration, but prefer not to carry patches, pins or overrides for specific
packages.
