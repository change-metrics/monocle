# lentille-bugzilla

Run the cli:

```ShellSession
$ cabal run lentille-bugzilla -- --help
Lentille worker

Usage: lentille-bugzilla --monocle-url TEXT --index TEXT --crawler-name TEXT
                         [--bugzilla-url TEXT] --bugzilla-product TEXT
                         [--since STRING] [--print-bugs]

Available options:
  -h,--help                Show this help text
  --monocle-url TEXT       The monocle API
  --index TEXT             The index name
  --crawler-name TEXT      The name of the crawler
  --bugzilla-url TEXT      The bugzilla url
  --bugzilla-product TEXT  The bugzilla product name
  --since STRING           Get bugs since
  --print-bugs             Just print bugs, to not amend monocle
```
