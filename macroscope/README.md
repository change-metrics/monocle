# Macroscope

Macroscope is the Lentille crawlers manager. Based on the configuration file, it schedules the execution of configured provider to crawl.

## Run the cli:

```ShellSession
$ cabal run macroscope -- --help
Macroscope lentille runner

Usage: macroscope --monocle-url TEXT --config STRING [--debug]
                  [--interval WORD32]

Available options:
  -h,--help                Show this help text
  --monocle-url TEXT       The monocle API
  --config STRING          The monocle configuration
  --debug                  Verbose mode
  --interval WORD32        Interval in seconds, default to 600
```
