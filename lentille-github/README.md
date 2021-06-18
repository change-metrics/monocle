# lentille-github

GraphQL query powered by the http-client, streaming and morpheus-graphql libraries

- A streaming utility: [Lentille.GitHub](./src/Lentille/GitHub.hs)
- Example query: [Lentille.GitHub.Favorites](./src/Lentille/GitHub/Favorites.hs)

## Run the cli:

```ShellSession
Lentille GitHub Issue worker

Usage: lentille-github --monocle-url TEXT --index TEXT --crawler-name TEXT
                       [--github-url TEXT] [--since STRING] --repo TEXT
                       [--print-bugs]

Available options:
  -h,--help                Show this help text
  --monocle-url TEXT       The monocle API
  --index TEXT             The index name
  --crawler-name TEXT      The name of the crawler
  --github-url TEXT        The github url
  --since STRING           Get issues since
  --repo TEXT              Repo to get linked issue on
  --print-bugs             Just print TaskData, to not amend monocle
```
