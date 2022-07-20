let P = ../Auth/Provider.dhall

in  { OIDC = P.OIDCProvider, P.GithubOAuth = P.GithubAuthProvider }
