let P = ../Crawler/Provider.dhall

in  { Gitlab = P.GitlabProvider
    , Github = P.GithubProvider
    , GithubApplication = P.GithubApplicationProvider
    , Bugzilla = P.BugzillaProvider
    , Gerrit = P.GerritProvider
    , TaskData = P.TaskDataProvider
    }
