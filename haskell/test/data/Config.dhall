{ tenants :
    List
      { crawlers :
          Optional
            ( List
                { name : Text
                , provider :
                    < BugzillaProvider :
                        { bugzilla_api_key : Text
                        , bugzilla_url : Text
                        , products : List Text
                        }
                    | GerritProvider :
                        { gerrit_http_password : Text
                        , gerrit_url : Text
                        , repositories : Optional (List Text)
                        , repositories_regex : Optional (List Text)
                        }
                    | GithubProvider :
                        { github_api_key : Text
                        , github_organizations : Optional (List Text)
                        , github_repositories : Optional (List Text)
                        , github_url : Text
                        }
                    | GitlabProvider :
                        { gitlab_api_key : Text
                        , gitlab_organizations : Optional (List Text)
                        , gitlab_repositories : Optional (List Text)
                        , gitlab_url : Text
                        }
                    >
                , update_since : Text
                }
            )
      , crawlers_api_key : Optional Text
      , idents :
          Optional
            ( List
                { aliases : List Text
                , groups : Optional (List Text)
                , ident : Text
                }
            )
      , index : Text
      , projects :
          Optional
            ( List
                { branch_regex : Optional Text
                , file_regex : Optional Text
                , name : Text
                , repository_regex : Optional Text
                }
            )
      }
}
