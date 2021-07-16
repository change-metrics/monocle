{ tenants :
    List
      { crawlers :
          List
            { name : Text
            , provider :
                < BugzillaProvider :
                    { bugzilla_products : Optional (List Text)
                    , bugzilla_token : Text
                    , bugzilla_url : Text
                    }
                | GerritProvider :
                    { gerrit_login : Optional Text
                    , gerrit_password : Optional Text
                    , gerrit_prefix : Optional Text
                    , gerrit_repositories : Optional (List Text)
                    , gerrit_url : Text
                    , gerrit_url_insecure : Bool
                    }
                | GithubProvider :
                    { github_organization : Text
                    , github_repositories : Optional (List Text)
                    , github_token : Text
                    , github_url : Optional Text
                    }
                | GitlabProvider :
                    { gitlab_organizations : Optional (List Text)
                    , gitlab_repositories : Optional (List Text)
                    , gitlab_token : Text
                    , gitlab_url : Optional Text
                    }
                | TaskDataProvider
                >
            , update_since : Text
            }
      , crawlers_api_key : Text
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
      , search_aliases : Optional (List { alias : Text, name : Text })
      , users : Optional (List Text)
      }
}
