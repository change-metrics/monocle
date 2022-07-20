let Prelude =
      https://prelude.dhall-lang.org/v17.0.0/package.dhall
        sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e

let Config = ./Config/Type.dhall

let Workspace = ./Workspace/Type.dhall

let getIndexNames =
      λ(config : Config) →
        Prelude.List.map
          Workspace
          Text
          (λ(index : Workspace) → index.name)
          config.workspaces

let getIndexNamesList =
      λ(config : Config) → Prelude.Text.concatSep "\n" (getIndexNames config)

in  { getIndexNames, getIndexNamesList }
