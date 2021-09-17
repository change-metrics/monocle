open Prelude

let cmp = (s, x, y) => x->s < y->s

module ProjectsSummaryTable = {
  type t = ConfigTypes.project_definition
  @react.component
  let make = (~store: Store.t, ~projects: list<t>) => {
    let columnNames = ["Name", "Repository", "Branch", "File"]

    let isOrdered = (first: t, second: t, index) =>
      switch index {
      | 0 => ((x: t) => x.name)->cmp(first, second)
      | 1 => ((x: t) => x.repository_regex)->cmp(first, second)
      | 2 => ((x: t) => x.branch_regex)->cmp(first, second)
      | 3 => ((x: t) => x.file_regex)->cmp(first, second)
      | _ => false
      }

    //    let mkLink = (entity: ChangeLink.t, label: string) => <ChangeLink store entity name={label} />
    let formatters: list<t => React.element> = list{
      project =>
        <MLink.MonoLink
          store path={"changes"} name={project.name} filter={"project:" ++ project.name}
        />,
      project => project.repository_regex->str,
      project => project.branch_regex->str,
      project => project.file_regex->str,
    }

    <SortableTable items={projects} defaultSortedColumn=2 columnNames isOrdered formatters />
  }
}

@react.component
let make = (~store: Store.t) => {
  let title = "Projects"
  let tooltip_content =
    "This shows the list of defined projects." ++
    " A project is composed of multiple" ++
    " repositories and can also be set " ++ "on a specific branch."
  let icon = <Patternfly.Icons.Project />
  switch Store.Fetch.projects(store) {
  | None => <Spinner />
  | Some(Error(title)) => <Alert variant=#Danger title />
  | Some(Ok({projects: list{}})) => <Alert variant=#Warning title={"Please define projects."} />
  | Some(Ok({projects})) =>
    <MCenteredContent>
      <MonoCard title tooltip_content icon> <ProjectsSummaryTable store projects /> </MonoCard>
    </MCenteredContent>
  }
}

let default = make
