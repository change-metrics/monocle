open Prelude

// The definition of a filter:
module Filter = {
  type kind = Text | Date | Choice(list<string>)
  type t = {title: string, description: string, default: option<string>, kind: kind}

  // Helper functions:
  let make = (title, description) => {
    title: title,
    description: description,
    default: None,
    kind: Text,
  }
  let makeChoice = (title, description, choices) => {
    ...make(title, description),
    kind: Choice(choices),
  }
  let getValue = filter => filter.default->fromMaybe("")
  let validate = (filter, value) =>
    switch filter.kind {
    | Text | Date => true
    | Choice(values) => values->elemText(value)
    }
}

// The definition of filters
module Filters = {
  // See: https://rescript-lang.org/docs/manual/latest/api/belt/map-string
  // The value type is a tuple of (the filter, it's value, and a setState function)
  type t = Belt.Map.String.t<(Filter.t, string, (string => string) => unit)>

  // The list of static filters:
  let staticFilters = [
    ("authors", Filter.make("Authors", "Author names")),
    ("exclude_authors", Filter.make("Exclude authors", "Author names")),
    ("repository", Filter.make("Repository", "Repositories regexp")),
    ("branch", Filter.make("Branch", "Branch regexp")),
    ("files", Filter.make("Files", "Files regexp")),
    (
      "gte",
      {
        ...Filter.make("From date", "yyyy-MM-dd"),
        default: Time.getDateMinusMonth(3)->Some,
        kind: Date,
      },
    ),
    ("lte", {...Filter.make("To date", "yyyy-MM-dd"), kind: Date}),
    ("approvals", Filter.make("Approvals", "Change approval")),
    ("exclude_approvals", Filter.make("Exclude Approvals", "Change approval")),
    (
      "state",
      Filter.makeChoice(
        "Change state",
        "Filter by state",
        list{"ALL", "OPEN", "MERGED", "SELF-MERGED", "CLOSED"},
      ),
    ),
    ("task_priority", Filter.makeChoice("Task priority", "Filter by priority", Env.bzPriority)),
    ("task_severity", Filter.makeChoice("Task severity", "Filter by severity", Env.bzPriority)),
    ("task_type", Filter.make("Task type", "Filter by task type")),
  ]

  // Helper functions:
  let map = (dict, f) => dict->Belt.Map.String.mapWithKey(f)
  let mapM = (dict, f) => dict->map(f)->ignore
  let get = (dict, name) => dict->Belt.Map.String.getExn(name)

  // Loads the values from the url search params
  let loads = (searchParams, states) =>
    states->mapM((name, (filter, _, set)) =>
      searchParams
      ->URLSearchParams.get(name)
      ->Js.Nullable.bind((. value) => set(_ => filter->Filter.validate(value) ? value : ""))
    )

  // Dump the values to a query string
  let dumps = states => {
    let searchParams = URLSearchParams.current()
    states->mapM((name, (_, value, _)) =>
      switch value {
      | "" => searchParams->URLSearchParams.delete(name)
      | value => searchParams->URLSearchParams.set(name, value)
      }
    )
    searchParams->URLSearchParams.toString
  }

  // A custom hook to manage filter value:
  type queryStringName = string
  let useFilters = (dynamicFilters: array<(queryStringName, Filter.t)>): t => {
    // First we create a state hook for each filter
    let states: t =
      Belt.Array.concat(staticFilters, dynamicFilters)
      ->Belt.Map.String.fromArray
      ->Belt.Map.String.map(x => {
        let (value, setValue) = React.useState(_ => x->Filter.getValue)
        (x, value, setValue)
      })
    // Then we load the current value when the url search params change
    React.useEffect1(() => {
      Js.log("Loading filter values")
      URLSearchParams.current()->loads(states)
      None
    }, [readWindowLocationSearch()])
    states
  }
}

// A component to manage user input
module Field = {
  let fieldStyle = ReactDOM.Style.make(~marginTop="5px", ~marginBottom="15px", ())
  @react.component
  let make = (~name, ~states: Filters.t) => {
    let (filter, value, setValue) = states->Filters.get(name)
    let onChange = (v, _) => setValue(_ => v)
    <FormGroup label={filter.title} fieldId={name ++ "-fg"} hasNoPaddingTop=false>
      <div style={fieldStyle}>
        {switch filter.kind {
        | Text =>
          <TextInput id={name ++ "-input"} placeholder={filter.description} onChange value />
        | Date =>
          <DatePicker id={name ++ "-date"} placeholder={filter.description} onChange value />
        | Choice(options) =>
          <MSelect
            placeholder={filter.description} options valueChanged={v => onChange(v, ())} value
          />
        }}
      </div>
    </FormGroup>
  }
}

module FieldGroup = {
  @react.component
  let make = (~children) => <MGridItem> <MSimpleCard> {children} </MSimpleCard> </MGridItem>
}

module FieldGroups = {
  @react.component
  let make = (~children) => <Form> <MGrid> {children} </MGrid> </Form>
}

module FilterSummary = {
  @react.component
  let make = (~states: Filters.t) => {
    let activeList =
      states
      ->Filters.map((_, (filter, value, _)) =>
        switch value {
        | "" => None
        | value =>
          (filter.title->Js.String.toLowerCase ++
            switch filter.kind {
            | Text => " named (" ++ value ++ ")"
            | Date => " " ++ value
            | Choice(_) => " set to " ++ value
            })->Some
        }
      )
      ->Belt.Map.String.valuesToArray
      ->Belt.List.fromArray
      ->catMaybes
    switch activeList {
    | list{} => React.null
    | xs => <b> {"Active filters: "->str} {xs->concatSep(", ")->str} </b>
    }
  }
}

module FilterBox = {
  @react.component
  let make = (
    ~updateFilters: string => unit,
    ~showChangeParams: bool,
    ~projects: array<Project.t>,
  ) => {
    let states = Filters.useFilters([
      (
        "project_definition",
        Filter.makeChoice(
          "Projects",
          "Select a project",
          projects->Belt.Array.map(project => project.name)->Belt.List.fromArray,
        ),
      ),
    ])
    let onClick = _ => states->Filters.dumps->updateFilters
    <MStack>
      <MExpandablePanel title="Filter">
        <FieldGroups>
          <FieldGroup> <Field name="gte" states /> <Field name="lte" states /> </FieldGroup>
          <FieldGroup>
            <Field name="authors" states /> <Field name="exclude_authors" states />
          </FieldGroup>
          <FieldGroup>
            {projects->maybeRenderList(<Field name="project_definition" states />)}
            <Field name="repository" states />
            <Field name="branch" states />
            <Field name="files" states />
          </FieldGroup>
          {showChangeParams->maybeRender(<>
            <FieldGroup>
              <Field name="approvals" states /> <Field name="exclude_approvals" states />
            </FieldGroup>
            <FieldGroup> <Field name="state" states /> </FieldGroup>
          </>)}
          {Env.withBZ->maybeRender(
            <FieldGroup>
              <Field name="task_priority" states />
              <Field name="task_severity" states />
              <Field name="task_type" states />
            </FieldGroup>,
          )}
          <ActionGroup>
            <Button variant=#Primary onClick> {"Apply"->React.string} </Button>
          </ActionGroup>
        </FieldGroups>
      </MExpandablePanel>
      <FilterSummary states />
    </MStack>
  }
}

@react.component
let make = (~updateFilters: string => unit, ~showChangeParams: bool, ~index: string) => {
  let indices = useAutoGet(() => getProjects(index))
  switch indices {
  | None => <Spinner />
  | Some(Ok(projects)) => <FilterBox updateFilters showChangeParams projects />
  // We ignore the error right now because the api returns a 404
  // TODO: make 404 non error?
  | Some(Error(_error)) => <FilterBox updateFilters showChangeParams projects={[]} />
  }
}

let default = make
