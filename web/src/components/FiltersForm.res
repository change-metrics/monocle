open Prelude

// The definition of a filter:
module Filter = {
  type kind = Text | Date | Choice(list<string>)
  type t = {title: string, description: string, default: option<string>, kind: kind}

  // helper functions:
  let make = (title, description) => {
    title: title,
    description: description,
    default: None,
    kind: Text,
  }
  let makeChoice = (title, description, choices) => {
    title: title,
    description: description,
    default: None,
    kind: Choice(choices),
  }
  let getValue = filter => filter.default->fromMaybe("")
}

// The definition of filters:
module Filters = {
  type t = Belt.Map.String.t<Filter.t>

  // The list of filters:
  let filters = Belt.Map.String.fromArray([
    ("authors", Filter.make("Authors", "Author names")),
    ("exclude_authors", Filter.make("Exclude authors", "Author names")),
    ("repository", Filter.make("Repository", "Repositories regexp")),
    ("branch", Filter.make("Branch", "Branch regexp")),
    ("files", Filter.make("Files", "Files regexp")),
    ("gte", {...Filter.make("From date", "yyyy-MM-dd"), kind: Date}),
    ("lte", {...Filter.make("To date", "yyyy-MM-dd"), kind: Date}),
    ("approvals", Filter.make("Approvals", "Change approval")),
    ("exclude_approvals", Filter.make("Exclude Approvals", "Change approval")),
    (
      "state",
      Filter.makeChoice("Change state", "Filter by state", list{"OPEN", "CLOSED", "MERGED"}),
    ),
    ("task_priority", Filter.makeChoice("Task priority", "Filter by priority", Env.bzPriority)),
    ("task_severity", Filter.makeChoice("Task severity", "Filter by severity", Env.bzPriority)),
    ("task_type", Filter.make("Task type", "Filter by task type")),
  ])
  let map = f => filters->Belt.Map.String.keysToArray->Belt.Array.map(f)->ignore
  let mapWithKey = f => filters->Belt.Map.String.toArray->Belt.Array.map(f)
  let get = name => filters->Belt.Map.String.getExn(name)
}

// A module to manage filters values
module Values = {
  type t = Js.Dict.t<string>

  let empty: t = Js.Dict.empty()
  let get = (xs: t, name: string) => xs->Js.Dict.get(name)
  let set = (xs: t, name: string, value: string) => xs->Js.Dict.set(name, value)

  // Helper function to load values from the current url query string
  let loads = values => {
    let searchParams = URLSearchParams.current()
    Filters.map(name =>
      searchParams
      ->URLSearchParams.get(name)
      ->Js.Nullable.bind((. value) => values->set(name, value))
    )
    Js.log4("Set filters from", windowLocationSearch, "to", values)
    values
  }

  // Helper function to dump values to an url query string
  let dumps = values => {
    let searchParams = URLSearchParams.current()
    Filters.map(name =>
      switch values->get(name) {
      | None
      | Some("") =>
        // When a filter is not set or empty, delete the QS
        searchParams->URLSearchParams.delete(name)
      | Some(value) =>
        // Otherwise set the QS
        searchParams->URLSearchParams.set(name, value)
      }
    )
    let queryString = searchParams->URLSearchParams.toString
    Js.log4("Set qs from", values, "to", queryString)
    queryString
  }
}

// A component to manage user input
module Field = {
  let fieldStyle = ReactDOM.Style.make(~marginTop="5px", ~marginBottom="15px", ())
  @react.component
  let make = (~name, ~values) => {
    let filter = Filters.get(name)
    let (value, setValue) = React.useState(_ =>
      // Initial value is picked from query string, or filter default
      values->Js.Dict.get(name)->fromMaybe(filter->Filter.getValue)
    )
    let onChange = (v, _) => {
      // Values is the filter values dictionary
      values->Js.Dict.set(name, v)
      // We also store the local field value in a react state
      setValue(_ => v)
    }
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
  let make = (~values) => {
    let activeList =
      Filters.mapWithKey(((name, filter)) =>
        switch values->Values.get(name) {
        | None
        | Some("") =>
          None
        | Some(value) =>
          (filter.description->Js.String.toLowerCase ++
            switch filter.kind {
            | Text => " named (" ++ value ++ ")"
            | Date => " " ++ value
            | Choice(_) => " set to " ++ value
            })->Some
        }
      )
      ->Belt.List.fromArray
      ->catMaybes
    switch activeList {
    | list{} => React.null
    | xs => <b> {"Active filters: "->str} {xs->concatSep(", ")->str} </b>
    }
  }
}

@react.component
let make = (~updateFilters: string => unit, ~showChangeParams: bool) => {
  // We use a (ref) dictionary to store the current filter values
  let values = React.useRef(Values.empty->Values.loads).current
  let applyFilters = _ => {
    values->Values.dumps->updateFilters
  }
  <>
    <MExpandablePanel title="Filter">
      <FieldGroups>
        <FieldGroup> <Field name="gte" values /> <Field name="lte" values /> </FieldGroup>
        <FieldGroup>
          <Field name="authors" values /> <Field name="exclude_authors" values />
        </FieldGroup>
        <FieldGroup>
          <Field name="repository" values />
          <Field name="branch" values />
          <Field name="files" values />
        </FieldGroup>
        {showChangeParams->maybeRender(<>
          <FieldGroup>
            <Field name="approvals" values /> <Field name="exclude_approvals" values />
          </FieldGroup>
          <FieldGroup> <Field name="state" values /> </FieldGroup>
        </>)}
        {Env.withBZ->maybeRender(
          <FieldGroup>
            <Field name="task_priority" values />
            <Field name="task_severity" values />
            <Field name="task_type" values />
          </FieldGroup>,
        )}
        <ActionGroup>
          <Button variant=#Primary onClick=applyFilters> {"Apply"->React.string} </Button>
        </ActionGroup>
      </FieldGroups>
    </MExpandablePanel>
    <FilterSummary values />
  </>
}
//
let default = make
