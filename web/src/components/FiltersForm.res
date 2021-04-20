open Prelude

// The definition of a filter:
module Filter = {
  type kind = Text | Date
  type t = {description: string, default: option<string>, kind: kind}

  // helper functions:
  let make = description => {description: description, default: None, kind: Text}
  let getValue = filter => filter.default->fromMaybe("")
}

// The definition of filters:
module Filters = {
  type t = Belt.Map.String.t<Filter.t>
  // The list of filters:
  let filters = Belt.Map.String.fromArray([
    ("authors", Filter.make("Authors")),
    ("exclude_authors", Filter.make("Exclude Authors")),
    ("repository", Filter.make("Repositories regexp")),
    ("branch", Filter.make("Branch regexp")),
    ("files", Filter.make("Files regexp")),
    ("gte", {...Filter.make("From date"), kind: Date}),
    ("lte", {...Filter.make("To date"), kind: Date}),
    ("approvals", Filter.make("Approvals")),
    ("exclude_approvals", Filter.make("Exclude Approvals")),
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
  let fieldStyle = ReactDOM.Style.make(~marginTop="5px", ~marginBottom="5px", ())
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
    switch filter.kind {
    | Text =>
      <TextInput
        style={fieldStyle} id={name ++ "-input"} placeholder={filter.description} onChange value
      />
    | Date =>
      <div style={fieldStyle}>
        <DatePicker id={name ++ "-date"} placeholder={filter.description} onChange value />
      </div>
    }
  }
}

module FieldGroup = {
  @react.component
  let make = (~children, ~label) =>
    <MGridItem> <FormGroup label fieldId={label ++ "fid"}> {children} </FormGroup> </MGridItem>
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
      <Form>
        <MGrid>
          <FieldGroup label="Date">
            <Field name="gte" values /> <Field name="lte" values />
          </FieldGroup>
          <FieldGroup label="Authors">
            <Field name="authors" values /> <Field name="exclude_authors" values />
          </FieldGroup>
          <FieldGroup label="Projects">
            <Field name="repository" values />
            <Field name="branch" values />
            <Field name="files" values />
          </FieldGroup>
          {showChangeParams->maybeRender(
            <FieldGroup label="Approvals">
              <Field name="approvals" values /> <Field name="exclude_approvals" values />
            </FieldGroup>,
          )}
        </MGrid>
        <ActionGroup>
          <Button variant=#Primary onClick=applyFilters> {"Apply"->React.string} </Button>
        </ActionGroup>
      </Form>
    </MExpandablePanel>
    <FilterSummary values />
  </>
}
//
let default = make
