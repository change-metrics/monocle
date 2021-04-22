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
}

// The definition of filters
module Filters = {
  // See: https://rescript-lang.org/docs/manual/latest/api/belt/map-string
  // The value type is a tuple of (the filter, it's value, and a setState function)
  type t = Belt.Map.String.t<(Filter.t, string, (string => string) => unit)>

  // The list of filters:
  let all = [
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
  ]->Belt.Map.String.fromArray

  // Helper functions:
  let map = (dict, f) => dict->Belt.Map.String.mapWithKey(f)
  let mapM = (dict, f) => dict->map(f)->ignore
  let get = (dict, name) => dict->Belt.Map.String.getExn(name)

  // Loads the values from the url search params
  let loads = (searchParams, states) =>
    states->mapM((name, (_, _, set)) =>
      searchParams->URLSearchParams.get(name)->Js.Nullable.bind((. value) => set(_ => value))
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
  let useFilter = (): t => {
    // First we create a state hook for each filter
    let states: t = all->Belt.Map.String.map(x => {
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

@react.component
let make = (~updateFilters: string => unit, ~showChangeParams: bool) => {
  let states = Filters.useFilter()
  let onClick = _ => states->Filters.dumps->updateFilters
  <MStack>
    <MExpandablePanel title="Filter">
      <FieldGroups>
        <FieldGroup> <Field name="gte" states /> <Field name="lte" states /> </FieldGroup>
        <FieldGroup>
          <Field name="authors" states /> <Field name="exclude_authors" states />
        </FieldGroup>
        <FieldGroup>
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
//
let default = make
