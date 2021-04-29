open Prelude

module RelativeDate = {
  type t = WeeksAgo(int) | MonthsAgo(int) | YearsAgo(int)
  let rdates = list{
    WeeksAgo(1),
    WeeksAgo(2),
    WeeksAgo(3),
    MonthsAgo(1),
    MonthsAgo(2),
    MonthsAgo(3),
    MonthsAgo(6),
    YearsAgo(1),
    YearsAgo(2),
    YearsAgo(3),
    YearsAgo(4),
    YearsAgo(5),
    YearsAgo(10),
  }
  let toString = (rd: t): string =>
    switch rd {
    | WeeksAgo(1) => "1 week ago"
    | WeeksAgo(v) => v->Belt.Int.toString ++ " weeks ago"
    | MonthsAgo(1) => "1 month ago"
    | MonthsAgo(v) => v->Belt.Int.toString ++ " months ago"
    | YearsAgo(1) => "1 year ago"
    | YearsAgo(v) => v->Belt.Int.toString ++ " years ago"
    }
  let toStringList = (rds: list<t>) => rds->Belt.List.map(toString)
  let fromString = (s: string): t => {
    let get = (s: string): int => s->Belt.Int.fromString->Belt.Option.getWithDefault(1)
    switch s |> Js.String.split(" ") {
    | [n, u, "ago"] =>
      switch u {
      | "week" | "weeks" => WeeksAgo(n->get)
      | "month" | "months" => MonthsAgo(n->get)
      | "year" | "years" => YearsAgo(n->get)
      | _ => WeeksAgo(1)
      }
    | _ => WeeksAgo(1)
    }
  }
  let toDateString = (rd: t): string =>
    switch rd {
    | WeeksAgo(v) => v->Time.getDateMinusWeek
    | MonthsAgo(v) => v->Time.getDateMinusMonth
    | YearsAgo(v) => v->Time.getDateMinusYear
    }
  let strToDateString = (strRd: string): string => strRd->fromString->toDateString
}

// The definition of a filter:
module Filter = {
  type choiceType =
    Keywords(list<string>) | RelativeDates(list<RelativeDate.t>) | Projects(list<string>)
  type kind = Text | Date | Choice(choiceType)
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
    | Choice(Keywords(values)) => values->elemText(value)
    | _ => true
    }
}

// The definition of filters
module Filters = {
  // See: https://rescript-lang.org/docs/manual/latest/api/belt/map-string
  // The value type is a tuple of (the filter, it's value, a value setState function,
  // a disabled boolean value and a setDisabled function)
  type t = Belt.Map.String.t<(Filter.t, string, (string => string) => unit, bool, bool => unit)>

  // The list of static filters:
  let staticFilters = [
    ("authors", Filter.make("Authors", "Author names")),
    ("exclude_authors", Filter.make("Exclude authors", "Author names")),
    ("repository", Filter.make("Repository", "Repository regexp")),
    ("branch", Filter.make("Branch", "Branch regexp")),
    ("files", Filter.make("Files", "File regexp")),
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
        Keywords(list{"ALL", "OPEN", "MERGED", "SELF-MERGED", "CLOSED"}),
      ),
    ),
    (
      "relativedate",
      Filter.makeChoice(
        "Relative date",
        "Select a relative date",
        RelativeDates(RelativeDate.rdates),
      ),
    ),
    (
      "task_priority",
      Filter.makeChoice("Task priority", "Filter by priority", Keywords(Env.bzPriority)),
    ),
    (
      "task_severity",
      Filter.makeChoice("Task severity", "Filter by severity", Keywords(Env.bzPriority)),
    ),
    ("task_type", Filter.make("Task type", "Filter by task type")),
  ]

  // Helper functions:
  let map = (dict, f) => dict->Belt.Map.String.mapWithKey(f)
  let mapM = (dict, f) => dict->map(f)->ignore
  let get = (dict, name) => dict->Belt.Map.String.getExn(name)

  let setAbsoluteDateFromRelativeDate = (states, rchoice) => {
    let (_, _, setGte, _, _) = states->get("gte")
    let (_, _, setLte, _, _) = states->get("lte")
    setGte(_ => rchoice->RelativeDate.strToDateString)
    setLte(_ => "")
  }

  let disableAbsoluteDate = (states, disable: bool) => {
    let (_, _, _, _, setGteIsDisabled) = states->get("gte")
    let (_, _, _, _, setLteIsDisabled) = states->get("lte")
    setGteIsDisabled(disable)
    setLteIsDisabled(disable)
  }

  // Loads the values from the url search params
  let loads = (searchParams, states) =>
    states->mapM((name, (filter, _, set, _, _)) =>
      searchParams
      ->URLSearchParams.get(name)
      ->Js.Nullable.bind((. value) => {
        set(_ => filter->Filter.validate(value) ? value : "")
        // TODO(fbo) disable related fields when project parameter is set
        switch name {
        | "relativedate" => {
            states->disableAbsoluteDate(true)
            states->setAbsoluteDateFromRelativeDate(value)
          }
        | _ => ()
        }
      })
    )

  // Dump the values to a query string
  let dumps = states => {
    let searchParams = URLSearchParams.current()
    states->mapM((name, (_, value, _, _, _)) =>
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
        let (isDisabled, _setIsDisabled) = React.useState(_ => false)
        let setIsDisabled = (val: bool) => _setIsDisabled(_ => val)
        (x, value, setValue, isDisabled, setIsDisabled)
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
  let make = (~name, ~states: Filters.t, ~projects: option<list<ConfigTypes.project_definition>>=?) => {
    let (filter, value, setValue, isDisabled, _) = states->Filters.get(name)
    let onChange = (v, _) => setValue(_ => v)
    let rdOnChange = (v, _) => {
      setValue(_ => v)
      switch v {
      | "" => states->Filters.disableAbsoluteDate(false)
      | value => {
          states->Filters.disableAbsoluteDate(true)
          states->Filters.setAbsoluteDateFromRelativeDate(value)
        }
      }
    }
    let projectOnChange = (v, _) => {
      setValue(_ => v)
      let disableEnableField = (fname: string, disable: bool) => {
        let (_, _, _, _, setIsDisabled) = states->Filters.get(fname)
        setIsDisabled(disable)
      }
      let disableFieldIfNeeded = (field: string, fname: string): unit => switch field {
        | "" => ()
        | _ => fname->disableEnableField(true)
      }
      let enableFields = () => {
        "branch"->disableEnableField(false)
        "files"->disableEnableField(false)
        "repository"->disableEnableField(false)
      }
      switch v {
      | "" => enableFields()
      | _ => {
          enableFields()
          projects
          ->Belt.Option.flatMap(mps =>
            mps
            ->Belt.List.getBy(p => p.name == v)
            ->Belt.Option.flatMap(sp => {
              sp.branch_regex->disableFieldIfNeeded("branch")->ignore
              sp.file_regex->disableFieldIfNeeded("files")->ignore
              sp.repository_regex->disableFieldIfNeeded("repository")->Some
            })
          )
          ->ignore
        }
      }
    }
    <FormGroup label={filter.title} fieldId={name ++ "-fg"} hasNoPaddingTop=false>
      <div style={fieldStyle}>
        {switch filter.kind {
        | Text =>
          <TextInput
            id={name ++ "-input"} placeholder={filter.description} onChange value isDisabled
          />
        | Date =>
          <DatePicker
            id={name ++ "-date"} placeholder={filter.description} onChange value isDisabled
          />
        | Choice(Keywords(options)) =>
          <MSelect
            placeholder={filter.description} options valueChanged={v => onChange(v, ())} value
          />
        | Choice(RelativeDates(options)) => {
            let options = options->RelativeDate.toStringList
            <MSelect
              placeholder={filter.description} options valueChanged={v => rdOnChange(v, ())} value
            />
          }
        | Choice(Projects(options)) =>
          <MSelect
            placeholder={filter.description}
            options
            valueChanged={v => projectOnChange(v, ())}
            value
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
      ->Filters.map((_, (filter, value, _, _, _)) =>
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
    ~projects: list<ConfigTypes.project_definition>,
  ) => {
    let states = Filters.useFilters([
      (
        "project",
        Filter.makeChoice(
          "Projects",
          "Select a project",
          projects->Belt.List.map(project => project.name)->Projects,
        ),
      ),
    ])
    let onClick = _ => states->Filters.dumps->updateFilters
    <MStack>
      <MExpandablePanel title="Filter">
        <FieldGroups>
          <FieldGroup>
            <Field name="relativedate" states />
            <Field name="gte" states />
            <Field name="lte" states />
          </FieldGroup>
          <FieldGroup>
            <Field name="authors" states /> <Field name="exclude_authors" states />
          </FieldGroup>
          <FieldGroup>
            {projects->maybeRenderList(<Field name="project" states projects />)}
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
  let indices = useAutoGet(() => WebApi.Config.getProjects({index: index}))
  switch indices {
  | None => <Spinner />
  | Some(Ok({projects})) => <FilterBox updateFilters showChangeParams projects />
  | Some(Error(_error)) => <Alert title={_error} variant=#Danger />
  }
}

let default = make
