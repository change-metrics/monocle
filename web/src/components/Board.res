// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The board view
//
open Prelude

let startWithEditorOpen = false
let startWithFieldModalOpen = false

module Column = {
  type t = {name: string, query: string}

  @react.component
  let make = (~index, ~column) => {
    let (result, setResult) = React.useState(_ => None)
    let handleOk = (resp: WebApi.axiosResponse<SearchTypes.changes_query_response>) =>
      setResult(_ => resp.data->Some)->Js.Promise.resolve
    React.useEffect1(() => {
      switch column.query {
      | "" => ignore()
      | query =>
        ignore(
          WebApi.Search.changesQuery({index: index, query: query}) |> Js.Promise.then_(handleOk),
        )
      }
      None
    }, [column.query])

    <Patternfly.Card>
      <Patternfly.CardHeader> {column.name->str} </Patternfly.CardHeader>
      <Patternfly.CardBody>
        {switch result {
        | None => React.null
        | Some(SearchTypes.Error(err)) =>
          <Alert
            title={err.message ++ " at " ++ string_of_int(Int32.to_int(err.position))}
            variant=#Danger
          />
        | Some(SearchTypes.Items(items)) => {
            let changes = items.changes->Belt.List.toArray
            switch changes->Belt.Array.length {
            | 0 => <p> {"No changes matched"->str} </p>
            | _ =>
              <Patternfly.DataList isCompact={true}>
                {changes
                ->Belt.Array.map(change => <Change.Search key={change.url} change={change} />)
                ->React.array}
              </Patternfly.DataList>
            }
          }
        }}
      </Patternfly.CardBody>
    </Patternfly.Card>
  }

  let mk = str => {name: str, query: ""}
}

module FieldSelectorModal = {
  type res<'data> = option<result<'data, string>>
  module FieldSelector = {
    @react.component
    let make = (
      ~suggestions: SearchTypes.search_suggestions_response,
      ~fields: list<SearchTypes.field>,
      ~fieldName,
      ~setFieldName,
      ~fieldValue,
      ~setFieldValue,
    ) => {
      let get: string => option<SearchTypes.field> = name =>
        fields->Belt.List.getBy(field => field.name == name)
      let getValues: string => list<string> = name =>
        switch name {
        | "state" => list{"open", "merged", "self_merged", "abandoned"}
        | "author" => suggestions.authors
        | "approval" => suggestions.approvals
        | "priority" => suggestions.priorities
        | "severity" => suggestions.severities
        | _ => list{}
        }

      let value = fieldValue
      let onChange = (v, _) => setFieldValue(_ => v)

      <>
        <MSelect
          placeholder={"Pick a field"}
          options={fields->Belt.List.map(f => f.name)}
          multi={false}
          value={fieldName}
          valueChanged={v => setFieldName(_ => v)}
        />
        {switch get(fieldName) {
        | Some(field) =>
          switch getValues(fieldName) {
          | list{} =>
            <TextInput id={"field-input"} placeholder={field.description} onChange value />
          | xs =>
            <MSelect
              isCreatable={true}
              placeholder={field.description}
              options={xs}
              valueChanged={v => setFieldValue(_ => v)}
              value
            />
          }
        | None => React.null
        }}
        <br />
        <br />
      </>
    }
  }
  @react.component
  let make = (
    ~isOpen,
    ~onClose: option<(string, string)> => unit,
    ~suggestions: res<SearchTypes.search_suggestions_response>,
    ~fields,
  ) => {
    let (fieldName, setFieldName) = React.useState(_ => "")
    let (fieldValue, setFieldValue) = React.useState(_ => "")
    let onConfirm = _ => onClose(Some(fieldName, fieldValue))
    let onCancel = _ => onClose(None)
    <Patternfly.Modal
      title="Field selector"
      variant=#Large
      isOpen
      onClose={_ => onClose(None)}
      actions=[
        <Patternfly.Button key="confirm" variant=#Primary onClick={onConfirm}>
          {"Confirm"->str}
        </Patternfly.Button>,
        <Patternfly.Button key="cancel" variant=#Link onClick={onCancel}>
          {"Cancel"->str}
        </Patternfly.Button>,
      ]>
      <div style={ReactDOM.Style.make(~height="400px", ())}>
        {switch suggestions {
        | Some(Ok(suggestions)) =>
          <FieldSelector suggestions fields fieldName setFieldName fieldValue setFieldValue />
        | _ => <Spinner />
        }}
      </div>
    </Patternfly.Modal>
  }
}

module ColumnEditor = {
  @react.component
  let make = (
    ~pos: int,
    ~count: int,
    ~nameRef: ref<string>,
    ~queryRef: ref<string>,
    ~onRemove: int => unit,
    ~fields,
    ~suggestions,
  ) => {
    let (_, doRender) = React.useState(_ => "")
    let (showFieldSelector, setShowFieldSelector) = React.useState(_ => startWithFieldModalOpen)
    let setAndRender = (r, v) => {
      r.contents = v
      doRender(_ => v)
    }
    let setName = (v, _) => setAndRender(nameRef, v)
    let setQuery = (v, _) => setAndRender(queryRef, v)
    let appendField = v => {
      switch v {
      | Some(name, value) => {
          let prefix = queryRef.contents == "" ? "" : " and "
          let expr = switch Js.String.split(",", value)->Belt.Array.map(value =>
            name ++ ":" ++ value
          ) {
          | [x] => x
          | xs => "(" ++ Js.Array.joinWith(" or ", xs) ++ ")"
          }
          setQuery(queryRef.contents ++ prefix ++ expr, ())
        }
      | None => ignore()
      }
      setShowFieldSelector(_ => false)
    }
    <span style={ReactDOM.Style.make(~display="flex", ())}>
      <FieldSelectorModal isOpen={showFieldSelector} onClose={appendField} fields suggestions />
      <Patternfly.TextInput
        style={ReactDOM.Style.make(~width="200px", ())}
        id="col-name"
        value={nameRef.contents}
        onChange={setName}
        _type=#Text
      />
      <Patternfly.Button onClick={_ => setShowFieldSelector(_ => true)}>
        {"Add field"->str}
      </Patternfly.Button>
      <Patternfly.TextInput
        id="col-search"
        value={queryRef.contents}
        onChange={setQuery}
        _type=#Text
        iconVariant=#Search
      />
      {maybeRender(
        count > 1,
        <Patternfly.Button variant=#Danger onClick={_ => onRemove(pos)}>
          {"Remove"->str}
        </Patternfly.Button>,
      )}
    </span>
  }
}

module Board = {
  type t = {title: string, columns: list<Column.t>}

  let default = {
    title: "Reviewer Board",
    columns: list{
      {Column.name: "To Review", query: "state: open and updated_at < now-1week"},
      {Column.name: "To Approve", query: "state: open and updated_at > now-1week"},
      {Column.name: "Done", query: "state:merged and updated_at > now-1week"},
    },
  }

  let columnsArray = (board: t) => {
    let arr = board.columns->Belt.List.toArray
    (arr, arr->Belt.Array.length)
  }

  let saveToUrl = (board: t) => {
    resetLocationSearch()->ignore
    setLocationSearch("t", board.title)->ignore
    Belt.List.mapWithIndex(board.columns, (index, column) => {
      let posStr = string_of_int(index)
      setLocationSearch("n" ++ posStr, column.name)->ignore
      setLocationSearch("q" ++ posStr, column.query)->ignore
    })->ignore
    board
  }

  let loadFromUrl: unit => t = () => {
    let params = URLSearchParams.current()
    let getP = name => params->URLSearchParams.get(name)->Js.Nullable.toOption
    let rec go = pos => {
      let posStr = string_of_int(pos)
      switch (getP("n" ++ posStr), getP("q" ++ posStr)) {
      | (Some(name), Some(query)) => go(pos + 1)->Belt.List.add({Column.name: name, query: query})
      | _ => list{}
      }
    }
    switch (getP("t"), go(0)) {
    | (None, _)
    | (_, list{}) => default
    | (Some(title), columns) => {
        title: title,
        columns: columns,
      }
    }
  }

  let addNewColumn = (board: t) => {
    let newName = "Column #" ++ string_of_int(board.columns->Belt.List.length + 1)
    {...board, columns: board.columns->Belt.List.concat(list{Column.mk(newName)})}
  }

  let removeColumn = (pos: int, board: t) => {
    {...board, columns: board.columns->Belt.List.keepWithIndex((_, index) => index != pos)}
  }

  let saveRefs = (title: string, columnsRefs: array<(ref<string>, ref<string>)>, _) =>
    {
      title: title,
      columns: columnsRefs
      ->Belt.Array.map(((nameRef, queryRef)) => {
        Column.name: nameRef.contents,
        query: queryRef.contents,
      })
      ->Belt.List.fromArray,
    }->saveToUrl

  module Editor = {
    @react.component
    let make = (
      ~board: t,
      ~columns: array<Column.t>,
      ~onAdd,
      ~onRemove,
      ~onSave,
      ~suggestions,
      ~fields,
    ) => {
      let (showColumnEditor, setShowColumnEditor) = React.useState(_ => startWithEditorOpen)
      let (title, setTitle) = React.useState(_ => board.title)
      let columnsCount = columns->Belt.Array.length
      // We store a ref for each columns name and query, so that individual update doesn't
      // refresh the whole editor.
      let columnsRefs: array<(ref<string>, ref<string>)> =
        columns->Belt.Array.map(column => (ref(column.name), ref(column.query)))

      // When removing a middle column, the one after lose their state, so we need to save
      // first
      let onRemove = pos => {
        onSave(title, columnsRefs)
        onRemove(pos)
      }

      let topRow =
        <span style={ReactDOM.Style.make(~display="flex", ())}>
          <Patternfly.TextInput
            id="board-name"
            isDisabled={!showColumnEditor}
            value={title}
            onChange={(v, _) => setTitle(_ => v)}
            _type=#Text
          />
          {showColumnEditor
            ? <Patternfly.Button
                _type=#Submit
                onClick={_ => {
                  onSave(title, columnsRefs)
                  setShowColumnEditor(_ => false)
                }}>
                {"Save"->str}
              </Patternfly.Button>
            : <Patternfly.Button variant=#Tertiary onClick={_ => setShowColumnEditor(_ => true)}>
                {"Edit"->str}
              </Patternfly.Button>}
        </span>

      let bottomRow =
        <>
          <SearchToolTip fields />
          <Patternfly.Button
            onClick={_ => {
              onSave(title, columnsRefs)
              onAdd()
            }}>
            {"AddColumn"->str}
          </Patternfly.Button>
        </>

      let columnsEditor = showColumnEditor
        ? <>
            {columnsRefs
            ->Belt.Array.mapWithIndex((pos, (nameRef, queryRef)) =>
              <ColumnEditor
                key={nameRef.contents ++ string_of_int(pos)}
                pos
                nameRef
                queryRef
                onRemove
                count={columnsCount}
                fields
                suggestions
              />
            )
            ->React.array}
            {bottomRow}
          </>
        : React.null

      <div> {topRow} {columnsEditor} </div>
    }
  }
}

@react.component
let make = (~index: string) => {
  // Fetch search info from api
  let fieldsM = useAutoGet(() => WebApi.Search.fields({version: "1"}))
  let suggestions = useAutoGet(() => WebApi.Search.suggestions({index: index}))

  // Load from url and store the column state
  let (board, setBoard) = React.useState(Board.loadFromUrl)
  let columns = board.columns->Belt.List.toArray

  // Callbacks to manage the state
  let onAdd = () => setBoard(Board.addNewColumn)
  let onRemove = pos => setBoard(Board.removeColumn(pos))

  let onSave = (title: string, columnsRefs: array<(ref<string>, ref<string>)>) =>
    setBoard(Board.saveRefs(title, columnsRefs))

  let editor = switch fieldsM {
  | Some(Ok({fields})) => <Board.Editor columns board onAdd onRemove onSave fields suggestions />
  | _ => React.null
  }

  let board =
    <Patternfly.Layout.Split hasGutter={true}>
      {columns
      ->Belt.Array.mapWithIndex((pos, column) =>
        <Patternfly.Layout.SplitItem key={column.name ++ string_of_int(pos)}>
          <Column index column />
        </Patternfly.Layout.SplitItem>
      )
      ->React.array}
    </Patternfly.Layout.Split>

  <MStack>
    <MStackItem> {editor} </MStackItem>
    <MStackItem>
      <span style={ReactDOM.Style.make(~overflowX="scroll", ~width="2000px", ())}> {board} </span>
    </MStackItem>
  </MStack>
}

let default = make
