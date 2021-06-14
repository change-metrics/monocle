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

  let addQuery = (columnQuery, query) => {
    let queryL = query->Js.String.toLowerCase
    let prefix =
      Js.String.startsWith("limit ", queryL) || Js.String.startsWith("order by ", queryL)
        ? " "
        : " and "
    columnQuery != "" ? "(" ++ columnQuery ++ ")" ++ prefix ++ query : query
  }

  @react.component
  let make = (~index, ~column, ~query: string) => {
    let (result, setResult) = React.useState(_ => None)
    let handleOk = (resp: WebApi.axiosResponse<SearchTypes.changes_query_response>) =>
      setResult(_ => resp.data->Some)->Js.Promise.resolve
    React.useEffect1(() => {
      switch column.query {
      | "" => ignore()
      | _ =>
        ignore(
          WebApi.Search.changesQuery({
            index: index,
            query: addQuery(column.query, query),
          }) |> Js.Promise.then_(handleOk),
        )
      }
      None
    }, [column.query ++ query])

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
                ->Belt.Array.map(change => <Change.DataItem key={change.url} change={change} />)
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

module ColumnEditor = {
  @react.component
  let make = (
    ~store: Store.t,
    ~pos: int,
    ~count: int,
    ~nameRef: ref<string>,
    ~queryRef: ref<string>,
    ~onRemove: int => unit,
  ) => {
    let (_, doRender) = React.useState(_ => "")
    let setAndRender = (r, v) => {
      r.contents = v
      doRender(_ => v)
    }
    let setName = (v, _) => setAndRender(nameRef, v)
    let setQuery = (v, _) => setAndRender(queryRef, v)

    <span style={ReactDOM.Style.make(~display="flex", ())}>
      <Patternfly.TextInput
        style={ReactDOM.Style.make(~width="200px", ())}
        id="col-name"
        value={nameRef.contents}
        onChange={setName}
        _type=#Text
      />
      <Search.Bar store value={queryRef.contents} setValue={v => setQuery(v, ())} />
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

  let saveToUrl = (board: t, query: string) => {
    resetLocationSearch()->ignore
    setLocationSearch("t", board.title)->ignore
    setLocationSearch("q", query)->ignore
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

  type action =
    AddColumn | RemoveColumn(int) | Save(string, string, array<(ref<string>, ref<string>)>)

  let reducer = (board: t, action: action) =>
    switch action {
    | AddColumn => {
        let newName = "Column #" ++ string_of_int(board.columns->Belt.List.length + 1)
        {...board, columns: board.columns->Belt.List.concat(list{Column.mk(newName)})}
      }
    | RemoveColumn(pos) => {
        ...board,
        columns: board.columns->Belt.List.keepWithIndex((_, index) => index != pos),
      }
    | Save(query, title, columnsRefs) =>
      {
        title: title,
        columns: columnsRefs
        ->Belt.Array.map(((nameRef, queryRef)) => {
          Column.name: nameRef.contents,
          query: queryRef.contents,
        })
        ->Belt.List.fromArray,
      }->saveToUrl(query)
    }

  let use = () => React.useReducer(reducer, loadFromUrl())

  module Editor = {
    @react.component
    let make = (~store: Store.t, ~board: t, ~columns: array<Column.t>, ~dispatch) => {
      let (state, _) = store
      let (showColumnEditor, setShowColumnEditor) = React.useState(_ => startWithEditorOpen)
      let (title, setTitle) = React.useState(_ => board.title)
      let columnsCount = columns->Belt.Array.length
      // We store a ref for each columns name and query, so that individual update doesn't
      // refresh the whole editor.
      let columnsRefs: array<(ref<string>, ref<string>)> =
        columns->Belt.Array.map(column => (ref(column.name), ref(column.query)))

      let doSave = () => Save(state.query, title, columnsRefs)->dispatch

      // When removing a middle column, the one after lose their state, so we need to save
      // first
      let onRemove = pos => {
        doSave()
        RemoveColumn(pos)->dispatch
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
                  doSave()
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
          <SearchToolTip store />
          <Patternfly.Button
            onClick={_ => {
              doSave()
              AddColumn->dispatch
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
                store
                pos
                nameRef
                queryRef
                onRemove
                count={columnsCount}
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
let make = (~store: Store.t) => {
  let (state, _) = store
  let index = state.index

  // Load from url and store the column state
  let (board, dispatch) = Board.use()
  // let (board, setBoard) = React.useState(Board.loadFromUrl)
  let columns = board.columns->Belt.List.toArray

  let editor = <Board.Editor store columns board dispatch />

  let board =
    <Patternfly.Layout.Split hasGutter={true}>
      {columns
      ->Belt.Array.mapWithIndex((pos, column) =>
        <Patternfly.Layout.SplitItem key={column.name ++ string_of_int(pos)}>
          <Column index column query={state.query} />
        </Patternfly.Layout.SplitItem>
      )
      ->React.array}
    </Patternfly.Layout.Split>

  <MStack>
    <MStackItem> <Search.Top store /> </MStackItem>
    <MStackItem>
      <Patternfly.Layout.Bullseye>
        <div style={ReactDOM.Style.make(~overflowX="width", ~width="1024px", ())}> {editor} </div>
      </Patternfly.Layout.Bullseye>
    </MStackItem>
    <MStackItem>
      <span style={ReactDOM.Style.make(~overflowX="scroll", ())}> {board} </span>
    </MStackItem>
  </MStack>
}

let default = make
