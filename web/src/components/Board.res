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
    // TODO: stop doing text combinaison of columnQuery and globalQuery
    // instead, let the api combine both and correctly handle columnquery mod.
    //
    // In the meantime, if the global query is just a mod, e.g. `limit 5`, then append
    // it correctly:
    let queryL = query->Js.String.toLowerCase->Js.String.trim
    let prefix =
      Js.String.startsWith("limit ", queryL) || Js.String.startsWith("order by ", queryL)
        ? " "
        : " and "

    // If the column query contains a order by mod, then move it to the global query
    let queryModRe = %re("/order by .*$/")
    let queryMod =
      queryModRe
      ->Js.Re.exec_(columnQuery)
      ->Belt.Option.flatMap(res =>
        res->Js.Re.captures->Js.Array.unsafe_get(0)->Js.Nullable.toOption
      )
    let (columnQuery, query) = switch queryMod {
    | None
    | Some("") => (columnQuery, query)
    | Some(queryMod) => (Js.String.replace(queryMod, "", columnQuery), query ++ " " ++ queryMod)
    }

    columnQuery->Js.String.trim != "" ? "(" ++ columnQuery ++ ")" ++ prefix ++ query : query
  }

  module Row = {
    // TODO: merge common code with Column
    @react.component
    let make = (~index, ~column, ~query: string) => {
      let (result, setResult) = React.useState(_ => None)
      let handleOk = (resp: WebApi.axiosResponse<SearchTypes.query_response>) =>
        setResult(_ => resp.data->Some)->Js.Promise.resolve
      let query = addQuery(column.query, query)
      React.useEffect1(() => {
        switch query {
        | "" => ignore()
        | _ =>
          ignore(
            WebApi.Search.query({
              index: index,
              query: query,
            }) |> Js.Promise.then_(handleOk),
          )
        }
        None
      }, [query])
      switch result {
      | None => React.null
      | Some(SearchTypes.Error(err)) =>
        <Alert
          title={err.message ++ " at " ++ string_of_int(Int32.to_int(err.position))} variant=#Danger
        />
      | Some(SearchTypes.Items(items)) => {
          let changes = items.changes->Belt.List.toArray
          switch changes->Belt.Array.length {
          | 0 => <p> {"No changes matched"->str} </p>
          | _ =>
            changes
            ->Belt.Array.map(change => <Change.RowItem index key={change.url} change={change} />)
            ->React.array
          }
        }
      }
    }
  }

  @react.component
  let make = (~index, ~column, ~query: string) => {
    let (result, setResult) = React.useState(_ => None)
    let handleOk = (resp: WebApi.axiosResponse<SearchTypes.query_response>) =>
      setResult(_ => resp.data->Some)->Js.Promise.resolve
    let query = addQuery(column.query, query)
    React.useEffect1(() => {
      switch query {
      | "" => ignore()
      | _ =>
        ignore(
          WebApi.Search.query({
            index: index,
            query: query,
          }) |> Js.Promise.then_(handleOk),
        )
      }
      None
    }, [query])

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
                ->Belt.Array.map(change =>
                  <Change.DataItem index key={change.url} change={change} />
                )
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
    let (_, doRender) = React.useState(_ => 0)
    let setAndRender = (r, v) => {
      r.contents = v
      doRender(x => x + 1)
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
  type style = Kanban | Table
  type t = {title: string, columns: list<Column.t>, style: style}

  let defaultNegativeApprovals = "(approval:Workflow-1 or approval:Code-Review-1 or approval:Code-Review-2)"
  let defaultPositiveApprovals = "approval:Verified+1"

  let default = {
    title: "Reviewer Board",
    columns: list{
      {
        Column.name: "To Review",
        query: "state: open and updated_at < now-1week and updated_at > now-3week " ++
        defaultPositiveApprovals ++
        " and not " ++
        defaultNegativeApprovals ++ " order by created_at",
      },
      {
        Column.name: "To Approve",
        query: "state:open and updated_at > now-1week order by created_at desc",
      },
      {
        Column.name: "Done",
        query: "state:merged and updated_at > now-1week order by updated_at desc",
      },
      {
        Column.name: "Oldies",
        query: "state:open and updated_at > now-3week order by updated_at desc",
      },
    },
    style: Kanban,
  }

  let columnsArray = (board: t) => {
    let arr = board.columns->Belt.List.toArray
    (arr, arr->Belt.Array.length)
  }

  let saveToUrl = (board: t, query: string) => {
    resetLocationSearch()->ignore
    board.style == Table ? setLocationSearch("s", "table")->ignore : ignore()
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
    let style = switch getP("s") {
    | Some("table") => Table
    | _ => Kanban
    }
    switch (getP("t"), go(0)) {
    | (None, _)
    | (_, list{}) => default
    | (Some(title), columns) => {
        title: title,
        columns: columns,
        style: style,
      }
    }
  }

  type action =
    | AddColumn
    | RemoveColumn(int)
    | SetStyle(style)
    | Save(string, string, array<(ref<string>, ref<string>)>)

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
    | SetStyle(style) => {
        ...board,
        style: style,
      }
    | Save(query, title, columnsRefs) =>
      {
        style: board.style,
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

      let (currentStyle, setStyle) = React.useState(_ => board.style == Table)
      let toggleStyle = () => {
        setStyle(_ => board.style != Table)
        switch board.style {
        | Kanban => Table
        | Table => Kanban
        }
        ->SetStyle
        ->dispatch
      }

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
            ? <>
                <Patternfly.Checkbox
                  id="style"
                  description="table"
                  isChecked={currentStyle}
                  onChange={(_, _) => toggleStyle()}
                />
                <Patternfly.Button
                  _type=#Submit
                  onClick={_ => {
                    doSave()
                    setShowColumnEditor(_ => false)
                  }}>
                  {"Save"->str}
                </Patternfly.Button>
              </>
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

  let board = switch board.style {
  | Board.Kanban =>
    <Patternfly.Layout.Split hasGutter={true}>
      {columns
      ->Belt.Array.mapWithIndex((pos, column) =>
        <Patternfly.Layout.SplitItem key={column.name ++ string_of_int(pos)}>
          <Column index column query={state.query} />
        </Patternfly.Layout.SplitItem>
      )
      ->React.array}
    </Patternfly.Layout.Split>
  | Board.Table =>
    <table className="pf-c-table pf-m-compact pf-m-grid-md" role="grid">
      <Change.RowItem.Head />
      {columns
      ->Belt.Array.mapWithIndex((pos, column) =>
        <React.Fragment key={column.name ++ string_of_int(pos)}>
          <tbody>
            <tr>
              <td colSpan=42>
                {pos > 0 ? <br /> : React.null}
                <b> {column.name->str} </b>
                <i> {(" : " ++ column.query)->str} </i>
              </td>
            </tr>
            <Column.Row index column query={state.query} />
          </tbody>
        </React.Fragment>
      )
      ->React.array}
    </table>
  }

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
