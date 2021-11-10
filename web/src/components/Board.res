// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The board view
//
open Prelude

// Set to true during development
let startWithEditorOpen = false

module Column = {
  type t = {name: string, query: string, order: option<SearchTypes.order>}

  let noChangeFound = <tr role="row"> <td role="cell"> <p> {"No change found"->str} </p> </td> </tr>

  module Row = {
    module RView = {
      @react.component
      let make = (~store: Store.t, ~changesAll: array<SearchTypes.change>) => {
        let (state, _) = store
        let (changes, dispatchChange) = HiddenChanges.use(state.dexie, changesAll)
        switch changes->Belt.Array.length {
        | 0 => noChangeFound
        | _ =>
          changes
          ->Belt.Array.map(((status, change)) =>
            <Change.RowItem store key={change.url} change status dispatchChange />
          )
          ->React.array
        }
      }
    }

    // TODO: merge common code with Column
    @react.component
    let make = (~store: Store.t, ~column) => {
      let (state, _dispatch) = store
      let (result, setResult) = React.useState(_ => None)
      let handleOk = (resp: WebApi.axiosResponse<SearchTypes.query_response>) =>
        setResult(_ => resp.data->Some)->Js.Promise.resolve
      let query = addQuery(column.query, state.query)
      React.useEffect1(() => {
        switch query {
        | "" => ignore()
        | _ =>
          ignore(
            WebApi.Search.query({
              ...Store.mkSearchRequest(state, SearchTypes.Query_change),
              query: query,
              order: column.order,
            }) |> Js.Promise.then_(handleOk),
          )
        }
        None
      }, [query, column.order->orderToQS, state.limit->string_of_int])
      switch result {
      | None => React.null
      | Some(SearchTypes.Error(err)) =>
        <Alert
          title={err.message ++ " at " ++ string_of_int(Int32.to_int(err.position))} variant=#Danger
        />
      | Some(SearchTypes.Changes(items)) =>
        <RView store changesAll={items.changes->Belt.List.toArray} />
      | Some(_) => React.null
      }
    }
  }

  module CView = {
    @react.component
    let make = (~store: Store.t, ~changesAll: array<SearchTypes.change>) => {
      let (state, _) = store
      let (changes, dispatchChange) = HiddenChanges.use(state.dexie, changesAll)
      switch changes->Belt.Array.length {
      | 0 => noChangeFound
      | _ =>
        <Patternfly.DataList isCompact={true}>
          {changes
          ->Belt.Array.map(((status, change)) =>
            <Change.DataItem store key={change.url} change status dispatchChange />
          )
          ->React.array}
        </Patternfly.DataList>
      }
    }
  }

  @react.component
  let make = (~store: Store.t, ~column) => {
    let (state, _) = store
    let (result, setResult) = React.useState(_ => None)
    let handleOk = (resp: WebApi.axiosResponse<SearchTypes.query_response>) =>
      setResult(_ => resp.data->Some)->Js.Promise.resolve
    let query = addQuery(column.query, state.query)
    React.useEffect1(() => {
      switch query {
      | "" => ignore()
      | _ =>
        ignore(
          WebApi.Search.query({
            ...Store.mkSearchRequest(state, SearchTypes.Query_change),
            query: query,
            order: column.order,
          }) |> Js.Promise.then_(handleOk),
        )
      }
      None
    }, [query, column.order->orderToQS, state.limit->string_of_int])

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
        | Some(SearchTypes.Changes(items)) =>
          <CView store changesAll={items.changes->Belt.List.toArray} />
        | Some(_) => React.null
        }}
      </Patternfly.CardBody>
    </Patternfly.Card>
  }

  let mk = str => {name: str, query: "", order: None}
}

module ColumnEditor = {
  @react.component
  let make = (
    ~store: Store.t,
    ~pos: int,
    ~count: int,
    ~nameRef: ref<string>,
    ~queryRef: ref<string>,
    ~orderRef: ref<option<SearchTypes.order>>,
    ~onRemove: int => unit,
  ) => {
    let (_, doRender) = React.useState(_ => 0)
    let setAndRender = (r, v) => {
      r.contents = v
      doRender(x => x + 1)
    }
    let setName = (v, _) => setAndRender(nameRef, v)
    let setQuery = (v, _) => setAndRender(queryRef, v)
    let setOrder = (v, _) => setAndRender(orderRef, v)

    <MGrid>
      <MGridItemXl1>
        <Patternfly.TextInput
          id="col-name" value={nameRef.contents} onChange={setName} _type=#Text
        />
      </MGridItemXl1>
      <MGridItemXl6>
        <Search.Bar
          store
          value={queryRef.contents}
          showTooltips={false}
          setValue={v => setQuery(v, ())}
          onSave={v => setQuery(v, ())}
          error={None} /* TODO: handle per column query errors */
        />
      </MGridItemXl6>
      <MGridItemXl3>
        <Search.Order value={orderRef.contents} setValue={v => setOrder(v, ())} />
      </MGridItemXl3>
      <MGridItemXl1>
        {maybeRender(
          count > 1,
          <Patternfly.Button variant=#Danger onClick={_ => onRemove(pos)}>
            {"Remove"->str}
          </Patternfly.Button>,
        )}
      </MGridItemXl1>
    </MGrid>
  }
}

module Board = {
  type style = Kanban | Table
  type t = {title: string, columns: list<Column.t>, style: style}

  let defaultNegativeApprovals = "(approval:CHANGE_REQUESTED or approval:Verified-1 or approval:Workflow-1 or approval:Code-Review-1 or approval:Code-Review-2)"
  let defaultPositiveApprovals = "(approval:APPROVED or approval:Code-Review+1 or approval:Code-Review+2)"

  let mkOrder = (field, dirM) =>
    {
      SearchTypes.field: field,
      direction: dirM->Belt.Option.getWithDefault(SearchTypes.Asc),
    }->Some

  let default = {
    title: "Reviewer Board",
    columns: list{
      {
        Column.name: "To Review",
        order: mkOrder("created_at", None),
        query: "state:open and updated_at>now-3week " ++ " and not " ++ defaultNegativeApprovals,
      },
      {
        Column.name: "To Approve",
        order: mkOrder("created_at", None),
        query: "state:open and updated_at>now-3week " ++
        "and not " ++
        defaultNegativeApprovals ++
        "and " ++
        defaultPositiveApprovals,
      },
      {
        Column.name: "Oldies",
        order: mkOrder("updated_at", Desc->Some),
        query: "state:open and updated_at<now-3week",
      },
      {
        Column.name: "Done",
        order: mkOrder("updated_at", Desc->Some),
        query: "state:merged and updated_at>now-1week",
      },
    },
    style: Table,
  }

  let columnsArray = (board: t) => {
    let arr = board.columns->Belt.List.toArray
    (arr, arr->Belt.Array.length)
  }

  let orderToQS = (order: SearchTypes.order) =>
    order.field ++
    "#" ++
    switch order.direction {
    | Asc => "A"
    | Desc => "D"
    }

  let saveToUrl = (board: t, query: string) => {
    resetLocationSearch()->ignore
    board.style == Kanban ? setLocationSearch("s", "kanban")->ignore : ignore()
    setLocationSearch("t", board.title)->ignore
    setLocationSearch("q", query)->ignore
    Belt.List.mapWithIndex(board.columns, (index, column) => {
      let posStr = string_of_int(index)
      setLocationSearch("n" ++ posStr, column.name)->ignore
      setLocationSearch("q" ++ posStr, column.query)->ignore
      switch column.order {
      | None => ignore()
      | Some(order) => setLocationSearch("o" ++ posStr, order->orderToQS)->ignore
      }
    })->ignore
    board
  }

  let loadFromUrl: unit => t = () => {
    let params = URLSearchParams.current()
    let getP = name => params->URLSearchParams.get(name)->Js.Nullable.toOption
    let rec go = pos => {
      let posStr = string_of_int(pos)
      switch (getP("n" ++ posStr), getP("q" ++ posStr), getP("o" ++ posStr)) {
      | (Some(name), Some(query), orderM) =>
        go(pos + 1)->Belt.List.add({
          Column.name: name,
          query: query,
          order: orderM->Belt.Option.flatMap(orderFromQS),
        })
      | _ => list{}
      }
    }
    let style = switch getP("s") {
    | Some("kanban") => Kanban
    | _ => Table
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
    | Save(string, string, array<(ref<string>, ref<string>, ref<option<SearchTypes.order>>)>)

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
        ->Belt.Array.map(((nameRef, queryRef, orderRef)) => {
          Column.name: nameRef.contents,
          query: queryRef.contents,
          order: orderRef.contents,
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
      let columnsRefs: array<(ref<string>, ref<string>, ref<option<SearchTypes.order>>)> =
        columns->Belt.Array.map(column => (ref(column.name), ref(column.query), ref(column.order)))

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
        <MGrid>
          <MGridItemXl6>
            <Patternfly.TextInput
              id="board-name"
              isDisabled={!showColumnEditor}
              value={title}
              onChange={(v, _) => setTitle(_ => v)}
              _type=#Text
            />
          </MGridItemXl6>
          <MGridItemXl3>
            <Patternfly.Button
              isDisabled={!showColumnEditor} variant=#Tertiary onClick={_ => toggleStyle()}>
              {showColumnEditor
                ? "Switch board style"->str
                : ("Board mode: " ++ {
                    currentStyle ? "Row" : "Kanban"
                  })->str}
            </Patternfly.Button>
          </MGridItemXl3>
          {showColumnEditor
            ? <MGridItemXl3>
                <Patternfly.Button
                  _type=#Submit
                  onClick={_ => {
                    doSave()
                    setShowColumnEditor(_ => false)
                  }}>
                  {"Save"->str}
                </Patternfly.Button>
              </MGridItemXl3>
            : <MGridItemXl3>
                <Patternfly.Button variant=#Tertiary onClick={_ => setShowColumnEditor(_ => true)}>
                  {"Edit"->str}
                </Patternfly.Button>
              </MGridItemXl3>}
        </MGrid>

      let bottomRow =
        <Patternfly.Button
          onClick={_ => {
            doSave()
            AddColumn->dispatch
          }}>
          {switch board.style {
          | Kanban => "Add Column"->str
          | Table => "Add Row"->str
          }}
        </Patternfly.Button>

      let columnsEditor = showColumnEditor
        ? <MStack>
            <MStackItem>
              {columnsRefs
              ->Belt.Array.mapWithIndex((pos, (nameRef, queryRef, orderRef)) =>
                <ColumnEditor
                  key={nameRef.contents ++ string_of_int(pos)}
                  store
                  pos
                  nameRef
                  queryRef
                  orderRef
                  onRemove
                  count={columnsCount}
                />
              )
              ->React.array}
            </MStackItem>
            <MStackItem> {bottomRow} </MStackItem>
          </MStack>
        : React.null

      <MCenteredContent>
        <MStack>
          <MStackItem>
            <Patternfly.Layout.Bullseye> {topRow} </Patternfly.Layout.Bullseye>
          </MStackItem>
          <MStackItem> {columnsEditor} </MStackItem>
        </MStack>
      </MCenteredContent>
    }
  }
}

@react.component
let make = (~store: Store.t) => {
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
          <Column store column />
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
            <Column.Row store column />
          </tbody>
        </React.Fragment>
      )
      ->React.array}
    </table>
  }

  <MStack>
    <MStackItem> {editor} </MStackItem>
    <MStackItem>
      <span style={ReactDOM.Style.make(~overflowX="scroll", ())}> {board} </span>
    </MStackItem>
  </MStack>
}

let default = make
