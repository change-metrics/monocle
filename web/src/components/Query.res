// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The search language parser for client side.
//
open BsParse.Combinators
open BsParse.CommonCombinators

// Infix operator functions:
let fmap: (parser<'a>, 'a => 'b) => parser<'b> = BsParse.Combinators.DP.map
let andThen: (
  parser<'a>,
  'a => parser<'b>,
) => parser<'b> = BsParse.Combinators.BasicCombinators.flatMap

module Expr = {
  type rec t =
    | AndExpr(t, t)
    | OrExpr(t, t)
    | NotExpr(t)
    | EqExpr(string, string)
    | GtExpr(string, string)
    | LtExpr(string, string)
    | GteExpr(string, string)
    | LteExpr(string, string)
    | AliasExpr(string)

  // Get the first result of a regexp parser
  let regex1: string => parser<string> = s => regex(s)->fmap(r => r[0])

  // A literal parser
  let litUnquoted = regex1("[^\"()<>=: ]+")
  let lit = litUnquoted->orElse(lazy str)

  // Field expression, `field` OP `value`
  let fieldExpr = (op, ctor) =>
    litUnquoted->andThen(field => regex1(op)->andThen(_ => lit->fmap(value => ctor(field, value))))
  let eqExpr: parser<t> = fieldExpr(":", (k, v) => EqExpr(k, v))
  let gtExpr: parser<t> = fieldExpr(">", (k, v) => GtExpr(k, v))
  let gteExpr: parser<t> = fieldExpr(">=", (k, v) => GteExpr(k, v))
  let ltExpr: parser<t> = fieldExpr("<", (k, v) => LtExpr(k, v))
  let lteExpr: parser<t> = fieldExpr("<=", (k, v) => LteExpr(k, v))
  let allFieldExpr =
    eqExpr->orElse(lazy gtExpr)->orElse(lazy gteExpr)->orElse(lazy ltExpr)->orElse(lazy lteExpr)

  // Not expr
  let notExpr = expr => regex1("(not|NOT)")->andThen(_ => Lazy.force(expr)->map(e => NotExpr(e)))

  // Paren expr
  let parenExpr = expr => surround(string("("), Lazy.force(expr), string(")"))

  // Alias expr
  let aExpr = litUnquoted->map(e => AliasExpr(e))

  // 'closedExpr' is a single expression, this is used for the left factoring technique.
  // See Monocle.Search.Parser.hs for details.
  let closedExpr = expr =>
    spaceAround(
      notExpr(expr)->orElse(lazy allFieldExpr)->orElse(lazy aExpr)->orElse(lazy parenExpr(expr)),
    )

  // Bool expressions
  let boolExpr = (sep, ctor, expr) =>
    closedExpr(expr)->andThen(e1 =>
      regex1(sep)->andThen(_ => Lazy.force(expr)->fmap(e2 => ctor(e1, e2)))
    )
  let andExpr = boolExpr("(and|AND)", (e1, e2) => AndExpr(e1, e2))
  let orExpr = boolExpr("(or|OR)", (e1, e2) => OrExpr(e1, e2))
  let implicitAndExpr = expr =>
    closedExpr(expr)->andThen(e1 => Lazy.force(expr)->fmap(e2 => AndExpr(e1, e2)))
  let allBoolExpr = expr =>
    andExpr(expr)->orElse(lazy orExpr(expr))->orElse(lazy implicitAndExpr(expr))

  // rescript being strict, we need to use this `let rec expr = lazy ...` trick to parse recursively.
  // this let sub expression such as bool expression call the main parser.
  let rec expr = lazy (allBoolExpr(expr)->orElse(lazy closedExpr(expr)))
  let exprParser: parser<t> = Lazy.force(expr)->andThen(e => eof->fmap(_ => e))

  let prettyValue = v => Js.String.indexOf(" ", v) >= 0 ? "\"" ++ v ++ "\"" : v

  // Pretty print an expression
  let rec pretty = expr => {
    // add paren around or expr
    let parenOr = expr =>
      switch expr {
      | OrExpr(_, _) => "(" ++ pretty(expr) ++ ")"
      | _ => pretty(expr)
      }

    // add paren around and expr
    let parenAnd = expr =>
      switch expr {
      | AndExpr(_, _) => "(" ++ pretty(expr) ++ ")"
      | _ => pretty(expr)
      }

    switch expr {
    | AndExpr(x, y) => parenOr(x) ++ " " ++ parenOr(y)
    | OrExpr(x, y) => parenAnd(x) ++ " or " ++ parenAnd(y)
    | NotExpr(x) =>
      switch x {
      | NotExpr(y) => pretty(y) // not (not y) == y
      | AndExpr(_, _)
      | OrExpr(_, _) =>
        "not (" ++ pretty(x) ++ ")"
      | _ => "not " ++ pretty(x)
      }

    | EqExpr(f, v) => f ++ ":" ++ prettyValue(v)
    | GtExpr(f, v) => f ++ ">" ++ prettyValue(v)
    | LtExpr(f, v) => f ++ "<" ++ prettyValue(v)
    | GteExpr(f, v) => f ++ ">=" ++ prettyValue(v)
    | LteExpr(f, v) => f ++ "<=" ++ prettyValue(v)
    | AliasExpr(x) => x
    }
  }

  // for some reason, we can't pass data constructor to function
  // so let's create helper function
  let andE = (x, y) => AndExpr(x, y)
  let orE = (x, y) => OrExpr(x, y)

  // Remove a field matching the predicate
  let dropField: (string => bool, t) => option<t> = (pred, expr) => {
    // recursively go
    let rec go = e => {
      // for boolean operator, we check both branch
      let tryBoth = (op, x, y) =>
        switch (go(x), go(y)) {
        | (Some(x), Some(y)) => Some(op(x, y)) // the field was not found, we can reconstruct the expr
        | (x, None) => x
        | (None, y) => y
        }

      // for field expr, we check if the field match the pred
      let unlessField = (field, x) => pred(field) ? None : Some(x)

      switch e {
      | AndExpr(x, y) => tryBoth(andE, x, y)
      | OrExpr(x, y) => tryBoth(orE, x, y)
      | NotExpr(x) =>
        switch go(x) {
        | None => None
        | Some(x) => Some(NotExpr(x))
        }
      | EqExpr(f, _) => unlessField(f, e)
      | GtExpr(f, _) => unlessField(f, e)
      | LtExpr(f, _) => unlessField(f, e)
      | GteExpr(f, _) => unlessField(f, e)
      | LteExpr(f, _) => unlessField(f, e)
      | AliasExpr(_) => Some(e)
      }
    }
    go(expr)
  }

  // Try adding a field using an OrExpr
  let addField: (string, string, t) => (t, bool) = (field, value, expr) => {
    // recursively go
    let rec go = e => {
      // for boolean oeprator, we try both branch
      let tryBoth = (op, x, y) => {
        let (x, addedX) = go(x)
        let (y, addedY) = go(y)
        (op(x, y), addedX || addedY)
      }
      switch e {
      | EqExpr(f, _) if f == field => (OrExpr(e, EqExpr(field, value)), true)
      | AndExpr(x, y) => tryBoth(andE, x, y)
      | OrExpr(x, y) => tryBoth(orE, x, y)
      | _ => (e, false)
      }
    }
    go(expr)
  }
}

let parse: string => option<Expr.t> = (code: string) =>
  switch run(Expr.exprParser, code) |> get_exn {
  | v => Some(v)
  | exception _ => None
  }

let addOrUpdate = (code: string, field: string, value: string) => {
  let newExpr = Expr.EqExpr(field, value)
  switch parse(code) {
  | None => {
      Js.log2("Could not parse", code)
      code ++ " " ++ Expr.pretty(newExpr)
    }
  | Some(expr) =>
    Expr.pretty(
      switch Expr.dropField(v => field == v, expr) {
      | None => newExpr
      | Some(expr) => Expr.AndExpr(expr, newExpr)
      },
    )
  }
}

let addField = (code: string, field: string, value: string) => {
  let newExpr = Expr.EqExpr(field, value)
  switch parse(code) {
  | None => {
      Js.log2("Could not parse", code)
      code ++ " " ++ Expr.pretty(newExpr)
    }
  | Some(expr) =>
    Expr.pretty(
      switch Expr.addField(field, value, expr) {
      | (expr, true) => expr
      | (expr, false) => Expr.AndExpr(expr, newExpr)
      },
    )
  }
}
