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
}
