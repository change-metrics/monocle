// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later

open Query

let shouldEqual = (name, got, expected) =>
  got == expected
    ? true
    : {
        Js.log3("Failure: ", name, ", got:")
        Js.log(got)
        Js.log("Wanted:")
        Js.log(expected)
        false
      }

let testParser = (code, expected) => {
  Js.log2("Parsing: ", code)
  let got = BsParse.Combinators.run(Query.Expr.exprParser, code) |> BsParse.Combinators.get_exn
  code->shouldEqual(got, expected)
}

let testPrinter = (code, expected) => {
  let got = Query.Expr.pretty(code)
  code->shouldEqual(got, expected)
}

let testUpdate = (code, field, value, expected) => {
  let got = Query.addOrUpdate(code, field, value)
  code->shouldEqual(got, expected)
}

let testAdd = (code, field, value, expected) => {
  let got = Query.addField(code, field, value)
  code->shouldEqual(got, expected)
}

open Expr
let props = list{
  () => testParser("toto:42", EqExpr("toto", "42")),
  () => testParser("sprint42", AliasExpr("sprint42")),
  () => testParser("not a:\"1 2\"", NotExpr(EqExpr("a", "1 2"))),
  () => testParser("a:4 and b:5", AndExpr(EqExpr("a", "4"), EqExpr("b", "5"))),
  () => testParser("a:4 AND b:5", AndExpr(EqExpr("a", "4"), EqExpr("b", "5"))),
  () => testParser("a:4     b:5", AndExpr(EqExpr("a", "4"), EqExpr("b", "5"))),
  () => testParser("(a:4)", EqExpr("a", "4")),
  () =>
    testParser(
      "(a:1 b:2) or c:3",
      OrExpr(AndExpr(EqExpr("a", "1"), EqExpr("b", "2")), EqExpr("c", "3")),
    ),
  () => testPrinter(NotExpr(EqExpr("a", "b")), "not a:b"),
  () => testPrinter(NotExpr(AndExpr(EqExpr("a", "b"), EqExpr("c", "d"))), "not (a:b c:d)"),
  () => testUpdate("(author:Jenkins project:rdo)", "author", "Zuul", "project:rdo author:Zuul"),
  () =>
    testAdd(
      "(author:Jenkins project:rdo)",
      "author",
      "Zuul",
      "(author:Jenkins or author:Zuul) project:rdo",
    ),
  () =>
    testAdd(
      "state:open and author:\"John Doe\"",
      "state",
      "merged",
      "(state:open or state:merged) author:\"John Doe\"",
    ),
}

Node.Process.exit(props->Belt.List.every(t => t()) ? 0 : 1)
