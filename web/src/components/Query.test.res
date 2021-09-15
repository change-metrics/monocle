// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later

open Query

let testParser = (code, expected) => {
  Js.log2("Parsing: ", code)
  let expr = BsParse.Combinators.run(Query.Expr.exprParser, code) |> BsParse.Combinators.get_exn
  expr == expected
    ? true
    : {
        Js.log3("Failure: decode ", code, ", got:")
        Js.log(expr)
        Js.log("Wanted:")
        Js.log(expected)
        false
      }
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
}

Node.Process.exit(props->Belt.List.every(t => t()) ? 0 : 1)
