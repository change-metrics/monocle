Monocle Query Language
======================

This document introduces the Monocle Query Language standard.

Example queries:

- `state:open and review_count:0`
- `(repo : openstack/nova or repo: openstack/neutron) and user_group:qa and updated_at > 2020 order by score`


# Backus–Naur form

Machine readable specification of the grammar:

```abnf
; ABNF syntax based on RFC 5234
;
; notes:
; - `/`       means OR
; - `*`       repeats zero or more
; - `1*`      repeats one or more
; - `%xXX-YY` means byte range between 0xXX and 0xYY


; whitespace
whitespace-chunk = " " / "\t"
whsp = *whitespace-chunk

; nonempty whitespace
whsp1 = 1*whitespace-chunk

; Uppercase or lowercase ASCII letter
ascii = %x41-5A / %x61-7A
; ASCII digit
digit = %x30-39

; ASCII letter or "_"
label-char = ascii / "_"

; label-char or ASCII digit or extra
value-char = label-char / digit / "-" / "/" / "." / "*"

; Field and value
field = 1*label-char
value = 1*value-char

; boolean operator
and = "and" / "AND" / "∧" / "&&"
or  = "or"  / "OR"  / "∨" / "||"
not = "not" / "NOT" / "¬" / "!"

; field operator
eq = ":" / "=" / "=="
gt = ">"
ge = ">="
lt = "<"
le = "<="

; search operator
order-by = "order by" / "ORDER BY"
order-by-sort = "asc" / "ASC" / "desc" / "DESC"
limit = "limit" / "LIMIT"

expression =
    ; boolean operator
      expression whsp1 and whsp1 expression
    / expression whsp1 or whsp1 expression
    / not whsp1 expression

    ; field operator
    / field whsp eq whsp value
    / field whsp gt whsp value
    / field whsp ge whsp value
    / field whsp lt whsp value
    / field whsp le whsp value

    ; search operator
    / expression whsp1 order-by whsp1 field whsp1 order-by-sort
    / expression whsp1 order-by whsp1 field
    / expression whsp1 limit whsp1 1*digit

    ; priority operator
    / "(" whsp expression whsp ")"
```

# Expr data type

The language is parsed into a AST composed of a single recursive Expr data type defined as follow:

```haskell
type Field = Text
type Value = Text

data Expr =
  -- Bool operator
    AndExpr Expr Expr
  | OrExpr  Expr Expr
  | NotExpr Expr
  -- Field operator
  | EqExpr   Field Value
  | GtExpr   Field Value
  | LtExpr   Field Value
  | GtEqExpr Field Value
  | LtEqExpr Field Value
  -- Search operator
  | OrderByExpr Field Expr
  | LimitExpr   Int   Expr
```

Thus, the example queries are represented like this:

```haskell
example_query1 :: Expr
example_query1 =
  OrExpr
    (EqExpr "state" "open")
    (EqExpr "review_count" "0")

example_query2 :: Expr
example_query2 =
  OrderByExpr "score"
    (AndExpr
      (OrExpr
        (EqExpr "repo" "openstack/nova")
        (EqExpr "repo" "openstack/neutron"))
      (AndExpr
        (EqExpr "user_group" "qa")
        (GtExpr "updated_at" "2020")))
```

# Type check rules

The expression is validated using field specifications defined as follow:

```haskell
data FieldType = FieldDate | FieldNumber | FieldText

fields :: [(FieldType, [Field])]
fields = [
    (FieldDate,   ["updated_at", "created_at"])
  , (FieldNumber, ["pm_score", "review_count", "comment_count"])
  , (FieldText,   ["repo",    "repo_regexp"])
  , (FieldText,   ["user",    "user_regexp",    "user_group"])
  , (FieldText,   ["project", "project_regexp", "project_group"])
]
```

- DateField must follow one of these syntaxs: YYYY, YYYY-MM, YYYY-MM-DD
- Greater and Lower field operator must only be used with date or number fields.
- Search operator (OrderBy and Limit) may only appear once at the outer layer.

# Compilation to ElasticeSearch query

The expression may be transformed with such rules:

- `NotExpr (NotExpr e)`  -> `e`
- `NotExpr (EqExpr f v)` -> `f != v`
- `NotExpr (GtExpr f v)` -> `f <= v`
- `NotExpr (LtExpr f v)` -> `f >= v`
- `AndExpr e1 (AndExpr e2 e3)` -> `e1 && e2 && e3`
- `OrExpr (EqExpr f v1) (EqExpr f v2)` -> `f in [v1, v2]`

The operator are converted to ES query:

- And -> bool.must
- Or  -> bool.should
- Eq  -> regexp.{field}.value  (when field type is regexp)
- Eq  -> term.{field}
- Gt  -> range.{field}.gte
- Lt  -> range.{field}.lte

The field name and values are transformed accordingly by the API based on the internal document schema.
For example, the query field `repo_regexp` is converted to the document field `repository_fullname`.

The example query is converted to:

```json
{
  "bool": {
    "must": [
      {
        "bool": {
          "should": [
            {
              "term": {
                "repository_fullname": "openstack/nova"
              }
            },
            {
              "term": {
                "repository_fullname": "openstack/neutron"
              }
            }
          ]
        }
      },
      {
        "terms": {
          "author": ["qa-user-1", "qa-user-2"]
        }
      },
      {
        "range": {
          "update_at": {
            "gte": "2020"
          }
        }
      }
    ]
  }
}
```

# Extension

The language may be extended with the following features:

- `field in [x, xs..]`
- inline range, e.g.: `count:0..10`
- relative date, e.g.: `now-3weeks`
- unicode value
