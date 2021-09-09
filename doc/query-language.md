Monocle Query Language
======================

This document introduces the Monocle Query Language standard.

Example queries:

- `state:open and review_count:0`
- `(repo:openstack/nova or repo:openstack/neutron) and user_group:qa and updated_at>2020`


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

; Uppercase or lowercase ASCII letter or "_"
label-char = %x41-5A / %x61-7A / "_"

; Any char excluding paren, operator and double quote
; operators are skipped so that we can lex 'name:value' in three token
value-char =
                      ; 21 is '!' and 22 is '"'
      %x23-27         ; 28 is '(' and 29 is ')'
    / %x2A-39         ; 3A is ':'
    / %x3B            ; 3C is '<' and 3D is '=' and 3E is '>'
    / %x3F-10FFFF

; Any char excluding double quote
quoted-char = %x20-21 / %x23-10FFFF
quoted-value = %x22 1*quoted-char %x22

; Field and value
field = 1*label-char
value = quoted-value / 1*value-char

; boolean operator
and = "and" / "AND"
or  = "or"  / "OR"
not = "not" / "NOT"

; field operator
operator = ":" / ">" / ">=" / "<" / "<="

expression =
    ; boolean operator
      expression whsp1 and whsp1 expression
    / expression whsp1 or whsp1 expression
    / expression whsp1 expression ; implicit AND
    / not whsp1 expression
    / field operator value
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
- escape quote, e.g.: `"literal \"quote"`
