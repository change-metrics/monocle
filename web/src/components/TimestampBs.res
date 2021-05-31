// TODO: implement rfc3339 encoding
let decode_timestamp = (json: string) => Js.Date.fromString(json)->Some

let encode_timestamp = (v: TimestampTypes.timestamp) =>
  switch v {
  | Some(v) => Js.Date.toJSONUnsafe(v)
  | None => "1970-01-01T01:01:01Z"
  }
