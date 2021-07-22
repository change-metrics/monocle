// An helper function to convert int64 timestamp to javascript date
let default = (ts: int64) => ts->Int64.to_float->Js.Date.fromFloat
