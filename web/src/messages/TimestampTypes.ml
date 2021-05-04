[@@@ocaml.warning "-27-30-39"]


type timestamp = {
  seconds : int64;
  nanos : int32;
}

let rec default_timestamp 
  ?seconds:((seconds:int64) = 0L)
  ?nanos:((nanos:int32) = 0l)
  () : timestamp  = {
  seconds;
  nanos;
}
