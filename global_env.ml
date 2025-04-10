module StringMap = Map.Make(String)

let user_types = ref (StringMap.empty : string list StringMap.t)

