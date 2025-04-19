signature ERROR_FORMATTER =
sig
  val fmt: exn -> string -> string
end

structure PlainFmt: ERROR_FORMATTER =
struct
  fun fmt e source =
    "Unexpected exception " ^ exnName e ^ " raised at " ^ source
end
