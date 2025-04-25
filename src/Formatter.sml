signature FORMATTER =
sig
  val fmt: Runner.result -> string
  val summarize: Runner.result list -> string
end

structure PlainFmt: FORMATTER =
struct
  fun quote s = "\"" ^ s ^ "\""
  
  fun fmt (Assert.Assert, source) =
    "Assertion failed at " ^ source
    | fmt (Assert.NEq (a, b), source) =
      "Expected " ^ quote a ^ " but found " ^ quote b ^ " at " ^ source
    | fmt (Assert.NEmpty s, source) =
      "Expected empty item but found " ^ quote s ^ " at " ^ source
    | fmt (e, source) =
      "Unexpected exception " ^ quote (exnName e) ^ " raised at " ^ source

  fun summarize results =
    let
      val total = length results
      val failed = map (fn (_, s) => s) results
    in
      String.concatWith "\n"
        [ "Total: " ^ (Int.toString total)
        , "Passed: " ^ (Int.toString (total - length failed))
        , "Failed: " ^ (Int.toString (length failed))
        , "Failing tests: " ^ (String.concatWith ", " failed)
        ]
    end
end
