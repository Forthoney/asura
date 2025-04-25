signature SUITE =
sig
  val desc: string
  val tests: Assert.test list
end

structure Runner:
sig
  exception Trace of string list
  type result
  type summary
  (*!
   * Run a test.
   * Returns a function name if the function failed, otherwise NONE
   * All exceptions thrown by the test must be caught and handled here
   *)
  val run: Assert.test -> result option
end =
struct
  exception Trace of string list
  (*!
   * Trace was malformed in some unexpected way
   *)
  type result = exn * string
  type summary = {total: int, failed: result list}

  fun findSource traces =
    let
      fun parseTrace trace =
        case String.tokens (fn c => c = #" ") trace of
          [func, file, lineCol] =>
            (case map Int.fromString (String.tokens (fn c => c = #".") lineCol) of
               [SOME line, SOME col] =>
                 SOME {func = func, file = file, line = line, col = col}
             | _ => NONE)
        | _ => NONE

      fun toString {func, file, line, col} =
        if func = "fn" then
          "<anonymous fn at " ^ file ^ " line: " ^ (Int.toString line) ^ ", col: " ^ (Int.toString col)
        else
          func

      fun bugNotice func =
        if String.isSuffix "AsuraFn.assert" func then NONE
        else SOME func

      val rec loop =
        fn [] | [_] => NONE
         | ("<main>" :: rest) => loop rest
         | (fst :: snd :: rest) =>
          case
            Option.compose
              (rev o String.tokens (fn c => c = #".") o #func, parseTrace) fst
          of
            SOME ("run" :: _ :: _) =>
              Option.composePartial (bugNotice o toString, parseTrace) snd
          | SOME _ => loop (snd :: rest)
          | NONE => NONE
    in
      loop (rev traces)
    end

  fun run test =
    (test (); NONE)
    handle e =>
      let
        val traces = MLton.Exn.history e
      in
        case findSource traces of
          SOME src => SOME (e, src)
        | NONE => raise Trace traces
      end
end
