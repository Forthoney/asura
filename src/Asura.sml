(*!
 * Trace was malformed in some unexpected way
 *)
exception Trace of string list

functor AsuraFn(Fmt: ERROR_FORMATTER):
sig
  type test
  type result
  type summary

  (*!
   * Run a test.
   * Returns a function name if the function failed, otherwise NONE
   * All exceptions thrown by the test must be caught and handled here
   *)
  val run: test -> result

  val assert: bool -> unit

  val raises: (exn -> bool) -> test -> unit
  val raisesAny: test -> unit

  (*!
   * Turn a list of results
   *)
  val summarize: test list -> summary

  val toString: summary -> string

  (*!
   * Convenience function for running tests and printing their summary to stderr
   *)
  val printSummary: test list -> unit
end =
struct
  exception Assert

  fun assert true = ()
    | assert false = raise Assert

  fun raises cond f =
    let
      val raised = (f (); false) handle e => cond e
    in
      assert raised
    end

  val raisesAny = raises (fn _ => true)

  type test = unit -> unit
  type result = (exn * string) option
  type summary = {total: int, failed: string list}

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

      fun bugNotice func =
        if String.isSuffix "AsuraFn.assert" func then "<stack frame missing>"
        else func

      val rec loop =
        fn [] | [_] => NONE
         | ("<main>" :: rest) => loop rest
         | (fst :: snd :: rest) =>
          case
            Option.compose
              (rev o String.tokens (fn c => c = #".") o #func, parseTrace) fst
          of
            SOME ("run" :: _ :: _) =>
              Option.compose (bugNotice o #func, parseTrace) snd
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

  fun summarize tests =
    {total = length tests, failed = map #2 (List.mapPartial run tests)}

  fun toString {total, failed} =
    String.concatWith "\n"
      [ "Total: " ^ (Int.toString total)
      , "Passed: " ^ (Int.toString (total - length failed))
      , "Failed: " ^ (Int.toString (length failed))
      , "Failing tests: " ^ (String.concatWith ", " failed)
      ]

  fun eprintLn s =
    (TextIO.output (TextIO.stdErr, s ^ "\n"); TextIO.flushOut TextIO.stdErr)

  val printSummary = eprintLn o toString o summarize
end

structure Asura = AsuraFn(PlainFmt)
