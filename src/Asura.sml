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
   * An assertion in the form of an exception
   * If a test's success is determined by some condition, use `raise Assert cond` at the end of the test
   * 
   * Tests that raise an `Assert true` will be considered a _success_
   *)
  exception Assert of bool

  (*!
   * Run a test.
   * Returns a function name if the function failed, otherwise NONE
   * All exceptions thrown by the test must be caught and handled here
   *)
  val run: test -> result

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
  exception Assert of bool

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
        if String.isSuffix "AsuraFn.assert" func then
          "<mlton bug...can't track>"
        else
          func

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
    handle
      Assert true => NONE
    | e => 
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
