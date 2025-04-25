val assert = Asura.Assert.assert
val run = Asura.Runner.run

fun triviallyTrue () = assert true

fun triviallyFalse () = assert false

fun wrapOverTriviallyFalse () = triviallyFalse ()

fun arithmetic () =
  assert (1 + 1 = 2)

fun arithmeticWrong () =
  assert (1 + 1 = 3)

fun raising () = List.hd []

fun unwrapGood () =
  Option.valOf (SOME ())
fun unwrap () = Option.valOf NONE

fun nestedAssertGood () =
  let
    fun foo n = n + 1
    fun bar n =
      assert (n = 2)
  in
    bar (foo 1)
  end

fun nestedAssertBad () =
  let
    fun bar n = assert false
  in
    bar 1
  end

fun badCheck (test, name) =
  (case run test of
     SOME (_, v) =>
       if v <> name then (print (v ^ " neq " ^ name ^ "\n"); false) else true
   | NONE => (print (name ^ " failed\n"); false))
  handle (Asura.Runner.Trace traces) =>
    ( print
        (String.concatWith "\n"
           ("==="::(name ^ " failed with the following traces") :: traces @ ["==="]) ^ "\n")
    ; false
    )

fun plain () =
  let
    val good =
      List.filter (Option.isSome o run o #1)
        [ (triviallyTrue, "triviallyTrue")
        , (arithmetic, "arithmetic")
        , (unwrapGood, "unwrapGood")
        ]
    val bad =
      List.map badCheck
        [ (triviallyFalse, "triviallyFalse")
        , (wrapOverTriviallyFalse, "wrapOverTriviallyFalse")
        , (arithmeticWrong, "arithmeticWrong")
        , (raising, "raising")
        , (unwrap, "unwrap")
        , (fn () => assert false, "<anonymous>")
        ]

    val _ = print (List.foldl (fn ((_, s), acc) => s ^ "\n" ^ acc) "" good)
  in
    ()
  end

val _ = plain ()

structure Wrapper =
struct
  fun triviallyTrue () = assert true

  fun triviallyFalse () = assert false

  fun arithmetic () =
    assert (1 + 1 = 2)

  fun arithmeticWrong () =
    assert (1 + 1 = 3)

  fun raising () =
    assert (List.hd [])

  fun unwrapGood () =
    Option.valOf (SOME ())

  fun unwrap () = Option.valOf NONE
end

fun structureNested () =
  let
    val good = [Wrapper.triviallyTrue, Wrapper.arithmetic, Wrapper.unwrapGood]

    val bad =
      [ (Wrapper.triviallyFalse, "Wrapper.triviallyFalse")
      , (Wrapper.arithmeticWrong, "Wrapper.arithmeticWrong")
      , (Wrapper.raising, "Wrapper.raising")
      , (Wrapper.unwrap, "Wrapper.unwrap")
      ]

    val _ =
      if List.all (not o Option.isSome o run) good then ()
      else print "good structure failed\n"
    val _ = if List.all badCheck bad then () else print "bad structure failed\n"
  in
    ()
  end

val _ = structureNested ()
