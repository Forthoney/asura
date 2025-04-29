structure Assert =
struct
  type test = unit -> unit

  exception Assert
  exception NEq of string * string
  exception NEmpty of string

  fun assert true = ()
    | assert false = raise Assert

  fun eq expected actual =
    if expected = actual then () else raise Assert

  fun eq' toString expected actual =
    if expected = actual then ()
    else raise NEq (toString expected, toString actual)

  val eqInt = eq' Int.toString
  val eqStr = eq' String.toString
  fun eqSubstr a b =
    eqStr (Substring.string a) (Substring.string b)
  fun eqSubstr' a b =
    eqStr a (Substring.string b)

  fun isEmptyStr s =
    if String.size s = 0 then
      ()
    else
      raise NEmpty (String.toString s)

  fun isEmptySubstr s =
    if Substring.isEmpty s then
      ()
    else
      raise (NEmpty o String.toString o Substring.string) s

  structure List =
  struct
    fun isEqLength a b = eqInt (length a) (length b)

    fun eq expected actual =
      if ListPair.allEq op= (expected, actual) then ()
      else raise Assert
  
    fun eq' toString expected actual =
      let
        fun listFmt toString xs =
          "[" ^ String.concatWith ", " (map toString xs) ^ "]"
      in
        if ListPair.allEq op= (expected, actual) then ()
        else raise NEq (listFmt toString expected, listFmt toString actual)
      end

    val eqInt = eq' Int.toString
    val eqStr = eq' String.toString
    fun eqSubstr a b = eqStr (map Substring.string a) (map Substring.string b)
    fun eqSubstr' a b = eqStr (map Substring.string a) (map Substring.string b)
  end


  fun raises cond f =
    let val raised = (f (); false) handle e => cond e
    in assert raised
    end

  val raisesAny = raises (fn _ => true)
end

signature SUITE =
sig
  val tests: Assert.test list
end
