functor CustomAutoRunner(structure F: FORMATTER
                     structure S: SUITE): sig end =
struct
  val _ =
    let
      val run = Option.compose (fn res => (print (F.fmt res ^ "\n"); res), Runner.run)
      val results = List.mapPartial run S.tests
    in
      print (F.summarize results ^ "\n")
    end
end

functor AutoRunner(S: SUITE): sig end =
struct
  structure ST = CustomAutoRunner(structure F = PlainFmt
  structure S = S)
end
