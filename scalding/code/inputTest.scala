import com.twitter.scalding.{Job, Args, TextLine, Tsv, TypedPipe, TypedTsv}

class InputTest(args : Args) extends Job(args) {
  val input = TextLine(args("input"))
  val lines : TypedPipe[String] = TypedPipe.from(input)
  val output = args("output")
  
  //input
  //  .read
  lines
    .map{x : String => x.split("\\t")(0)}
    .write(TypedTsv[String](output))
}

