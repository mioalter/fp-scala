#Scala Project Template

###What Is Here
This project shows the general folder structure for a Scala project.

In the `ScalaTemplate` folder, there is a 
* `build.sbt` file. This is where you add dependencies
* `project/assembly.sbt`. `Assembly` is a plugin used for making fat `JAR`s
* code goes in `src/main/scala/`
* tests go in `src/test/scala/`

###How-Tos
To play with this project, at the command line, in the `ScalaTemplate` folder
* type `sbt`, this starts sbt and gives you a prompt
* type `run myname` to run the code with argument `myname`
* type `test` to run the tests
* type `assembly` to compile a fat `JAR`. 
  This will get saved into the `target/scala-2.11/` folder.
  To execute the `JAR` type `scala myjar.jar arguments`
* type `console` to start an interactive REPL in which all code in this project and its dependencies are importable
* type `exit` to leave.

SBT is [Scala's interactive build tool](http://www.scala-sbt.org/0.13/docs/index.html). 
It makes it incredibly easy to add dependencies, run tests, and compile code.

If you just want to try out some code and you don't have a full project going, you can always just start an interactive command-line session by typing `scala` at the command line.
In the REPL session, you can load code from a standalone scala file using `:load filename.scala`.

###Testing Your Code
You can write code to automatically verify your code in either of two ways: _Tests_ and _Properties_. 

To write tests, you choose a specific input to test, say what the output should be, and check that the result computed by your program and the expected result agree. This is very easy to do. The downside is that you will never test anything you don't think of. 

To write properties, you describe a property that your program should satisfy and say what type of input your program expects, then `ScalaCheck` automatically generates input of the correct type and checks that the property you described is satisifed _for all_ inputs generated. 

The advantage of properties is that `ScalaCheck` can generate some really gnarly inputs that you probably wouldn't think of yourself. An easy example is: you write a function of type `Int => Int` and it seems to do the right thing for the several examples you can think of, you write a property and it fails. You look at the input on which it fails and realize that you made some assumption that is not enforced by your code, like, that the input is always `> 0`. This is why properties are great.

Tests are much easier to think of and get started on than properties, so really, writing both is best.

###Adding Dependencies
Dependencies go under `libraryDepencencies` in `build.sbt`. Usually, if you find a new library you want to try out, on its Github page will be an example what to add to your `libraryDependencies` to import it. For example, [this library](https://github.com/jcazevedo/moultingyaml) has an `Installation` section in its README that shows you what to do.

###For reference: to start an interactive Spark session
You need Spark to be in your dependencies (which it isn't in this template). To add it you want to add 
`"org.apache.spark" %% "spark-core" % "1.6.1"` and `"org.apache.spark" %% "spark-mllib" % "1.6.1"` so you get something that looks like

```
libraryDependencies ++= Seq(
  ....
  , "org.apache.spark" %% "spark-core" % "1.6.1" 
  , "org.apache.spark" %% "spark-mllib" % "1.6.1" 
  ....
)
```

in your `build.sbt`.

* type `sbt` to start SBT
* type `console` to start the Scala REPL
* cut and paste the following into the REPL

```
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf

val conf = new SparkConf().setMaster("local[2]").setAppName("interactive")
val sc = new SparkContext(conf)
```

* type `val data = sc.textFile(inFile)` to load data from `inFile` into an `RDD[String]`.You can then parse these rows of strings into something more structured as you see fit.

###*Bonus*: Tutorial!
Check out `tutorial/tutorial.scala` for a quick intro to Scala.
At the command line, in the `tutorial` folder, you can play with it by typing
* `scala` to start a REPL
* `:load tutorial.scala` to load the code
* `import Tutorial._` to import all the functions and things in there

Also, see the _Tutorial Part Deux: Generic Programming in Scala_ in the same folder!
It is an introduction to some more advanced (but still basic!) ideas in Scala.