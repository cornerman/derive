# derive

derive implementations. inspired by rusts `#[derive]` and scalas case classes.

# usage

Dependency in build.sbt:
```
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "com.github.cornerman" %% "derive" % "0.1.0-SNAPSHOT"

addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M8" cross CrossVersion.full)
```

# example

```
import derive.derive

@derive(toString) // same as @derive((x, y) => toString)
class Position(val x: Int, val y: Int)

val pos = new Position(1, 2)
println(pos) // Position(1, 2)


@derive(name => Equality, toString)
class Person(val name: String, val age: Int)

val person = new Person("hans", 52)
println(person) // Person("hans", 52)
assert(person == new Person("hans", 49))
assert(person != new Person("gisela", 52))


@derive(Case) // derive same methods as case class
class Data(val a: Int, val b: Int)
```

# derivables

* toString
* unapply
* hashCode
* equals: canEqual
* canEqual
* copy
* copyF
* apply
* productPrefix
* productIterator: Product
* productArity
* productElement
* Product: (extends Product) + ProductPrefix + ProductIterator + ProductArity + ProductElement + canEqual
* Equality: equals + hashCode
* Factory: apply + unapply
* Case: toString + copy + Product + Equality + Factory
