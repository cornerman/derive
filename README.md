# derive

derive implementations. inspired by rusts `#[derive]` and scalas case classes.

# usage

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
```

# derivables

* toString
* unapply
* hashCode
* equals
* canEqual
* copy
* apply
* productPrefix
* productIterator
* productArity
* productElement
* Product: ProductPrefix + ProductIterator + ProductArity + ProductElement (extends Product)
* Equality: equals + canEqual + hashCode
* Factory: apply + unapply 
* Case: toString + copy + Equality + Factory
