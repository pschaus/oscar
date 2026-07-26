# OscaR — Scala in OR

[![CI & Code Coverage](https://github.com/pschaus/oscar/actions/workflows/ci.yml/badge.svg)](https://github.com/pschaus/oscar/actions/workflows/ci.yml)
[![Test Coverage](https://raw.githubusercontent.com/pschaus/oscar/gh-pages/badges/coverbadge.svg)](https://pschaus.github.io/oscar/coverage/index.html)

> **Important Note:** OscaR is maintained by Pierre Schaus, but its development is no longer active. Pierre Schaus is currently actively working on **[MaxiCP](https://github.com/aia-uclouvain/maxicp)**. If you find that any OscaR-CP feature you need is missing, please feel free to contact Pierre Schaus to request or discuss developing it in MaxiCP.

**OscaR** (Operations Research in Scala) is a Constraint Programming (CP) solver written in Scala 3.
It provides a rich set of global constraints, powerful search mechanisms, and Large Neighborhood Search (LNS) support.

---

## Table of Contents

- [Getting Started](#getting-started)
- [Building the Project](#building-the-project)
- [OscaR-CP Tutorial](#oscar-cp-tutorial)
  - [Create a Basic Model](#create-a-basic-model)
  - [Add an Objective Function](#add-an-objective-function)
  - [Predefined Searches](#predefined-searches)
  - [Search Limits](#search-limits)
  - [Custom Search](#custom-search)
  - [Constraint Filtering Levels](#constraint-filtering-levels)
  - [Arithmetic & Logical Constraints](#arithmetic--logical-constraints)
  - [Reified Constraints](#reified-constraints)
  - [Element Constraints](#element-constraints)
  - [Global Constraints](#global-constraints)
  - [Implement Your Own Constraint](#implement-your-own-constraint)
  - [Large Neighborhood Search (LNS)](#large-neighborhood-search-lns)
- [Examples](#examples)
- [Authors & Acknowledgments](#authors--acknowledgments)
- [Citing OscaR](#citing-oscar)

---

## Getting Started

OscaR is written specifically for **Scala 3** (3.3.x) and is published on **[Maven Central](https://central.sonatype.com/artifact/io.github.pschaus/oscar-cp_3/overview)**.

**Prerequisites:**
- Java 11+ (JDK 17/21 recommended)
- sbt 1.9+ or Maven 3.8+
- Scala 3

**sbt Dependency Coordinates (Scala 3):**
```scala
libraryDependencies += "io.github.pschaus" % "oscar-cp_3" % "4.0.0"
// or using %% (sbt automatically appends _3 for Scala 3):
libraryDependencies += "io.github.pschaus" %% "oscar-cp" % "4.0.0"
```

**Maven Dependency Coordinates:**
```xml
<dependency>
    <groupId>io.github.pschaus</groupId>
    <artifactId>oscar-cp_3</artifactId>
    <version>4.0.0</version>
</dependency>
```

**Clone from Source:**
```bash
git clone https://github.com/pschaus/oscar.git
cd oscar
```

---

## Building the Project

```bash
# Compile the main sources
sbt compile

# Compile and run all tests
sbt test

# Compile test sources only
sbt test:compile
```

---

## OscaR-CP Tutorial

This section is adapted from the OscaR user guide. It covers the main features of the CP solver, from beginner to advanced level.

**Skill levels used below:**
- **L1** — Beginning CP Modeler
- **L2** — Advanced CP Modeler
- **L3** — Expert CP Modeler

---

### Create a Basic Model

> **L1** — Declare a solver, create variables, and add constraints.

See [`BasicModel.scala`](src/main/scala/oscar/cp/examples/userguide/BasicModel.scala) for a complete runnable example.

A typical OscaR-CP model mixes in the `CPModel` trait which implicitly defines a `CPSolver` for you, letting you:
- Create all variables (attached to the solver), and
- Add every constraint of the problem you want to solve.

```scala
import oscar.cp._

object MyModel extends CPModel with App {
  val x1 = CPIntVar(Set(1, 2, 3))
  val x2 = CPIntVar(0 to 5)
  val x3 = CPBoolVar()

  add(x1 + x2 > 3)
  add(x3 ==> (x1 === 2))

  search { binaryFirstFail(Array(x1, x2, x3)) }
  onSolution { println(s"x1=$x1") }

  val stats = start(nSols = 1)
}
```

Each call to `add` triggers the fix-point propagation algorithm immediately, so you can observe propagation effects interactively (e.g. with `println`).

---

### Add an Objective Function

> **L1** — Maximize or minimize an objective variable.

See [`BasicOptimizationModel.scala`](src/main/scala/oscar/cp/examples/userguide/BasicOptimizationModel.scala).

`start()` uses branch-and-bound DFS. The `onSolution` block is called each time an improved solution is found. The last solution found is the proven optimum.

---

### Predefined Searches

> **L1** — Built-in search heuristics.

See [`PredefinedSearches.scala`](src/main/scala/oscar/cp/examples/userguide/PredefinedSearches.scala).

OscaR provides several ready-to-use branching strategies for a variable array `X`:

| Strategy | Description |
|---|---|
| `binaryFirstFail(X)` | Binary search, minimum domain size first |
| `binaryMaxDegree(X)` | Binary search, maximum constraint degree first |
| `binaryStatic(X)` | Binary search, static variable ordering |
| `conflictOrderingSearch(X, ...)` | Recommended for scheduling problems |

> **Tip:** When unsure, use `conflictOrderingSearch` — it performs well across many problem types.

---

### Search Limits

> **L1** — Stop the search early.

See [`SearchLimit.scala`](src/main/scala/oscar/cp/examples/userguide/SearchLimit.scala).

The `start` method accepts limits:

```scala
start(nSols = 10)              // stop after 10 solutions
start(failureLimit = 1000)     // stop after 1000 backtracks
start(timeLimit = 4)           // stop after 4 seconds
```

---

### Custom Search

> **L2** — Define your own branching strategy.

#### Binary Search

See [`BinarySearch.scala`](src/main/scala/oscar/cp/examples/userguide/BinarySearch.scala).

```scala
search {
  X.find(!_.isBound) match {
    case None    => noAlternative
    case Some(x) => branch { add(x === 0) } { add(x !== 0) }
  }
}
```

#### N-ary Search

See [`NarySearch.scala`](src/main/scala/oscar/cp/examples/userguide/NarySearch.scala).

```scala
search {
  X.find(!_.isBound) match {
    case None    => noAlternative
    case Some(x) => branchAll(x.min to x.max) { v => add(x === v) }
  }
}
```

#### Custom First-Fail

See [`FirstFailSearch.scala`](src/main/scala/oscar/cp/examples/userguide/FirstFailSearch.scala).

The **first-fail** principle selects the unbound variable with the smallest domain, then tries values in ascending order.

---

### Constraint Filtering Levels

> **L2** — Control the strength of propagation.

When adding a constraint you can specify a filtering level:

```scala
add(someConstraint, CPPropagStrength.Weak)
add(someConstraint, CPPropagStrength.Strong)  // default: Medium
```

| Level | Typical meaning |
|---|---|
| `Weak` | Cheap, lightweight filtering |
| `Medium` | Default — balance of speed and pruning |
| `Strong` | Full domain / arc consistency (can be expensive) |

Use `add` (throws `NoSolutionException` on failure) vs `post` (returns outcome, check `isFailed`):

```scala
add(someConstraint)                           // may throw NoSolutionException
val failed = post(someConstraint) == Failure  // or check solver.isFailed
```

---

### Arithmetic & Logical Constraints

> **L1** — Operators and summation constructs.

See [`ArithmeticConstraints.scala`](src/main/scala/oscar/cp/examples/userguide/ArithmeticConstraints.scala) and [`Logical.scala`](src/main/scala/oscar/cp/examples/userguide/Logical.scala).

**Arithmetic:** OscaR supports `+`, `-`, `*`, negation (`-x`), absolute value (`x.abs`), and summation:

```scala
val s = sum(X)          // sum of all variables in X
val s2 = sum(0 until n, 0 until m)((i, j) => X(i)(j))
```

**Logical:** `||`, `&&`, `==>` on `CPBoolVar`. Results are fresh boolean variables:

```scala
add(b1 || b2)
add(b1 ==> (x === 3))
```

---

### Reified Constraints

> **L1** — Turn constraint satisfaction into a boolean variable.

See [`Reified.scala`](src/main/scala/oscar/cp/examples/userguide/Reified.scala).

Reified operators return `CPBoolVar`:

| Operator | Meaning |
|---|---|
| `x >== v` | `true` iff `x >= v` |
| `x <<= v` | `true` iff `x < v` |
| `x === v` | `true` iff `x == v` |

Example — count variables in [2..4):
```scala
val count = sum(X.map(xi => (xi >== 2) && (xi <<= 4)))
```

---

### Element Constraints

> **L1** — Index arrays with variables.

See [`Element.scala`](src/main/scala/oscar/cp/examples/userguide/Element.scala).

```scala
val y = CPIntVar(0 to 4)
val result = arrayOfInts(y)   // CPIntVar = arrayOfInts[y]

// 2D indexing
val z = matrix(y1)(y2)        // CPIntVar
```

---

### Global Constraints

> **L1-L2** — Powerful high-level constraints.

OscaR includes many global constraints, among them:

| Constraint | Description |
|---|---|
| `allDifferent(X)` | All variables take distinct values |
| `gcc(X, vals, low, up)` | Global Cardinality Constraint |
| `sum(X) === s` | Summation |
| `element(T, y, z)` | Element indexing |
| `table(X, tuples)` | Extensional (table) constraint |
| `unaryResource(...)` | Unary scheduling resource |
| `cumulative(...)` | Cumulative scheduling resource |
| `circuit(X)` | Hamiltonian circuit (TSP) |
| `minAssignment(X, w, cost)` | Minimum weight assignment |

Global constraints can dramatically reduce the solving time compared to their decompositions.

---

### Implement Your Own Constraint

> **L3** — Extend the library with a custom propagator.

See [`SimpleUserConstraint.scala`](src/main/scala/oscar/cp/examples/userguide/SimpleUserConstraint.scala).

Extend `oscar.cp.core.Constraint` and override:
- `def setup(l: CPPropagStrength): Unit` — initial propagation and event registration.
- `def propagate(): Unit` — triggered when a watched domain changes.

Signal inconsistency by throwing `Inconsistency.get`.

```scala
class MyLeq(x: CPIntVar, y: CPIntVar) extends Constraint(x.store, "MyLeq") {
  override def setup(l: CPPropagStrength): Unit = {
    x.callPropagateWhenBoundsChange(this)
    y.callPropagateWhenBoundsChange(this)
    propagate()
  }
  override def propagate(): Unit = {
    y.updateMin(x.min)
    x.updateMax(y.max)
  }
}
```

---

### Large Neighborhood Search (LNS)

> **L2** — Scale CP to large optimization problems.

See [`QuadraticAssignmentLNS.scala`](src/main/scala/oscar/cp/examples/userguide/QuadraticAssignmentLNS.scala).

LNS avoids getting stuck in one region of the search tree by restarting frequently. Each restart:
1. **Relaxes** a subset of variables (releases their assignment from the best solution found so far), and
2. **Restarts** CP within the relaxed subspace with a failure limit.

```scala
var bestSol = Array.fill(n)(0)

onSolution { bestSol = x.map(_.value) }

for (_ <- 1 to 100) {
  startSubjectTo(failureLimit = 1000) {
    // Fix ~50% of variables to their best-solution value
    for (i <- x.indices if Random.nextDouble() < 0.5)
      add(x(i) === bestSol(i))
  }
}
```

---

## Examples

A broad set of ready-to-run examples can be found under:

```
src/main/scala/oscar/cp/examples/
```

Key examples include:

| File | Description |
|---|---|
| [`Queens.scala`](src/main/scala/oscar/cp/examples/Queens.scala) | N-Queens problem |
| [`VRPTW.scala`](src/main/scala/oscar/cp/examples/VRPTW.scala) | Vehicle Routing with Time Windows |
| [`Nurses.scala`](src/main/scala/oscar/cp/examples/Nurses.scala) | Nurse scheduling |
| [`Steel.scala`](src/main/scala/oscar/cp/examples/Steel.scala) | Steel mill slab design |
| [`QuadraticAssignmentLNS.scala`](src/main/scala/oscar/cp/examples/QuadraticAssignmentLNS.scala) | Quadratic Assignment with LNS |
| [`RCPSP.scala`](src/main/scala/oscar/cp/examples/RCPSP.scala) | Resource-Constrained Project Scheduling |
| [`MagicSequence.scala`](src/main/scala/oscar/cp/examples/MagicSequence.scala) | Magic sequence |
| [`Domino.scala`](src/main/scala/oscar/cp/examples/Domino.scala) | Domino tiling |
| [`KnightTour.scala`](src/main/scala/oscar/cp/examples/KnightTour.scala) | Knight's tour |
| [`PatientTransportationProblem.scala`](src/main/scala/oscar/cp/examples/PatientTransportationProblem.scala) | Patient transport scheduling |

**Tutorial examples** (step-by-step code from this guide):
```
src/main/scala/oscar/cp/examples/userguide/
```

**Scheduling examples:**
```
src/main/scala/oscar/cp/examples/scheduling/
```

**Hakank examples** (classic puzzles and combinatorial problems):
```
src/main/scala/oscar/cp/examples/hakank/
```

---

## Authors & Acknowledgments

- [**Pierre Schaus**](http://www.info.ucl.ac.be/~pschaus/) — Professor at UCLouvain, ICTEAM/INGI

Many researchers, students, and collaborators have contributed ideas, feedback, and code to OscaR over the years.

**Institutional support:**
- [UCLouvain](http://www.uclouvain.be)
- [ICTEAM / INGI](https://www.uclouvain.be/ingi.html)

---

## Citing OscaR

If you use OscaR-CP in academic work, please cite:

```bibtex
@Misc{oscar,
  author = "{Pierre Schaus}",
  title  = "{O}sca{R}-CP",
  year   = {2012},
  note   = {Available from \texttt{https://github.com/pschaus/oscar}},
}
```
