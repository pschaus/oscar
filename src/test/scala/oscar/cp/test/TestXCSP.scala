package oscar.cp.test

import oscar.cp.xcsp.modeling.DefaultConstraints
import oscar.cp.xcsp.XCSPSolver
import oscar.cp.xcsp.ast.ParameterParser
import oscar.cp.xcsp.ast.IntegerVariable
import oscar.cp.testUtils.TestSuite
import oscar.cp._

class TestXCSP extends TestSuite {

  test("test format") {

    val solver = new XCSPSolver with DefaultConstraints
    val str: String = """<?xml version="1.0" encoding="UTF-8"?> 
	    <instance> 
	    		<presentation name="?" maxConstraintArity="3" format="XCSP 1.9"/>
	    </instance>"""
    val thrown = intercept[RuntimeException] { solver.model(str) }
    assert(thrown.getMessage === "Only XCSP 2.0 or 2.1 format is supported.")
  }

  test("nqueen intension") {
    val solver = new XCSPSolver with DefaultConstraints
    val str: String = """<?xml version="1.0" encoding="UTF-8"?>

<instance>
<presentation name="?" maxConstraintArity="2" format="XCSP 2.1"/>

<domains nbDomains="1">
<domain name="D0" nbValues="4">1..4</domain>
</domains>

<variables nbVariables="4">
<variable name="V0" domain="D0"/>
<variable name="V1" domain="D0"/>
<variable name="V2" domain="D0"/>
<variable name="V3" domain="D0"/>
</variables>

<predicates nbPredicates="1">
<predicate name="P0">
  <parameters>int X0 int X1 int X2 int X3 int X4</parameters>
  <expression> 
    <functional>and(ne(X0,X1),ne(abs(sub(X2,X3)),X4))</functional>
  </expression>
</predicate>
</predicates>

<constraints nbConstraints="6">
<constraint name="C0" arity="2" scope="V0 V1" reference="P0">
  <parameters>V0 V1 V0 V1 1</parameters>
</constraint>
<constraint name="C1" arity="2" scope="V0 V2" reference="P0">
  <parameters>V0 V2 V0 V2 2</parameters>
</constraint>
<constraint name="C2" arity="2" scope="V0 V3" reference="P0">
  <parameters>V0 V3 V0 V3 3</parameters>
</constraint>
<constraint name="C3" arity="2" scope="V1 V2" reference="P0">
  <parameters>V1 V2 V1 V2 1</parameters>
</constraint>
<constraint name="C4" arity="2" scope="V1 V3" reference="P0">
  <parameters>V1 V3 V1 V3 2</parameters>
</constraint>
<constraint name="C5" arity="2" scope="V2 V3" reference="P0">
  <parameters>V2 V3 V2 V3 1</parameters>
</constraint>
</constraints>
</instance> """

    val (cp, vars) = solver.model(str)
    var nbSol: Int = 0
    cp.onSolution(nbSol += 1)

    cp.search(binaryFirstFail(vars.toSeq))
    cp.start()
    assert(nbSol === 2)
  }

  test("hanoi 03 sat extension") {
    val solver = new XCSPSolver with DefaultConstraints
    val str: String = """<?xml version="1.0" encoding="UTF-8"?>

<instance>
<presentation name="?" maxConstraintArity="2" format="XCSP 2.1"/>

<domains nbDomains="3">
<domain name="D0" nbValues="2">1..2</domain>
<domain name="D1" nbValues="27">0..26</domain>
<domain name="D2" nbValues="2">24..25</domain>
</domains>

<variables nbVariables="6">
<variable name="V0" domain="D0"/>
<variable name="V1" domain="D1"/>
<variable name="V2" domain="D1"/>
<variable name="V3" domain="D1"/>
<variable name="V4" domain="D1"/>
<variable name="V5" domain="D2"/>
</variables>

<relations nbRelations="3">
<relation name="R0" arity="2" nbTuples="6" semantics="supports">1 0|1 2|1 7|2 0|2 1|2 5</relation>
<relation name="R1" arity="2" nbTuples="78" semantics="supports">0 1|0 2|1 0|1 2|1 7|2 0|2 1|2 5|3 4|3 5|3 6|4 3|4 5|4 22|5 2|5 3|5 4|6 3|6 7|6 8|7 1|7 6|7 8|8 6|8 7|8 17|9 10|9 11|9 18|10 9|10 11|10 16|11 9|11 10|11 14|12 13|12 14|12 15|13 12|13 14|14 11|14 12|14 13|15 12|15 16|15 17|16 10|16 15|16 17|17 8|17 15|17 16|18 9|18 19|18 20|19 18|19 20|19 25|20 18|20 19|20 23|21 22|21 23|21 24|22 4|22 21|22 23|23 20|23 21|23 22|24 21|24 25|24 26|25 19|25 24|25 26|26 24|26 25</relation>
<relation name="R2" arity="2" nbTuples="6" semantics="supports">19 25|21 24|24 25|25 24|26 24|26 25</relation>
</relations>

<constraints nbConstraints="5">
<constraint name="C0" arity="2" scope="V0 V1" reference="R0"/>
<constraint name="C1" arity="2" scope="V1 V2" reference="R1"/>
<constraint name="C2" arity="2" scope="V2 V3" reference="R1"/>
<constraint name="C3" arity="2" scope="V3 V4" reference="R1"/>
<constraint name="C4" arity="2" scope="V4 V5" reference="R2"/>
</constraints>
</instance>"""

    val (cp, vars) = solver.model(str)
    var nbSol: Int = 0
    cp.onSolution(nbSol += 1)

    cp.search(binaryFirstFail(vars.toSeq))
    cp.start(1)
    assert(nbSol === 1)
  }

  test("test weightedsum") {
    val solver = new XCSPSolver with DefaultConstraints
    val str: String = """<?xml version="1.0" encoding="UTF-8"?>
	  	<instance>
	  		<presentation name="?" maxConstraintArity="3" format="XCSP 2.1"/>
	  		<domains nbDomains="1">
	  			<domain name="D0" nbValues="3">0..2</domain>
	  		</domains>

	  		<variables nbVariables="3">
	  			<variable name="V0" domain="D0"/>
	  			<variable name="V1" domain="D0"/>
	  			<variable name="V2" domain="D0"/>
	  		</variables>

	  		<constraints nbConstraints="1">
	  			<constraint name="C2" arity="3" scope="V0 V1 V2" reference="global:weightedSum">
	  				<parameters>
	  [ { 1 V0 } { 2 V1 } { -3 V2 } ]
	  		<eq/>
	  				6
	  				</parameters>
	  			</constraint>
	  		</constraints>
	  	</instance>"""
    val (cp, vars) = solver.model(str)
    var nbSol: Int = 0
    cp.onSolution(nbSol += 1)

    cp.search(binaryFirstFail(vars.toSeq))
    cp.start()
    assert(nbSol === 1)
  }

  test("test alldiff") {
    val solver = new XCSPSolver with DefaultConstraints
    val str: String = """<?xml version="1.0" encoding="UTF-8"?>
	  <instance>
	  <presentation name="?" maxConstraintArity="3" format="XCSP 2.1"/>
	  <domains nbDomains="1">
	  	<domain name="D0" nbValues="3">0..2</domain>
	  </domains>

	  <variables nbVariables="3">
	  	<variable name="V0" domain="D0"/>
	  	<variable name="V1" domain="D0"/>
	  	<variable name="V2" domain="D0"/>
	  </variables>

	  <constraints nbConstraints="1">
	  	<constraint name="C0" arity="3" scope="V0 V1 V2" reference="global:allDifferent">
	  		<parameters>[      V0 V1 V2 ]</parameters>
	  	</constraint>
	  </constraints>
	  </instance>"""
    val (cp, vars) = solver.model(str)
    var nbSol: Int = 0
    cp.onSolution(nbSol += 1)

    cp.search(binaryFirstFail(vars.toSeq))
    cp.start()
    assert(nbSol === 6)

  }

  test("test alldiff 2") {
    val solver = new XCSPSolver with DefaultConstraints
    val str: String = """<?xml version="1.0" encoding="UTF-8"?>
	  <instance>
	  <presentation name="?" maxConstraintArity="3" format="XCSP 2.1"/>
	  <domains nbDomains="1">
	  	<domain name="D0" nbValues="3">0..2</domain>
	  </domains>

	  <variables nbVariables="2">
	  	<variable name="V0" domain="D0"/>
	  	<variable name="V1" domain="D0"/>
	  </variables>

	  <constraints nbConstraints="1">
	  	<constraint name="C0" arity="2" scope="V0 V1" reference="global:allDifferent">
	  		<parameters>[      V0 V1 2 ]</parameters>
	  	</constraint>
	  </constraints>
	  </instance>"""
    val (cp, vars) = solver.model(str)
    var nbSol: Int = 0
    cp.onSolution(nbSol += 1)

    cp.search(binaryFirstFail(vars.toSeq))
    cp.start()
    assert(nbSol === 2)

  }

  test("test among") {
    val solver = new XCSPSolver with DefaultConstraints
    val str: String = """<?xml version="1.0" encoding="UTF-8"?>
	  	<instance>
	  		<presentation name="?" maxConstraintArity="3" format="XCSP 2.1"/>
	  		<domains nbDomains="1">
	  			<domain name="D0" nbValues="3">0..2</domain>
	  		</domains>

	  		<variables nbVariables="3">
	  			<variable name="V0" domain="D0"/>
	  			<variable name="V1" domain="D0"/>
	  			<variable name="V2" domain="D0"/>
	  		</variables>

	  		<constraints nbConstraints="1">
	  			<constraint name="C2" arity="3" scope="V0 V1 V2" reference="global:among">
	  				<parameters>
	  	V0
[ V1 V2 ]
[ 5]

	  				</parameters>
	  			</constraint>
	  		</constraints>
	  	</instance>"""
    val (cp, vars) = solver.model(str)
    cp.onSolution(assert(vars(0).value === 0))

    cp.search(binaryFirstFail(vars.toSeq))
    cp.start()

  }

  test("test atleast") {
    val solver = new XCSPSolver with DefaultConstraints
    val str: String = """<?xml version="1.0" encoding="UTF-8"?>
	  	<instance>
	  		<presentation name="?" maxConstraintArity="3" format="XCSP 2.1"/>
	  		<domains nbDomains="1">
	  			<domain name="D0" nbValues="3">0..2</domain>
	  		</domains>

	  		<variables nbVariables="2">
	  			<variable name="V0" domain="D0"/>
	  			<variable name="V1" domain="D0"/>
	  		</variables>

	  		<constraints nbConstraints="1">
	  			<constraint name="C2" arity="2" scope="V0 V1" reference="global:atleast">
	  				<parameters>
	  	1
[V0 V1]
2

	  				</parameters>
	  			</constraint>
	  		</constraints>
	  	</instance>"""

    var nbSol = 0
    val (cp, vars) = solver.model(str)
    cp.onSolution(nbSol += 1)

    cp.search(binaryFirstFail(vars.toSeq))
    cp.start()
    assert(nbSol === 5)

  }

  test("test atmost") {
    val solver = new XCSPSolver with DefaultConstraints
    val str: String = """<?xml version="1.0" encoding="UTF-8"?>
	  	<instance>
	  		<presentation name="?" maxConstraintArity="3" format="XCSP 2.1"/>
	  		<domains nbDomains="1">
	  			<domain name="D0" nbValues="3">0..2</domain>
	  		</domains>

	  		<variables nbVariables="2">
	  			<variable name="V0" domain="D0"/>
	  			<variable name="V1" domain="D0"/>
	  		</variables>

	  		<constraints nbConstraints="1">
	  			<constraint name="C2" arity="2" scope="V0 V1" reference="global:atmost">
	  				<parameters>
	  	1
[V0 V1]
2

	  				</parameters>
	  			</constraint>
	  		</constraints>
	  	</instance>"""

    var nbSol = 0
    val (cp, vars) = solver.model(str)
    cp.onSolution {
      val t = vars
      nbSol += 1
    }

    cp.search(binaryFirstFail(vars.toSeq))
    cp.start()
    assert(nbSol === 8)

  }

  test("test cumulative") {
    val solver = new XCSPSolver with DefaultConstraints
    val str: String = """<?xml version="1.0" encoding="UTF-8"?>
	  	<instance>
	  		<presentation name="?" maxConstraintArity="3" format="XCSP 2.1"/>
	  		<domains nbDomains="1">
	  			<domain name="D0" nbValues="1">0..0</domain>
	  		</domains>

	  		<variables nbVariables="1">
	  			<variable name="V0" domain="D0"/>
	  		</variables>

	  		<constraints nbConstraints="1">
	  			<constraint name="C2" arity="1" scope="V0" reference="global:cumulative">
	  				<parameters> 
	    	[{V0 4 <nil/> 1}] 3 
	    </parameters>
	  			</constraint>
	  		</constraints>
	  	</instance>"""

    var nbSol = 0
    val (cp, vars) = solver.model(str)
    cp.onSolution {
      val t = vars
      nbSol += 1
    }

    cp.search(binaryFirstFail(vars.toSeq))
    cp.start()
    assert(nbSol === 1)

  }

  test("test cumulative 2") {
    val solver = new XCSPSolver with DefaultConstraints
    val str: String = """<?xml version="1.0" encoding="UTF-8"?>
	  	<instance>
	  		<presentation name="?" maxConstraintArity="3" format="XCSP 2.1"/>
	  		<domains nbDomains="1">
	  			<domain name="D0" nbValues="1">0..0</domain>
	  		</domains>

	  		<variables nbVariables="1">
	  			<variable name="V0" domain="D0"/>
	  		</variables>

	  		<constraints nbConstraints="1">
	  			<constraint name="C2" arity="1" scope="V0" reference="global:cumulative">
	  				<parameters> 
	    	[{V0 4 <nil/> 5}] 3 
	    </parameters>
	  			</constraint>
	  		</constraints>
	  	</instance>"""

    var nbSol = 0
    val (cp, vars) = solver.model(str)
    cp.onSolution {
      val t = vars
      nbSol += 1
    }

    cp.search(binaryFirstFail(vars.toSeq))
    cp.start()
    assert(nbSol === 0)

  }

  test("test disjunctive") {
    val solver = new XCSPSolver with DefaultConstraints
    val str: String = """<?xml version="1.0" encoding="UTF-8"?>
	  	<instance>
	  		<presentation name="?" maxConstraintArity="3" format="XCSP 2.1"/>
	  		<domains nbDomains="2">
	  			<domain name="D0" nbValues="1">0..0</domain>
	    		<domain name="D1" nbValues="1">1..1</domain>
	  		</domains>

	  		<variables nbVariables="4">
	  			<variable name="O1" domain="D0"/>
	    		<variable name="DUR1" domain="D1"/>
	    		<variable name="O2" domain="D1"/>
	    		<variable name="DUR2" domain="D1"/>
	  		</variables>

	  		<constraints nbConstraints="1">
	  			<constraint name="C2" arity="4" scope="O1 DUR1 O2 DUR2" reference="global:disjunctive">
	  				<parameters> 
	    		[{O1 DUR1} {O2 DUR2}] 
	    </parameters>
	  			</constraint>
	  		</constraints>
	  	</instance>"""

    var nbSol = 0
    val (cp, vars) = solver.model(str)
    cp.onSolution {
      val t = vars
      nbSol += 1
    }

    cp.search(binaryFirstFail(vars.toSeq))
    cp.start()
    assert(nbSol === 1)

  }

  test("test disjunctive 2") {
    val solver = new XCSPSolver with DefaultConstraints
    val str: String = """<?xml version="1.0" encoding="UTF-8"?>
	  	<instance>
	  		<presentation name="?" maxConstraintArity="3" format="XCSP 2.1"/>
	  		<domains nbDomains="2">
	  			<domain name="D0" nbValues="1">0..0</domain>
	    		<domain name="D1" nbValues="1">1..1</domain>
	  		</domains>

	  		<variables nbVariables="4">
	  			<variable name="O1" domain="D0"/>
	    		<variable name="DUR1" domain="D1"/>
	    		<variable name="O2" domain="D0"/>
	    		<variable name="DUR2" domain="D1"/>
	  		</variables>

	  		<constraints nbConstraints="1">
	  			<constraint name="C2" arity="4" scope="O1 DUR1 O2 DUR2" reference="global:disjunctive">
	  				<parameters> 
	    		[{O1 DUR1} {O2 DUR2}] 
	    </parameters>
	  			</constraint>
	  		</constraints>
	  	</instance>"""

    var nbSol = 0
    val (cp, vars) = solver.model(str)
    cp.onSolution {
      val t = vars
      nbSol += 1
    }

    cp.search(binaryFirstFail(vars.toSeq))
    cp.start()
    assert(nbSol === 0)

  }

  //index is going from 1 to n, not from 0 to n-1 in XCSP format (follows specifications of global constraint catalog)
  test("test element") {
    val solver = new XCSPSolver with DefaultConstraints
    val str: String = """<?xml version="1.0" encoding="UTF-8"?>
	  	<instance>
	  		<presentation name="?" maxConstraintArity="3" format="XCSP 2.1"/>
	  		<domains nbDomains="3">
	  			<domain name="D0" nbValues="1">1..1</domain>
	    		<domain name="D1" nbValues="1">0..1</domain>
	    		<domain name="D2" nbValues="1">1..1</domain>
	  		</domains>

	  		<variables nbVariables="4">
	  			<variable name="I" domain="D0"/>
	    		<variable name="T0" domain="D1"/>
	    		<variable name="T1" domain="D1"/>
	    		<variable name="V" domain="D2"/>
	  		</variables>

	  		<constraints nbConstraints="1">
	  			<constraint name="C2" arity="4" scope="I T0 T1 V" reference="global:element">
	  				<parameters> 
	    			I
	    			[T0 T1]
	    			V
	    			</parameters>
	  			</constraint>
	  		</constraints>
	  	</instance>"""

    var nbSol = 0
    val (cp, vars) = solver.model(str)
    cp.onSolution {
      val t = vars
      nbSol += 1
    }

    cp.search(binaryFirstFail(vars.toSeq))
    cp.start()
    assert(nbSol === 2)

  }

  test("test global_cardinality") {
    val solver = new XCSPSolver with DefaultConstraints
    val str: String = """<?xml version="1.0" encoding="UTF-8"?>
	  	<instance>
	  		<presentation name="?" maxConstraintArity="3" format="XCSP 2.1"/>
	  		<domains nbDomains="3">
	  			<domain name="D0" nbValues="1">1..2</domain>
	    		<domain name="D1" nbValues="1">1..1</domain>
	    		<domain name="D2" nbValues="1">2..2</domain>
	  		</domains>

	  		<variables nbVariables="5">
	  			<variable name="V0" domain="D0"/>
	    		<variable name="V1" domain="D0"/>
	    		<variable name="V2" domain="D0"/>
	    		<variable name="V3" domain="D2"/>
	    		<variable name="V4" domain="D1"/>
	  		</variables>
	    <constraints nbConstraints="1">
	  		<constraint name="C1" arity="5" scope="V0 V1 V2 V3 V4" reference="global:global_cardinality">
	  		<parameters>
	  		[ V0 V1 V2 ]
	  		[ { 1 V3 } { 2 V4 } ]
	  		</parameters>
	  		</constraint>
	  		</constraints>
	  	</instance>"""

    var nbSol = 0
    val (cp, vars) = solver.model(str)
    cp.onSolution {
      val t = vars
      nbSol += 1
    }

    cp.search(binaryFirstFail(vars.toSeq))
    cp.start()
    assert(nbSol === 3)
  }

  test("test minimum_weight_all_different") {
    val solver = new XCSPSolver with DefaultConstraints
    val str: String = """<?xml version="1.0" encoding="UTF-8"?>
	  	<instance>
	  		<presentation name="?" maxConstraintArity="3" format="XCSP 2.1"/>
	  		<domains nbDomains="2">
	  			<domain name="D0" nbValues="1">0..1</domain>
	    		<domain name="D1" nbValues="1">6..7</domain>
	  		</domains>

	  		<variables nbVariables="3">
	    		<variable name="V1" domain="D0"/>
	    		<variable name="V2" domain="D0"/>
	    		<variable name="C" domain="D1"/>
	  		</variables>
	    <constraints nbConstraints="2">
	    <constraint name="C1" arity="3" scope="V1 V2 C" reference="global:minimum_weight_all_different">
	  	<parameters>
	  		[ V1 V2]
	    [ 
	  {1 1 5} 
	  {1 2 0} 
	  {2 1 1}
	  {2 2 2}
	  ]
	  		C
	  	</parameters>
	  </constraint>
                        <constraint name="C2" arity="2" scope="V1 V2" reference="global:allDifferent">
                        	  		<parameters>[      V1 V2 2 ]</parameters>
                        	  	</constraint>


	    </constraints>

	  	</instance>"""

    var nbSol = 0
    val (cp, vars) = solver.model(str)
    cp.onSolution {
      val t = vars
      nbSol += 1
    }

    cp.search(binaryFirstFail(vars.toSeq))
    cp.start()
    assert(nbSol === 3)
  }

  test("parsing variables in scope must work") {
    val parser = new ParameterParser(Seq("V0", "V01"))
    assert(parser.parseAll(parser.variableList, "[ V01 V0 ] ").get === List(IntegerVariable("V01"), IntegerVariable("V0")))
  }

  test("parsing variables not in scope must not work") {
    val parser = new ParameterParser(Seq("V0"))
    intercept[RuntimeException] { parser.parseAll(parser.variableList, "[ V01 V0 ] ").get }
  }

  test("constraint in intension") {
    val solver = new XCSPSolver with DefaultConstraints
    val str: String = """<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<instance>
<presentation format="XCSP 2.1" maxConstraintArity="17"/>
	    
	    <domains nbDomains="1">
	        <domain name="D11" nbValues="16">1..16</domain>
	    </domains>
	      <variables nbVariables="1">
	        <variable domain="D11" name="V28"/>
	    </variables> 
	    <predicates nbPredicates="1">
	    <predicate name="P3">
      <parameters> int X0 int X1</parameters>
      <expression> 
        <functional>eq(add(mul(4,sub(X0,1)),1),sub(X1,1))</functional>
      </expression>
    </predicate>
  </predicates>
	    
	    <constraints nbConstraints="1">
	    <constraint arity="1" name="C21" reference="P3" scope="V28">
      <parameters>2 V28</parameters> 
    </constraint>
	    </constraints>
	    </instance>
	    """
    val (cp, vars) = solver.model(str)

    assert((2 - 1) * 4 + 1 === vars(0).value - 1)
  }

  test("test weightedSum operator different from equality") {
    val solver = new XCSPSolver with DefaultConstraints
    val str: String = """<?xml version="1.0" encoding="UTF-8"?>
<instance>
<presentation  maxConstraintArity="10" format="XCSP 2.1" type="CSP"/>
<domains nbDomains="1">
<domain name="D10" nbValues="2">0..1</domain>
</domains>
<variables nbVariables="4">
<variable name="V20" domain="D10"/>
<variable name="V21" domain="D10"/>
<variable name="V22" domain="D10"/>
<variable name="V23" domain="D10"/>
</variables>
<constraints nbConstraints="1">
<constraint name="C180" arity="4" scope="V20 V21 V22 V23" reference="global:weightedSum">
<parameters>[{ 1 V20} {1 V21} {1 V22} { 1 V23}] <ge/> 1 </parameters>
</constraint>
</constraints>
</instance>
	    """

    val thrown = intercept[RuntimeException] { solver.model(str) }
    assert(thrown.getMessage === "weightedSum with an operator different from equality is not supported.")

  }

}

