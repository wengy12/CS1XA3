<h1>Very Bad Prototype of Math Library in Haskell 2018</h1>
 <a href="http://ugweb.cas.mcmaster.ca/~wengy12/docs" target="_blank">See the Documentation Here</a>

<h3>Basic Functionalities:</h3>
	<li> An expression datatype that can encode:</li>
			<li>Addition</li>
			<li>Multiplication</li>
      <li>Power</li>
			<li>Cos, Sin, Exp, ln</li>
			<li>Variables</li>
			<li>Constants</li>
			</br>
	 <li>Can partiall evaluate an expression</li>
	 </br><li>Can perform partial differentiation (everything exept of ln' cause I dont have division)</li>
	 </br><li>Can perform simplification of expressions</li>
	 </br><li>Can parse certain strings into an expression datatype (specify required format in documentation)</li>

</br>

<h1>Core Functionality Testing:</h1>

1) simplify (Map.fromList [("y", 15)]) ((Var "x")  !+ (Var "y") !+ (Const 42) !+ (Const 23))
> ((Var "x")) !+ ((Val 80.0))

2) partDiff "x" (Mult (Var "x") (Var "y"))
> ((Var "y"))

3) eval (Map.fromList [("x", 25), ("y", 10.45), ("z", 20)]) ((Var "x") !+ (Var "y") !+ (Var "z") !+ (Const 2) !+ (Const 3) !+ (Const 4))
> 64.45

Special Thanks to https://github.com/zhanc37, https://github.com/sahur1, https://github.com/bucklj4 and https://github.com/ibrahimq1 for making this possible. Cheers.
