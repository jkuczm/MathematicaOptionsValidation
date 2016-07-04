(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["OptionsValidation`Tests`chooseOptionDuplicates`", {"MUnit`"}]


Get["OptionsValidation`"]


chooseOptionDuplicates = OptionsValidation`Private`chooseOptionDuplicates


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*All*)


TestMatch[
	chooseOptionDuplicatesFunc = chooseOptionDuplicates[All]
	,
	Identity
	,
	TestID -> "All: function generation"
]


(* ::Subsection:: *)
(*First*)


BeginTestSection["First"]


Module[{chooseOptionDuplicatesFunc, valA, valB, valC, valD, valE, valF},
	TestMatch[
		chooseOptionDuplicatesFunc = chooseOptionDuplicates[First]
		,
		_Function
		,
		TestID -> "First: function generation",
		TestFailureAction -> "SkipSection"
	];

	Block[{opt1, opt2, opt3},
		Test[
			chooseOptionDuplicatesFunc[{
				opt2 -> valA, "opt1" :> valB, opt1 -> valC, "opt3" -> valD,
				"opt2" :> valE, opt1 -> valF
			}]
			,
			{opt2 -> valA, "opt1" :> valB, "opt3" -> valD}
			,
			TestID -> "First: function evaluation"
		]
	]
]


EndTestSection[]


(* ::Subsection:: *)
(*Last*)


BeginTestSection["Last"]


Module[{chooseOptionDuplicatesFunc, valA, valB, valC, valD, valE, valF},
	TestMatch[
		chooseOptionDuplicatesFunc = chooseOptionDuplicates[Last]
		,
		_Function
		,
		TestID -> "Last: function generation",
		TestFailureAction -> "SkipSection"
	];

	Block[{opt1, opt2, opt3},
		Test[
			chooseOptionDuplicatesFunc[{
				opt2 -> valA, "opt1" :> valB, opt1 -> valC, "opt3" -> valD,
				"opt2" :> valE, opt1 -> valF
			}]
			,
			{"opt2" :> valE, opt1 -> valF, "opt3" -> valD}
			,
			TestID -> "Last: function evaluation"
		]
	]
]


EndTestSection[]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
