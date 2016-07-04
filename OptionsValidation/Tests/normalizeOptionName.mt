(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["OptionsValidation`Tests`normalizeOptionName`", {"MUnit`"}]


Get["OptionsValidation`"]


normalizeOptionName = OptionsValidation`Private`normalizeOptionName


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Basic*)


Test[
	normalizeOptionName["testOptName"]
	,
	"testOptName"
	,
	TestID -> "Basic: String"
]
Block[{testOpt},
	Test[
		normalizeOptionName[testOpt]
		,
		"testOpt"
		,
		TestID -> "Basic: Symbol"
	]
]
Block[{`tmp`testOptArbCont},
	Test[
		normalizeOptionName[`tmp`testOptArbCont]
		,
		"testOptArbCont"
		,
		TestID -> "Basic: Symbol from arbitrary context"
	]
]
Test[
	normalizeOptionName[5]
	,
	5
	,
	TestID -> "Basic: Integer"
]


(* ::Subsection:: *)
(*Rule*)


Block[{testOpt},
	Test[
		normalizeOptionName[testOpt ->  "testSubOpt"]
		,
		"testOpt" :>  "testSubOpt"
		,
		TestID -> "Rule: Symbol, String"
	]
]
Block[{`tmp`subOptName, x, y},
	Test[
		normalizeOptionName[x + y -> `tmp`subOptName]
		,
		x + y :> "subOptName"
		,
		TestID -> "Rule: Plus symbols, Symbol from arbitrary context"
	]
]


(* ::Subsection:: *)
(*RuleDelayed*)


Block[{`tmp`subOptName},
	Test[
		normalizeOptionName["testOptionName" :>  `tmp`subOptName]
		,
		"testOptionName" :>  "subOptName"
		,
		TestID -> "RuleDelayed: String, Symbol from arbitrary context"
	]
]
Block[{testOpt},
	Test[
		normalizeOptionName[testOpt :> 2.6 * 0.7]
		,
		"testOpt" :> 2.6 * 0.7
		,
		TestID -> "RuleDelayed: Symbol, Times reals"
	]
]


(* ::Subsection:: *)
(*Nested*)


Block[{opt, x, tmp`subSubSubOpt, y},
	Test[
		normalizeOptionName[
			opt -> (x :> 2 + 3) -> "subSubOpt" :> tmp`subSubSubOpt -> y - y
		]
		,
		"opt" :> (x :> 2 + 3) :> "subSubOpt" :> "subSubSubOpt" :> y - y
		,
		TestID -> "Nested rules"
	]
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
