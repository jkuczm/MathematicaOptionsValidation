(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["OptionsValidation`Tests`normalizeOptionNamePattern`", {"MUnit`"}]


Get["OptionsValidation`"]


normalizeOptionNamePattern =
	OptionsValidation`Private`normalizeOptionNamePattern


SetAttributes[TestSownParentOptions, HoldAllComplete]

TestSownParentOptions[
	expr_,
	expected_,
	expectedSown_,
	Shortest[messages_:{}],
	opts:OptionsPattern[Test]
] :=
	With[
		{
			reaped =
				Join @@ Last@Reap[
					Test[
						expr
						,
						expected
						,
						messages
						,
						opts
						,
						TestFailureMessage ->
							"normalizeOptionNamePattern evaluation"
					],
					OptionsValidation`Private`parentOptionTag
				]
		}
		,
		Test[
			reaped
			,
			expectedSown
			,
			opts
			,
			TestFailureMessage -> "sown parent options"
		]
	]

TestSownParentOptions[args___] :=
	With[{msg = "Incorrect arguments: " <> ToString[Unevaluated[{args}]]},
		MUnit`Package`testError[msg, {}, args]
	]


SetAttributes[TestNoSownParentOptions, HoldAllComplete]

TestNoSownParentOptions[
	expr_,
	expected_,
	Shortest[messages_:{}],
	opts:OptionsPattern[Test]
] :=
	TestSownParentOptions[
		expr,
		expected,
		{},
		messages,
		opts
	]

TestNoSownParentOptions[args___] :=
	With[{msg = "Incorrect arguments: " <> ToString[Unevaluated[{args}]]},
		MUnit`Package`testError[msg, {}, args]
	]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Basic*)


TestNoSownParentOptions[
	normalizeOptionNamePattern["testOptName"]
	,
	"testOptName"
	,
	TestID -> "Basic: String"
]
Block[{testOpt},
	TestNoSownParentOptions[
		normalizeOptionNamePattern[testOpt]
		,
		"testOpt"
		,
		TestID -> "Basic: Symbol"
	]
]
Block[{`tmp`testOptArbCont},
	TestNoSownParentOptions[
		normalizeOptionNamePattern[`tmp`testOptArbCont]
		,
		"testOptArbCont"
		,
		TestID -> "Basic: Symbol from arbitrary context"
	]
]
TestNoSownParentOptions[
	normalizeOptionNamePattern[_]
	,
	_
	,
	TestID -> "Basic: Blank"
]
TestNoSownParentOptions[
	normalizeOptionNamePattern[5]
	,
	5
	,
	Message[CheckOption::unknownOptionNamePattern, 5]
	,
	TestID -> "Basic: Integer"
]


(* ::Subsection:: *)
(*Pattern*)


Block[{testPattNameStr},
	TestNoSownParentOptions[
		normalizeOptionNamePattern[testPattNameStr:"testOptName"]
		,
		testPattNameStr:"testOptName"
		,
		TestID -> "Pattern: String"
	]
]
Block[{testOpt, testPattNameSym},
	TestNoSownParentOptions[
		normalizeOptionNamePattern[testPattNameSym:testOpt]
		,
		testPattNameSym:"testOpt"
		,
		TestID -> "Pattern: Symbol"
	]
]
Block[{`tmp`testOptArbCont, testPattNameArbCont},
	TestNoSownParentOptions[
		normalizeOptionNamePattern[testPattNameArbCont:`tmp`testOptArbCont]
		,
		testPattNameArbCont:"testOptArbCont"
		,
		TestID -> "Pattern: Symbol from arbitrary context"
	]
]
Block[{testPattNameBlank},
	TestNoSownParentOptions[
		normalizeOptionNamePattern[testPattNameBlank_]
		,
		testPattNameBlank_
		,
		TestID -> "Pattern: Blank"
	]
]
Block[{testPattNameBlankInt},
	TestNoSownParentOptions[
		normalizeOptionNamePattern[testPattNameBlankInt_String]
		,
		testPattNameBlankInt_String
		,
		Message[CheckOption::unknownOptionNamePattern, Verbatim[_String]]
		,
		TestID -> "Pattern: Blank String"
	]
]


(* ::Subsection:: *)
(*Rule*)


Block[{testOpt},
	TestSownParentOptions[
		normalizeOptionNamePattern[testOpt ->  "testSubOpt"]
		,
		"testOpt" :>  "testSubOpt"
		,
		{{HoldPattern["testOpt"]}}
		,
		TestID -> "Rule: Symbol, String"
	]
]
Block[{`tmp`subOptName},
	TestSownParentOptions[
		normalizeOptionNamePattern[_ -> `tmp`subOptName]
		,
		_ :> "subOptName"
		,
		{{HoldPattern[_]}}
		,
		TestID -> "Rule: Blank, Symbol from arbitrary context"
	]
]
TestSownParentOptions[
	normalizeOptionNamePattern[2.1 -> _Integer]
	,
	2.1 :> _Integer
	,
	{{HoldPattern[2.1]}}
	,
	{
		Message[CheckOption::unknownOptionNamePattern, 2.1],
		Message[CheckOption::unknownOptionNamePattern, Verbatim[_Integer]]
	}
	,
	TestID -> "Rule: Real, Blank Integer"
]


(* ::Subsection:: *)
(*RuleDelayed*)


Block[{`tmp`subOptName},
	TestSownParentOptions[
		normalizeOptionNamePattern["testOptionName" :>  `tmp`subOptName]
		,
		"testOptionName" :>  "subOptName"
		,
		{{HoldPattern["testOptionName"]}}
		,
		TestID -> "RuleDelayed: String, Symbol from arbitrary context"
	]
]
Block[{testOpt},
	TestSownParentOptions[
		normalizeOptionNamePattern[testOpt :> _]
		,
		"testOpt" :> _
		,
		{{HoldPattern["testOpt"]}}
		,
		TestID -> "RuleDelayed: Symbol, Blank"
	]
]
Module[{test},
	TestSownParentOptions[
		normalizeOptionNamePattern[_?test :> 2 + 3]
		,
		_?test :> 2 + 3
		,
		{{HoldPattern[_?test]}}
		,
		{
			Message[CheckOption::unknownOptionNamePattern, Verbatim[_?test]],
			Message[CheckOption::unknownOptionNamePattern, 2 + 3]
		}
		,
		TestID -> "RuleDelayed: PatternTest Blank, Plus integers"
	]
]


(* ::Subsection:: *)
(*Alternatives*)


Block[{optName, `tmp`anotherOptName},
	TestNoSownParentOptions[
		normalizeOptionNamePattern[
			optName | "optStrName" | `tmp`anotherOptName
		]
		,
		"optName" | "optStrName" | "anotherOptName"
		,
		TestID -> "Alternatives of known basic patterns"
	]
]
Block[{optName, testF, head},
	TestNoSownParentOptions[
		normalizeOptionNamePattern[optName?testF | _head]
		,
		optName?testF | _head
		,
		{
			Message[CheckOption::unknownOptionNamePattern,
				Verbatim[optName?testF]
			],
			Message[CheckOption::unknownOptionNamePattern, Verbatim[_head]]
		}
		,
		TestID -> "Alternatives of of unknown basic patterns"
	]
]


(* ::Subsection:: *)
(*Nested*)


Block[{name1, name2, opt1, tmp`subSubOpt, subOpt1},
	TestSownParentOptions[
		normalizeOptionNamePattern[
			name1 : (
				opt1 | name2_ | (
					"opt2" :> (name3 : subOpt1 | "subOpt2" -> tmp`subSubOpt)
				)
			)
		]
		,
		name1 : (
			"opt1" | name2_ | (
				"opt2" :> (name3 : "subOpt1" | "subOpt2" :> "subSubOpt")
			)
		)
		,
		{
			{HoldPattern["opt2"]},
			{HoldPattern[name3 : "subOpt1" | "subOpt2"], HoldPattern["opt2"]}
		}
		,
		TestID -> "Nested: known patterns"
	]
]
Block[{name, opt1, subOpt, x, test},
	Off[General::stop];
	TestSownParentOptions[
		normalizeOptionNamePattern[
			name : (
				name_Real |
				(opt1 :> subOpt -> x + x) |
				HoldPattern["opt2" -> _?test]
			)
		]
		,
		name : (
			name_Real |
			("opt1" :> "subOpt" :> x + x) |
			HoldPattern["opt2" :> _?test]
		)
		,
		{
			{HoldPattern["opt1"]},
			{HoldPattern["subOpt"], HoldPattern["opt1"]},
			{HoldPattern["opt2"]}
		}
		,
		{
			Message[CheckOption::unknownOptionNamePattern, Verbatim[_Real]],
			Message[CheckOption::unknownOptionNamePattern, x + x],
			Message[CheckOption::unknownOptionNamePattern, Verbatim[_?test]]
		}
		,
		TestID -> "Nested: unknown patterns"
	];
	On[General::stop]
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
