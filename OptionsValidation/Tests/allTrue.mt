(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["OptionsValidation`Tests`allTrue`", {"MUnit`"}]


Get["OptionsValidation`"]


allTrue = OptionsValidation`Private`allTrue


SetAttributes[regiteringFunction, HoldFirst]
regiteringFunction[register_Symbol, function_] :=
	Function[Null,
		With[{result = function[#]},
			AppendTo[register, result];
			result
		]
		,
		HoldAllComplete
	]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*failFast: True*)


BeginTestSection["failFast: True"]


TestMatch[
	allTrueFunc = allTrue[True]
	,
	If[$VersionNumber < 10,
		_Function
	(* else *),
		AllTrue
	]
	,
	TestID -> "True: allTrue function generation",
	TestFailureAction -> "SkipSection"
]


Module[{testQCalls = {}},
	Test[
		allTrueFunc[Range[2, 12], regiteringFunction[testQCalls, EvenQ]]
		,
		False
		,
		TestID -> "True: False result: allTrue function evaluation"
	];
	Test[
		testQCalls
		,
		{True, False}
		,
		TestID -> "True: False result: testQ calls results"
	]
]
Module[{a, b, c, testQCalls = {}},
	Test[
		allTrueFunc[{a, b, c},
			regiteringFunction[testQCalls, MatchQ[#, _Symbol]&]
		]
		,
		True
		,
		TestID -> "True: True result: allTrue function evaluation"
	];
	Test[
		testQCalls
		,
		{True, True, True}
		,
		TestID -> "True: True result: testQ calls results"
	]
]


Module[{testQCalls = {}, leaks = ConstantArray[False, 4]},
	Test[
		allTrueFunc[
			Unevaluated@{
				leaks[[1]] = True,
				leaks[[2]] = True,
				leaks[[3]] = True; 5,
				leaks[[4]] = True
			},
			regiteringFunction[testQCalls,
				Function[Null, MatchQ[Unevaluated[#], _Set], HoldFirst]
			]
		]
		,
		False
		,
		TestID -> "True: Unevaluated: False result: \
allTrue function evaluation"
	];
	Test[
		testQCalls
		,
		{True, True, False}
		,
		TestID -> "True: Unevaluated: False result: testQ calls results"
	];
	Test[
		leaks
		,
		ConstantArray[False, 4]
		,
		TestID -> "True: Unevaluated: False result: leaks"
	]
]
Module[{testQCalls = {}, leaks = ConstantArray[False, 2]},
	Test[
		allTrueFunc[
			Unevaluated@{leaks[[1]] = True, leaks[[2]] = True},
			regiteringFunction[testQCalls,
				Function[Null, Length@Unevaluated[#] === 2, HoldFirst]
			]
		]
		,
		True
		,
		TestID -> "True: Unevaluated: True result: allTrue function evaluation"
	];
	Test[
		testQCalls
		,
		{True, True}
		,
		TestID -> "True: Unevaluated: True result: testQ calls results"
	];
	Test[
		leaks
		,
		ConstantArray[False, 2]
		,
		TestID -> "True: Unevaluated: True result: leaks"
	]
]


EndTestSection[]


(* ::Subsection:: *)
(*failFast: False*)


BeginTestSection["failFast: False"]


TestMatch[
	allTrueFunc = allTrue[False]
	,
	_Function
	,
	TestID -> "False: allTrue function generation",
	TestFailureAction -> "SkipSection"
]


Module[{testQCalls = {}},
	Test[
		allTrueFunc[Range[5], regiteringFunction[testQCalls, OddQ]]
		,
		False
		,
		TestID -> "False: False result: allTrue function evaluation"
	];
	Test[
		testQCalls
		,
		{True, False, True, False, True}
		,
		TestID -> "False: False result: testQ calls results"
	]
]
Module[{testQCalls = {}},
	Test[
		allTrueFunc[{"a", "str"}, regiteringFunction[testQCalls, StringQ]]
		,
		True
		,
		TestID -> "False: True result: allTrue function evaluation"
	];
	Test[
		testQCalls
		,
		{True, True}
		,
		TestID -> "False: True result: testQ calls results"
	]
]


Module[{testQCalls = {}, leaks = ConstantArray[False, 3]},
	Test[
		allTrueFunc[
			Unevaluated@{
				leaks[[1]] = True,
				leaks[[2]] = True,
				leaks[[3]] = True
			},
			regiteringFunction[testQCalls,
				Function[Null, OddQ@Unevaluated[#][[1, 2]], HoldFirst]
			]
		]
		,
		False
		,
		TestID -> "False: Unevaluated: False result: \
allTrue function evaluation"
	];
	Test[
		testQCalls
		,
		{True, False, True}
		,
		TestID -> "False: Unevaluated: False result: testQ calls results"
	];
	Test[
		leaks
		,
		ConstantArray[False, 3]
		,
		TestID -> "False: Unevaluated: False result: leaks"
	]
]
Module[{testQCalls = {}, leaks = ConstantArray[False, 3]},
	Test[
		allTrueFunc[
			Unevaluated@{
				leaks[[1]] = True,
				leaks[[2]] = True,
				leaks[[3]] = True
			},
			regiteringFunction[testQCalls,
				Function[Null, Unevaluated[#][[2]], HoldFirst]
			]
		]
		,
		True
		,
		TestID -> "False: Unevaluated: True result: \
allTrue function evaluation"
	];
	Test[
		testQCalls
		,
		{True, True, True}
		,
		TestID -> "False: Unevaluated: True result: testQ calls results"
	];
	Test[
		leaks
		,
		ConstantArray[False, 3]
		,
		TestID -> "False: Unevaluated: True result: leaks"
	]
]


EndTestSection[]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
