(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["OptionsValidation`Tests`generateValidator`", {"MUnit`"}]


Get["OptionsValidation`"]

generateValidator = OptionsValidation`Private`generateValidator


Options[testFunc1] = {opt1 -> -2, opt2 -> 5}

CheckOption[testFunc1, opt1, val : Except[_Integer]] :=
	Message[testFunc1::invalidOpt1, HoldForm@val]

CheckOption[testFunc1, opt2] =
	IntegerQ[#] && Positive[#] ||
		Message[testFunc1::invalidOpt2, HoldForm@#] &

CheckOptionRelations[testFunc1] =
	With[{opts = OptionValue[testFunc1, #, {opt1, opt2}]},
		Less @@ opts || Message[testFunc1::invalidRelation, HoldForm@opts]
	]&


Options[testFunc2] = {opt1 -> 10}

CheckOption[testFunc2, opt1, val : Except[_Integer?Negative | _String]] :=
	Message[testFunc2::invalidOpt1, HoldForm@val]


Options[testFunc3] = {opt1 :> "default"}

CheckOption[testFunc3, opt1] =
	Function[val,
		MatchQ[Unevaluated@val, _Set] ||
			Message[testFunc3::invalidOpt1, HoldForm@val]
		,
		HoldFirst
	]


customCheckOption =
	Function[{f, name},
		Function[val,
			Message[customCheckOption::used,
				HoldForm@f, HoldForm@name, HoldForm@val
			]
		]
	]

customCheckOptionRelations =
	Function[f,
		Function[optsList,
			Message[customCheckOptionRelations::used,
				HoldForm@f, HoldForm@optsList
			]
		]
	]


Off[General::stop]


(* ::Section:: *)
(*Tests: Symbol*)


(* ::Subsection:: *)
(*stopOnInvalid: True*)


(* ::Subsubsection:: *)
(*First, CheckOption, CheckOptionRelations*)


BeginTestSection["Symbol: True, First, CheckOption, CheckOptionRelations"]


TestMatch[
	validator =
		generateValidator[Symbol][
			True, First, CheckOption, CheckOptionRelations
		]
	,
	_Function
	,
	TestID -> "Symbol: True, First, CheckOption, CheckOptionRelations: \
validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[testFunc1, {opt1 -> -2}]
	,
	True
	,
	TestID -> "Symbol: True, First, CheckOption, CheckOptionRelations: \
1 valid"
]
Test[
	validator[testFunc1, {opt1 -> 10}]
	,
	False
	,
	Message[testFunc1::invalidRelation, {10, 5}]
	,
	TestID -> "Symbol: True, First, CheckOption, CheckOptionRelations: \
1 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -1]
	,
	TestID -> "Symbol: True, First, CheckOption, CheckOptionRelations: \
1 invalid"
]
Test[
	validator[testFunc1, {opt2 -> -5}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -5]
	,
	TestID -> "Symbol: True, First, CheckOption, CheckOptionRelations: \
1 invalid, invalid rel"
]


Test[
	validator[testFunc1, {opt1 -> 3, opt2 -> 7}]
	,
	True
	,
	TestID -> "Symbol: True, First, CheckOption, CheckOptionRelations: \
2 valid"
]
Test[
	validator[testFunc1, {opt1 -> 6, opt2 -> 2}]
	,
	False
	,
	Message[testFunc1::invalidRelation, {6, 2}]
	,
	TestID -> "Symbol: True, First, CheckOption, CheckOptionRelations: \
2 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -10, opt2 -> -2}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -2]
	,
	TestID -> "Symbol: True, First, CheckOption, CheckOptionRelations: \
1 valid, 1 invalid"
]
Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> -2}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -2]
	,
	TestID -> "Symbol: True, First, CheckOption, CheckOptionRelations: \
1 valid, 1 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -2.5, opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, -2.5]
	,
	TestID -> "Symbol: True, First, CheckOption, CheckOptionRelations: \
2 invalid"
]
Test[
	validator[testFunc1, {opt2 -> "b", opt1 -> "a"}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, "b"]
	,
	TestID -> "Symbol: True, First, CheckOption, CheckOptionRelations: \
2 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1, opt2 -> -10}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -1]
	,
	TestID -> "Symbol: True, First, CheckOption, CheckOptionRelations: \
2 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> -10, opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -10]
	,
	TestID -> "Symbol: True, First, CheckOption, CheckOptionRelations: \
2 invalid, 1 duplicated, invalid rel (first dup)"
]


Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> 1, opt1 -> 2}]
	,
	True
	,
	TestID -> "Symbol: True, First, CheckOption, CheckOptionRelations: \
3 valid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 0.3, opt2 -> 4, opt1 -> 2}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, 0.3]
	,
	TestID -> "Symbol: True, First, CheckOption, CheckOptionRelations: \
2 valid, 1 invalid (first dup)"
]
Test[
	validator[testFunc1, {opt1 -> 5, opt1 -> -3.7, opt2 -> 22}]
	,
	True
	,
	TestID -> "Symbol: True, First, CheckOption, CheckOptionRelations: \
2 valid, 1 invalid (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> 0.2, opt1 -> 0.1, opt1 -> 0.3}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, 0.2]
	,
	TestID -> "Symbol: True, First, CheckOption, CheckOptionRelations: \
3 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 3.9, opt2 -> -2, opt1 -> "x"}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, 3.9]
	,
	TestID -> "Symbol: True, First, CheckOption, CheckOptionRelations: \
3 invalid, 1 duplicated, invalid rel"
]


Module[{unknownOpt, val},
	With[{unknownOptStr = SymbolName[unknownOpt]},
		Test[
			validator[testFunc1, {unknownOpt -> val, opt1 -> -10, opt2 -> -2}]
			,
			False
			,
			Message[CheckOption::optnf, unknownOptStr, testFunc1]
			,
			TestID ->
				"Symbol: True, First, CheckOption, CheckOptionRelations: \
1 unknown, 1 valid, 1 invalid"
		]
	]
]


Module[{leaked = False},
	Test[
		validator[testFunc3, {opt1 :> (leaked = True)}]
		,
		True
		,
		TestID -> "Symbol: True, First, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 valid: evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: True, First, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 valid: leaked"
	]
]
Module[{leaked = False},
	Test[
		validator[testFunc3,
			{opt1 :> (leaked = True; leaked), opt1 :> (leaked = True)}
		]
		,
		False
		,
		Message[testFunc3::invalidOpt1, (leaked = True; leaked)]
		,
		TestID -> "Symbol: True, First, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: True, First, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): leaked"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*First, CheckOption, None*)


BeginTestSection["Symbol: True, First, CheckOption, None"]


TestMatch[
	validator = generateValidator[Symbol][True, First, CheckOption, None]
	,
	_Function
	,
	TestID -> "Symbol: True, First, CheckOption, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[testFunc1, {opt1 -> -2}]
	,
	True
	,
	TestID -> "Symbol: True, First, CheckOption, None: 1 valid"
]
Test[
	validator[testFunc1, {opt1 -> 10}]
	,
	True
	,
	TestID -> "Symbol: True, First, CheckOption, None: 1 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -1]
	,
	TestID -> "Symbol: True, First, CheckOption, None: 1 invalid"
]
Test[
	validator[testFunc1, {opt2 -> -5}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -5]
	,
	TestID -> "Symbol: True, First, CheckOption, None: 1 invalid, invalid rel"
]


Test[
	validator[testFunc1, {opt1 -> 3, opt2 -> 7}]
	,
	True
	,
	TestID -> "Symbol: True, First, CheckOption, None: 2 valid"
]
Test[
	validator[testFunc1, {opt1 -> 6, opt2 -> 2}]
	,
	True
	,
	TestID -> "Symbol: True, First, CheckOption, None: 2 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -10, opt2 -> -2}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -2]
	,
	TestID -> "Symbol: True, First, CheckOption, None: 1 valid, 1 invalid"
]
Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> -2}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -2]
	,
	TestID -> "Symbol: True, First, CheckOption, None: \
1 valid, 1 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -2.5, opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, -2.5]
	,
	TestID -> "Symbol: True, First, CheckOption, None: 2 invalid"
]
Test[
	validator[testFunc1, {opt2 -> "b", opt1 -> "a"}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, "b"]
	,
	TestID -> "Symbol: True, First, CheckOption, None: \
2 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1, opt2 -> -10}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -1]
	,
	TestID -> "Symbol: True, First, CheckOption, None: \
2 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> -10, opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -10]
	,
	TestID -> "Symbol: True, First, CheckOption, None: \
2 invalid, 1 duplicated, invalid rel (first dup)"
]


Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> 1, opt1 -> 2}]
	,
	True
	,
	TestID -> "Symbol: True, First, CheckOption, None: \
3 valid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 0.3, opt2 -> 4, opt1 -> 2}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, 0.3]
	,
	TestID -> "Symbol: True, First, CheckOption, None: \
2 valid, 1 invalid (first dup)"
]
Test[
	validator[testFunc1, {opt1 -> 5, opt1 -> -3.7, opt2 -> 22}]
	,
	True
	,
	TestID -> "Symbol: True, First, CheckOption, None: \
2 valid, 1 invalid (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> 0.2, opt1 -> 0.1, opt1 -> 0.3}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, 0.2]
	,
	TestID -> "Symbol: True, First, CheckOption, None: \
3 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 3.9, opt2 -> -2, opt1 -> "x"}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, 3.9]
	,
	TestID -> "Symbol: True, First, CheckOption, None: \
3 invalid, 1 duplicated, invalid rel"
]


Module[{unknownOpt, val},
	With[{unknownOptStr = SymbolName[unknownOpt]},
		Test[
			validator[testFunc1, {unknownOpt -> val, opt1 -> -10, opt2 -> -2}]
			,
			False
			,
			Message[CheckOption::optnf, unknownOptStr, testFunc1]
			,
			TestID -> "Symbol: True, First, CheckOption, None: \
1 unknown, 1 valid, 1 invalid"
		]
	]
]


Module[{leaked = False},
	Test[
		validator[testFunc3, {opt1 :> (leaked = True)}]
		,
		True
		,
		TestID -> "Symbol: True, First, CheckOption, None: \
Evaluation leak: 1 valid: evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: True, First, CheckOption, None: \
Evaluation leak: 1 valid: leaked"
	]
]
Module[{leaked = False},
	Test[
		validator[testFunc3,
			{opt1 :> (leaked = True; leaked), opt1 :> (leaked = True)}
		]
		,
		False
		,
		Message[testFunc3::invalidOpt1, (leaked = True; leaked)]
		,
		TestID -> "Symbol: True, First, CheckOption, None: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: True, First, CheckOption, None: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): leaked"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*First, custom, custom*)


BeginTestSection["Symbol: True, First, custom, custom"]


TestMatch[
	validator =
		generateValidator[Symbol][
			True, First, customCheckOption, customCheckOptionRelations
		]
	,
	_Function
	,
	TestID -> "Symbol: True, First, custom, custom: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[testFunc1,
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		Message[customCheckOption::used, testFunc1, opt1, val1]
		,
		TestID -> "Symbol: True, First, custom, custom: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*First, custom, None*)


BeginTestSection["Symbol: True, First, custom, None"]


TestMatch[
	validator = generateValidator[Symbol][True, First, customCheckOption, None]
	,
	_Function
	,
	TestID -> "Symbol: True, First, custom, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[testFunc1,
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		Message[customCheckOption::used, testFunc1, opt1, val1]
		,
		TestID -> "Symbol: True, First, custom, None: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*Last, CheckOption, CheckOptionRelations*)


BeginTestSection["Symbol: True, Last, CheckOption, CheckOptionRelations"]


TestMatch[
	validator =
		generateValidator[Symbol][
			True, Last, CheckOption, CheckOptionRelations
		]
	,
	_Function
	,
	TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[testFunc1, {opt1 -> -2}]
	,
	True
	,
	TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
1 valid"
]
Test[
	validator[testFunc1, {opt1 -> 10}]
	,
	False
	,
	Message[testFunc1::invalidRelation, {10, 5}]
	,
	TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
1 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -1]
	,
	TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
1 invalid"
]
Test[
	validator[testFunc1, {opt2 -> -5}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -5]
	,
	TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
1 invalid, invalid rel"
]


Test[
	validator[testFunc1, {opt1 -> 3, opt2 -> 7}]
	,
	True
	,
	TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
2 valid"
]
Test[
	validator[testFunc1, {opt1 -> 6, opt2 -> 2}]
	,
	False
	,
	Message[testFunc1::invalidRelation, {6, 2}]
	,
	TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
2 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -10, opt2 -> -2}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -2]
	,
	TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
1 valid, 1 invalid"
]
Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> -2}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -2]
	,
	TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
1 valid, 1 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -2.5, opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, -2.5]
	,
	TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
2 invalid"
]
Test[
	validator[testFunc1, {opt2 -> "b", opt1 -> "a"}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, "b"]
	,
	TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
2 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1, opt2 -> -10}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -10]
	,
	TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
2 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> -10, opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -1]
	,
	TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
2 invalid, 1 duplicated, invalid rel (first dup)"
]


Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> 1, opt1 -> 2}]
	,
	False
	,
	Message[testFunc1::invalidRelation, {2, 1}]
	,
	TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
3 valid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 0.3, opt2 -> 4, opt1 -> 2}]
	,
	True
	,
	TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
2 valid, 1 invalid (first dup)"
]
Test[
	validator[testFunc1, {opt1 -> 5, opt1 -> -3.7, opt2 -> 22}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, -3.7]
	,
	TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
2 valid, 1 invalid (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> 0.2, opt1 -> 0.1, opt1 -> 0.3}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, 0.2]
	,
	TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
3 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 3.9, opt2 -> -2, opt1 -> "x"}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, "x"]
	,
	TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
3 invalid, 1 duplicated, invalid rel"
]


Module[{unknownOpt, val},
	With[{unknownOptStr = SymbolName[unknownOpt]},
		Test[
			validator[testFunc1, {unknownOpt -> val, opt1 -> -10, opt2 -> -2}]
			,
			False
			,
			Message[CheckOption::optnf, unknownOptStr, testFunc1]
			,
			TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
1 unknown, 1 valid, 1 invalid"
		]
	]
]


Module[{leaked = False},
	Test[
		validator[testFunc3, {opt1 :> (leaked = True)}]
		,
		True
		,
		TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 valid: evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 valid: leaked"
	]
]
Module[{leaked = False},
	Test[
		validator[testFunc3,
			{opt1 :> (leaked = True; leaked), opt1 :> (leaked = True)}
		]
		,
		True
		,
		TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: True, Last, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): leaked"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*Last, CheckOption, None*)


BeginTestSection["Symbol: True, Last, CheckOption, None"]


TestMatch[
	validator = generateValidator[Symbol][True, Last, CheckOption, None]
	,
	_Function
	,
	TestID -> "Symbol: True, Last, CheckOption, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[testFunc1, {opt1 -> -2}]
	,
	True
	,
	TestID -> "Symbol: True, Last, CheckOption, None: 1 valid"
]
Test[
	validator[testFunc1, {opt1 -> 10}]
	,
	True
	,
	TestID -> "Symbol: True, Last, CheckOption, None: 1 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -1]
	,
	TestID -> "Symbol: True, Last, CheckOption, None: 1 invalid"
]
Test[
	validator[testFunc1, {opt2 -> -5}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -5]
	,
	TestID -> "Symbol: True, Last, CheckOption, None: 1 invalid, invalid rel"
]


Test[
	validator[testFunc1, {opt1 -> 3, opt2 -> 7}]
	,
	True
	,
	TestID -> "Symbol: True, Last, CheckOption, None: 2 valid"
]
Test[
	validator[testFunc1, {opt1 -> 6, opt2 -> 2}]
	,
	True
	,
	TestID -> "Symbol: True, Last, CheckOption, None: 2 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -10, opt2 -> -2}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -2]
	,
	TestID -> "Symbol: True, Last, CheckOption, None: 1 valid, 1 invalid"
]
Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> -2}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -2]
	,
	TestID -> "Symbol: True, Last, CheckOption, None: \
1 valid, 1 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -2.5, opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, -2.5]
	,
	TestID -> "Symbol: True, Last, CheckOption, None: 2 invalid"
]
Test[
	validator[testFunc1, {opt2 -> "b", opt1 -> "a"}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, "b"]
	,
	TestID -> "Symbol: True, Last, CheckOption, None: 2 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1, opt2 -> -10}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -10]
	,
	TestID -> "Symbol: True, Last, CheckOption, None: \
2 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> -10, opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -1]
	,
	TestID -> "Symbol: True, Last, CheckOption, None: \
2 invalid, 1 duplicated, invalid rel (first dup)"
]


Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> 1, opt1 -> 2}]
	,
	True
	,
	TestID -> "Symbol: True, Last, CheckOption, None: \
3 valid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 0.3, opt2 -> 4, opt1 -> 2}]
	,
	True
	,
	TestID -> "Symbol: True, Last, CheckOption, None: \
2 valid, 1 invalid (first dup)"
]
Test[
	validator[testFunc1, {opt1 -> 5, opt1 -> -3.7, opt2 -> 22}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, -3.7]
	,
	TestID -> "Symbol: True, Last, CheckOption, None: \
2 valid, 1 invalid (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> 0.2, opt1 -> 0.1, opt1 -> 0.3}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, 0.2]
	,
	TestID -> "Symbol: True, Last, CheckOption, None: \
3 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 3.9, opt2 -> -2, opt1 -> "x"}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, "x"]
	,
	TestID -> "Symbol: True, Last, CheckOption, None: \
3 invalid, 1 duplicated, invalid rel"
]


Module[{unknownOpt, val},
	With[{unknownOptStr = SymbolName[unknownOpt]},
		Test[
			validator[testFunc1, {unknownOpt -> val, opt1 -> -10, opt2 -> -2}]
			,
			False
			,
			Message[CheckOption::optnf, unknownOptStr, testFunc1]
			,
			TestID -> "Symbol: True, Last, CheckOption, None: \
1 unknown, 1 valid, 1 invalid"
		]
	]
]


Module[{leaked = False},
	Test[
		validator[testFunc3, {opt1 :> (leaked = True)}]
		,
		True
		,
		TestID -> "Symbol: True, Last, CheckOption, None: \
Evaluation leak: 1 valid: evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: True, Last, CheckOption, None: \
Evaluation leak: 1 valid: leaked"
	]
]
Module[{leaked = False},
	Test[
		validator[testFunc3,
			{opt1 :> (leaked = True; leaked), opt1 :> (leaked = True)}
		]
		,
		True
		,
		TestID -> "Symbol: True, Last, CheckOption, None: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: True, Last, CheckOption, None: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): leaked"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*Last, custom, custom*)


BeginTestSection["Symbol: True, Last, custom, custom"]


TestMatch[
	validator =
		generateValidator[Symbol][
			True, Last, customCheckOption, customCheckOptionRelations
		]
	,
	_Function
	,
	TestID -> "Symbol: True, Last, custom, custom: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[testFunc1,
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		Message[customCheckOption::used, testFunc1, "opt1", val4]
		,
		TestID -> "Symbol: True, Last, custom, custom: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*Last, custom, None*)


BeginTestSection["Symbol: True, Last, custom, None"]


TestMatch[
	validator = generateValidator[Symbol][True, Last, customCheckOption, None]
	,
	_Function
	,
	TestID -> "Symbol: True, Last, custom, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[testFunc1,
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		Message[customCheckOption::used, testFunc1, "opt1", val4]
		,
		TestID -> "Symbol: True, Last, custom, None: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*All, CheckOption, CheckOptionRelations*)


BeginTestSection["Symbol: True, All, CheckOption, CheckOptionRelations"]


TestMatch[
	validator =
		generateValidator[Symbol][True, All, CheckOption, CheckOptionRelations]
	,
	_Function
	,
	TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[testFunc1, {opt1 -> -2}]
	,
	True
	,
	TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
1 valid"
]
Test[
	validator[testFunc1, {opt1 -> 10}]
	,
	False
	,
	Message[testFunc1::invalidRelation, {10, 5}]
	,
	TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
1 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -1]
	,
	TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
1 invalid"
]
Test[
	validator[testFunc1, {opt2 -> -5}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -5]
	,
	TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
1 invalid, invalid rel"
]


Test[
	validator[testFunc1, {opt1 -> 3, opt2 -> 7}]
	,
	True
	,
	TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
2 valid"
]
Test[
	validator[testFunc1, {opt1 -> 6, opt2 -> 2}]
	,
	False
	,
	Message[testFunc1::invalidRelation, {6, 2}]
	,
	TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
2 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -10, opt2 -> -2}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -2]
	,
	TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
1 valid, 1 invalid"
]
Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> -2}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -2]
	,
	TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
1 valid, 1 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -2.5, opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, -2.5]
	,
	TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
2 invalid"
]
Test[
	validator[testFunc1, {opt2 -> "b", opt1 -> "a"}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, "b"]
	,
	TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
2 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1, opt2 -> -10}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -1]
	,
	TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
2 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> -10, opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -10]
	,
	TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
2 invalid, 1 duplicated, invalid rel (first dup)"
]


Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> 1, opt1 -> 2}]
	,
	True
	,
	TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
3 valid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 0.3, opt2 -> 4, opt1 -> 2}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, 0.3]
	,
	TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
2 valid, 1 invalid (first dup)"
]
Test[
	validator[testFunc1, {opt1 -> 5, opt1 -> -3.7, opt2 -> 22}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, -3.7]
	,
	TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
2 valid, 1 invalid (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> 0.2, opt1 -> 0.1, opt1 -> 0.3}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, 0.2]
	,
	TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
3 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 3.9, opt2 -> -2, opt1 -> "x"}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, 3.9]
	,
	TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
3 invalid, 1 duplicated, invalid rel"
]


Module[{unknownOpt, val},
	With[{unknownOptStr = SymbolName[unknownOpt]},
		Test[
			validator[testFunc1, {unknownOpt -> val, opt1 -> -10, opt2 -> -2}]
			,
			False
			,
			Message[CheckOption::optnf, unknownOptStr, testFunc1]
			,
			TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
1 unknown, 1 valid, 1 invalid"
		]
	]
]


Module[{leaked = False},
	Test[
		validator[testFunc3, {opt1 :> (leaked = True)}]
		,
		True
		,
		TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 valid: evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 valid: leaked"
	]
]
Module[{leaked = False},
	Test[
		validator[testFunc3,
			{opt1 :> (leaked = True; leaked), opt1 :> (leaked = True)}
		]
		,
		False
		,
		Message[testFunc3::invalidOpt1, (leaked = True; leaked)]
		,
		TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: True, All, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): leaked"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*All, CheckOption, None*)


BeginTestSection["Symbol: True, All, CheckOption, None"]


TestMatch[
	validator = generateValidator[Symbol][True, All, CheckOption, None]
	,
	_Function
	,
	TestID -> "Symbol: True, All, CheckOption, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[testFunc1, {opt1 -> -2}]
	,
	True
	,
	TestID -> "Symbol: True, All, CheckOption, None: 1 valid"
]
Test[
	validator[testFunc1, {opt1 -> 10}]
	,
	True
	,
	TestID -> "Symbol: True, All, CheckOption, None: 1 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -1]
	,
	TestID -> "Symbol: True, All, CheckOption, None: 1 invalid"
]
Test[
	validator[testFunc1, {opt2 -> -5}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -5]
	,
	TestID -> "Symbol: True, All, CheckOption, None: 1 invalid, invalid rel"
]


Test[
	validator[testFunc1, {opt1 -> 3, opt2 -> 7}]
	,
	True
	,
	TestID -> "Symbol: True, All, CheckOption, None: 2 valid"
]
Test[
	validator[testFunc1, {opt1 -> 6, opt2 -> 2}]
	,
	True
	,
	TestID -> "Symbol: True, All, CheckOption, None: 2 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -10, opt2 -> -2}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -2]
	,
	TestID -> "Symbol: True, All, CheckOption, None: 1 valid, 1 invalid"
]
Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> -2}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -2]
	,
	TestID -> "Symbol: True, All, CheckOption, None: \
1 valid, 1 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -2.5, opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, -2.5]
	,
	TestID -> "Symbol: True, All, CheckOption, None: 2 invalid"
]
Test[
	validator[testFunc1, {opt2 -> "b", opt1 -> "a"}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, "b"]
	,
	TestID -> "Symbol: True, All, CheckOption, None: 2 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1, opt2 -> -10}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -1]
	,
	TestID -> "Symbol: True, All, CheckOption, None: \
2 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> -10, opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -10]
	,
	TestID -> "Symbol: True, All, CheckOption, None: \
2 invalid, 1 duplicated, invalid rel (first dup)"
]


Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> 1, opt1 -> 2}]
	,
	True
	,
	TestID -> "Symbol: True, All, CheckOption, None: 3 valid, 1 duplicated, \
invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 0.3, opt2 -> 4, opt1 -> 2}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, 0.3]
	,
	TestID -> "Symbol: True, All, CheckOption, None: \
2 valid, 1 invalid (first dup)"
]
Test[
	validator[testFunc1, {opt1 -> 5, opt1 -> -3.7, opt2 -> 22}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, -3.7]
	,
	TestID -> "Symbol: True, All, CheckOption, None: \
2 valid, 1 invalid (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> 0.2, opt1 -> 0.1, opt1 -> 0.3}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, 0.2]
	,
	TestID -> "Symbol: True, All, CheckOption, None: \
3 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 3.9, opt2 -> -2, opt1 -> "x"}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, 3.9]
	,
	TestID -> "Symbol: True, All, CheckOption, None: \
3 invalid, 1 duplicated, invalid rel"
]


Module[{unknownOpt, val},
	With[{unknownOptStr = SymbolName[unknownOpt]},
		Test[
			validator[testFunc1, {unknownOpt -> val, opt1 -> -10, opt2 -> -2}]
			,
			False
			,
			Message[CheckOption::optnf, unknownOptStr, testFunc1]
			,
			TestID -> "Symbol: True, All, CheckOption, None: \
1 unknown, 1 valid, 1 invalid"
		]
	]
]


Module[{leaked = False},
	Test[
		validator[testFunc3, {opt1 :> (leaked = True)}]
		,
		True
		,
		TestID -> "Symbol: True, All, CheckOption, None: \
Evaluation leak: 1 valid: evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: True, All, CheckOption, None: \
Evaluation leak: 1 valid: leaked"
	]
]
Module[{leaked = False},
	Test[
		validator[testFunc3,
			{opt1 :> (leaked = True; leaked), opt1 :> (leaked = True)}
		]
		,
		False
		,
		Message[testFunc3::invalidOpt1, (leaked = True; leaked)]
		,
		TestID -> "Symbol: True, All, CheckOption, None: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: True, All, CheckOption, None: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): leaked"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*All, custom, custom*)


BeginTestSection["Symbol: True, All, custom, custom"]


TestMatch[
	validator =
		generateValidator[Symbol][
			True, All, customCheckOption, customCheckOptionRelations
		]
	,
	_Function
	,
	TestID -> "Symbol: True, All, custom, custom: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[testFunc1,
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		Message[customCheckOption::used, testFunc1, opt1, val1]
		,
		TestID -> "Symbol: True, All, custom, custom: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*All, custom, None*)


BeginTestSection["Symbol: True, All, custom, None"]


TestMatch[
	validator = generateValidator[Symbol][True, All, customCheckOption, None]
	,
	_Function
	,
	TestID -> "Symbol: True, All, custom, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[testFunc1,
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		Message[customCheckOption::used, testFunc1, opt1, val1]
		,
		TestID -> "Symbol: True, All, custom, None: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Subsection:: *)
(*stopOnInvalid: False*)


(* ::Subsubsection:: *)
(*First, CheckOption, CheckOptionRelations*)


BeginTestSection["Symbol: False, First, CheckOption, CheckOptionRelations"]


TestMatch[
	validator =
		generateValidator[Symbol][
			False, First, CheckOption, CheckOptionRelations
		]
	,
	_Function
	,
	TestID -> "Symbol: False, First, CheckOption, CheckOptionRelations: \
validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[testFunc1, {opt1 -> -2}]
	,
	True
	,
	TestID -> "Symbol: False, First, CheckOption, CheckOptionRelations: \
1 valid"
]
Test[
	validator[testFunc1, {opt1 -> 10}]
	,
	False
	,
	Message[testFunc1::invalidRelation, {10, 5}]
	,
	TestID -> "Symbol: False, First, CheckOption, CheckOptionRelations: \
1 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -1]
	,
	TestID -> "Symbol: False, First, CheckOption, CheckOptionRelations: \
1 invalid"
]
Test[
	validator[testFunc1, {opt2 -> -5}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, -5],
		Message[testFunc1::invalidRelation, {-2, -5}]
	}
	,
	TestID -> "Symbol: False, First, CheckOption, CheckOptionRelations: \
1 invalid, invalid rel"
]


Test[
	validator[testFunc1, {opt1 -> 3, opt2 -> 7}]
	,
	True
	,
	TestID -> "Symbol: False, First, CheckOption, CheckOptionRelations: \
2 valid"
]
Test[
	validator[testFunc1, {opt1 -> 6, opt2 -> 2}]
	,
	False
	,
	Message[testFunc1::invalidRelation, {6, 2}]
	,
	TestID -> "Symbol: False, First, CheckOption, CheckOptionRelations: \
2 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -10, opt2 -> -2}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -2]
	,
	TestID -> "Symbol: False, First, CheckOption, CheckOptionRelations: \
1 valid, 1 invalid"
]
Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> -2}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, -2],
		Message[testFunc1::invalidRelation, {0, -2}]
	}
	,
	TestID -> "Symbol: False, First, CheckOption, CheckOptionRelations: \
1 valid, 1 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -2.5, opt2 -> -1}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt1, -2.5],
		Message[testFunc1::invalidOpt2, -1]
	}
	,
	TestID -> "Symbol: False, First, CheckOption, CheckOptionRelations: \
2 invalid"
]
Test[
	validator[testFunc1, {opt2 -> "b", opt1 -> "a"}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, "b"],
		Message[testFunc1::invalidOpt1, "a"],
		Message[testFunc1::invalidRelation, {"a", "b"}]
	}
	,
	TestID -> "Symbol: False, First, CheckOption, CheckOptionRelations: \
2 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1, opt2 -> -10}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -1]
	,
	TestID -> "Symbol: False, First, CheckOption, CheckOptionRelations: \
2 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> -10, opt2 -> -1}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, -10],
		Message[testFunc1::invalidRelation, {-2, -10}]
	}
	,
	TestID -> "Symbol: False, First, CheckOption, CheckOptionRelations: \
2 invalid, 1 duplicated, invalid rel (first dup)"
]


Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> 1, opt1 -> 2}]
	,
	True
	,
	TestID -> "Symbol: False, First, CheckOption, CheckOptionRelations: \
3 valid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 0.3, opt2 -> 4, opt1 -> 2}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, 0.3]
	,
	TestID -> "Symbol: False, First, CheckOption, CheckOptionRelations: \
2 valid, 1 invalid (first dup)"
]
Test[
	validator[testFunc1, {opt1 -> 5, opt1 -> -3.7, opt2 -> 22}]
	,
	True
	,
	TestID -> "Symbol: False, First, CheckOption, CheckOptionRelations: \
2 valid, 1 invalid (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> 0.2, opt1 -> 0.1, opt1 -> 0.3}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, 0.2],
		Message[testFunc1::invalidOpt1, 0.1]
	}
	,
	TestID -> "Symbol: False, First, CheckOption, CheckOptionRelations: \
3 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 3.9, opt2 -> -2, opt1 -> "x"}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt1, 3.9],
		Message[testFunc1::invalidOpt2, -2],
		Message[testFunc1::invalidRelation, {3.9, -2}]
	}
	,
	TestID -> "Symbol: False, First, CheckOption, CheckOptionRelations: \
3 invalid, 1 duplicated, invalid rel"
]


Module[{unknownOpt, val},
	With[{unknownOptStr = SymbolName[unknownOpt]},
		Test[
			validator[testFunc1, {unknownOpt -> val, opt1 -> -10, opt2 -> -2}]
			,
			False
			,
			{
				Message[CheckOption::optnf, unknownOptStr, testFunc1],
				Message[testFunc1::invalidOpt2, -2],
				(* OptionValue call in CheckOptionRelations gives: *)
				Message[OptionValue::nodef, unknownOptStr, testFunc1]
			}
			,
			TestID ->
				"Symbol: False, First, CheckOption, CheckOptionRelations: \
1 unknown, 1 valid, 1 invalid"
		]
	]
]


Module[{leaked = False},
	Test[
		validator[testFunc3, {opt1 :> (leaked = True)}]
		,
		True
		,
		TestID -> "Symbol: False, First, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 valid: evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: False, First, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 valid: leaked"
	]
]
Module[{leaked = False},
	Test[
		validator[testFunc3,
			{opt1 :> (leaked = True; leaked), opt1 :> (leaked = True)}
		]
		,
		False
		,
		Message[testFunc3::invalidOpt1, (leaked = True; leaked)]
		,
		TestID -> "Symbol: False, First, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: False, First, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): leaked"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*First, CheckOption, None*)


BeginTestSection["Symbol: False, First, CheckOption, None"]


TestMatch[
	validator = generateValidator[Symbol][False, First, CheckOption, None]
	,
	_Function
	,
	TestID -> "Symbol: False, First, CheckOption, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[testFunc1, {opt1 -> -2}]
	,
	True
	,
	TestID -> "Symbol: False, First, CheckOption, None: 1 valid"
]
Test[
	validator[testFunc1, {opt1 -> 10}]
	,
	True
	,
	TestID -> "Symbol: False, First, CheckOption, None: 1 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -1]
	,
	TestID -> "Symbol: False, First, CheckOption, None: 1 invalid"
]
Test[
	validator[testFunc1, {opt2 -> -5}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -5]
	,
	TestID -> "Symbol: False, First, CheckOption, None: \
1 invalid, invalid rel"
]


Test[
	validator[testFunc1, {opt1 -> 3, opt2 -> 7}]
	,
	True
	,
	TestID -> "Symbol: False, First, CheckOption, None: 2 valid"
]
Test[
	validator[testFunc1, {opt1 -> 6, opt2 -> 2}]
	,
	True
	,
	TestID -> "Symbol: False, First, CheckOption, None: 2 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -10, opt2 -> -2}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -2]
	,
	TestID -> "Symbol: False, First, CheckOption, None: 1 valid, 1 invalid"
]
Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> -2}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -2]
	,
	TestID -> "Symbol: False, First, CheckOption, None: \
1 valid, 1 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -2.5, opt2 -> -1}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt1, -2.5],
		Message[testFunc1::invalidOpt2, -1]
	}
	,
	TestID -> "Symbol: False, First, CheckOption, None: 2 invalid"
]
Test[
	validator[testFunc1, {opt2 -> "b", opt1 -> "a"}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, "b"],
		Message[testFunc1::invalidOpt1, "a"]
	}
	,
	TestID -> "Symbol: False, First, CheckOption, None: \
2 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1, opt2 -> -10}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -1]
	,
	TestID -> "Symbol: False, First, CheckOption, None: \
2 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> -10, opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -10]
	,
	TestID -> "Symbol: False, First, CheckOption, None: \
2 invalid, 1 duplicated, invalid rel (first dup)"
]


Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> 1, opt1 -> 2}]
	,
	True
	,
	TestID -> "Symbol: False, First, CheckOption, None: \
3 valid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 0.3, opt2 -> 4, opt1 -> 2}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, 0.3]
	,
	TestID -> "Symbol: False, First, CheckOption, None: \
2 valid, 1 invalid (first dup)"
]
Test[
	validator[testFunc1, {opt1 -> 5, opt1 -> -3.7, opt2 -> 22}]
	,
	True
	,
	TestID -> "Symbol: False, First, CheckOption, None: \
2 valid, 1 invalid (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> 0.2, opt1 -> 0.1, opt1 -> 0.3}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, 0.2],
		Message[testFunc1::invalidOpt1, 0.1]
	}
	,
	TestID -> "Symbol: False, First, CheckOption, None: \
3 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 3.9, opt2 -> -2, opt1 -> "x"}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt1, 3.9],
		Message[testFunc1::invalidOpt2, -2]
	}
	,
	TestID -> "Symbol: False, First, CheckOption, None: \
3 invalid, 1 duplicated, invalid rel"
]


Module[{unknownOpt, val},
	With[{unknownOptStr = SymbolName[unknownOpt]},
		Test[
			validator[testFunc1, {unknownOpt -> val, opt1 -> -10, opt2 -> -2}]
			,
			False
			,
			{
				Message[CheckOption::optnf, unknownOptStr, testFunc1],
				Message[testFunc1::invalidOpt2, -2]
			}
			,
			TestID -> "Symbol: False, First, CheckOption, None: \
1 unknown, 1 valid, 1 invalid"
		]
	]
]


Module[{leaked = False},
	Test[
		validator[testFunc3, {opt1 :> (leaked = True)}]
		,
		True
		,
		TestID -> "Symbol: False, First, CheckOption, None: \
Evaluation leak: 1 valid: evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: False, First, CheckOption, None: \
Evaluation leak: 1 valid: leaked"
	]
]
Module[{leaked = False},
	Test[
		validator[testFunc3,
			{opt1 :> (leaked = True; leaked), opt1 :> (leaked = True)}
		]
		,
		False
		,
		Message[testFunc3::invalidOpt1, (leaked = True; leaked)]
		,
		TestID -> "Symbol: False, First, CheckOption, None: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: False, First, CheckOption, None: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): leaked"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*First, custom, custom*)


BeginTestSection["Symbol: False, First, custom, custom"]


TestMatch[
	validator =
		generateValidator[Symbol][
			False, First, customCheckOption, customCheckOptionRelations
		]
	,
	_Function
	,
	TestID -> "Symbol: False, First, custom, custom: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[testFunc1,
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		{
			Message[customCheckOption::used, testFunc1, opt1, val1],
			Message[customCheckOption::used, testFunc1, opt2, val2],
			Message[customCheckOption::used, testFunc1, unknownOpt, val3],
			Message[customCheckOptionRelations::used, testFunc1,
				{opt1 :> val1, opt2 -> val2, unknownOpt -> val3}
			]
		}
		,
		TestID -> "Symbol: False, First, custom, custom: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*First, custom, None*)


BeginTestSection["Symbol: False, First, custom, None"]


TestMatch[
	validator =
		generateValidator[Symbol][False, First, customCheckOption, None]
	,
	_Function
	,
	TestID -> "Symbol: False, First, custom, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[testFunc1,
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		{
			Message[customCheckOption::used, testFunc1, opt1, val1],
			Message[customCheckOption::used, testFunc1, opt2, val2],
			Message[customCheckOption::used, testFunc1, unknownOpt, val3]
		}
		,
		TestID -> "Symbol: False, First, custom, None: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*Last, CheckOption, CheckOptionRelations*)


BeginTestSection["Symbol: False, Last, CheckOption, CheckOptionRelations"]


TestMatch[
	validator =
		generateValidator[Symbol][
			False, Last, CheckOption, CheckOptionRelations
		]
	,
	_Function
	,
	TestID -> "Symbol: False, Last, CheckOption, CheckOptionRelations: \
validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[testFunc1, {opt1 -> -2}]
	,
	True
	,
	TestID -> "Symbol: False, Last, CheckOption, CheckOptionRelations: \
1 valid"
]
Test[
	validator[testFunc1, {opt1 -> 10}]
	,
	False
	,
	Message[testFunc1::invalidRelation, {10, 5}]
	,
	TestID -> "Symbol: False, Last, CheckOption, CheckOptionRelations: \
1 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -1]
	,
	TestID -> "Symbol: False, Last, CheckOption, CheckOptionRelations: \
1 invalid"
]
Test[
	validator[testFunc1, {opt2 -> -5}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, -5],
		Message[testFunc1::invalidRelation, {-2, -5}]
	}
	,
	TestID -> "Symbol: False, Last, CheckOption, CheckOptionRelations: \
1 invalid, invalid rel"
]


Test[
	validator[testFunc1, {opt1 -> 3, opt2 -> 7}]
	,
	True
	,
	TestID -> "Symbol: False, Last, CheckOption, CheckOptionRelations: \
2 valid"
]
Test[
	validator[testFunc1, {opt1 -> 6, opt2 -> 2}]
	,
	False
	,
	Message[testFunc1::invalidRelation, {6, 2}]
	,
	TestID -> "Symbol: False, Last, CheckOption, CheckOptionRelations: \
2 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -10, opt2 -> -2}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -2]
	,
	TestID -> "Symbol: False, Last, CheckOption, CheckOptionRelations: \
1 valid, 1 invalid"
]
Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> -2}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, -2],
		Message[testFunc1::invalidRelation, {0, -2}]
	}
	,
	TestID -> "Symbol: False, Last, CheckOption, CheckOptionRelations: \
1 valid, 1 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -2.5, opt2 -> -1}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt1, -2.5],
		Message[testFunc1::invalidOpt2, -1]
	}
	,
	TestID -> "Symbol: False, Last, CheckOption, CheckOptionRelations: \
2 invalid"
]
Test[
	validator[testFunc1, {opt2 -> "b", opt1 -> "a"}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, "b"],
		Message[testFunc1::invalidOpt1, "a"],
		Message[testFunc1::invalidRelation, {"a", "b"}]
	}
	,
	TestID -> "Symbol: False, Last, CheckOption, CheckOptionRelations: \
2 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1, opt2 -> -10}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, -10],
		Message[testFunc1::invalidRelation, {-2, -10}]
	}
	,
	TestID -> "Symbol: False, Last, CheckOption, CheckOptionRelations: \
2 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> -10, opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -1]
	,
	TestID -> "Symbol: False, Last, CheckOption, CheckOptionRelations: \
2 invalid, 1 duplicated, invalid rel (first dup)"
]


Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> 1, opt1 -> 2}]
	,
	False
	,
	Message[testFunc1::invalidRelation, {2, 1}]
	,
	TestID -> "Symbol: False, Last, CheckOption, CheckOptionRelations: \
3 valid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 0.3, opt2 -> 4, opt1 -> 2}]
	,
	True
	,
	TestID -> "Symbol: False, Last, CheckOption, CheckOptionRelations: \
2 valid, 1 invalid (first dup)"
]
Test[
	validator[testFunc1, {opt1 -> 5, opt1 -> -3.7, opt2 -> 22}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, -3.7]
	,
	TestID -> "Symbol: False, Last, CheckOption, CheckOptionRelations: \
2 valid, 1 invalid (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> 0.2, opt1 -> 0.1, opt1 -> 0.3}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, 0.2],
		Message[testFunc1::invalidOpt1, 0.3],
		Message[testFunc1::invalidRelation, {0.3, 0.2}]
	}
	,
	TestID -> "Symbol: False, Last, CheckOption, CheckOptionRelations: \
3 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 3.9, opt2 -> -2, opt1 -> "x"}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt1, "x"],
		Message[testFunc1::invalidOpt2, -2],
		Message[testFunc1::invalidRelation, {"x", -2}]
	}
	,
	TestID -> "Symbol: False, Last, CheckOption, CheckOptionRelations: \
3 invalid, 1 duplicated, invalid rel"
]


Module[{unknownOpt, val},
	With[{unknownOptStr = SymbolName[unknownOpt]},
		Test[
			validator[testFunc1, {unknownOpt -> val, opt1 -> -10, opt2 -> -2}]
			,
			False
			,
			{
				Message[CheckOption::optnf, unknownOptStr, testFunc1],
				Message[testFunc1::invalidOpt2, -2],
				(* OptionValue call in CheckOptionRelations gives: *)
				Message[OptionValue::nodef, unknownOptStr, testFunc1]
			}
			,
			TestID ->
				"Symbol: False, Last, CheckOption, CheckOptionRelations: \
1 unknown, 1 valid, 1 invalid"
		]
	]
]


Module[{leaked = False},
	Test[
		validator[testFunc3, {opt1 :> (leaked = True)}]
		,
		True
		,
		TestID -> "Symbol: False, Last, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 valid: evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: False, Last, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 valid: leaked"
	]
]
Module[{leaked = False},
	Test[
		validator[testFunc3,
			{opt1 :> (leaked = True; leaked), opt1 :> (leaked = True)}
		]
		,
		True
		,
		TestID -> "Symbol: False, Last, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: False, Last, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): leaked"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*Last, CheckOption, None*)


BeginTestSection["Symbol: False, Last, CheckOption, None"]


TestMatch[
	validator = generateValidator[Symbol][False, Last, CheckOption, None]
	,
	_Function
	,
	TestID -> "Symbol: False, Last, CheckOption, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[testFunc1, {opt1 -> -2}]
	,
	True
	,
	TestID -> "Symbol: False, Last, CheckOption, None: 1 valid"
]
Test[
	validator[testFunc1, {opt1 -> 10}]
	,
	True
	,
	TestID -> "Symbol: False, Last, CheckOption, None: 1 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -1]
	,
	TestID -> "Symbol: False, Last, CheckOption, None: 1 invalid"
]
Test[
	validator[testFunc1, {opt2 -> -5}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -5]
	,
	TestID -> "Symbol: False, Last, CheckOption, None: 1 invalid, invalid rel"
]


Test[
	validator[testFunc1, {opt1 -> 3, opt2 -> 7}]
	,
	True
	,
	TestID -> "Symbol: False, Last, CheckOption, None: 2 valid"
]
Test[
	validator[testFunc1, {opt1 -> 6, opt2 -> 2}]
	,
	True
	,
	TestID -> "Symbol: False, Last, CheckOption, None: 2 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -10, opt2 -> -2}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -2]
	,
	TestID -> "Symbol: False, Last, CheckOption, None: 1 valid, 1 invalid"
]
Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> -2}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -2]
	,
	TestID -> "Symbol: False, Last, CheckOption, None: \
1 valid, 1 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -2.5, opt2 -> -1}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt1, -2.5],
		Message[testFunc1::invalidOpt2, -1]
	}
	,
	TestID -> "Symbol: False, Last, CheckOption, None: 2 invalid"
]
Test[
	validator[testFunc1, {opt2 -> "b", opt1 -> "a"}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, "b"],
		Message[testFunc1::invalidOpt1, "a"]
	}
	,
	TestID -> "Symbol: False, Last, CheckOption, None: 2 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1, opt2 -> -10}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -10]
	,
	TestID -> "Symbol: False, Last, CheckOption, None: \
2 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> -10, opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -1]
	,
	TestID -> "Symbol: False, Last, CheckOption, None: \
2 invalid, 1 duplicated, invalid rel (first dup)"
]


Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> 1, opt1 -> 2}]
	,
	True
	,
	TestID -> "Symbol: False, Last, CheckOption, None: \
3 valid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 0.3, opt2 -> 4, opt1 -> 2}]
	,
	True
	,
	TestID -> "Symbol: False, Last, CheckOption, None: \
2 valid, 1 invalid (first dup)"
]
Test[
	validator[testFunc1, {opt1 -> 5, opt1 -> -3.7, opt2 -> 22}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, -3.7]
	,
	TestID -> "Symbol: False, Last, CheckOption, None: \
2 valid, 1 invalid (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> 0.2, opt1 -> 0.1, opt1 -> 0.3}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, 0.2],
		Message[testFunc1::invalidOpt1, 0.3]
	}
	,
	TestID -> "Symbol: False, Last, CheckOption, None: \
3 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 3.9, opt2 -> -2, opt1 -> "x"}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt1, "x"],
		Message[testFunc1::invalidOpt2, -2]
	}
	,
	TestID -> "Symbol: False, Last, CheckOption, None: \
3 invalid, 1 duplicated, invalid rel"
]


Module[{unknownOpt, val},
	With[{unknownOptStr = SymbolName[unknownOpt]},
		Test[
			validator[testFunc1, {unknownOpt -> val, opt1 -> -10, opt2 -> -2}]
			,
			False
			,
			{
				Message[CheckOption::optnf, unknownOptStr, testFunc1],
				Message[testFunc1::invalidOpt2, -2]
			}
			,
			TestID -> "Symbol: False, Last, CheckOption, None: \
1 unknown, 1 valid, 1 invalid"
		]
	]
]


Module[{leaked = False},
	Test[
		validator[testFunc3, {opt1 :> (leaked = True)}]
		,
		True
		,
		TestID -> "Symbol: False, Last, CheckOption, None: \
Evaluation leak: 1 valid: evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: False, Last, CheckOption, None: \
Evaluation leak: 1 valid: leaked"
	]
]
Module[{leaked = False},
	Test[
		validator[testFunc3,
			{opt1 :> (leaked = True; leaked), opt1 :> (leaked = True)}
		]
		,
		True
		,
		TestID -> "Symbol: False, Last, CheckOption, None: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: False, Last, CheckOption, None: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): leaked"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*Last, custom, custom*)


BeginTestSection["Symbol: False, Last, custom, custom"]


TestMatch[
	validator =
		generateValidator[Symbol][
			False, Last, customCheckOption, customCheckOptionRelations
		]
	,
	_Function
	,
	TestID -> "Symbol: False, Last, custom, custom: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[testFunc1,
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		{
			Message[customCheckOption::used, testFunc1, "opt1", val4],
			Message[customCheckOption::used, testFunc1, opt2, val2],
			Message[customCheckOption::used, testFunc1, unknownOpt, val3],
			Message[customCheckOptionRelations::used, testFunc1,
				{"opt1" -> val4, opt2 -> val2, unknownOpt -> val3}
			]
		}
		,
		TestID -> "Symbol: False, Last, custom, custom: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*Last, custom, None*)


BeginTestSection["Symbol: False, Last, custom, None"]


TestMatch[
	validator = generateValidator[Symbol][False, Last, customCheckOption, None]
	,
	_Function
	,
	TestID -> "Symbol: False, Last, custom, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[testFunc1,
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		{
			Message[customCheckOption::used, testFunc1, "opt1", val4],
			Message[customCheckOption::used, testFunc1, opt2, val2],
			Message[customCheckOption::used, testFunc1, unknownOpt, val3]
		}
		,
		TestID -> "Symbol: False, Last, custom, None: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*All, CheckOption, CheckOptionRelations*)


BeginTestSection["Symbol: False, All, CheckOption, CheckOptionRelations"]


TestMatch[
	validator =
		generateValidator[Symbol][
			False, All, CheckOption, CheckOptionRelations
		]
	,
	_Function
	,
	TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[testFunc1, {opt1 -> -2}]
	,
	True
	,
	TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
1 valid"
]
Test[
	validator[testFunc1, {opt1 -> 10}]
	,
	False
	,
	Message[testFunc1::invalidRelation, {10, 5}]
	,
	TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
1 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -1]
	,
	TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
1 invalid"
]
Test[
	validator[testFunc1, {opt2 -> -5}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, -5],
		Message[testFunc1::invalidRelation, {-2, -5}]
	}
	,
	TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
1 invalid, invalid rel"
]


Test[
	validator[testFunc1, {opt1 -> 3, opt2 -> 7}]
	,
	True
	,
	TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
2 valid"
]
Test[
	validator[testFunc1, {opt1 -> 6, opt2 -> 2}]
	,
	False
	,
	Message[testFunc1::invalidRelation, {6, 2}]
	,
	TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
2 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -10, opt2 -> -2}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -2]
	,
	TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
1 valid, 1 invalid"
]
Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> -2}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, -2],
		Message[testFunc1::invalidRelation, {0, -2}]
	}
	,
	TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
1 valid, 1 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -2.5, opt2 -> -1}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt1, -2.5],
		Message[testFunc1::invalidOpt2, -1]
	}
	,
	TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
2 invalid"
]
Test[
	validator[testFunc1, {opt2 -> "b", opt1 -> "a"}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, "b"],
		Message[testFunc1::invalidOpt1, "a"],
		Message[testFunc1::invalidRelation, {"a", "b"}]
	}
	,
	TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
2 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1, opt2 -> -10}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, -1],
		Message[testFunc1::invalidOpt2, -10]
	}
	,
	TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
2 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> -10, opt2 -> -1}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, -10],
		Message[testFunc1::invalidOpt2, -1],
		Message[testFunc1::invalidRelation, {-2, -10}]
	}
	,
	TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
2 invalid, 1 duplicated, invalid rel (first dup)"
]


Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> 1, opt1 -> 2}]
	,
	True
	,
	TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
3 valid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 0.3, opt2 -> 4, opt1 -> 2}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, 0.3]
	,
	TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
2 valid, 1 invalid (first dup)"
]
Test[
	validator[testFunc1, {opt1 -> 5, opt1 -> -3.7, opt2 -> 22}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, -3.7]
	,
	TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
2 valid, 1 invalid (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> 0.2, opt1 -> 0.1, opt1 -> 0.3}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, 0.2],
		Message[testFunc1::invalidOpt1, 0.1],
		Message[testFunc1::invalidOpt1, 0.3]
	}
	,
	TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
3 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 3.9, opt2 -> -2, opt1 -> "x"}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt1, 3.9],
		Message[testFunc1::invalidOpt2, -2],
		Message[testFunc1::invalidOpt1, "x"],
		Message[testFunc1::invalidRelation, {3.9, -2}]
	}
	,
	TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
3 invalid, 1 duplicated, invalid rel"
]


Module[{unknownOpt, val},
	With[{unknownOptStr = SymbolName[unknownOpt]},
		Test[
			validator[testFunc1, {unknownOpt -> val, opt1 -> -10, opt2 -> -2}]
			,
			False
			,
			{
				Message[CheckOption::optnf, unknownOptStr, testFunc1],
				Message[testFunc1::invalidOpt2, -2],
				(* OptionValue call in CheckOptionRelations gives: *)
				Message[OptionValue::nodef, unknownOptStr, testFunc1]
			}
			,
			TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
1 unknown, 1 valid, 1 invalid"
		]
	]
]


Module[{leaked = False},
	Test[
		validator[testFunc3, {opt1 :> (leaked = True)}]
		,
		True
		,
		TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 valid: evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 valid: leaked"
	]
]
Module[{leaked = False},
	Test[
		validator[testFunc3,
			{opt1 :> (leaked = True; leaked), opt1 :> (leaked = True)}
		]
		,
		False
		,
		Message[testFunc3::invalidOpt1, (leaked = True; leaked)]
		,
		TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: False, All, CheckOption, CheckOptionRelations: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): leaked"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*All, CheckOption, None*)


BeginTestSection["Symbol: False, All, CheckOption, None"]


TestMatch[
	validator = generateValidator[Symbol][False, All, CheckOption, None]
	,
	_Function
	,
	TestID -> "Symbol: False, All, CheckOption, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[testFunc1, {opt1 -> -2}]
	,
	True
	,
	TestID -> "Symbol: False, All, CheckOption, None: 1 valid"
]
Test[
	validator[testFunc1, {opt1 -> 10}]
	,
	True
	,
	TestID -> "Symbol: False, All, CheckOption, None: 1 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -1]
	,
	TestID -> "Symbol: False, All, CheckOption, None: 1 invalid"
]
Test[
	validator[testFunc1, {opt2 -> -5}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -5]
	,
	TestID -> "Symbol: False, All, CheckOption, None: 1 invalid, invalid rel"
]


Test[
	validator[testFunc1, {opt1 -> 3, opt2 -> 7}]
	,
	True
	,
	TestID -> "Symbol: False, All, CheckOption, None: 2 valid"
]
Test[
	validator[testFunc1, {opt1 -> 6, opt2 -> 2}]
	,
	True
	,
	TestID -> "Symbol: False, All, CheckOption, None: 2 valid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -10, opt2 -> -2}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -2]
	,
	TestID -> "Symbol: False, All, CheckOption, None: 1 valid, 1 invalid"
]
Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> -2}]
	,
	False
	,
	Message[testFunc1::invalidOpt2, -2]
	,
	TestID -> "Symbol: False, All, CheckOption, None: \
1 valid, 1 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt1 -> -2.5, opt2 -> -1}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt1, -2.5],
		Message[testFunc1::invalidOpt2, -1]
	}
	,
	TestID -> "Symbol: False, All, CheckOption, None: 2 invalid"
]
Test[
	validator[testFunc1, {opt2 -> "b", opt1 -> "a"}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, "b"],
		Message[testFunc1::invalidOpt1, "a"]
	}
	,
	TestID -> "Symbol: False, All, CheckOption, None: 2 invalid, invalid rel"
]

Test[
	validator[testFunc1, {opt2 -> -1, opt2 -> -10}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, -1],
		Message[testFunc1::invalidOpt2, -10]
	}
	,
	TestID -> "Symbol: False, All, CheckOption, None: \
2 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> -10, opt2 -> -1}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, -10],
		Message[testFunc1::invalidOpt2, -1]
	}
	,
	TestID -> "Symbol: False, All, CheckOption, None: \
2 invalid, 1 duplicated, invalid rel (first dup)"
]


Test[
	validator[testFunc1, {opt1 -> 0, opt2 -> 1, opt1 -> 2}]
	,
	True
	,
	TestID -> "Symbol: False, All, CheckOption, None: \
3 valid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 0.3, opt2 -> 4, opt1 -> 2}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, 0.3]
	,
	TestID -> "Symbol: False, All, CheckOption, None: \
2 valid, 1 invalid (first dup)"
]
Test[
	validator[testFunc1, {opt1 -> 5, opt1 -> -3.7, opt2 -> 22}]
	,
	False
	,
	Message[testFunc1::invalidOpt1, -3.7]
	,
	TestID -> "Symbol: False, All, CheckOption, None: \
2 valid, 1 invalid (last dup)"
]
Test[
	validator[testFunc1, {opt2 -> 0.2, opt1 -> 0.1, opt1 -> 0.3}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt2, 0.2],
		Message[testFunc1::invalidOpt1, 0.1],
		Message[testFunc1::invalidOpt1, 0.3]
	}
	,
	TestID -> "Symbol: False, All, CheckOption, None: \
3 invalid, 1 duplicated, invalid rel (last dup)"
]
Test[
	validator[testFunc1, {opt1 -> 3.9, opt2 -> -2, opt1 -> "x"}]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt1, 3.9],
		Message[testFunc1::invalidOpt2, -2],
		Message[testFunc1::invalidOpt1, "x"]
	}
	,
	TestID -> "Symbol: False, All, CheckOption, None: \
3 invalid, 1 duplicated, invalid rel"
]


Module[{unknownOpt, val},
	With[{unknownOptStr = SymbolName[unknownOpt]},
		Test[
			validator[testFunc1, {unknownOpt -> val, opt1 -> -10, opt2 -> -2}]
			,
			False
			,
			{
				Message[CheckOption::optnf, unknownOptStr, testFunc1],
				Message[testFunc1::invalidOpt2, -2]
			}
			,
			TestID -> "Symbol: False, All, CheckOption, None: \
1 unknown, 1 valid, 1 invalid"
		]
	]
]


Module[{leaked = False},
	Test[
		validator[testFunc3, {opt1 :> (leaked = True)}]
		,
		True
		,
		TestID -> "Symbol: False, All, CheckOption, None: \
Evaluation leak: 1 valid: evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: False, All, CheckOption, None: \
Evaluation leak: 1 valid: leaked"
	]
]
Module[{leaked = False},
	Test[
		validator[testFunc3,
			{opt1 :> (leaked = True; leaked), opt1 :> (leaked = True)}
		]
		,
		False
		,
		Message[testFunc3::invalidOpt1, (leaked = True; leaked)]
		,
		TestID -> "Symbol: False, All, CheckOption, None: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "Symbol: False, All, CheckOption, None: \
Evaluation leak: 1 invalid (first dup), 1 valid (last dup): leaked"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*All, custom, custom*)


BeginTestSection["Symbol: False, All, custom, custom"]


TestMatch[
	validator =
		generateValidator[Symbol][
			False, All, customCheckOption, customCheckOptionRelations
		]
	,
	_Function
	,
	TestID -> "Symbol: False, All, custom, custom: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[testFunc1,
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		{
			Message[customCheckOption::used, testFunc1, opt1, val1],
			Message[customCheckOption::used, testFunc1, opt2, val2],
			Message[customCheckOption::used, testFunc1, unknownOpt, val3],
			Message[customCheckOption::used, testFunc1, "opt1", val4],
			Message[customCheckOptionRelations::used, testFunc1, {
				opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4
			}]
		}
		,
		TestID -> "Symbol: False, All, custom, custom: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*All, custom, None*)


BeginTestSection["Symbol: False, All, custom, None"]


TestMatch[
	validator = generateValidator[Symbol][False, All, customCheckOption, None]
	,
	_Function
	,
	TestID -> "Symbol: False, All, custom, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[testFunc1,
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		{
			Message[customCheckOption::used, testFunc1, opt1, val1],
			Message[customCheckOption::used, testFunc1, opt2, val2],
			Message[customCheckOption::used, testFunc1, unknownOpt, val3],
			Message[customCheckOption::used, testFunc1, "opt1", val4]
		}
		,
		TestID -> "Symbol: False, All, custom, None: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Section:: *)
(*Tests: List*)


(* ::Subsection:: *)
(*stopOnInvalid: True*)


(* ::Subsubsection:: *)
(*First, CheckOption, CheckOptionRelations*)


BeginTestSection["List: True, First, CheckOption, CheckOptionRelations"]


TestMatch[
	validator =
		generateValidator[List][True, First, CheckOption, CheckOptionRelations]
	,
	_Function
	,
	TestID -> "List: True, First, CheckOption, CheckOptionRelations: \
validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[{testFunc2, testFunc1},
		{opt1 -> E, opt2 -> -2, unknownOpt -> 1.3, opt1 -> "string"}
	]
	,
	False
	,
	Message[testFunc2::invalidOpt1, E]
	,
	TestID -> "List: True, First, CheckOption, CheckOptionRelations: \
1 invalid (first dup), 1 valid (last dup), 1 unknown: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
]


EndTestSection[]


(* ::Subsubsection:: *)
(*First, CheckOption, None*)


BeginTestSection["List: True, First, CheckOption, None"]


TestMatch[
	validator = generateValidator[List][True, First, CheckOption, None]
	,
	_Function
	,
	TestID -> "List: True, First, CheckOption, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[{testFunc2, testFunc1},
		{opt1 -> E, opt2 -> -2, unknownOpt -> 1.3, opt1 -> "string"}
	]
	,
	False
	,
	Message[testFunc2::invalidOpt1, E]
	,
	TestID -> "List: True, First, CheckOption, None: \
1 invalid (first dup), 1 valid (last dup), 1 unknown: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
]


EndTestSection[]


(* ::Subsubsection:: *)
(*First, custom, custom*)


BeginTestSection["List: True, First, custom, custom"]


TestMatch[
	validator =
		generateValidator[List][
			True, First, customCheckOption, customCheckOptionRelations
		]
	,
	_Function
	,
	TestID -> "List: True, First, custom, custom: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[{testFunc2, testFunc1},
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		Message[customCheckOption::used, testFunc2, opt1, val1]
		,
		TestID -> "List: True, First, custom, custom: \
2 invalid, 1 duplicated, 1 unknown, invalid rel: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*First, custom, None*)


BeginTestSection["List: True, First, custom, None"]


TestMatch[
	validator = generateValidator[List][True, First, customCheckOption, None]
	,
	_Function
	,
	TestID -> "List: True, First, custom, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[{testFunc2, testFunc1},
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		Message[customCheckOption::used, testFunc2, opt1, val1]
		,
		TestID -> "List: True, First, custom, None: \
2 invalid, 1 duplicated, 1 unknown, invalid rel: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*Last, CheckOption, CheckOptionRelations*)


BeginTestSection["List: True, Last, CheckOption, CheckOptionRelations"]


TestMatch[
	validator =
		generateValidator[List][True, Last, CheckOption, CheckOptionRelations]
	,
	_Function
	,
	TestID -> "List: True, Last, CheckOption, CheckOptionRelations: \
validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[{testFunc2, testFunc1},
		{opt1 -> E, opt2 -> -2, unknownOpt -> 1.3, opt1 -> "string"}
	]
	,
	False
	,
	Message[testFunc1::invalidOpt1, "string"]
	,
	TestID -> "List: True, Last, CheckOption, CheckOptionRelations: \
1 invalid (first dup), 1 valid (last dup), 1 unknown: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
]


EndTestSection[]


(* ::Subsubsection:: *)
(*Last, CheckOption, None*)


BeginTestSection["List: True, Last, CheckOption, None"]


TestMatch[
	validator = generateValidator[List][True, Last, CheckOption, None]
	,
	_Function
	,
	TestID -> "List: True, Last, CheckOption, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[{testFunc2, testFunc1},
		{opt1 -> E, opt2 -> -2, unknownOpt -> 1.3, opt1 -> "string"}
	]
	,
	False
	,
	Message[testFunc1::invalidOpt1, "string"]
	,
	TestID -> "List: True, Last, CheckOption, None: \
1 invalid (first dup), 1 valid (last dup), 1 unknown: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
]


EndTestSection[]


(* ::Subsubsection:: *)
(*Last, custom, custom*)


BeginTestSection["List: True, Last, custom, custom"]


TestMatch[
	validator =
		generateValidator[List][
			True, Last, customCheckOption, customCheckOptionRelations
		]
	,
	_Function
	,
	TestID -> "List: True, Last, custom, custom: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[{testFunc2, testFunc1},
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		Message[customCheckOption::used, testFunc2, "opt1", val4]
		,
		TestID -> "List: True, Last, custom, custom: \
2 invalid, 1 duplicated, 1 unknown, invalid rel: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*Last, custom, None*)


BeginTestSection["List: True, Last, custom, None"]


TestMatch[
	validator = generateValidator[List][True, Last, customCheckOption, None]
	,
	_Function
	,
	TestID -> "List: True, Last, custom, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[{testFunc2, testFunc1},
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		Message[customCheckOption::used, testFunc2, "opt1", val4]
		,
		TestID -> "List: True, Last, custom, None: \
2 invalid, 1 duplicated, 1 unknown, invalid rel: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*All, CheckOption, CheckOptionRelations*)


BeginTestSection["List: True, All, CheckOption, CheckOptionRelations"]


TestMatch[
	validator =
		generateValidator[List][True, All, CheckOption, CheckOptionRelations]
	,
	_Function
	,
	TestID -> "List: True, All, CheckOption, CheckOptionRelations: \
validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[{testFunc2, testFunc1},
		{opt1 -> E, opt2 -> -2, unknownOpt -> 1.3, opt1 -> "string"}
	]
	,
	False
	,
	Message[testFunc2::invalidOpt1, E]
	,
	TestID -> "List: True, All, CheckOption, CheckOptionRelations: \
1 invalid (first dup), 1 valid (last dup), 1 unknown: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
]


EndTestSection[]


(* ::Subsubsection:: *)
(*All, CheckOption, None*)


BeginTestSection["List: True, All, CheckOption, None"]


TestMatch[
	validator = generateValidator[List][True, All, CheckOption, None]
	,
	_Function
	,
	TestID -> "List: True, All, CheckOption, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[{testFunc2, testFunc1},
		{opt1 -> E, opt2 -> -2, unknownOpt -> 1.3, opt1 -> "string"}
	]
	,
	False
	,
	Message[testFunc2::invalidOpt1, E]
	,
	TestID -> "List: True, All, CheckOption, None: \
1 invalid (first dup), 1 valid (last dup), 1 unknown: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
]


EndTestSection[]


(* ::Subsubsection:: *)
(*All, custom, custom*)


BeginTestSection["List: True, All, custom, custom"]


TestMatch[
	validator =
		generateValidator[List][
			True, All, customCheckOption, customCheckOptionRelations
		]
	,
	_Function
	,
	TestID -> "List: True, All, custom, custom: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[{testFunc2, testFunc1},
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		Message[customCheckOption::used, testFunc2, opt1, val1]
		,
		TestID -> "List: True, All, custom, custom: \
2 invalid, 1 duplicated, 1 unknown, invalid rel: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*All, custom, None*)


BeginTestSection["List: True, All, custom, None"]


TestMatch[
	validator = generateValidator[List][True, All, customCheckOption, None]
	,
	_Function
	,
	TestID -> "List: True, All, custom, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[{testFunc2, testFunc1},
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		Message[customCheckOption::used, testFunc2, opt1, val1]
		,
		TestID -> "List: True, All, custom, None: \
2 invalid, 1 duplicated, 1 unknown, invalid rel: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Subsection:: *)
(*stopOnInvalid: False*)


(* ::Subsubsection:: *)
(*First, CheckOption, CheckOptionRelations*)


BeginTestSection["List: False, First, CheckOption, CheckOptionRelations"]


TestMatch[
	validator =
		generateValidator[List][
			False, First, CheckOption, CheckOptionRelations
		]
	,
	_Function
	,
	TestID -> "List: False, First, CheckOption, CheckOptionRelations: \
validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[{testFunc2, testFunc1},
		{opt1 -> E, opt2 -> -2, unknownOpt -> 1.3, opt1 -> "string"}
	]
	,
	False
	,
	{
		Message[testFunc2::invalidOpt1, E],
		Message[testFunc1::invalidOpt1, E],
		Message[testFunc1::invalidOpt2, -2],
		Message[testFunc1::invalidRelation, {E, -2}]
	}
	,
	TestID -> "List: False, First, CheckOption, CheckOptionRelations: \
1 invalid (first dup), 1 valid (last dup), 1 unknown: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
]


EndTestSection[]


(* ::Subsubsection:: *)
(*First, CheckOption, None*)


BeginTestSection["List: False, First, CheckOption, None"]


TestMatch[
	validator = generateValidator[List][False, First, CheckOption, None]
	,
	_Function
	,
	TestID -> "List: False, First, CheckOption, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[{testFunc2, testFunc1},
		{opt1 -> E, opt2 -> -2, unknownOpt -> 1.3, opt1 -> "string"}
	]
	,
	False
	,
	{
		Message[testFunc2::invalidOpt1, E],
		Message[testFunc1::invalidOpt1, E],
		Message[testFunc1::invalidOpt2, -2]
	}
	,
	TestID -> "List: False, First, CheckOption, None: \
1 invalid (first dup), 1 valid (last dup), 1 unknown: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
]


EndTestSection[]


(* ::Subsubsection:: *)
(*First, custom, custom*)


BeginTestSection["List: False, First, custom, custom"]


TestMatch[
	validator =
		generateValidator[List][
			False, First, customCheckOption, customCheckOptionRelations
		]
	,
	_Function
	,
	TestID -> "List: False, First, custom, custom: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[{testFunc2, testFunc1},
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		{
			Message[customCheckOption::used, testFunc2, opt1, val1],
			Message[customCheckOptionRelations::used, testFunc2,
				{opt1 :> val1}
			],
			Message[customCheckOption::used, testFunc1, opt1, val1],
			Message[customCheckOption::used, testFunc1, opt2, val2],
			Message[customCheckOptionRelations::used, testFunc1,
				{opt1 :> val1, opt2 -> val2}
			]
		}
		,
		TestID -> "List: False, First, custom, custom: \
2 invalid, 1 duplicated, 1 unknown, invalid rel: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*First, custom, None*)


BeginTestSection["List: False, First, custom, None"]


TestMatch[
	validator = generateValidator[List][False, First, customCheckOption, None]
	,
	_Function
	,
	TestID -> "List: False, First, custom, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[{testFunc2, testFunc1},
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		{
			Message[customCheckOption::used, testFunc2, opt1, val1],
			Message[customCheckOption::used, testFunc1, opt1, val1],
			Message[customCheckOption::used, testFunc1, opt2, val2]
		}
		,
		TestID -> "List: False, First, custom, None: \
2 invalid, 1 duplicated, 1 unknown, invalid rel: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*Last, CheckOption, CheckOptionRelations*)


BeginTestSection["List: False, Last, CheckOption, CheckOptionRelations"]


TestMatch[
	validator =
		generateValidator[List][False, Last, CheckOption, CheckOptionRelations]
	,
	_Function
	,
	TestID -> "List: False, Last, CheckOption, CheckOptionRelations: \
validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[{testFunc2, testFunc1},
		{opt1 -> E, opt2 -> -2, unknownOpt -> 1.3, opt1 -> "string"}
	]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt1, "string"],
		Message[testFunc1::invalidOpt2, -2],
		Message[testFunc1::invalidRelation, {"string", -2}]
	}
	,
	TestID -> "List: False, Last, CheckOption, CheckOptionRelations: \
1 invalid (first dup), 1 valid (last dup), 1 unknown: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
]


EndTestSection[]


(* ::Subsubsection:: *)
(*Last, CheckOption, None*)


BeginTestSection["List: False, Last, CheckOption, None"]


TestMatch[
	validator = generateValidator[List][False, Last, CheckOption, None]
	,
	_Function
	,
	TestID -> "List: False, Last, CheckOption, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[{testFunc2, testFunc1},
		{opt1 -> E, opt2 -> -2, unknownOpt -> 1.3, opt1 -> "string"}
	]
	,
	False
	,
	{
		Message[testFunc1::invalidOpt1, "string"],
		Message[testFunc1::invalidOpt2, -2]
	}
	,
	TestID -> "List: False, Last, CheckOption, None: \
1 invalid (first dup), 1 valid (last dup), 1 unknown: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
]


EndTestSection[]


(* ::Subsubsection:: *)
(*Last, custom, custom*)


BeginTestSection["List: False, Last, custom, custom"]


TestMatch[
	validator =
		generateValidator[List][
			False, Last, customCheckOption, customCheckOptionRelations
		]
	,
	_Function
	,
	TestID -> "List: False, Last, custom, custom: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[{testFunc2, testFunc1},
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		{
			Message[customCheckOption::used, testFunc2, "opt1", val4],
			Message[customCheckOptionRelations::used, testFunc2,
				{"opt1" -> val4}
			],
			Message[customCheckOption::used, testFunc1, "opt1", val4],
			Message[customCheckOption::used, testFunc1, opt2, val2],
			Message[customCheckOptionRelations::used, testFunc1,
				{"opt1" -> val4, opt2 -> val2}
			]
		}
		,
		TestID -> "List: False, Last, custom, custom: \
2 invalid, 1 duplicated, 1 unknown, invalid rel: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*Last, custom, None*)


BeginTestSection["List: False, Last, custom, None"]


TestMatch[
	validator = generateValidator[List][False, Last, customCheckOption, None]
	,
	_Function
	,
	TestID -> "List: False, Last, custom, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[{testFunc2, testFunc1},
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		{
			Message[customCheckOption::used, testFunc2, "opt1", val4],
			Message[customCheckOption::used, testFunc1, "opt1", val4],
			Message[customCheckOption::used, testFunc1, opt2, val2]
		}
		,
		TestID -> "List: False, Last, custom, None: \
2 invalid, 1 duplicated, 1 unknown, invalid rel: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*All, CheckOption, CheckOptionRelations*)


BeginTestSection["List: False, All, CheckOption, CheckOptionRelations"]


TestMatch[
	validator =
		generateValidator[List][False, All, CheckOption, CheckOptionRelations]
	,
	_Function
	,
	TestID -> "List: False, All, CheckOption, CheckOptionRelations: \
validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[{testFunc2, testFunc1},
		{opt1 -> E, opt2 -> -2, unknownOpt -> 1.3, opt1 -> "string"}
	]
	,
	False
	,
	{
		Message[testFunc2::invalidOpt1, E],
		Message[testFunc1::invalidOpt1, E],
		Message[testFunc1::invalidOpt2, -2],
		Message[testFunc1::invalidOpt1, "string"],
		Message[testFunc1::invalidRelation, {E, -2}]
	}
	,
	TestID -> "List: False, All, CheckOption, CheckOptionRelations: \
1 invalid (first dup), 1 valid (last dup), 1 unknown: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
]


EndTestSection[]


(* ::Subsubsection:: *)
(*All, CheckOption, None*)


BeginTestSection["List: False, All, CheckOption, None"]


TestMatch[
	validator = generateValidator[List][False, All, CheckOption, None]
	,
	_Function
	,
	TestID -> "List: False, All, CheckOption, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Test[
	validator[{testFunc2, testFunc1},
		{opt1 -> E, opt2 -> -2, unknownOpt -> 1.3, opt1 -> "string"}
	]
	,
	False
	,
	{
		Message[testFunc2::invalidOpt1, E],
		Message[testFunc1::invalidOpt1, E],
		Message[testFunc1::invalidOpt2, -2],
		Message[testFunc1::invalidOpt1, "string"]
	}
	,
	TestID -> "List: False, All, CheckOption, None: \
1 invalid (first dup), 1 valid (last dup), 1 unknown: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
]


EndTestSection[]


(* ::Subsubsection:: *)
(*All, custom, custom*)


BeginTestSection["List: False, All, custom, custom"]


TestMatch[
	validator =
		generateValidator[List][
			False, All, customCheckOption, customCheckOptionRelations
		]
	,
	_Function
	,
	TestID -> "List: False, All, custom, custom: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[{testFunc2, testFunc1},
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		{
			Message[customCheckOption::used, testFunc2, opt1, val1],
			Message[customCheckOption::used, testFunc2, "opt1", val4],
			Message[customCheckOptionRelations::used, testFunc2,
				{opt1 :> val1, "opt1" -> val4}
			],
			Message[customCheckOption::used, testFunc1, opt1, val1],
			Message[customCheckOption::used, testFunc1, opt2, val2],
			Message[customCheckOption::used, testFunc1, "opt1", val4],
			Message[customCheckOptionRelations::used, testFunc1,
				{opt1 :> val1, opt2 -> val2, "opt1" -> val4}
			]
		}
		,
		TestID -> "List: False, All, custom, custom: \
2 invalid, 1 duplicated, 1 unknown, invalid rel: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*All, custom, None*)


BeginTestSection["List: False, All, custom, None"]


TestMatch[
	validator = generateValidator[List][False, All, customCheckOption, None]
	,
	_Function
	,
	TestID -> "List: False, All, custom, None: validator generation",
	TestFailureAction -> "SkipSection"
]


Module[{unknownOpt, val1, val2, val3, val4},
	Test[
		validator[{testFunc2, testFunc1},
			{opt1 :> val1, opt2 -> val2, unknownOpt -> val3, "opt1" -> val4}
		]
		,
		False
		,
		{
			Message[customCheckOption::used, testFunc2, opt1, val1],
			Message[customCheckOption::used, testFunc2, "opt1", val4],
			Message[customCheckOption::used, testFunc1, opt1, val1],
			Message[customCheckOption::used, testFunc1, opt2, val2],
			Message[customCheckOption::used, testFunc1, "opt1", val4]
		}
		,
		TestID -> "List: False, All, custom, None: \
2 invalid, 1 duplicated, 1 unknown, invalid rel: \
3 invalid, 1 duplicated, 1 unknown, invalid rel"
	]
]


EndTestSection[]


(* ::Section:: *)
(*TearDown*)


On[General::stop]


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
