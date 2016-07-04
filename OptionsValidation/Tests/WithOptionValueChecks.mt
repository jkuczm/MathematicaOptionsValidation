(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["OptionsValidation`Tests`WithOptionValueChecks`", {"MUnit`"}]


Get["OptionsValidation`"]


Options[testFuncA] = {"opt1" :> defaultValid1, opt2 -> defaultValid2}

CheckOption[testFuncA, opt1] =
	# === defaultValid1 || # === valid1 ||
		Message[testFuncA::opt1, HoldForm["opt1"], HoldForm[#]] &

CheckOption[testFuncA, "opt2", val : Except[valid2 | defaultValid2]] :=
	Message[testFuncA::opt2, HoldForm[opt2], HoldForm[val]]


Options[testFuncB] = {opt1 :> 1 + 2, "opt2" :> 5 * 6}

CheckOption[testFuncB, name : "opt1" | opt2] =
	Function[val,
		Length@Unevaluated[val] === 2 ||
			Message[MessageName[testFuncB, name],
				HoldForm[name], HoldForm[val]
			],
		HoldFirst
	]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*0 args*)


Test[
	WithOptionValueChecks[] // Hold[#]&
	,
	WithOptionValueChecks[] // Hold
	,
	Message[WithOptionValueChecks::argt, WithOptionValueChecks, 0, 1, 2]
	,
	TestID -> "0 args"
]


(* ::Subsection:: *)
(*1 arg*)


(* ::Subsubsection:: *)
(*Held expression*)


Test[
	WithOptionValueChecks@Hold[OptionValue]
	,
	Hold[OptionValue]
	,
	TestID -> "1 arg: Held expression: OptionValue symbol"
]
Module[{a, b, c},
	Test[
		WithOptionValueChecks@Hold[OptionValue[a, b, c]]
		,
		Hold[CheckedOptionValue@OptionValue[a, b, c]]
		,
		TestID -> "1 arg: Held expression: OptionValue basic"
	]
]
Module[{a, b, c, d, e, f},
	Test[
		WithOptionValueChecks@Hold[OptionValue[a, b][c][d, e, f]]
		,
		Hold[CheckedOptionValue[OptionValue[a, b]][c][d, e, f]]
		,
		TestID -> "1 arg: Held expression: OptionValue in head"
	]
]
Module[{a, b, c, d},
	Test[
		WithOptionValueChecks@Hold[
			OptionValue[
				OptionValue[a],
				OptionValue[
					OptionValue[a, b, c],
					OptionValue[a, b, c, d]
				]
			]
		]
		,
		Hold[
			CheckedOptionValue@OptionValue[
				CheckedOptionValue@OptionValue[a],
				CheckedOptionValue@OptionValue[
					CheckedOptionValue@OptionValue[a, b, c],
					CheckedOptionValue@OptionValue[a, b, c, d]
				]
			]
		]
		,
		TestID -> "1 arg: Held expression: OptionValue nested"
	]
]
Test[
	WithOptionValueChecks@Hold@Module[{a, b, c, d, e, f, g, h, i, j},
		a = OptionValue[b];
		f[a];
		OptionValue;
		OptionValue[c, d, e] +
			OptionValue[g, h, i,
				OptionValue[j]
			];
		OptionValue[a, b][c, d]
	]
	,
	Hold@Module[{a, b, c, d, e, f, g, h, i, j},
		a = CheckedOptionValue@OptionValue[b];
		f[a];
		OptionValue;
		CheckedOptionValue@OptionValue[c, d, e] +
			CheckedOptionValue@OptionValue[g, h, i,
				CheckedOptionValue@OptionValue[j]
			];
		CheckedOptionValue[OptionValue[a, b]][c, d]
	]
	,
	TestID -> "1 arg: Held expression: complex code"
]


Module[{f, g, check},
	Test[
		WithOptionValueChecks[
			Hold@Block[{a, b, c, d},
				c =
					f[
						OptionValue,
						OptionValue[a, b]
					];
				OptionValue[a, b, c, d][
					OptionValue[
						g@OptionValue[a],
						b
					]
				]
			],
			CheckOption -> check
		]
		,
		Hold@Block[{a, b, c, d},
			c =
				f[
					OptionValue,
					CheckedOptionValue[OptionValue[a, b], CheckOption -> check]
				];
			CheckedOptionValue[OptionValue[a, b, c, d], CheckOption -> check][
				CheckedOptionValue[OptionValue[
					g@CheckedOptionValue[OptionValue[a], CheckOption -> check],
					b
				], CheckOption -> check]
			]
		]
		,
		TestID -> "1 arg: Held expression: custom CheckOption"
	]
]


(* ::Subsubsection:: *)
(*Flow*)


Module[{optVal, wrapper, before = False, after = False},
	Test[
		WithOptionValueChecks[
			before = True;
			optVal = OptionValue[testFuncA, {opt2 -> valid2}, opt2, wrapper];
			after = True
		]
		,
		True
		,
		TestID -> "1 arg: Flow: valid: evaluation"
	];
	
	Test[
		before
		,
		True
		,
		TestID -> "1 arg: Flow: valid: code before OptionValue"
	];
	Test[
		optVal
		,
		wrapper[valid2]
		,
		TestID -> "1 arg: Flow: valid: returned OptionValue"
	];
	Test[
		after
		,
		True
		,
		TestID -> "1 arg: Flow: valid: code after OptionValue"
	]
]


Module[{optVal, before = False, after = False},
	Test[
		WithOptionValueChecks[
			before = True;
			optVal =
				OptionValue[
					{opt1 -> defaultInvalid1, testFuncA},
					{"opt1", "opt2"}
				];
			after = True
		]
		,
		$Failed
		,
		Message[testFuncA::opt1, "opt1", defaultInvalid1]
		,
		TestID -> "1 arg: Flow: invalid: evaluation"
	];
	Test[
		before
		,
		True
		,
		TestID -> "1 arg: Flow: invalid: code before OptionValue"
	];
	Test[
		optVal // Hold[#]&
		,
		optVal // Hold
		,
		TestID -> "1 arg: Flow: invalid: returned OptionValue"
	];
	Test[
		after
		,
		False
		,
		TestID -> "1 arg: Flow: invalid: code after OptionValue"
	]
]


(* ::Subsubsection:: *)
(*OptionValue arguments from variable*)


Test[
	Replace[testFuncA[opt2 -> valid2],
		testFuncA@OptionsPattern[] :>
			WithOptionValueChecks@Module[{optList = {"opt2", opt1}},
				OptionValue[optList]
			]
	]
	,
	{valid2, defaultValid1}
	,
	TestID -> "1 arg: OptionValue arguments from variable: valid"
]


Test[
	WithOptionValueChecks@Module[
		{args = Sequence[{opt1 -> invalid1, testFuncA}, opt1]}
		,
		OptionValue[args]
	]
	,
	$Failed
	,
	Message[testFuncA::opt1, "opt1", invalid1]
	,
	TestID -> "1 arg: OptionValue arguments from variable: invalid"
]


(* ::Subsubsection:: *)
(*Evaluation leak*)


Module[{leaked = False},
	Test[
		WithOptionValueChecks@OptionValue[
			testFuncB, "opt1" :> {leaked = True}, opt1, HoldComplete
		]
		,
		$Failed
		,
		Message[testFuncB::opt1, "opt1", {leaked = True}]
		,
		TestID -> "1 arg: Evaluation leak: evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "1 arg: Evaluation leak: leaked"
	]
]


Module[{leaked1 = False, leaked2 = False, ioh},
	SetAttributes[ioh, HoldRest];
	Test[
		WithOptionValueChecks[
			OptionValue[
				{"opt2" :> {leaked2 = True}, testFuncB},
				opt1 :> {leaked1 = True}, {"opt1", opt2}
			],
			InvalidOptionHandler -> ioh
		]
		,
		ioh[testFuncB, "opt1", {leaked1 = True}]
		,
		Message[testFuncB::opt1, "opt1", {leaked1 = True}]
		,
		TestID -> "1 arg: Evaluation leak: custom invalid option handler: \
evaluation"
	];
	Test[
		leaked1
		,
		False
		,
		TestID -> "1 arg: Evaluation leak: custom invalid option handler: \
leaked1"
	];
	Test[
		leaked2
		,
		False
		,
		TestID -> "1 arg: Evaluation leak: custom invalid option handler: \
leaked2"
	]
]


(* ::Subsection:: *)
(*2 args*)


(* ::Subsubsection:: *)
(*Held expression*)


Module[{f},
	Test[
		WithOptionValueChecks[Hold[OptionValue], f]
		,
		Hold[OptionValue]
		,
		TestID -> "2 args: Held expression: OptionValue symbol"
	]
]
Module[{a, b, c, d, f, g},
	Test[
		WithOptionValueChecks[Hold[OptionValue[a, b, c, d]], {f, g}]
		,
		Hold[CheckedOptionValue[OptionValue[a, b, c, d], {f, g}]]
		,
		TestID -> "2 args: Held expression: OptionValue basic"
	]
]
Module[{a, b, c, f},
	Test[
		WithOptionValueChecks[Hold[OptionValue[a][b, c]], f]
		,
		Hold[CheckedOptionValue[OptionValue[a], f][b, c]]
		,
		TestID -> "2 args: Held expression: OptionValue in head"
	]
]
Module[{a, b, c, f, g, h},
	Test[
		WithOptionValueChecks[
			Hold[
				OptionValue[
					OptionValue[a, b,
						OptionValue[
							OptionValue[c]
						]
					],
					OptionValue[a, b]
				]
			],
			{f, g, h}
		]
		,
		Hold[
			CheckedOptionValue[OptionValue[
				CheckedOptionValue[OptionValue[a, b,
					CheckedOptionValue[OptionValue[
						CheckedOptionValue[OptionValue[c], {f, g, h}]
					], {f, g, h}]
				], {f, g, h}],
				CheckedOptionValue[OptionValue[a, b], {f, g, h}]
			], {f, g, h}]
		]
		,
		TestID -> "2 args: Held expression: OptionValue nested"
	]
]
Module[{f},
	Test[
		WithOptionValueChecks[
			Hold@Block[{a, b, c, d},
				f[OptionValue[a, b, c][d]];
				OptionValue[c,
					OptionValue[d]
				] * OptionValue
			],
			f
		]
		,
		Hold@Block[{a, b, c, d},
			f[CheckedOptionValue[OptionValue[a, b, c], f][d]];
			CheckedOptionValue[OptionValue[c,
				CheckedOptionValue[OptionValue[d], f]
			], f] * OptionValue
		]
		,
		TestID -> "2 args: Held expression: complex code"
	]
]


Module[{f, g, check},
	Test[
		WithOptionValueChecks[
			Hold@Block[{a, b, c, d},
				OptionValue[a, b] +
					OptionValue[a, b, c, d]
			],
			{f, g},
			"CheckOption" :> check
		]
		,
		Hold@Block[{a, b, c, d},
			CheckedOptionValue[OptionValue[a, b], {f, g},
				CheckOption -> check
			] +
				CheckedOptionValue[OptionValue[a, b, c, d], {f, g},
					CheckOption -> check
				]
		]
		,
		TestID -> "2 args: Held expression: custom CheckOption"
	]
]


Test[
	WithOptionValueChecks[
		Hold@Module[{a, b, c},
			OptionValue[a, b, c];
			OptionValue[a]
		],
		{}
	]
	,
	Hold@Module[{a, b, c},
		OptionValue[a, b, c];
		OptionValue[a]
	]
	,
	TestID -> "2 args: Held expression: empty list"
]


(* ::Subsubsection:: *)
(*Flow*)


Module[{optVal, before = False, after = False},
	Test[
		WithOptionValueChecks[
			before = True;
			optVal =
				OptionValue[
					{"opt1" -> defaultValid1, opt2 :> defaultValid2},
					{"opt2", opt1}
				];
			after = True
			,
			testFuncA
		]
		,
		True
		,
		TestID -> "2 args: Flow: valid: evaluation"
	];
	
	Test[
		before
		,
		True
		,
		TestID -> "2 args: Flow: valid: code before OptionValue"
	];
	Test[
		optVal
		,
		{defaultValid2, defaultValid1}
		,
		TestID -> "2 args: Flow: valid: returned OptionValue"
	];
	Test[
		after
		,
		True
		,
		TestID -> "2 args: Flow: valid: code after OptionValue"
	]
]


Module[{optVal, before = False, after = False},
	Test[
		Replace[testFuncA["opt1" :> valid1],
			testFuncA@OptionsPattern[] :>
				WithOptionValueChecks[
					before = True;
					optVal = OptionValue[opt1];
					after = True
					,
					{testFuncA, testFuncB}
				]
		]
		,
		$Failed
		,
		Message[testFuncB::opt1, "opt1", valid1]
		,
		TestID -> "2 args: Flow: invalid: evaluation"
	];
	Test[
		before
		,
		True
		,
		TestID -> "2 args: Flow: invalid: code before OptionValue"
	];
	Test[
		optVal // Hold[#]&
		,
		optVal // Hold
		,
		TestID -> "2 args: Flow: invalid: returned OptionValue"
	];
	Test[
		after
		,
		False
		,
		TestID -> "2 args: Flow: invalid: code after OptionValue"
	]
]


(* ::Subsubsection:: *)
(*OptionValue arguments from variable*)


Test[
	WithOptionValueChecks[
		Module[
			{
				args =
					Sequence[
						"opt1" -> defaultInvalid1, opt1 :> (2 + 2; valid1),
						opt1
					]
			},
			OptionValue[args]
		],
		{testFuncB, testFuncA}
	]
	,
	valid1
	,
	TestID -> "2 args: OptionValue arguments from variable: valid"
]

Test[
	WithOptionValueChecks[
		Replace[testFuncB[opt1 -> valid1],
			testFuncB@OptionsPattern[
				{opt1 :> defaultInvalid1, "opt2" -> defaultInvalid2}
			] :>
				Module[{optList = {opt1, opt2}},
					OptionValue[optList]
				]
		],
		testFuncA
	]
	,
	$Failed
	,
	Message[testFuncA::opt2, opt2, defaultInvalid2]
	,
	TestID -> "2 args: OptionValue arguments from variable: invalid"
]


(* ::Subsubsection:: *)
(*Evaluation leak*)


Module[{leaked1 = False, leaked2 = False},
	Test[
		WithOptionValueChecks[
			OptionValue[
				{opt2 :> {leaked2 = True; invalid2}, testFuncA},
				{opt1 :> (leaked1 = True)}, {opt1, "opt2"}, Hold
			],
			testFuncB
		]
		,
		$Failed
		,
		Message[testFuncB::opt2, "opt2", {leaked2 = True; invalid2}]
		,
		TestID -> "2 args: Evaluation leak: evaluation"
	];
	Test[
		leaked1
		,
		False
		,
		TestID -> "2 args: Evaluation leak: leaked1"
	];
	Test[
		leaked2
		,
		False
		,
		TestID -> "2 args: Evaluation leak: leaked2"
	]
]


Module[{leaked = False, ioh},
	SetAttributes[ioh, HoldRest];
	Test[
		Replace[testFuncA[opt2 :> {leaked = True; invalid2}],
			testFuncA@OptionsPattern[] :>
				WithOptionValueChecks[
					OptionValue[opt2],
					{testFuncB, testFuncA},
					InvalidOptionHandler -> ioh
				]
		]
		,
		ioh[testFuncB, opt2, {leaked = True; invalid2}]
		,
		Message[testFuncB::opt2, "opt2", {leaked = True; invalid2}]
		,
		TestID -> "2 args: Evaluation leak: custom invalid option handler: \
evaluation"
	];
	Test[
		leaked
		,
		False
		,
		TestID -> "2 args: Evaluation leak: custom invalid option handler: \
leaked"
	]
]


(* ::Subsubsection:: *)
(*Invalid*)


Module[{code, val},
	Test[
		WithOptionValueChecks[code, "testOpt" :> val] // Hold[#]&
		,
		WithOptionValueChecks[code, "testOpt" :> val] // Hold
		,
		Message[CheckOption::optnf, "testOpt", WithOptionValueChecks]
		,
		TestID -> "2 args: invalid: symbol, unknown option"
	]
]
Module[{code},
	Test[
		WithOptionValueChecks[code, "str"] // Hold[#]&
		,
		WithOptionValueChecks[code, "str"] // Hold
		,
		Message[WithOptionValueChecks::symse,
			2, WithOptionValueChecks[code, "str"]
		]
		,
		TestID -> "2 args second invalid: symbol, string"
	]
]


(* ::Subsection:: *)
(*3 args*)


Module[{code, f, g, opt, val},
	With[{optName = SymbolName[opt]},
		Test[
			WithOptionValueChecks[code, {f, g}, opt -> val] //
				Hold[#]&
			,
			WithOptionValueChecks[code, {f, g}, opt -> val] //
				Hold
			,
			Message[CheckOption::optnf, optName, WithOptionValueChecks]
			,
			TestID -> "3 args: symbol, list of symbols, unknown option"
		]
	]
]
Module[{code, f, g, opt, val},
	Test[
		WithOptionValueChecks[code, {f, -3.1, g}, opt -> val] // Hold[#]&
		,
		WithOptionValueChecks[code, {f, -3.1, g}, opt -> val] // Hold
		,
		Message[WithOptionValueChecks::symse,
			2, WithOptionValueChecks[code, {f, -3.1, g}, opt -> val]
		]
		,
		TestID -> "3 args: symbol, list with non-symbol, unknown option"
	]
]
Module[{code, nonOpt},
	Test[
		WithOptionValueChecks[code, 0, nonOpt] // Hold[#]&
		,
		WithOptionValueChecks[code, 0, nonOpt] // Hold
		,
		{
			Message[WithOptionValueChecks::symse,
				2, WithOptionValueChecks[code, 0, nonOpt]
			],
			Message[WithOptionValueChecks::nonopt,
				nonOpt, 2, WithOptionValueChecks[code, 0, nonOpt]
			]
		}
		,
		TestID -> "3 args: symbol, integer, non-option"
	]
]


(* ::Subsection:: *)
(*4 args*)


Module[{arg2, val1, val3, val4},
	Test[
		WithOptionValueChecks[
			"opt1" -> val1, arg2, "opt3" -> val3, "opt4" :> val4
		] // Hold[#]&
		,
		WithOptionValueChecks[
			"opt1" -> val1, arg2, "opt3" -> val3, "opt4" :> val4
		] // Hold
		,
		{
			Message[CheckOption::optnf, "opt3", WithOptionValueChecks],
			Message[CheckOption::optnf, "opt4", WithOptionValueChecks]
		}
		,
		TestID -> "4 args: option, symbol, 2 options"
	]
]
Module[{arg1, arg2, arg3, arg4},
	Test[
		WithOptionValueChecks[arg1, arg2, arg3, arg4] // Hold[#]&
		,
		WithOptionValueChecks[arg1, arg2, arg3, arg4] // Hold
		,
		Message[WithOptionValueChecks::nonopt,
			arg4, 2, WithOptionValueChecks[arg1, arg2, arg3, arg4]
		]
		,
		TestID -> "4 args: 4 symbols"
	]
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
