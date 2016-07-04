(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["OptionsValidation`Tests`QuietValidOptionsPattern`", {"MUnit`"}]


Get["OptionsValidation`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Returned pattern*)


(* ::Subsubsection:: *)
(*Default options*)


Block[{OptionsValidation`Private`generateValidator},
	Module[{f},
		TestMatch[
			(*	QuietValidOptionsPattern returns a Condition, this brakes code
				of Test, leading to an error. Looks like a bug in Test.
				Wrapping result with List to avoid it. *)
			QuietValidOptionsPattern[f] // List
			,
			Verbatim[Condition][
				Verbatim[Pattern][
					optsPattName_, Verbatim[OptionsPattern][f]
				],
				HoldPattern@Quiet@
					OptionsValidation`Private`generateValidator[Symbol][
						True, First, CheckOption, CheckOptionRelations
					][f, Flatten@{optsPattName_}]
			] // List
			,
			TestID -> "Pattern: Default options: symbol"
		]
	];
	Module[{f},
		TestMatch[
			QuietValidOptionsPattern[f, {f::x, f::y}] // List
			,
			Verbatim[Condition][
				Verbatim[Pattern][
					optsPattName_, Verbatim[OptionsPattern][f]
				],
				HoldPattern@Function[Null, Quiet[#, {f::x, f::y}], HoldFirst]@
					OptionsValidation`Private`generateValidator[Symbol][
						True, First, CheckOption, CheckOptionRelations
					][f, Flatten@{optsPattName_}]
			] // List
			,
			TestID -> "Pattern: Default options: symbol, messages"
		]
	];
	
	Module[{f},
		TestMatch[
			QuietValidOptionsPattern[{f}] // List
			,
			Verbatim[Condition][
				Verbatim[Pattern][
					optsPattName_, Verbatim[OptionsPattern][{f}]
				],
				HoldPattern@Quiet@
					OptionsValidation`Private`generateValidator[List][
						True, First, CheckOption, CheckOptionRelations
					][{f}, Flatten@{optsPattName_}]
			] // List
			,
			TestID -> "Pattern: Default options: list"
		]
	];
	Module[{a, b, f},
		TestMatch[
			QuietValidOptionsPattern[{f, a :> b}, f::x, f::x] // List
			,
			Verbatim[Condition][
				Verbatim[Pattern][
					optsPattName_, Verbatim[OptionsPattern][{f, a :> b}]
				],
				HoldPattern@Function[Null, Quiet[#, f::x, f::x], HoldFirst]@
					OptionsValidation`Private`generateValidator[List][
						True, First, CheckOption, CheckOptionRelations
					][{f}, Flatten@{optsPattName_}]
			] // List
			,
			TestID -> "Pattern: Default options: list, messages"
		]
	];
	
	Module[{f, g},
		TestMatch[
			QuietValidOptionsPattern[f, g] // List
			,
			Verbatim[Condition][
				Verbatim[Pattern][
					optsPattName_, Verbatim[OptionsPattern][f]
				],
				HoldPattern@Quiet@
					OptionsValidation`Private`generateValidator[Symbol][
						True, First, CheckOption, CheckOptionRelations
					][g, Flatten@{optsPattName_}]
			] // List
			,
			TestID -> "Pattern: Default options: symbol, symbol"
		]
	];
	Module[{a, b, c, d, f, g},
		TestMatch[
			QuietValidOptionsPattern[{a -> b, c -> d}, {f, g}, {f::x}] // List
			,
			Verbatim[Condition][
				Verbatim[Pattern][
					optsPattName_, Verbatim[OptionsPattern][{a -> b, c -> d}]
				],
				HoldPattern@Function[Null, Quiet[#, {f::x}], HoldFirst]@
					OptionsValidation`Private`generateValidator[List][
						True, First, CheckOption, CheckOptionRelations
					][{f, g}, Flatten@{optsPattName_}]
			] // List
			,
			TestID -> "Pattern: Default options: list, list, messages"
		]
	]
]


Module[{a, b},
	Test[
		QuietValidOptionsPattern[a -> b] // List
		,
		OptionsPattern[a -> b] // List
		,
		Message[QuietValidOptionsPattern::unknownOptionOwner, a -> b]
		,
		TestID -> "Pattern: Default options: no option owner: implicit"
	]
]
Module[{a, b, f, g},
	Test[
		QuietValidOptionsPattern[{{f, a :> b}, g}, {}] // List
		,
		OptionsPattern[{{f, a :> b}, g}] // List
		,
		TestID -> "Pattern: Default options: no option owner: explicit"
	]
]


(* ::Subsubsection:: *)
(*Explicit valid options*)


Do[
	With[
		{
			stopOnInvalid = stopOnInvalid,
			duplicate = duplicate,
			failureMessage =
				StringJoin@Riffle[ToString /@ {stopOnInvalid, duplicate}, ", "]
		}
		,
		Block[{OptionsValidation`Private`generateValidator},
			Module[{f, check, checkRel},
				TestMatch[
					QuietValidOptionsPattern[f,
						ChecksStopOnInvalid -> stopOnInvalid,
						"CheckOptionRelations" -> checkRel,
						CheckedDuplicate -> duplicate,
						CheckOption :> check
					] // List
					,
					Verbatim[Condition][
						Verbatim[Pattern][
							optsPattName_, Verbatim[OptionsPattern][f]
						],
						HoldPattern@Quiet@
							OptionsValidation`Private`generateValidator[Symbol]
								[stopOnInvalid, duplicate, check, checkRel]
								[f, Flatten@{optsPattName_}]
					] // List
					,
					TestID -> "Pattern: Explicit valid options: symbol",
					TestFailureMessage -> failureMessage
				]
			];
			Module[{f, invalidStopOnInvalid, check, checkRel},
				TestMatch[
					QuietValidOptionsPattern[f, "Spelling", f::x,
						CheckOption -> check,
						"ChecksStopOnInvalid" -> stopOnInvalid,
						CheckOptionRelations -> checkRel,
						{
							ChecksStopOnInvalid -> invalidStopOnInvalid,
							CheckedDuplicate :> duplicate
						}
					] // List
					,
					Verbatim[Condition][
						Verbatim[Pattern][
							optsPattName_, Verbatim[OptionsPattern][f]
						],
						HoldPattern@Function[
							Null, Quiet[#, "Spelling", f::x], HoldFirst
						]@
							OptionsValidation`Private`generateValidator[Symbol]
								[stopOnInvalid, duplicate, check, checkRel]
								[f, Flatten@{optsPattName_}]
					] // List
					,
					TestID ->
						"Pattern: Explicit valid options: symbol, messages",
					TestFailureMessage -> failureMessage
				]
			];
			
			Module[{f, g, check, check2, checkRel},
				TestMatch[
					QuietValidOptionsPattern[{f, g},
						"CheckedDuplicate" -> duplicate,
						{{
							ChecksStopOnInvalid -> stopOnInvalid,
							"CheckOption" :> check,
							CheckOptionRelations -> checkRel
						}},
						CheckedDuplicate :> All,
						"CheckOption" -> check2
					] // List
					,
					Verbatim[Condition][
						Verbatim[Pattern][
							optsPattName_, Verbatim[OptionsPattern][{f, g}]
						],
						HoldPattern@Quiet@
							OptionsValidation`Private`generateValidator[List]
								[stopOnInvalid, duplicate, check, checkRel]
								[{f, g}, Flatten@{optsPattName_}]
					] // List
					,
					TestID -> "Pattern: Explicit valid options: list",
					TestFailureMessage -> failureMessage
				]
			];
			Module[{f, g, h, i, check, checkRel},
				TestMatch[
					QuietValidOptionsPattern[{f, g, h, i}, {f::x, h::x},
						{
							CheckOptionRelations :> checkRel,
							CheckOption -> check,
							ChecksStopOnInvalid -> stopOnInvalid,
							CheckedDuplicate -> duplicate
						}
					] // List
					,
					Verbatim[Condition][
						Verbatim[Pattern][
							optsPattName_,
							Verbatim[OptionsPattern][{f, g, h, i}]
						],
						HoldPattern@Function[
							Null, Quiet[#, {f::x, h::x}], HoldFirst
						]@
							OptionsValidation`Private`generateValidator[List]
								[stopOnInvalid, duplicate, check, checkRel]
								[{f, g, h, i}, Flatten@{optsPattName_}]
					] // List
					,
					TestID ->
						"Pattern: Explicit valid options: list, messages",
					TestFailureMessage -> failureMessage
				]
			];
			
			Module[{f, g, check, checkRel},
				TestMatch[
					QuietValidOptionsPattern[f, {g},
						CheckOptionRelations -> checkRel,
						CheckedDuplicate :> duplicate,
						{
							"ChecksStopOnInvalid" -> stopOnInvalid, 
							CheckOption -> check
						}
					] // List
					,
					Verbatim[Condition][
						Verbatim[Pattern][
							optsPattName_, Verbatim[OptionsPattern][f]
						],
						HoldPattern@Quiet@
							OptionsValidation`Private`generateValidator[List]
								[stopOnInvalid, duplicate, check, checkRel]
								[{g}, Flatten@{optsPattName_}]
					] // List
					,
					TestID -> "Pattern: Explicit valid options: symbol, list",
					TestFailureMessage -> failureMessage
				]
			];
			Module[{a, b, f, check, checkRel, invalidStopOnInvalid},
				TestMatch[
					QuietValidOptionsPattern[a :> b, f, None, f::x,
						"ChecksStopOnInvalid" :> stopOnInvalid,
						"CheckOption" -> check,
						{
							"CheckOptionRelations" -> checkRel,
							"CheckedDuplicate" -> duplicate
						},
						"ChecksStopOnInvalid" -> invalidStopOnInvalid
					] // List
					,
					Verbatim[Condition][
						Verbatim[Pattern][
							optsPattName_,
							Verbatim[OptionsPattern][a :> b]
						],
						HoldPattern@Function[
							Null, Quiet[#, None, f::x], HoldFirst
						]@
							OptionsValidation`Private`generateValidator[Symbol]
								[stopOnInvalid, duplicate, check, checkRel]
								[f, Flatten@{optsPattName_}]
					] // List
					,
					TestID -> "Pattern: Explicit valid options: \
rule, symbol, messages",
					TestFailureMessage -> failureMessage
				]
			]
		]
	],
	{stopOnInvalid, {True, False}},
	{duplicate, {First, Last, All}}
]


(* ::Subsubsection:: *)
(*Explicit invalid options*)


Module[{f, checkRel, invalid},
	Test[
		QuietValidOptionsPattern[f,
			{CheckOptionRelations -> checkRel, "CheckedDuplicate" -> First},
			ChecksStopOnInvalid -> invalid
		] // Hold[#]&
		,
		QuietValidOptionsPattern[f,
			{CheckOptionRelations -> checkRel, "CheckedDuplicate" -> First},
			ChecksStopOnInvalid -> invalid
		] // Hold
		,
		Message[QuietValidOptionsPattern::opttf, ChecksStopOnInvalid, invalid]
		,
		TestID -> "Pattern: Explicit invalid options: symbol"
	]
]
Module[{a, b, f, g, check, invalid1, invalid2},
	Test[
		QuietValidOptionsPattern[{a -> b, f}, g,
			CheckedDuplicate -> invalid1,
			ChecksStopOnInvalid :> invalid2,
			{ChecksStopOnInvalid -> False, CheckOption -> check},
			ChecksStopOnInvalid -> True
		] // Hold[#]&
		,
		QuietValidOptionsPattern[{a -> b, f}, g,
			CheckedDuplicate -> invalid1,
			ChecksStopOnInvalid :> invalid2,
			{ChecksStopOnInvalid -> False, CheckOption -> check},
			ChecksStopOnInvalid -> True
		] // Hold
		,
		{
			Message[QuietValidOptionsPattern::optfla,
				CheckedDuplicate, invalid1
			],
			Message[QuietValidOptionsPattern::opttf,
				ChecksStopOnInvalid, invalid2
			]
		}
		,
		TestID -> "Pattern: Explicit invalid options: list, symbol"
	]
]
Module[{f, g, checkRel, invalid1, invalid2},
	Test[
		QuietValidOptionsPattern[{f, g}, g::x,
			"ChecksStopOnInvalid" -> True,
			CheckedDuplicate -> invalid1,
			{{CheckOptionRelations -> checkRel}},
			ChecksStopOnInvalid :> invalid2
		] // Hold[#]&
		,
		QuietValidOptionsPattern[{f, g}, g::x,
			"ChecksStopOnInvalid" -> True,
			CheckedDuplicate -> invalid1,
			{{CheckOptionRelations -> checkRel}},
			ChecksStopOnInvalid :> invalid2
		] // Hold
		,
		Message[QuietValidOptionsPattern::optfla, CheckedDuplicate, invalid1]
		,
		TestID -> "Pattern: Explicit invalid options: list, messages"
	]
]


(* ::Subsection:: *)
(*Invalid arguments*)


Test[
	QuietValidOptionsPattern[] // Hold[#]&
	,
	QuietValidOptionsPattern[] // Hold
	,
	Message[QuietValidOptionsPattern::argb, QuietValidOptionsPattern, 0, 1, 4]
	,
	TestID -> "Invalid arguments: no args"
]


Module[{f, g, h, val},
	Test[
		QuietValidOptionsPattern[{f, g, h}, "unknownTestOpt" -> val] //
			Hold[#]&
		,
		QuietValidOptionsPattern[{f, g, h}, "unknownTestOpt" -> val] //
			Hold
		,
		Message[CheckOption::optnf, "unknownTestOpt", QuietValidOptionsPattern]
		,
		TestID -> "Invalid arguments: list of symbols, unknown option"
	]
]
Module[{f},
	Test[
		QuietValidOptionsPattern[f, 17] // Hold[#]&
		,
		QuietValidOptionsPattern[f, 17] // Hold
		,
		Message[QuietValidOptionsPattern::symse,
			2, QuietValidOptionsPattern[f, 17]
		]
		,
		TestID -> "Invalid arguments: symbol, integer"
	]
]


Module[{a, b, opt, val},
	With[{optName = SymbolName[opt]},
		Test[
			QuietValidOptionsPattern[{a -> b}, "str", opt :> val] // Hold[#]&
			,
			QuietValidOptionsPattern[{a -> b}, "str", opt :> val] // Hold
			,
			Message[CheckOption::optnf, optName, QuietValidOptionsPattern]
			,
			TestID -> "Invalid arguments: default spec, string, unknown option"
		]
	]
]
Module[{f, g, h},
	Test[
		QuietValidOptionsPattern[{f}, g, h] // Hold[#]&
		,
		QuietValidOptionsPattern[{f}, g, h] // Hold
		,
		Message[QuietValidOptionsPattern::anmlist,
			3, QuietValidOptionsPattern[{f}, g, h]
		]
		,
		TestID -> "Invalid arguments: list of symbols, 2 symbols"
	]
]
Module[{a, b, f, g, h},
	Test[
		QuietValidOptionsPattern[{a :> b, f}, g::x, h] // Hold[#]&
		,
		QuietValidOptionsPattern[{a :> b, f}, g::x, h] // Hold
		,
		Message[QuietValidOptionsPattern::anmlist,
			3, QuietValidOptionsPattern[{a :> b, f}, g::x, h]
		]
		,
		TestID -> "Invalid arguments: deafult spec, message name, symbol"
	]
]


Module[{f, g, h},
	Test[
		QuietValidOptionsPattern[f, {g, h}, All, "testOpt" :> 2.3] // Hold[#]&
		,
		QuietValidOptionsPattern[f, {g, h}, All, "testOpt" :> 2.3] // Hold
		,
		Message[CheckOption::optnf, "testOpt", QuietValidOptionsPattern]
		,
		TestID -> "Invalid arguments: \
symbol, list of symbols, All, unknown option"
	]
]
Module[{f, g, h},
	Test[
		QuietValidOptionsPattern[{f, g}, "str", h::x, None] // Hold[#]&
		,
		QuietValidOptionsPattern[{f, g}, "str", h::x, None] // Hold
		,
		Message[QuietValidOptionsPattern::symse,
			2, QuietValidOptionsPattern[{f, g}, "str", h::x, None]
		]
		,
		TestID -> "Invalid arguments: \
list of symbols, string, message name, None"
	]
]
Module[{f, g, h, a, b},
	Test[
		QuietValidOptionsPattern[f, a :> b, g, h] // Hold[#]&
		,
		QuietValidOptionsPattern[f, a :> b, g, h] // Hold
		,
		{
			Message[QuietValidOptionsPattern::symse,
				2, QuietValidOptionsPattern[f, a :> b, g, h]
			],
			Message[QuietValidOptionsPattern::anmlist,
				3, QuietValidOptionsPattern[f, a :> b, g, h]
			],
			Message[QuietValidOptionsPattern::anmlist,
				4, QuietValidOptionsPattern[f, a :> b, g, h]
			]
		}
		,
		TestID -> "Invalid arguments: symbol, option, 2 symbols"
	]
]


Off[General::stop]
Module[{a, b, c, d, e},
	Test[
		QuietValidOptionsPattern[
			"opt1" -> a, "opt2" -> b, "opt3" -> c, "opt4" -> d, "opt5" -> e
		] // Hold[#]&
		,
		QuietValidOptionsPattern[
			"opt1" -> a, "opt2" -> b, "opt3" -> c, "opt4" -> d, "opt5" -> e
		] // Hold
		,
		{
			Message[CheckOption::optnf, "opt2", QuietValidOptionsPattern],
			Message[CheckOption::optnf, "opt3", QuietValidOptionsPattern],
			Message[CheckOption::optnf, "opt4", QuietValidOptionsPattern],
			Message[CheckOption::optnf, "opt5", QuietValidOptionsPattern]
		}
		,
		TestID -> "Invalid arguments: \
symbol, list with non-symbol, option, non-option, option"
	]
]
On[General::stop]


Module[{a, b, c, d, e, f, g, h, nonOption},
	Test[
		QuietValidOptionsPattern[
			f, {g, "str", h}, a :> a, b -> b, c -> c, nonOption, d -> d, e -> e
		] // Hold[#]&
		,
		QuietValidOptionsPattern[
			f, {g, "str", h}, a :> a, b -> b, c -> c, nonOption, d -> d, e -> e
		] // Hold
		,
		{
			Message[QuietValidOptionsPattern::symse,
				2,
				QuietValidOptionsPattern[f, {g, "str", h},
					a :> a, b -> b, c -> c, nonOption, d -> d, e -> e
				]
			],
			Message[QuietValidOptionsPattern::anmlist,
				3,
				QuietValidOptionsPattern[f, {g, "str", h},
					a :> a, b -> b, c -> c, nonOption, d -> d, e -> e
				]
			],
			Message[QuietValidOptionsPattern::anmlist,
				4,
				QuietValidOptionsPattern[f, {g, "str", h},
					a :> a, b -> b, c -> c, nonOption, d -> d, e -> e
				]
			],
			Message[QuietValidOptionsPattern::nonopt,
				nonOption, 4,
				QuietValidOptionsPattern[f, {g, "str", h},
					a :> a, b -> b, c -> c, nonOption, d -> d, e -> e
				]
			]
		}
		,
		TestID -> "Invalid arguments: \
symbol, list with non-symbol, 3 options, non-option, 2 options"
	]
]


(* ::Subsection:: *)
(*Duplicate usage in one pattern*)


Module[{f, arg, opt1, opt2, val1, val2, default1, default2},
	Options[f] = {opt1 -> default1, opt2 -> default2};
	Test[
		MatchQ[
			f[opt1 -> val1, arg, opt2 -> val2],
			f[QuietValidOptionsPattern[f], arg, QuietValidOptionsPattern[f]]
		]
		,
		True
		,
		TestID -> "Duplicate usage in one pattern: valid options"
	]
]
Module[{a, b, f, g, opt, x, y},
	Options[f] = {opt -> x};
	CheckOption[f, opt, val : Except[_Symbol]] :=
		Message[f::opt, HoldForm[val]];
	
	Test[
		MatchQ[
			{{opt -> y}, {opt -> 2}},
			{
				{QuietValidOptionsPattern[a :> b, {f, g}, f::opt]},
				{QuietValidOptionsPattern[a :> b, {f, g}, f::opt]}
			}
		]
		,
		False
		,
		TestID -> "Duplicate usage in one pattern: invalid options"
	]
]


(* ::Subsection:: *)
(*Set invalid default options*)


Module[{check, checkRel, invalid1, invalid2},
	Test[
		SetOptions[QuietValidOptionsPattern,
			CheckedDuplicate :> invalid1,
			{"CheckOption" -> check},
			{{"ChecksStopOnInvalid" :> invalid2}},
			CheckOptionRelations -> checkRel
		]
		,
		$Failed
		,
		{
			Message[QuietValidOptionsPattern::optfla,
				CheckedDuplicate, invalid1
			],
			Message[QuietValidOptionsPattern::opttf,
				ChecksStopOnInvalid, invalid2
			]
		}
		,
		TestID -> "Set invalid default options"
	]
]


(* ::Section:: *)
(*TearDown*)


On[General::stop]


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
