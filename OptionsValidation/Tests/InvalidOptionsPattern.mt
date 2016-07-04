(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["OptionsValidation`Tests`InvalidOptionsPattern`", {"MUnit`"}]


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
			(*	InvalidOptionsPattern returns a Condition, this brakes code of
				Test, leading to an error. Looks like a bug in Test. Wrapping
				result with List to avoid it. *)
			InvalidOptionsPattern[f] // List
			,
			Verbatim[Condition][
				Verbatim[Pattern][
					optsPattName_, Verbatim[OptionsPattern][f]
				],
				Not@OptionsValidation`Private`generateValidator[Symbol]
					[False, First, CheckOption, CheckOptionRelations]
					[f, HoldPattern@Flatten@{optsPattName_}]
			] // List
			,
			TestID -> "Pattern: Default options: symbol"
		]
	];
	Module[{f, g},
		TestMatch[
			InvalidOptionsPattern[{f, g}] // List
			,
			Verbatim[Condition][
				Verbatim[Pattern][
					optsPattName_, Verbatim[OptionsPattern][{f, g}]
				],
				Not@OptionsValidation`Private`generateValidator[List]
					[False, First, CheckOption, CheckOptionRelations]
					[{f, g}, HoldPattern@Flatten@{optsPattName_}]
			] // List
			,
			TestID -> "Pattern: Default options: list"
		]
	];
	
	Module[{a, b, f, g},
		TestMatch[
			InvalidOptionsPattern[{a-> b, f}, g] // List
			,
			Verbatim[Condition][
				Verbatim[Pattern][
					optsPattName_, Verbatim[OptionsPattern][{a-> b, f}]
				],
				Not@OptionsValidation`Private`generateValidator[Symbol]
					[False, First, CheckOption, CheckOptionRelations]
					[g, HoldPattern@Flatten@{optsPattName_}]
			] // List
			,
			TestID -> "Pattern: Default options: list, symbol"
		]
	]
]


Module[{a, b, c, d, e, f},
	Test[
		InvalidOptionsPattern[{a :> b, c -> d, e -> f}] // List
		,
		OptionsPattern[{a :> b, c -> d, e -> f}] // List
		,
		Message[InvalidOptionsPattern::unknownOptionOwner,
			{a :> b, c -> d, e -> f}
		]
		,
		TestID -> "Pattern: Default options: no option owner: implicit"
	]
]
Module[{f, g, h, i},
	Test[
		InvalidOptionsPattern[{f, g, h, i}, {}] // List
		,
		OptionsPattern[{f, g, h, i}] // List
		,
		TestID -> "Pattern: Default options: no option owner: explicit"
	]
]


(* ::Subsubsection:: *)
(*Explicit valid options*)


Do[
	With[
		{
			failureMessage =
				StringJoin@Riffle[ToString /@ {stopOnInvalid, duplicate}, ", "]
		}
		,
		Block[{OptionsValidation`Private`generateValidator},
			Module[{f, check, checkRel},
				TestMatch[
					InvalidOptionsPattern[f,
						{
							CheckOptionRelations -> checkRel,
							CheckedDuplicate :> duplicate,
							{ChecksStopOnInvalid -> stopOnInvalid},
							"CheckOption" -> check
						}
					] // List
					,
					Verbatim[Condition][
						Verbatim[Pattern][
							optsPattName_, Verbatim[OptionsPattern][f]
						],
						Not@OptionsValidation`Private`generateValidator[Symbol]
							[stopOnInvalid, duplicate, check, checkRel]
							[f, HoldPattern@Flatten@{optsPattName_}]
					] // List
					,
					TestID -> "Pattern: Explicit valid options: symbol",
					TestFailureMessage -> failureMessage
				]
			];
			
			Module[{f, g, h, check, checkRel, checkRel2, invalidDuplicates},
				TestMatch[
					InvalidOptionsPattern[{f, g, h},
						"ChecksStopOnInvalid" :> stopOnInvalid,
						CheckOptionRelations -> checkRel,
						{
							CheckedDuplicate -> duplicate,
							CheckOption -> check,
							CheckedDuplicate :> invalidDuplicates
						},
						CheckOptionRelations -> checkRel2
					] // List
					,
					Verbatim[Condition][
						Verbatim[Pattern][
							optsPattName_, Verbatim[OptionsPattern][{f, g, h}]
						],
						Not@OptionsValidation`Private`generateValidator[List]
							[stopOnInvalid, duplicate, check, checkRel]
							[{f, g, h}, HoldPattern@Flatten@{optsPattName_}]
					] // List
					,
					TestID -> "Pattern: Explicit valid options: list",
					TestFailureMessage -> failureMessage
				]
			];
			
			Module[{f, g, h, check, checkRel, invalidStopOnInvalid},
				TestMatch[
					InvalidOptionsPattern[f, {g, h},
						CheckOption :> check,
						{{ChecksStopOnInvalid :> stopOnInvalid}},
						{
							CheckedDuplicate -> duplicate,
							ChecksStopOnInvalid -> invalidStopOnInvalid
						},
						"CheckOptionRelations" :> checkRel
					] // List
					,
					Verbatim[Condition][
						Verbatim[Pattern][
							optsPattName_, Verbatim[OptionsPattern][f]
						],
						Not@OptionsValidation`Private`generateValidator[List]
							[stopOnInvalid, duplicate, check, checkRel]
							[{g, h}, HoldPattern@Flatten@{optsPattName_}]
					] // List
					,
					TestID -> "Pattern: Explicit valid options: symbol, list",
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


Module[{f, check, invalid1, invalid2, invalid3, invalid4},
	Test[
		InvalidOptionsPattern[f, {
			CheckedDuplicate :> invalid1,
			"CheckOption" :> check,
			CheckedDuplicate -> invalid2,
			"ChecksStopOnInvalid" -> invalid3,
			CheckedDuplicate -> invalid4
		}] // Hold[#]&
		,
		InvalidOptionsPattern[f, {
			CheckedDuplicate :> invalid1,
			"CheckOption" :> check,
			CheckedDuplicate -> invalid2,
			"ChecksStopOnInvalid" -> invalid3,
			CheckedDuplicate -> invalid4
		}] // Hold
		,
		{
			Message[InvalidOptionsPattern::optfla, CheckedDuplicate, invalid1],
			Message[InvalidOptionsPattern::opttf,
				ChecksStopOnInvalid, invalid3
			]
		}
		,
		TestID -> "Pattern: Explicit invalid options: symbol"
	]
]
Module[{f, g, h, i, checkRel, invalid1, invalid2},
	Test[
		InvalidOptionsPattern[f, {g, h, i},
			{ChecksStopOnInvalid -> True},
			CheckedDuplicate :> invalid1,
			ChecksStopOnInvalid -> invalid2,
			{
				CheckOptionRelations -> checkRel,
				"CheckedDuplicate" -> All
			}
		] // Hold[#]&
		,
		InvalidOptionsPattern[f, {g, h, i},
			{ChecksStopOnInvalid -> True},
			CheckedDuplicate :> invalid1,
			ChecksStopOnInvalid -> invalid2,
			{
				CheckOptionRelations -> checkRel,
				"CheckedDuplicate" -> All
			}
		] // Hold
		,
		Message[InvalidOptionsPattern::optfla, CheckedDuplicate, invalid1]
		,
		TestID -> "Pattern: Explicit invalid options: symbol, list"
	]
]


(* ::Subsection:: *)
(*Invalid arguments*)


Test[
	InvalidOptionsPattern[] // Hold[#]&
	,
	InvalidOptionsPattern[] // Hold
	,
	Message[InvalidOptionsPattern::argt, InvalidOptionsPattern, 0, 1, 2]
	,
	TestID -> "Invalid arguments: no args"
]


Module[{a, b, f, x},
	Test[
		InvalidOptionsPattern[a -> b, f[x]] // Hold[#]&
		,
		InvalidOptionsPattern[a -> b, f[x]] // Hold
		,
		Message[InvalidOptionsPattern::symse,
			2, InvalidOptionsPattern[a -> b, f[x]]
		]
		,
		TestID -> "Invalid arguments: option, expr"
	]
]


Module[{f, g, opt1, opt2, val1, val2},
	With[{opt1Name = SymbolName[opt1], opt2Name = SymbolName[opt2]},
		Test[
			InvalidOptionsPattern[{f, g}, opt1 :> val1, opt2Name -> val2] //
				Hold[#]&
			,
			InvalidOptionsPattern[{f, g}, opt1 :> val1, opt2Name -> val2] //
				Hold
			,
			{
				Message[CheckOption::optnf, opt1Name, InvalidOptionsPattern],
				Message[CheckOption::optnf, opt2Name, InvalidOptionsPattern]
			}
			,
			TestID -> "Invalid arguments: list of symbols, 2 options"
		]
	]
]


Module[{f, a, b, sym1, sym2},
	Test[
		InvalidOptionsPattern[f, "str", a -> b, sym1, sym2] // Hold[#]&
		,
		InvalidOptionsPattern[f, "str", a -> b, sym1, sym2] // Hold
		,
		{
			Message[InvalidOptionsPattern::symse,
				2, InvalidOptionsPattern[f, "str", a -> b, sym1, sym2]
			],
			Message[InvalidOptionsPattern::nonopt,
				sym2, 2,
				InvalidOptionsPattern[f, "str", a -> b, sym1, sym2]
			]
		}
		,
		TestID -> "Invalid arguments: \
symbol, string, option, non-option, option"
	]
]


(* ::Subsection:: *)
(*Duplicate usage in one pattern*)


Module[{f, arg, opt1, opt2, val1, val2},
	CheckOption[f, opt1 | opt2] = False&;
	Test[
		MatchQ[
			f[opt1 -> val1, arg, opt2 -> val2],
			f[InvalidOptionsPattern[f], arg, InvalidOptionsPattern[f]]
		]
		,
		True
		,
		TestID -> "Duplicate usage in one pattern: matched"
	]
]
Module[{f, g, h, opt},
	Options[g] = Options[h] = {opt -> 10.};
	CheckOption[g, opt, val : Except[_Real]] := Message[g::opt, HoldForm[val]];
	CheckOption[h, opt] = TrueQ[# > 2] || Message[h::opt, HoldForm[#]] &;
	
	Test[
		MatchQ[
			{{opt -> -3}, {opt :> 7.}},
			{
				{InvalidOptionsPattern[f, {g, h}]},
				{InvalidOptionsPattern[f, {g, h}]}
			}
		]
		,
		False
		,
		{Message[g::opt, -3], Message[h::opt, -3]}
		,
		TestID -> "Duplicate usage in one pattern: not matched"
	]
]


(* ::Subsection:: *)
(*Set invalid default options*)


Module[{check, checkRel, checkRel2, invalid1, invalid2, invalid3, invalid4},
	Test[
		SetOptions[InvalidOptionsPattern,
			ChecksStopOnInvalid -> True,
			ChecksStopOnInvalid -> invalid1,
			CheckOption -> check,
			CheckOptionRelations -> checkRel,
			CheckedDuplicate -> invalid2,
			CheckedDuplicate -> All,
			CheckedDuplicate -> invalid3,
			CheckOptionRelations -> checkRel2,
			CheckedDuplicate -> invalid4
		]
		,
		$Failed
		,
		{
			Message[InvalidOptionsPattern::opttf,
				ChecksStopOnInvalid, invalid1
			],
			Message[InvalidOptionsPattern::optfla, CheckedDuplicate, invalid4]
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
