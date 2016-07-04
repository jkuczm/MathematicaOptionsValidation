(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["OptionsValidation`Tests`ValidOptionsPattern`", {"MUnit`"}]


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
			(*	ValidOptionsPattern returns a Condition, this brakes code of
				Test, leading to an error. Looks like a bug in Test. Wrapping
				result with List to avoid it. *)
			ValidOptionsPattern[f] // List
			,
			Verbatim[Condition][
				Verbatim[Pattern][
					optsPattName_, Verbatim[OptionsPattern][f]
				],
				HoldPattern@Identity@
					OptionsValidation`Private`generateValidator[Symbol]
						[False, First, CheckOption, CheckOptionRelations]
						[f, Flatten@{optsPattName_}]
			] // List
			,
			TestID -> "Pattern: Default options: symbol"
		]
	];
	Module[{a, b, f, g, h, i},
		TestMatch[
			ValidOptionsPattern[{f, g, {a -> b, h, i}}] // List
			,
			Verbatim[Condition][
				Verbatim[Pattern][
					optsPattName_,
					Verbatim[OptionsPattern][{f, g, {a -> b, h, i}}]
				],
				HoldPattern@Identity@
					OptionsValidation`Private`generateValidator[List]
						[False, First, CheckOption, CheckOptionRelations]
						[{f, g, h, i}, Flatten@{optsPattName_}]
			] // List
			,
			TestID -> "Pattern: Default options: list"
		]
	];
	
	Module[{a, b, f, g, h},
		TestMatch[
			ValidOptionsPattern[a -> b, {f, g, h}] // List
			,
			Verbatim[Condition][
				Verbatim[Pattern][
					optsPattName_,
					Verbatim[OptionsPattern][a -> b]
				],
				HoldPattern@Identity@
					OptionsValidation`Private`generateValidator[List]
						[False, First, CheckOption, CheckOptionRelations]
						[{f, g, h}, Flatten@{optsPattName_}]
			] // List
			,
			TestID -> "Pattern: Default options: rule, list"
		]
	]
]


Module[{a, b, c, d},
	Test[
		ValidOptionsPattern[{a -> b, {c :> d}}] // List
		,
		OptionsPattern[{a -> b, {c :> d}}] // List
		,
		Message[ValidOptionsPattern::unknownOptionOwner, {a -> b, {c :> d}}
		]
		,
		TestID -> "Pattern: Default options: no option owner: implicit"
	]
]
Module[{f},
	Test[
		ValidOptionsPattern[f, {}] // List
		,
		OptionsPattern[f] // List
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
			Module[{f, check, checkRel, unusedCheckRel},
				TestMatch[
					ValidOptionsPattern[f,
						{CheckOptionRelations -> checkRel},
						"ChecksStopOnInvalid" -> stopOnInvalid,
						CheckOptionRelations -> unusedCheckRel,
						{{CheckedDuplicate :> duplicate}},
						CheckOption -> check
					] // List
					,
					Verbatim[Condition][
						Verbatim[Pattern][
							optsPattName_, Verbatim[OptionsPattern][f]
						],
						HoldPattern@Identity@
							OptionsValidation`Private`generateValidator[Symbol]
								[stopOnInvalid, duplicate, check, checkRel]
								[f, Flatten@{optsPattName_}]
					] // List
					,
					TestID -> "Pattern: Explicit valid options: symbol",
					TestFailureMessage -> failureMessage
				]
			];
			
			Module[{a, b, c, d, f, g, check, checkRel},
				TestMatch[
					ValidOptionsPattern[{a -> b, c :> d, {f}, g},
						"CheckOption" -> check,
						{
							ChecksStopOnInvalid -> stopOnInvalid,
							CheckedDuplicate -> duplicate
						},
						CheckOptionRelations :> checkRel
					] // List
					,
					Verbatim[Condition][
						Verbatim[Pattern][
							optsPattName_,
							Verbatim[OptionsPattern][{a -> b, c :> d, {f}, g}]
						],
						HoldPattern@Identity@
							OptionsValidation`Private`generateValidator[List]
								[stopOnInvalid, duplicate, check, checkRel]
								[{f, g}, Flatten@{optsPattName_}]
					] // List
					,
					TestID -> "Pattern: Explicit valid options: list",
					TestFailureMessage -> failureMessage
				]
			];
			
			Module[{a, b, c, d, f, g, check, checkRel, invalidDuplicate},
				TestMatch[
					ValidOptionsPattern[{f, a -> b, {c :> d}}, g,
						{ChecksStopOnInvalid -> stopOnInvalid},
						{
							"CheckedDuplicate" -> duplicate,
							CheckOption :> check,
							{"CheckOptionRelations" -> checkRel}
						},
						"CheckedDuplicate" -> invalidDuplicate
					] // List
					,
					Verbatim[Condition][
						Verbatim[Pattern][
							optsPattName_,
							Verbatim[OptionsPattern][{f, a -> b, {c :> d}}]
						],
						HoldPattern@Identity@
							OptionsValidation`Private`generateValidator[Symbol]
								[stopOnInvalid, duplicate, check, checkRel]
								[g, Flatten@{optsPattName_}]
					] // List
					,
					TestID -> "Pattern: Explicit valid options: list, symbol",
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


Module[{f, invalid},
	Test[
		ValidOptionsPattern[f, ChecksStopOnInvalid -> invalid] // Hold[#]&
		,
		ValidOptionsPattern[f, ChecksStopOnInvalid -> invalid] // Hold
		,
		Message[ValidOptionsPattern::opttf, ChecksStopOnInvalid, invalid]
		,
		TestID -> "Pattern: Explicit invalid options: symbol"
	]
]
Module[{f, g, h, i, check, invalid1, invalid2, invalid3},
	Test[
		ValidOptionsPattern[{f, g, h}, {i},
			{{CheckOption -> check}},
			"CheckedDuplicate" -> invalid1,
			CheckOptionRelations -> checkRel,
			{CheckedDuplicate -> invalid2},
			ChecksStopOnInvalid :> invalid3
		] // Hold[#]&
		,
		ValidOptionsPattern[{f, g, h}, {i},
			{{CheckOption -> check}},
			"CheckedDuplicate" -> invalid1,
			CheckOptionRelations -> checkRel,
			{CheckedDuplicate -> invalid2},
			ChecksStopOnInvalid :> invalid3
		] // Hold
		,
		{
			Message[ValidOptionsPattern::optfla, CheckedDuplicate, invalid1],
			Message[ValidOptionsPattern::opttf, ChecksStopOnInvalid, invalid3]
		}
		,
		TestID -> "Pattern: Explicit invalid options: list, list"
	]
]


(* ::Subsection:: *)
(*Invalid arguments*)


Test[
	ValidOptionsPattern[] // Hold[#]&
	,
	ValidOptionsPattern[] // Hold
	,
	Message[ValidOptionsPattern::argt, ValidOptionsPattern, 0, 1, 2]
	,
	TestID -> "Invalid arguments: no args"
]


Module[{f, val},
	Test[
		ValidOptionsPattern[f, "unknownTestOpt" -> val] // Hold[#]&
		,
		ValidOptionsPattern[f, "unknownTestOpt" -> val] // Hold
		,
		Message[CheckOption::optnf, "unknownTestOpt", ValidOptionsPattern]
		,
		TestID -> "Invalid arguments: symbol, unknown option"
	]
]
Module[{f, a, b},
	Test[
		ValidOptionsPattern[{f, a -> b}, "nonSymbol"] // Hold[#]&
		,
		ValidOptionsPattern[{f, a -> b}, "nonSymbol"] // Hold
		,
		Message[ValidOptionsPattern::symse,
			2, ValidOptionsPattern[{f, a -> b}, "nonSymbol"]
		]
		,
		TestID -> "Invalid arguments: default spec, string"
	]
]


Module[{f, g, a, b, c, d, nonOption},
	Test[
		ValidOptionsPattern[f, {1, g}, a -> b, nonOption, c :> d] // Hold[#]&
		,
		ValidOptionsPattern[f, {1, g}, a -> b, nonOption, c :> d] // Hold
		,
		{
			Message[ValidOptionsPattern::symse,
				2, ValidOptionsPattern[f, {1, g}, a -> b, nonOption, c :> d]
			],
			Message[ValidOptionsPattern::nonopt,
				nonOption, 2,
				ValidOptionsPattern[f, {1, g}, a -> b, nonOption, c :> d]
			]
		}
		,
		TestID -> "Invalid arguments: \
symbol, list with non-symbol, option, non-option, option"
	]
]


(* ::Subsection:: *)
(*Duplicate usage in one pattern*)


Module[{f, arg, opt1, opt2, val1, val2, default1, default2},
	Options[f] = {opt1 -> default1, opt2 -> default2};
	Test[
		MatchQ[
			f[opt1 -> val1, arg, opt2 -> val2],
			f[ValidOptionsPattern[f], arg, ValidOptionsPattern[f]]
		]
		,
		True
		,
		TestID -> "Duplicate usage in one pattern: valid options"
	]
]
Module[{f, g, opt},
	CheckOption[f, opt] = IntegerQ[#] || Message[f::opt, HoldForm[#]] &;
	
	Test[
		MatchQ[
			{{opt -> 5}, {opt -> 1.7}},
			{{ValidOptionsPattern[{g}, f]}, {ValidOptionsPattern[{g}, f]}}
		]
		,
		False
		,
		Message[f::opt, 1.7]
		,
		TestID -> "Duplicate usage in one pattern: invalid options"
	]
]


(* ::Subsection:: *)
(*Set invalid default options*)


Module[{check, checkRel, invalid1, invalid2, invalid3},
	Test[
		SetOptions[ValidOptionsPattern,
			{
				CheckedDuplicate -> Last,
				CheckOptionRelations -> checkRel,
				CheckedDuplicate -> invalid1
			},
			ChecksStopOnInvalid -> invalid2,
			CheckOption -> check,
			ChecksStopOnInvalid -> invalid3
		]
		,
		$Failed
		,
		{
			Message[ValidOptionsPattern::optfla, CheckedDuplicate, invalid1],
			Message[ValidOptionsPattern::opttf, ChecksStopOnInvalid, invalid3]
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
