(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["OptionsValidation`Tests`SetDefaultOptionsValidation`",
	{"MUnit`"}
]


Get["OptionsValidation`"]


Options[testFunc1] = testFunc1ValidOptions = {opt1 -> default, "opt2" :> 1}

Options[testFunc2] = testFunc2ValidOptions =
	Append[Options[testFunc1], opt3 -> "default"]

CheckOption[f : testFunc1 | testFunc2, name : opt1, val : Except[_Symbol]] :=
	Message[f::opt1, HoldForm[name], HoldForm[val]]

CheckOption[testFunc1, name : "opt2"] =
	Function[Null,
		IntegerQ[#] || Message[testFunc1::opt2, HoldForm[name], HoldForm[#]],
		HoldFirst
	]

CheckOption[testFunc2, name : opt2] =
	IntegerQ[#] && Positive[#] ||
		Message[testFunc2::opt2, HoldForm[name], HoldForm[#]]&


withLocalTestFunctions =
	Function[body,
		Internal`InheritedBlock[{testFunc1, testFunc2},
			body
		],
		HoldFirst
	]

withLocalTestFunctionsValid =
	Function[body,
		Internal`InheritedBlock[{testFunc1, testFunc2},
			Options[testFunc1] = testFunc1ValidOptions;
			Options[testFunc2] = testFunc2ValidOptions;
			
			body
		],
		HoldFirst
	]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Symbol*)


(* ::Subsubsection:: *)
(*Valid base*)


BeginTestSection["Symbol: Valid base"]


withLocalTestFunctionsValid[
	Test[
		SetDefaultOptionsValidation[testFunc1]
		,
		testFunc1
		,
		TestID -> "Symbol: Valid base: SetDefaultOptionsValidation evaluation",
		TestFailureAction -> "SkipSection"
	];
	
	withLocalTestFunctionsValid@Module[{newOptions, newValA, newValB},
		newOptions = {opt1 :> newValB, "opt2" :> 2 + 3};
		
		Test[
			SetOptions[testFunc1,
				{"opt1" -> newValA}, opt2 :> 2 + 3, opt1 :> newValB
			]
			,
			newOptions
			,
			TestID -> "Symbol: Valid base: 3 valid, 1 duplicate: SetOptions"
		];
		Test[
			Options[testFunc1]
			,
			newOptions
			,
			TestID -> "Symbol: Valid base: 3 valid, 1 duplicate: Options"
		]
	];
	
	withLocalTestFunctionsValid@Module[{newVal},
		Test[
			SetOptions[testFunc1,
				opt1 :> newVal, "opt2" :> 2 + 3, "opt1" -> "str"
			]
			,
			$Failed
			,
			Message[testFunc1::opt1, "opt1", "str"]
			,
			TestID -> "Symbol: Valid base: 1 invalid (last dup), 2 valid: \
SetOptions"
		];
		Test[
			Options[testFunc1]
			,
			testFunc1ValidOptions
			,
			TestID -> "Symbol: Valid base: 1 invalid (last dup), 2 valid: \
Options"
		]
	];
	
	withLocalTestFunctionsValid@Module[{newVal},
		Test[
			SetOptions[testFunc1,
				opt1 :> newVal, "opt2" :> 2 + 3, "opt1" -> "str"
			]
			,
			$Failed
			,
			Message[testFunc1::opt1, "opt1", "str"]
			,
			TestID -> "Symbol: Valid base: 1 invalid (last dup), 2 valid: \
SetOptions"
		];
		Test[
			Options[testFunc1]
			,
			testFunc1ValidOptions
			,
			TestID -> "Symbol: Valid base: 1 invalid (last dup), 2 valid: \
Options"
		]
	];
	
	withLocalTestFunctionsValid[
		Test[
			SetOptions[testFunc1,
				{opt1 -> "str", opt2 :> 1.1 + 2.2, opt1 -> 12.3}
			]
			,
			$Failed
			,
			{
				Message[testFunc1::opt1, "opt1", 12.3],
				Message[testFunc1::opt2, "opt2", 1.1 + 2.2]
			}
			,
			TestID -> "Symbol: Valid base: 3 invalid, 1 duplicate: SetOptions"
		];
		Test[
			Options[testFunc1]
			,
			testFunc1ValidOptions
			,
			TestID -> "Symbol: Valid base: 3 invalid, 1 duplicate: Options"
		]
	];
	
	withLocalTestFunctionsValid@Module[{newOptions1, newOptions2, newVal},
		newOptions1 = {opt1 -> newVal, "opt2" -> -11};
		newOptions2 = Append[newOptions1, Last[testFunc2ValidOptions]];
		Test[
			SetOptions[{testFunc1, testFunc2}, opt1 -> newVal, opt2 -> -11]
			,
			{newOptions1, newOptions2}
			,
			TestID -> "Symbol: Valid base: List: 2 valid: 1 valid, 1 invalid: \
SetOptions"
		];
		Test[
			Options[testFunc1]
			,
			newOptions1
			,
			TestID -> "Symbol: Valid base: List: 2 valid: 1 valid, 1 invalid: \
Options first func"
		];
		Test[
			Options[testFunc2]
			,
			newOptions2
			,
			TestID -> "Symbol: Valid base: List: 2 valid: 1 valid, 1 invalid: \
Options second func"
		]
	];
	
	withLocalTestFunctionsValid@Module[{newOptions2},
		newOptions2 = {
			opt1 -> "str", "opt2" :> 1, Last[testFunc2ValidOptions]
		};
		Test[
			SetOptions[{testFunc1, testFunc2}, {opt1 -> "str"}]
			,
			{$Failed, newOptions2}
			,
			Message[testFunc1::opt1, "opt1", "str"]
			,
			TestID -> "Symbol: Valid base: List: 1 invalid: 1 invalid: \
SetOptions"
		];
		Test[
			Options[testFunc1]
			,
			testFunc1ValidOptions
			,
			TestID -> "Symbol: Valid base: List: 1 invalid: 1 invalid: \
Options first func"
		];
		Test[
			Options[testFunc2]
			,
			newOptions2
			,
			TestID -> "Symbol: Valid base: List: 1 invalid: 1 invalid: \
Options second func"
		]
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*Invalid base*)


BeginTestSection["Symbol: Invalid base"]


withLocalTestFunctions@Module[{newOptions = {opt1 -> "str1", opt2 -> "str2"}},
	Options[testFunc1] = {opt1 -> 23, opt2 -> "str2"};
	Test[
		SetDefaultOptionsValidation[testFunc1] // Hold[#]&
		,
		SetDefaultOptionsValidation[testFunc1] // Hold
		,
		{
			Message[testFunc1::opt1, "opt1", 23],
			Message[testFunc1::opt2, "opt2", "str2"],
			Message[SetDefaultOptionsValidation::currentInvalid, testFunc1]
		}
		,
		TestID -> "Symbol: Invalid base: \
SetDefaultOptionsValidation evaluation"
		,
		TestFailureAction -> "SkipSection"
	];
	
	Test[
		SetOptions[testFunc1, opt1 -> "str1"]
		,
		newOptions
		,
		TestID -> "Symbol: Invalid base: 1 invalid: SetOptions"
	];
	Test[
		Options[testFunc1]
		,
		newOptions
		,
		TestID -> "Symbol: Invalid base: 1 invalid: Options"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*Options*)


BeginTestSection["Symbol: Options valid"]


Block[{OptionsValidation`Private`generateValidator},
	Module[{f, check, checkRel},
		Test[
			SetDefaultOptionsValidation[f, {
				"ChecksStopOnInvalid" -> True, CheckedDuplicate -> First,
				CheckOption :> check, CheckOptionRelations -> checkRel
			}]
			,
			f
			,
			TestID -> "Symbol: Options valid: \
SetDefaultOptionsValidation evaluation"
			,
			TestFailureAction -> "SkipSection"
		];
		TestMatch[
			UpValues[f]
			,
			{
				Verbatim[HoldPattern]@HoldPattern[SetOptions][
					f,
					Verbatim[Condition][
						Verbatim[Pattern][
							optsPattName_, Verbatim[OptionsPattern][f]
						],
						Not@OptionsValidation`Private`generateValidator[Symbol]
							[True, First, check, checkRel]
							[f, HoldPattern@Flatten@{optsPattName_}]
					]
				] :>
					$Failed
			}
			,
			TestID -> "Symbol: Options valid: UpValues"
		]
	]
]


EndTestSection[]


BeginTestSection["Symbol: Options invalid"]


Block[{OptionsValidation`Private`generateValidator},
	Module[{f, checkRel, invalid1, invalid2},
		Test[
			SetDefaultOptionsValidation[f,
				{ChecksStopOnInvalid -> invalid1},
				{"CheckedDuplicate" -> invalid2},
				CheckOptionRelations :> checkRel
			] // Hold[#]&
			,
			SetDefaultOptionsValidation[f,
				{ChecksStopOnInvalid -> invalid1},
				{"CheckedDuplicate" -> invalid2},
				CheckOptionRelations :> checkRel
			] // Hold
			,
			{
				Message[SetDefaultOptionsValidation::opttf,
					ChecksStopOnInvalid, invalid1
				],
				Message[SetDefaultOptionsValidation::optfla,
					CheckedDuplicate, invalid2
				]
			}
			,
			TestID -> "Symbol: Options invalid: \
SetDefaultOptionsValidation evaluation"
			,
			TestFailureAction -> "SkipSection"
		];
		TestMatch[
			UpValues[f]
			,
			{}
			,
			TestID -> "Symbol: Options invalid: UpValues"
		]
	]
]


EndTestSection[]


(* ::Subsection:: *)
(*List*)


(* ::Subsubsection:: *)
(*Valid base*)


BeginTestSection["List: Valid base"]


withLocalTestFunctionsValid[
	Test[
		SetDefaultOptionsValidation[{testFunc1, testFunc2}]
		,
		{testFunc1, testFunc2}
		,
		TestID -> "List: Valid base: SetDefaultOptionsValidation evaluation",
		TestFailureAction -> "SkipSection"
	];
	
	withLocalTestFunctionsValid@Module[{newOptions, newVal},
		newOptions = {opt1 -> newVal, "opt2" :> 2 + 3};
		Test[
			SetOptions[testFunc1,
				"opt1" -> 51, {opt2 :> 2 + 3}, opt1 -> newVal
			]
			,
			newOptions
			,
			TestID -> "List: Valid base: 1 invalid (first dup), 2 valid: \
SetOptions first func"
		];
		Test[
			Options[testFunc1]
			,
			newOptions
			,
			TestID -> "List: Valid base: 1 invalid (first dup), 2 valid: \
Options first func"
		]
	];
	withLocalTestFunctionsValid@Module[{newOptions, newVal},
		newOptions =
			{opt1 -> newVal, "opt2" :> 2 + 3, Last[testFunc2ValidOptions]};
		Test[
			SetOptions[testFunc2,
				"opt1" -> 51, {opt2 :> 2 + 3}, opt1 -> newVal
			]
			,
			newOptions
			,
			TestID -> "List: Valid base: 1 invalid (first dup), 2 valid: \
SetOptions second func"
		];
		Test[
			Options[testFunc2]
			,
			newOptions
			,
			TestID -> "List: Valid base: 1 invalid (first dup), 2 valid: \
Options second func"
		]
	];
	
	withLocalTestFunctionsValid[
		Test[
			SetOptions[testFunc1, "opt2" :> 2 + 3, "opt1" -> "str"
			]
			,
			$Failed
			,
			Message[testFunc1::opt1, "opt1", "str"]
			,
			TestID -> "List: Valid base: 1 valid, 1 invalid: \
SetOptions first func"
		];
		Test[
			Options[testFunc1]
			,
			testFunc1ValidOptions
			,
			TestID -> "List: Valid base: 1 valid, 1 invalid: \
Options first func"
		]
	];
	withLocalTestFunctionsValid[
		Test[
			SetOptions[testFunc2, "opt2" :> 2 + 3, "opt1" -> "str"
			]
			,
			$Failed
			,
			Message[testFunc2::opt1, "opt1", "str"]
			,
			TestID -> "List: Valid base: 1 valid, 1 invalid: \
SetOptions second func"
		];
		Test[
			Options[testFunc2]
			,
			testFunc2ValidOptions
			,
			TestID -> "List: Valid base: 1 valid, 1 invalid: \
Options second func"
		]
	];
	
	withLocalTestFunctionsValid@Module[
		{newOptions1, newOptions2, newValA, newValB}
		,
		newOptions1 = {opt1 -> newValB, "opt2" -> 97};
		newOptions2 = Append[newOptions1, Last[testFunc2ValidOptions]];
		Test[
			SetOptions[{testFunc1, testFunc2},
				opt1 :> newValA, {"opt1" -> newValB}, opt2 -> 97
			]
			,
			{newOptions1, newOptions2}
			,
			TestID -> "List: Valid base: List: \
3 valid, 1 duplicate: 3 valid, 1 duplicate: SetOptions"
		];
		Test[
			Options[testFunc1]
			,
			newOptions1
			,
			TestID -> "List: Valid base: List: \
3 valid, 1 duplicate: 3 valid, 1 duplicate: Options first func"
		];
		Test[
			Options[testFunc2]
			,
			newOptions2
			,
			TestID -> "List: Valid base: List: \
3 valid, 1 duplicate: 3 valid, 1 duplicate: Options second func"
		]
	];
	
	withLocalTestFunctionsValid@Module[{newOptions1},
		newOptions1 = {opt1 -> default, "opt2" -> -3};
		Test[
			SetOptions[{testFunc1, testFunc2}, opt2 -> -3]
			,
			{newOptions1, $Failed}
			,
			Message[testFunc2::opt2, "opt2", -3]
			,
			TestID -> "List: Valid base: List: 1 valid: 1 invalid: SetOptions"
		];
		Test[
			Options[testFunc1]
			,
			newOptions1
			,
			TestID -> "List: Valid base: List: 1 valid: 1 invalid: \
Options first func"
		];
		Test[
			Options[testFunc2]
			,
			testFunc2ValidOptions
			,
			TestID -> "List: Valid base: List: 1 valid: 1 invalid: \
Options second func"
		]
	];
	
	withLocalTestFunctionsValid@Module[{newOptions2},
		newOptions2 = {
			opt1 -> "str", "opt2" :> 1, Last[testFunc2ValidOptions]
		};
		Test[
			SetOptions[{testFunc1, testFunc2}, {"opt1" -> 10, opt2 :> 3. - 1.}]
			,
			{$Failed, $Failed}
			,
			{
				Message[testFunc1::opt1, "opt1", 10],
				Message[testFunc1::opt2, "opt2", 3. - 1.],
				Message[testFunc2::opt1, "opt1", 10],
				Message[testFunc2::opt2, "opt2", 2.]
			}
			,
			TestID -> "List: Valid base: List: 2 invalid: 2 invalid: \
SetOptions"
		];
		Test[
			Options[testFunc1]
			,
			testFunc1ValidOptions
			,
			TestID -> "List: Valid base: List: 1 invalid: 1 invalid: \
Options first func"
		];
		Test[
			Options[testFunc2]
			,
			testFunc2ValidOptions
			,
			TestID -> "List: Valid base: List: 1 invalid: 1 invalid: \
Options second func"
		]
	];
]


EndTestSection[]


(* ::Subsubsection:: *)
(*Invalid base*)


BeginTestSection["List: Invalid base (first func)"]


withLocalTestFunctions@Module[{newOptions1 = {opt1 -> 85, opt2 -> "str"}},
	Options[testFunc1] = {opt1 -> default, opt2 -> "str"};
	Test[
		SetDefaultOptionsValidation[{testFunc1, testFunc2}] // Hold[#]&
		,
		{SetDefaultOptionsValidation[testFunc1], testFunc2} // Hold
		,
		{
			Message[testFunc1::opt2, "opt2", "str"],
			Message[SetDefaultOptionsValidation::currentInvalid, testFunc1]
		}
		,
		TestID -> "List: Invalid base (first func): \
SetDefaultOptionsValidation evaluation"
		,
		TestFailureAction -> "SkipSection"
	];
	
	Test[
		SetOptions[{testFunc1, testFunc2}, opt1 -> 85]
		,
		{newOptions1, $Failed}
		,
		Message[testFunc2::opt1, "opt1", 85]
		,
		TestID -> "List: Invalid base (first func): List: \
1 invalid: 1 invalid: SetOptions"
	];
	Test[
		Options[testFunc1]
		,
		newOptions1
		,
		TestID -> "List: Invalid base (first func): List: \
1 invalid: 1 invalid: Options first func"
	];
	Test[
		Options[testFunc2]
		,
		testFunc2ValidOptions
		,
		TestID -> "List: Invalid base (first func): List: \
1 invalid: 1 invalid: Options second func"
	]
]


EndTestSection[]


BeginTestSection["List: Invalid base (both func)"]


withLocalTestFunctions@Module[{newOptions1, newOptions2},
	Options[testFunc1] = {"opt1" -> 17, "opt2" -> -2};
	Options[testFunc2] =
		Append[Options[testFunc1], Last[testFunc2ValidOptions]];
	Test[
		SetDefaultOptionsValidation[{testFunc1, testFunc2}] // Hold[#]&
		,
		{
			SetDefaultOptionsValidation[testFunc1],
			SetDefaultOptionsValidation[testFunc2]
		} // Hold
		,
		{
			Message[testFunc1::opt1, "opt1", 17],
			Message[SetDefaultOptionsValidation::currentInvalid, testFunc1],
			Message[testFunc2::opt1, "opt1", 17],
			Message[testFunc2::opt2, "opt2", -2],
			Message[SetDefaultOptionsValidation::currentInvalid, testFunc2]
		}
		,
		TestID -> "List: Invalid base (both func): \
SetDefaultOptionsValidation evaluation"
		,
		TestFailureAction -> "SkipSection"
	];
	
	newOptions1 = {"opt1" -> 0, "opt2" -> 7.9};
	newOptions2 = Append[newOptions1, Last[testFunc2ValidOptions]];
	Test[
		SetOptions[{testFunc1, testFunc2}, {
			opt1 -> "str1", "opt2" :> Pi, opt2 -> 7.9, "opt1" -> 0
		}]
		,
		{newOptions1, newOptions2}
		,
		TestID -> "List: Invalid base (both func): List: \
4 invalid, 2 duplicates: 4 invalid, 2 duplicates: SetOptions"
	];
	Test[
		Options[testFunc1]
		,
		newOptions1
		,
		TestID -> "List: Invalid base (both func): List: \
4 invalid, 2 duplicates: 4 invalid, 2 duplicates: Options first func"
	];
	Test[
		Options[testFunc2]
		,
		newOptions2
		,
		TestID -> "List: Invalid base (both func): List: \
4 invalid, 2 duplicates: 4 invalid, 2 duplicates: Options second func"
	]
]


EndTestSection[]


(* ::Subsubsection:: *)
(*Options*)


BeginTestSection["List: Options valid"]


Block[{OptionsValidation`Private`generateValidator},
	Module[{f, g, check, check2, checkRel, invalid},
		Test[
			SetDefaultOptionsValidation[{f, g},
				{
					"CheckedDuplicate" -> All, CheckOption :> check,
					{ChecksStopOnInvalid -> False}
				},
				CheckOptionRelations -> checkRel, "CheckOption" -> check2,
				CheckedDuplicate -> invalid
			]
			,
			{f, g}
			,
			TestID -> "List: Options valid: \
SetDefaultOptionsValidation evaluation"
			,
			TestFailureAction -> "SkipSection"
		];
		TestMatch[
			UpValues[f]
			,
			{
				Verbatim[HoldPattern]@HoldPattern[SetOptions][
					f,
					Verbatim[Condition][
						Verbatim[Pattern][
							optsPattName_, Verbatim[OptionsPattern][f]
						],
						Not@OptionsValidation`Private`generateValidator[Symbol]
							[False, All, check, checkRel]
							[f, HoldPattern@Flatten@{optsPattName_}]
					]
				] :>
					$Failed
			}
			,
			TestID -> "List: Options valid: UpValues first func"
		];
		TestMatch[
			UpValues[g]
			,
			{
				Verbatim[HoldPattern]@HoldPattern[SetOptions][
					g,
					Verbatim[Condition][
						Verbatim[Pattern][
							optsPattName_, Verbatim[OptionsPattern][g]
						],
						Not@OptionsValidation`Private`generateValidator[Symbol]
							[False, All, check, checkRel]
							[g, HoldPattern@Flatten@{optsPattName_}]
					]
				] :>
					$Failed
			}
			,
			TestID -> "List: Options valid: UpValues second func"
		]
	]
]


EndTestSection[]


BeginTestSection["List: Options invalid"]


Block[{OptionsValidation`Private`generateValidator},
	Module[{f, g, h, checkRel, checkRel2, invalid1, invalid2},
		Test[
			SetDefaultOptionsValidation[{f, g, h},
				CheckOptionRelations :> checkRel,
				{
					"CheckOptionRelations" -> checkRel2,
					CheckedDuplicate -> First
				},
				"ChecksStopOnInvalid" :> invalid1,
				ChecksStopOnInvalid -> invalid2
			] // Hold[#]&
			,
			SetDefaultOptionsValidation[{f, g, h},
				CheckOptionRelations :> checkRel,
				{
					"CheckOptionRelations" -> checkRel2,
					CheckedDuplicate -> First
				},
				"ChecksStopOnInvalid" :> invalid1,
				ChecksStopOnInvalid -> invalid2
			] // Hold
			,
			Message[SetDefaultOptionsValidation::opttf,
				ChecksStopOnInvalid, invalid1
			]
			,
			TestID -> "List: Options invalid: \
SetDefaultOptionsValidation evaluation"
			,
			TestFailureAction -> "SkipSection"
		];
		Test[
			UpValues[f]
			,
			{}
			,
			TestID -> "List: Options invalid: UpValues first func"
		];
		Test[
			UpValues[g]
			,
			{}
			,
			TestID -> "List: Options invalid: UpValues second func"
		];
		Test[
			UpValues[h]
			,
			{}
			,
			TestID -> "List: Options invalid: UpValues thid func"
		]
	]
]


EndTestSection[]


(* ::Subsection:: *)
(*Invalid arguments*)


Test[
	SetDefaultOptionsValidation[] // Hold[#]&
	,
	SetDefaultOptionsValidation[] // Hold
	,
	Message[SetDefaultOptionsValidation::argx, SetDefaultOptionsValidation, 0]
	,
	TestID -> "Invalid arguments: no args"
]


Module[{f},
	Test[
		SetDefaultOptionsValidation[{f, 11}] // Hold[#]&
		,
		SetDefaultOptionsValidation[{f, 11}] // Hold
		,
		Message[SetDefaultOptionsValidation::symse,
			1, SetDefaultOptionsValidation[{f, 11}]
		]
		,
		TestID -> "Invalid arguments: list with non-symbol"
	];
	Test[
		UpValues[f]
		,
		{}
		,
		TestID -> "Invalid arguments: list with non-symbol: UpValues"
	]
]


Module[{f, g},
	Test[
		SetDefaultOptionsValidation[f, g] // Hold[#]&
		,
		SetDefaultOptionsValidation[f, g] // Hold
		,
		Message[SetDefaultOptionsValidation::nonopt,
			g, 1, SetDefaultOptionsValidation[f, g]
		]
		,
		TestID -> "Invalid arguments: 2 symbols"
	];
	Test[
		UpValues[f]
		,
		{}
		,
		TestID -> "Invalid arguments: 2 symbols: UpValues first func"
	];
	Test[
		UpValues[g]
		,
		{}
		,
		TestID -> "Invalid arguments: 2 symbols: UpValues second func"
	]
]


Module[{f, g, opt1, opt2, val1, val2},
	With[{optName1 = SymbolName[opt1], optName2 = SymbolName[opt2]},
		Test[
			SetDefaultOptionsValidation[{f, g}, opt1 :> val1, opt2 :> val2] //
				Hold[#]&
			,
			SetDefaultOptionsValidation[{f, g}, opt1 :> val1, opt2 :> val2] //
				Hold
			,
			{
				Message[CheckOption::optnf,
					optName1, SetDefaultOptionsValidation
				],
				Message[CheckOption::optnf,
					optName2, SetDefaultOptionsValidation
				]
			}
			,
			TestID -> "Invalid arguments: list of symbols, 2 options"
		];
		Test[
			UpValues[f]
			,
			{}
			,
			TestID -> "Invalid arguments: list of symbols, 2 options: \
UpValues first func"
		];
		Test[
			UpValues[g]
			,
			{}
			,
			TestID -> "Invalid arguments: list of symbols, 2 options: \
UpValues second func"
		]
	]
]


(* ::Section:: *)
(*TearDown*)


On[General::stop]


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
