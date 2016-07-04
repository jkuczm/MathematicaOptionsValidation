(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["OptionsValidation`Tests`ValidOptionsQ`", {"MUnit`"}]


Get["OptionsValidation`"]


withMockedGenerateValidator =
	Function[body,
		Block[
			{
				OptionsValidation`Private`generateValidator,
				$generateValidatorLog = {},
				validatorResult
			},
			OptionsValidation`Private`generateValidator
				[args1___][args2___][ags3___] :=
			(
				AppendTo[$generateValidatorLog,
					Hold@OptionsValidation`Private`generateValidator
						[args1][args2][ags3]
				];
				validatorResult
			);
			
			body
		],
		HoldFirst
	]


Off[General::stop]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Default options*)


withMockedGenerateValidator@Module[{validator, f, opt1, opt2, valA, valB},
	TestMatch[
		validator = ValidOptionsQ[f]
		,
		_Function
		,
		TestID -> "Default options: symbol: validator generation"
	];
	
	Test[
		validator[opt1 -> valA, opt2 :> valB]
		,
		validatorResult
		,
		TestID -> "Default options: symbol: options: validator evaluation"
	];
	Test[
		$generateValidatorLog
		,
		{
			Hold@OptionsValidation`Private`generateValidator[Symbol][
				False, First, CheckOption, CheckOptionRelations
			][f, {opt1 -> valA, opt2 :> valB}]
		}
		,
		TestID -> "Default options: symbol: options: $generateValidatorLog"
	]
]


(* ::Subsection:: *)
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
		withMockedGenerateValidator[
			Module[{validator, f, check, checkRel},
				TestMatch[
					validator = ValidOptionsQ[f,
						CheckOption -> check,
						ChecksStopOnInvalid -> stopOnInvalid,
						{
							"CheckOptionRelations" -> checkRel,
							CheckedDuplicate :> duplicate
						}
					]
					,
					_Function
					,
					TestID -> "Explicit valid options: symbol: \
validator generation",
					TestFailureMessage -> failureMessage
				];
				
				Module[{opt1, opt3, valA, valB, valC, valD},
					Test[
						validator[
							opt1 -> valA, "opt2" -> valB,
							{{opt3 :> valC}, "opt1" :> valD}
						]
						,
						validatorResult
						,
						TestID -> "Explicit valid options: symbol: options: \
validator evaluation",
						TestFailureMessage -> failureMessage
					];
					Test[
						$generateValidatorLog
						,
						{
							Hold@OptionsValidation`Private`generateValidator[
								Symbol
							][stopOnInvalid, duplicate, check, checkRel][
								f, {
									opt1 -> valA, "opt2" -> valB,
									opt3 :> valC, "opt1" :> valD
								}
							]
						}
						,
						TestID -> "Explicit valid options: symbol: options: \
$generateValidatorLog",
						TestFailureMessage -> failureMessage
					]
				];
				
				$generateValidatorLog = {};
				Module[{opt1, valA, valB, nonOpt1},
					Test[
						validator[
							{opt1 -> valA, {nonOpt1}},
							"nonOpt2", "opt2" :> valB
						]
						,
						$Failed
						,
						{
							Message[ValidOptionsQ::rep, nonOpt1],
							Message[ValidOptionsQ::rep, "nonOpt2"]
						}
						,
						TestID -> "Explicit valid options: symbol: \
non-options: validator evaluation",
						TestFailureMessage -> failureMessage
					];
					Test[
						$generateValidatorLog
						,
						{}
						,
						TestID -> "Explicit valid options: symbol: \
non-options: $generateValidatorLog",
						TestFailureMessage -> failureMessage
					]
				]
			]
		];
		
		withMockedGenerateValidator[
			Module[{validator, f, g, h, check, checkRel, inValidDuplicates},
				TestMatch[
					validator = ValidOptionsQ[{f, g, h},
						{CheckedDuplicate -> duplicate},
						CheckOption :> check,
						ChecksStopOnInvalid -> stopOnInvalid,
						"CheckedDuplicate" -> inValidDuplicates,
						"CheckOptionRelations" -> checkRel
					]
					,
					_Function
					,
					TestID -> "Explicit valid options: list: \
validator generation",
					TestFailureMessage -> failureMessage
				];
				
				Module[{opt1, opt2, valA, valB},
					Test[
						validator[
							{{"opt1" -> valA}}, opt2 -> valB, opt1 :> "valC"
						]
						,
						validatorResult
						,
						TestID -> "Explicit valid options: list: options: \
validator evaluation",
						TestFailureMessage -> failureMessage
					];
					Test[
						$generateValidatorLog
						,
						{
							Hold@OptionsValidation`Private`generateValidator[
								List
							][stopOnInvalid, duplicate, check, checkRel][
								{f, g, h},
								{"opt1" -> valA, opt2 -> valB, opt1 :> "valC"}
							]
						}
						,
						TestID -> "Explicit valid options: list: options: \
$generateValidatorLog",
						TestFailureMessage -> failureMessage
					]
				];
				
				$generateValidatorLog = {};
				Module[{nonOpt1, nonOpt2, nonOpt3},
					Test[
						validator[nonOpt1, {nonOpt2, nonOpt3}]
						,
						$Failed
						,
						{
							Message[ValidOptionsQ::rep, nonOpt1],
							Message[ValidOptionsQ::rep, nonOpt2],
							Message[ValidOptionsQ::rep, nonOpt3]
						}
						,
						TestID -> "Explicit valid options: list: non-options: \
validator evaluation",
						TestFailureMessage -> failureMessage
					];
					Test[
						$generateValidatorLog
						,
						{}
						,
						TestID -> "Explicit valid options: list: non-options: \
$generateValidatorLog",
						TestFailureMessage -> failureMessage
					]
				]
			]
		]
	],
	{stopOnInvalid, {True, False}},
	{duplicate, {First, Last, All}}
]


(* ::Subsection:: *)
(*Invalid options*)


Module[{f, check, checkRel, invalid1, invalid2, invalid3},
	Test[
		ValidOptionsQ[f,
			"ChecksStopOnInvalid" -> invalid1,
			{
				CheckedDuplicate -> invalid2,
				{
					ChecksStopOnInvalid :> invalid3,
					CheckOptionRelations -> checkRel
				},
				CheckOption -> check
			}
		] // Hold[#]&
		,
		ValidOptionsQ[f,
			"ChecksStopOnInvalid" -> invalid1,
			{
				CheckedDuplicate -> invalid2,
				{
					ChecksStopOnInvalid :> invalid3,
					CheckOptionRelations -> checkRel
				},
				CheckOption -> check
			}
		] // Hold
		,
		{
			Message[ValidOptionsQ::opttf, ChecksStopOnInvalid, invalid1],
			Message[ValidOptionsQ::optfla, CheckedDuplicate, invalid2]
		}
		,
		TestID -> "Explicit invalid options: symbol"
	]
]
Module[{f, g},
	Test[
		ValidOptionsQ[{f, g},
			CheckedDuplicate -> "invalidDuplicatesValue",
			"ChecksStopOnInvalid" :> "invalidChecksStopOnInvalid"
		] // Hold[#]&
		,
		ValidOptionsQ[{f, g},
			CheckedDuplicate -> "invalidDuplicatesValue",
			"ChecksStopOnInvalid" :> "invalidChecksStopOnInvalid"
		] // Hold
		,
		{
			Message[ValidOptionsQ::optfla,
				CheckedDuplicate, "invalidDuplicatesValue"
			],
			Message[ValidOptionsQ::opttf,
				ChecksStopOnInvalid, "invalidChecksStopOnInvalid"
			]
		}
		,
		TestID -> "Explicit invalid options: list"
	]
]


(* ::Subsection:: *)
(*Invalid arguments*)


Test[
	ValidOptionsQ[] // Hold[#]&
	,
	ValidOptionsQ[] // Hold
	,
	Message[ValidOptionsQ::argx, ValidOptionsQ, 0]
	,
	TestID -> "Invalid arguments: no args"
]


Test[
	ValidOptionsQ["str"] // Hold[#]&
	,
	ValidOptionsQ["str"] // Hold
	,
	Message[ValidOptionsQ::symse, 1, ValidOptionsQ["str"]]
	,
	TestID -> "Invalid arguments: string"
]


Module[{nonOption},
	Test[
		ValidOptionsQ[3, nonOption] // Hold[#]&
		,
		ValidOptionsQ[3, nonOption] // Hold
		,
		{
			Message[ValidOptionsQ::symse, 1, ValidOptionsQ[3, nonOption]],
			Message[ValidOptionsQ::nonopt,
				nonOption, 1, ValidOptionsQ[3, nonOption]
			]
		}
		,
		TestID -> "Invalid arguments: integer, non-option"
	]
]


Module[{f, opt1, opt2, val1, val2},
	With[{opt1Name = SymbolName[opt1], opt2Name = SymbolName[opt2]},
		Test[
			ValidOptionsQ[f, opt1 -> val1, opt2 :> val2] // Hold[#]&
			,
			ValidOptionsQ[f, opt1 -> val1, opt2 :> val2] // Hold
			,
			{
				Message[CheckOption::optnf, opt1Name, ValidOptionsQ],
				Message[CheckOption::optnf, opt2Name, ValidOptionsQ]
			}
			,
			TestID -> "Invalid arguments: symbol, 2 unknown options"
		]
	]
]


(* ::Subsection:: *)
(*Set invalid default options*)


Module[{check, invalid1, invalid2, invalid3},
	Test[
		SetOptions[ValidOptionsQ,
			{
				ChecksStopOnInvalid -> False,
				CheckOption -> check
			},
			ChecksStopOnInvalid -> invalid1,
			{
				CheckedDuplicate -> invalid2,
				{CheckedDuplicate -> invalid3}
			}
		]
		,
		$Failed
		,
		{
			Message[ValidOptionsQ::opttf, ChecksStopOnInvalid, invalid1],
			Message[ValidOptionsQ::optfla, CheckedDuplicate, invalid3]
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
