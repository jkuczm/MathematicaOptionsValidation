(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["OptionsValidation`Tests`CheckOption`", {"MUnit`"}]


Get["OptionsValidation`"]


symmetricDifference = Complement[Join[##], Intersection[##]]&


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Definitions*)


(* ::Subsubsection:: *)
(*SubValues*)


Module[
	{
		f, opt, wrongValue, wrongValueResult,
		oldCheckOptionDef = Language`ExtendedDefinition[CheckOption]
	}
	,
	Test[
		CheckOption[f, "testName" | opt][wrongValue] = wrongValueResult
		,
		wrongValueResult
		,
		TestID -> "SubValues: Set: 1 symbol: define"
	];
	
	With[{optSymName = SymbolName[opt]},
		Test[
			UpValues[f]
			,
			{HoldPattern@CheckOption[f, "testName" | optSymName, wrongValue] :>
				wrongValueResult}
			,
			TestID -> "SubValues: Set: 1 symbol: symbol UpValues"
		]
	];
	Test[
		Language`ExtendedDefinition[CheckOption]
		,
		oldCheckOptionDef
		,
		TestID -> "SubValues: Set: 1 symbol: CheckOption definition"
	]
]


Module[
	{
		f, g, opt, wrongValue,
		oldCheckOptionDef = Language`ExtendedDefinition[CheckOption]
	}
	,
	Test[
		CheckOption[func : f | g, optName : opt][optVal : wrongValue] :=
			{func, optName, optVal}
		,
		Null
		,
		TestID -> "SubValues: SetDelayed: 2 symbols: define"
	];
	
	With[{optSymName = SymbolName[opt]},
		Test[
			UpValues[f]
			,
			{
				HoldPattern@CheckOption[
					func : f, optName : optSymName, optVal : wrongValue
				] :>
					{func, optName, optVal}
			}
			,
			TestID -> "SubValues: SetDelayed: 2 symbols: \
first symbol UpValues"
		];
		Test[
			UpValues[g]
			,
			{
				HoldPattern@CheckOption[
					func : g, optName : optSymName, optVal : wrongValue
				] :>
					{func, optName, optVal}
			}
			,
			TestID -> "SubValues: SetDelayed: 2 symbols: \
second symbol UpValues"
		]
	];
	Test[
		Language`ExtendedDefinition[CheckOption]
		,
		oldCheckOptionDef
		,
		TestID -> "SubValues: SetDelayed: 2 symbols: CheckOption definition"
	]
]


Module[
	{
		f, g, h, opt, wrongValue, wrongValueResult1, wrongValueResult2,
		oldCheckOptionDef = Language`ExtendedDefinition[CheckOption]
	}
	,
	CheckOption[f, opt][wrongValue] = wrongValueResult1;
	CheckOption[g | h, opt][wrongValue] := wrongValueResult2;
	
	Test[
		CheckOption[f | g | h, opt][wrongValue] =.
		,
		Null
		,
		TestID -> "SubValues: Unset: 3 symbols: define"
	];
	
	Test[
		UpValues[f]
		,
		{}
		,
		TestID -> "SubValues: Unset: 3 symbols: first symbol UpValues"
	];
	Test[
		UpValues[g]
		,
		{}
		,
		TestID -> "SubValues: Unset: 3 symbols: second symbol UpValues"
	];
	Test[
		UpValues[h]
		,
		{}
		,
		TestID -> "SubValues: Unset: 3 symbols: third symbol UpValues"
	];
	
	Test[
		Language`ExtendedDefinition[CheckOption]
		,
		oldCheckOptionDef
		,
		TestID -> "SubValues: Unset: 3 symbols: CheckOption definition"
	]
]


Module[
	{
		f, g, h, opt, subOpt, val, wrongValueResult,
		oldCheckOptionDef = Language`ExtendedDefinition[CheckOption]
	}
	,
	Test[
		CheckOption[f | g | h, opt -> subOpt][val] = wrongValueResult
		,
		wrongValueResult
		,
		TestID -> "SubValues: Set: sub-option: define"
	];
	
	With[{optSymName = SymbolName[opt], subOptSymName = SymbolName[subOpt]},
		TestMatch[
			UpValues[f]
			,
			{
				Verbatim[
					HoldPattern@
						CheckOption[f, optSymName :> subOptSymName, val] :>
							wrongValueResult
				],
				Verbatim[HoldPattern]@HoldPattern@CheckOption[
					f,
					Verbatim[Pattern][_, Verbatim[HoldPattern][optSymName]],
					Verbatim[Pattern][_, Verbatim[_]]
				] :>
					_
			}
			,
			TestID -> "SubValues: Set: sub-option: first symbol UpValues"
		];
		TestMatch[
			UpValues[g]
			,
			{
				Verbatim[
					HoldPattern@
						CheckOption[g, optSymName :> subOptSymName, val] :>
							wrongValueResult
				],
				Verbatim[HoldPattern]@HoldPattern@CheckOption[
					g,
					Verbatim[Pattern][_, Verbatim[HoldPattern][optSymName]],
					Verbatim[Pattern][_, Verbatim[_]]
				] :>
					_
			}
			,
			TestID -> "SubValues: Set: sub-option: second symbol UpValues"
		];
		TestMatch[
			UpValues[h]
			,
			{
				Verbatim[
					HoldPattern@
						CheckOption[h, optSymName :> subOptSymName, val] :>
							wrongValueResult
				],
				Verbatim[HoldPattern]@HoldPattern@CheckOption[
					h,
					Verbatim[Pattern][_, Verbatim[HoldPattern][optSymName]],
					Verbatim[Pattern][_, Verbatim[_]]
				] :>
					_
			}
			,
			TestID -> "SubValues: Set: sub-option: third symbol UpValues"
		]
	];
	Test[
		Language`ExtendedDefinition[CheckOption]
		,
		oldCheckOptionDef
		,
		TestID -> "SubValues: Set: sub-option: CheckOption definition"
	]
]


Module[
	{
		wrongValue, wrongValueResult,
		oldCheckOptionDef = Language`ExtendedDefinition[CheckOption]
	}
	,
	Test[
		CheckOption[_Symbol, "testOptName"][wrongValue] = wrongValueResult
		,
		$Failed
		,
		Message[CheckOption::unknownOptionOwnerPattern,
			CheckOption, Verbatim[_Symbol], TagSet
		]
		,
		TestID -> "SubValues: Set: wrong function pattern: define"
	];
	
	Test[
		Language`ExtendedDefinition[CheckOption]
		,
		oldCheckOptionDef
		,
		TestID -> "SubValues: Set: wrong function pattern: \
CheckOption definition"
	]
]


Module[
	{
		f, g, opt1, opt2, wrongValue, wrongValueResult, args,
		oldCheckOptionDef = Language`ExtendedDefinition[CheckOption]
	}
	,
	args = Sequence[f | g, opt1 | opt2];
	
	Test[
		CheckOption[args][wrongValue] := wrongValueResult
		,
		Null
		,
		TestID -> "SubValues: SetDelayed: CheckOption args from variable: \
define"
	];
	
	With[{opt1SymName = SymbolName[opt1], opt2SymName = SymbolName[opt2]},
		Test[
			UpValues[f]
			,
			{HoldPattern@
				CheckOption[f, opt1SymName | opt2SymName, wrongValue] :>
					wrongValueResult}
			,
			TestID -> "SubValues: SetDelayed: CheckOption args from variable: \
first symbol UpValues"
		];
		Test[
			UpValues[g]
			,
			{HoldPattern@
				CheckOption[g, opt1SymName | opt2SymName, wrongValue] :>
					wrongValueResult}
			,
			TestID -> "SubValues: SetDelayed: CheckOption args from variable: \
second symbol UpValues"
		]
	];
	Test[
		Language`ExtendedDefinition[CheckOption]
		,
		oldCheckOptionDef
		,
		TestID -> "SubValues: SetDelayed: CheckOption args from variable: \
CheckOption definition"
	]
]


Module[{opt, val, wrongValueResult, oldSubValues = SubValues[CheckOption]},
	Internal`InheritedBlock[{CheckOption},
		Unprotect[CheckOption];
		Test[
			CheckOption[_Symbol, opt][val] := wrongValueResult
			,
			Null
			,
			Message[CheckOption::unprotected, CheckOption,
				CheckOption[Verbatim[_Symbol], opt][val] := wrongValueResult
			]
			,
			TestID -> "SubValues: SetDelayed: unprotected CheckOption: define"
		];
		
		Test[
			symmetricDifference[SubValues[CheckOption], oldSubValues]
			,
			{HoldPattern@CheckOption[_Symbol, opt][val] :> wrongValueResult}
			,
			TestID -> "SubValues: SetDelayed: unprotected CheckOption: \
CheckOption new SubValues"
		]
	]
]


(* ::Subsubsection:: *)
(*DownValues*)


Module[
	{
		f, g, h, testOpt, testFunc,
		oldCheckOptionDef = Language`ExtendedDefinition[CheckOption]
	}
	,
	Test[
		CheckOption[f | g | h, testOpt] = testFunc
		,
		testFunc
		,
		TestID -> "DownValues: Set: 3 symbols: define"
	];
	
	With[{testOptSymName = SymbolName[testOpt]},
		Test[
			UpValues[f]
			,
			{HoldPattern@CheckOption[f, testOptSymName] :> testFunc}
			,
			TestID -> "DownValues: Set: 3 symbols: first symbol UpValues"
		];
		Test[
			UpValues[g]
			,
			{HoldPattern@CheckOption[g, testOptSymName] :> testFunc}
			,
			TestID -> "DownValues: Set: 3 symbols: second symbol UpValues"
		];
		Test[
			UpValues[h]
			,
			{HoldPattern@CheckOption[h, testOptSymName] :> testFunc}
			,
			TestID -> "DownValues: Set: 3 symbols: third symbol UpValues"
		]
	];
	Test[
		Language`ExtendedDefinition[CheckOption]
		,
		oldCheckOptionDef
		,
		TestID -> "DownValues: Set: 3 symbols: CheckOption definition"
	]
]


Module[
	{
		f, testFunc,
		oldCheckOptionDef = Language`ExtendedDefinition[CheckOption]
	}
	,
	Test[
		CheckOption[f, name_Complex] := testFunc[name, #]&
		,
		Null
		,
		Message[CheckOption::unknownOptionNamePattern, Verbatim[_Complex]]
		,
		TestID -> "DownValues: SetDelayed: 1 symbol: define"
	];
	
	Test[
		UpValues[f]
		,
		{HoldPattern@CheckOption[f, name_Complex] :> (testFunc[name, #]&)}
		,
		TestID -> "DownValues: SetDelayed: 1 symbol: symbol UpValues"
	];
	Test[
		Language`ExtendedDefinition[CheckOption]
		,
		oldCheckOptionDef
		,
		TestID -> "DownValues: SetDelayed: 1 symbol: CheckOption definition"
	]
]


Module[
	{
		f, g, testOpt,
		oldCheckOptionDef = Language`ExtendedDefinition[CheckOption]
	}
	,
	CheckOption[f | g, name:testOpt] = name&;
	
	With[{testOptSymName = SymbolName[testOpt]},
		Test[
			CheckOption[f, name:testOptSymName] =.
			,
			Null
			,
			TestID -> "DownValues: Unset: 1 of 2 symbols: define"
		];
		
		Test[
			UpValues[f]
			,
			{}
			,
			TestID -> "DownValues: Unset: 1 of 2 symbols: \
first symbol UpValues"
		];
		Test[
			UpValues[g]
			,
			{HoldPattern@CheckOption[g, name:testOptSymName] :> (name&)}
			,
			TestID -> "DownValues: Unset: 1 of 2 symbols: \
second symbol UpValues"
		]
	];
	Test[
		Language`ExtendedDefinition[CheckOption]
		,
		oldCheckOptionDef
		,
		TestID -> "DownValues: Unset: 1 of 2 symbols: CheckOption definition"
	]
]


Module[
	{
		f, g, opt, x, subSubOpt, testFunc,
		oldCheckOptionDef = Language`ExtendedDefinition[CheckOption]
	}
	,
	Test[
		CheckOption[f | g, opt :> x + x -> subSubOpt] := testFunc
		,
		Null
		,
		{
			Message[CheckOption::unknownOptionNamePattern, x + x],
			Message[CheckOption::unknownOptionNamePattern, x + x]
		}
		,
		TestID -> "DownValues: SetDelayed: sub-option: define"
	];
	
	With[
		{
			optSymName = SymbolName[opt],
			subSubOptSymName = SymbolName[subSubOpt]
		}
		,
		TestMatch[
			UpValues[f]
			,
			{
				Verbatim[
					HoldPattern@CheckOption[
						f, optSymName :> x + x :> subSubOptSymName
					] :>
						testFunc
				],
				Verbatim[HoldPattern]@HoldPattern@CheckOption[f,
					Verbatim[Pattern][_, Verbatim[HoldPattern][optSymName]],
					Verbatim[Pattern][_, Verbatim[_]]
				] :>
					_,
				Verbatim[HoldPattern]@HoldPattern@CheckOption[f,
					Verbatim[Pattern][_, Verbatim[HoldPattern][optSymName]] :>
						Verbatim[Pattern][
							_, Verbatim[HoldPattern]@HoldPattern[x + x]
						],
					Verbatim[Pattern][_, Verbatim[_]]
				] :>
					_
			}
			,
			TestID -> "DownValues: SetDelayed: sub-option: \
first symbol UpValues"
		];
		TestMatch[
			UpValues[g]
			,
			{
				Verbatim[
					HoldPattern@CheckOption[
						g, optSymName :> x + x :> subSubOptSymName
					] :>
						testFunc
				],
				Verbatim[HoldPattern]@HoldPattern@CheckOption[g,
					Verbatim[Pattern][_, Verbatim[HoldPattern][optSymName]],
					Verbatim[Pattern][_, Verbatim[_]]
				] :>
					_,
				Verbatim[HoldPattern]@HoldPattern@CheckOption[g,
					Verbatim[Pattern][_, Verbatim[HoldPattern][optSymName]] :>
						Verbatim[Pattern][
							_, Verbatim[HoldPattern]@HoldPattern[x + x]
						],
					Verbatim[Pattern][_, Verbatim[_]]
				] :>
					_
			}
			,
			TestID -> "DownValues: SetDelayed: sub-option: \
second symbol UpValues"
		]
	];
	Test[
		Language`ExtendedDefinition[CheckOption]
		,
		oldCheckOptionDef
		,
		TestID -> "DownValues: SetDelayed: sub-option: CheckOption definition"
	]
]


Module[
	{
		f, g, symTest, opt, testFunc,
		oldCheckOptionDef = Language`ExtendedDefinition[CheckOption]
	}
	,
	Test[
		CheckOption[func : f | g?symTest, opt] := testFunc[func]
		,
		$Failed
		,
		Message[CheckOption::unknownOptionOwnerPattern,
			CheckOption, Verbatim[func$ : f | g?symTest], TagSetDelayed
		]
		,
		TestID -> "DownValues: SetDelayed: wrong function pattern: define"
	];
	
	Test[
		UpValues[f]
		,
		{}
		,
		TestID -> "DownValues: SetDelayed: wrong function pattern: \
first symbol UpValues"
	];
	Test[
		UpValues[g]
		,
		{}
		,
		TestID -> "DownValues: SetDelayed: wrong function pattern: \
second symbol UpValues"
	];
	Test[
		Language`ExtendedDefinition[CheckOption]
		,
		oldCheckOptionDef
		,
		TestID -> "DownValues: SetDelayed: wrong function pattern: \
CheckOption definition"
	]
]


Module[
	{
		f, opt, testFunc, args,
		oldCheckOptionDef = Language`ExtendedDefinition[CheckOption]
	}
	,
	CheckOption[f, opt] := testFunc;
	args = Sequence[f, opt];
	
	Test[
		CheckOption[args] =.
		,
		Null
		,
		TestID -> "DownValues: Unset: CheckOption args from variable: define"
	];
	
	Test[
		UpValues[f]
		,
		{}
		,
		TestID -> "DownValues: Unset: CheckOption args from variable: \
symbol UpValues"
	];
	Test[
		Language`ExtendedDefinition[CheckOption]
		,
		oldCheckOptionDef
		,
		TestID -> "DownValues: Unset: CheckOption args from variable: \
CheckOption definition"
	]
]


Module[{opt, testFunc, oldDownValues = DownValues[CheckOption]},
	Internal`InheritedBlock[{CheckOption},
		Unprotect[CheckOption];
		Test[
			CheckOption[_, opt] = testFunc
			,
			testFunc
			,
			Message[CheckOption::unprotected, CheckOption,
				CheckOption[Verbatim[_], opt] = testFunc
			]
			,
			TestID -> "DownValues: Set: unprotected CheckOption: define"
		];
		
		Test[
			symmetricDifference[DownValues[CheckOption], oldDownValues]
			,
			{HoldPattern@CheckOption[_, opt] :> testFunc}
			,
			TestID -> "DownValues: Set: unprotected CheckOption: \
CheckOption new DownValues"
		]
	]
]


(* ::Subsubsection:: *)
(*UpValues*)


Module[
	{
		f, opt, subOpt, subSubOpt, subSubSubOpt, val, res,
		oldCheckOptionDef = Language`ExtendedDefinition[CheckOption]
	}
	,
	Test[
		f /: CheckOption[f, opt -> subOpt -> subSubOpt -> subSubSubOpt, val] :=
			res
		,
		Null
		,
		TestID -> "UpValues: 3 args: TagSetDelayed: sub-option: define"
	];
	
	With[
		{
			optSymName = SymbolName[opt],
			subOptSymName = SymbolName[subOpt],
			subSubOptSymName = SymbolName[subSubOpt],
			subSubSubOptSymName = SymbolName[subSubSubOpt]
		}
		,
		TestMatch[
			UpValues[f]
			,
			{
				Verbatim[
					HoldPattern@CheckOption[f,
						optSymName :> subOptSymName :> subSubOptSymName :>
							subSubSubOptSymName,
						val
					] :>
						res
				],
				Verbatim[HoldPattern]@HoldPattern@CheckOption[f,
					Verbatim[Pattern][_, Verbatim[HoldPattern][optSymName]],
					Verbatim[Pattern][_, Verbatim[_]]
				] :>
					_,
				Verbatim[HoldPattern]@HoldPattern@CheckOption[f,
					Verbatim[Pattern][_, Verbatim[HoldPattern][optSymName]] :>
						Verbatim[Pattern][
							_, Verbatim[HoldPattern]@HoldPattern[subOptSymName]
						],
					Verbatim[Pattern][_, Verbatim[_]]
				] :>
					_,
				Verbatim[HoldPattern]@HoldPattern@CheckOption[f,
					Verbatim[Pattern][_, Verbatim[HoldPattern][optSymName]] :>
						Verbatim[Pattern][
							_, Verbatim[HoldPattern]@HoldPattern[subOptSymName]
						] :>
							Verbatim[Pattern][
								_,
								Verbatim[HoldPattern]@
									HoldPattern[subSubOptSymName]
							]
					,
					Verbatim[Pattern][_, Verbatim[_]]
				] :>
					_
			}
			,
			TestID -> "UpValues: 3 args: TagSetDelayed: sub-option: \
symbol UpValues"
		]
	];
	Test[
		Language`ExtendedDefinition[CheckOption]
		,
		oldCheckOptionDef
		,
		TestID -> "UpValues: 3 args: TagSetDelayed: sub-option: \
CheckOption definition"
	]
]


Module[
	{
		f, opt, wrongValue, wrongValueResult, args,
		oldCheckOptionDef = Language`ExtendedDefinition[CheckOption]
	}
	,
	args = Sequence[f, opt, wrongValue];
	
	With[{optSymName = SymbolName[opt]},
		f /: CheckOption[f, optSymName, wrongValue] = wrongValueResult;
	];
		
	Test[
		f /: CheckOption[args] =.
		,
		Null
		,
		TestID -> "UpValues: 3 args: TagUnset: \
CheckOption args from variable: define"
	];
	
	Test[
		UpValues[f]
		,
		{}
		,
		TestID -> "UpValues: 3 args: TagUnset: \
CheckOption args from variable: symbol UpValues"
	];
	Test[
		Language`ExtendedDefinition[CheckOption]
		,
		oldCheckOptionDef
		,
		TestID -> "UpValues: 3 args: TagUnset: \
CheckOption args from variable: CheckOption definition"
	]
]


Module[
	{
		f, fTest, opt1, testFunc, args,
		oldCheckOptionDef = Language`ExtendedDefinition[CheckOption]
	}
	,
	args = Sequence[f?fTest, opt1 | "opt2"];
	
	Test[
		f /: CheckOption[args] = testFunc
		,
		testFunc
		,
		TestID -> "UpValues: 2 args: TagSet: \
CheckOption args from variable: define"
	];
	
	With[{optSymName = SymbolName[opt1]},
		Test[
			UpValues[f]
			,
			{HoldPattern@CheckOption[f?fTest, optSymName | "opt2"] :> testFunc}
			,
			TestID -> "UpValues: 2 args: TagSet: \
CheckOption args from variable: symbol UpValues"
		]
	];
	Test[
		Language`ExtendedDefinition[CheckOption]
		,
		oldCheckOptionDef
		,
		TestID -> "UpValues: 2 args: TagSet: \
CheckOption args from variable: CheckOption definition"
	]
]


(* ::Subsection:: *)
(*Call*)


(* ::Subsubsection:: *)
(*DownValues*)


Module[{f},
	Test[
		CheckOption[f, "testOptName"] // Hold[#]&
		,
		CheckOption[f, "testOptName"] // Hold
		,
		TestID -> "Call: DownValues: String"
	]
]
Module[{f, name},
	With[{nameStr = SymbolName[name]},
		Test[
			CheckOption[f, name] // Hold[#]&
			,
			CheckOption[f, nameStr] // Hold
			,
			TestID -> "Call: DownValues: Symbol"
		]
	]
]
Module[{f, opt, subOpt},
	With[{optSymName = SymbolName[opt], subOptSymName = SymbolName[subOpt]},
		Test[
			CheckOption[f, opt -> subOpt] // Hold[#]&
			,
			CheckOption[f, optSymName :> subOptSymName] // Hold
			,
			TestID -> "Call: DownValues: Rule"
		]
	]
]
Module[{f, opt, subOpt},
	With[{optSymName = SymbolName[opt], subOptSymName = SymbolName[subOpt]},
		Test[
			CheckOption[f, opt :> subOpt] // Hold[#]&
			,
			CheckOption[f, optSymName :> subOptSymName] // Hold
			,
			TestID -> "Call: DownValues: RuleDelayed"
		]
	]
]
Module[{f, opt, subSubOpt, subSubSubOpt, x},
	With[
		{
			optSymName = SymbolName[opt],
			subSubOptSymName = SymbolName[subSubOpt],
			subSubSubOptSymName = SymbolName[subSubSubOpt]
		}
		,
		Test[
			CheckOption[f, opt :> x + x -> subSubOpt :> subSubSubOpt] //
				Hold[#]&
			,
			CheckOption[f,
				optSymName :>
					x + x :> subSubOptSymName :> subSubSubOptSymName
			] // Hold
			,
			TestID -> "Call: DownValues: nested rules"
		]
	]
]


Test[
	CheckOption[] // Hold[#]&
	,
	CheckOption[] // Hold
	,
	Message[CheckOption::argt, CheckOption, 0, 2, 3]
	,
	TestID -> "Call: DownValues: incorrect args: no args"
]
Module[{f},
	Test[
		CheckOption[f] // Hold[#]&
		,
		CheckOption[f] // Hold
		,
		Message[CheckOption::argtu, CheckOption, 2, 3]
		,
		TestID -> "Call: DownValues: incorrect args: symbol"
	]
]
Module[{a, b},
	Test[
		CheckOption["nonSym", a, b] // Hold[#]&
		,
		CheckOption["nonSym", a, b] // Hold
		,
		Message[CheckOption::sym, "nonSym", 1]
		,
		TestID -> "Call: DownValues: incorrect args: string, 2 symbols"
	]
]
Module[{a, b, c, d},
	Test[
		CheckOption[a, b, c, d] // Hold[#]&
		,
		CheckOption[a, b, c, d] // Hold
		,
		Message[CheckOption::argt, CheckOption, 4, 2, 3]
		,
		TestID -> "Call: DownValues: incorrect args: 4 symbols"
	]
]


(* ::Subsubsection:: *)
(*SubValues*)


Module[{f, val},
	Test[
		CheckOption[f, "testOptName"][val]
		,
		False
		,
		Message[CheckOption::optnf, "testOptName", f]
		,
		TestID -> "Call: SubValues: String: undefined"
	]
]
Module[{f, testOpt, default, val},
	Options[f] = {testOpt -> default};
	Test[
		CheckOption[f, SymbolName[testOpt]][val]
		,
		True
		,
		TestID -> "Call: SubValues: String: valid"
	]
]
Module[{f, opt, result},
	CheckOption[f, opt][value_] :=
		(Message[f::opt, HoldForm[value]]; result);
	
	Module[{val},
		Test[
			CheckOption[f, SymbolName[opt]][val]
			,
			result
			,
			Message[f::opt, val]
			,
			TestID -> "Call: SubValues: String: invalid: SubValues def"
		]
	]
]
Module[{f, opt, result},
	CheckOption[f, opt] = (Message[f::opt, HoldForm[#]]; result)&;
	
	Module[{val},
		Test[
			CheckOption[f, SymbolName[opt]][val]
			,
			result
			,
			Message[f::opt, val]
			,
			TestID -> "Call: SubValues: String: invalid: DownValues def"
		]
	]
]


Module[{f, name, val},
	With[{nameStr = SymbolName[name]},
		Test[
			CheckOption[f, name][val]
			,
			False
			,
			Message[CheckOption::optnf, nameStr, f]
			,
			TestID -> "Call: SubValues: Symbol: undefined"
		]
	]
]
Module[{f, name, default, val},
	Options[f] = {SymbolName[name] -> default};
	Test[
		CheckOption[f, name][val]
		,
		True
		,
		TestID -> "Call: SubValues: Symbol: valid"
	]
]
Module[{f, name, result},
	CheckOption[f, SymbolName[name]][value_] :=
		(Message[f::opt, HoldForm[value]]; result);
	
	Module[{val},
		Test[
			CheckOption[f, name][val]
			,
			result
			,
			Message[f::opt, val]
			,
			TestID -> "Call: SubValues: Symbol: invalid: SubValues def"
		]
	]
]
Module[{f, name, result},
	CheckOption[f, SymbolName[name]] = (Message[f::opt, HoldForm[#]]; result)&;
	
	Module[{val},
		Test[
			CheckOption[f, name][val]
			,
			result
			,
			Message[f::opt, val]
			,
			TestID -> "Call: SubValues: Symbol: invalid: DownValues def"
		]
	]
]


Module[{f, name, subName, val},
	With[{nameStr = SymbolName[name]},
		Test[
			CheckOption[f, name -> subName][val]
			,
			False
			,
			Message[CheckOption::optnf, nameStr, f]
			,
			TestID -> "Call: SubValues: Rule: unknown"
		]
	]
]
Module[{f, name, subName, default, val},
	Options[f] = {name -> default};
	Test[
		CheckOption[f, name -> subName][val]
		,
		True
		,
		TestID -> "Call: SubValues: Rule: valid"
	]
]
Module[{f, name, subName, result},
	CheckOption[f, name -> SymbolName[subName]][value_] :=
		(Message[f::opt, HoldForm[value]]; result);
	
	Module[{val},
		Test[
			CheckOption[f, SymbolName[name] -> subName][val]
			,
			result
			,
			Message[f::opt, val]
			,
			TestID -> "Call: SubValues: Rule: invalid: SubValues def"
		]
	];
	Module[{val, val2},
		Test[
			CheckOption[f, SymbolName[name]][{subName -> val, subName :> val2}]
			,
			result
			,
			Message[f::opt, val]
			,
			TestID -> "Call: SubValues: Rule: invalid: SubValues def: \
parent with"
		]
	];
	Module[{val, subName2},
		Test[
			CheckOption[f, SymbolName[name]][{subName2 -> val}]
			,
			True
			,
			TestID -> "Call: SubValues: Rule: invalid: SubValues def: \
parent without"
		]
	]
]
Module[{f, name, subName, result},
	CheckOption[f, SymbolName[name] -> subName] =
		(Message[f::opt, HoldForm[#]]; result)&;
	
	Module[{val},
		Test[
			CheckOption[f, name -> SymbolName[subName]][val]
			,
			result
			,
			Message[f::opt, val]
			,
			TestID -> "Call: SubValues: Rule: invalid: DownValues def"
		]
	];
	Module[{val},
		Test[
			CheckOption[f, name][SymbolName[subName] :> val]
			,
			result
			,
			Message[f::opt, val]
			,
			TestID -> "Call: SubValues: Rule: invalid: DownValues def: \
parent with"
		]
	];
	Module[{val1, val2},
		Test[
			CheckOption[f, name][{subName -> val1, subName :> val2}]
			,
			result
			,
			Message[f::opt, val1]
			,
			TestID -> "Call: SubValues: Rule: invalid: DownValues def: \
parent with duplicated symbol, symbol"
		]
	];
	Module[{val1, val2},
		Test[
			CheckOption[f, name][
				{SymbolName[subName] -> val1, subName -> val2}
			]
			,
			result
			,
			Message[f::opt, val1]
			,
			TestID -> "Call: SubValues: Rule: invalid: DownValues def: \
parent with duplicated string, symbol"
		]
	];
	Module[{val},
		Test[
			CheckOption[f, SymbolName[name]][val]
			,
			True
			,
			TestID -> "Call: SubValues: Rule: invalid: DownValues def: \
parent without"
		]
	]
]


Module[{f, name, subName, val},
	With[{nameStr = SymbolName[name]},
		Test[
			CheckOption[f, name :> subName][val]
			,
			False
			,
			Message[CheckOption::optnf, nameStr, f]
			,
			TestID -> "Call: SubValues: RuleDelayed: unknown"
		]
	]
]
Module[{f, name, subName, default, val},
	Options[f] = {name :> default};
	Test[
		CheckOption[f, SymbolName[name] :> subName][val]
		,
		True
		,
		TestID -> "Call: SubValues: RuleDelayed: valid"
	]
]
Module[{f, name, subName, result},
	CheckOption[f, SymbolName[name] :> subName][value_] :=
		(Message[f::opt, HoldForm[value]]; result);
	
	Module[{val},
		Test[
			CheckOption[f, name :> Evaluate@SymbolName[subName]][val]
			,
			result
			,
			Message[f::opt, val]
			,
			TestID -> "Call: SubValues: RuleDelayed: invalid: SubValues def"
		]
	];
	Module[{val},
		Test[
			CheckOption[f, SymbolName[name]][{val, subName :> val}]
			,
			result
			,
			Message[f::opt, val]
			,
			TestID -> "Call: SubValues: RuleDelayed: invalid: SubValues def: \
parent with"
		]
	];
	Module[{val1, val2},
		Test[
			CheckOption[f, SymbolName[name]][
				{SymbolName[subName] :> val1, SymbolName[subName] -> val2}
			]
			,
			result
			,
			Message[f::opt, val1]
			,
			TestID -> "Call: SubValues: RuleDelayed: invalid: SubValues def: \
parent with duplicated string, string"
		]
	];
	Module[{val1, val2},
		Test[
			CheckOption[f, SymbolName[name]][
				{subName -> val1, SymbolName[subName] :> val2}
			]
			,
			result
			,
			Message[f::opt, val1]
			,
			TestID -> "Call: SubValues: RuleDelayed: invalid: SubValues def: \
parent with duplicated symbol, string"
		]
	];
	Test[
		CheckOption[f, name][{}]
		,
		True
		,
		TestID -> "Call: SubValues: RuleDelayed: invalid: SubValues def: \
parent without"
	]
]
Module[{f, name, subName, result},
	CheckOption[f, SymbolName[name] :> Evaluate@SymbolName[subName]] =
		(Message[f::opt, HoldForm[#]]; result)&;
	
	Module[{val},
		Test[
			CheckOption[f, name :> subName][val]
			,
			result
			,
			Message[f::opt, val]
			,
			TestID -> "Call: SubValues: RuleDelayed: invalid: DownValues def"
		]
	];
	Module[{subName2, val, val2},
		Test[
			CheckOption[f, name][
				{subName2 -> val2, SymbolName[subName] -> val}
			]
			,
			result
			,
			Message[f::opt, val]
			,
			TestID -> "Call: SubValues: RuleDelayed: invalid: DownValues def: \
parent with"
		]
	];
	Module[{val},
		Test[
			CheckOption[f, SymbolName[name]][{val, {}}]
			,
			True
			,
			TestID -> "Call: SubValues: RuleDelayed: invalid: DownValues def: \
parent without"
		]
	]
]


Module[{f, name, subName, subSubName, val},
	With[{nameStr = SymbolName[name], subNameStr = SymbolName[subName]},
		Test[
			CheckOption[f, name :> subNameStr -> subSubName][val]
			,
			False
			,
			Message[CheckOption::optnf, nameStr, f]
			,
			TestID -> "Call: SubValues: nested rules: unknown"
		]
	]
]
Module[{f, name, subName, subSubName, val, default},
	Options[f] = {SymbolName[name] -> default};
	With[{subNameStr = SymbolName[subName]},
		Test[
			CheckOption[f, name :> subNameStr -> subSubName][val]
			,
			True
			,
			TestID -> "Call: SubValues: nested rules: valid"
		]
	]
]
Module[{f, name, subName, x, result},
	Quiet[
		CheckOption[f, name -> SymbolName[subName] :> x * x][value_] :=
			(Message[f::opt, HoldForm[value]]; result)
		,
		CheckOption::unknownOptionNamePattern
	];
	
	Module[{val},
		Test[
			CheckOption[f, SymbolName[name] :> subName -> x * x][val]
			,
			result
			,
			Message[f::opt, val]
			,
			TestID -> "Call: SubValues: nested rules: invalid: SubValues def"
		]
	];
	
	Module[{val},
		Test[
			CheckOption[f, name -> subName][Unevaluated[{x * x -> val}]]
			,
			result
			,
			Message[f::opt, val]
			,
			TestID -> "Call: SubValues: nested rules: invalid: SubValues def: \
parent with"
		]
	];
	Module[{val},
		Test[
			CheckOption[f, SymbolName[name] :> Evaluate@SymbolName[subName]][
				{{val}, 10 -> 11}
			]
			,
			True
			,
			TestID -> "Call: SubValues: nested rules: invalid: SubValues def: \
parent without"
		]
	];
	
	Module[{val},
		Test[
			CheckOption[f, SymbolName[name]][
				{subName :> {{x * x :> val}, "str"}, val}
			]
			,
			result
			,
			Message[f::opt, val]
			,
			TestID -> "Call: SubValues: nested rules: invalid: SubValues def: \
parent of parent with sub and subsub"
		]
	];
	Module[{val},
		Test[
			CheckOption[f, name][{subName -> {x^2 -> val}}]
			,
			True
			,
			TestID -> "Call: SubValues: nested rules: invalid: SubValues def: \
parent of parent with sub without subsub"
		]
	];
	Module[{val},
		Test[
			CheckOption[f, SymbolName[name]][Unevaluated[{x * x -> val}]]
			,
			True
			,
			TestID -> "Call: SubValues: nested rules: invalid: SubValues def: \
parent of parent without sub with subsub directly"
		]
	];
	Module[{val},
		Test[
			CheckOption[f, name][{SymbolName[subName2] :> {x * x -> val}}]
			,
			True
			,
			TestID -> "Call: SubValues: nested rules: invalid: SubValues def: \
parent of parent without sub with subsub in other"
		]
	];
	Module[{a, b, c},
		Test[
			CheckOption[f, name][{a -> {b -> c}}]
			,
			True
			,
			TestID -> "Call: SubValues: nested rules: invalid: SubValues def: \
parent of parent without sub withot subsub"
		]
	]
]
Module[{f, name, subName, subSubSubName, x, result},
	With[
		{
			nameStr = SymbolName[name],
			subNameStr = SymbolName[subName],
			subSubSubNameStr = SymbolName[subSubSubName]
		}
		,
		Quiet[
			CheckOption[f, nameStr -> subName :> x + x -> subSubSubNameStr] =
				(Message[f::opt, HoldForm[#]]; result)&;
			,
			CheckOption::unknownOptionNamePattern
		];
	
		Module[{val},
			Test[
				CheckOption[f, name :> subNameStr -> x + x :> subSubSubName][
					val
				]
				,
				result
				,
				Message[f::opt, val]
				,
				TestID -> "Call: SubValues: nested rules: invalid: \
DownValues def"
			]
		];
		
		Module[{val},
			Test[
				CheckOption[f, name][
					Unevaluated[subName -> x + x -> subSubSubName -> val]
				]
				,
				result
				,
				Message[f::opt, val]
				,
				TestID -> "Call: SubValues: nested rules: invalid: \
DownValues def: parent of parent of parent with"
			]
		];
		Module[{val, subSubSubName2},
			Test[
				CheckOption[f, nameStr -> subNameStr :> x + x][
					{SymbolName[subSubSubName2] -> val}
				]
				,
				True
				,
				TestID -> "Call: SubValues: nested rules: invalid: \
DownValues def: parent without"
			]
		]
	]
]
Module[{f, name, subSubName1, subSubName2, result1, result2},
	With[
		{
			nameStr = SymbolName[name],
			subSubName1Str = SymbolName[subSubName1],
			subSubName2Str = SymbolName[subSubName2]
		}
		,
		Quiet[
			CheckOption[f, nameStr :> 2 + 2 -> subSubName1][value_] :=
				(Message[f::opt1, HoldForm[value]]; result1);
			CheckOption[f, name :> 2 + 2 -> subSubName2Str] =
				(Message[f::opt2, HoldForm[#]]; result2)&;
			,
			CheckOption::unknownOptionNamePattern
		];
		
		Module[{val},
			Test[
				CheckOption[f, name :> 2 + 2 :> subSubName1Str][val]
				,
				result1
				,
				Message[f::opt1, val]
				,
				TestID -> "Call: SubValues: nested rules: invalid: \
SubValues+DownValues def: first"
			]
		];
		Module[{val},
			Test[
				CheckOption[f, nameStr :> 2 + 2 -> subSubName2][val]
				,
				result2
				,
				Message[f::opt2, val]
				,
				TestID -> "Call: SubValues: nested rules: invalid: \
SubValues+DownValues def: second"
			]
		];
		
		Module[{val1, val2, val3, x, y},
			Test[
				CheckOption[f, name :> 2 + 2][{
					subSubName2Str :> val1, x,
					{{y}, subSubName1 -> val2},
					subSubName2 -> val3
				}]
				,
				result2 && result1
				,
				{Message[f::opt2, val1], Message[f::opt1, val2]}
				,
				TestID -> "Call: SubValues: nested rules: invalid: \
SubValues+DownValues def: parent with both (one duplicated)"
			]
		];
		Module[{val1, val2, val3, x, y},
			Test[
				CheckOption[f, name]@Unevaluated@{
					4 -> subSubName1Str -> val1,
					x,
					{2 + 2 -> subSubName1Str :> val2},
					2 + 2 -> {subSubName1Str :> val3, y}
				}
				,
				result1
				,
				Message[f::opt1, val2]
				,
				TestID -> "Call: SubValues: nested rules: invalid: \
SubValues+DownValues def: parent of parent with parent (duplicated) \
with first (3 duplicates in different parents)"
			]
		];
		Module[{val1, val2, subsubName3},
			Test[
				CheckOption[f, name]@Unevaluated@{
					2 + 2 -> {subsubName3 -> val1},
					2 + 2 -> {subSubName1Str -> val2}
				}
				,
				True
				,
				TestID -> "Call: SubValues: nested rules: invalid: \
SubValues+DownValues def: parent of parent with parent \
(duplicated: without, with)"
			]
		];
	]
]


Module[{f, a, b, val},
	Test[
		CheckOption[f, a + b][val]
		,
		False
		,
		Message[CheckOption::optnf, a + b, f]
		,
		TestID -> "Call: SubValues: non-standard: undefined"
	]
]
Module[{f, g, x, default, val},
	Options[f] = {g[x] :> default};
	Test[
		CheckOption[f, g[x]][val]
		,
		True
		,
		TestID -> "Call: SubValues: non-standard: valid"
	]
]
Module[{f, result},
	Quiet[
		CheckOption[f, Hold[1 + 1]][value_] :=
			(Message[f::heldOnePlusOne, HoldForm[value]]; result)
		,
		CheckOption::unknownOptionNamePattern
	];
	
	Module[{val},
		Test[
			CheckOption[f, Hold[1 + 1]][val]
			,
			result
			,
			Message[f::heldOnePlusOne, val]
			,
			TestID -> "Call: SubValues: non-standard: invalid: SubValues def"
		]
	]
]
Module[{f, x, result},
	Quiet[
		CheckOption[f, 2 x] = (Message[f::opt, HoldForm[#]]; result)&,
		CheckOption::unknownOptionNamePattern
	];
	
	Module[{val},
		Test[
			CheckOption[f, 2 x][val]
			,
			result
			,
			Message[f::opt, val]
			,
			TestID -> "Call: SubValues: non-standard: invalid: DownValues def"
		]
	]
]


Module[{f, counter = 0},
	Test[
		CheckOption[f, "unknownOpt"]@Unevaluated[++counter]
		,
		False
		,
		Message[CheckOption::optnf, "unknownOpt", f]
		,
		TestID -> "Call: SubValues: Evaluation count: undefined"
	];
	Test[
		counter
		,
		0
		,
		TestID -> "Call: SubValues: Evaluation count: undefined: counter"
	]
]
Module[{f, testOpt, defaultCounter = 0, counter = 0},
	Options[f] = {testOpt :> ++defaultCounter};
	Test[
		CheckOption[f, testOpt]@Unevaluated[++counter]
		,
		True
		,
		TestID -> "Call: SubValues: Evaluation count: valid"
	];
	Test[
		defaultCounter
		,
		0
		,
		TestID -> "Call: SubValues: Evaluation count: valid: default counter"
	];
	Test[
		counter
		,
		0
		,
		TestID -> "Call: SubValues: Evaluation count: valid: counter"
	]
]
Module[{f, opt, result},
	CheckOption[f, opt][value_] :=
		(Message[f::opt, HoldForm[value]]; result);
	
	Module[{counter = 0},
		Test[
			CheckOption[f, SymbolName[opt]]@Unevaluated[++counter]
			,
			result
			,
			Message[f::opt, ++counter]
			,
			TestID -> "Call: SubValues: Evaluation count: invalid: \
SubValues def"
		];
		Test[
			counter
			,
			0
			,
			TestID -> "Call: SubValues: Evaluation count: invalid: \
SubValues def: counter"
		]
	]
]
Module[{f, opt, result},
	CheckOption[f, opt] =
		Function[Null, Message[f::opt, HoldForm[#]]; result, HoldFirst];
	
	Module[{counter = 0},
		Test[
			CheckOption[f, SymbolName[opt]][++counter]
			,
			result
			,
			Message[f::opt, ++counter]
			,
			TestID -> "Call: SubValues: Evaluation count: invalid: \
DownValues def"
		];
		Test[
			counter
			,
			0
			,
			TestID -> "Call: SubValues: Evaluation count: invalid: \
DownValues def: counter"
		]
	]
]


Module[{f, opt},
	With[{optName = SymbolName[opt]},
		Test[
			CheckOption[f, opt][] // Hold[#]&
			,
			CheckOption[f, optName][] // Hold
			,
			Message[CheckOption::subargx, CheckOption[f, optName][], 0]
			,
			TestID -> "Call: SubValues: incorrect args: \
symbol, symbol; no sub-args"
		]
	]
]
Module[{f, g, opt, val},
	Test[
		CheckOption[{f, g}, opt][val] // Hold[#]&
		,
		CheckOption[{f, g}, opt][val] // Hold
		,
		Message[CheckOption::sym, {f, g}, 1]
		,
		TestID -> "Call: SubValues: incorrect args: \
list of symbols, symbol; symbol"
	]
]
Module[{f, opt, a, b},
	With[{optName = SymbolName[opt]},
		Test[
			CheckOption[f, opt][a, b] // Hold[#]&
			,
			CheckOption[f, optName][a, b] // Hold
			,
			Message[CheckOption::subargx, CheckOption[f, optName][a, b], 2]
			,
			TestID -> "Call: SubValues: incorrect args: \
symbol, symbol; 2 sub-args"
		]
	]
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
