(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["OptionsValidation`Tests`CheckOptionRelations`", {"MUnit`"}]


Get["OptionsValidation`"]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*Definitions*)


Module[{f, testFunc, oldDownValues = DownValues[CheckOptionRelations]},
	Test[
		CheckOptionRelations[f] = testFunc
		,
		testFunc
		,
		TestID -> "Set: 1 symbol: define"
	];
	
	Test[
		UpValues[f]
		,
		{HoldPattern@CheckOptionRelations[f] :> testFunc}
		,
		TestID -> "Set: 1 symbol: symbol UpValues"
	];
	Test[
		DownValues[CheckOptionRelations]
		,
		oldDownValues
		,
		TestID -> "Set: 1 symbol: CheckOptionRelations DownValues"
	]
]


Module[{f, g, h, testFunc, oldDownValues = DownValues[CheckOptionRelations]},
	Test[
		CheckOptionRelations[func : f | g | h] := testFunc[func, #]&
		,
		Null
		,
		TestID -> "SetDelayed: 3 symbols: define"
	];
	
	Test[
		UpValues[f]
		,
		{HoldPattern@CheckOptionRelations[func : f] :> (testFunc[func, #]&)}
		,
		TestID -> "SetDelayed: 3 symbols: first symbol UpValues"
	];
	Test[
		UpValues[g]
		,
		{HoldPattern@CheckOptionRelations[func : g] :> (testFunc[func, #]&)}
		,
		TestID -> "SetDelayed: 3 symbols: second symbol UpValues"
	];
	Test[
		UpValues[h]
		,
		{HoldPattern@CheckOptionRelations[func : h] :> (testFunc[func, #]&)}
		,
		TestID -> "SetDelayed: 3 symbols: third symbol UpValues"
	];
	
	Test[
		DownValues[CheckOptionRelations]
		,
		oldDownValues
		,
		TestID -> "SetDelayed: 3 symbols: CheckOptionRelations DownValues"
	]
]


Module[{f, g, testFunc, oldDownValues = DownValues[CheckOptionRelations]},
	CheckOptionRelations[f] = testFunc;
	
	Test[
		CheckOptionRelations[f | g] =.
		,
		{Null, $Failed}
		,
		Message[TagUnset::norep, CheckOptionRelations[g], g]
		,
		TestID -> "Unset: 2 of 1 symbols: define"
	];
	
	Test[
		UpValues[f]
		,
		{}
		,
		TestID -> "Unset: 2 of 1 symbols: first symbol UpValues"
	];
	Test[
		UpValues[g]
		,
		{}
		,
		TestID -> "Unset: 2 of 1 symbols: second symbol UpValues"
	];
	
	Test[
		DownValues[CheckOptionRelations]
		,
		oldDownValues
		,
		TestID -> "Unset: 2 of 1 symbols: CheckOptionRelations DownValues"
	]
]


Module[{oldDownValues = DownValues[CheckOptionRelations]},
	Test[
		CheckOptionRelations[name_] =.
		,
		$Failed
		,
		Message[CheckOptionRelations::unknownOptionOwnerPattern,
			CheckOptionRelations, Verbatim[name_], TagUnset
		]
		,
		TestID -> "Unset: wrong function pattern: define"
	];
	
	Test[
		DownValues[CheckOptionRelations]
		,
		oldDownValues
		,
		TestID ->
			"Unset: wrong function pattern: CheckOptionRelations DownValues"
	]
]


Module[
	{f, g, testFunc, getArg, oldDownValues = DownValues[CheckOptionRelations]},
	getArg = f | g &;
	
	Test[
		CheckOptionRelations[getArg[]] = testFunc
		,
		testFunc
		,
		TestID -> "Set: argument evaluates to valid function pattern: \
define"
	];
	
	Test[
		UpValues[f]
		,
		{HoldPattern@CheckOptionRelations[f] :> testFunc}
		,
		TestID -> "Set: argument evaluates to valid function pattern: \
first symbol UpValues"
	];
	Test[
		UpValues[g]
		,
		{HoldPattern@CheckOptionRelations[g] :> testFunc}
		,
		TestID -> "Set: argument evaluates to valid function pattern: \
second symbol UpValues"
	];
	Test[
		DownValues[CheckOptionRelations]
		,
		oldDownValues
		,
		TestID -> "Set: argument evaluates to valid function pattern: \
CheckOptionRelations DownValues"
	]
]


Module[{testFunc, oldDownValues = DownValues[CheckOptionRelations]},
	Internal`InheritedBlock[{CheckOptionRelations},
		Unprotect[CheckOptionRelations];
		Test[
			CheckOptionRelations[_] := testFunc
			,
			Null
			,
			Message[CheckOptionRelations::unprotected, CheckOptionRelations,
				CheckOptionRelations[Verbatim[_]] := testFunc
			]
			,
			TestID -> "SetDelayed: unprotected CheckOptionRelations: define"
		];
		
		Test[
			Complement[
				Join[DownValues[CheckOptionRelations], oldDownValues], 
 				Intersection[DownValues[CheckOptionRelations], oldDownValues]
 			]
			,
			{HoldPattern@CheckOptionRelations[_] :> testFunc}
			,
			TestID -> "SetDelayed: unprotected CheckOptionRelations: \
CheckOptionRelations new DownValues"
		]
	]
]


(* ::Subsection:: *)
(*Call*)


(* ::Subsubsection:: *)
(*DownValues*)


Test[
	CheckOptionRelations[] // Hold[#]&
	,
	CheckOptionRelations[] // Hold
	,
	Message[CheckOptionRelations::argx, CheckOptionRelations, 0]
	,
	TestID -> "Call: DownValues: no args"
]


Module[{f},
	Test[
		CheckOptionRelations[f] // Hold[#]&
		,
		CheckOptionRelations[f] // Hold
		,
		TestID -> "Call: DownValues: symbol"
	]
]
Module[{f, g},
	Test[
		CheckOptionRelations[{f, g}] // Hold[#]&
		,
		CheckOptionRelations[{f, g}] // Hold
		,
		Message[CheckOptionRelations::sym, {f, g}, 1]
		,
		TestID -> "Call: DownValues: list of symbols"
	]
]


Module[{a, b},
	Test[
		CheckOptionRelations[a, b] // Hold[#]&
		,
		CheckOptionRelations[a, b] // Hold
		,
		Message[CheckOptionRelations::argx, CheckOptionRelations, 2]
		,
		TestID -> "Call: DownValues: 2 symbols"
	]
]


(* ::Subsubsection:: *)
(*SubValues*)


Module[{f},
	Test[
		CheckOptionRelations[f][] // Hold[#]&
		,
		CheckOptionRelations[f][] // Hold
		,
		Message[CheckOptionRelations::subargx, CheckOptionRelations[f][], 0]
		,
		TestID -> "Call: SubValues: symbol; no sub-args"
	]
]
Module[{f, opt1, opt2, val1, val2},
	Test[
		CheckOptionRelations[f][{opt1 -> val1, opt2 :> val2}]
		,
		True
		,
		TestID -> "Call: SubValues: symbol; option list"
	]
]
Module[{f, a, b},
	Test[
		CheckOptionRelations[f][a, b] // Hold[#]&
		,
		CheckOptionRelations[f][a, b] // Hold
		,
		Message[CheckOptionRelations::subargx,
			CheckOptionRelations[f][a, b], 2
		]
		,
		TestID -> "Call: SubValues: symbol; no sub-args"
	]
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
