(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["OptionsValidation`Tests`Examples`unevaluatedExclusiveHeld`",
	{"MUnit`"}
]


Get["OptionsValidation`"]


Options[f] = {opt :> 2 + 2};

f::optPlus =
	"Value of option `1` -> `2` should be an expression with head Plus."

CheckOption[f, opt] =
	Function[val,
		Head@Unevaluated[val] === Plus ||
			Message[f::optPlus, HoldForm@opt, HoldForm@val],
		HoldFirst
	]

SetDefaultOptionsValidation[f]

f[i_Integer, ValidOptionsPattern[f]] :=
	{Integer, i, OptionValue[Automatic, Automatic, opt, Hold]}
f[s_String, ValidOptionsPattern[f]] :=
	{String, s, OptionValue[Automatic, Automatic, opt, Hold]}


(* ::Section:: *)
(*Tests*)


Test[
	f[5, opt :> 1 + 10]
	,
	{Integer, 5, Hold[1 + 10]}
	,
	TestID -> "Integer, valid delayed option"
]
Test[
	f["str", `arbitraryContext`opt -> a + b]
	,
	{String, "str", Hold[a + b]}
	,
	TestID -> "String, valid option"
]

Test[
	f[5, "opt" -> 10] // Hold[#]&
	,
	f[5, "opt" -> 10] // Hold
	,
	Message[f::optPlus, opt, 10]
	,
	TestID -> "Integer, invalid option"
]
Test[
	f["str", opt :> 2 * 3] // Hold[#]&
	,
	f["str", opt :> 2 * 3] // Hold
	,
	Message[f::optPlus, opt, 2 * 3]
	,
	TestID -> "String, invalid delayed option"
]

Test[
	f[2, unknown -> val] // Hold[#]&
	,
	f[2, unknown -> val] // Hold
	,
	Message[CheckOption::optnf, "unknown", f]
	,
	TestID -> "Integer, unknown option"
]
Test[
	f["something", "unknownOptionName" -> "value"] // Hold[#]&
	,
	f["something", "unknownOptionName" -> "value"] // Hold
	,
	Message[CheckOption::optnf, "unknownOptionName", f]
	,
	TestID -> "String, unknown option"
]


Test[
	SetOptions[f, opt -> "nonPlus"]
	,
	$Failed
	,
	Message[f::optPlus, opt, "nonPlus"]
	,
	TestID -> "Set invalid default"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
