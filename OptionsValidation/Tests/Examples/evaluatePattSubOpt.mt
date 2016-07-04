(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["OptionsValidation`Tests`Examples`evaluatePattSubOpt`",
	{"MUnit`"}
]


Get["OptionsValidation`"]


Options[f] = {opt -> {subOpt1 -> True, subOpt2 -> "strOpt"}};

CheckOption[f, opt -> subOpt1][val : Except[True | False]] :=
	Message[f::opttf, HoldForm[opt -> subOpt1], HoldForm[val]]

SetDefaultOptionsValidation[f]

f[_Integer | _String, InvalidOptionsPattern[f]] := $Failed
f[i_Integer, OptionsPattern[]] :=
	{Integer, i, OptionValue[{opt -> subOpt1, opt -> subOpt2}]}
f[s_String, OptionsPattern[]] :=
	{String, s, OptionValue[{opt -> subOpt1, opt -> subOpt2}]}


(* ::Section:: *)
(*Tests*)


Test[
	f[5, opt -> `arbitraryContext`subOpt1 -> True]
	,
	{Integer, 5, {True, "strOpt"}}
	,
	TestID -> "Integer, valid option"
]
Test[
	f["str", "opt" -> {subOpt1 -> False, subOpt2 -> x}]
	,
	{String, "str", {False, x}}
	,
	TestID -> "String, valid option"
]

Test[
	f[-8, opt -> {subOpt2 -> 10, subOpt1 -> "strOpt"}]
	,
	$Failed
	,
	Message[f::opttf, opt -> subOpt1, "strOpt"]
	,
	TestID -> "Integer, invalid option"
]
Test[
	f["str", `arbitraryContext`opt -> "subOpt1" -> 10]
	,
	$Failed
	,
	Message[f::opttf, opt -> subOpt1, 10]
	,
	TestID -> "String, invalid option"
]

Test[
	f[2, unknown1 -> val1, unknown2 -> val2]
	,
	$Failed
	,
	{
		Message[CheckOption::optnf, "unknown1", f],
		Message[CheckOption::optnf, "unknown2", f]
	}
	,
	TestID -> "Integer, unknown option"
]
Test[
	f["something", "unknownOptionName" -> "value"]
	,
	$Failed
	,
	Message[CheckOption::optnf, "unknownOptionName", f]
	,
	TestID -> "String, unknown option"
]


Test[
	SetOptions[f, opt -> subOpt1 -> "invalid"]
	,
	$Failed
	,
	Message[f::opttf, opt -> subOpt1, "invalid"]
	,
	TestID -> "Set invalid default"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
