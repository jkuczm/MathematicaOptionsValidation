(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["OptionsValidation`Tests`Examples`evaluateBodyRuntime`",
	{"MUnit`"}
]


Get["OptionsValidation`"]


Options[f] = {opt -> 100};

CheckOption[f, opt][val : Except[_Integer?NonNegative | Infinity]] :=
	Message[f::iopnf, HoldForm@opt, HoldForm@val]

SetDefaultOptionsValidation[f]

f[x_, OptionsPattern[]] := WithOptionValueChecks@{x, OptionValue[opt]}


(* ::Section:: *)
(*Tests*)


Test[
	f["str", opt -> 11]
	,
	{"str", 11}
	,
	TestID -> "valid option"
]

Test[
	f[sym, "opt" -> -2]
	,
	$Failed
	,
	Message[f::iopnf, opt, -2]
	,
	TestID -> "invalid option"
]


Test[
	SetOptions[f, opt -> "nonInteger"]
	,
	$Failed
	,
	Message[f::iopnf, opt, "nonInteger"]
	,
	TestID -> "Set invalid default"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
