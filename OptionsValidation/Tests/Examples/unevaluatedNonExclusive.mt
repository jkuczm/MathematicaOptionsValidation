(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["OptionsValidation`Tests`Examples`unevaluatedNonExclusive`",
	{"MUnit`"}
]


Get["OptionsValidation`"]


Options[f] = {opt1 -> "default", opt2 -> 0};

CheckOption[f, opt1][val : Except[_String | {__String}]] :=
	Message[f::opstl, HoldForm@opt1, HoldForm@val]

CheckOption[f, opt2] =
	IntegerQ[#] || Message[f::modn, HoldForm@opt2, HoldForm@#] &

f::optRel = "\
Value of option opt2 -> `2` shouldn't be larger than total length of strings \
given in option opt1 -> `1`."

CheckOptionRelations[f] =
	Module[{val1, val2},
		{val1, val2} = OptionValue[f, #, {opt1, opt2}];
		val2 <= Quiet@StringLength@StringJoin[val1] ||
			Message[f::optRel, HoldForm@Evaluate@val1, HoldForm@Evaluate@val2]
	]&

SetDefaultOptionsValidation[f]

f[i_Integer, QuietValidOptionsPattern[f]] :=
	{Integer, i, OptionValue[{opt1, opt2}]}
f[x_, QuietValidOptionsPattern[f]] :=
	{"general", x, OptionValue[{opt1, opt2}]}
f[_, ValidOptionsPattern[f]] := "never reached"


(* ::Section:: *)
(*Tests*)


Test[
	f[5, opt1 -> "optStr", opt2 -> 3]
	,
	{Integer, 5, {"optStr", 3}}
	,
	TestID -> "Integer, valid options"
]
Test[
	f["str", `arbitraryContext`opt1 -> {"optStr1", "optStr2"}]
	,
	{"general", "str", {{"optStr1", "optStr2"}, 0}}
	,
	TestID -> "String, valid option"
]

Test[
	f[5, opt1 -> "str", "opt2" -> 1.1] // Hold[#]&
	,
	f[5, opt1 -> "str", "opt2" -> 1.1] // Hold[#]&
	,
	Message[f::modn, opt2, 1.1]
	,
	TestID -> "Integer, invalid second option"
]
Test[
	f[x, opt2 -> 10] // Hold[#]&
	,
	f[x, opt2 -> 10] // Hold
	,
	Message[f::optRel, "default", 10]
	,
	TestID -> "Symbol, invalid option relations"
]

Test[
	f[2, unknown -> val] // Hold[#]&
	,
	f[2, unknown -> val] // Hold
	,
	{
		Message[CheckOption::optnf, "unknown", f],
		Message[OptionValue::nodef, "unknown", f]
	}
	,
	TestID -> "Integer, unknown option"
]
Test[
	f["something", "unknownOptionName" -> "value"] // Hold[#]&
	,
	f["something", "unknownOptionName" -> "value"] // Hold
	,
	{
		Message[CheckOption::optnf, "unknownOptionName", f],
		Message[OptionValue::nodef, "unknownOptionName", f]
	}
	,
	TestID -> "String, unknown option"
]


Test[
	SetOptions[f, opt1 -> nonString]
	,
	$Failed
	,
	{Message[f::opstl, opt1, nonString], Message[f::optRel, nonString, 0]}
	,
	TestID -> "Set invalid default"
]


(* ::Section:: *)
(*TearDown*)


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
