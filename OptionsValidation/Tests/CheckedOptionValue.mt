(* Mathematica Test File *)

(* ::Section:: *)
(*SetUp*)


BeginPackage["OptionsValidation`Tests`CheckedOptionValue`", {"MUnit`"}]


Get["OptionsValidation`"]


Options[testFunc] = Options[testFuncB] =
Options[testFuncPassCheck] = Options[testFuncFailCheck] =
	{"opt1" -> defaultValid1, opt2 :> defaultValid2}


CheckOption[f : testFunc | testFuncB,
	name : opt1, val : Except[defaultValid1 | valid1]
] :=
	Message[f::opt1, HoldForm[name], HoldForm[val]]

CheckOption[f : testFunc | testFuncB, name : "opt2"] =
	# === defaultValid2 || # === valid2 ||
		Message[f::opt2, HoldForm[name], HoldForm[#]] &


CheckOption[testFuncFailCheck, opt1] =
	Message[testFuncFailCheck::opt1, HoldForm[opt1], HoldForm[#]] &

CheckOption[testFuncFailCheck, opt2, val_] :=
	Message[testFuncFailCheck::opt2, HoldForm[opt2], HoldForm[val]]


Options[testFuncHeldCheck] = Options[testFuncHeldCheckB] =
	{opt1 :> defaultInvalidHeld1, "opt2" :> defaultInvalidHeld1}

CheckOption[f : testFuncHeldCheck | testFuncHeldCheckB, name : "opt1" | opt2] =
	Function[val,
		Length@Unevaluated[val] === 1 ||
			Message[MessageName[f, name], HoldForm[name], HoldForm[val]],
		HoldFirst
	]


Options[testFuncIntCheck] = Options[testFuncIntCheckB] =
	{opt1 :> defaultInvalidInt1, "opt2" :> defaultInvalidInt1}

CheckOption[f : testFuncIntCheck | testFuncIntCheckB, name : opt1 | opt2] =
	IntegerQ[#] ||
		Message[MessageName[f, name], HoldForm[name], HoldForm[#]] &


Options[testFuncSubOpt] = {
	opt :> {
		subOptPart + subOptPart -> subSubOpt -> defaultSubSubSubValid,
		subOpt -> defaultSubSubValid
	}
}

Quiet[
	CheckOption[testFuncSubOpt,
		name: (opt :> subOptPart + subOptPart -> subSubOpt),
		val: Except[defaultSubSubSubValid | subSubSubValid]
	] :=
		Message[testFuncSubOpt::subSubOpt, HoldForm[name], HoldForm[val]]
	,
	CheckOption::unknownOptionNamePattern
]

CheckOption[testFuncSubOpt, name: (opt -> subOpt)] :=
	MatchQ[#, defaultSubSubValid | subSubValid] ||
		Message[testFuncSubOpt::subOpt, HoldForm[name], HoldForm[#]] &


Options[testFuncSubOptB] =
	{opt :> subOptPart + subOptPart -> defaultSubSubValidB}

Quiet[
	CheckOption[testFuncSubOptB, opt :> subOptPart + subOptPart] =
		# === defaultSubSubValidB || # === subSubValidB ||
			Message[testFuncSubOptB::subOpt,
				HoldForm[opt :> subOptPart + subOptPart],
				HoldForm[#]
			] &
	,
	CheckOption::unknownOptionNamePattern
]

(*	Checks defined for testFuncSubOpt and testFuncSubOptB opt option should
	hold their option value argument, so that potentially present name of
	subOptPart + subOptPart sub0option remains unevaluated. *)
CheckOption[f : testFuncSubOpt | testFuncSubOptB, opt] =
	Function[val,
		CheckOption[f, "opt", Unevaluated[val]],
		HoldFirst
	]


customCheckOption =
	Function[{f, name},
		Function[val,
			Message[customCheckOption::used,
				HoldForm@f, HoldForm@name, HoldForm@val
			];
			MatchQ[val, _valid]
		]
	]


With[
	{
		testFunctions = {
			testFunc, testFuncPassCheck, testFuncFailCheck, testFuncHeldCheck,
			testFuncHeldCheckB, testFuncIntCheck, testFuncIntCheckB
		}
	},
	withUnusedDefaults =
		Function[expr,
			Internal`InheritedBlock[testFunctions,
				SetOptions[testFunctions,
					opt1 -> unusedDefaultInvalid1,
					opt2 -> unusedDefaultInvalid2
				];
				expr
			],
			HoldFirst
		]
]


catchAll =
	Function[expr,
		Catch[
			expr;
			"Expected Throw didn't occur."
			,
			_
			,
			HoldComplete
		],
		HoldAllComplete
	]


Off[General::stop]


(* ::Section:: *)
(*Tests*)


(* ::Subsection:: *)
(*0 args*)


Test[
	CheckedOptionValue[] // Hold[#]&
	,
	CheckedOptionValue[] // Hold
	,
	Message[CheckedOptionValue::argt, CheckedOptionValue, 0, 1, 2]
	,
	TestID -> "0 args"
]


(* ::Subsection:: *)
(*1 arg*)


(* ::Subsubsection:: *)
(*OptionValue 0 args*)


Test[
	CheckedOptionValue@OptionValue[] // Hold[#]&
	,
	OptionValue[] // Hold
	,
	{
		Message[CheckedOptionValue::unknownOptionValueUsage, OptionValue[]],
		Message[OptionValue::argb, OptionValue, 0, 1, 4]
	}
	,
	TestID -> "1 arg: no args"
]


(* ::Subsubsection:: *)
(*OptionValue 1 arg: option name specification*)


Test[
	CheckedOptionValue@OptionValue["opt1"] // Hold[#]&
	,
	CheckedOptionValue@OptionValue["opt1"] // Hold
	,
	TestID -> "1 arg: option name"
]
Test[
	CheckedOptionValue@OptionValue[{opt1, opt2}] // Hold[#]&
	,
	CheckedOptionValue@OptionValue[{opt1, opt2}] // Hold
	,
	TestID -> "1 arg: option names list"
]


(* ::Subsubsection:: *)
(*OptionValue 2 args: function, option name specification*)


Test[
	CheckedOptionValue@OptionValue[testFunc, opt1]
	,
	defaultValid1
	,
	TestID -> "1 arg: function, option name: valid"
]
Internal`InheritedBlock[{testFunc},
	SetOptions[testFunc, opt1 :> defaultInvalid1];
	Test[
		CheckedOptionValue@OptionValue[testFunc, opt1] // catchAll
		,
		HoldComplete[
			$Failed,
			InvalidOptionValue[testFunc, opt1, defaultInvalid1]
		]
		,
		Message[testFunc::opt1, "opt1", defaultInvalid1]
		,
		TestID -> "1 arg: function, option name: invalid"
	]
]
Test[
	CheckedOptionValue@OptionValue[testFunc, {opt1, "opt2"}]
	,
	{defaultValid1, defaultValid2}
	,
	TestID -> "1 arg: function, option names list: valid, valid"
]
Internal`InheritedBlock[{testFunc},
	SetOptions[testFunc, "opt2" -> defaultInvalid2];
	Test[
		CheckedOptionValue@OptionValue[testFunc, {"opt1", "opt2"}] // catchAll
		,
		HoldComplete[
			$Failed,
			InvalidOptionValue[testFunc, "opt2", defaultInvalid2]
		]
		,
		Message[testFunc::opt2, "opt2", defaultInvalid2]
		,
		TestID -> "1 arg: function, option names list: valid, invalid"
	]
]
Internal`InheritedBlock[{testFunc},
	SetOptions[testFunc, opt1 :> defaultInvalid1];
	Test[
		CheckedOptionValue@OptionValue[testFunc, {opt1, opt2}] // catchAll
		,
		HoldComplete[
			$Failed,
			InvalidOptionValue[testFunc, opt1, defaultInvalid1]
		]
		,
		Message[testFunc::opt1, "opt1", defaultInvalid1]
		,
		TestID -> "1 arg: function, option names list: invalid, valid"
	]
]
Internal`InheritedBlock[{testFunc},
	SetOptions[testFunc,
		"opt1" -> defaultInvalid1, opt2 -> defaultInvalid2
	];
	Test[
		CheckedOptionValue@OptionValue[testFunc, {"opt1", opt2}] // catchAll
		,
		HoldComplete[
			$Failed,
			InvalidOptionValue[testFunc, "opt1", defaultInvalid1]
		]
		,
		Message[testFunc::opt1, "opt1", defaultInvalid1]
		,
		TestID -> "1 arg: function, option names list: invalid, invalid"
	]
]


(* ::Subsubsection:: *)
(*OptionValue 2 args: default options, option name specification*)


withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[{opt1 -> defaultValid1}, opt1]
	,
	defaultValid1
	,
	Message[CheckedOptionValue::unknownOptionOwner, {opt1 -> defaultValid1}]
	,
	TestID -> "1 arg: default options, option name: valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[{"opt1" :> defaultInvalid1}, opt1]
	,
	defaultInvalid1
	,
	Message[CheckedOptionValue::unknownOptionOwner,
		{"opt1" :> defaultInvalid1}
	]
	,
	TestID -> "1 arg: default options, option name: invalid"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{opt1 -> defaultValid1, opt2 -> defaultValid2}, {opt1, "opt2"}
	]
	,
	{defaultValid1, defaultValid2}
	,
	Message[CheckedOptionValue::unknownOptionOwner,
		{opt1 -> defaultValid1, opt2 -> defaultValid2}
	]
	,
	TestID -> "1 arg: default options, option names list: valid, valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{"opt1" :> defaultValid1, opt2 :> defaultInvalid2}, {"opt1", "opt2"}
	]
	,
	{defaultValid1, defaultInvalid2}
	,
	Message[CheckedOptionValue::unknownOptionOwner,
		{"opt1" :> defaultValid1, opt2 :> defaultInvalid2}
	]
	,
	TestID -> "1 arg: default options, option names list: valid, invalid"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{{"opt1" -> defaultInvalid1}, "opt2" -> defaultValid2}, {opt1, opt2}
	]
	,
	{defaultInvalid1, defaultValid2}
	,
	Message[CheckedOptionValue::unknownOptionOwner,
		{{"opt1" -> defaultInvalid1}, "opt2" -> defaultValid2}
	]
	,
	TestID -> "1 arg: default options, option names list: invalid, valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{"opt1" :> defaultInvalid1, opt2 -> defaultInvalid2}, {"opt1", opt2}
	]
	,
	{defaultInvalid1, defaultInvalid2}
	,
	Message[CheckedOptionValue::unknownOptionOwner,
		{"opt1" :> defaultInvalid1, opt2 -> defaultInvalid2}
	]
	,
	TestID -> "1 arg: default options, option names list: invalid, invalid"
]


(* ::Subsubsection:: *)
(*OptionValue 2 args: default options with symbols, option name specification*)


withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{{opt1 -> defaultValid1}, testFunc, testFuncPassCheck},
		opt1
	]
	,
	defaultValid1
	,
	TestID -> "1 arg: default options with symbols, option name: \
(valid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{"opt1" :> defaultInvalid1, testFuncPassCheck, testFunc}, opt1] //
			catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, opt1, defaultInvalid1]
	]
	,
	Message[testFunc::opt1, "opt1", defaultInvalid1]
	,
	TestID -> "1 arg: default options with symbols, option name: \
(valid, invalid)"
]
Test[
	CheckedOptionValue@OptionValue[
		{testFunc, opt1 -> defaultInvalid1, {testFuncPassCheck}},
		{opt1, "opt2"}
	]
	,
	{defaultValid1, defaultValid2}
	,
	TestID -> "1 arg: default options with symbols, option names list: \
(valid, valid), (valid, valid)"
]
Test[
	CheckedOptionValue@OptionValue[
		{
			opt2 :> defaultInvalid2, {{testFunc}, "opt1" :> defaultInvalid1},
			testFuncPassCheck
		},
		{"opt1", "opt2"}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt2", defaultInvalid2]
	]
	,
	Message[testFunc::opt2, "opt2", defaultInvalid2]
	,
	TestID -> "1 arg: default options with symbols, option names list: \
(valid, valid), (invalid, valid)"
]
Test[
	CheckedOptionValue@OptionValue[
		{"opt1" -> defaultInvalid1, testFunc, testFuncFailCheck}, {opt2, opt1}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncFailCheck, opt2, defaultValid2]
	]
	,
	Message[testFuncFailCheck::opt2, opt2, defaultValid2]
	,
	TestID -> "1 arg: default options with symbols, option names list: \
(valid, invalid), (invalid, invalid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{
			{
				"opt1" :> defaultInvalid1, opt2 -> defaultInvalid2,
				testFuncFailCheck
			},
			testFunc
		},
		{"opt1", opt2}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncFailCheck, "opt1", defaultInvalid1]
	]
	,
	Message[testFuncFailCheck::opt1, opt1, defaultInvalid1]
	,
	TestID -> "1 arg: default options with symbols, option names list: \
(invalid, invalid), (invalid, invalid)"
]


(* ::Subsubsection:: *)
(*OptionValue 3 args: function, options, option name specification*)


Test[
	CheckedOptionValue@OptionValue[testFunc, {"opt1" :> valid1}, opt1]
	,
	valid1
	,
	TestID -> "1 arg: function, options, option name: valid"
]
Test[
	CheckedOptionValue@OptionValue[testFunc, {"opt1" -> invalid1}, opt1] //
		catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "1 arg: function, options, option name: invalid"
]
Test[
	CheckedOptionValue@OptionValue[testFunc, {opt2 -> valid2}, {opt1, "opt2"}]
	,
	{defaultValid1, valid2}
	,
	TestID -> "1 arg: function, options, option names list: valid, valid"
]
Test[
	CheckedOptionValue@OptionValue[
		testFunc, {opt1 -> valid1, opt2 :> invalid2}, {"opt1", "opt2"}
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, "opt2", invalid2]]
	,
	Message[testFunc::opt2, "opt2", invalid2]
	,
	TestID -> "1 arg: function, options, option names list: valid, invalid"
]
Test[
	CheckedOptionValue@OptionValue[
		testFunc, {opt1 -> invalid1}, {opt1, opt2}
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "1 arg: function, options, option names list: invalid, valid"
]
Test[
	CheckedOptionValue@OptionValue[
		testFunc, {opt1 :> invalid1, opt2 :> invalid2}, {"opt1", opt2}
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, "opt1", invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "1 arg: function, options, option names list: invalid, invalid"
]


(* ::Subsubsection:: *)
(*OptionValue 3 args: default options, options, option name specification*)


withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{opt1 -> defaultInvalid1}, {"opt1" :> valid1}, opt1
	]
	,
	valid1
	,
	Message[CheckedOptionValue::unknownOptionOwner, {opt1 -> defaultInvalid1}]
	,
	TestID -> "1 arg: default options, options, option name: valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{{{"opt1" -> defaultValid1}}}, {"opt1" -> invalid1}, opt1
	]
	,
	invalid1
	,
	Message[CheckedOptionValue::unknownOptionOwner,
		{{{"opt1" -> defaultValid1}}}
	]
	,
	TestID -> "1 arg: default options, options, option name: invalid"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{"opt1" -> defaultValid1, "opt2" -> defaultInvalid2},
		{opt2 :> valid2}, {opt1, "opt2"}
	]
	,
	{defaultValid1, valid2}
	,
	Message[CheckedOptionValue::unknownOptionOwner,
		{"opt1" -> defaultValid1, "opt2" -> defaultInvalid2}
	]
	,
	TestID -> "1 arg: default options, options, option names list: \
valid, valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{"opt1" -> defaultInvalid1, opt2 -> defaultInvalid2},
		{opt1 :> valid1}, {"opt1", "opt2"}
	]
	,
	{valid1, defaultInvalid2}
	,
	Message[CheckedOptionValue::unknownOptionOwner,
		{"opt1" -> defaultInvalid1, opt2 -> defaultInvalid2}
	]
	,
	TestID -> "1 arg: default options, options, option names list: \
valid, invalid"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{"opt1" -> defaultValid1, "opt2" :> defaultInvalid2},
		{"opt1" -> invalid1, opt2 -> valid2}, {opt1, opt2}
	]
	,
	{invalid1, valid2}
	,
	Message[CheckedOptionValue::unknownOptionOwner,
		{"opt1" -> defaultValid1, "opt2" :> defaultInvalid2}
	]
	,
	TestID -> "1 arg: default options, options, option names list: \
invalid, valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{"opt1" -> defaultInvalid1, {opt2 -> defaultInvalid2}},
		{opt2 :> invalid2, opt1 :> invalid1}, {"opt1", opt2}
	]
	,
	{invalid1, invalid2}
	,
	Message[CheckedOptionValue::unknownOptionOwner,
		{"opt1" -> defaultInvalid1, {opt2 -> defaultInvalid2}}
	]
	,
	TestID -> "1 arg: default options, options, option names list: \
invalid, invalid"
]


(* ::Subsubsection:: *)
(*OptionValue 3 args: default options with symbols, options, option name specification*)


withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{testFunc, {opt1 -> defaultInvalid1, testFuncPassCheck}},
		{"opt1" :> valid1}, opt1
	]
	,
	valid1
	,
	TestID -> "1 arg: default options with symbols, options, option name: \
(valid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{"opt1" -> defaultValid1, testFuncFailCheck, testFunc},
		{"opt1" -> valid1}, opt1
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncFailCheck, opt1, valid1]
	]
	,
	Message[testFuncFailCheck::opt1, opt1, valid1]
	,
	TestID -> "1 arg: default options with symbols, options, option name: \
(invalid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{"opt1" -> defaultValid1, testFuncPassCheck, testFunc},
		{opt2 :> valid2}, {opt1, "opt2"}
	]
	,
	{defaultValid1, valid2}
	,
	TestID -> "1 arg: \
default options with symbols, options, option names list: \
(valid, valid), (valid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{"opt1" -> defaultValid1, testFuncB, opt2 -> defaultValid2, testFunc},
		{opt2 :> invalid2}, {"opt1", "opt2"}
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFuncB, "opt2", invalid2]]
	,
	Message[testFuncB::opt2, "opt2", invalid2]
	,
	TestID -> "1 arg: \
default options with symbols, options, option names list: \
(valid, valid), (invalid, invalid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{
			"opt1" -> defaultValid1, {testFunc}, testFuncPassCheck,
			"opt2" :> defaultInvalid2
		},
		{"opt1" -> invalid1, opt2 -> valid2}, {opt1, opt2}
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "1 arg: \
default options with symbols, options, option names list: \
(invalid, valid), (valid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{
			testFuncFailCheck, "opt1" -> defaultInvalid1,
			opt2 -> defaultInvalid2, {{testFunc}}
		},
		{opt2 :> valid2, opt1 :> invalid1}, {"opt1", opt2}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncFailCheck, "opt1", invalid1]
	]
	,
	Message[testFuncFailCheck::opt1, opt1, invalid1]
	,
	TestID -> "1 arg: \
default options with symbols, options, option names list: \
(invalid, invalid), (invalid, valid)"
]


(* ::Subsubsection:: *)
(*OptionValue 4 args: function, options, option name specification, wrapper*)


Test[
	CheckedOptionValue@
		OptionValue[testFunc, {"opt1" :> valid1}, opt1, wrapper]
	,
	wrapper[valid1]
	,
	TestID -> "1 arg: function, options, option name, wrapper: valid"
]
Test[
	CheckedOptionValue@OptionValue[
		testFunc, {"opt1" -> invalid1}, opt1, wrapper
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "1 arg: function, options, option name, wrapper: invalid"
]
Test[
	CheckedOptionValue@OptionValue[
		testFunc, {opt2 -> valid2}, {opt1, "opt2"}, wrapper
	]
	,
	{wrapper[defaultValid1], wrapper[valid2]}
	,
	TestID -> "1 arg: function, options, option names list, wrapper: \
valid, valid"
]
Test[
	CheckedOptionValue@OptionValue[
		testFunc, {opt1 -> valid1, opt2 :> invalid2}, {"opt1", "opt2"},
		wrapper
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, "opt2", invalid2]]
	,
	Message[testFunc::opt2, "opt2", invalid2]
	,
	TestID -> "1 arg: function, options, option names list, wrapper: \
valid, invalid"
]
Test[
	CheckedOptionValue@OptionValue[
		testFunc, {opt1 -> invalid1}, {opt1, opt2}, wrapper
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "1 arg: function, options, option names list, wrapper: \
invalid, valid"
]
Test[
	CheckedOptionValue@OptionValue[
		testFunc, {opt1 :> invalid1, opt2 :> invalid2}, {"opt1", opt2},
		wrapper
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, "opt1", invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "1 arg: function, options, option names list, wrapper: \
invalid, invalid"
]


(* ::Subsubsection:: *)
(*OptionValue 4 args: default options, options, option name specification, wrapper*)


withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{opt1 -> defaultInvalid1}, {"opt1" :> valid1}, opt1, wrapper
	]
	,
	wrapper[valid1]
	,
	Message[CheckedOptionValue::unknownOptionOwner, {opt1 -> defaultInvalid1}]
	,
	TestID -> "1 arg: default options, options, option name, wrapper: valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{"opt1" -> defaultValid1}, {"opt1" -> invalid1}, opt1, wrapper
	]
	,
	wrapper[invalid1]
	,
	Message[CheckedOptionValue::unknownOptionOwner, {"opt1" -> defaultValid1}]
	,
	TestID -> "1 arg: default options, options, option name, wrapper: invalid"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{"opt1" -> defaultValid1, "opt2" :> defaultInvalid2},
		{opt2 :> valid2}, {opt1, "opt2"}, wrapper
	]
	,
	{wrapper[defaultValid1], wrapper[valid2]}
	,
	Message[CheckedOptionValue::unknownOptionOwner,
		{"opt1" -> defaultValid1, "opt2" :> defaultInvalid2}
	]
	,
	TestID -> "1 arg: default options, options, option names list, wrapper: \
valid, valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{"opt1" -> defaultInvalid1, {{opt2 -> defaultInvalid2}}},
		{opt1 :> valid1}, {"opt1", "opt2"}, wrapper
	]
	,
	{wrapper[valid1], wrapper[defaultInvalid2]}
	,
	Message[CheckedOptionValue::unknownOptionOwner,
		{"opt1" -> defaultInvalid1, {{opt2 -> defaultInvalid2}}}
	]
	,
	TestID -> "1 arg: default options, options, option names list, wrapper: \
valid, invalid"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{"opt1" -> defaultValid1, "opt2" :> defaultInvalid2},
		{"opt1" -> invalid1, opt2 -> valid2}, {opt1, opt2}, wrapper
	]
	,
	{wrapper[invalid1], wrapper[valid2]}
	,
	Message[CheckedOptionValue::unknownOptionOwner,
		{"opt1" -> defaultValid1, "opt2" :> defaultInvalid2}
	]
	,
	TestID -> "1 arg: default options, options, option names list, wrapper: \
invalid, valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{{"opt1" -> defaultInvalid1}, opt2 -> defaultInvalid2},
		{opt2 :> invalid2, opt1 :> invalid1}, {"opt1", opt2}, wrapper
	]
	,
	{wrapper[invalid1], wrapper[invalid2]}
	,
	Message[CheckedOptionValue::unknownOptionOwner,
		{{"opt1" -> defaultInvalid1}, opt2 -> defaultInvalid2}
	]
	,
	TestID -> "1 arg: default options, options, option names list, wrapper: \
invalid, invalid"
]


(* ::Subsubsection:: *)
(*OptionValue 4 args: default options with symbols, options, option name specification, wrapper*)


withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{opt1 -> defaultInvalid1, testFunc, {}, testFuncPassCheck},
		{"opt1" :> valid1}, opt1, wrapper
	]
	,
	wrapper[valid1]
	,
	TestID -> "1 arg: \
default options with symbols, options, option name, wrapper: (valid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{testFunc, {testFuncFailCheck, "opt1" -> defaultValid1}},
		{"opt1" -> invalid1}, opt1, wrapper
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "1 arg: \
default options with symbols, options, option name, wrapper: \
(invalid, invalid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{"opt1" -> defaultValid1, testFunc, testFuncPassCheck},
		{opt2 :> valid2}, {opt1, "opt2"}, wrapper
	]
	,
	{wrapper[defaultValid1], wrapper[valid2]}
	,
	TestID -> "1 arg: \
default options with symbols, options, option names list, wrapper: \
(valid, valid), (valid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{
			{opt2 -> defaultInvalid2, testFuncPassCheck}, testFunc,
			{{}, "opt1" -> defaultInvalid1}
		},
		{opt1 :> valid1}, {"opt1", "opt2"}, wrapper
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt2", defaultInvalid2]
	]
	,
	Message[testFunc::opt2, "opt2", defaultInvalid2]
	,
	TestID -> "1 arg: \
default options with symbols, options, option names list, wrapper: \
(valid, valid), (valid, invalid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{
			testFuncPassCheck, testFunc,
			"opt1" -> defaultValid1, "opt2" :> defaultInvalid2
		},
		{"opt1" -> invalid1, opt2 -> valid2}, {opt1, opt2}, wrapper
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "1 arg: \
default options with symbols, options, option names list, wrapper: \
(valid, invalid), (valid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue@OptionValue[
		{
			"opt1" -> defaultInvalid1, {testFuncFailCheck},
			opt2 -> defaultInvalid2, testFunc
		},
		{opt2 :> invalid2, opt1 -> valid1}, {"opt1", opt2}, wrapper
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncFailCheck, "opt1", valid1]
	]
	,
	Message[testFuncFailCheck::opt1, opt1, valid1]
	,
	TestID -> "1 arg: \
default options with symbols, options, option names list, wrapper: \
(invalid, valid), (invalid, invalid)"
]


(* ::Subsubsection:: *)
(*OptionsPattern - OptionValue 1 arg: option name specification*)


Test[
	Replace[testFunc[opt2 -> invalid2],
		testFunc@OptionsPattern[] :> CheckedOptionValue@OptionValue[opt1]
	]
	,
	defaultValid1
	,
	TestID -> "1 arg: RHS: option name: valid"
]
withUnusedDefaults@Test[
	Replace[testFunc[],
		testFunc@OptionsPattern[{opt1 :> defaultInvalid1, testFunc}] :>
			CheckedOptionValue@OptionValue["opt1"]
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt1", defaultInvalid1]
	]
	,
	Message[testFunc::opt1, "opt1", defaultInvalid1]
	,
	TestID -> "1 arg: RHS: option name: invalid"
]
Test[
	Replace[testFunc["opt1" :> valid1],
		testFunc@OptionsPattern[] :>
			CheckedOptionValue@OptionValue[{opt1, "opt2"}]
	]
	,
	{valid1, defaultValid2}
	,
	TestID -> "1 arg: RHS: option names list: valid, valid"
]
Test[
	Replace[testFunc[],
		testFunc@OptionsPattern[{"opt2" -> defaultInvalid2, testFunc}] :>
			CheckedOptionValue@OptionValue[{"opt1", "opt2"}]
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt2", defaultInvalid2]
	]
	,
	Message[testFunc::opt2, "opt2", defaultInvalid2]
	,
	TestID -> "1 arg: RHS: option names list: valid, invalid"
]
withUnusedDefaults@Test[
	Replace[testFunc["opt2" :> valid2, opt1 -> invalid1],
		testFunc@OptionsPattern[] :>
			CheckedOptionValue@OptionValue[{opt1, opt2}]
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "1 arg: RHS: option names list: invalid, valid"
]
withUnusedDefaults@Test[
	Replace[testFunc["opt2" :> invalid2],
		testFunc@OptionsPattern[
			{"opt1" :> defaultInvalid1, opt2 -> defaultValid2, testFunc, f}
		] :>
			CheckedOptionValue@OptionValue[{"opt2", opt1}]
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, "opt2", invalid2]]
	,
	Message[testFunc::opt2, "opt2", invalid2]
	,
	TestID -> "1 arg: RHS: option names list: invalid, invalid"
]


(* ::Subsubsection:: *)
(*Sub-options*)


Module[{nonOpt, wrapper},
	Test[
		CheckedOptionValue@OptionValue[
			testFuncSubOpt,
			{opt :> subOptPart + subOptPart ->
				{nonOpt, "subSubOpt" -> subSubSubValid}},
			opt :> subOptPart + subOptPart,
			wrapper
		]
		,
		wrapper[{nonOpt, "subSubOpt" -> subSubSubValid}]
		,
		TestID -> "1 arg: sub-options: \
function, options, parent sub-option name, wrapper: {non-opt, valid}"
	]
]
Module[{subOpt2, invalid1, invalid2, invalid3},
	Test[
		CheckedOptionValue@OptionValue[
			{
				{opt :> {
					subOpt2 -> invalid1,
					subOptPart + subOptPart -> subSubOpt -> invalid2,
					"subOpt" :> invalid3
				}},
				testFuncSubOpt
			},
			{"opt", opt -> subOpt}
		] // catchAll
		,
		HoldComplete[
			$Failed,
			InvalidOptionValue[testFuncSubOpt, "opt", {
					subOpt2 -> invalid1,
					subOptPart + subOptPart -> subSubOpt -> invalid2,
					"subOpt" :> invalid3
			}]
		]
		,
		{
			Message[testFuncSubOpt::subSubOpt,
				"opt" :> subOptPart + subOptPart :> "subSubOpt", invalid2
			],
			Message[testFuncSubOpt::subOpt, "opt" :> "subOpt", invalid3]
		}
		,
		TestID -> "1 arg: sub-options: \
default options with symbols, parent option and sub-option names list: \
{not-checked, invalid}, invalid"
	]
]


(* ::Subsubsection:: *)
(*OptionValue arguments from variables*)


withUnusedDefaults@Test[
	Replace[testFunc[opt1 -> valid1],
		testFunc@OptionsPattern[{opt2 -> defaultValid2, testFunc}] :>
			Module[{optList = {opt2, "opt1"}},
				CheckedOptionValue@
					OptionValue[Automatic, Automatic, optList, wrapper]
			]
	]
	,
	{wrapper[defaultValid2], wrapper[valid1]}
	,
	TestID -> "1 arg: RHS: \
Automatic, Automatic, option names list from variable, wrapper: valid, valid"
]
Test[
	Replace[testFunc["opt2" :> invalid2],
		testFunc@OptionsPattern[] :>
			Module[{optList = {opt1, opt2}},
				CheckedOptionValue@OptionValue[optList]
			]
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt2, invalid2]]
	,
	Message[testFunc::opt2, "opt2", invalid2]
	,
	TestID -> "1 arg: RHS: option names list from variable: valid, invalid"
]


Test[
	Module[{args = Sequence[testFunc, {opt1 :> invalid1}, "opt1"]},
		CheckedOptionValue@OptionValue[args]
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, "opt1", invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "1 arg: argument sequence from variable: invalid"
]


(* ::Subsubsection:: *)
(*Count evaluations of option value*)


Module[{counter1 = 0, counter2 = 0},
	Test[
		CheckedOptionValue@OptionValue[
			{"opt1" :> (++counter1), opt2 :> (++counter2), testFuncIntCheck},
			{opt1, opt2}
		]
		,
		{1, 1}
		,
		TestID -> "1 arg: Evaluation count: non-holding, non-holding: \
default options with function, option names list: valid, valid: evaluation"
	];
	Test[
		counter1
		,
		1
		,
		TestID -> "1 arg: Evaluation count: non-holding, non-holding: \
default options with function, option names list: valid, valid: counter1"
	];
	Test[
		counter2
		,
		1
		,
		TestID -> "1 arg: Evaluation count: non-holding, non-holding: \
default options with function, option names list: valid, valid: counter2"
	]
]

Module[{counter = 0},
	Test[
		CheckedOptionValue@OptionValue[
			testFuncHeldCheck, {opt1 :> (++counter; invalid1)}, opt1
		] // catchAll
		,
		HoldComplete[
			$Failed,
			InvalidOptionValue[testFuncHeldCheck, opt1, (++counter; invalid1)]
		]
		,
		Message[testFuncHeldCheck::opt1, "opt1", (++counter; invalid1)]
		,
		TestID -> "1 arg: Evaluation count: holding, non-holding: \
function, options, option name: invalid: evaluation"
	];
	Test[
		counter
		,
		0
		,
		TestID -> "1 arg: Evaluation count: holding, non-holding: \
function, options, option name: invalid: counter"
	]
]

Module[{counter = 0},
	Test[
		CheckedOptionValue@OptionValue[
			{"opt1" :> ++counter, testFuncIntCheck}, {}, opt1, Hold
		]
		,
		Hold[++counter]
		,
		TestID -> "1 arg: Evaluation count: non-holding, holding: \
default options with function, options, option name, wrapper: valid: \
evaluation"
	];
	Test[
		counter
		,
		1
		,
		TestID -> "1 arg: Evaluation count: non-holding, holding: \
default options with function, options, option name, wrapper: valid: counter"
	]
]

Module[{counter1 = 0, counter2 = 0},
	Test[
		CheckedOptionValue@OptionValue[
			{opt2 :> (++counter2; defaultInvalid2), testFuncHeldCheck},
			{opt1 :> (++counter1)}, {opt1, "opt2"}, HoldComplete
		] // catchAll
		,
		HoldComplete[
			$Failed,
			InvalidOptionValue[testFuncHeldCheck,
				"opt2", (++counter2; defaultInvalid2)
			]
		]
		,
		Message[testFuncHeldCheck::opt2,
			"opt2", (++counter2; defaultInvalid2)
		]
		,
		TestID -> "1 arg: Evaluation count: holding, holding: \
default options with function, options, option names list, wrapper: \
valid, invalid: evaluation"
	];
	Test[
		counter1
		,
		0
		,
		TestID -> "1 arg: Evaluation count: holding, holding: \
default options with function, options, option names list, wrapper: \
valid, invalid: counter1"
	];
	Test[
		counter2
		,
		0
		,
		TestID -> "1 arg: Evaluation count: holding, holding: \
default options with function, options, option names list, wrapper: \
valid, invalid: counter2"
	]
]


(* ::Subsubsection:: *)
(*Custom CheckOption*)


Test[
	CheckedOptionValue[
		OptionValue[testFunc, {"opt1" :> valid[1]}, opt1],
		CheckOption -> customCheckOption
	]
	,
	valid[1]
	,
	Message[customCheckOption::used, testFunc, opt1, valid[1]]
	,
	TestID -> "1 arg: Custom CheckOption: valid"
]
Test[
	CheckedOptionValue[
		OptionValue[{opt2 -> invalid[2], testFunc}, {"opt2", opt1}],
		"CheckOption" -> customCheckOption
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, "opt2", invalid[2]]]
	,
	Message[customCheckOption::used, testFunc, "opt2", invalid[2]]
	,
	TestID -> "1 arg: Custom CheckOption: invalid"
]


(* ::Subsubsection:: *)
(*OptionValue 5 args*)


Test[
	CheckedOptionValue@
		OptionValue[testFunc, {opt1 -> valid1}, opt1, wrapper, fifth] //
			Hold[#]&
	,
	OptionValue[testFunc, {opt1 -> valid1}, opt1, wrapper, fifth] // Hold
	,
	{
		Message[CheckedOptionValue::unknownOptionValueUsage,
			OptionValue[testFunc, {opt1 -> valid1}, opt1, wrapper, fifth]
		],
		Message[OptionValue::argb, OptionValue, 5, 1, 4]
	}
	,
	TestID -> "1 arg: 5 args"
]


(* ::Subsubsection:: *)
(*Non-OptionValue*)


Module[{nonOptionValue},
	Test[
		CheckedOptionValue@nonOptionValue // Hold[#]&
		,
		CheckedOptionValue@nonOptionValue // Hold
		,
		Message[CheckedOptionValue::optionValueHead,
			CheckedOptionValue@nonOptionValue
		]
		,
		TestID -> "1 arg: Non-OptionValue"
	]
]


(* ::Subsection:: *)
(*2 args symbol*)


(* ::Subsubsection:: *)
(*OptionValue 0 args*)


Test[
	CheckedOptionValue[OptionValue[], testFunc] // Hold[#]&
	,
	OptionValue[] // Hold
	,
	{
		Message[CheckedOptionValue::unknownOptionValueUsage, OptionValue[]],
		Message[OptionValue::argb, OptionValue, 0, 1, 4]
	}
	,
	TestID -> "2 args symbol: no args"
]


(* ::Subsubsection:: *)
(*OptionValue 1 arg: option name specification*)


Test[
	CheckedOptionValue[OptionValue["opt1"], testFunc] // Hold[#]&
	,
	CheckedOptionValue[OptionValue["opt1"], testFunc] // Hold
	,
	TestID -> "2 args symbol: option name"
]
Test[
	CheckedOptionValue[OptionValue[{opt1, opt2}], testFunc] // Hold[#]&
	,
	CheckedOptionValue[OptionValue[{opt1, opt2}], testFunc] // Hold
	,
	TestID -> "2 args symbol: option names list"
]


(* ::Subsubsection:: *)
(*OptionValue 2 args: function, option name specification*)


Test[
	CheckedOptionValue[OptionValue[testFuncFailCheck, opt1], testFunc]
	,
	defaultValid1
	,
	TestID -> "2 args symbol: function, option name: valid"
]
Internal`InheritedBlock[{testFuncFailCheck},
	SetOptions[testFuncFailCheck, opt1 :> defaultInvalid1];
	Test[
		CheckedOptionValue[OptionValue[testFuncFailCheck, opt1], testFunc] //
			catchAll
		,
		HoldComplete[
			$Failed,
			InvalidOptionValue[testFunc, opt1, defaultInvalid1]
		]
		,
		Message[testFunc::opt1, "opt1", defaultInvalid1]
		,
		TestID -> "2 args symbol: function, option name: invalid"
	]
]
Test[
	CheckedOptionValue[
		OptionValue[testFuncFailCheck, {opt1, "opt2"}], testFunc
	]
	,
	{defaultValid1, defaultValid2}
	,
	TestID -> "2 args symbol: function, option names list: valid, valid"
]
Internal`InheritedBlock[{testFuncFailCheck},
	SetOptions[testFuncFailCheck, "opt2" -> defaultInvalid2];
	Test[
		CheckedOptionValue[
			OptionValue[testFuncFailCheck, {"opt1", "opt2"}], testFunc
		] // catchAll
		,
		HoldComplete[
			$Failed,
			InvalidOptionValue[testFunc, "opt2", defaultInvalid2]
		]
		,
		Message[testFunc::opt2, "opt2", defaultInvalid2]
		,
		TestID -> "2 args symbol: function, option names list: valid, invalid"
	]
]
Internal`InheritedBlock[{testFuncFailCheck},
	SetOptions[testFuncFailCheck, opt1 :> defaultInvalid1];
	Test[
		CheckedOptionValue[
			OptionValue[testFuncFailCheck, {opt1, opt2}], testFunc
		] // catchAll
		,
		HoldComplete[
			$Failed,
			InvalidOptionValue[testFunc, opt1, defaultInvalid1]
		]
		,
		Message[testFunc::opt1, "opt1", defaultInvalid1]
		,
		TestID -> "2 args symbol: function, option names list: invalid, valid"
	]
]
Internal`InheritedBlock[{testFuncFailCheck},
	SetOptions[testFuncFailCheck,
		"opt1" -> defaultInvalid1, opt2 -> defaultInvalid2
	];
	Test[
		CheckedOptionValue[
			OptionValue[testFuncFailCheck, {"opt1", opt2}], testFunc
		] // catchAll
		,
		HoldComplete[
			$Failed,
			InvalidOptionValue[testFunc, "opt1", defaultInvalid1]
		]
		,
		Message[testFunc::opt1, "opt1", defaultInvalid1]
		,
		TestID -> "2 args symbol: function, option names list: \
invalid, invalid"
	]
]


(* ::Subsubsection:: *)
(*OptionValue 2 args: default options, option name specification*)


withUnusedDefaults@Test[
	CheckedOptionValue[OptionValue[{opt1 -> defaultValid1}, opt1], testFunc]
	,
	defaultValid1
	,
	TestID -> "2 args symbol: default options, option name: valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[{"opt1" :> defaultInvalid1}, opt1], testFunc
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, opt1, defaultInvalid1]
	]
	,
	Message[testFunc::opt1, "opt1", defaultInvalid1]
	,
	TestID -> "2 args symbol: default options, option name: invalid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{opt1 -> defaultValid1, opt2 -> defaultValid2}, {opt1, "opt2"}
		],
		testFunc
	]
	,
	{defaultValid1, defaultValid2}
	,
	TestID -> "2 args symbol: default options, option names list: valid, valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" :> defaultValid1, opt2 :> defaultInvalid2},
			{"opt1", "opt2"}
		],
		testFunc
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt2", defaultInvalid2]
	]
	,
	Message[testFunc::opt2, "opt2", defaultInvalid2]
	,
	TestID -> "2 args symbol: default options, option names list: \
valid, invalid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultInvalid1, "opt2" -> defaultValid2}, {opt1, opt2}
		],
		testFunc
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, opt1, defaultInvalid1]
	]
	,
	Message[testFunc::opt1, "opt1", defaultInvalid1]
	,
	TestID -> "2 args symbol: default options, option names list: \
invalid, valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" :> defaultInvalid1, opt2 -> defaultInvalid2},
			{"opt1", opt2}
		],
		testFunc
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt1", defaultInvalid1]
	]
	,
	Message[testFunc::opt1, "opt1", defaultInvalid1]
	,
	TestID -> "2 args symbol: default options, option names list: \
invalid, invalid"
]


(* ::Subsubsection:: *)
(*OptionValue 2 args: default options with symbols, option name specification*)


withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[{{opt1 -> defaultValid1}, testFuncFailCheck}, opt1],
		testFunc
	]
	,
	defaultValid1
	,
	TestID -> "2 args symbol: default options with symbols, option name: valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[{"opt1" :> defaultInvalid1, testFuncFailCheck}, opt1],
		testFunc
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, opt1, defaultInvalid1]
	]
	,
	Message[testFunc::opt1, "opt1", defaultInvalid1]
	,
	TestID -> "2 args symbol: default options with symbols, option name: \
invalid"
]
Test[
	CheckedOptionValue[
		OptionValue[
			{testFuncFailCheck, opt1 -> defaultInvalid1, {f}}, {opt1, "opt2"}
		],
		testFunc
	]
	,
	{defaultValid1, defaultValid2}
	,
	TestID -> "2 args symbol: \
default options with symbols, option names list: valid, valid"
]
Test[
	CheckedOptionValue[
		OptionValue[
			{
				opt2 :> defaultInvalid2,
				{{testFuncFailCheck}, "opt1" :> defaultInvalid1}
			},
			{"opt1", "opt2"}
		],
		testFunc
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt2", defaultInvalid2]
	]
	,
	Message[testFunc::opt2, "opt2", defaultInvalid2]
	,
	TestID -> "2 args symbol: \
default options with symbols, option names list: valid, invalid"
]
Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultInvalid1, testFuncFailCheck}, {opt1, opt2}
		],
		testFunc
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, opt1, defaultInvalid1]
	]
	,
	Message[testFunc::opt1, "opt1", defaultInvalid1]
	,
	TestID -> "2 args symbol: \
default options with symbols, option names list: invalid, valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{
				{"opt1" :> defaultInvalid1, opt2 -> defaultInvalid2},
				testFuncFailCheck
			},
			{"opt1", opt2}
		],
		testFunc
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt1", defaultInvalid1]
	]
	,
	Message[testFunc::opt1, "opt1", defaultInvalid1]
	,
	TestID -> "2 args symbol: \
default options with symbols, option names list: invalid, invalid"
]


(* ::Subsubsection:: *)
(*OptionValue 3 args: function, options, option name specification*)


Test[
	CheckedOptionValue[
		OptionValue[testFuncFailCheck, {"opt1" :> valid1}, opt1],
		testFunc
	]
	,
	valid1
	,
	TestID -> "2 args symbol: function, options, option name: valid"
]
Test[
	CheckedOptionValue[
		OptionValue[testFuncFailCheck, {"opt1" -> invalid1}, opt1],
		testFunc
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args symbol: function, options, option name: invalid"
]
Test[
	CheckedOptionValue[
		OptionValue[testFuncFailCheck, {opt2 -> valid2}, {opt1, "opt2"}],
		testFunc
	]
	,
	{defaultValid1, valid2}
	,
	TestID -> "2 args symbol: function, options, option names list: \
valid, valid"
]
Test[
	CheckedOptionValue[
		OptionValue[
			testFuncFailCheck, {opt1 -> valid1, opt2 :> invalid2},
			{"opt1", "opt2"}
		],
		testFunc
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, "opt2", invalid2]]
	,
	Message[testFunc::opt2, "opt2", invalid2]
	,
	TestID -> "2 args symbol: function, options, option names list: \
valid, invalid"
]
Test[
	CheckedOptionValue[
		OptionValue[testFuncFailCheck, {opt1 -> invalid1}, {opt1, opt2}],
		testFunc
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args symbol: function, options, option names list: \
invalid, valid"
]
Test[
	CheckedOptionValue[
		OptionValue[
			testFuncFailCheck, {opt1 :> invalid1, opt2 :> invalid2},
			{"opt1", opt2}
		],
		testFunc
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, "opt1", invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args symbol: function, options, option names list: \
invalid, invalid"
]


(* ::Subsubsection:: *)
(*OptionValue 3 args: default options, options, option name specification*)


withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[{opt1 -> defaultInvalid1}, {"opt1" :> valid1}, opt1],
		testFunc
	]
	,
	valid1
	,
	TestID -> "2 args symbol: default options, options, option name: valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[{"opt1" -> defaultValid1}, {"opt1" -> invalid1}, opt1],
		testFunc
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args symbol: default options, options, option name: invalid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultValid1, {"opt2" -> defaultInvalid2}},
			{opt2 :> valid2}, {opt1, "opt2"}
		],
		testFunc
	]
	,
	{defaultValid1, valid2}
	,
	TestID -> "2 args symbol: default options, options, option names list: \
valid, valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultInvalid1, opt2 -> defaultInvalid2},
			{opt1 :> valid1}, {"opt1", "opt2"}
		],
		testFunc
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt2", defaultInvalid2]
	]
	,
	Message[testFunc::opt2, "opt2", defaultInvalid2]
	,
	TestID -> "2 args symbol: default options, options, option names list: \
valid, invalid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultValid1, "opt2" :> defaultInvalid2},
			{"opt1" -> invalid1, opt2 -> valid2}, {opt1, opt2}
		],
		testFunc
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args symbol: default options, options, option names list: \
invalid, valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultInvalid1, opt2 -> defaultInvalid2},
			{opt2 :> invalid2, opt1 :> invalid1}, {"opt1", opt2}
		],
		testFunc
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, "opt1", invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args symbol: default options, options, option names list: \
invalid, invalid"
]


(* ::Subsubsection:: *)
(*OptionValue 3 args: default options with symbols, options, option name specification*)


withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{testFuncFailCheck, {opt1 -> defaultInvalid1, f}},
			{"opt1" :> valid1}, opt1
		],
		testFunc
	]
	,
	valid1
	,
	TestID -> "2 args symbol: \
default options with symbols, options, option name: valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultValid1, testFuncFailCheck},
			{"opt1" -> invalid1}, opt1
		],
		testFunc
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args symbol: \
default options with symbols, options, option name: invalid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultValid1, testFuncFailCheck, f}, {opt2 :> valid2},
			{opt1, "opt2"}
		],
		testFunc
	]
	,
	{defaultValid1, valid2}
	,
	TestID -> "2 args symbol: \
default options with symbols, options, option names list: valid, valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{
				"opt1" -> defaultInvalid1, opt2 -> defaultInvalid2,
				testFuncFailCheck
			},
			{opt1 :> valid1}, {"opt1", "opt2"}
		],
		testFunc
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt2", defaultInvalid2]
	]
	,
	Message[testFunc::opt2, "opt2", defaultInvalid2]
	,
	TestID -> "2 args symbol: \
default options with symbols, options, option names list: valid, invalid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{
				"opt1" -> defaultValid1, testFuncFailCheck,
				"opt2" :> defaultInvalid2, f
			},
			{"opt1" -> invalid1, opt2 -> valid2}, {opt1, opt2}
		],
		testFunc
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args symbol: \
default options with symbols, options, option names list: invalid, valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{
				testFuncFailCheck,
				"opt1" -> defaultInvalid1, opt2 -> defaultInvalid2
			},
			{opt2 :> invalid2, opt1 :> invalid1}, {"opt1", opt2}
		],
		testFunc
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, "opt1", invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args symbol: \
default options with symbols, options, option names list: invalid, invalid"
]


(* ::Subsubsection:: *)
(*OptionValue 4 args: function, options, option name specification, wrapper*)


Test[
	CheckedOptionValue[
		OptionValue[testFuncFailCheck, {"opt1" :> valid1}, opt1, wrapper],
		testFunc
	]
	,
	wrapper[valid1]
	,
	TestID -> "2 args symbol: function, options, option name, wrapper: valid"
]
Test[
	CheckedOptionValue[
		OptionValue[testFuncFailCheck, {"opt1" -> invalid1}, opt1, wrapper],
		testFunc
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args symbol: function, options, option name, wrapper: invalid"
]
Test[
	CheckedOptionValue[
		OptionValue[
			testFuncFailCheck, {opt2 -> valid2}, {opt1, "opt2"}, wrapper
		],
		testFunc
	]
	,
	{wrapper[defaultValid1], wrapper[valid2]}
	,
	TestID -> "2 args symbol: function, options, option names list, wrapper: \
valid, valid"
]
Test[
	CheckedOptionValue[
		OptionValue[
			testFuncFailCheck, {opt1 -> valid1, opt2 :> invalid2},
			{"opt1", "opt2"}, wrapper
		],
		testFunc
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, "opt2", invalid2]]
	,
	Message[testFunc::opt2, "opt2", invalid2]
	,
	TestID -> "2 args symbol: function, options, option names list, wrapper: \
valid, invalid"
]
Test[
	CheckedOptionValue[
		OptionValue[
			testFuncFailCheck, {opt1 -> invalid1}, {opt1, opt2}, wrapper
		],
		testFunc
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args symbol: function, options, option names list, wrapper: \
invalid, valid"
]
Test[
	CheckedOptionValue[
		OptionValue[
			testFuncFailCheck, {opt1 :> invalid1, opt2 :> invalid2},
			{"opt1", opt2}, wrapper
		],
		testFunc
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, "opt1", invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args symbol: function, options, option names list, wrapper: \
invalid, invalid"
]


(* ::Subsubsection:: *)
(*OptionValue 4 args: default options, options, option name specification, wrapper*)


withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{opt1 -> defaultInvalid1}, {"opt1" :> valid1}, opt1, wrapper
		],
		testFunc
	]
	,
	wrapper[valid1]
	,
	TestID -> "2 args symbol: default options, options, option name, wrapper: \
valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultValid1}, {"opt1" -> invalid1}, opt1, wrapper
		],
		testFunc
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args symbol: default options, options, option name, wrapper: \
invalid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{{opt1 :> defaultValid1}, opt2 -> defaultValid2}, {opt2 :> valid2},
			{opt1, "opt2"}, wrapper
		],
		testFunc
	]
	,
	{wrapper[defaultValid1], wrapper[valid2]}
	,
	TestID -> "2 args symbol: \
default options, options, option names list, wrapper: valid, valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultInvalid1, opt2 -> defaultInvalid2},
			{opt1 :> valid1}, {"opt1", "opt2"}, wrapper
		],
		testFunc
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt2", defaultInvalid2]
	]
	,
	Message[testFunc::opt2, "opt2", defaultInvalid2]
	,
	TestID -> "2 args symbol: \
default options, options, option names list, wrapper: valid, invalid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultValid1, "opt2" :> defaultInvalid2},
			{"opt1" -> invalid1, opt2 -> valid2}, {opt1, opt2}, wrapper
		],
		testFunc
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args symbol: \
default options, options, option names list, wrapper: invalid, valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultInvalid1, opt2 -> defaultInvalid2},
			{opt2 :> invalid2, opt1 :> invalid1}, {"opt1", opt2}, wrapper
		],
		testFunc
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, "opt1", invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args symbol: \
default options, options, option names list, wrapper: invalid, invalid"
]


(* ::Subsubsection:: *)
(*OptionValue 4 args: default options with symbols, options, option name specification, wrapper*)


withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{opt1 -> defaultInvalid1, testFuncFailCheck}, {"opt1" :> valid1},
			opt1, wrapper
		],
		testFunc
	]
	,
	wrapper[valid1]
	,
	TestID -> "2 args symbol: \
default options with symbols, options, option name, wrapper: valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{testFuncFailCheck, {f, "opt1" -> defaultValid1}},
			{"opt1" -> invalid1}, opt1, wrapper
		],
		testFunc
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args symbol: \
default options with symbols, options, option name, wrapper: invalid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultValid1, testFuncFailCheck}, {opt2 :> valid2},
			{opt1, "opt2"}, wrapper
		],
		testFunc
	]
	,
	{wrapper[defaultValid1], wrapper[valid2]}
	,
	TestID -> "2 args symbol: \
default options with symbols, options, option names list, wrapper: \
valid, valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{
				{opt2 -> defaultInvalid2, testFuncFailCheck}, f,
				{{g}, "opt1" -> defaultInvalid1}
			},
			{opt1 :> valid1}, {"opt1", "opt2"}, wrapper
		],
		testFunc
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt2", defaultInvalid2]
	]
	,
	Message[testFunc::opt2, "opt2", defaultInvalid2]
	,
	TestID -> "2 args symbol: \
default options with symbol, options, option names list, wrapper: \
valid, invalid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{
				testFuncFailCheck, f,
				"opt1" -> defaultValid1, "opt2" :> defaultInvalid2
			},
			{"opt1" -> invalid1, opt2 -> valid2}, {opt1, opt2}, wrapper
		],
		testFunc
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args symbol: \
default options with symbol, options, option names list, wrapper: \
invalid, valid"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{
				"opt1" -> defaultInvalid1, opt2 -> defaultInvalid2,
				testFuncFailCheck
			},
			{opt2 :> invalid2, opt1 :> invalid1}, {"opt1", opt2}, wrapper
		],
		testFunc
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, "opt1", invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args symbol: \
default options with symbol, options, option names list, wrapper: \
invalid, invalid"
]


(* ::Subsubsection:: *)
(*OptionsPattern - OptionValue 1 arg: option name specification*)


Test[
	Replace[testFuncFailCheck[opt2 -> invalid2],
		testFuncFailCheck@OptionsPattern[] :>
			CheckedOptionValue[OptionValue[opt1], testFunc]
	]
	,
	defaultValid1
	,
	TestID -> "2 args symbol: RHS: option name: valid"
]
withUnusedDefaults@Test[
	Replace[testFuncFailCheck[],
		testFuncFailCheck@OptionsPattern[opt1 :> defaultInvalid1] :>
			CheckedOptionValue[OptionValue["opt1"], testFunc]
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt1", defaultInvalid1]
	]
	,
	Message[testFunc::opt1, "opt1", defaultInvalid1]
	,
	TestID -> "2 args symbol: RHS: option name: invalid"
]
Test[
	Replace[testFuncFailCheck["opt1" :> valid1],
		testFuncFailCheck@OptionsPattern[] :>
			CheckedOptionValue[OptionValue[{opt1, "opt2"}], testFunc]
	]
	,
	{valid1, defaultValid2}
	,
	TestID -> "2 args symbol: RHS: option names list: valid, valid"
]
Test[
	Replace[testFuncFailCheck[],
		testFuncFailCheck@OptionsPattern[
			{{"opt2" -> defaultInvalid2, opt1 :> defaultValid1}}
		] :>
			CheckedOptionValue[OptionValue[{"opt1", "opt2"}], testFunc]
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt2", defaultInvalid2]
	]
	,
	Message[testFunc::opt2, "opt2", defaultInvalid2]
	,
	TestID -> "2 args symbol: RHS: option names list: valid, invalid"
]
withUnusedDefaults@Test[
	Replace[testFuncFailCheck["opt2" :> valid2, opt1 -> invalid1],
		testFuncFailCheck@OptionsPattern[] :>
			CheckedOptionValue[OptionValue[{opt1, opt2}], testFunc]
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args symbol: RHS: option names list: invalid, valid"
]
withUnusedDefaults@Test[
	Replace[testFuncFailCheck["opt2" :> invalid2],
		testFuncFailCheck@OptionsPattern[
			{f, "opt1" :> defaultInvalid1, opt2 -> defaultValid2}
		] :>
			CheckedOptionValue[OptionValue[{"opt2", opt1}], testFunc]
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, "opt2", invalid2]]
	,
	Message[testFunc::opt2, "opt2", invalid2]
	,
	TestID -> "2 args symbol: RHS: option names list: invalid, invalid"
]


(* ::Subsubsection:: *)
(*Sub-options*)


Test[
	CheckedOptionValue[
		OptionValue[
			"opt" :> {
				subOpt :> defaultSubSubValid,
				"subOpt" -> invalid,
				subOptPart + subOptPart -> "subSubOpt" :> defaultSubSubSubValid
			},
			{
				"opt" -> "subOpt",
				opt :> subOptPart + subOptPart :> subSubOpt
			}
		],
		testFuncSubOpt
	]
	,
	{defaultSubSubValid, defaultSubSubSubValid}
	,
	TestID -> "2 args symbol: sub-options: \
default options, sub-option names list: {valid, invalid dup}, valid"
]
Module[{nonOpt, invalid1, invalid2},
	Test[
		CheckedOptionValue[
			OptionValue[
				opt -> nonOpt,
				opt :> {
					subOpt -> invalid1,
					subOpt -> subSubValid,
					subOptPart + subOptPart :> subSubOpt -> invalid2
				},
				opt
			],
			testFuncSubOpt
		] // catchAll
		,
		HoldComplete[
			$Failed,
			InvalidOptionValue[testFuncSubOpt, opt, {
				subOpt -> invalid1,
				subOpt -> subSubValid,
				subOptPart + subOptPart :> subSubOpt -> invalid2
			}]
		]
		,
		{
			Message[testFuncSubOpt::subOpt, "opt" :> "subOpt", invalid1],
			Message[testFuncSubOpt::subSubOpt,
				"opt" :> subOptPart + subOptPart :> "subSubOpt", invalid2
			]
		}
		,
		TestID -> "2 args symbol: sub-options: \
default options, options, parent option name: {invalid, valid dup, invalid}"
	]
]


(* ::Subsubsection:: *)
(*OptionValue arguments from variables*)


withUnusedDefaults@Test[
	Replace[testFuncFailCheck[opt1 -> valid1],
		testFuncFailCheck@OptionsPattern[{opt2 -> defaultValid2, testFunc}] :>
			Module[{optList = {opt1, opt2}},
				CheckedOptionValue[OptionValue[optList], testFunc]
			]
	]
	,
	{valid1, defaultValid2}
	,
	TestID -> "2 args symbol: RHS: option names list from variable: \
valid, valid"
]
Test[
	Module[{optList = {opt1, "opt2"}},
		CheckedOptionValue[
			OptionValue[
				{opt1 :> defaultValid1, opt2 -> defaultInvalid2},
				{"opt1" -> invalid1}, optList
			],
			testFunc
		]
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args symbol: \
default options, options, option names list from variable: invalid, invalid"
]


Test[
	Replace[testFuncFailCheck[opt1 -> valid1],
		testFuncFailCheck@OptionsPattern[
			{opt1 -> defaultInvalid1, opt2 :> defaultValid2}
		] :>
			Module[{args = Sequence[{opt2, opt1}, wrapper]},
				CheckedOptionValue[OptionValue[args], testFunc]
			]
	]
	,
	{wrapper[defaultValid2], wrapper[valid1]}
	,
	TestID -> "2 args symbol: RHS: argument sequence from variable: valid"
]


(* ::Subsubsection:: *)
(*Count evaluations of option value*)


Module[{counter = 0},
	withUnusedDefaults@Test[
		Replace[testFunc[opt1 :> (++counter; invalid1)],
			testFunc@OptionsPattern[] :>
				CheckedOptionValue[OptionValue["opt1"], testFuncIntCheck]
		] // catchAll
		,
		HoldComplete[
			$Failed,
			InvalidOptionValue[testFuncIntCheck, "opt1", (++counter; invalid1)]
		]
		,
		Message[testFuncIntCheck::opt1, "opt1", invalid1]
		,
		TestID -> "2 args symbol: Evaluation count: non-holding, non-holding: \
RHS: option name: invalid: evaluation"
	];
	Test[
		counter
		,
		1
		,
		TestID -> "2 args symbol: Evaluation count: non-holding, non-holding: \
RHS: option name: invalid: counter"
	]
]

Module[{counter1 = 0, counter2 = 0},
	Test[
		CheckedOptionValue[
			OptionValue[
				{opt1 :> (++counter1), opt2 :> (++counter2)}, {opt1, "opt2"}
			],
			testFuncHeldCheck
		]
		,
		{1, 1}
		,
		TestID -> "2 args symbol: Evaluation count: holding, non-holding: \
default options, option names list: valid, valid: evaluation"
	];
	Test[
		counter1
		,
		1
		,
		TestID -> "2 args symbol: Evaluation count: holding, non-holding: \
default options, option names list: valid, valid: counter1"
	];
	Test[
		counter2
		,
		1
		,
		TestID -> "2 args symbol: Evaluation count: holding, non-holding: \
default options, option names list: valid, valid: counter2"
	]
]

Module[{counter1 = 0, counter2 = 0},
	withUnusedDefaults@Test[
		Replace[testFunc[opt2 :> (++counter2; invalid2)],
			testFunc@OptionsPattern[
				{"opt1" :> (++counter1; invalid1), "opt2" -> defaultInvalid2}
			] :>
				CheckedOptionValue[
					OptionValue[
						Automatic, Automatic, {"opt2", opt1}, HoldComplete
					],
					testFuncIntCheck
				]
		] // catchAll
		,
		HoldComplete[
			$Failed,
			InvalidOptionValue[testFuncIntCheck,
				"opt2", (++counter2; invalid2)
			]
		]
		,
		Message[testFuncIntCheck::opt2, "opt2", invalid2]
		,
		TestID -> "2 args symbol: Evaluation count: non-holding, holding: \
RHS: Automatic, Automatic, option names list, wrapper: invalid, invalid: \
evaluation"
	];
	Test[
		counter1
		,
		0
		,
		TestID -> "2 args symbol: Evaluation count: non-holding, holding: \
RHS: Automatic, Automatic, option names list, wrapper: invalid, invalid: \
counter1"
	];
	Test[
		counter2
		,
		1
		,
		TestID -> "2 args symbol: Evaluation count: non-holding, holding: \
RHS: Automatic, Automatic, option names list, wrapper: invalid, invalid: \
counter2"
	]
]

Module[{counter = 0},
	withUnusedDefaults@Test[
		CheckedOptionValue[
			OptionValue[
				opt2 -> defaultInvalid2, opt2 :> (++counter), "opt2", Hold
			],
			testFuncHeldCheck
		]
		,
		Hold[++counter]
		,
		TestID -> "2 args symbol: Evaluation count: holding, holding: \
default options, options, option name, wrapper: valid: evaluation"
	];
	Test[
		counter
		,
		0
		,
		TestID -> "2 args symbol: Evaluation count: holding, holding: \
default options, options, option name, wrapper: valid: counter"
	]
]


(* ::Subsubsection:: *)
(*Custom CheckOption*)


Test[
	CheckedOptionValue[
		OptionValue[
			{opt1 -> valid[1], "opt2" -> invalid[2]}, opt2 :> valid[2],
			{"opt1", opt2}, wrapper
		],
		testFunc,
		CheckOption -> customCheckOption
	]
	,
	{wrapper@valid[1], wrapper@valid[2]}
	,
	{
		Message[customCheckOption::used, testFunc, "opt1", valid[1]],
		Message[customCheckOption::used, testFunc, opt2, valid[2]]
	}
	,
	TestID -> "2 args symbol: Custom CheckOption: valid"
]
Test[
	CheckedOptionValue[
		OptionValue[opt1 -> valid[1], opt1 :> invalid[1], opt1],
		testFunc,
		"CheckOption" -> customCheckOption
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid[1]]]
	,
	Message[customCheckOption::used, testFunc, opt1, invalid[1]]
	,
	TestID -> "2 args symbol: Custom CheckOption: invalid"
]


(* ::Subsubsection:: *)
(*OptionValue 5 args*)


Test[
	CheckedOptionValue[
		OptionValue[testFunc, {opt1 -> valid1}, opt1, wrapper, fifth],
		testFunc
	] // Hold[#]&
	,
	OptionValue[testFunc, {opt1 -> valid1}, opt1, wrapper, fifth] // Hold
	,
	{
		Message[CheckedOptionValue::unknownOptionValueUsage,
			OptionValue[testFunc, {opt1 -> valid1}, opt1, wrapper, fifth]
		],
		Message[OptionValue::argb, OptionValue, 5, 1, 4]
	}
	,
	TestID -> "2 args symbol: 5 args"
]


(* ::Subsubsection:: *)
(*Non-OptionValue*)


Module[{nonOptionValue},
	Test[
		CheckedOptionValue[nonOptionValue, testFunc] // Hold[#]&
		,
		CheckedOptionValue[nonOptionValue, testFunc] // Hold
		,
		Message[CheckedOptionValue::optionValueHead,
			CheckedOptionValue[nonOptionValue, testFunc]
		]
		,
		TestID -> "2 args symbol: Non-OptionValue"
	]
]


(* ::Subsection:: *)
(*2 args list*)


(* ::Subsubsection:: *)
(*OptionValue 0 args*)


Test[
	CheckedOptionValue[OptionValue[], {testFunc, testFuncFailCheck}] //
		Hold[#]&
	,
	OptionValue[] // Hold
	,
	{
		Message[CheckedOptionValue::unknownOptionValueUsage, OptionValue[]],
		Message[OptionValue::argb, OptionValue, 0, 1, 4]
	}
	,
	TestID -> "2 args list: no args"
]
Test[
	CheckedOptionValue[OptionValue[], {}] // Hold[#]&
	,
	OptionValue[] // Hold
	,
	Message[OptionValue::argb, OptionValue, 0, 1, 4]
	,
	TestID -> "2 args list empty: no args"
]


(* ::Subsubsection:: *)
(*OptionValue 1 arg: option name specification*)


Test[
	CheckedOptionValue[OptionValue["opt1"], {testFunc, testFuncFailCheck}] //
		Hold[#]&
	,
	CheckedOptionValue[OptionValue["opt1"], {testFunc, testFuncFailCheck}] //
		Hold
	,
	TestID -> "2 args list: option name"
]
Test[
	CheckedOptionValue[
		OptionValue[{opt1, opt2}], {testFunc, testFuncFailCheck}
	] // Hold[#]&
	,
	CheckedOptionValue[
		OptionValue[{opt1, opt2}], {testFunc, testFuncFailCheck}
	] // Hold
	,
	TestID -> "2 args list: option names list"
]


Test[
	CheckedOptionValue[OptionValue["opt1"], {}] // Hold[#]&
	,
	OptionValue["opt1"] // Hold
	,
	TestID -> "2 args list empty: option name"
]


(* ::Subsubsection:: *)
(*OptionValue 2 args: function, option name specification*)


Test[
	CheckedOptionValue[
		OptionValue[testFuncFailCheck, opt1], {testFunc, testFuncPassCheck}
	]
	,
	defaultValid1
	,
	TestID -> "2 args list: function, option name: (valid, valid)"
]
Internal`InheritedBlock[{testFuncPassCheck},
	SetOptions[testFuncPassCheck, opt1 :> defaultInvalid1];
	Test[
		CheckedOptionValue[
			OptionValue[testFuncPassCheck, opt1], {testFuncFailCheck, testFunc}
		] // catchAll
		,
		HoldComplete[
			$Failed,
			InvalidOptionValue[testFuncFailCheck, opt1, defaultInvalid1]
		]
		,
		Message[testFuncFailCheck::opt1, opt1, defaultInvalid1]
		,
		TestID -> "2 args list: function, option name: (invalid, invalid)"
	]
]
Test[
	CheckedOptionValue[
		OptionValue[testFuncFailCheck, {opt1, "opt2"}],
		{testFuncPassCheck, testFunc}
	]
	,
	{defaultValid1, defaultValid2}
	,
	TestID -> "2 args list: function, option names list: \
(valid, valid), (valid, valid)"
]
Internal`InheritedBlock[{testFuncFailCheck},
	SetOptions[testFuncFailCheck, "opt2" -> defaultInvalid2];
	Test[
		CheckedOptionValue[
			OptionValue[testFuncFailCheck, {"opt1", "opt2"}],
			{testFuncPassCheck, testFunc}
		] // catchAll
		,
		HoldComplete[
			$Failed,
			InvalidOptionValue[testFunc, "opt2", defaultInvalid2]
		]
		,
		Message[testFunc::opt2, "opt2", defaultInvalid2]
		,
		TestID -> "2 args list: function, option names list: \
(valid, valid), (valid, invalid)"
	]
]
Internal`InheritedBlock[{testFuncFailCheck},
	SetOptions[testFuncFailCheck, opt1 :> defaultInvalid1];
	Test[
		CheckedOptionValue[
			OptionValue[testFuncFailCheck, {opt1, opt2}],
			{testFunc, testFuncPassCheck}
		] // catchAll
		,
		HoldComplete[
			$Failed,
			InvalidOptionValue[testFunc, opt1, defaultInvalid1]
		]
		,
		Message[testFunc::opt1, "opt1", defaultInvalid1]
		,
		TestID -> "2 args list: function, option names list: \
(invalid, valid), (valid, valid)"
	]
]
Internal`InheritedBlock[{testFuncPassCheck},
	SetOptions[testFuncPassCheck, "opt1" -> defaultInvalid1];
	Test[
		CheckedOptionValue[
			OptionValue[testFuncPassCheck, {"opt1", opt2}],
			{testFuncFailCheck, testFunc}
		] // catchAll
		,
		HoldComplete[
			$Failed,
			InvalidOptionValue[testFuncFailCheck, "opt1", defaultInvalid1]
		]
		,
		Message[testFuncFailCheck::opt1, opt1, defaultInvalid1]
		,
		TestID -> "2 args list: function, option names list: \
(invalid, invalid), (invalid, valid)"
	]
]

Test[
	CheckedOptionValue[OptionValue[testFuncFailCheck, {opt1, "opt2"}], {}]
	,
	{defaultValid1, defaultValid2}
	,
	TestID -> "2 args list empty: function, option names list"
]


(* ::Subsubsection:: *)
(*OptionValue 2 args: default options, option name specification*)


withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[{opt1 -> defaultValid1}, opt1],
		{testFuncPassCheck, testFunc}
	]
	,
	defaultValid1
	,
	TestID -> "2 args list: default options, option name: (valid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[{"opt1" :> defaultInvalid1}, opt1],
		{testFuncPassCheck, testFuncFailCheck}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncFailCheck, opt1, defaultInvalid1]
	]
	,
	Message[testFuncFailCheck::opt1, opt1, defaultInvalid1]
	,
	TestID -> "2 args list: default options, option name: (valid, invalid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{opt1 -> defaultValid1, opt2 -> defaultValid2}, {opt1, "opt2"}
		],
		{testFunc, testFuncPassCheck}
	]
	,
	{defaultValid1, defaultValid2}
	,
	TestID -> "2 args list: default options, option names list: \
(valid, valid), (valid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" :> defaultValid1, opt2 :> defaultInvalid2},
			{"opt1", "opt2"}
		],
		{testFunc, testFuncPassCheck}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt2", defaultInvalid2]
	]
	,
	Message[testFunc::opt2, "opt2", defaultInvalid2]
	,
	TestID -> "2 args list: default options, option names list: \
(valid, valid), (invalid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultInvalid1, "opt2" -> defaultValid2}, {opt1, opt2}
		],
		{testFuncPassCheck, testFuncFailCheck}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncFailCheck, opt1, defaultInvalid1]
	]
	,
	Message[testFuncFailCheck::opt1, opt1, defaultInvalid1]
	,
	TestID -> "2 args list: default options, option names list: \
(valid, invalid), (valid, invalid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" :> defaultInvalid1, opt2 -> defaultInvalid2},
			{"opt1", opt2}
		],
		{testFunc, testFuncFailCheck}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt1", defaultInvalid1]
	]
	,
	Message[testFunc::opt1, "opt1", defaultInvalid1]
	,
	TestID -> "2 args list: default options, option names list: \
(invalid, invalid), (invalid, invalid)"
]

Test[
	CheckedOptionValue[OptionValue[{opt1 -> defaultValid1}, opt1], {}]
	,
	defaultValid1
	,
	TestID -> "2 args list empty: default options, option name"
]


(* ::Subsubsection:: *)
(*OptionValue 2 args: default options with symbol, option name specification*)


withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[{{opt1 -> defaultValid1}, testFuncFailCheck}, opt1],
		{testFunc, testFuncPassCheck}
	]
	,
	defaultValid1
	,
	TestID -> "2 args list: default options with symbol, option name: \
(valid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[{"opt1" :> defaultValid1, testFuncPassCheck}, opt1],
		{testFuncFailCheck, testFunc}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncFailCheck, opt1, defaultValid1]
	]
	,
	Message[testFuncFailCheck::opt1, opt1, defaultValid1]
	,
	TestID -> "2 args list: default options with symbol, option name: \
(invalid, valid)"
]
Test[
	CheckedOptionValue[
		OptionValue[
			{testFuncFailCheck, opt1 -> defaultInvalid1, {f}}, {opt1, "opt2"}
		],
		{testFuncPassCheck, testFunc}
	]
	,
	{defaultValid1, defaultValid2}
	,
	TestID -> "2 args list: default options with symbol, option names list: \
(valid, valid), (valid, valid)"
]
Test[
	CheckedOptionValue[
		OptionValue[
			{
				opt2 :> defaultInvalid2,
				{{testFuncFailCheck}, "opt1" :> defaultInvalid1}
			},
			{"opt1", "opt2"}
		],
		{testFuncPassCheck, testFunc}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt2", defaultInvalid2]
	]
	,
	Message[testFunc::opt2, "opt2", defaultInvalid2]
	,
	TestID -> "2 args list: default options with symbol, option names list: \
(valid, valid), (valid, invalid)"
]
Test[
	CheckedOptionValue[
		OptionValue[
			{
				"opt1" -> defaultValid1, opt2 :> {Sequence[1, 2]},
				testFuncFailCheck
			},
			{opt1, opt2}
		],
		{testFunc, testFuncHeldCheck}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncHeldCheck, opt1, defaultValid1]
	]
	,
	Message[testFuncHeldCheck::opt1, "opt1", defaultValid1]
	,
	TestID -> "2 args list: default options with symbol, option names list: \
(valid, invalid), (invalid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{
				{"opt1" :> {Sequence[3, 9]}, opt2 -> defaultInvalid2},
				testFuncFailCheck
			},
			{"opt1", opt2}
		],
		{testFuncHeldCheck, testFunc}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt1", {Sequence[3, 9]}]
	]
	,
	Message[testFunc::opt1, "opt1", {3, 9}]
	,
	TestID -> "2 args list: default options with symbol, option names list: \
(valid, invalid), (invalid, invalid)"
]

Test[
	CheckedOptionValue[
		OptionValue[
			{testFuncFailCheck, opt1 -> defaultInvalid1}, {opt1, "opt2"}
		],
		{}
	]
	,
	{defaultValid1, defaultValid2}
	,
	TestID -> "2 args list empty: \
default options with symbol, option names list"
]


(* ::Subsubsection:: *)
(*OptionValue 3 args: function, options, option name specification*)


Test[
	CheckedOptionValue[
		OptionValue[testFuncFailCheck, {"opt1" :> valid1}, opt1],
		{testFuncPassCheck, testFunc}
	]
	,
	valid1
	,
	TestID -> "2 args list: function, options, option name: (valid, valid)"
]
Test[
	CheckedOptionValue[
		OptionValue[testFunc, {"opt1" -> invalid1}, opt1],
		{testFuncPassCheck, testFuncFailCheck}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncFailCheck, opt1, invalid1]
	]
	,
	Message[testFuncFailCheck::opt1, opt1, invalid1]
	,
	TestID -> "2 args list: function, options, option name: (valid, invalid)"
]
Test[
	CheckedOptionValue[
		OptionValue[testFuncFailCheck, {opt2 -> valid2}, {opt1, "opt2"}],
		{testFunc, testFuncPassCheck}
	]
	,
	{defaultValid1, valid2}
	,
	TestID -> "2 args list: function, options, option names list: \
(valid, valid), (valid, valid)"
]
Test[
	CheckedOptionValue[
		OptionValue[
			testFuncFailCheck, {opt1 -> valid1, opt2 :> invalid2},
			{"opt1", "opt2"}
		],
		{testFunc, testFuncPassCheck}
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, "opt2", invalid2]]
	,
	Message[testFunc::opt2, "opt2", invalid2]
	,
	TestID -> "2 args list: function, options, option names list: \
(valid, valid), (invalid, valid)"
]
Test[
	CheckedOptionValue[
		OptionValue[testFunc, {opt1 -> invalid1}, {opt1, opt2}],
		{testFuncPassCheck, testFuncFailCheck}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncFailCheck, opt1, invalid1]
	]
	,
	Message[testFuncFailCheck::opt1, opt1, invalid1]
	,
	TestID -> "2 args list: function, options, option names list: \
(valid, invalid), (valid, invalid)"
]
Test[
	CheckedOptionValue[
		OptionValue[
			testFuncPassCheck, {opt1 :> invalid1, opt2 :> invalid2},
			{"opt1", opt2}
		],
		{testFuncFailCheck, testFunc}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncFailCheck, "opt1", invalid1]
	]
	,
	Message[testFuncFailCheck::opt1, opt1, invalid1]
	,
	TestID -> "2 args list: function, options, option names list: \
(invalid, invalid), (invalid, invalid)"
]

Test[
	CheckedOptionValue[
		OptionValue[testFuncFailCheck, {"opt1" :> valid1}, opt1], {}
	]
	,
	valid1
	,
	TestID -> "2 args list empty: function, options, option name"
]


(* ::Subsubsection:: *)
(*OptionValue 3 args: default options, options, option name specification*)


withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[{opt1 -> defaultInvalid1}, {"opt1" :> valid1}, opt1],
		{testFuncPassCheck, testFunc}
	]
	,
	valid1
	,
	TestID -> "2 args list: default options, options, option name: \
(valid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[{"opt1" -> defaultInvalid1}, {"opt1" -> valid1}, opt1],
		{testFuncFailCheck, testFunc}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncFailCheck, opt1, valid1]
	]
	,
	Message[testFuncFailCheck::opt1, opt1, valid1]
	,
	TestID -> "2 args list: default options, options, option name: \
(invalid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultValid1, {"opt2" -> defaultInvalid2}},
			{opt2 :> valid2}, {opt1, "opt2"}
		],
		{testFuncPassCheck, testFunc}
	]
	,
	{defaultValid1, valid2}
	,
	TestID -> "2 args list: default options, options, option names list: \
(valid, valid), (valid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultInvalid1, opt2 -> defaultInvalid2},
			{opt1 :> valid1}, {"opt1", "opt2"}
		],
		{testFuncPassCheck, testFunc}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt2", defaultInvalid2]
	]
	,
	Message[testFunc::opt2, "opt2", defaultInvalid2]
	,
	TestID -> "2 args list: default options, options, option names list: \
(valid, valid), (valid, invalid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultValid1, "opt2" :> defaultInvalid2},
			{"opt1" -> valid1, opt2 :> {Sequence[2, 7]}}, {opt1, opt2}
		],
		{testFunc, testFuncHeldCheck}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncHeldCheck, opt1, valid1]
	]
	,
	Message[testFuncHeldCheck::opt1, "opt1", valid1]
	,
	TestID -> "2 args list: default options, options, option names list: \
(valid, invalid), (invalid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultInvalid1, opt2 -> defaultInvalid2},
			{opt1 :> {Sequence[4, 21]}}, {"opt1", opt2}
		],
		{testFuncHeldCheck, testFunc}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt1", {Sequence[4, 21]}]
	]
	,
	Message[testFunc::opt1, "opt1", {4, 21}]
	,
	TestID -> "2 args list: default options, options, option names list: \
(valid, invalid), (invalid, invalid)"
]

Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultValid1, {"opt2" -> defaultInvalid2}},
			{opt2 :> valid2}, {opt1, "opt2"}
		],
		{}
	]
	,
	{defaultValid1, valid2}
	,
	TestID -> "2 args list empty: default options, options, option names list"
]


(* ::Subsubsection:: *)
(*OptionValue 3 args: default options with symbol, options, option name specification*)


withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{testFuncFailCheck, {opt1 -> defaultInvalid1, f}},
			{"opt1" :> valid1}, opt1
		],
		{testFuncPassCheck, testFunc}
	]
	,
	valid1
	,
	TestID -> "2 args list: \
default options with symbol, options, option name: (valid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultValid1, testFuncPassCheck},
			{"opt1" -> invalid1}, opt1
		],
		{testFuncFailCheck, testFunc}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncFailCheck, opt1, invalid1]
	]
	,
	Message[testFuncFailCheck::opt1, opt1, invalid1]
	,
	TestID -> "2 args list: \
default options with symbol, options, option name: (invalid, invalid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultValid1, testFuncFailCheck, f}, {opt2 :> valid2},
			{opt1, "opt2"}
		],
		{testFunc, testFuncPassCheck}
	]
	,
	{defaultValid1, valid2}
	,
	TestID -> "2 args list: \
default options with symbol, options, option names list: \
(valid, valid), (valid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{
				"opt1" -> defaultInvalid1, opt2 -> defaultInvalid2,
				testFuncFailCheck
			},
			{opt1 :> valid1}, {"opt1", "opt2"}
		],
		{testFuncPassCheck, testFunc}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt2", defaultInvalid2]
	]
	,
	Message[testFunc::opt2, "opt2", defaultInvalid2]
	,
	TestID -> "2 args list: \
default options with symbol, options, option names list: \
(valid, valid), (valid, invalid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{
				"opt1" -> defaultValid1, testFuncFailCheck,
				"opt2" :> defaultInvalid2, f
			},
			{"opt1" -> invalid1, opt2 -> valid2}, {opt1, opt2}
		],
		{testFunc, testFuncPassCheck}
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args list: \
default options with symbol, options, option names list: \
(invalid, valid), (valid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{
				testFuncPassCheck,
				"opt1" -> defaultInvalid1, opt2 -> defaultInvalid2
			},
			{opt2 :> invalid2, opt1 :> invalid1}, {"opt1", opt2}
		],
		{testFuncFailCheck, testFunc}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncFailCheck, "opt1", invalid1]
	]
	,
	Message[testFuncFailCheck::opt1, opt1, invalid1]
	,
	TestID -> "2 args list: \
default options with symbol, options, option names list: \
(invalid, invalid), (invalid, valid)"
]

Test[
	CheckedOptionValue[
		OptionValue[
			{testFuncFailCheck, {opt1 -> defaultInvalid1, f}},
			{"opt1" :> valid1}, opt1
		],
		{}
	]
	,
	valid1
	,
	TestID -> "2 args list empty: \
default options with symbol, options, option name"
]


(* ::Subsubsection:: *)
(*OptionValue 4 args: function, options, option name specification, wrapper*)


Test[
	CheckedOptionValue[
		OptionValue[testFuncFailCheck, {"opt1" :> valid1}, opt1, wrapper],
		{testFunc, testFuncPassCheck}
	]
	,
	wrapper[valid1]
	,
	TestID -> "2 args list: function, options, option name, wrapper: \
(valid, valid)"
]
Test[
	CheckedOptionValue[
		OptionValue[testFuncPassCheck, {"opt1" -> invalid1}, opt1, wrapper],
		{testFuncFailCheck, testFunc}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncFailCheck, opt1, invalid1]
	]
	,
	Message[testFuncFailCheck::opt1, opt1, invalid1]
	,
	TestID -> "2 args list: function, options, option name, wrapper: \
(invalid, valid)"
]
Test[
	CheckedOptionValue[
		OptionValue[
			testFuncFailCheck, {opt2 -> valid2}, {opt1, "opt2"}, wrapper
		],
		{testFuncPassCheck, testFunc}
	]
	,
	{wrapper[defaultValid1], wrapper[valid2]}
	,
	TestID -> "2 args list: function, options, option names list, wrapper: \
(valid, valid), (valid, valid)"
]
Test[
	CheckedOptionValue[
		OptionValue[
			testFuncFailCheck, {opt1 -> valid1, opt2 :> invalid2},
			{"opt1", "opt2"}, wrapper
		],
		{testFuncPassCheck, testFunc}
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, "opt2", invalid2]]
	,
	Message[testFunc::opt2, "opt2", invalid2]
	,
	TestID -> "2 args list: function, options, option names list, wrapper: \
(valid, valid), (valid, invalid)"
]
Test[
	CheckedOptionValue[
		OptionValue[
			testFuncFailCheck, {opt1 :> {Sequence[0, 5]}}, {opt1, opt2}, wrapper
		],
		{testFuncHeldCheck, testFunc}
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, {Sequence[0, 5]}]]
	,
	Message[testFunc::opt1, "opt1", {0, 5}]
	,
	TestID -> "2 args list: function, options, option names list, wrapper: \
(valid, invalid), (invalid, valid)"
]
Test[
	CheckedOptionValue[
		OptionValue[
			testFuncFailCheck, {"opt2" -> invalid2}, {"opt1", opt2}, wrapper
		],
		{testFunc, testFuncHeldCheck}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncHeldCheck, "opt1", defaultValid1]
	]
	,
	Message[testFuncHeldCheck::opt1, "opt1", defaultValid1]
	,
	TestID -> "2 args list: function, options, option names list, wrapper: \
(valid, invalid), (invalid, invalid)"
]

Test[
	CheckedOptionValue[
		OptionValue[
			testFuncFailCheck, {opt2 -> valid2}, {opt1, "opt2"}, wrapper
		],
		{}
	]
	,
	{wrapper[defaultValid1], wrapper[valid2]}
	,
	TestID -> "2 args list empty: \
function, options, option names list, wrapper"
]


(* ::Subsubsection:: *)
(*OptionValue 4 args: default options, options, option name specification, wrapper*)


withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{opt1 -> defaultInvalid1}, {"opt1" :> valid1}, opt1, wrapper
		],
		{testFunc, testFuncPassCheck}
	]
	,
	wrapper[valid1]
	,
	TestID -> "2 args list: default options, options, option name, wrapper: \
(valid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultValid1}, {"opt1" -> invalid1}, opt1, wrapper
		],
		{testFuncFailCheck, testFunc}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncFailCheck, opt1, invalid1]
	]
	,
	Message[testFuncFailCheck::opt1, opt1, invalid1]
	,
	TestID -> "2 args list: default options, options, option name, wrapper: \
(invalid, invalid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{{opt1 :> defaultValid1}, opt2 -> defaultValid2}, {opt2 :> valid2},
			{opt1, "opt2"}, wrapper
		],
		{testFuncPassCheck, testFunc}
	]
	,
	{wrapper[defaultValid1], wrapper[valid2]}
	,
	TestID -> "2 args list: \
default options, options, option names list, wrapper: \
(valid, valid), (valid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultInvalid1, opt2 -> defaultInvalid2},
			{opt1 :> valid1}, {"opt1", "opt2"}, wrapper
		],
		{testFuncPassCheck, testFunc}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt2", defaultInvalid2]
	]
	,
	Message[testFunc::opt2, "opt2", defaultInvalid2]
	,
	TestID -> "2 args list: \
default options, options, option names list, wrapper: \
(valid, valid), (valid, invalid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultValid1, "opt2" :> defaultInvalid2},
			{"opt1" -> invalid1, opt2 -> valid2}, {opt1, opt2}, wrapper
		],
		{testFunc, testFuncPassCheck}
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, opt1, invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args list: \
default options, options, option names list, wrapper: \
(invalid, valid), (valid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultInvalid1, opt2 -> defaultInvalid2},
			{opt2 :> invalid2, opt1 :> invalid1}, {"opt1", opt2}, wrapper
		],
		{testFuncFailCheck, testFunc}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncFailCheck, "opt1", invalid1]
	]
	,
	Message[testFuncFailCheck::opt1, opt1, invalid1]
	,
	TestID -> "2 args list: \
default options, options, option names list, wrapper: \
(invalid, invalid), (invalid, valid)"
]

Test[
	CheckedOptionValue[
		OptionValue[
			{opt1 -> defaultInvalid1}, {"opt1" :> valid1}, opt1, wrapper
		],
		{}
	]
	,
	wrapper[valid1]
	,
	TestID -> "2 args list empty: \
default options, options, option name, wrapper"
]


(* ::Subsubsection:: *)
(*OptionValue 4 args: default options with symbol, options, option name specification, wrapper*)


withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{opt1 -> defaultInvalid1, testFuncFailCheck}, {"opt1" :> valid1},
			opt1, wrapper
		],
		{testFuncPassCheck, testFunc}
	]
	,
	wrapper[valid1]
	,
	TestID -> "2 args list: \
default options with symbol, options, option name, wrapper: (valid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{testFuncFailCheck, f, "opt1" -> defaultValid1},
			{"opt1" -> invalid1}, opt1, wrapper
		],
		{testFuncPassCheck, testFuncFailCheck}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncFailCheck, opt1, invalid1]
	]
	,
	Message[testFuncFailCheck::opt1, opt1, invalid1]
	,
	TestID -> "2 args list: \
default options with symbol, options, option name, wrapper: (valid, invalid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultValid1, {testFuncFailCheck}}, {opt2 :> valid2},
			{opt1, "opt2"}, wrapper
		],
		{testFunc, testFuncPassCheck}
	]
	,
	{wrapper[defaultValid1], wrapper[valid2]}
	,
	TestID -> "2 args list: \
default options with symbol, options, option names list, wrapper: \
(valid, valid), (valid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{
				{opt2 -> defaultInvalid2, testFuncFailCheck}, f,
				{{g}, "opt1" -> defaultInvalid1}
			},
			{opt1 :> valid1}, {"opt1", "opt2"}, wrapper
		],
		{testFunc, testFuncPassCheck}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt2", defaultInvalid2]
	]
	,
	Message[testFunc::opt2, "opt2", defaultInvalid2]
	,
	TestID -> "2 args list: \
default options with symbol, options, option names list, wrapper: \
(valid, valid), (invalid, valid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{
				testFuncFailCheck, f,
				"opt1" -> defaultValid1, "opt2" :> defaultInvalid2
			},
			{"opt1" -> invalid1, opt2 -> valid2}, {opt1, opt2}, wrapper
		],
		{testFuncPassCheck, testFuncFailCheck}
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncFailCheck, opt1, invalid1]
	]
	,
	Message[testFuncFailCheck::opt1, opt1, invalid1]
	,
	TestID -> "2 args list: \
default options with symbol, options, option names list, wrapper: \
(valid, invalid), (valid, invalid)"
]
withUnusedDefaults@Test[
	CheckedOptionValue[
		OptionValue[
			{
				"opt1" -> defaultInvalid1, opt2 -> defaultInvalid2,
				testFuncFailCheck
			},
			{opt2 :> invalid2, opt1 :> invalid1}, {"opt1", opt2}, wrapper
		],
		{testFunc, testFuncFailCheck}
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFunc, "opt1", invalid1]]
	,
	Message[testFunc::opt1, "opt1", invalid1]
	,
	TestID -> "2 args list: \
default options with symbol, options, option names list, wrapper: \
(invalid, invalid), (invalid, invalid)"
]

Test[
	CheckedOptionValue[
		OptionValue[
			{"opt1" -> defaultValid1, {testFuncFailCheck}}, {opt2 :> valid2},
			{opt1, "opt2"}, wrapper
		],
		{}
	]
	,
	{wrapper[defaultValid1], wrapper[valid2]}
	,
	TestID -> "2 args list empty: \
default options with symbol, options, option names list, wrapper"
]


(* ::Subsubsection:: *)
(*OptionsPattern - OptionValue 1 arg: option name specification*)


Test[
	Replace[testFuncFailCheck[opt2 -> invalid2],
		testFuncFailCheck@OptionsPattern[] :>
			CheckedOptionValue[
				OptionValue[opt1], {testFunc, testFuncPassCheck}
			]
	]
	,
	defaultValid1
	,
	TestID -> "2 args list: RHS: option name: (valid, valid)"
]
withUnusedDefaults@Test[
	Replace[testFuncPassCheck[],
		testFuncPassCheck@OptionsPattern[opt1 :> defaultInvalid1] :>
			CheckedOptionValue[
				OptionValue["opt1"], {testFuncFailCheck, testFunc}
			]
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncFailCheck, "opt1", defaultInvalid1]
	]
	,
	Message[testFuncFailCheck::opt1, opt1, defaultInvalid1]
	,
	TestID -> "2 args list: RHS: option name: (invalid, valid)"
]
Test[
	Replace[testFuncFailCheck["opt1" :> valid1],
		testFuncFailCheck@OptionsPattern[] :>
			CheckedOptionValue[
				OptionValue[{opt1, "opt2"}], {testFuncPassCheck, testFunc}
			]
	]
	,
	{valid1, defaultValid2}
	,
	TestID -> "2 args list: RHS: option names list: \
(valid, valid), (valid, valid)"
]
Test[
	Replace[testFuncFailCheck[],
		testFuncFailCheck@OptionsPattern[
			{{"opt2" -> defaultInvalid2, opt1 :> defaultValid1}}
		] :>
			CheckedOptionValue[
				OptionValue[{"opt1", "opt2"}], {testFuncPassCheck, testFunc}
			]
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFunc, "opt2", defaultInvalid2]
	]
	,
	Message[testFunc::opt2, "opt2", defaultInvalid2]
	,
	TestID -> "2 args list: RHS: option names list: \
(valid, valid), (valid, invalid)"
]
withUnusedDefaults@Test[
	Replace[testFuncFailCheck["opt2" :> {Sequence[11, 6]}, opt1 :> valid1],
		testFuncFailCheck@OptionsPattern[] :>
			CheckedOptionValue[
				OptionValue[{opt1, opt2}], {testFunc, testFuncHeldCheck}
			]
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncHeldCheck, opt1, valid1]
	]
	,
	Message[testFuncHeldCheck::opt1, "opt1", valid1]
	,
	TestID -> "2 args list: RHS: option names list: \
(valid, invalid), (invalid, valid)"
]
withUnusedDefaults@Test[
	Replace[testFuncFailCheck["opt2" :> invalid2],
		testFuncFailCheck@OptionsPattern[
			{f, "opt1" :> defaultInvalid1, opt2 -> defaultValid2}
		] :>
			CheckedOptionValue[
				OptionValue[{"opt2", opt1}], {testFuncHeldCheck, testFunc}
			]
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncHeldCheck, "opt2", invalid2]
	]
	,
	Message[testFuncHeldCheck::opt2, "opt2", invalid2]
	,
	TestID -> "2 args list: RHS: option names list: \
(valid, invalid), (invalid, invalid)"
]


(* ::Subsubsection:: *)
(*Sub-options*)


Test[
	CheckedOptionValue[
		OptionValue[
			opt :> subOptPart + subOptPart -> defaultSubSubValidB,
			{},
			opt :> subOptPart + subOptPart
		],
		{testFuncSubOpt, testFuncSubOptB}
	]
	,
	defaultSubSubValidB
	,
	TestID -> "2 args list: sub-options: \
default options, options, sub-option name: (not-checked, valid)"
]
Module[{wrapper},
	Test[
		CheckedOptionValue[
			OptionValue[testFuncSubOpt,
				opt :> subOptPart + subOptPart ->
					{defaultSubSubValidB, subSubOpt -> defaultSubSubSubValid},
				opt :> subOptPart + subOptPart,
				wrapper
			],
			{testFuncSubOpt, testFuncSubOptB}
		] // catchAll
		,
		HoldComplete[
			$Failed,
			InvalidOptionValue[testFuncSubOptB,
				opt :> subOptPart + subOptPart,
				{defaultSubSubValidB, subSubOpt -> defaultSubSubSubValid}
			]
		]
		,
		Message[testFuncSubOptB::subOpt,
			opt :> subOptPart + subOptPart,
			{defaultSubSubValidB, subSubOpt -> defaultSubSubSubValid}
		]
		,
		TestID -> "2 args list: sub-options: \
function, options, sub-option name, wrapper: \
({not-checked, valid}, {valid, invalid})"
	]
]


(* ::Subsubsection:: *)
(*OptionValue arguments from variables*)


withUnusedDefaults@Test[
	Replace[testFuncFailCheck[opt1 -> valid1],
		testFuncFailCheck@OptionsPattern[{opt2 -> defaultValid2, testFunc}] :>
			Module[{optList = {opt1, opt2}},
				CheckedOptionValue[
					OptionValue[optList], {testFuncPassCheck, testFunc}
				]
			]
	]
	,
	{valid1, defaultValid2}
	,
	TestID -> "2 args list: RHS: option names list from variable: \
(valid, valid), (valid, valid)"
]
Test[
	Module[{optList = {opt1, "opt2"}},
		CheckedOptionValue[
			OptionValue[
				{opt1 :> defaultValid1, opt2 -> defaultInvalid2},
				{"opt1" -> valid1}, optList
			],
			{testFunc, testFuncFailCheck}
		]
	] // catchAll
	,
	HoldComplete[
		$Failed,
		InvalidOptionValue[testFuncFailCheck, opt1, valid1]
	]
	,
	Message[testFuncFailCheck::opt1, opt1, valid1]
	,
	TestID -> "2 args list: \
default options, options, option names list from variable: \
(valid, invalid), (invalid, invalid)"
]


Test[
	Module[{args = Sequence[testFuncFailCheck, {"opt2", opt1}]},
		CheckedOptionValue[OptionValue[args], {testFuncPassCheck, testFunc}]
	]
	,
	{defaultValid2, defaultValid1}
	,
	TestID -> "2 args list: argument sequence from variable: \
(valid, valid), (valid, valid)"
]


(* ::Subsubsection:: *)
(*Count evaluations of option value*)


Module[{counter = 0},
	withUnusedDefaults@Test[
		CheckedOptionValue[
			OptionValue[testFunc, "opt1" :> (++counter), "opt1"],
			{testFuncIntCheck, testFuncIntCheckB}
		]
		,
		1
		,
		TestID -> "2 args list: Evaluation count: \
(non-holding, non-holding), non-holding: \
function, options, option name: (valid, valid): evaluation"
	];
	Test[
		counter
		,
		1
		,
		TestID -> "2 args list: Evaluation count: \
(non-holding, non-holding), non-holding: \
function, options, option name: (valid, valid): counter"
	]
]

Module[{counter1 = 0, counter2 = 0},
	withUnusedDefaults@Test[
		Replace[
			testFunc[opt2 :> (++counter2), opt1 :> (++counter1)],
			testFunc@OptionsPattern[] :>
				CheckedOptionValue[
					OptionValue[testFunc, Automatic, {opt1, opt2}],
					{testFuncHeldCheck, testFuncFailCheck}
				]
		] // catchAll
		,
		HoldComplete[
			$Failed,
			InvalidOptionValue[testFuncFailCheck, opt1, ++counter1]
		]
		,
		Message[testFuncFailCheck::opt1, opt1, 1]
		,
		TestID -> "2 args list: Evaluation count: \
(holding, non-holding), non-holding: \
RHS: function, Automatic, option names list, wrapper: \
(valid, invalid), (valid, invalid): evaluation"
	];
	Test[
		counter1
		,
		1
		,
		TestID -> "2 args list: Evaluation count: \
(holding, non-holding), non-holding: \
RHS: function, Automatic, option names list, wrapper: \
(valid, invalid), (valid, invalid): counter1"
	];
	Test[
		counter2
		,
		0
		,
		TestID -> "2 args list: Evaluation count: \
(holding, non-holding), non-holding: \
RHS: function, Automatic, option names list, wrapper: \
(valid, invalid), (valid, invalid): counter2"
	]
]

Module[{counter = 0},
	withUnusedDefaults@Test[
		CheckedOptionValue[
			OptionValue[{}, opt2 :> (++counter), "opt2", HoldComplete],
			{testFuncIntCheck, testFunc}
		] // catchAll
		,
		HoldComplete[$Failed, InvalidOptionValue[testFunc, "opt2", ++counter]]
		,
		Message[testFunc::opt2, "opt2", 1]
		,
		TestID -> "2 args list: Evaluation count: \
(non-holding, non-holding), holding: \
default options, options, option name, wrapper: (valid, invalid): evaluation"
	];
	Test[
		counter
		,
		1
		,
		TestID -> "2 args list: Evaluation count: \
(non-holding, non-holding), holding: \
default options, options, option name, wrapper: (valid, invalid): counter"
	]
]

Module[{counter1 = 0, counter2 = 0},
	withUnusedDefaults@Test[
		Replace[
			testFuncFailCheck[],
			testFuncFailCheck@OptionsPattern[] :>
				CheckedOptionValue[
					OptionValue[
						{opt1 :> (++counter1), opt2 :> defaultInvalid2},
						"opt2" :> (++counter2), {"opt1", "opt2"}, Hold
					],
					{testFuncIntCheck, testFuncHeldCheck}
				]
		]
		,
		{Hold[++counter1], Hold[++counter2]}
		,
		TestID -> "2 args list: Evaluation count: \
(non-holding, holding), holding: \
RHS: default options, options, option names list, wrapper: \
(valid, valid), (valid, valid): evaluation"
	];
	Test[
		counter1
		,
		1
		,
		TestID -> "2 args list: Evaluation count: \
(non-holding, holding), holding: \
RHS: default options, options, option names list, wrapper: \
(valid, valid), (valid, valid): counter1"
	];
	Test[
		counter2
		,
		1
		,
		TestID -> "2 args list: Evaluation count: \
(non-holding, holding), holding: \
RHS: default options, options, option names list, wrapper: \
(valid, valid), (valid, valid): counter2"
	]
]


(* ::Subsubsection:: *)
(*Custom CheckOption*)


Test[
	CheckedOptionValue[
		OptionValue["opt2" :> valid[2], opt2],
		{testFunc, testFuncB},
		CheckOption -> customCheckOption
	]
	,
	valid[2]
	,
	{
		Message[customCheckOption::used, testFunc, opt2, valid[2]],
		Message[customCheckOption::used, testFuncB, opt2, valid[2]]
	}
	,
	TestID -> "2 args list: Custom CheckOption: valid"
]
Test[
	CheckedOptionValue[
		OptionValue[
			{opt2 :> invalid[2], testFuncFailCheck}, "opt1" -> valid[1],
			{opt1, opt2}, wrapper
		],
		{testFuncB, testFunc},
		"CheckOption" -> customCheckOption
	] // catchAll
	,
	HoldComplete[$Failed, InvalidOptionValue[testFuncB, opt2, invalid[2]]]
	,
	{
		Message[customCheckOption::used, testFuncB, opt1, valid[1]],
		Message[customCheckOption::used, testFunc, opt1, valid[1]],
		Message[customCheckOption::used, testFuncB, opt2, invalid[2]]
	}
	,
	TestID -> "2 args list: Custom CheckOption: invalid"
]


(* ::Subsubsection:: *)
(*OptionValue 5 args*)


Test[
	CheckedOptionValue[
		OptionValue[{opt2 :> invalid2}, {}, "opt2", wrapper, fifth],
		{testFunc, testFuncFailCheck}
	] // Hold[#]&
	,
	OptionValue[{opt2 :> invalid2}, {}, "opt2", wrapper, fifth] // Hold
	,
	{
		Message[CheckedOptionValue::unknownOptionValueUsage,
			OptionValue[{opt2 :> invalid2}, {}, "opt2", wrapper, fifth]
		],
		Message[OptionValue::argb, OptionValue, 5, 1, 4]
	}
	,
	TestID -> "2 args list: 5 args"
]


(* ::Subsubsection:: *)
(*Non-OptionValue*)


Module[{nonOptionValue},
	Test[
		CheckedOptionValue[nonOptionValue, {testFunc, testFuncFailCheck}] //
			Hold[#]&
		,
		CheckedOptionValue[nonOptionValue, {testFunc, testFuncFailCheck}] //
			Hold
		,
		Message[CheckedOptionValue::optionValueHead,
			CheckedOptionValue[nonOptionValue, {testFunc, testFuncFailCheck}]
		]
		,
		TestID -> "2 args list: Non-OptionValue"
	]
]


(* ::Subsection:: *)
(*2 args second invalid*)


Module[{f, opt, unknownOpt, val},
	With[{unknownOptName = SymbolName[unknownOpt]},
		Test[
			CheckedOptionValue[OptionValue[f, opt], unknownOpt -> val] //
				Hold[#]&
			,
			CheckedOptionValue[OptionValue[f, opt], unknownOpt -> val] //
				Hold
			,
			Message[CheckOption::optnf, unknownOptName, CheckedOptionValue]
			,
			TestID -> "2 args second invalid: OptionValue, unknown option"
		]
	]
]
Module[{opt},
	Test[
		CheckedOptionValue[OptionValue[opt], {testFunc, "nonSymbol"}] //
			Hold[#]&
		,
		CheckedOptionValue[OptionValue[opt], {testFunc, "nonSymbol"}] // Hold
		,
		Message[CheckedOptionValue::symse,
			2,
			CheckedOptionValue[OptionValue[opt], {testFunc, "nonSymbol"}]
		]
		,
		TestID -> "2 args second invalid: OptionValue, list with non-symbol"
	]
]
Module[{nonOptionValue},
	Test[
		CheckedOptionValue[nonOptionValue, 10] // Hold[#]&
		,
		CheckedOptionValue[nonOptionValue, 10] // Hold
		,
		{
			Message[CheckedOptionValue::optionValueHead,
				CheckedOptionValue[nonOptionValue, 10]
			],
			Message[CheckedOptionValue::symse,
				2, CheckedOptionValue[nonOptionValue, 10]
			]
		}
		,
		TestID -> "2 args second invalid: non-OptionValue, integer"
	]
]


(* ::Subsection:: *)
(*3 args*)


Module[{f, opt, unknownOpt, val},
	With[{unknownOptName = SymbolName[unknownOpt]},
		Test[
			CheckedOptionValue[OptionValue[opt], f, unknownOpt -> val] //
				Hold[#]&
			,
			CheckedOptionValue[OptionValue[opt], f, unknownOpt -> val] //
				Hold
			,
			Message[CheckOption::optnf, unknownOptName, CheckedOptionValue]
			,
			TestID -> "3 args: OptionValue, symbol, unknown option"
		]
	]
]
Module[{f, g, opt, nonOption},
	Test[
		CheckedOptionValue[OptionValue[f, opt], {f, g}, nonOption] // Hold[#]&
		,
		CheckedOptionValue[OptionValue[f, opt], {f, g}, nonOption] // Hold
		,
		Message[CheckedOptionValue::nonopt,
			nonOption, 2,
			CheckedOptionValue[OptionValue[f, opt], {f, g}, nonOption]
		]
		,
		TestID -> "3 args: OptionValue, list of symbols, non-option"
	]
]
Module[{nonOptionValue, nonOption},
	Test[
		CheckedOptionValue[nonOptionValue, "str", nonOption] // Hold[#]&
		,
		CheckedOptionValue[nonOptionValue, "str", nonOption] // Hold
		,
		{
			Message[CheckedOptionValue::optionValueHead,
				CheckedOptionValue[nonOptionValue, "str", nonOption]
			],
			Message[CheckedOptionValue::symse,
				2, CheckedOptionValue[nonOptionValue, "str", nonOption]
			],
			Message[CheckedOptionValue::nonopt,
				nonOption, 2,
				CheckedOptionValue[nonOptionValue, "str", nonOption]
			]
		}
		,
		TestID -> "3 args: non-OptionValue, string, non-option"
	]
]


(* ::Subsection:: *)
(*4 args*)


Module[{arg1, arg2, arg3, arg4},
	Test[
		CheckedOptionValue[arg1, arg2, arg3, arg4] // Hold[#]&
		,
		CheckedOptionValue[arg1, arg2, arg3, arg4] // Hold
		,
		{
			Message[CheckedOptionValue::optionValueHead,
				CheckedOptionValue[arg1, arg2, arg3, arg4]
			],
			Message[CheckedOptionValue::nonopt,
				arg4, 2, CheckedOptionValue[arg1, arg2, arg3, arg4]
			]
		}
		,
		TestID -> "4 args: non-OptionValue, symbol, 2 non-options"
	]
]
Module[{arg3, opt1, opt2, opt4, val1, val2, val4},
	Test[
		CheckedOptionValue[opt1 -> val1, opt2 :> val2, arg3, opt4 -> val4] //
			Hold[#]&
		,
		CheckedOptionValue[opt1 -> val1, opt2 :> val2, arg3, opt4 -> val4] //
			Hold
		,
		{
			Message[CheckedOptionValue::optionValueHead,
				CheckedOptionValue[
					opt1 -> val1, opt2 :> val2, arg3, opt4 -> val4
				]
			],
			Message[CheckedOptionValue::nonopt,
				arg3, 2,
				CheckedOptionValue[
					opt1 -> val1, opt2 :> val2, arg3, opt4 -> val4
				]
			]
		}
		,
		TestID -> "4 args: \
non-OptionValue, unknown option, non-option, unknown option"
	]
]


(* ::Section:: *)
(*TearDown*)


On[General::stop]


Unprotect["`*", "`*`*"]
Quiet[Remove["`*", "`*`*"], {Remove::rmnsm}]


EndPackage[]
$ContextPath = Rest[$ContextPath]
