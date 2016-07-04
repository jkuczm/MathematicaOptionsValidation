(* ::Package:: *)

(* OptionsValidation package

Copyright (c) 2016 Jakub Kuczmarski Jakub.Kuczmarski@gmail.com
Released under The MIT License
https://github.com/jkuczm/MathematicaOptionsValidation/blob/master/LICENSE *)


(*	Begining of package context with ` is intentionall it allows loading
	OptionsValidation as a sub-package. Apropriate steps to load package with
	OptionsValidation` as "top level" context are taken in Kernel/init.m.
	This is inspired by system used in https://github.com/szhorvat/LTemplate *)
BeginPackage["`OptionsValidation`"]


Unprotect["`*"]
ClearAll["`*"]


(* ::Section:: *)
(*Messages*)


(* ::Subsection:: *)
(*Public*)


(* ::Subsubsection:: *)
(*Usage*)


CheckOption::usage =
"\
CheckOption[f, name][value] \
checks validity, of given option value, as option of f symbol, with given \
name. Returns True if option is valid, otherwise returns expression different \
than True. This function can have arbitrary user defined side effects. \
By default all options that have same names as options present in Options[f] \
are considered valid. Any other option is, by default, considered invalid and \
when passed to CheckOption, CheckOption::optnf message will be emited and \
False returned.\

CheckOption[f, namePatt][valuePatt] := code \
can be used to define tests for options of symbol f, with names matching \
namePatt and values matching valuePatt. namePatt will be automatically \
conveted to pattern matching stringified option names. If it's to complicated \
for conversion, then a warning will be issued. code can be arbitrary, if it \
returns True - option is considered valid, otherwise - it's considered \
invalid.\

CheckOption[fName : f | g | ..., namePatt][valuePatt] := code \
defines tests for all given symbols f, g, ...\

CheckOption[fPatt, namePatt] = valueTestFunc \
assigns valueTestFunc as function testing value of option matching namePatt. \
It'll be called as valueTestFunc[optionValue], if it returns True - option is \
considered valid, otherwise - it's considered invalid.\

CheckOption[fPatt, namePatt][valuePatt] =. or CheckOption[fPatt, namePatt] =. \
can be used to remove already defined tests.\

CheckOption \
is an option for ValidOptionsQ, ValidOptionsPattern, InvalidOptionsPattern, \
QuietValidOptionsPattern, SetDefaultOptionsValidation, CheckedOptionValue, \
and WithOptionValueChecks that can be set to any value. Value of this option \
will be called with: [f, name][value] arguments to check value of f's option \
with given name. Default value of CheckOption option is CheckOption."


CheckOptionRelations::usage =
"\
CheckOptionRelations[f][optsList] \
checks validity of values of options, given in optsList, in relation to each \
other. Returns True if options are valid, otherwise returns expression \
different than True. This function can have arbitrary user defined side \
effects.\

CheckOptionRelations[f] = relationsTestFunc \
can be used to define tests for relations of options of symbol f. \
relationsTestFunc  can be arbitrary function accepting list of options as \
argument, if it returns True - options are considered valid, otherwise - \
they're considered invalid.\

CheckOptionRelations[fName : f | g | ...] := relationsTestFunc \
defines tests for all given symbols f, g, ...\

CheckOptionRelations[fPatt] =. \
can be used to remove already defined tests.\

CheckOptionRelations \
is an option for ValidOptionsQ, ValidOptionsPattern, InvalidOptionsPattern, \
QuietValidOptionsPattern, and SetDefaultOptionsValidation that can be set to \
any value. Value of this option will be called with: [f][optsList] arguments \
to check relations between values of f's option from given list optsList. \
Default value of CheckOptionRelations option is CheckOptionRelations."


ValidOptionsQ::usage =
"\
ValidOptionsQ[f][opt1 -> val1, opt2 -> val2, ...] \
returns True if values of given options pass CheckOption tests defined for \
symbol f, otherwise returns False.\

ValidOptionsQ[{f, g, ...}][opt1 -> val1, opt2 -> val2, ...] \
tests validity of options filtered for f, g, ..., using tests defined for \
f, g, ...  respectively."


ValidOptionsPattern::usage =
"\
ValidOptionsPattern[f] \
returns a pattern object that represents a collection of options given as \
rules, where the values of the options can be accessed using OptionValue, \
with default options values taken from Options[f]. All rules are tested as \
valid options of f symbol, if they're not valid - pattern does not match.\

ValidOptionsPattern[{f, g, ...}] \
tests validity of options filtered for f, g, ..., using tests defined for \
f, g, ... respectively. If pattern matches, OptionValue takes default option \
values from {Options[f], Options[g], ...}.\

ValidOptionsPattern[defaultsSpec] \
where defaultsSpec is arbitrary nested list of options and symbols. Tests \
validity of options using tests defined for each symbolic specification in \
defaultsSpec. If pattern matches, OptionValue takes default option values \
from options given explicitly in defaultsSpec and from Options of each \
symbolic specification.\

ValidOptionsPattern[defaultsSpec, f] \
takes default option values from defaultsSpec, performs only tests defined \
for f.\

ValidOptionsPattern[defaultsSpec, {f, g, ...}] \
takes default option values from defaultsSpec, performs tests defined for \
f, g, ... of options filtered for f, g, ... respectively."


QuietValidOptionsPattern::usage =
"\
QuietValidOptionsPattern[f] \
returns a pattern object that represents a collection of options given as \
rules, where the values of the options can be accessed using OptionValue, \
with default options values taken from Options[f]. Rules are tested as valid \
options of f symbol, when first invalid option is found tests stop and \
pattern does not match. Tests are evaluated inside Quiet, so any error \
messages informing about incorrect options are suppressed. \

QuietValidOptionsPattern[{f, g, ...}] \
tests validity of options filtered for f, g, ..., using tests defined for \
f, g, ...  respectively. If pattern matches, OptionValue takes default option \
values from {Options[f], Options[g], ...}.\

QuietValidOptionsPattern[defaultsSpec] \
where defaultsSpec is arbitrary nested list of options and symbols. Tests \
validity of options using tests defined for each symbolic specification in \
defaultsSpec. If pattern matches, OptionValue takes default option values \
from options given explicitly in defaultsSpec and from Options of each \
symbolic specification.\

QuietValidOptionsPattern[defaultsSpec, f] \
takes default option values from defaultsSpec, performs only tests defined \
for f.\

QuietValidOptionsPattern[defaultsSpec, {f, g, ...}] \
takes default option values from defaultsSpec, performs tests defined for \
f, g, ... of options filtered for f, g, ... respectively.\

QuietValidOptionsPattern[defaultsSpec, messages] or \
QuietValidOptionsPattern[defaultsSpec, fSpec, messages] \
Quiets only given messages."


InvalidOptionsPattern::usage =
"\
InvalidOptionsPattern[f] \
returns a pattern object that represents a collection of options given as \
rules, where the values of the options can be accessed using OptionValue, \
with default options values taken from Options[f]. All rules are tested as \
valid options of f symbol, pattern matches if at least one option value is \
invalid.\

InvalidOptionsPattern[{f, g, ...}] \
tests validity of options filtered for f, g, ..., using tests defined for \
f, g, ...  respectively. If pattern matches, OptionValue takes default option \
values from {Options[f], Options[g], ...}.\

InvalidOptionsPattern[defaultsSpec] \
where defaultsSpec is arbitrary nested list of options and symbols. Tests \
validity of options using tests defined for each symbolic specification in \
defaultsSpec. If pattern matches, OptionValue takes default option values \
from options given explicitly in defaultsSpec and from Options of each \
symbolic specification.\

InvalidOptionsPattern[defaultsSpec, f] \
takes default option values from defaultsSpec, performs only tests defined \
for f.\

InvalidOptionsPattern[defaultsSpec, {f, g, ...}] \
takes default option values from defaultsSpec, performs tests defined for \
f, g, ... of options filtered for f, g, ... respectively."


SetDefaultOptionsValidation::usage =
"\
SetDefaultOptionsValidation[f] \
assigns UpValues to f symbol ensuring that SetOptions function will test \
validity of given options. Returns f.\

SetDefaultOptionsValidation[{f, g, ...}] \
assigns UpValues to all f, g, ... symbols. Returns List of results of \
individual assignments."


CheckedOptionValue::usage =
"\
CheckedOptionValue[OptionValue[...], f] \
if value of option returned by OptionValue call passes CheckOption test \
defined for symbol f, then this value is returned. Otherwise $Failed is \
thrown tagged with InvalidOptionValue[f, name, value], where name is name of \
option. If OptionValue call extracts list of options, then first invalid \
option value triggers throwing, if all option values are valid their list is \
returned. \

CheckedOptionValue[OptionValue[...], {f1, f2, ...}] \
tests option value(s) using CheckOption tests defined for symbols f1, f2, ...\

CheckedOptionValue[OptionValue[...]] \
extracts symbols, used as owners of CheckOption tests, from specification of \
default option values. Default option values specification is an arbitrarily \
nested list of options and symbols. It can be explicitly given as first \
argument of OptionValue call, or, if OptionValue call is on right hand side \
of rule or assignmet with OptionsPattern on left hand side, implicit \
specification can be used. All symbolic default option value specifications \
are used as owners of CheckOption tests."


WithOptionValueChecks::usage =
"\
WithOptionValueChecks[code, f] \
evaluates given code with each OptionValue call, that is present in code, \
accompanied by CheckOption test of option value, defined for symbol f. \
If CheckOption test fails evaluation of code stops and $Failed is returned. \
If all CheckOption tests pass, result of code evaluation is returned.\

WithOptionValueChecks[code, {f1, f2, ...}] \
tests option value(s) using CheckOption tests defined for symbols f1, f2, ...\

WithOptionValueChecks[code] \
automatically infers symbols, used as owners of CheckOption tests, from \
specification of default option values in each OptionValue call. Default \
option values specification is an arbitrarily nested list of options and \
symbols. It can be explicitly given as first argument of OptionValue call, \
or, if OptionValue call is on right hand side of rule or assignmet with \
OptionsPattern on left hand side, implicit specification can be used. All \
symbolic default option value specifications are used as owners of \
CheckOption tests."


ChecksStopOnInvalid::usage =
"\
ChecksStopOnInvalid \
is an option for ValidOptionsQ, ValidOptionsPattern, InvalidOptionsPattern, \
QuietValidOptionsPattern, and SetDefaultOptionsValidation that can be set to \
True or False.\

If set to True, option value checks will stop on first invalid value.\

If set to False, all options will be checked."


CheckedDuplicate::usage =
"\
CheckedDuplicate \
is an option for ValidOptionsQ, ValidOptionsPattern, InvalidOptionsPattern, \
QuietValidOptionsPattern, and SetDefaultOptionsValidation that can be set to \
First, Last or All.\

If set to First, then, when given several options with same name, only first \
of them will be tested.\

If set to Last, only last of duplicate options will be tested.\

If set to All, all duplicates will be tested."


InvalidOptionHandler::usage =
"\
InvalidOptionHandler \
is an option for WithOptionValueChecks that can be set to any value. Value of \
this option, called with: [f, name, value] arguments, will be returned from \
WithOptionValueChecks expression when option with given name and value will \
fail test defined for function f. By default invalid option handler returns \
$Failed without evaluation of passed to it arguments."


InvalidOptionValue::usage =
"\
InvalidOptionValue[f, name, val] \
is used as throw tag by CheckedOptionValue function, when value val of option \
with given name doesn't pass test defined for function f."


(* ::Subsubsection:: *)
(*Warnings and Errors*)


CheckOption::optnf = Options::optnf


CheckOption::unknownOptionNamePattern =
"\
Pattern for second argument of CheckOption should match stringified option \
names, it'll then take advantage of automatic conversion of symbolic option \
and sub-option names to strings, built into CheckOption. Some patterns can be \
automatically verified or converted to appropriate forms, `1` is not one of \
them.\

If you're sure that `1` matches stringified names of appropriate options you \
can ignore this warning."


CheckOption::unknownOptionOwnerPattern =
CheckOptionRelations::unknownOptionOwnerPattern =
"\
Pattern for first argument of `1` should be a symbol or Alternatives \
of symbols, so that option checks can be attached to symbols owning relevant \
options. \"Option owners\" can't be extracted from `2` pattern.\

Try using `3` with explicit \"option owner\"."


CheckOption::unprotected = CheckOptionRelations::unprotected =
"\
`1` symbol is currently not Protected, and ordinary assignment: `2`, \
without any modifications, occurred. If this was intentional you can ignore \
this warning."


ValidOptionsQ::optfla = ValidOptionsPattern::optfla =
QuietValidOptionsPattern::optfla = InvalidOptionsPattern::optfla =
"\
Value of option `1` -> `2` should be First, Last or All."


ValidOptionsPattern::unknownOptionOwner =
QuietValidOptionsPattern::unknownOptionOwner =
InvalidOptionsPattern::unknownOptionOwner =
CheckedOptionValue::unknownOptionOwner =
"\
List of symbols extracted from default option values specification `1` is \
empty, so no validity tests will be performed. If that's your intention you \
can ignore this warning. If you want validity tests to be performed, use \
default value specification containing symbols, or use second argument with \
explicit symbol or list of symbols for which tests are defined."


CheckedOptionValue::unknownOptionValueUsage =
"\
OptionValue calls with between 1 and 4 arguments are supported, `1` is not \
one of them, no option value checks were performed."


CheckedOptionValue::optionValueHead =
"\
Expression with OptionValue head expected at position 1 in `1`."


SetDefaultOptionsValidation::currentInvalid =
"\
Validation can be set only for functions with valid current default option \
values, which is not true for `1` function."


CheckOption::subargx = CheckOptionRelations::subargx =
"\
`1` expression called with `2` sub-arguments; 1 sub-argument is expected."


ValidOptionsQ::symse = ValidOptionsPattern::symse =
QuietValidOptionsPattern::symse = InvalidOptionsPattern::symse =
SetDefaultOptionsValidation::symse = CheckedOptionValue::symse =
WithOptionValueChecks::symse =
"\
Symbol or list of symbols expected at position `1` in `2`."


QuietValidOptionsPattern::anmlist = Quiet::anmlist


(* ::Subsection:: *)
(*Private*)


Begin["`Private`"]


ClearAll["`*"]


symbolPatt::usage =
"\
symbolPatt \
is a pattern matching symbols, but not other expressions with head Symbol."


nonSymbolPatt::usage =
"\
nonSymbolPatt \
is a pattern matching anything except symbol."


msgSpecPatt::usage =
"\
msgSpecPatt \
is a pattern matching valid message specification."


optPatt::usage =
"\
optPatt \
is a pattern matching option that can be used inside Except in all suported \
Mathematica versions."


optionName::usage =
"\
optionName[opt -> val] or optionName[opt :> val] \
if opt is a symbol returns string with its name, otherwise returns unchanged \
opt."


setToTagSet::usage =
"\
setToTagSet[setFunc] \
returns \"tag set\" variant of given ...Set function."


hold::usage =
"\
hold[expr1, expr2, ...] \
maintains expri in an unevaluated form. It is used internally by the package \
to temporary hold expressions without interfering with builtin holding \
wrappers."


holdDeepRelease::usage =
"\
holdDeepRelease[expr] \
returns expr with all hold wrappers removed."


normalizeOptionName::usage =
"\
normalizeOptionName[optNameSpec] \
returns normalized version of given option name specification. Symbolic names \
are converted to strings, rules representing sub-options are converted to \
delayed rules.\

normalizeOptionName[expr, mode] \
is used as parser handling option name specification normalization. mode can \
be \"Full\" which stringifies symbols and parses elements of rules, or \
\"LHS\" which only stringifies symbol, \"LHS\" mode is used on left hand \
sides of rules."


normalizeOptionNamePattern::usage =
"\
normalizeOptionNamePattern[patt] \
returns pattern that will match normalized name specifications of options \
matching given pattern patt. If given pattern can't be normalized \
CheckOption::unknownOptionNamePattern warning is given. For each rule present \
in patt, a list of patterns matching subsequent ancestor options is sown \
tagged with parentOptionTag.\

normalizeOptionNamePattern[patt, {HoldPattern[x1], HoldPattern[x2], ...}] \
is used as pattern parser on right hand sides of nested rules with x1, x2 on \
left hand sides."


$checkOptionNames::usage =
"\
$checkOptionNames \
if set to True and ther's no custom CheckOption definition matching option \
with given name for given function then CheckOption will test whether option \
with given name is among default options of given function, if so option will \
be considered valid, if it's not among defaults it'll be considered invalid. \
If $checkOptionNames is set to something else than True, no automatic option \
name tests will be performed."


parentOptionTag::usage =
"\
parentOptionTag \
is used as sow tag by normalizeOptionNamePattern function, when rule is \
encountered during parsing and parent options are sown."


$defineParentOptionChecks::usage =
"\
$defineParentOptionChecks \
if set to True parent options checks will be automatically defined when \
defining checks for sub-options, otherwise parent options checks will not be \
defined automatically."


defineParentOptionCheck::usage =
"\
defineParentOptionCheck[f][parentOptPattList] \
defines check for parent option with name composed of sub-options matching \
successive elements of given list of patterns: parentOptPattList. Deined \
check is attached to f symbol."


protectedQ::usage =
"\
protectedQ[sym] \
rerns True if given symbol is protected, otherwise returns False."


useOverriddenQ::usage =
"\
useOverriddenQ[tag, func] \
is used as flag determining whether definitions of func overridden for given \
tag should be used."


tagSetProtected::usage =
"\
tagSetProtected[... = rhs, func][arg1, arg2, ...] \
if func is Protected, sets func[arg1, arg2, ...] = rhs definition(s) as \
up-value(s) of symbol(s) extracted from arg1. If no suitable symbol can be \
extracted func::unknownOptionOwnerPattern error is given and $Failed is \
returned. If func is not Protected, gives func::unprotected warning and \
evaluates given assignment.\

tagSetProtected[... := rhs, func][arg1, arg2, ...] \
sets delayed definition(s).\

tagSetProtected[... =., func][arg1, arg2, ...] \
unsets definition(s)."


allTrue::usage =
"\
allTrue[failFast][{e1, e2, ...}, testQ] \
returns True if testQ[ei] is True for all ei and False otherwise. If failFast \
is False all ei are tested if it's True tests stop on first False returned by \
testQ[ei]. Given testQ function should always return True or False."


chooseOptionDuplicates::usage =
"\
chooseOptionDuplicates[choiceType][optList] \
returns List of given options with options, from groups of options with \
same name, chossen according to given choiceType. For choiceType All - all \
options are returned, for First - only first option with each name is \
returned, for Last - only last option with each name is returned. optList \
should be a flat list of options."


generateValidator::usage =
"\
generateValidator[Symbol][stopOnInv, duplicate, check, checkRel][f, opts] \
returns True if options, given in flat list opts, pass check[f, name][val] \
and checkRel[f][opts] tests, where name and val are name and value of options \
from opts, otherwise returns False. If stopOnInv is True tests stop in first \
invalid option, if it's False all options are tested. Duplicated options are \
tested depending on duplicate value, for All all duplicates are tested, for \
First, or Last ,only first, or last, option, from each group of options with \
same names, is tested. If checkRel is None, checkRel[f][opts] test is not \
performed.\

generateValidator[List][stopOnInv, duplicate, check, checkRel]\
[{f, g, ...}, opts] \
tests validity of options filtered for f, g, ..., using tests defined for \
f, g, ...  respectively."


packageOptionsValidator
"\
packageOptionsValidator[f, opts] \
returns True if values of options, given in flat List opts, pass CheckOption \
tests defined for symbol f, otherwise returns False. It doesn't stop on \
invalid options. It doesn't test relations between options, if options with \
duplicate names are given, only value of first option from each duplicates \
group is tested.\

packageOptionsValidator[{f, g, ...}, opts] \
tests validity of options filtered for f, g, ..., using tests defined for \
f, g, ...  respectively."


checkRule::usage =
"\
checkRule[f][nonRule] \
for nonRule being anything except Rule or RuleDelayed with two arguments \
f::rep message is issued, otherwise remains unevaluated."


generateOptionsPattern::usage =
"\
generateOptionsPattern[type, wrapper, defaultSpec, optOwners, validatorOpts] \
returns OptionsPattern[defaultSpec] expression with a condition. Condition \
tests validity of options, matching OptionsPattern, using validator generated \
for given option owners optOwners, with given validatorOpts. Call to \
validator is wrapped with given wrapper. Messages signaling problems in \
pattern generation will be attached to type symbol. If optOwners is Automatic \
option owners are extracted from defaultSpec."


getAttributes::usage =
"\
getAttributes[expr] \
if expr is a symbol or Function expression returns list of its attributes, \
otherwise returns empty list."


holdsFirstQ::usage =
"\
holdsFirstQ[expr] \
returns True if expr is a symbol or Function expression that holds its first \
argument, otherwise returns False."


wrapOrThrow::usage =
"\
wrapOrThrow[check, {f1, f2, ...}, name, wrapper][val] \
if given option value val passes check[fi, name][val] tests for each fi, then \
this value is returned, wrapped with given wrapper. Otherwise $Failed is \
thrown tagged with InvalidOptionValue[fi, name, value]."


internalCheckedOptionValue::usage =
"\
internalCheckedOptionValue[check, f, defaultSpec, opts, name, wrapper] \
if value of option with given name, extracted from given opts, with default \
values taken from given defaultSpec, pass check[f, name][value] test, then \
this value is returned wrapped with given wrapper. Otherwise $Failed is \
thrown tagged with InvalidOptionValue[f, name, value].\

internalCheckedOptionValue[\
check, f, defaultSpec, opts, {name1, ...}, wrapper\
] \
performs checks of all options with given names, throws for first invalid \
option value. If all option values are valid, returns list with each option \
value wrapped with given wrapper.\

internalCheckedOptionValue[\
check, {f1, ...}, defaultSpec, opts, nameSpec, wrapper\
] \
uses check[fi, name] tests for all symbols f1, ..., throws for first failed \
test.\

internalCheckedOptionValue[\
check, Automatic, defaultSpec, opts, nameSpec, wrapper\
] \
uses check[fi, name] tests for all symbols fi from flattened defaultSpec list.\

internalCheckedOptionValue[check, fSpec, defaultSpec, opts, nameSpec] \
uses Identity as wrapper.\

internalCheckedOptionValue[check, fSpec, defaultSpec, nameSpec] \
extracts option values only from defaultSpec."


setSymseOpts::usage =
"\
setSymseOpts[f] \
assigns, to given symbol f, definitions reporting incorrect arguments and \
SyntaxInformation ArgumentsPattern for function accepting one mandatory \
argument, which can be a symbol or list of symbols, and options."


setMandatorySymseOpts::usage =
"\
setMandatorySymseOpts[f] \
assigns, to given symbol f, definitions reporting incorrect arguments and \
SyntaxInformation ArgumentsPattern for function accepting one mandatory \
argument, second optional argument which can be a symbol or list of symbols, \
and options."


(* ::Section:: *)
(*Implementation*)


(* ::Subsection:: *)
(*Private*)


(* ::Subsubsection:: *)
(*symbolPatt*)


symbolPatt = Except[HoldPattern@Symbol[___], _Symbol]


(* ::Subsubsection:: *)
(*nonSymbolPatt*)


nonSymbolPatt = Except[_Symbol] | HoldPattern@Symbol[___]


(* ::Subsubsection:: *)
(*msgSpecPatt*)


msgSpecPatt =
	_MessageName | _String | {(_MessageName | _String)..} | All | None


(* ::Subsubsection:: *)
(*optPatt*)


(* OptionsPattern[] can't be used inside Except before v10.1. *)
optPatt =
	Module[{nonOption},
		If[Quiet[MatchQ[nonOption, Except@OptionsPattern[]], Except::named],
			OptionsPattern[]
		(* else *),
			_?OptionQ
		]
	]


(* ::Subsubsection:: *)
(*optionName*)


optionName[(Rule | RuleDelayed)[sym_Symbol, _]] := SymbolName[sym]

optionName[(Rule | RuleDelayed)[opt_, _]] := opt


(* ::Subsubsection:: *)
(*setToTagSet*)


setToTagSet[Set] = TagSet

setToTagSet[SetDelayed] = TagSetDelayed

setToTagSet[Unset] = TagUnset


(* ::Subsubsection:: *)
(*hold*)


SetAttributes[hold, HoldAll]


(* ::Subsubsection:: *)
(*holdDeepRelease*)


holdDeepRelease = DeleteCases[#, hold, {0, Infinity}, Heads -> True]&


(* ::Subsubsection:: *)
(*normalizeOptionName*)


normalizeOptionName[optNameSpec_] :=
	holdDeepRelease@normalizeOptionName[optNameSpec, "Full"]


normalizeOptionName[sym : symbolPatt, _] := SymbolName@Unevaluated[sym]

normalizeOptionName[(Rule | RuleDelayed)[lhs_, rhs___], "Full"] :=
	RuleDelayed[
		normalizeOptionName[lhs, "LHS"],
		Evaluate@normalizeOptionName[rhs, "Full"]
	]

normalizeOptionName[expr_, _] := hold[expr]


(*	Setting Hold... attribute after definitions to let patterns, used in
	definitions, evaluate. *)
SetAttributes[normalizeOptionName, HoldFirst]


(* ::Subsubsection:: *)
(*normalizeOptionNamePattern*)


normalizeOptionNamePattern[patt_] :=
	holdDeepRelease@normalizeOptionNamePattern[patt, {}]


normalizeOptionNamePattern[Verbatim[_], _] = _

normalizeOptionNamePattern[str_String, _] := str

normalizeOptionNamePattern[sym : symbolPatt, _] := SymbolName@Unevaluated[sym]

normalizeOptionNamePattern[(Rule | RuleDelayed)[lhs_, rhs_], parentOpts_] :=
	With[{lhsNormalized = normalizeOptionNamePattern[lhs, parentOpts]},
		(* Normalized pattern always uses RuleDelayed. *)
		hold[RuleDelayed][
			lhsNormalized,
			normalizeOptionNamePattern[
				rhs
				,
				Sow[
					Prepend[parentOpts,
						HoldPattern[lhsNormalized] // holdDeepRelease
					],
					parentOptionTag
				]
			]
		]
	]

normalizeOptionNamePattern[patt : _Alternatives | _HoldPattern, parentOpts_] :=
	Operate[hold,
		Function[Null, normalizeOptionNamePattern[#, parentOpts], HoldFirst] /@
			Unevaluated[patt]
	]

normalizeOptionNamePattern[Verbatim[Pattern][name_, patt_], parentOpts_] :=
	hold[Pattern][
		name,
		normalizeOptionNamePattern[patt, parentOpts]
	]

normalizeOptionNamePattern[unknownPatt_, _] := (
	Message[CheckOption::unknownOptionNamePattern, HoldForm[unknownPatt]];
	hold[unknownPatt]
)


SetAttributes[normalizeOptionNamePattern, HoldFirst]


(* ::Subsubsection:: *)
(*$checkOptionNames*)


$checkOptionNames = True


(* ::Subsubsection:: *)
(*$defineParentOptionChecks*)


$defineParentOptionChecks = True


(* ::Subsubsection:: *)
(*defineParentOptionCheck*)


defineParentOptionCheck = Function[f, Function[parentOptPattList,
	With[
		{
			names = Table[Unique["subName$"], {Length[parentOptPattList]}],
			lhsName = Unique["lhs$"]
		},
	With[
		{
			lhsPatt = Pattern[lhsName, _],
			fullSubOptName = Fold[RuleDelayed[#2, #1]&, lhsName, names],
			parentOptNamedPattList =
				MapThread[Pattern, {names, parentOptPattList}]
		},
		Internal`InheritedBlock[{useOverriddenQ},
			useOverriddenQ[CheckOption, TagSetDelayed] = False;
			f /: CheckOption[f,
				Fold[RuleDelayed[#2, #1]&,
					(*	Supporting versions older than 10.1, so can't use new
						2-arg version of Fold. *)
					First[parentOptNamedPattList], Rest[parentOptNamedPattList]
				],
				subOpts_
			] :=
				Module[{notCheckedQ},
					notCheckedQ[x : symbolPatt] :=
						notCheckedQ@Evaluate@SymbolName@Unevaluated[x];
					call : notCheckedQ[_] := (call = False; True);
					SetAttributes[notCheckedQ, HoldAll];
					
					(*	Option name chacks are performed for parent option only
						no need to repeate them on sub options. *)
					Block[{$checkOptionNames = False},
						And @@ Cases[Flatten[Hold[subOpts], Infinity, List],
							(Rule | RuleDelayed)[lhsPatt?notCheckedQ, rhs_] :>
								CheckOption[f, fullSubOptName]@Unevaluated[rhs]
						]
					]
				]
		]
	]
	]
]]


(* ::Subsubsection:: *)
(*protectedQ*)


protectedQ = MemberQ[Attributes[#], Protected]&


(* ::Subsubsection:: *)
(*useOverriddenQ*)


useOverriddenQ[_, _] = True


(* ::Subsubsection:: *)
(*tagSetProtected*)


SetAttributes[tagSetProtected, HoldFirst]


tagSetProtected[set_[_, Repeated[rhs_, {0, 1}]], func_?protectedQ][
	fPatt : f_Symbol | Verbatim[Pattern][_, f_Symbol],
	rest___
] :=
	setToTagSet[set][f, func[fPatt, rest], rhs]

tagSetProtected[assignment_, func_?protectedQ][
	fPatt : Alternatives[
		Verbatim[Alternatives][__Symbol],
		Verbatim[Pattern][_, Verbatim[Alternatives][__Symbol]]
	],
	rest___
] :=
	Replace[
		tagSetProtected[assignment, func][#, rest]& /@
			List @@ Thread[fPatt, Alternatives],
		{(expr_)..} :> expr
	]

tagSetProtected[set_[__], func_?protectedQ][
	unknownPatt_, ___
] := (
	Message[func::unknownOptionOwnerPattern,
		HoldForm[func],
		HoldForm[unknownPatt],
		HoldForm@Evaluate@setToTagSet[set]
	];
	$Failed
)

tagSetProtected[assignment_, func_][___] := (
	Message[func::unprotected, HoldForm[func], HoldForm[assignment]];
	assignment
)


(* ::Subsubsection:: *)
(*allTrue*)


allTrue[True] =
	If[Context[AllTrue] === "System`",
		AllTrue
	(* else *),
		(*	This is not an exact backport, it behaves differently when result
			of test is non-boolean, but we'll use it only for functions
			returning explicit True or False. *)
		Function[{list, testQ},
			Scan[
				Function[Null,
					If[!testQ[#], Return[False, CompoundExpression]],
					HoldAllComplete
				],
				Unevaluated[list]
			];
			True
		]
	]

allTrue[False] = Function[{list, testQ}, And @@ Map[testQ, Unevaluated[list]]]


(* ::Subsubsection:: *)
(*chooseOptionDuplicates*)


chooseOptionDuplicates[All] = Identity

chooseOptionDuplicates[First] = GatherBy[#, optionName][[All, 1]]&

chooseOptionDuplicates[Last] = GatherBy[#, optionName][[All, -1]]&


(* ::Subsubsection:: *)
(*generateValidator*)


generateValidator[Symbol] =
	Function[{stopOnInvalid, duplicate, check, checkRel},
		With[
			{
				andFunc = If[stopOnInvalid, And, And[##]&],
				allTrueFunc = allTrue[stopOnInvalid],
				chooseOptionDuplicatesFunc = chooseOptionDuplicates[duplicate]
			},
			If[checkRel === None,
				Function[{f, opts},
					allTrueFunc[
						chooseOptionDuplicatesFunc[opts],
						(*	Extracting second element of option rule with
							CheckOption[...] as third argument, since it's
							possible that rule is delayed and CheckOption[...]
							function has Hold... attribute. *)
						TrueQ@Extract[#, {2}, check[f, First[#]]]&
					]
				]
			(* else *),
				Function[{f, opts},
					With[{preProcessedOpts = chooseOptionDuplicatesFunc[opts]},
						andFunc[
							allTrueFunc[
								preProcessedOpts,
								TrueQ@Extract[#, {2}, check[f, First[#]]]&
							],
							TrueQ@checkRel[f][preProcessedOpts]
						]
					]
				]
			]
		]
	]

generateValidator[List] =
	Function[{stopOnInvalid, duplicate, check, checkRel},
		With[
			{
				allTrueFunc = allTrue[stopOnInvalid],
				chooseOptionDuplicatesFunc = chooseOptionDuplicates[duplicate]
				,
				(*	We take care of duplicates once, validator will receive
					pre-processed options, so passing All to
					generateValidator. *)
				validator =
					generateValidator[Symbol][
						stopOnInvalid, All, check, checkRel
					]
			},
			Function[{fs, opts},
				With[{preProcessedOpts = chooseOptionDuplicatesFunc[opts]},
					Block[{$checkOptionNames = False},
						allTrueFunc[
							fs,
							validator[
								#,
								FilterRules[preProcessedOpts, Options[#]]
							]&
						]
					]
				]
			]
		]
	]


(* ::Subsubsection:: *)
(*packageOptionsValidator*)


packageOptionsValidator =
	generateValidator[Symbol][False, First, CheckOption, None]


(* ::Subsubsection:: *)
(*checkRule*)


checkRule[f_][nonRule : Except[(Rule | RuleDelayed)[_, _]]] :=
	Message[f::rep, HoldForm[nonRule]]


(* ::Subsubsection:: *)
(*generateOptionsPattern*)


generateOptionsPattern[_, _, defaultSpec_, {}, _] :=
	OptionsPattern[defaultSpec]

generateOptionsPattern[type_, wrapper_, f_Symbol, Automatic, optVals_] :=
	generateOptionsPattern[type, wrapper, f, f, optVals]

generateOptionsPattern[type_, wrapper_, defaultSpec_, Automatic, optVals_] :=
	With[{optOwners = Cases[Flatten[{defaultSpec}], _Symbol]},
		If[optOwners === {},
			Message[type::unknownOptionOwner, HoldForm@defaultSpec];
		];
		generateOptionsPattern[type, wrapper, defaultSpec, optOwners, optVals]
	]

(*	Pattern name should be unique to avoid possible conflict with other pattern
	names in final definition in which ValidOptionsPattern[...] is used.
	Fact that we're in package private context and opts is really
	OptionsValidation`Private`opts should be enough for most cases, but to be
	completely sure that e.g. using generated options pattern twice in a
	pattern, won't break the code, we use unique symbol. *)
(*	Wrapping Identity around Condition, so that it'll be returned not treated
	as conditional definition of ValidOptionsPattern. It also prevents treating
	Condition as scoping construct, while evaluating Module, so opts symbol
	inside condition is not renamed and unique symbol from module is used. *)
generateOptionsPattern[_, wrapper_, defaultSpec_, optOwners_, optVals_] :=
	With[{validator = generateValidator@Head@Unevaluated[optOwners]@@optVals},
		Module[{opts},
			Identity[Condition][
				opts : OptionsPattern[defaultSpec],
				wrapper@validator[optOwners, Flatten[{opts}]]
			]
		]
	]


(* ::Subsubsection:: *)
(*getAttributes*)


getAttributes[sym_Symbol] := Attributes[sym]

getAttributes[HoldPattern[Function][_, _, attrs_List]] := attrs

getAttributes[HoldPattern[Function][_, _, attr_]] := {attr}

getAttributes[_] = {}


(* ::Subsubsection:: *)
(*holdsFirstQ*)


holdsFirstQ = MemberQ[getAttributes[#], HoldFirst | HoldAll | HoldAllComplete]&


(* ::Subsubsection:: *)
(*wrapOrThrow*)


wrapOrThrow =
	Function[{check, funcList, name, wrapper},
		Function[val,
			Module[{valEvaluated, callWithVal},
				(*	If evaluation of option value is necessary at all,
					it'll be evaluated only once. *)
				valEvaluated := valEvaluated = val;
				callWithVal = If[holdsFirstQ[#], #[val], #[valEvaluated]]&;
				Scan[
					If[callWithVal@check[#, name] =!= True,
						Throw[$Failed, InvalidOptionValue[#, name, val]]
					]&,
					funcList
				];
				callWithVal[wrapper]
			]
			,
			HoldAllComplete
		]
	]


(* ::Subsubsection:: *)
(*internalCheckedOptionValue*)


internalCheckedOptionValue[
	check_, Automatic, defaults_, rest : Repeated[_, 3]
] :=
	With[{fs = Cases[Flatten[{defaults}], _Symbol]},
		If[fs === {},
			Message[CheckedOptionValue::unknownOptionOwner, HoldForm@defaults];
			OptionValue[defaults, rest]
		(* else *),
			internalCheckedOptionValue[check, fs, defaults, rest]
		]
	]

internalCheckedOptionValue[check_, fSpec_, defaults_, opts_ : {}, nameSpec_] :=
	internalCheckedOptionValue[
		check, fSpec, defaults, opts, nameSpec, Identity
	]

internalCheckedOptionValue[
	check_, f_Symbol, defaults_, opts_, nameSpec_, wrapper_
] :=
	internalCheckedOptionValue[check, {f}, defaults, opts, nameSpec, wrapper]

internalCheckedOptionValue[
	check_, fs_, defaults_, opts_, names_List, wrapper_
] :=
	Module[{i = 1},
		OptionValue[defaults, opts, names,
			Function[val,
				wrapOrThrow[check, fs, names[[i++]], wrapper][val],
				HoldAllComplete
			]
		]
	]

internalCheckedOptionValue[check_, fs_, defaults_, opts_, name_, wrapper_] :=
	OptionValue[defaults, opts, name, wrapOrThrow[check, fs, name, wrapper]]

internalCheckedOptionValue[_, _, optValArgs___] := (
	Message[CheckedOptionValue::unknownOptionValueUsage,
		HoldForm@OptionValue[optValArgs]
	];
	OptionValue[optValArgs]
)


(* ::Subsubsection:: *)
(*setSymseOpts*)


setSymseOpts[f_Symbol] := (
	expr : f[Except[symbolPatt | {symbolPatt...}], ___] /;
		Message[f::symse, HoldForm[1], HoldForm[expr]] =
			"NeverReached";
	
	expr : f[__, nonOpt : Except@optPatt, OptionsPattern[]] /;
		Message[f::nonopt, HoldForm[nonOpt], HoldForm[1], HoldForm[expr]] =
			"NeverReached";
	
	f[] /; Message[f::argx, HoldForm[f], HoldForm[0]] = "NeverReached";


	SyntaxInformation[f] = {"ArgumentsPattern" -> {_, OptionsPattern[]}};
)


(* ::Subsubsection:: *)
(*setMandatorySymseOpts*)


setMandatorySymseOpts[f_Symbol] := (
	expr : f[_, Except[symbolPatt | {symbolPatt...} | optPatt], ___] /;
		Message[f::symse, HoldForm[2], HoldForm[expr]] =
			"NeverReached";
	
	expr : f[_, __, nonOpt : Except@optPatt, OptionsPattern[]] /;
		Message[f::nonopt, HoldForm[nonOpt], HoldForm[2], HoldForm[expr]] =
			"NeverReached";
	
	f[] /;
		Message[f::argt, HoldForm[f], HoldForm[0], HoldForm[1], HoldForm[2]] =
			"NeverReached";
	
	
	SyntaxInformation[f] = {"ArgumentsPattern" -> {_, _., OptionsPattern[]}};
)	


(* ::Subsection:: *)
(*Public*)


(* ::Subsubsection:: *)
(*CheckOption*)


(*	Idea for 2 argument "operator form" of CheckOption inspired by:
	http://mathematica.stackexchange.com/a/116656/14303 *)
CheckOption[f : symbolPatt, name : symbolPatt] :=
	CheckOption[f, SymbolName@Unevaluated[name]]

CheckOption[f : symbolPatt, subOptSpec: _Rule | _RuleDelayed] :=
	With[{normalized = normalizeOptionName[subOptSpec]},
		CheckOption[f, normalized] /; normalized =!= subOptSpec
	]


(*	If there's no 2 argument CheckOption defined fallback to 3 arguments.
	This will pick up UpValues of f with explicit option value pattern. *)
CheckOption[f : symbolPatt, name_][val_] :=
	CheckOption[f, name, Unevaluated[val]]

(*	By default check that option with given name is defined for given
	function f. For sub option specifications check only "root" parent.
	Matching only RuleDelayed since 3 argument version of CheckOption receives
	already normalized option name specifications. *)
(CheckOption /; $checkOptionNames)[f : symbolPatt, (name_ :> _) | name_, _] :=
	Quiet[
		Check[
			OptionValue[f, {}, name, HoldComplete];
			True
			,
			Message[CheckOption::optnf, HoldForm[name], HoldForm[f]];
			False
			,
			OptionValue::optnf
		],
		OptionValue::optnf
	]

(*	If option name checking is switched off, then, by default, all options are
	valid. *)
CheckOption[symbolPatt, _, _] = True


(* Incorrect arguments *)
CheckOption[nonSym : nonSymbolPatt, _][_] /;
	Message[CheckOption::sym, HoldForm[nonSym], HoldForm[1]] =
		"NeverReached"

CheckOption[nonSym : nonSymbolPatt, _, _] /;
	Message[CheckOption::sym, HoldForm[nonSym], HoldForm[1]] =
		"NeverReached"

expr_CheckOption /;
	(ArgumentCountQ[CheckOption, Length@Unevaluated[expr], 2, 3]; False) =
		"NeverReached"

expr : CheckOption[_, _][PatternSequence[] | Repeated[_, {2, Infinity}]] /;
	Message[CheckOption::subargx,
		HoldForm[expr], HoldForm@Evaluate@Length@Unevaluated[expr]
	] =
		"NeverReached"


(*	UpValues overriding set-family functions, ensuring that definitions of
	checks will be attached to appropriate symbols. *)

CheckOption /: assignment : (
	set : Set | SetDelayed | Unset /; useOverriddenQ[CheckOption, set]
)[
	HoldPattern@CheckOption[args__], ___
] :=
	Internal`InheritedBlock[{useOverriddenQ},
		useOverriddenQ[CheckOption, set] = False;
		tagSetProtected[assignment, CheckOption][args]
	]

(*	Since f /: CheckOption[f, namePatt][valPatt] = ... definitions don't work
	(f is to deep), we instead define 3 argument versions:
	f /: CheckOption[f, namePatt, valPatt] = ... *)
CheckOption /: assignment : (
	set : Set | SetDelayed | Unset /; useOverriddenQ[CheckOption, set]
)[
	HoldPattern@CheckOption[args__][subArgs__], ___
] :=
	Internal`InheritedBlock[{useOverriddenQ},
		useOverriddenQ[CheckOption, set] = False;
		tagSetProtected[assignment, CheckOption][args, subArgs]
	]


(*	UpValues overriding tag-set-family functions, ensuring that option name
	patterns will be properly normalized. *)
CheckOption /: (
	tagSet : TagSet | TagSetDelayed | TagUnset /;
		useOverriddenQ[CheckOption, tagSet]
)[
	f : symbolPatt, HoldPattern@CheckOption[args__], Repeated[rhs_, {0, 1}]
] :=
	(* Let arguments of CheckOption evaluate. *)
	With[{argsList = {args}},
		Internal`InheritedBlock[{useOverriddenQ},
			useOverriddenQ[CheckOption, tagSet] = False;
			tagSet[f, CheckOption[##], rhs]
		]& @@
			If[Length[argsList] >= 2,
				First@Reap[
					MapAt[normalizeOptionNamePattern, argsList, 2],
					parentOptionTag,
					If[$defineParentOptionChecks,
						(defineParentOptionCheck[f] /@ Union[#2])&
					]
				]
			(* else *),
				argsList
			]
	]


SyntaxInformation[CheckOption] = {"ArgumentsPattern" -> {_, _, _.}}


(*	Since automatic attaching of CheckOption definitions to specific functions
	works when CheckOption is protected, protect it here so that it can be
	used for defining checks for this package. *)
Protect[CheckOption]


(* ::Subsubsection:: *)
(*CheckOptionRelations*)


(*	By default all options are valid. *)
CheckOptionRelations[symbolPatt][_] = True

(* Incorrect arguments *)
CheckOptionRelations[nonSym : nonSymbolPatt] /;
	Message[CheckOptionRelations::sym, HoldForm[nonSym], HoldForm[1]] =
		"NeverReached"

expr : CheckOptionRelations[PatternSequence[] | Repeated[_, {2, Infinity}]] /;
	Message[CheckOptionRelations::argx,
		HoldForm[CheckOptionRelations],
		HoldForm@Evaluate@Length@Unevaluated[expr]
	] =
		"NeverReached"

expr : HoldPattern@CheckOptionRelations[_][
	PatternSequence[] | Repeated[_, {2, Infinity}]
] /;
	Message[CheckOptionRelations::subargx,
		HoldForm[expr], HoldForm@Evaluate@Length@Unevaluated[expr]
	] =
		"NeverReached"


(*	UpValues overriding set-family functions, ensuring that definitions of
	checks will be attached to appropriate symbols. *)
CheckOptionRelations /: assignment : (
	set : Set | SetDelayed | Unset /; useOverriddenQ[CheckOptionRelations, set]
)[
	HoldPattern@CheckOptionRelations[args__], ___
] :=
	Internal`InheritedBlock[{useOverriddenQ},
		useOverriddenQ[CheckOptionRelations, set] = False;
		tagSetProtected[assignment, CheckOptionRelations][args]
	]


SyntaxInformation[CheckOptionRelations] = {"ArgumentsPattern" -> {_}}


(* ::Subsubsection:: *)
(*ValidOptionsQ*)


Options[ValidOptionsQ] = {
	ChecksStopOnInvalid -> False,
	CheckedDuplicate -> First,
	CheckOption -> CheckOption,
	CheckOptionRelations -> CheckOptionRelations
}


ValidOptionsQ[
	fSpec : symbolPatt | {symbolPatt..},
	optsVOQ : OptionsPattern[] /;
		packageOptionsValidator[ValidOptionsQ, Flatten@{optsVOQ}]
] :=
	With[
		{
			validator =
				generateValidator@Head@Unevaluated[fSpec] @@ OptionValue @ {
					ChecksStopOnInvalid, CheckedDuplicate,
					CheckOption, CheckOptionRelations
				}
		},
		Replace[Flatten[{##}], {
			{} -> True,
			optsList : OptionsPattern[] :> validator[fSpec, optsList],
			nonOptsList_ :> (
				Scan[checkRule[ValidOptionsQ], nonOptsList];
				$Failed
			)
		}]&
	]


(* Incorrect arguments *)
setSymseOpts[ValidOptionsQ]


(* ::Subsubsection:: *)
(*ValidOptionsPattern*)


Options[ValidOptionsPattern] = Options[ValidOptionsQ]


ValidOptionsPattern[
	defaultSpec_,
	fSpec : symbolPatt | {symbolPatt...} : Automatic,
	optsVOP : OptionsPattern[] /;
		packageOptionsValidator[ValidOptionsPattern, Flatten@{optsVOP}]
] :=
	generateOptionsPattern[
		ValidOptionsPattern, Identity, defaultSpec, Unevaluated[fSpec],
		OptionValue@{
			ChecksStopOnInvalid, CheckedDuplicate,
			CheckOption, CheckOptionRelations
		}
	]


(* Incorrect arguments *)
setMandatorySymseOpts[ValidOptionsPattern]


(* ::Subsubsection:: *)
(*QuietValidOptionsPattern*)


Options[QuietValidOptionsPattern] = Options[ValidOptionsQ]

SetOptions[QuietValidOptionsPattern, ChecksStopOnInvalid -> True];


QuietValidOptionsPattern[
	defaultSpec_,
	fSpec :
		Alternatives[
			Except[All | None | HoldPattern@Symbol[___], _Symbol],
			{symbolPatt...}
		] :
		Automatic,
	messagesSpec : Repeated[msgSpecPatt, {0, 2}],
	optsQVOP : OptionsPattern[] /;
		packageOptionsValidator[QuietValidOptionsPattern, Flatten@{optsQVOP}]
] :=
	generateOptionsPattern[
		QuietValidOptionsPattern,
		If[Hold[messagesSpec] === Hold[],
			Quiet
		(* else *),
			Function[Null, Quiet[#, messagesSpec], HoldFirst]
		],
		defaultSpec, Unevaluated[fSpec],
		OptionValue@{
			ChecksStopOnInvalid, CheckedDuplicate,
			CheckOption, CheckOptionRelations
		}
	]


(* Incorrect arguments *)
expr : QuietValidOptionsPattern[
	_,
	PatternSequence[
		Except[symbolPatt | {symbolPatt...} | msgSpecPatt | optPatt], ___
	] |
	PatternSequence[msgSpecPatt, Except@optPatt, Except@optPatt, ___] |
	PatternSequence[_?OptionQ, rest___ /; MemberQ[Hold[rest], Except@optPatt]]
] /; Message[QuietValidOptionsPattern::symse, HoldForm[2], HoldForm[expr]] =
	"NeverReached"

expr : QuietValidOptionsPattern[
	_, _,
	PatternSequence[Except[msgSpecPatt | optPatt], ___] |
	PatternSequence[_?OptionQ, rest___ /; MemberQ[Hold[rest], Except@optPatt]]
] /; Message[QuietValidOptionsPattern::anmlist, HoldForm[3], HoldForm[expr]] =
	"NeverReached"

expr : QuietValidOptionsPattern[
	_, _, _,
	PatternSequence[Except[msgSpecPatt | optPatt], ___] |
	PatternSequence[_?OptionQ, rest___ /; MemberQ[Hold[rest], Except@optPatt]]
] /; Message[QuietValidOptionsPattern::anmlist, HoldForm[4], HoldForm[expr]] =
	"NeverReached"

expr : QuietValidOptionsPattern[
	_, _, _, __, nonOpt : Except@optPatt, OptionsPattern[]
] /;
	Message[QuietValidOptionsPattern::nonopt,
		HoldForm[nonOpt], HoldForm[4], HoldForm[expr]
	] =
		"NeverReached"

QuietValidOptionsPattern[] /;
	Message[QuietValidOptionsPattern::argb, HoldForm[QuietValidOptionsPattern],
		HoldForm[0], HoldForm[1], HoldForm[4]
	] =
		"NeverReached"


SetAttributes[QuietValidOptionsPattern, HoldRest]


SyntaxInformation[QuietValidOptionsPattern] =
	{"ArgumentsPattern" -> {_, _., _., _., OptionsPattern[]}}


(* ::Subsubsection:: *)
(*InvalidOptionsPattern*)


Options[InvalidOptionsPattern] = Options[ValidOptionsQ]


InvalidOptionsPattern[
	defaultSpec_,
	fSpec : symbolPatt | {symbolPatt...} : Automatic,
	optsIOP : OptionsPattern[] /;
		packageOptionsValidator[InvalidOptionsPattern, Flatten@{optsIOP}]
] :=
	generateOptionsPattern[
		InvalidOptionsPattern, Not, defaultSpec, Unevaluated[fSpec],
		OptionValue@{
			ChecksStopOnInvalid, CheckedDuplicate,
			CheckOption, CheckOptionRelations
		}
	]


(* Incorrect arguments *)
setMandatorySymseOpts[InvalidOptionsPattern]


(* ::Subsubsection:: *)
(*SetDefaultOptionsValidation*)


Options[SetDefaultOptionsValidation] = Options[ValidOptionsQ]

SetOptions[SetDefaultOptionsValidation, CheckedDuplicate -> Last];


SetDefaultOptionsValidation[
	f : symbolPatt,
	opts : OptionsPattern[] /;
		packageOptionsValidator[SetDefaultOptionsValidation, Flatten@{opts}]
] /; (
	packageOptionsValidator[f, Options[f]] ||
		Message[SetDefaultOptionsValidation::currentInvalid, HoldForm[f]]
) := (
	f /: SetOptions[f,
		InvalidOptionsPattern[f, opts, Options[SetDefaultOptionsValidation]]
	] :=
		$Failed;
	f
)

SetDefaultOptionsValidation[
	fs : {symbolPatt..},
	opts : OptionsPattern[] /;
		packageOptionsValidator[SetDefaultOptionsValidation, Flatten@{opts}]
] :=
	SetDefaultOptionsValidation[#, opts]& /@ fs


(* Incorrect arguments *)
setSymseOpts[SetDefaultOptionsValidation]


(* ::Subsubsection:: *)
(*CheckedOptionValue*)


Options[CheckedOptionValue] = {CheckOption -> CheckOption}


SetAttributes[CheckedOptionValue, HoldFirst]


CheckedOptionValue[
	ovCall_OptionValue, {},
	opts : OptionsPattern[] /;
		packageOptionsValidator[CheckedOptionValue, Flatten@{opts}]
] := ovCall

CheckedOptionValue[
	OptionValue[args___],
	fSpec: symbolPatt | {symbolPatt..} : Automatic,
	opts : OptionsPattern[] /;
		packageOptionsValidator[CheckedOptionValue, Flatten@{opts}]
] /; (
	(*	OptionValue with 1 argument is a valid usage, but if present, at
		evaluation time, it means that expression is intended to be put on
		RHS of rule with OptionsPattern on LHS, and not evaluated now.
		OptionValue remains unevaluated without error and so does
		CheckedOptionValue. Putting this test at the end, not next to
		OptionValue pattern, so that checks of options possibly given to
		CheckedOptionValue are performed even if OptionValue has 1 argument
		and expression will remain unevaluated. *)
	Length[{args}] =!= 1
):=
	internalCheckedOptionValue[OptionValue[CheckOption], fSpec, args]


(* Incorrect arguments *)
expr : CheckedOptionValue[Except[_OptionValue], ___] /;
	Message[CheckedOptionValue::optionValueHead, HoldForm[expr]] =
		"NeverReached"

setMandatorySymseOpts[CheckedOptionValue]


(* ::Subsubsection:: *)
(*WithOptionValueChecks*)


Options[WithOptionValueChecks] = {
	CheckOption -> Automatic,
	InvalidOptionHandler -> Function[{}, $Failed, HoldAllComplete]
}


SetAttributes[WithOptionValueChecks, HoldFirst]


WithOptionValueChecks[body_, {}, OptionsPattern[]] := body

(* Inspired by http://mathematica.stackexchange.com/a/116633/14303 *)
WithOptionValueChecks[
	body_,
	fSpec : symbolPatt | {symbolPatt..} | PatternSequence[],
	opts : OptionsPattern[] /;
		packageOptionsValidator[WithOptionValueChecks, Flatten@{opts}]
] :=
	Catch[
		Replace[
			Unevaluated[body],
			(ovCall_OptionValue :> CheckedOptionValue[ovCall, fSpec, ##])& @@
				Replace[OptionValue[CheckOption], {
					Automatic -> {},
					check_ :> {CheckOption -> check}
				}],
			{0, Infinity},
			Heads -> True
		],
		_InvalidOptionValue,
		(OptionValue[InvalidOptionHandler] @@ #2)&
	]


(* Incorrect arguments *)
setMandatorySymseOpts[WithOptionValueChecks]


(* ::Subsubsection:: *)
(*InvalidOptionValue*)


SetAttributes[InvalidOptionValue, HoldAllComplete]


(* ::Subsubsection:: *)
(*Package options*)


(*	Option tests are defined at the end of package, so that full functionality
	of package itself can be used to do it. *)
With[
	{
		functions =
			ValidOptionsQ | ValidOptionsPattern | QuietValidOptionsPattern |
			InvalidOptionsPattern | SetDefaultOptionsValidation
	},


	CheckOption[f : functions, ChecksStopOnInvalid][
		val : Except[True | False]
	] :=
		Message[f::opttf, HoldForm[ChecksStopOnInvalid], HoldForm[val]];
	
	CheckOption[f : functions, CheckedDuplicate][
		val : Except[First | Last | All]
	] :=
		Message[f::optfla, HoldForm[CheckedDuplicate], HoldForm[val]];


	SetDefaultOptionsValidation[List @@ functions]
]


End[]


Protect["`*"]


EndPackage[]
