(* Mathematica Init File *)

Internal`InheritedBlock[{BeginPackage},
	Unprotect[BeginPackage];
	(*	If package is loaded through init.m it'll be loaded as package with
		ordinary "top-level" context. *)
	BeginPackage["`OptionsValidation`"] := BeginPackage["OptionsValidation`"];
	Protect[BeginPackage];
	Get[ "OptionsValidation`OptionsValidation`"]
]
