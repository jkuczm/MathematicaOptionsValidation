(* ::Package:: *)


Internal`InheritedBlock[{BeginPackage},
	Unprotect[BeginPackage];
	(*	If package is loaded through NoInstall.m it'll be loaded as package
		with ordinary "top-level" context. *)
	BeginPackage["`OptionsValidation`"] := BeginPackage["OptionsValidation`"];
	Protect[BeginPackage];
	Import["https://raw.githubusercontent.com/jkuczm/MathematicaOptionsValidation/master/OptionsValidation/OptionsValidation.m"]
]
