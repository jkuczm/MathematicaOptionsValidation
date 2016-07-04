(* ::Package:: *)

Import["https://raw.githubusercontent.com/jkuczm/MathematicaBootstrapInstaller/v0.1.1/BootstrapInstaller.m"]


BootstrapInstall[
	"OptionsValidation",
	"https://github.com/jkuczm/MathematicaOptionsValidation/releases/download/v0.1.0/OptionsValidation.zip",
	"AdditionalFailureMessage" ->
		Sequence[
			"You can ",
			Hyperlink[
				"install OptionsValidation package manually",
				"https://github.com/jkuczm/MathematicaOptionsValidation#manual-installation"
			],
			"."
		]
]
