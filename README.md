# OptionsValidation

[![releases](http://img.shields.io/github/release/jkuczm/MathematicaOptionsValidation.svg)](https://github.com/jkuczm/MathematicaOptionsValidation/releases)
[![SemVer 2.0.0](http://img.shields.io/badge/SemVer-2.0.0-brightgreen.svg)](http://semver.org/spec/v2.0.0.html)
[![license MIT](http://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/jkuczm/MathematicaOptionsValidation/blob/master/LICENSE)
[![Mathematica 8.0 - 10.4](http://img.shields.io/badge/Mathematica-8.0 -- 10.4-brightgreen.svg)](#compatibility)


Framework for options validation in *Mathematica*.


* [Description](#description)
* [Installation](#installation)
    * [Automatic installation](#automatic-installation)
    * [Manual installation](#manual-installation)
    * [No installation](#no-installation)
* [Compatibility](#compatibility)
* [Bugs and requests](#bugs-and-requests)
* [Contributing](#contributing)
* [Usage in other packages](#usage-in-other_packages)
* [License](#license)
* [Versioning](#versioning)



## Description

This package provides a framework simplifying validation of options.

It provides functions registering arbitrary tests for values of options,
of given symbols, with names matching given patterns. Test of relations between
different options can be also registered.

Registered tests can be automatically used in various different strategies
of option value testing. Tests can be performed while evaluating body of
function when option values are accessed, or they can be performed upfront
while matching function pattern.
When tests fail - function can either return a value denoting failure
(e.g. `$Failed`), or can remain unevaluated.

Default values of options can be also automatically validated when they are
changed.

You can find usage example in
[answer to "Manipulations with options" question](http://mathematica.stackexchange.com/a/105773/14303)
on Mathematica Stack Exchange.



## Installation


### Automatic installation

To install OptionsValidation package evaluate:
```Mathematica
Import["https://raw.githubusercontent.com/jkuczm/MathematicaOptionsValidation/master/BootstrapInstall.m"]
```

Note that this will also install
[ProjectInstaller](https://github.com/lshifr/ProjectInstaller)
package, if you don't have it already installed.

To load OptionsValidation package evaluate: ``Needs["OptionsValidation`"]``.


### Manual installation

1. Download latest released
   [OptionsValidation.zip](https://github.com/jkuczm/MathematicaOptionsValidation/releases/download/v0.1.0/OptionsValidation.zip)
   file.

2. Extract downloaded `OptionsValidation.zip` to any directory which is on
   Mathematica `$Path`, e.g. to one obtained by evaluating
   `FileNameJoin[{$UserBaseDirectory,"Applications"}]`.

3. To load the package evaluate: ``Needs["OptionsValidation`"]``


### No installation

To use package directly from the Web, without installation, evaluate:
```Mathematica
Import["https://raw.githubusercontent.com/jkuczm/MathematicaOptionsValidation/master/NoInstall.m"]
```



## Compatibility

This package contains extensive
[automatic test suite](https://github.com/jkuczm/MathematicaOptionsValidation/tree/master/OptionsValidation/Tests).
Package is tested with *Mathematica* versions 8.0, 9.0, 10.0, 10.1, 10.2,
10.3 and 10.4 on Linux. Since it doesn't contain any OS specific code it should
work with above versions on all operating systems.

There's also no obvious reason for package not to work on earlier (6.0+)
versions of Mathematica.



## Bugs and requests

If you find any bugs, or have a feature request, please create an
[issue on GitHub](https://github.com/jkuczm/MathematicaOptionsValidation/issues).



## Contributing

Feel free to fork and send pull requests.

If you want to use Ant scripts from this repository you will also need to
install [WWBCommon](https://github.com/jkuczm/WWBCommon) project.

All contributions are welcome!



## Usage in other packages

There are two ways to use OptionsValidation with your own package.

First is to require users, of your package, to install OptionsValidation
separately. Your package can then load it, as any other external package, using
``Get["OptionsValidation`"]``. Be aware that if your package requires specific
version of OptionsValidation and user wants to use other package requiring
different version of OptionsValidation it may lead to a version conflict.

Second way is to include specific version of OptionsValidation as sub-package
of your package. To do it, simply put `OptionsValidation.m` file inside main
directory of your package. And load it using ``Get["`OptionsValidation`"]``
(note the grave accent character at the beginning of string) somewhere inside
main context of your package.

Directory structure of ``YourPackage` `` can, for example, look like this:

```
YourPackage
├── Kernel
│   └── init.m
├── OptionsValidation.m
└── YourPackage.m
```

and `YourPackage.m` file:

```Mathematica
BeginPackage["YourPackage`"]
(* Public symbols usage *)
Get["`OptionsValidation`"]
Begin["`Private`"]
(* Implementation *)
End[]
EndPackage[]
```

This way specific version of OptionsValidation package, distributed with your
package, will be loaded as ``YourPackage`OptionsValidation` ``, and will be
completely independent of other versions of OptionsValidation possibly used by
user of your package.



## License

This package is released under
[The MIT License](https://github.com/jkuczm/MathematicaOptionsValidation/blob/master/LICENSE).


### Attribution

`WithOptionValueChecks` and `CheckedOptionValue` functions are a derivative of
[`OptionCheck`](http://mathematica.stackexchange.com/a/116633/14303) function
written by
[Leonid Shifrin](http://mathematica.stackexchange.com/users/81/leonid-shifrin)
used under
[Creative Commons Attribution-ShareAlike 3.0 Unported License](http://creativecommons.org/licenses/by-sa/3.0/).

Idea for 2 argument "operator form" of `CheckOption` function was inspired by:
[`test`](http://mathematica.stackexchange.com/a/116656/14303) function written
by [Mr.Wizard](http://mathematica.stackexchange.com/users/121/mr-wizard)
used under
[Creative Commons Attribution-ShareAlike 3.0 Unported License](http://creativecommons.org/licenses/by-sa/3.0/).

Idea to allow loading package as a sub-package was inspired by system used in
[LTemplate](https://github.com/szhorvat/LTemplate) package written by
[Szabolcs Horvát](http://szhorvat.net) used under
[The MIT License](https://github.com/szhorvat/LTemplate/blob/v0.3/LTemplate/LICENSE.txt).



## Versioning

Releases of this package will be numbered using
[Semantic Versioning guidelines](http://semver.org).
