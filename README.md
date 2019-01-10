# OptionsValidation

[![releases](https://img.shields.io/github/release/jkuczm/MathematicaOptionsValidation.svg)](https://github.com/jkuczm/MathematicaOptionsValidation/releases)
[![Mathematica 8.0 - 11.0](https://img.shields.io/badge/Mathematica-8.0_--_11.0-brightgreen.svg)](#compatibility)
[![license MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/jkuczm/MathematicaOptionsValidation/blob/master/LICENSE)
[![SemVer 2.0.0](https://img.shields.io/badge/SemVer-2.0.0-brightgreen.svg)](http://semver.org/spec/v2.0.0.html)


Framework for options validation in *Mathematica*.


* [Description](#description)
* [Installation](#installation)
    * [Automatic installation](#automatic-installation)
    * [Manual installation](#manual-installation)
    * [No installation](#no-installation)
* [Compatibility](#compatibility)
* [Bugs and requests](#bugs-and-requests)
* [Contributing](#contributing)
* [Usage in other packages](#usage-in-other-packages)
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

You can find basic usage examples in
[OptionsValidation PackageData entry](http://packagedata.net/index.php/links/examples/id/248).
More involved examples can be found in answers to 
[Manipulations with options](http://mathematica.stackexchange.com/a/105773/14303)
and
[How to check the validity of an option value](http://mathematica.stackexchange.com/a/119973/14303)
questions on Mathematica Stack Exchange.



## Installation


### Automatic installation

To install newest version of OptionsValidation package,
directly from repository, in *Mathematica* version 10 or newer,
evaluate following code:
```Mathematica
PacletInstall@"http://github.com/jkuczm/MathematicaOptionsValidation/releases/download/v0.1.1/OptionsValidation-0.1.1.paclet"
```

Note that above requires allowing *Mathematica* to use the Internet.

To load OptionsValidation package evaluate:
```Mathematica
Needs@"OptionsValidation`"
```

To uninstall OptionsValidation package evaluate:
```Mathematica
PacletUninstall@"OptionsValidation"
```


### Manual installation

If in your setup *Mathematica* doesn't have Internet access,
or you're using version older than 10, download
[OptionsValidation.0.1.1.paclet](https://github.com/jkuczm/MathematicaOptionsValidation/releases/download/v0.1.1/OptionsValidation-0.1.1.paclet)
file and evaluate `PacletInstall` with path to downloaded file:
```Mathematica
PacletInstall@"path/to/downloaded/OptionsValidation.0.1.1.paclet"
```

To load OptionsValidation package evaluate:
```Mathematica
Needs@"OptionsValidation`"
```

To uninstall OptionsValidation package evaluate:
```Mathematica
PacletUninstall@"OptionsValidation"
```


### No installation

To use package directly from the Web, without installation, evaluate:
```Mathematica
Import@"https://raw.githubusercontent.com/jkuczm/MathematicaOptionsValidation/master/NoInstall.m"
```



## Compatibility

This package contains extensive
[automatic test suite](https://github.com/jkuczm/MathematicaOptionsValidation/tree/master/OptionsValidation/Tests).
Package is tested with all *Mathematica* major and minor versions from 8.0 to
11.0 on Linux. Since it doesn't contain any OS specific code it should work
with above versions on all operating systems.

There's also no obvious reason for package not to work on older (6.0+)
and newer (11.1+) versions of *Mathematica*,
but it was not tested with these versions.



## Bugs and requests

If you find any bugs, or have a feature request, please create an
[issue on GitHub](https://github.com/jkuczm/MathematicaOptionsValidation/issues).



## Contributing

Feel free to fork and send pull requests.

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
