# MDcountrycode
Converts country codes between various standards (2letter, 3letter, name, numeric, etc.) and displays country groups (EU, OECD, etc)
The key use of this package is to provide custom-tailored assistance to specfic packages for macro data: MD3 and MDstats 




This package is inspired by Vincent Arel-Bundock's countrycode package. IT is for this reason that the underlying county table 'dictccode' (which uses part of the initial countrycode pacakge) is licensed under GPL, whereas the rest was made from scratch and is licensed under LGPL. 

This package puts more emphasis on macroeconomic aspects, in particular:

Capturing small territories, also by regex

Adding institutions and country group codes

Adding alternative code matching

Reflecting the latest name and code changes etc. (e.g. "North Macedonia")
