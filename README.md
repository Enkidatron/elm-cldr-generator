# elm-cldr-generator

This is the code for generating the `Cldr.Locale` file in [enkidatron/elm-cldr][elm-cldr]. 

It contains [enkidatron/elm-cldr][elm-cldr] and the JSON release of Unicode's [CLDR Project][cldr] as submodules. It uses [albertdahlin/elm-posix][elm-posix] to read the CLDR JSON files and automatically generate the `Cldr.Locale` file with all of the discovered locales. 

[elm-cldr]: https://github.com/enkidatron/elm-cldr
[cldr]: https://cldr.unicode.org/index
[elm-posix]: https://package.elm-lang.org/packages/albertdahlin/elm-posix/latest/

## Generate elm-cldr

To generate elm-cldr from the current `cldr-json`, run: 
```bash
elm-cli run src/Generate.elm
```
