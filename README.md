# Parsing and validation for finnish national identification number

[Personal identity code - Digi- ja väestötietovirasto](https://dvv.fi/en/personal-identity-code)

This library aims to provide a reliable way to deserialize all valid hetus. Also temporary hetus (id number >= 900) are supported.

```purescript
> import Hetu
> import Data.Functor
> hetu = parseHetu "260993-5658"
> hetu
(Right { birthday: (Date (Year 1993) September (Day 26)), id: 565 })

> map gender hetu
(Right Male)

> map formatHetu hetu
(Right "260993-5658")
```

Detailed error messages are provided with exact reason why the hetu is not valid:

```purescript
> parseHetu "280264-051E"
(Left "Invalid checksum at column 12")

> parseHetu "a60993-5658"
(Left "Expected digit at column 1")

> parseHetu "131052B308T"
(Left "Invalid century: \"B\" at column 8")
```

### Development

Install [spago](https://github.com/purescript/spago) and run `spago test`

### Related projects

* https://github.com/vkomulai/finnish-ssn

* https://gist.github.com/puumuki/11172310

* https://github.com/mharj/hetu

* https://github.com/bittisiirto/henkilotunnus

* https://github.com/orangitfi/FinnPic

* https://github.com/rOpenGov/hetu
