# spamgen

Work in progress, especially this page.

## Installation

Clone this repository then `lein test`. But right now it displays a bunch of nonsense which I am fixing.

## Usage

`cd` to the top-level and `lein bin`.

Then only this works:

````bash
bin/spamgen -h`
````
Fails because (for one) the `config.edn` is not being picked up by the standalone.

Putting in a jury-rig fix next.

## Options

Try: `bin/spamgen -h`

## Examples

RSN

### Bugs

Running in parallel (the `-m` option) is a tad slower. Investigating.

## License

Copyright © 2018 Kenneth Tilton

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
