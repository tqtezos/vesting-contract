
A LIGO module to embed the Tez Vesting Contract.

`vesting_tez.mligo` contains an embedding of the Tez Vesting Contract's source,
defined in `vesting_tez_main`.

## Types

```ocaml
type vesting_tez_param =
  SetDelegate of key_hash option
| Vest of nat

type wrapped = address * address
type schedule_seconds = nat * nat
type schedule = timestamp * schedule_seconds
type vested_schedule = nat * schedule
type vesting_tez_storage = wrapped * vested_schedule
```

## Building

The contract can be built from `../contracts/vesting_tez.tz` by running:

```bash
./mk_vesting_tez.mligo.sh
```

## Importing

To include this module in your LIGO project, add an `#include` statement with
the path to `vesting_tez.mligo`:

```ocaml
#include "path_to_vesting_tez_mligo/vesting_tez.mligo"
```

For a minimal example of wrapping a contract, see [`admin_wrapper`](https://github.com/tqtezos/smart-contracts/blob/c93bddaccd3d72e88a421d9304dc3cd3e92c103c/shared/fa2_modules/test/simple_admin_wrapper.mligo)

## Compiling

The contract can be compiled to Michelson using the following command:

```bash
ligo compile-contract --syntax cameligo \
  --output-file=vesting_tez.mligo.tz \
  vesting_tez.mligo vesting_tez_main
```

Or, using `docker`:

```bash
docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:0.13.0 compile-contract \
  --syntax cameligo --output-file=vesting_tez.mligo.tz \
  vesting_tez.mligo vesting_tez_main
```

