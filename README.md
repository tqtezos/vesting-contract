
See the FA1.2 [Quick Start Tutorial](https://assets.tqtezos.com/token-contracts/1-fa12-lorentz) for more detail.

You can find the following example on carthagenet [here](https://better-call.dev/carthagenet/KT1Gkixg9sDd3LE5Rn1RqkiQnAk2xnFVuq7a/operations).

# Introduction

## Contracts

This repo defines two contracts:
- A compile-time polymorphic vesting contract, which
  accepts a token transfer method and implements a
  vesting schedule for it
- A Tez vesting contract, which allows a single admin, set at origination,
  to delegate its balance.

The following examples use the Tez vesting contract.

## Specification

A vesting contract holds some tokens and periodically allows
some of its tokens to be transferred to a fixed address,
i.e. "_flushed_":

```haskell
parameter (or (option %setDelegate key_hash)
              (nat %vest))
```

We call one period of waiting a `tick`,
i.e. a fixed number of tokens are added to the allowed number every tick.

The number of tokens that may be transferred over time is specified by a
"_vesting schedule_", which includes:
- `timestamp`: when the vesting should begin
- `nat`: the number of seconds per tick
- `nat`: the number of mutez that are allowed per tick

## Example

Suppose we originate the contract with `60 Mutez` and then set:
- `timestamp := now`
- `seconds_per_tick := 60`
- `mutez_per_tick := 1`

Then we can have the following scenarios:

- One minute after origination, anyone may trigger the flush of `1 Mutez`.
- If no one flushes the contract after origination, `10 minutes` later,
  anyone may trigger the flush of _up to_ `10 Mutez`.
- If the maximum number is flushed `5 minutes` after origination (`5 Mutez`),
  anyone may flush up to `10 Mutez` `10 minutes` later
- One hour after origination, the entire balance of the contract may be flushed.


## Setting Up
### Requirements
#### Tezos-client

To set up the tezos-client, follow the instructions in the [Setup Tezos Client](https://assets.tqtezos.com/setup/1-tezos-client) tutorial

### Installing Dependencies

```bash
stack build
```

Note that this will take some time.

## Originating

### Print the `vestingTezContract`

Required Haskell imports:

```haskell
import qualified Lorentz.Contracts.Vesting
import Tezos.Crypto.Orphans ()
import Text.Read
```

(Re)generate the contract:

```haskell
printVestingTezContract (Just "contracts/vesting_tez.tz") False
```

### Generate the initial storage

```bash
❯❯❯ echo $ALICE_ADDRESS
tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr

❯❯❯ echo $BOB_ADDRESS
tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
```

The invocation is:
```haskell
printInitVestingTezContract
  targetAddress
  delegateAdminAddress
  secondPerTick
  mutezPerTick
```

For example:

```haskell
printInitVestingTezContract (read "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr") (read "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm") 30 1

Pair (Pair "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm") (Pair 0 (Pair "2020-09-10T18:57:56Z" (Pair 30 1)))
```

This will allow `1 Mutez` to be flushed every `30 seconds`, which is the block time for `carthagenet`.


### Originate the contract

```bash
$ tezos-client --wait none originate contract VestingTez \
  transferring 0 from $BOB_ADDRESS running \
  "$(cat contracts/vesting_tez.tz | tr -d '\n')" \
  --init 'Pair (Pair "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm") (Pair 0 (Pair "2020-09-10T18:57:56Z" (Pair 30 1)))' \
  --burn-cap 0.753

Waiting for the node to be bootstrapped before injection...
Current head: BKr17Pt8XfgL (timestamp: 2020-09-10T19:01:14-00:00, validation: 2020-09-10T19:01:32-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 23273 units (will add 100 for safety)
Estimated storage: 753 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ooa1EPyaoe6Gm8WEzfuRJSqH6VkhWxwPP3HyfH98GZQS5Ej8QpQ'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooa1EPyaoe6Gm8WEzfuRJSqH6VkhWxwPP3HyfH98GZQS5Ej8QpQ to be included --confirmations 30 --branch BKr17Pt8XfgLJmixB51xsjtR6mgWc4Z5PC2sgumtVNGzMqwpzZ6
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.003111
    Expected counter: 624017
    Gas limit: 23373
    Storage limit: 773 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.003111
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,348) ... +ꜩ0.003111
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter (or (option %setDelegate key_hash) (nat %vest)) ;
          storage (pair (pair address address) (pair nat (pair timestamp (pair nat nat)))) ;
          code { DUP ;
                 CAR ;
                 DIP { CDR } ;
                 IF_LEFT
                   { SWAP ;
                     DUP ;
                     DIP { CAR ;
                           CDR ;
                           SENDER ;
                           COMPARE ;
                           EQ ;
                           IF { DIP { NIL operation } ; SET_DELEGATE ; CONS } { FAILWITH } } ;
                     SWAP ;
                     PAIR }
                   { PAIR ;
                     DUP ;
                     CAR ;
                     DIP { CDR ;
                           DUP ;
                           DIP { CAR } ;
                           CDR ;
                           DUP ;
                           DIP { CDR } ;
                           DUP ;
                           CDR ;
                           DIP { CAR } } ;
                     DUP ;
                     DIP { DIP { DIP { DUP } ;
                                 DUP ;
                                 CAR ;
                                 NOW ;
                                 SUB ;
                                 DIP { CDR ; CAR } ;
                                 EDIV ;
                                 IF_NONE { FAILWITH } { CAR } ;
                                 SUB ;
                                 ISNAT } ;
                           SWAP ;
                           IF_NONE
                             { FAILWITH }
                             { DIP { DUP } ; SWAP ; COMPARE ; LE ; IF { ADD } { FAILWITH } } ;
                           DIP { DUP } ;
                           SWAP ;
                           DIP { PAIR ; SWAP ; DUP } } ;
                     DIP { CDR ; CDR } ;
                     MUL ;
                     SWAP ;
                     CAR ;
                     CONTRACT unit ;
                     IF_NONE
                       { FAILWITH }
                       { SWAP ;
                         PUSH mutez 1 ;
                         MUL ;
                         UNIT ;
                         TRANSFER_TOKENS ;
                         DIP { NIL operation } ;
                         CONS } ;
                     DIP { PAIR } ;
                     PAIR } } }
        Initial storage:
          (Pair (Pair "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")
                (Pair 0 (Pair "2020-09-10T18:57:56Z" (Pair 30 1))))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1PvGfa453DkooTHtBumpU9avi2N4Lp1sMH
        Storage size: 496 bytes
        Paid storage size diff: 496 bytes
        Consumed gas: 23273
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.496
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257

New contract KT1PvGfa453DkooTHtBumpU9avi2N4Lp1sMH originated.
Contract memorized as VestingTez.
```

To actually use it for vesting, we'll need to transfer some tez to the contract:

❯❯❯ tezos-client --wait none originate contract VestingTez \
  transferring 10000 from $BOB_ADDRESS running \
  "$(cat contracts/vesting_tez.tz | tr -d '\n')" \
  --init 'Pair (Pair "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm") (Pair 0 (Pair "2020-09-10T18:57:56Z" (Pair 30 1)))' \
  --burn-cap 0.753

Waiting for the node to be bootstrapped before injection...
Current head: BL8n2wxxshUz (timestamp: 2020-09-10T19:03:44-00:00, validation: 2020-09-10T19:04:14-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 23273 units (will add 100 for safety)
Estimated storage: 753 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'oniaFn6W7aaMcT7p56dN3N2ve24fr2S9iSdpoQShxdFL98dhq9E'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for oniaFn6W7aaMcT7p56dN3N2ve24fr2S9iSdpoQShxdFL98dhq9E to be included --confirmations 30 --branch BKra754C6iopunJKDJNfqCmqvpPCNz7TD4eFk2ACZhXaNNoPxcf
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.003115
    Expected counter: 624018
    Gas limit: 23373
    Storage limit: 773 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.003115
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,348) ... +ꜩ0.003115
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ10000
      Script:
        { ... }
        Initial storage:
          (Pair (Pair "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")
                (Pair 0 (Pair "2020-09-10T18:57:56Z" (Pair 30 1))))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1Gkixg9sDd3LE5Rn1RqkiQnAk2xnFVuq7a
        Storage size: 496 bytes
        Paid storage size diff: 496 bytes
        Consumed gas: 23273
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.496
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ10000
          KT1Gkixg9sDd3LE5Rn1RqkiQnAk2xnFVuq7a ... +ꜩ10000

New contract KT1Gkixg9sDd3LE5Rn1RqkiQnAk2xnFVuq7a originated.
Contract memorized as VestingTez.
```

Make a bash variable for the contract:

```bash
VESTING_TEZ="KT1Gkixg9sDd3LE5Rn1RqkiQnAk2xnFVuq7a"
```

To vest some tez, submit the number of ticks to the `vest` entrypoint.

Submitting too many ticks will error:

```bash
❯❯❯ tezos-client --wait none transfer 0 from $BOB_ADDRESS to $VESTING_TEZ \
  --entrypoint vest --arg 100

Waiting for the node to be bootstrapped before injection...
Current head: BKoDiFqSKwRZ (timestamp: 2020-09-10T19:08:24-00:00, validation: 2020-09-10T19:08:27-00:00)
Node is bootstrapped, ready for injecting operations.
This simulation failed:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0
    Expected counter: 624019
    Gas limit: 1040000
    Storage limit: 60000 bytes
    Transaction:
      Amount: ꜩ0
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT1Gkixg9sDd3LE5Rn1RqkiQnAk2xnFVuq7a
      Entrypoint: vest
      Parameter: 100
      This operation FAILED.

Runtime error in contract KT1Gkixg9sDd3LE5Rn1RqkiQnAk2xnFVuq7a:
  01: { parameter (or (option %setDelegate key_hash) (nat %vest)) ;
  02:   storage (pair (pair address address) (pair nat (pair timestamp (pair nat nat)))) ;
  03:   code { DUP ;
  04:          CAR ;
  05:          DIP { CDR } ;
  06:          IF_LEFT
  07:            { SWAP ;
  08:              DUP ;
  09:              DIP { CAR ;
  10:                    CDR ;
  11:                    SENDER ;
  12:                    COMPARE ;
  13:                    EQ ;
  14:                    IF { DIP { NIL operation } ; SET_DELEGATE ; CONS } { FAILWITH } } ;
  15:              SWAP ;
  16:              PAIR }
  17:            { PAIR ;
  18:              DUP ;
  19:              CAR ;
  20:              DIP { CDR ;
  21:                    DUP ;
  22:                    DIP { CAR } ;
  23:                    CDR ;
  24:                    DUP ;
  25:                    DIP { CDR } ;
  26:                    DUP ;
  27:                    CDR ;
  28:                    DIP { CAR } } ;
  29:              DUP ;
  30:              DIP { DIP { DIP { DUP } ;
  31:                          DUP ;
  32:                          CAR ;
  33:                          NOW ;
  34:                          SUB ;
  35:                          DIP { CDR ; CAR } ;
  36:                          EDIV ;
  37:                          IF_NONE { FAILWITH } { CAR } ;
  38:                          SUB ;
  39:                          ISNAT } ;
  40:                    SWAP ;
  41:                    IF_NONE
  42:                      { FAILWITH }
  43:                      { DIP { DUP } ; SWAP ; COMPARE ; LE ; IF { ADD } { FAILWITH } } ;
  44:                    DIP { DUP } ;
  45:                    SWAP ;
  46:                    DIP { PAIR ; SWAP ; DUP } } ;
  47:              DIP { CDR ; CDR } ;
  48:              MUL ;
  49:              SWAP ;
  50:              CAR ;
  51:              CONTRACT unit ;
  52:              IF_NONE
  53:                { FAILWITH }
  54:                { SWAP ;
  55:                  PUSH mutez 1 ;
  56:                  MUL ;
  57:                  UNIT ;
  58:                  TRANSFER_TOKENS ;
  59:                  DIP { NIL operation } ;
  60:                  CONS } ;
  61:              DIP { PAIR } ;
  62:              PAIR } } }
At line 43 characters 72 to 80,
script reached FAILWITH instruction
with 100
Fatal error:
  transfer simulation failed
```

But it will succeed for a valid number of ticks:

```bash
❯❯❯ tezos-client --wait none transfer 0 from $BOB_ADDRESS to $VESTING_TEZ \
  --entrypoint vest --arg 3

Waiting for the node to be bootstrapped before injection...
Current head: BLuzPvDHnWyW (timestamp: 2020-09-10T19:09:24-00:00, validation: 2020-09-10T19:09:44-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 42846 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'onyohn6fJBhqjgjp85iZbMiJFA9HttDxN2YWCcGqHFbtNuRDg8K'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for onyohn6fJBhqjgjp85iZbMiJFA9HttDxN2YWCcGqHFbtNuRDg8K to be included --confirmations 30 --branch BLuzPvDHnWyWtSGztZXdWnZcXrfnyE5hQUG6bQeukXTRVGpc7H5
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.004558
    Expected counter: 624019
    Gas limit: 42946
    Storage limit: 0 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.004558
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,348) ... +ꜩ0.004558
    Transaction:
      Amount: ꜩ0
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT1Gkixg9sDd3LE5Rn1RqkiQnAk2xnFVuq7a
      Entrypoint: vest
      Parameter: 3
      This transaction was successfully applied
      Updated storage:
        (Pair (Pair 0x00003b5d4596c032347b72fb51f688c45200d0cb50db
                    0x0000aad02222472cdf9892a3011c01caf6407f027081)
              (Pair 3 (Pair 1599764276 (Pair 30 1))))
      Storage size: 496 bytes
      Consumed gas: 32639
    Internal operations:
      Transaction:
        Amount: ꜩ0.000003
        From: KT1Gkixg9sDd3LE5Rn1RqkiQnAk2xnFVuq7a
        To: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
        This transaction was successfully applied
        Consumed gas: 10207
        Balance updates:
          KT1Gkixg9sDd3LE5Rn1RqkiQnAk2xnFVuq7a ... -ꜩ0.000003
          tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ... +ꜩ0.000003
```

