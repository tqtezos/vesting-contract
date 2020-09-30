
You can find the following example on delphinet [here](https://better-call.dev/delphinet/KT1SQEED3eYUSwh6CiU9MfELhVwHwsrF67Kn/operations).

## Setting Up
### Requirements
#### Tezos-client

To set up the tezos-client, follow the instructions in the [Setup Tezos Client](https://assets.tqtezos.com/setup/1-tezos-client) tutorial

```bash
❯❯❯ tezos-client -A 'delphinet.smartpy.io' -P 443 -S activate account bob with ~/Downloads/tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm.json --force
```

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

Pair (Pair "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm") (Pair 0 (Pair "2020-09-30T19:50:24Z" (Pair 30 1)))
```

This will allow `1 Mutez` to be flushed every `30 seconds`, which is the block time for `carthagenet`.


### Originate the contract

```bash
$ tezos-client -A 'delphinet.smartpy.io' -P 443 -S --wait none originate contract VestingTez \
  transferring 0 from $BOB_ADDRESS running \
  "$(cat contracts/vesting_tez.tz | tr -d '\n')" \
  --init 'Pair (Pair "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm") (Pair 0 (Pair "2020-09-30T19:50:24Z" (Pair 30 1)))' \
  --burn-cap 0.2145

Node is bootstrapped, ready for injecting operations.
Estimated gas: 3980.424 units (will add 100 for safety)
Estimated storage: 858 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ooUDZJpzP5VTjfaEtCKTHbZxXqMYcxdX99Fqii7k2cZJmqtgccA'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooUDZJpzP5VTjfaEtCKTHbZxXqMYcxdX99Fqii7k2cZJmqtgccA to be included --confirmations 30 --branch BMQhUF3B5T4Zxb9GraYGHRoBZXoui5df8ghLAzWh1qcmzws6Tys
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.001259
    Expected counter: 526598
    Gas limit: 10000
    Storage limit: 0 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............ -ꜩ0.001259
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,32) ... +ꜩ0.001259
    Revelation of manager public key:
      Contract: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Key: edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb
      This revelation was successfully applied
      Consumed gas: 1000
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.00119
    Expected counter: 526599
    Gas limit: 4081
    Storage limit: 878 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............ -ꜩ0.00119
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,32) ... +ꜩ0.00119
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter (or (option %setDelegate key_hash) (nat %vest)) ;
          storage
            (pair (pair %wrapped (address %target) (address %delegateAdmin))
                  (pair (nat %vested)
                        (pair %schedule (timestamp %epoch) (pair (nat %secondsPerTick) (nat %tokensPerTick))))) ;
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
                           DIP { PAIR ; SWAP ; DUP } ;
                           CDR ;
                           CDR } ;
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
                (Pair 0 (Pair "2020-09-30T19:50:24Z" (Pair 30 1))))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1VSR4k7NcWZvXsraA19RJyQBnXDiDnDqyh
        Storage size: 601 bytes
        Paid storage size diff: 601 bytes
        Consumed gas: 3980.424
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.15025
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.06425

New contract KT1VSR4k7NcWZvXsraA19RJyQBnXDiDnDqyh originated.
Contract memorized as VestingTez.
```

To actually use it for vesting, we'll need to transfer some tez to the contract:

[onyohn6fJBhqjgjp85iZbMiJFA9HttDxN2YWCcGqHFbtNuRDg8K](https://better-call.dev/carthagenet/opg/onyohn6fJBhqjgjp85iZbMiJFA9HttDxN2YWCcGqHFbtNuRDg8K/contents)

```bash
$ tezos-client -A 'delphinet.smartpy.io' -P 443 -S --wait none originate contract VestingTez \
  transferring 10000 from $BOB_ADDRESS running \
  "$(cat contracts/vesting_tez.tz | tr -d '\n')" \
  --init 'Pair (Pair "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm") (Pair 0 (Pair "2020-09-30T19:50:24Z" (Pair 30 1)))' \
  --burn-cap 0.2145

Node is bootstrapped, ready for injecting operations.
Estimated gas: 3980.424 units (will add 100 for safety)
Estimated storage: 858 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'oovpeFSr3uGRYpiESHHz5TX4BPoNqLJXyik47C1fhNmXYsouYBi'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for oovpeFSr3uGRYpiESHHz5TX4BPoNqLJXyik47C1fhNmXYsouYBi to be included --confirmations 30 --branch BM73QfE4sDUueErCgziLyKqUWkzLxqhTTWwJm37avbknc2b5oVM
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.00129
    Expected counter: 526600
    Gas limit: 4081
    Storage limit: 878 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............ -ꜩ0.00129
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,32) ... +ꜩ0.00129
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ10000
      Script:
        { ... }
        Initial storage:
          (Pair (Pair "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" "tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm")
                (Pair 0 (Pair "2020-09-30T19:50:24Z" (Pair 30 1))))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1SQEED3eYUSwh6CiU9MfELhVwHwsrF67Kn
        Storage size: 601 bytes
        Paid storage size diff: 601 bytes
        Consumed gas: 3980.424
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.15025
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.06425
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ10000
          KT1SQEED3eYUSwh6CiU9MfELhVwHwsrF67Kn ... +ꜩ10000

New contract KT1SQEED3eYUSwh6CiU9MfELhVwHwsrF67Kn originated.
Contract memorized as VestingTez.
```

Make a bash variable for the contract:

```bash
VESTING_TEZ="KT1SQEED3eYUSwh6CiU9MfELhVwHwsrF67Kn"
```

To vest some tez, submit the number of ticks to the `vest` entrypoint.

Submitting too many ticks will error:


```bash
$ tezos-client -A 'delphinet.smartpy.io' -P 443 -S --wait none transfer 0 from $BOB_ADDRESS to $VESTING_TEZ \
  --entrypoint vest --arg 100

Node is bootstrapped, ready for injecting operations.
This simulation failed:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0
    Expected counter: 526601
    Gas limit: 1040000
    Storage limit: 60000 bytes
    Transaction:
      Amount: ꜩ0
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT1SQEED3eYUSwh6CiU9MfELhVwHwsrF67Kn
      Entrypoint: vest
      Parameter: 100
      This operation FAILED.

Runtime error in contract KT1SQEED3eYUSwh6CiU9MfELhVwHwsrF67Kn:
  01: { parameter (or (option %setDelegate key_hash) (nat %vest)) ;
  02:   storage
  03:     (pair (pair %wrapped (address %target) (address %delegateAdmin))
  04:           (pair (nat %vested)
  05:                 (pair %schedule (timestamp %epoch) (pair (nat %secondsPerTick) (nat %tokensPerTick))))) ;
  06:   code { DUP ;
  07:          CAR ;
  08:          DIP { CDR } ;
  09:          IF_LEFT
  10:            { SWAP ;
  11:              DUP ;
  12:              DIP { CAR ;
  13:                    CDR ;
  14:                    SENDER ;
  15:                    COMPARE ;
  16:                    EQ ;
  17:                    IF { DIP { NIL operation } ; SET_DELEGATE ; CONS } { FAILWITH } } ;
  18:              SWAP ;
  19:              PAIR }
  20:            { PAIR ;
  21:              DUP ;
  22:              CAR ;
  23:              DIP { CDR ;
  24:                    DUP ;
  25:                    DIP { CAR } ;
  26:                    CDR ;
  27:                    DUP ;
  28:                    DIP { CDR } ;
  29:                    DUP ;
  30:                    CDR ;
  31:                    DIP { CAR } } ;
  32:              DUP ;
  33:              DIP { DIP { DIP { DUP } ;
  34:                          DUP ;
  35:                          CAR ;
  36:                          NOW ;
  37:                          SUB ;
  38:                          DIP { CDR ; CAR } ;
  39:                          EDIV ;
  40:                          IF_NONE { FAILWITH } { CAR } ;
  41:                          SUB ;
  42:                          ISNAT } ;
  43:                    SWAP ;
  44:                    IF_NONE
  45:                      { FAILWITH }
  46:                      { DIP { DUP } ; SWAP ; COMPARE ; LE ; IF { ADD } { FAILWITH } } ;
  47:                    DIP { DUP } ;
  48:                    SWAP ;
  49:                    DIP { PAIR ; SWAP ; DUP } ;
  50:                    CDR ;
  51:                    CDR } ;
  52:              MUL ;
  53:              SWAP ;
  54:              CAR ;
  55:              CONTRACT unit ;
  56:              IF_NONE
  57:                { FAILWITH }
  58:                { SWAP ;
  59:                  PUSH mutez 1 ;
  60:                  MUL ;
  61:                  UNIT ;
  62:                  TRANSFER_TOKENS ;
  63:                  DIP { NIL operation } ;
  64:                  CONS } ;
  65:              DIP { PAIR } ;
  66:              PAIR } } }
At line 46 characters 72 to 80,
script reached FAILWITH instruction
with 100
Fatal error:
  transfer simulation failed
```

But it will succeed for a valid number of ticks:

```bash
$ tezos-client -A 'delphinet.smartpy.io' -P 443 -S --wait none transfer 0 from $BOB_ADDRESS to $VESTING_TEZ \
  --entrypoint vest --arg 3

Node is bootstrapped, ready for injecting operations.
Estimated gas: 6565.902 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'ooDsQs3CN9jj6jdC2gNMAK4geCCxNK5mFBrPaviuVRvi2CYGVLT'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooDsQs3CN9jj6jdC2gNMAK4geCCxNK5mFBrPaviuVRvi2CYGVLT to be included --confirmations 30 --branch BLhQxk63cLpTSqvxP2YzTGEJQr84XfpDYrMcpFZvmNor96naRan
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.000929
    Expected counter: 526601
    Gas limit: 6666
    Storage limit: 0 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............ -ꜩ0.000929
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,32) ... +ꜩ0.000929
    Transaction:
      Amount: ꜩ0
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT1SQEED3eYUSwh6CiU9MfELhVwHwsrF67Kn
      Entrypoint: vest
      Parameter: 3
      This transaction was successfully applied
      Updated storage:
        (Pair (Pair 0x00003b5d4596c032347b72fb51f688c45200d0cb50db
                    0x0000aad02222472cdf9892a3011c01caf6407f027081)
              (Pair 3 (Pair 1601495424 (Pair 30 1))))
      Storage size: 601 bytes
      Consumed gas: 5138.902
    Internal operations:
      Transaction:
        Amount: ꜩ0.000003
        From: KT1SQEED3eYUSwh6CiU9MfELhVwHwsrF67Kn
        To: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
        This transaction was successfully applied
        Consumed gas: 1427
        Balance updates:
          KT1SQEED3eYUSwh6CiU9MfELhVwHwsrF67Kn ... -ꜩ0.000003
          tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ... +ꜩ0.000003
```

