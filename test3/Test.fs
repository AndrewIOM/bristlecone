namespace Test

open RProvider
open RProvider.zoo

module X =

    let x = R.c(1.,2.,3)

    let y = R.as_zoo(x)