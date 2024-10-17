#r "nuget:RProvider,2.1.0"

open RProvider
open RProvider.``base``
open RProvider.zoo

R.c (1., 2., 3)

R.as_zoo (R.c (1, 2, 3))
