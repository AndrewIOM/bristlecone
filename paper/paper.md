---
title: 'Bristlecone: an F# library for the long-term ecological detective'
tags:
  - F#
  - dendroecology
  - palaeoecology
  - time-series
  - mechanistic modelling
authors:
  - name: Andrew C. Martin
    orcid: 0000-0002-8236-980X
    affiliation: "1"
affiliations:
 - name: Scott Polar Research Institute, University of Cambridge, UK
   index: 1
date: 17 November 2025
bibliography: paper.bib
---

# Summary

Long-term ecology (LTE) seeks to extend our understanding of ecological systems to decades to centuries, beyond the observational period. LTE researchers employ environmental proxies (e.g., microfossils, wood rings) to reconstruct variability in biodiversity and environments over decades to centuries. Disparate dating methods (e.g., radiocarbon dating) and representations of time are thus usually required for interpretation. However, LTE methods naturally introduce greater uncertainty by being relatively removed from underlying ecological processes. Although the long view is integral to comprehensively understand ecosystem responses to environmental change, there has historically been limited application of causal inference within LTE [@Willis_Araújo_Bennett_Figueroa-Rangel_Froyd_Myers_2007].

Mechanistic modelling approaches may be used to infer the form and strength of ecological processes. In *The Ecological Detective*, @Hilborn_Mangel_1997 present a toolkit for confronting ecological models with data. Here, I developed a modelling framework to enable LTE researchers to apply the ecological detective approach to proxy time-series data for causal inference. Key capabilities of the framework include: a human-readable Domain Specific Language (DSL) for model definition with enforced dimensional correctness; model composition to scaffold competing hypotheses; confrontation of models with time-series of disparate dating methods; and associated diagnostics.

# Statement of need

## Research purpose

*Bristlecone*'s purpose is to enable researchers to build causal knowledge of the role of ecological mechanisms over decades to centuries, through confrontation of models with proxy data. LTE is essential in predicting biodiversity responses to climate change, as we may (indirectly) observe (a) ecological processes that operate over longer timescales (e.g., soil development [@McLauchlan_Gerhart_2017]); (b) biodiversity responses to broader environmental envelopes; and (c) ecological stable states and resilience [@Willis_Bailey_Bhagwat_Birks_2010]. Although LTE research has often been descriptive, causal understandings are essential for establishing predictive capability. Causal understanding may be achieved through combining multiple lines of evidence, including (a) experimentation, (b) identifying spatial-temporal associations in observational data, and (c) mechanistic investigation. For LTE, experimental approaches are not possible and surrogates such as space-for-time substitution can misrepresent the rate and order of processes [@Elmendorf_Henry_2015]; thus, mechanistic investigation is essential.

In contemporary ecology (‘*neo-ecology*’), the 'ecological detective' is a key approach for causal inference [@Hilborn_Mangel_1997]. The approach requires: (1) examining existing and emerging theory to understand plausible mechanisms; (2) understanding process and observation uncertainties; and (3) confronting plausible alternatives with data. Taking causal mechanisms as processes *"along which a signal can be propagated to produce a response"* (@Grace_HuntingtonKlein_2025, p5); the ecological detective approach may be applied to infer causal mechanisms using LTE time-series.

*Bristlecone* is a toolkit that enables LTE researchers to utilise the ecological detective approach by providing: (1) a human-readable, succinct declarative grammar for the ‘ecological detective’ workflow; and (2) implementations of key components required to use the grammar. The toolkit includes a grammar of time that reflects the dating methods in use within LTE and neo-ecology. As such, the library targets neo-ecologists who would like to integrate neo-ecological and LTE time-series for multi-scale analysis.

*Bristlecone* occupies a distinct niche: although tools exist for data wrangling and inference, *Bristlecone* is a high-level grammatical layer that focuses on correctness in model construction from the perspective of the ecological domain. General-purpose inference engines (e.g., Stan, JAGS) may be used for time-series inference but lack dimensional correctness and often require manual construction of time concepts. Stan's language, for example, does not support such type correctness [@Stan-Development-Team_2025]. *Bristlecone* provides that missing layer within a broader formalisation of the ecological detective workflow.

## State of the field

The ecological community predominantly utilises R. Within LTE, key statistical approaches include transfer functions for environmental reconstruction, generalised additive models, and canonical correspondence analyses. Proxy-specific mechanistic approaches have also been applied; for pollen, the landscape model REVEALS [@Sugita_2007] and local extension LOVE [@Sugita_2007_2] simulate taxon-specific biomass from pollen fluxes. However, mechanistic inference from long-term ecological time-series has been limited (e.g., @Jeffers_Bonsall_Watson_Willis_2011). In dendrochronology, (semi-)mechanistic approaches are more prevalent, such as VS-Lite for inferring temperature/moisture limits to tree growth [@TolwinskiWard_2011].

*Bristlecone*'s two unique contributions are: (1) integrating the ecological detective workflow within a single conceptual framework; and (2) providing dimensional correctness and ecological clarity. *Bristlecone* utilises F#’s type system and expressiveness to enforce dimensionally-consistent ecological models and encourage human readablity, improving transparency and reproducibility of supplementary model code. As the core design relies on F# language features that could not be replicated within R, *Bristlecone* was designed as a new project. However, it applies a compositional approach whereby required components (e.g. optimisation) may be plugged in where alternatives are beneficial. The F# R type provider [@RProvider] presents an avenue for further embedding of R libraries within *Bristlecone* through strongly typed R access.

*Bristlecone*’s target audience is long-term and neo-ecologists who have yet to explore mechanistic modelling. The library aims to reduce the computational knowledge required to utilise the ecological detective approach within long-term ecological research.

## Software design

Although F# is uncommon in ecological research, I considered that an F#-based domain-specific language could be intuitive for ecologists with R experience. To improve familiarity I considered connections to R package via *RProvider*, for example for visualisation. I sought to include the seven stages of any ‘ecological detective’ workflow in *Bristlecone*: (1) definition of time-series models; (2) specifying model-fitting methods ("estimation enignes"); (3) composition of alternative model hypotheses; (4) identifiability testing; (5) model-fitting; (6) model-selection; and (7) diagnostics.

![Figure 1](figure-1.png)
*Figure 1 Conceptual framework. Green circles indicate stage of analysis. Box colours: brown = Bristlecone core functions; black = user-defined elements using the Domain Specific Language; blue = user-supplied data; white = external functions.*

A core requirement was that ecological models should utilise F# type safety and units of measure such that all models, their composition, and methods are dimensionally consistent. I designed a DSL that ensures dimensional consistency across equations, parameters, external forcings (e.g. climate), and methods, supporting the scientific process by raising compile-time errors and methodlogical incompatabilities during prototyping.

To demonstrate, fish stocks (see: *The Ecological Detective*, Chapter 10) may be represented in discrete-time by:

```fsharp
module Schaefer =

    let B = state<kton> "biomass"
    let I = measure<1> "CPUE" // Catch per unit effort
    let C = environment<kton> "catch"

    let r = parameter "r" Positive 0.01</year> 1.0</year> // Intrinsic growth rate
    let K = parameter "K" Positive 100.<kton> 2000.<kton> // Carrying capacity
    let q = parameter "q" Positive 1e-6<1/kton> 1e-2<1/kton> // Catchability
    let B0 = P K

    let dt = Constant 1.<year>
    let ``B_est[t+1]`` =
        let n = State B + P r * State B * (Constant 1. - State B / P K) * dt - Environment C
        Conditional (n .> Constant 0.<kton>) n (Constant 0.<kton>)

    let It = P q * State B

    let sigma = parameter "sigma_v" NoConstraints 0.01 1.0
    let NLL = ModelLibrary.NegLogLikelihood.LogNormal (Require.measure I) sigma

    let model =
        Model.discrete<year>
        |> Model.addDiscreteEquation B ``B_est[t+1]``
        |> Model.addMeasure I It
        |> Model.initialiseHiddenStateWith B B0
        |> Model.estimateParameter r
        |> Model.estimateParameter K
        |> Model.estimateParameter q
        |> Model.useLikelihoodFunction NLL
        |> Model.compile
```

Here, 'catch per unit effort' must be dimensionless as required by lognormal observation error, and ``B_est[t+1]`` must result in `kton`. If the model was continuous-time, `B` would equal ``kton/year``. The entire model pipeline (i.e., state initialisers, likelihoods, measures) is unit-aware, preventing many model definition errors at compile-time.

I designed Bristlecone's *estimation engine* so that models and data may be combined across time concepts, with engines ensuring temporal consistency at compile-time. Radiocarbon, contemporary calendar, and AD/BC time-series may all be applied or combined with models of various resolutions; the only requirement is that appropriate temporal conversions are setup within the engine. For example, a monthly plant growth model may be combined with monthly climate and lower-resolution annual wood ring data, given specification of growing-season end date. By combining common ecological models to be confronted with different (proxy) time-series, the strength and importance of mechanisms over different timescales (e.g., observational- versus centennial-time) may be investigated.

To compose alternative mechanisms, I designed a pipe-based workflow that turns nested model systems into a set of hypotheses while ensuring dimensional consistency between sub-components:

```fsharp
let hypotheses =
    ``base model``
    |> Hypotheses.createFromModel
    |> Hypotheses.apply ``geometric constraint``
    |> Hypotheses.apply ``plant-soil feedback``
    |> Hypotheses.apply ``temperature limitation to growth``
    |> Hypotheses.apply ``N-limitation to growth``
    |> Hypotheses.compile
```

Identifiability testing - (assessing if the engine and model can infer known parameters) - is essential for valid inference. I designed a pipeline-based testing suite where synthetic time-series data can be generated and applied to test identifiability of model-engine combinations. For model-fitting, an orchestrator and multi-threaded loggers were included for multi-threaded fitting. For model-selection, I included Akaike weights, as *Bristlecone* currently focuses on information theoretic approaches. Finally, root mean square error (RMSE) and n-step predictions were included as goodness-of-fit diagnostics. For further analysis, estimated parameters must be retrieved in original units of measure to enforce onward unit consistency.

### Integration with other libraries

I designed *Bristlecone* as an abstraction for conducting an ecological detective workflow; it is thus extensible such that underlying components, such as integration and optimisation routines, may be substituted. To enable gradient-based approaches, *Bristlecone* utilises DiffSharp tensors to support automatic differentiation[@baydin2015diffsharpautomaticdifferentiationlibrary].

A suite of optimisation routines broadly suited to ecological time-series was included, while recognising that more suitable alternatives for specific problems may be substituted. First, included amoeba-based methods have been applied to palaeoecological inference [@Jeffers_Bonsall_Watson_Willis_2011] but are best suited to simpler likelihood surfaces. Second, I included Classical and Fast Simulated Annealing [@Lee_2015][@Szu_Hartley_1987]. Fast simulated annealing is more performant at consistently identifying global minima; *Bristlecone*'s implementation has been applied in dendroecology [@Martin_MaciasFauria_Bonsall_Forbes_Zetterberg_Jeffers_2021]. Third, I included Monte Carlo methods including Filzbach, a meta-heuristic designed specifically for high-dimensional ecological models [@Purves_2016]. As the existing Filzbach library is not actively developed, and F# math libraries lack annealing methods, I implemented these within *Bristlecone*.

## Research impact statement

*Bristlecone* has been developed openly since 2018, maturing through research requirements. It has been formatted for community use and contribution, including documentation, examples, a benchmark suite, and contributor guidelines. Previous research applied multi-proxy palaeoecological data to infer the role of soil nutrients in plant productivity through the Holocene [@Jeffers_Bonsall_Watson_Willis_2011] using custom C; I reproduced that analysis as a *Bristlecone* example.

*Bristlecone* was integral to @martin2019a and @Martin_MaciasFauria_Bonsall_Forbes_Zetterberg_Jeffers_2021, where it was applied to determine the role of soil nutrients in controlling Arctic shrub growth (*Bristlecone* v1). Alternative hypotheses of nutrient limitation were confronted with wood ring and nitrogen isotope time-series over 30 to 80 years. That analysis is included as an example, formatted for *Bristlecone* v3. The software is currently being applied to a pan-Arctic synthesis of Arctic palaeoecological records [@Martin_Bell_Blake_Bradshaw_Kuoppamaa_Pavey_Prendin_Speight_Villar_Macias-Fauria_2024].

## AI usage disclosure

AI has not been used to write this manuscript.

No AI tools were used prior to August 2025. From then, the Microsoft Copilot macOS app was used as an external tool to the development environment to suggest missing tests, code snippet refactors, potential approaches to proposed features for v3.0/3.1. Code suggestions were critically examined and if beneficial implemented manually with human oversight. In some cases, AI code suggestions were pasted into the development environment as a starting point and then adapted to meet requirements manually. No agents have been used.

## Acknowledgements

I thank support from Dr Elizabeth Jeffers and Prof Marc Macias-Fauria.

## References
