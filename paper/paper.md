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
    affiliation: "1" # (Multiple affiliations must be quoted)
affiliations:
 - name: Scott Polar Research Institute, University of Cambridge, UK
   index: 1
date: 17 November 2025
bibliography: paper.bib
---

# Summary

Ecological systems are formed of a complex web of non-linear interactions. The field of long-term ecology (LTE) extends the view of ecology over decadal to centennial timescales. LTE researchers employ environmental proxies – such as microfossils or wood rings – to reconstruct variability in biodiversity and abiotic factors in times before the recent observational period. Disparate dating methods and time systems are thus often required to connect measurements to timelines, for example by using radiocarbon dating. However, LTE methods naturally introduce greater uncertainty by being relatively removed from ecological processes. Although the long view is integral to comprehensively understand ecosystem responses to environmental change, there has been limited application of causal inference within LTE [@Willis_Araújo_Bennett_Figueroa-Rangel_Froyd_Myers_2007].

Mechanistic modelling approaches may be used to infer the form and strength of ecological processes. In *The Ecological Detective*, @Hilborn_Mangel_1997 present a toolkit for confronting ecological models with data. Here, I developed a modelling framework to enable LTE researchers to apply the ecological detective approach to proxy time-series data for causal inference. Key capabilities of the framework include: a human-readable Domain Specific Language (DSL) for model definition with enforced dimensional correctness; model composition and scaffolding of multiple competing hypotheses; confrontation of models with time-series of disparate dating methods; and associated testing and diagnostics functions.

# Statement of need

## Research purpose

*Bristlecone*'s purpose is to enable researchers to build causal knowledge of the role of ecological mechanisms in interactions between organisms and their environment over decades to centuries, through confrontation of models with proxy data. LTE is essential in predicting biodiversity responses to climate change, as (a) many ecological processes operate over decadal to centennial timescales (e.g., soil development [@McLauchlan_Gerhart_2017]); (b) responses may be (indirectly) observed over broader environmental envelopes; and (c) inference may be made about resilience and stable states based on past analogues [@Willis_Bailey_Bhagwat_Birks_2010]. Although LTE research has often relied on descriptive analysis, causal understandings are essential for gaining process-based predictive insights. Causal inference may be achieved through multiple lines of evidence, including (a) experimentation, (b) detecting associations in observational data in space and time, and (c) mechanistic investigation. For LTE, experimental approaches are not possible and surrogates such as space-for-time substitution are known to misrepresent the rate and order of processes [@Elmendorf_Henry_2015]; thus, mechanistic investigation is essential for building causal understanding.

In contemporary ecology (‘*neo-ecology*’), a key approach for causal inference is that of Hilborn and Mangel [@Hilborn_Mangel_1997], who provided a conceptual framework for the 'ecological detective'. Their approach requires: (1) examining existing and emerging theory to understand plausible mechanisms; (2) understanding process and observation uncertainties; and (3) confronting plausible alternatives with data. If we define causal mechanisms as *"some collection of spatiotemporally contiguous structures and/or processes along which a signal can be propagated to produce a response"* (@Grace_HuntingtonKlein_2025, p5), then the ecological detective approach may be used to gain understanding of causal mechanisms from LTE time-series data.

*Bristlecone* is a toolkit that enables LTE researchers to utilise the ecological detective approach by providing: (1) a human readable, succinct, and correct declarative grammar to conduct the workflow of the ‘ecological detective’; and (2) a suite of implementations of key components required to use the grammar. To enable ease of use within LTE, the toolkit includes a grammar of time that accurately represents the actual dating systems, methods and granularity in use within both neo-ecology and LTE. As well as LTE researchers, the library targets neo-ecologists who would like to integrate long-term ecological data into their research for multi-scale ecological analysis.

*Bristlecone* occupies a distinct niche: although existing tools exist provide data wrangling and inference capabilities, *Bristlecone* provides a domain-specific modelling language that is focused on achieving correctness in model construction from the perspective of the ecological domain. The aim is to avoid common errors and issues that arise using statistical libraries. In effect, *Bristlecone* is a grammatical layer that sits at a higher level of abstraction than optimisation libraries. General-purpose model inference engines – including Stan, JAGS, and others – may be used for time-series inference but often require users to manually construct representations of time and lack dimensional correctness. The Stan language itself does not support such type correctness [@Stan-Development-Team_2025]. *Bristlecone* provides that missing layer on top of model inference, while also formalising the broader ecological detective workflow.

## State of the field

The ecological community predominantly works within R. For LTE, key statistical approaches include transfer functions for reconstructing past environments from microfossil assemblages, generalised additive models, and canonical correspondence analyses. Proxy-specific mechanistic approaches have also been applied; for pollen, the landscape model REVEALS [@Sugita_2007] and local extension LOVE [@Sugita_2007_2] simulate taxon-specific biomass from pollen fluxes. However, research that infers mechanism from long-term ecological time-series is limited (e.g., @Jeffers_Bonsall_Watson_Willis_2011). In tree-ring research, (semi-)mechanistic approaches are more broadly applied, such as VS-Lite for inferring moisture and temperature limiting effects on tree growth [@TolwinskiWard_2011].

*Bristlecone* provides two unique contributions over existing libraries. First, it integrates the workflow of the ecological detective into a single conceptual framework. Second, no other existing library provides the mathematical correctness and ecological clarity of *Bristlecone*’s language. *Bristlecone* makes use of F#’s strong typing, meta-programming capabilities, and units of measure to require that ecological models are human readable and dimensionally correct. *Bristlecone* models therefore increase transparency of ecological modelling research by having human readable model structures in supplementary code, clear declarations of the key methodological considerations applied, and reproducible analyses.

The core design of *Bristlecone* mandated a strongly typed language with compile-time errors, making R suboptimal. As a result, *Bristlecone* was designed as a new project to supply the grammar and conceptual framework; however, it applies a compositional approach whereby other underlying components, such as optimisation and integration routines, may be plugged into *Bristlecone* to support existing alternatives. For example, for optimisation and inference ecologists often write models in R and pass to an optimiser. The RProvider F# type provider provides programmatic access to R functions from F# [@RProvider] and presents an avenue for further integration between R routines and Bristlecone.

*Bristlecone*’s target audience is long-term and neo-ecologists who have yet to apply mechanistic modelling within their work. The library aims to reduce the technical and computational knowledge required to explore the ecological detective approach and integrate it into long-term ecological research projects.

## Software design

Although F# is uncommon in ecological research, I considered that I could apply it's meta-programming capabilities to create a domain-specific language that would be intuitive for use by ecologists whose previous experience is with R. To improve familiarity, I considered connections to R package via *RProvider* where appropriate (e.g. visualisation). I sought to include the seven key stages of the ‘ecological detective’ workflow in *Bristlecone*: (1) definition of time-series models; (2) specifying model-fitting methods ("estimation enigne"); (3) composition of alternative model hypotheses; (4) identifiability testing; (5) model-fitting; (6) model-selection; and (7) diagnostics.

![Figure 1](figure-1.png)
*Figure 1 Conceptual framework. Green circles indicate stage of analysis. Box colours: brown = Bristlecone core functions; black = user-defined elements using the Domain Specific Language; blue = user-supplied data; white = external functions.*

A core requirement was that ecological models should make full use of F# type safety and units of measure to ensure that all equations, model composition, and model-fitting are dimensionally consistent. I created a DSL that expresses arithmetic, states, parameters, and external environmental forcings (e.g. air temperature) with their ecologically meaningful units. Dimensional consistency is always ensured.

To demonstrate, the Schaefer model (see: *The Ecological Detective*, Chapter 10) may be defined as:

```fsharp
module Schaefer =

    let B = state<kton> "biomass"
    let I = measure<1> "CPUE" // Catch per unit effort
    let C = environment<kton> "catch"

    let r = parameter "r" Positive 0.01</year> 1.0</year> // Intrinsic growth rate
    let K = parameter "K" Positive 100.<kton> 2000.<kton> // Carrying capacity
    let q = parameter "q" Positive 1e-6<1/kton> 1e-2<1/kton> // Catchability coefficient
    let B0 = P K // Biomass at time zero.

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

Here, catch per unit efficiency must be dimensionless as required by lognormal observation error, and ``B_est[t+1]`` must result in `kton`. If the model was continuous-time, it would require ``kton/year``. The entire model pipeline - state initialisers, likelihoods, measures - are unit-aware, preventing many model definition errors at compile-time.

I designed Bristlecone's *estimation engine* so that users may combine models and data of different temporal properties, with the engine configured to translate time concepts, which ensures temporal consistency at compile-time. Radiocarbon, modern calendar, and AD/BC dated time-series may all be applied or combined with models of various resolutions; the only requirement is that the appropriate temporal conversions are applied to the engine. As such, the same ecological models may be confronted with different sources of proxy data and dating methods; such an approach may be applied to compare the strength and importance of mechanisms over different timescales. For example, a monthly plant growth model may be combined with annual wood ring and monthly climate data, but requires a translation function from annual to monthly time (e.g., by  growing season end date).

For model composition, I designed a pipe-based workflow that turns nested model systems into a list of hypotheses (ensuring dimensional consistency) and assigns reference codes to each hypothesis:

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

Identifiability testing - assessing if the engine and model can infer known parameters - is essential for valid inference. I designed a pipeline-based testing suite where user-specified rules for generating synthetic time-series data can be applied to engine-model combinations to test identifiability, covering common scenarios for LTE data. For model-fitting, an orchestrator and multi-threaded loggers were included such that multiple hypotheses may be fit in parallel. For model-selection, I included constrained AIC and Akaike weights, as *Bristlecone* currently focuses on information theoretic approaches. Finally, root mean square error (RMSE) and n-step predictions were included as key diagnostics, to indicate goodness-of-fit. For further analysis, the DSL causes estimated parameters to be retrieved in original units of measure, so that unit consistency crosses the boundary into onward analyses.

Through *Bristlecone*’s design and implementation, I sought to provide a framework that is understandable to long-term ecologists and supports the scientific process by raising compile-time errors during model prototyping. I also sought to make these models able to be interrogated intuitively when using time-series proxy data and explored for performance and identifiability.

### Integration with other libraries

I designed *Bristlecone* as a high-level layer for conducting an ecological detective workflow; it is thus extensible such that underlying components may be swapped with other implementations as required. Two key components that are replacable within estimation engines are the integration and optimisation routines.

The performance of optimisation routines is integral to successful inference. To support gradient-based methods, the internals of *Bristlecone* run using DiffSharp tensors [@baydin2015diffsharpautomaticdifferentiationlibrary]. Consequently, the model DSL enforces model designs as required for gradient-based methods. A suite of optimisation routines broadly suited to ecological time-series was included, while recognising that alternatives exist. The suite includes amoeba, simulated annealing, and MCMC-based methods. First, amoeba-based methods have been applied to palaeoecological model-fitting and model selection [@Jeffers_Bonsall_Watson_Willis_2011] but are best suited to simpler likelihood surfaces. Second, I included Classical and Fast Simulated Annealing [@Lee_2015][@Szu_Hartley_1987]. Fast simulated annealing is more performant at consistently identifying global minima; Bristlecone's implementation has been applied in dendroecological modelling [@Martin_MaciasFauria_Bonsall_Forbes_Zetterberg_Jeffers_2021]. Third, I included Filzbach, a Monte Carlo-based optimisation and sampling routine designed for fitting high-dimensional ecological models [@Purves_2016]. As the existing Filzbach library is not actively developed, and F# math libraries also lacked SA implementations, I included new implementations in *Bristlecone*.

## Research impact statement

*Bristlecone* has been developed openly since 2018 and has matured through application to research requirements. It has been formatted for wider community use and contribution, including documentation, examples, and contributor guidelines. Previous research applied multi-proxy palaeoecological data to infer the role of soil nutrients in plant productivity through the Holocene [@Jeffers_Bonsall_Watson_Willis_2011], but using custom C code. I reproduced their analysis using *Bristlecone*, which is included as a palaeoecology example. A benchmark suite is included to indicate the performance and speed of built-in optimisation routines across different model types. 

*Bristlecone* was integral to @martin2019a and @Martin_MaciasFauria_Bonsall_Forbes_Zetterberg_Jeffers_2021, where it was used to determine the role of soil nutrients in controlling Arctic shrub growth (*Bristlecone* v1). Alternative hypotheses of nutrient limitation were confronted with annual wood ring width and stable nitrogen isotope time-series over decadal time. An updated version of this analysis for *Bristlecone* v3 is given as an example. The software is  currently being applied to a pan-Arctic synthesis of palaeoecological records from the Arctic Holocene Biodiversiy Database [@Martin_Bell_Blake_Bradshaw_Kuoppamaa_Pavey_Prendin_Speight_Villar_Macias-Fauria_2024].

## AI usage disclosure

AI has not been used to write this manuscript.

Within the codebase, no AI tools were used prior to August 2025. From then, the Microsoft Copilot macOS app was used as an external tool to the development environment to suggest approaches to refactoring existing code snippets and potential approaches to support proposed features for version 3.0 and 3.1 of *Bristlecone*. Any code suggestions were critically examined and only implemented within the codebase manually after human oversight. In some cases, AI code suggestions were pasted into the development environment as a starting point and then refactored and adapted to meet requirements manually. No agents operated directly on the codebase.

## Acknowledgements

I thank long-standing support from Dr Elizabeth Jeffers and Prof Marc Macias Fauria.

## References
