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

Ecological systems are formed of a complex web of non-linear interactions. The field of long-term ecology (LTE) extends the view of ecology to longer timescales of decades to centuries. LTE is essential for estimating the resilience of ecosystems to environmental change [@Willis_Bailey_Bhagwat_Birks_2010]. LTE researchers employ environmental proxies – such as microfossils or wood rings – to reconstruct variability in biodiversity and abiotic factors in times before the recent observational period. Disparate dating methods and time systems are thus often required to connect measurements to timelines, for example by using radiocarbon dating. However, LTE methods naturally introduce greater uncertainty by being relatively removed from ecological processes. Although the long view is integral to comprehensively understand ecosystem responses to environmental change, there has been limited application of causal inference within LTE [@Willis_Araújo_Bennett_Figueroa-Rangel_Froyd_Myers_2007].

Mechanistic modelling approaches may be used to infer the form and strength of ecological processes. In *The Ecological Detective*, Hilborn and Mangel present a toolkit for confronting ecological models with data [@Hilborn_Mangel_1997]. Here, I developed a modelling framework to enable LTE researchers to apply the ecological detective approach to proxy time-series data for causal inference. Key capabilities of the framework include: a human-readable Domain Specific Language (DSL) for model definition with enforced dimensional correctness; model composition and scaffolding of multiple competing hypotheses; confrontation of models with time-series of disparate dating methods; and associated testing and diagnostics functions.

# Statement of need

## Research purpose

The primary research purpose of *Bristlecone* is to enable researchers to build causal knowledge for the role of ecological mechanisms in interactions between organisms and their environment over decades to centuries, through confrontation of models with proxy data. LTE is essential for future prediction of biodiversity responses to climate change, because (a) some ecological processes operate over decadal to centennial timescales (e.g., vegetation effects on soil properties and soil development [@McLauchlan_Gerhart_2017]); (b) responses can be better characterised by comparison to broader environmental envelopes; and (c) inference may be made about resilience and stable states based on past analogues. Much research in LTE has focused on description of past changes, as proxy datasets are relatively far removed from underlying ecological processes making inference more challenging. Despite these challenges, causal understanding in ecology is essential for predicting species responses to environmental change and management, as it is the required basis driving process rather than statistical models. Causal inference may be achieved through multiple lines of evidence, including (a) reductionist approaches utilising experimentation (such as factorial plots), (b) detecting associations in observational data in space and time, and (c) mechanistic investigation. For LTE, experimental approaches are not possible and surrogates such as space-for-time substitution are known to misrepresent the rate and order of processes [@Elmendorf_Henry_2015]; thus, mechanistic investigation is essential to build causal understanding. However, such investigation has been rarely applied in LTE.

In contemporary ecology (or ‘neo-ecology’), a key approach for causal inference is that of Hilborn and Mangel [@Hilborn_Mangel_1997], who provided a conceptual framework for the 'ecological detective'. Their approach recognises that to gain an understanding the ecological mechanisms underlying geo-temporal patterns requires: (1) examining existing and emerging theory to understand plausible mechanisms; (2) understanding process and observation uncertainties; and (3) confronting plausible alternatives with data. If we define causal mechanisms as "some collection of spatiotemporally contiguous structures and/or processes along which a signal can be propagated to produce a response" (@Grace_HuntingtonKlein_2025, p5), then the ecological detective approach may be used to gain understanding of causal mechanisms from LTE time-series data.

*Bristlecone* is a toolkit that enables LTE researchers to utilise the ecological detective approach by providing: (1) a human readable, succinct, and correct declarative grammar to conduct the workflow of the ‘ecological detective’; and (2) a suite of implementations of key components required to use the grammar. To enable ease of use within LTE, the toolkit includes a grammar of time that accurately represents the actual dating systems, methods and granularity in use within both neo-ecology and LTE. As well as LTE researchers, the library targets neo-ecologists who would like to integrate long-term ecological data into their research for multi-scale ecological analysis.

*Bristlecone* occupies a distinct gap within existing software tools. Although existing software tools exist provide data wrangling and inference capabilities, *Bristlecone* provides a domain-specific modelling language that is focused on achieving correctness in model construction from the perspective of the ecological domain. The aim is to avoid common errors and issues that arise using statistical libraries. In effect, *Bristlecone* is a grammatical layer that sits at a higher level of abstraction than optimisation libraries. General-purpose model inference engines – including Stan, JAGS, and others – may be used for time-series inference but often require users to manually construct representations of time and lack dimensional correctness. The Stan language itself does not support such type correctness [@Stan-Development-Team_2025]. *Bristlecone* provides that missing layer on top of model inference, while also formalising the broader ecological detective workflow.

## State of the field

The ecological community predominantly works within R and R packages, with long-term ecological research focusing on purely statistical methods. Key approaches include PCAs and CCAs, Generalised Additive Models (GAMs), and transfer functions for reconstructing past environments from microfossil assemblages. Some mechanistic approaches have been applied; in pollen analysis, the landscape model REVEALS [@Sugita_2007] and local extension LOVE [@Sugita_2007_2] integrate contemporary datasets on pollen productivity with models of lake basin pollen capture to simulate taxon-specific biomass from pollen fluxes. However, research that infers mechanism from long-term ecological time-series is of limited example (e.g., @Jeffers_Bonsall_Watson_Willis_2011). In tree-ring research, mechanistic and semi-mechanistic approaches are more broadly applied, including using VS-Lite – a model based on Liebig’s law of the minimum – to infer moisture and temperature limiting effects on tree growth [@TolwinskiWard_2011].

*Bristlecone* provides two unique contributions over existing libraries. First, it integrates the workflow of the ecological detective into a single conceptual framework. Second, no other existing library within the field provides the ecological and mathematical correctness that *Bristlecone*’s language was explicitly designed to provide. *Bristlecone* makes use of F#’s strong typing, meta-programming capabilities, and units of measure to require that ecological models are human readable and dimensionally correct. *Bristlecone* models therefore increase the transparency of ecological modelling research by having human readable model structures in supplementary code, clear declarations of the key methodological considerations applied, and reproducible analyses.

The core design of *Bristlecone* mandated a strongly typed language with compile-time errors, making R suboptimal. As a result, *Bristlecone* was designed as a new project to supply the grammar and conceptual framework; however, it applies a compositional approach whereby other underlying components, such as optimisation and integration routines, may be plugged into *Bristlecone* to support existing alternatives. For example, for optimisation and inference ecologists often write models in R and pass to an optimiser. The RProvider F# type provider provides programmatic access to R functions from F# [@RProvider] and presents an avenue for further integration between R routines and Bristlecone.

*Bristlecone*’s target audience is long-term and neo-ecologists who have yet to apply mechanistic modelling within their work. The library aims to reduce the technical and computational knowledge required to explore the ecological detective approach and integrate it into long-term ecological research projects.

## Software design

Although F# is a language that is not used widely in ecology, I judged that I could use its meta-programming capabilities to create a domain-specific language that would be intuitive for use by ecologists whose previous experience was with R. By doing so, I have been able to make full use of unique F# features, such as units of measure, to implement the intended design. The existence of RProvider as an F#-R interop layer was also a consideration, as it enables integration of *Bristlecone* with more familiar tools such as ggplot.

I sought to include all key concepts of the ‘ecological detective’ workflow in *Bristlecone*; the seven key stages are: (1) definition of time-series models and plausible alternative components; (2) definition of an estimation engine (which describes the model-fitting approach); (3) composition of alternative model hypotheses; (4) testing identifiability of the model given the estimation engine; (5) model-fitting; (6) model-selection; and (7) diagnostics.

![Figure 1](figure-1.png)
*Figure 1 Conceptual framework of Bristlecone. Stages of an analysis (green circles) are: (1) user defines ecological models; (2) user defines an estimation engine that characterises the model-fitting method; (3) multiple working hypotheses are established via model composition; (4) identifiability of the hypotheses is tested using the given model-fitting method (user revises models accordingly); (5) model-fitting occurs to real time-series data; (6) model selection; and (7) diagnostics. Brown = Bristlecone core functions; black = user-defined elements using the Domain Specific Language; blue = user-supplied data; white = functions external to Bristlecone’s framework.*

A core requirement was that model definitions should make full use of F# type safety and units of measure to ensure that (a) all equations are dimensionally consistent, and (b) model composition is also dimensionally consistent. To this aim, a DSL was designed that is based on unit-typed ‘model expressions’ that are composed using mathematical operators and as F# values and functions. The user may declare states, parameters, and external environmental forcings (e.g. air temperature, precipitation sum) with their ecologically meaningful units and insert these into the model expression structures to ensure that any proposed model system is dimensionally consistent.

```fsharp
/// The Schaefer model of fishing 
module Schaefer =

    let B = state<ton> "biomass"
    let I = measure<1> "CPUE" // Catch per unit effort
    let C = environment<ton> "catch"

    // Intrinsic growth rate
    let r = parameter "r" NoConstraints 0.01</year> 1.0</year>
    // Carrying capacity
    let K = parameter "K" NoConstraints 0.01<ton> 1.0<ton>
    // Catchability coefficient
    let q = parameter "q" NoConstraints 1e-6<1/ton> 1e-2<1/ton>
    // Biomass at time zero. Assumes stock unfished at start time
    let B0 = P K

    let dt = Constant 1.<year>
    let ``B_est[t+1]`` =
        State B + P r * State B * (Constant 1. - State B / P K) * dt - Environment C
    let It = P q * State B

    let sigma = parameter "sigma_v" NoConstraints 0.01 1.0
    let NLL = ModelLibrary.NegLogLikelihood.LogNormal (Require.measure I) sigma

    let model =
        Model.discrete<year>
        |> Model.addDiscreteEquation B ``B_est[t+1]``
        |> Model.addMeasure I It
        |> Model.initialiseHiddenStateWith B B0
        |> Model.useLikelihoodFunction NLL
```

The above simple example is a discrete-time model from *The Ecological Detective*. It demonstrates the enforcement of dimensional consistency in the model expression system: catch per unit efficiency must be dimensionless to work with lognormal observation error, for example.

I designed the expression of the estimation engine – meaning a declaration of a model-fitting approach – to require dimensional consistency with any model system. For example, the units of proposed state equations must be a rate over time for continuous-time engines, unlike discrete-time engines. Another integral part of the estimation engine is knowledge of how to translate between the temporal characteristics of proxy data and the temporal resolution of the model system and its parameters. My design goal was to allow the user to supply data using one specific dating method, and for the estimation engine to know how to relate it to the temporal resolution specified in the model. For example, if a plant growth model is specified at monthly resolution and includes monthly climatology, but annual growth ring width data is used as a proxy measure, then the estimation engine will encode the relationship between the annual data and the monthly model. Further, if a model is confronted with different sources of data that use different dating methods, the *Bristlecone* system explicitly handles the complexity of the temporal domain.

```fsharp
let engine: EstimationEngine.EstimationEngine<DatingMethods.Annual,int<year>,year,1> =
    Bristlecone.mkContinuous ()
    |> Bristlecone.withBristleconeOptimiser
    |> Bristlecone.withConditioning Conditioning.RepeatFirstDataPoint
    |> Bristlecone.withSeed 1500
    |> Bristlecone.withTimeConversion DateMode.Conversion.Annual.toYears
```

The above definition of an estimation engine connects a model defined in annual time with an annually resolved time-series dataset. It applies the default optimiser (a combination of fast simulated annealing and a homogeneous MCMC chain) and conditioning of the first time point.

For model composition, a simple pipe-based workflow was included that automatically scaffolds nested model systems into a list of hypotheses and assigns reference codes to each hypothesis. For example, the below indicates what model composition looks like for a model designed to investigate environmental controls to plant growth.

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

The above example scaffolds a list of model hypotheses that are defined in the temporal resolution of the base model, with a declarative form that is human readable.

Identifiability testing is an essential stage in the ecological detective workflow for holding confidence in inference results. A testing module using a similar pipeline approach was included. Using the module, tests may be constructed using synthetic data with defined error structures to understand the ability of the estimation engine and hypotheses to correctly infer parameters for different hypothetical time-series data.

For model-fitting, an orchestrator and multi-threaded loggers were included such that multiple hypotheses may be fit in parallel. For model-selection, I included constrained AIC and Akaike weights, as the framework currently focuses on information theoretic approaches. Model selection statistics are generated from the list of hypotheses given the resultant fits. Finally, root mean square error (RMSE) and n-step predictions were included as key diagnostics, to indicate goodness-of-fit. For further analysis, the DSL enables estimated parameters to be retrieved from results in their original units of measure, so that unit consistency crosses the boundary into onward analyses.

Through *Bristlecone*’s design and implementation, I sought to provide a framework that is understandable to long-term ecologists and supports the scientific process by raising compile-time errors during model prototyping. I also sought to make these models able to be interrogated intuitively when using time-series proxy data and explored for performance and identifiability.

### Pluggable routines and integration with other libraries

I designed *Bristlecone* as a high-level layer for conducting an ecological detective workflow. As a high-level structure, I thus designed the system to be extensible such that most underlying components and routines may be swapped with implementations from other libraries or ecosystems as required. Two key components that may be swapped are the integration and optimisation routines. For integration, only Runge-Kutta 4 algorithms are currently included.

The performance of optimisation routines is integral to successful model-fitting. The included optimisation routines aimed to yield acceptable performance and accuracy for a broad suite of ecological time-series model structures, while recognising that other alternatives with differing strengths and weaknesses are available. I included three classes: amoeba-based, simulated annealing, and broader MCMC based methods. First, amoeba-based methods have been applied to palaeoecological model-fitting and model selection [@Jeffers_Bonsall_Watson_Willis_2011] but are suited to simpler likelihood surfaces of less than ten parameters and are more prone to local minima. Second, I included Classical and Fast Simulated Annealing [@Lee_2015][@Szu_Hartley_1987]. Fast simulated annealing is more performant at consistently identifying global minima; Bristlecone's implementation has been applied in dendroecological modelling [@Martin_MaciasFauria_Bonsall_Forbes_Zetterberg_Jeffers_2021]. Third, I included basic Monte Carlo samplers such as adaptive metropolis, as well as Filzbach. Filzbach is a Monte Carlo-based optimisation and sampling routine specifically designed and previously used for fitting high-dimensional ecological models [@Purves_2016], previously used by Microsoft Research. As the existing Filzbach library is not actively developed, and F# math libraries also lacked SA implementations, new F# implementations for both were included here.

Some of the most efficient optimisation approaches require function gradients through automatic differentiation. To support, this the internals of *Bristlecone* use DiffSharp tensors [@baydin2015diffsharpautomaticdifferentiationlibrary]. Consequently, the model DSL enforces model designs as required for gradient-based methods.

## Research impact statement

*Bristlecone* has been developed openly since 2018 and has matured through application to research requirements. It has been formatted for wider community use and contribution, including documentation, examples, and contributor guidelines. Previous research applied multi-proxy palaeoecological data to infer the role of soil nutrients in plant productivity through the Holocene [@Jeffers_Bonsall_Watson_Willis_2011], but using custom C code. I reproduced their analysis using *Bristlecone*, which is included as a palaeoecology example. A benchmark suite is included to indicate the performance and speed of built-in optimisation routines across different model types. 

*Bristlecone* was integral to @martin2019a and @Martin_MaciasFauria_Bonsall_Forbes_Zetterberg_Jeffers_2021, where it was used to determine the role of soil nutrients in controlling Arctic shrub growth (*Bristlecone* v1). Alternative hypotheses of nutrient limitation were confronted with annual wood ring width and stable nitrogen isotope time-series over decadal time. An updated version of this analysis for *Bristlecone* v3 is given as an example. The software is  currently being applied to a pan-Arctic synthesis of palaeoecological records from the Arctic Holocene Biodiversiy Database [@Martin_Bell_Blake_Bradshaw_Kuoppamaa_Pavey_Prendin_Speight_Villar_Macias-Fauria_2024].

## AI usage disclosure

AI has not been used to write this manuscript.

Within the codebase, no AI tools were used prior to August 2025. From then, the Microsoft Copilot macOS app was used as an external tool to the development environment to suggest approaches to refactoring existing code snippets and potential approaches to support proposed features for version 3.0 and 3.1 of *Bristlecone*. Any code suggestions were critically examined and only implemented within the codebase manually after human oversight. No agents operated directly on the codebase.

## Acknowledgements

I thank long-standing support from Dr Elizabeth Jeffers and Prof Marc Macias Fauria. I also thank members of the Biogeosciences Lab (Scott Polar Research Institute) for ongoing support in identifying improvements to the library to support ongoing research projects.

## References
