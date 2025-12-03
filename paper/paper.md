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

The structure and function of ecosystems reflects the complex web of interactions
that underpin them. Many interactions between organisms and their environment
are non-linear, with ecological data notoriously noisy. The field of *long-term ecology* (as opposed to *neo-ecology*) seeks to describe and understand how ecosystems have responded to past environmental over decadal to centennial timescales. Key methods include
the study of tree rings, microfossil counts (e.g. pollen, diatoms) in lake sediments and peat deposits, and historical texts.
The field of study is now over a century old, but has been historically rooted in descriptive methods and narrative accounts of data. However, to understand the resilience of ecosystems and look to future predictions, we must interrogate the underlying mechanisms leading to long-term ecological patterns. 

Within the frame of neo-ecology, @Hilborn_Mangel_1997 outlined an approach in *The Ecological Detective* that may be used to link models and data through mathematical models. Here, I implemented this toolkit for use by both neo-ecologists and long-term ecologists.


# Statement of need

### Research Purpose.
`Bristlecone` is an F# library for inference of the structure and strength of ecological processes from long-term ecological time-series data. F# is a strongly typed functional language that provides key features for ensuring expressiveness and correctness, such as units of measure. The library addresses four key priorities:

1. Write ecological time-series models succinctly and with minimal compile-time error.
2. Scaffold systems of hypotheses from alternative plausible ecological mechanisms.
3. Work freely across specialist time systems common in both long-term and neo ecology.
4. Confront models with long-term ecological data using appropriate and performant approaches.

First, `Bristlecone` includes a domain-specific language (DSL) for writing time-series models in either differential or discrete time, with full support for F# units of measure to ensure dimensional correctness at compile-time. F# quotations are used internally to compile models into tensor forms for performance.

Second, the DSL enables expression of alternative theoretical representations of ecological mechanisms, and their insertion into the appropriate time-series model, again with full units of measure support. Nested hypotheses may be scaffolded by combining these model components with a base model, ready for model competition.

Third, long-term ecology relies on specialist dating methods and date units. The `Bristlecone.Time` module enables both models and data to be expressed in calibrated (cal yr BP) and uncalibrated (BP) years before present, AD / BC / BCE, standard calendar systems (e.g. Gregorian), and at varying temporal resolution (e.g. day, month, annual). Models are expressed in model time units, such as centennial, with the conversion between model time and the time system of the data occurring automatically.

Fourth, `Bristlecone` contains a full workflow for model-fitting and model-selection based on maximum likelihood estimation. It includes a broad selection of optimisation routines including: Filzbach [@Purves_2016], a monte-carlo  method suited to noisy ecological data; simulated annealing; amoeba-based methods; and others. The full workflow remains performant by utilising tensors [@baydin2015diffsharpautomaticdifferentiationlibrary]; as this naturally supports full auto-differentiation capability, utilisation of gradients (e.g. in optimisation) is plausible in future.

For tree ring analysis (dendroecology), specialist functions in `Bristlecone.Dendro` are included, such as for incorporating sunlight hours for seasonality.

### Research Context.

The approaches of the *ecological detective* have been applied to sedimentary and dendroecological time-series. @Jeffers_Bonsall_Willis_2011 investigated tree population and nutrient dynamics in Hungary from 16,000 to 8,000 cal yr BP during end of the last glacial. Similar work has investigated: plant-plant interactions over successional timescales [Jeffers_Bonsall_Brooks_Willis_2011]; and the most plausible mechanisms relating fire, herbivory, and temperature to ecosystem functioning with a multi-proxy dataset [Jeffers_Bonsall_Watson_Willis_2011].

In dendroecology, the approach was used to infer spatial heterogeneity in the nature and strength of the mechanisms governing long-term shrub-nutrient relations in the Arctic tundra over decades [@Martin2022].

Much of the aforementioned research relied on custom C implementations. Although general purpose frameworks - such as Stan [@Stan] - may be used for long-term ecological modelling, Bristlecone provides model correctness, hypothesis scaffolding, and specialist time encapsulations for domain-specific modelling. F# is particularly suited for ecological modelling because...

### Ongoing research.

`Bristlecone` was designed to encapsulate the principles of the *ecological detective* toolkit such that they may be applied more readily by long-term ecological researchers. An earlier version of `Bristlecone` was applied in peer-reviewed research using wood ring and stable isotope time-series ([@Martin2022]). More broadly, an F# type provider is being developed for inclusion in `Bristlecone` that will allow direct connection to time-series data from the NOAA Palaeoecology Database and International Tree Ring Database, all with strongly-typed time systems and data units. `Bristlecone` is also being applied in a large-scale data analysis of newly compiled pan-Arctic sedimentary records.

# Acknowledgements

I acknowledge contributions from Elizabeth Jeffers and Marc Macias-Fauria in their support for this project.


# References

# Citations
