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

Ecosystems are shaped by complex, non‑linear interactions that operate over different timescales and result in noisy time‑series. The field of *long‑term ecology* seeks to understand how ecosystems have responded to past environmental change across decadal to centennial timescales, using proxies such as tree rings, microfossils (pollen, diatoms, etc.), and sedimentary records.

Bristlecone is an F# library that implements the approach set out in *the ecological detective* [@Hilborn_Mangel_1997], enabling researchers to link mechanistic models with long‑term ecological data. It provides a domain‑specific language (DSL) for succinct, type‑safe model specification, native support for specialist time systems, and workflows for hypothesis testing and model selection. Long‑term ecology has historically relied on descriptive and correlative approaches, which have provided valuable insights [@Willis_Araújo_Bennett_Figueroa-Rangel_Froyd_Myers_2007]. However, to build towards causal and predictive insights we must interrogate the mechanisms driving long‑term ecological patterns. By lowering barriers to mechanistic inference, Bristlecone makes ecological detective work accessible to ecologists with limited programming experience working with both long‑term and other ecological time‑series.

# Statement of need

### Research Purpose.
Bristlecone is an F# library for mechanistic inference from long‑term ecological time‑series, leveraging F#’s strong typing and units of measure for correctness. It focuses on four key priorities:

1. Succinct, type‑safe time‑series models.
2. Modular hypothesis scaffolding.
3. Native support for specialist time systems.
4. Integrated workflow for model fitting and selection.

The DSL supports differential or discrete time models, enforcing dimensional correctness at compile‑time through F# units of measure. F# quotations compile models into tensor forms for computational performance leveraging DiffSharp [@baydin2015diffsharpautomaticdifferentiationlibrary].
Alternative ecological mechanisms can be modularly expressed and nested within base models for systematic comparison and model selection.
The `Bristlecone.Time` module handles calibrated/uncalibrated radiocarbon dates in years before present, AD/BC/BCE, Gregorian calendar, and varying temporal resolutions, with automatic conversion between model and data time systems.
In the model-fitting and model-selection workflow, model fitting uses maximum likelihood estimation with optimisation routines including Filzbach [@Purves_2016], simulated annealing, and Nelder–Mead simplex. Specialist dendroecology functions extend the workflow, such as incorporating sunlight hours into models to reflect seasonal cycles in light availability relevant to plant growth and wood ring formation [@martin2019a].

### Research Context.

The ecological detective framework has been applied to palaeoecological and dendroecological time‑series [@Jeffers_Bonsall_Watson_Willis_2011; @Martin2022]. Some prior research relied on custom C code [@Jeffers_Bonsall_Watson_Willis_2011], though general-purpose frameworks such as Stan [@Stan-Development-Team_2025] can also be applied. Stan is widely used, but domain-agnostic: users must manually manage dimensional correctness, time conversions, and hypothesis scaffolding.

Bristlecone’s ecological specificity - strongly‑typed models, native time handling, and modular hypothesis comparison - reduces errors and lowers barriers, supporting adoption of the ecological detective approach, as demonstrated in published applications [@Martin2022].

### Ongoing research.

Bristlecone has already been applied to dendroecological and isotope time‑series [@Martin_Macias‐Fauria_Bonsall_Forbes_Zetterberg_Jeffers_2021].

Tensor compilation and tensor-based internal types lay the foundation for future gradient‑based optimisation. A feature in development but not yet available is an F# type provider for direct, strongly‑typed access to the NOAA Palaeoecology Database and International Tree Ring Database.

Ongoing research is applying Bristlecone to a large‑scale synthesis of pan‑Arctic palaeoecological datasets that have been recently compiled as part of a systematic mapping exercise [@Martin_Assmann_Bradshaw_Kuoppamaa_Kuosmanen_Normand_Speed_Macias-Fauria_2022; @Martin_Bell_Blake_Bradshaw_Kuoppamaa_Pavey_Prendin_Speight_Villar_Macias-Fauria_2024].

# Acknowledgements

I acknowledge contributions from Elizabeth Jeffers and Marc Macias-Fauria in their support for this project.

# References

# Citations
