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
    orcid: 0000-0000-0000-0000
    affiliation: "1" # (Multiple affiliations must be quoted)
affiliations:
 - name: Scott Polar Reesearch Institute, University of Cambridge, UK
   index: 1
date: 17 November 2025
bibliography: paper.bib
---

# Summary

The structure and function of ecosystems reflects the complex web of interactions
that underpin them. Many of these interactions between organisms and their environment
are non-linear, with ecological data notoriously noisy. Over decadal to centennial time,
the field of *long-term ecology* seeks to describe and understand how ecosystems 
have responded to past environmental change. Key methods include
the study of tree rings, and of microfossils (e.g. pollen, diatoms) in lake sediments and peat desposits.
The field is now over a century old, but has been historically rooted in description.
To interrogate the mechanisms leading to long-term ecological patterns, a common
blueprint of the "ecological detective" may be followed.

# Statement of need

**Research Purpose**. `Bristlecone` is an F# library for inference of ecological processes from
long-term ecological time-series data. F# is a strongly typed functional language that
provides key features for ensuring expressiveness and correctness,
such as units of measure. `Bristlecone` includes a domain-specific language (DSL)
for writing time-series models in either differential or discrete time,
with full support for F# units of measure to ensure dimensional correctness.
These models may then be applied in a full model-fitting and model-selection
procedure using included optimisers and integrators, some of which are specialised
to noisy ecological data [@Filzbach]. `Bristlecone` applies [@DiffSharp]
for performant tensor-based computation when applying DSL models.
Within the generic time-series and timeframe types and functions in `Time`, time-series
may be represented on Gregorian calendar date and time systems, but also in radiocarbon
years and archaeological dating methods. `Bristlecone` manages the connection
between model - data time units automatically. For dendroecology, specialist
functions in `Bristlecone.Dendro` are included, such
as for incorporating sunlight hours for incorporating seasonality.

**Research Context**. Simple top-down time-series models have been applied
in the principles of the ecological detective to both palaeoecological and
dendroecological proxy data, including: identifying the most plausable feedback mechanisms
between vegetation and each of herbivory, climate, and fire at the millenial timescale
from lake sediment data [@Jeffers2011]; and inferring spatial heterogeniety in the
nature and strength of the mechanisms governing long-term shrub-nutrient relations
in the Arctic tundra over decades [@Martin2022] to centuries. 

**Ongoing research**. `Bristlecone` was designed to encapsulate the principles of the
"ecological detective" toolkit such that they may be applied more readily by
long-term ecological researchers. An earlier version of `Bristlecone` was
applied in peer-reviewed research using wood ring and stable isotope
time-series ([@Martin2022]). More broadly,
an F# type provider is being developed for inclusion in `Bristlecone` that will
allow direct connection to
time-series data from the NOAA Palaeoecology Database and International Tree Ring
Database, all with strongly-typed time systems and data units.

# Acknowledgements

I acknowledge contributions from Elizabeth Jeffers and Marc Macias-Fauria in
their support for this project.

# References


# Citations

Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

If you want to cite a software repository URL (e.g. something on GitHub without a preferred
citation) then you can do it with the example BibTeX entry below for @fidgit.

For a quick reference, the following citation commands can be used:
- `@author:2001`  ->  "Author et al. (2001)"
- `[@author:2001]` -> "(Author et al., 2001)"
- `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)"
