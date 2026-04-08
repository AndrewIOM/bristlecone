(**
[![Script](https://acm.im/bristlecone//img/badge-script.svg)](https://acm.im/bristlecone//examples/plant-growth.fsx)&emsp;
[![Notebook](https://acm.im/bristlecone//img/badge-notebook.svg)](https://acm.im/bristlecone//examples/plant-growth.ipynb)

In this example, we use Bristlecone to fit a suite of classical non‑linear
plant‑growth models to data from a controlled growth experiment.

[Paine et al. (2012)](https://doi.org/10.1111%2Fj.2041-210X.2011.00155.x)
identify seven widely used functional forms for modelling plant biomass
through time. These models differ in their assumptions about asymptotes,
growth rates, and curvature, and are a natural testbed for demonstrating
Bristlecone’s model‑expression system and optimisation workflow.

We implement the dM/dt forms of all seven models using Bristlecone’s
declarative syntax, then fit them to the Holcus unshaded growth dataset
from Paine et al. (2012) using Nelder–Mead optimisation.

First, we open Bristlecone core libraries.

### Defining the models

For units of measure, we can choose to use built-in F# units from
the `FSharp.Data.UnitSystems.SI.UnitSymbols` namespace, or declare our own.
Similarly, time-based units (days, months, years) are available in the
Bristlecone.Time module. Here, we use the time units from Bristlecone,
and declare grams as a new unit type.

*)
[<Measure>] type gram

let mass = state<gram> "mass"
(**
For the growth functions, there are five different parameters
across the seven models, which we declare. To declare parameters,
a reasonable starting bound and a constraint are required. Often,
parameters will be declared as `Positive`, but can also be
`NoConstraints` for unconstrained. Parameter constraints are handled
automatically by the optimisers, such that the Nelder-Mead algorithm
will run in a transformed (unbounded) parameter space.

Each model is expressed in its differential form (dM/dt),
which Bristlecone integrates automatically when fitting
continuous‑time systems.

The parameter r represents different biological processes across
the seven models (absolute growth rate, proportional growth rate,
curvature parameter, etc.), so we define rAbs and r separately
to avoid conflating meanings.

*)
module GrowthFunctions =

    // Parameters
    let K = parameter "K" Positive 20.0<gram> 40.0<gram> // upper asymptote
    let β = parameter "β" Positive 0.1 2.0
    let L = parameter "L" Positive 20.0<gram> 40.0<gram> // lower asymptote

    // The meaning of r changes between models, so define it seperately
    let rAbs = parameter "r_abs" Positive 1.1<gram/day> 1.2<gram/day>
    let r = parameter "r" Positive 0.01</day> 2.00</day>

    // 7x typical model forms (ODEs)
    let linear = P rAbs
    let exponential = P r * This<gram>
    let powerLaw = P r * This<gram> ** P β
    let monomolecular = P r * (P K - This<gram>)
    let logistic = P r * This<gram> * (Constant 1. - This / P K)
    let logisticFour = P r * (This<gram> - P L) * ((P K - This) / (P K - P L))
    let gompertz = P r * This<gram> * Logarithm (P K / This)

    // Scaffold into a list of plausable hypotheses:        
    let hypotheses =
        let mkModel ode = Model.empty |> Model.addRateEquation mass ode
        [
            "linear", mkModel linear |> Model.estimateParameter rAbs
            "exponential", mkModel exponential |> Model.estimateParameter r
            "power", mkModel powerLaw |> Model.estimateParameter r |> Model.estimateParameter β
            "monomolecular", mkModel monomolecular |> Model.estimateParameter r |> Model.estimateParameter K
            "logistic (3-par)", mkModel logistic |> Model.estimateParameter r |> Model.estimateParameter K
            "logistic (4-par)", mkModel logisticFour |> Model.estimateParameter r |> Model.estimateParameter K |> Model.estimateParameter L
            "gompertz", mkModel gompertz |> Model.estimateParameter r |> Model.estimateParameter K
        ]
(**
### Load the dataset

Here, to keep the example simple we have defined the
data in code, but otherwise you may load data in as you see fit.

Bristlecone expects time‑series to be provided in Bristlecone's `TimeSeries` type.
For ecological datasets recorded in modern calendar time, the helper
`TimeSeries.fromNeoObservations` converts (`value`, `DateTime`) pairs into a
typed time‑series that Bristlecone can align with the model’s time units.

Lastly, Bristlecone expects a `Map` of `TimeSeries` for use in the fit function.

*)
let baseDay = System.DateTime(2000, 01, 01)
let data =
    [
        mass.Code, TimeSeries.fromNeoObservations [
        0.05, baseDay.AddDays 14
        0.33, baseDay.AddDays 35
        2.43, baseDay.AddDays 70
        3.63, baseDay.AddDays 98
        5.81, baseDay.AddDays 133
        7.28, baseDay.AddDays 161
        5.87, baseDay.AddDays 189
        ]
    ]
    |> Map.ofList
(**
### Model-fitting

All seven growth models are naturally expressed as ODEs, so we use a
continuous‑time estimation engine. The call to `Bristlecone.forDailyModel`
ensures that the engine interprets the model in daily units,
matching the temporal resolution of the dataset.

We use a simple Nelder–Mead optimiser, as the models are simple with only
between one and three parameters. If we wanted increased robustness to local
optima, we could choose to run a swarm-based Nelder-Mead instead.

Plant‑growth data typically exhibit heteroscedasticity: measurement error
increases with biomass. We therefore use a Gaussian likelihood with an
exponential variance function, parameterised by σ0 and σ1.

We specify no conditioning. Here, we know that plant biomass was effectively zero at time
zero, so we do not need to condition the time-zero value; it is known. The model fitting will
therefore run from the first time-point in the data onwards.

Then, we can just run all of the hypotheses.

Each model is compiled before fitting. Compilation resolves the symbolic
model expression into an executable system of equations and prepares
the likelihood function for evaluation.

*)
module Settings =
    let engine =
        Bristlecone.mkContinuous ()
        |> Bristlecone.forDailyModel
        |> Bristlecone.withCustomOptimisation (Optimisation.Amoeba.single Optimisation.Amoeba.Solver.Default)
        |> Bristlecone.withConditioning Conditioning.NoConditioning
        |> Bristlecone.withSeed 200000

    let endCond = Optimisation.EndConditions.whenNoBestValueImprovement 100<iteration>

let sigmaBase = parameter "σ0[x]" Positive 0.01<gram> 0.5<gram>
let sigmaGrowth = parameter "σ1[x]" Positive 0.01</gram> 0.5</gram>
let variance = ModelLibrary.NegLogLikelihood.Variance.exponential sigmaBase sigmaGrowth

let results =
    GrowthFunctions.hypotheses
    |> List.map (fun h ->
            let hy =
                snd h
                |> Model.useLikelihoodFunction (ModelLibrary.NegLogLikelihood.NormalWithVariance (Require.state mass) variance)
                |> Model.compile
            fst h, Bristlecone.fit Settings.engine Settings.endCond data hy
    )
(**
The plot below overlays the observed biomass with the fitted trajectories
from each model, allowing visual comparison of model performance.

*)
Graphing.fitPlot results
|> Plotly.NET.GenericChart.toChartHTML(* output: 
<div><div id="f561aed9-6dff-4db8-9e22-9b34ab49f16a"><!-- Plotly chart will be drawn inside this DIV --></div><script type="text/javascript">var renderPlotly_f561aed96dff4db89e229b34ab49f16a = function() {
    var data = [{"type":"scatter","name":"Observed","mode":"markers","x":["2000-02-05T00:00:00","2000-03-11T00:00:00","2000-04-08T00:00:00","2000-05-13T00:00:00","2000-06-10T00:00:00","2000-07-08T00:00:00"],"y":[0.33,2.43,3.63,5.81,7.28,5.87],"marker":{},"line":{},"xaxis":"x","yaxis":"y"},{"type":"scatter","name":"linear","mode":"lines","x":["2000-02-05T00:00:00","2000-03-11T00:00:00","2000-04-08T00:00:00","2000-05-13T00:00:00","2000-06-10T00:00:00","2000-07-08T00:00:00"],"y":[0.9425466906789058,2.430124508477082,3.6201867627156235,5.1077645805138,6.297826834752342,7.487889088990883],"marker":{},"line":{},"xaxis":"x","yaxis":"y"},{"type":"scatter","name":"exponential","mode":"lines","x":["2000-02-05T00:00:00","2000-03-11T00:00:00","2000-04-08T00:00:00","2000-05-13T00:00:00","2000-06-10T00:00:00","2000-07-08T00:00:00"],"y":[0.09198060001034036,0.25322579361095154,0.57024754071823,1.5699113295285496,3.5353352517664884,7.961338393637937],"marker":{},"line":{},"xaxis":"x","yaxis":"y"},{"type":"scatter","name":"power","mode":"lines","x":["2000-02-05T00:00:00","2000-03-11T00:00:00","2000-04-08T00:00:00","2000-05-13T00:00:00","2000-06-10T00:00:00","2000-07-08T00:00:00"],"y":[0.9366385037597734,2.43,3.630000003151392,5.133996596010654,6.339550390325179,7.54676227772745],"marker":{},"line":{},"xaxis":"x","yaxis":"y"},{"type":"scatter","name":"monomolecular","mode":"lines","x":["2000-02-05T00:00:00","2000-03-11T00:00:00","2000-04-08T00:00:00","2000-05-13T00:00:00","2000-06-10T00:00:00","2000-07-08T00:00:00"],"y":[0.9425466907598459,2.4301245086927143,3.6201867630388223,5.107764580971223,6.297826835316957,7.487889089662524],"marker":{},"line":{},"xaxis":"x","yaxis":"y"},{"type":"scatter","name":"logistic (3-par)","mode":"lines","x":["2000-02-05T00:00:00","2000-03-11T00:00:00","2000-04-08T00:00:00","2000-05-13T00:00:00","2000-06-10T00:00:00","2000-07-08T00:00:00"],"y":[0.19045955268206327,1.4130267950453899,4.105211465356286,5.938386407634697,6.376177284535502,6.497762562558152],"marker":{},"line":{},"xaxis":"x","yaxis":"y"},{"type":"scatter","name":"logistic (4-par)","mode":"lines","x":["2000-02-05T00:00:00","2000-03-11T00:00:00","2000-04-08T00:00:00","2000-05-13T00:00:00","2000-06-10T00:00:00","2000-07-08T00:00:00"],"y":[1.0345680036444016,2.470329441944631,3.466840754925061,4.558793972962677,5.330153739070274,6.02523807582702],"marker":{},"line":{},"xaxis":"x","yaxis":"y"},{"type":"scatter","name":"gompertz","mode":"lines","x":["2000-02-05T00:00:00","2000-03-11T00:00:00","2000-04-08T00:00:00","2000-05-13T00:00:00","2000-06-10T00:00:00","2000-07-08T00:00:00"],"y":[0.5154724264059161,2.43,3.6299686437677945,4.280808351188267,4.474744921864767,4.549554979724581],"marker":{},"line":{},"xaxis":"x","yaxis":"y"}];
    var layout = {"width":600,"height":600,"template":{"layout":{"title":{"x":0.05},"font":{"color":"rgba(42, 63, 95, 1.0)"},"paper_bgcolor":"rgba(255, 255, 255, 1.0)","plot_bgcolor":"rgba(229, 236, 246, 1.0)","autotypenumbers":"strict","colorscale":{"diverging":[[0.0,"#8e0152"],[0.1,"#c51b7d"],[0.2,"#de77ae"],[0.3,"#f1b6da"],[0.4,"#fde0ef"],[0.5,"#f7f7f7"],[0.6,"#e6f5d0"],[0.7,"#b8e186"],[0.8,"#7fbc41"],[0.9,"#4d9221"],[1.0,"#276419"]],"sequential":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]],"sequentialminus":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]},"hovermode":"closest","hoverlabel":{"align":"left"},"coloraxis":{"colorbar":{"outlinewidth":0.0,"ticks":""}},"geo":{"showland":true,"landcolor":"rgba(229, 236, 246, 1.0)","showlakes":true,"lakecolor":"rgba(255, 255, 255, 1.0)","subunitcolor":"rgba(255, 255, 255, 1.0)","bgcolor":"rgba(255, 255, 255, 1.0)"},"mapbox":{"style":"light"},"polar":{"bgcolor":"rgba(229, 236, 246, 1.0)","radialaxis":{"linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","ticks":""},"angularaxis":{"linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","ticks":""}},"scene":{"xaxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","gridwidth":2.0,"zerolinecolor":"rgba(255, 255, 255, 1.0)","backgroundcolor":"rgba(229, 236, 246, 1.0)","showbackground":true},"yaxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","gridwidth":2.0,"zerolinecolor":"rgba(255, 255, 255, 1.0)","backgroundcolor":"rgba(229, 236, 246, 1.0)","showbackground":true},"zaxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","gridwidth":2.0,"zerolinecolor":"rgba(255, 255, 255, 1.0)","backgroundcolor":"rgba(229, 236, 246, 1.0)","showbackground":true}},"ternary":{"aaxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)"},"baxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)"},"caxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)"},"bgcolor":"rgba(229, 236, 246, 1.0)"},"xaxis":{"title":{"standoff":15},"ticks":"","automargin":"height+width+left+right+top+bottom","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","zerolinecolor":"rgba(255, 255, 255, 1.0)","zerolinewidth":2.0},"yaxis":{"title":{"standoff":15},"ticks":"","automargin":"height+width+left+right+top+bottom","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","zerolinecolor":"rgba(255, 255, 255, 1.0)","zerolinewidth":2.0},"annotationdefaults":{"arrowcolor":"#2a3f5f","arrowhead":0,"arrowwidth":1},"shapedefaults":{"line":{"color":"rgba(42, 63, 95, 1.0)"}},"colorway":["rgba(99, 110, 250, 1.0)","rgba(239, 85, 59, 1.0)","rgba(0, 204, 150, 1.0)","rgba(171, 99, 250, 1.0)","rgba(255, 161, 90, 1.0)","rgba(25, 211, 243, 1.0)","rgba(255, 102, 146, 1.0)","rgba(182, 232, 128, 1.0)","rgba(255, 151, 255, 1.0)","rgba(254, 203, 82, 1.0)"]},"data":{"bar":[{"marker":{"line":{"color":"rgba(229, 236, 246, 1.0)","width":0.5},"pattern":{"fillmode":"overlay","size":10,"solidity":0.2}},"error_x":{"color":"rgba(42, 63, 95, 1.0)"},"error_y":{"color":"rgba(42, 63, 95, 1.0)"}}],"barpolar":[{"marker":{"line":{"color":"rgba(229, 236, 246, 1.0)","width":0.5},"pattern":{"fillmode":"overlay","size":10,"solidity":0.2}}}],"carpet":[{"aaxis":{"linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","endlinecolor":"rgba(42, 63, 95, 1.0)","minorgridcolor":"rgba(255, 255, 255, 1.0)","startlinecolor":"rgba(42, 63, 95, 1.0)"},"baxis":{"linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","endlinecolor":"rgba(42, 63, 95, 1.0)","minorgridcolor":"rgba(255, 255, 255, 1.0)","startlinecolor":"rgba(42, 63, 95, 1.0)"}}],"choropleth":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"contour":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"contourcarpet":[{"colorbar":{"outlinewidth":0.0,"ticks":""}}],"heatmap":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"heatmapgl":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"histogram":[{"marker":{"pattern":{"fillmode":"overlay","size":10,"solidity":0.2}}}],"histogram2d":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"histogram2dcontour":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"mesh3d":[{"colorbar":{"outlinewidth":0.0,"ticks":""}}],"parcoords":[{"line":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"pie":[{"automargin":true}],"scatter":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scatter3d":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}},"line":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scattercarpet":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scattergeo":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scattergl":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scattermapbox":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scatterpolar":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scatterpolargl":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scatterternary":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"surface":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"table":[{"cells":{"fill":{"color":"rgba(235, 240, 248, 1.0)"},"line":{"color":"rgba(255, 255, 255, 1.0)"}},"header":{"fill":{"color":"rgba(200, 212, 227, 1.0)"},"line":{"color":"rgba(255, 255, 255, 1.0)"}}}]}},"title":{"text":"mass"},"xaxis":{},"yaxis":{},"grid":{"rows":1,"columns":1,"pattern":"independent"}};
    var config = {"responsive":true};
    Plotly.newPlot('f561aed9-6dff-4db8-9e22-9b34ab49f16a', data, layout, config);
};
renderPlotly_f561aed96dff4db89e229b34ab49f16a();
</script></div>*)
(**
#### Model selection

Because this example uses a non‑Bayesian fitting approach
(maximum likelihood with Nelder–Mead optimisation), we can compare
competing growth models using constrained Akaike’s Information Criterion (AICc).

AIC provides a balance between goodness‑of‑fit and model complexity,
and the corresponding Akaike weights give the relative support for
each model in the candidate set.

*)
let weights = ModelSelection.Akaike.akaikeWeights (results |> Seq.map snd)
(**
For more complex analyses involving multiple subjects, multiple hypotheses, or replicate fits, see the Model Selection documentation for the ResultSet‑based workflow.

The resultant weights are:

*)
weights 
|> Seq.zip (results |> Seq.map fst)
|> Seq.map(fun (m,(r,w)) ->
    sprintf "| %s | -logL: %f | AICc: %f | Weight: %f |" m r.Likelihood w.AICc w.Weight
    )
|> String.concat "\n"(* output: 
"| linear | -logL: 0.329993 | AICc: 18.659986 | Weight: 0.000006 |
| exponential | -logL: 12.874793 | AICc: 43.749586 | Weight: 0.000000 |
| power | -logL: -5.599349 | AICc: 36.801302 | Weight: 0.000000 |
| monomolecular | -logL: 0.329993 | AICc: 48.659986 | Weight: 0.000000 |
| logistic (3-par) | -logL: 5.349318 | AICc: 58.698637 | Weight: 0.000000 |
| logistic (4-par) | -logL: 4.494261 | AICc: Infinity | Weight: 0.000000 |
| gompertz | -logL: -26.620829 | AICc: -5.241659 | Weight: 0.999994 |"*)
(**
The output indicates that the best‑supported model is the gompertz growth model,
with approximately 99% support.

The reason that the gompertz is identified as the best model, despite the fit not
looking as good by eye as the three-parameter logistic form, is because the likelihood
function with exponential variance penalises errors at larger biomasses less heavily.

We can estimate the implied sigma for each observation, for example for the
gompertz fit:

*)
let sigmaAt (expected: float<gram>) =
    let s0 = (snd results.[6]).Parameters.Get sigmaBase
    let s1 = (snd results.[6]).Parameters.Get sigmaGrowth
    s0 * exp (s1 * expected)

let diagnostics =
    (snd results.[6]).Series
    |> Map.find mass.Code
    |> fun ts -> ts.Values
    |> Seq.map (fun d ->
        let fitted = d.Fit
        let observed = d.Obs
        let sigma = sigmaAt (fitted * 1.<gram/ModelSystem.state>)
        sprintf "Fit = %f, Obs = %f, sigma = %f" fitted observed sigma
    )
    |> Seq.toList

diagnostics(* output: 
["Fit = 0.515472, Obs = 0.330000, sigma = 0.000000";
 "Fit = 2.430000, Obs = 2.430000, sigma = 0.000000";
 "Fit = 3.629969, Obs = 3.630000, sigma = 0.000025";
 "Fit = 4.280808, Obs = 5.810000, sigma = 0.962549";
 "Fit = 4.474745, Obs = 7.280000, sigma = 22.391603";
 "Fit = 4.549555, Obs = 5.870000, sigma = 75.382107"]*)
(**
The sigma values are very tight for the first three time points for the gompertz
fit, as shown above. Other models fail to predict the earlier biomasses with such
closeness, so are heavily penalised as a result.

#### Model fit quality / uncertainty

For maximum‑likelihood fits, Bristlecone supports profile‑likelihood confidence intervals for individual parameters.
Profile likelihoods are robust to non‑linearity and asymmetry in the likelihood surface, making them preferable to approximate Hessian‑based intervals for ecological models.

Below is an example of how to compute a profile likelihood for the parameters of a fitted model.

*)
let likelihoodProfile =
    results
    |> Seq.take 1 // just run the first only in this example
    |> Seq.map(fun (name,r) ->

        // Lookup the original hypothesis model using its name:
        let h =
            GrowthFunctions.hypotheses |> Seq.find (fun s -> fst s = name)
            |> snd
            |> Model.useLikelihoodFunction (ModelLibrary.NegLogLikelihood.NormalWithVariance (Require.state mass) variance)
            |> Model.compile

        // Run a profile likelihood around the Maximum Likelihood Estimate:
        let l = 
            Bristlecone.Confidence.ProfileLikelihood.profile
                Bristlecone.fit
                Settings.engine
                data
                h
                100<iteration>
                r
        name, l )
    |> Seq.toList
(**
The likelihood profile returns a 68% and 95% interval for each
parameter. We can inspect them like so:

*)
likelihoodProfile.Head
|> snd
|> Seq.map(fun kv -> sprintf "[%s] 95 CI %f - %f (estimate = %f)" kv.Key.Value kv.Value.``95%``.Lower kv.Value.``95%``.Upper kv.Value.Estimate )

