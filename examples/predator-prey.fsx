(**
[![Script](https://acm.im/bristlecone//img/badge-script.svg)](https://acm.im/bristlecone//examples/predator-prey.fsx)&emsp;
[![Notebook](https://acm.im/bristlecone//img/badge-notebook.svg)](https://acm.im/bristlecone//examples/predator-prey.ipynb)

## Predator-Prey Dynamics: Snowshoe Hare and Lynx

Here we use the classic example of snowshoe hare and lynx predator-prey dynamics,
to demonstrate the basic functions of Bristlecone. The dataset is a 90-year
time-series of snowshoe hare and lynx pelts purchased by the
Hudson's Bay Company of Canada. Data is in 1000s.

To get started, we first load and open the Bristlecone library in
an F# script file (.fsx):

*)
open Bristlecone // Opens Bristlecone core library and estimation engine
open Bristlecone.Language // Open the language for writing Bristlecone models
(**
### Defining the ecological model

In Bristlecone, a single ecological model (representing a single hypothesis)
is defined through the `ModelSystem` type. A `ModelSystem` needs to include three key
components:

* **Model equations.** When working in continuous time, these are a system of Ordinary Differential Equations (ODEs).

* **Parameters to be estimated.** You must specify the starting bounds and constraints for every parameter included in the model equations.

* **Likelihood function**. The (negative log) likelihood function **-logL** represents the probability of observing the data given the parameter set. We use a negative log likelihood function, which is then minimised during optimisation.

In this example, we demonstrate using the **Lotka–Volterra** predator–prey model as the
model system. For the -logL function we use a bivariate normal negative log likelihood function.
This -logL function assumes normally-distributed observation error around each observation
at each time-point, for both the lynx and hare data. The -logL function contains
three parameters that are to be estimated alongside the deterministic model: the variability
in lynx data, the variability in hare data, and their covariance.

*)
let ``predator-prey`` =

    let ``dh/dt`` = Parameter "α" * This - Parameter "β" * This * Environment "lynx"
    let ``dl/dt`` = -Parameter "δ" * This + Parameter "γ" * Environment "hare" * This

    Model.empty
    |> Model.addEquation "hare" ``dh/dt``
    |> Model.addEquation "lynx" ``dl/dt``

    |> Model.estimateParameter "α" noConstraints 0.75 1.25 // Natural growth rate of hares in absence of predation
    |> Model.estimateParameter "β" noConstraints 0.01 0.20 // Death rate per encounter of hares due to predation
    |> Model.estimateParameter "δ" noConstraints 0.75 1.25 // Natural death rate of lynx in the absence of food
    |> Model.estimateParameter "γ" noConstraints 0.01 0.20 // Efficiency of turning predated hares into lynx

    |> Model.useLikelihoodFunction (ModelLibrary.Likelihood.bivariateGaussian "hare" "lynx")
    |> Model.estimateParameter "ρ" noConstraints -0.500 0.500
    |> Model.estimateParameter "σ[x]" notNegative 0.001 0.100
    |> Model.estimateParameter "σ[y]" notNegative 0.001 0.100

    |> Model.compile
(**
### Setting up the **Bristlecone Engine**

A bristlecone engine provides a fixed setup for estimating parameters from data.
Use the same engine for all model fits within a single study.
This engine uses a gradident descent method (Nelder Mead simplex), and a basic
Runge-Kutta 4 integration method provided by MathNet Numerics.

*)
let engine =
    Bristlecone.mkContinuous
    // |> Bristlecone.withCustomOptimisation (Optimisation.Amoeba.swarm 5 20 Optimisation.Amoeba.Solver.Default)
    |> Bristlecone.withCustomOptimisation (
        Optimisation.MonteCarlo.Filzbach.filzbach
            { Optimisation.MonteCarlo.Filzbach.FilzbachSettings<float>.Default with
                BurnLength = Optimisation.EndConditions.afterIteration 10000 }
    )
    |> Bristlecone.withContinuousTime Integration.MathNet.integrate
    |> Bristlecone.withConditioning Conditioning.RepeatFirstDataPoint
    |> Bristlecone.withSeed 1000 // We are setting a seed for this example - see below
(**
**Note. We have set a seed for random number generation for this worked example. This ensures that the results are the same each time this documentation is generated.**

### Does it all work? Testing the engine and model

Before being confident in the ability of our estimation engine to
be able to arrive at the correct solution, we must run a full test
of the model and estimation engine.

Bristlecone includes the `Bristlecone.testModel` function, which
we use here. Given a model system and estimation engine, the function
generates a random parameter set (**θ**) many times; for each **θ**, the
'true' time-series are generated. The test result indicates the effectiveness
of the configuration at estimating **θ** given the auto-generated
time-series. If there is divergence, there is likely an
issue with your model or the Bristlecone configuration.

Bristlecone includes many settings to configure the test
procedure. A simple test configuration is set as `Test.defaultSettings`,
but here we will configure some additional settings:

*)
let testSettings =
    Test.create
    |> Test.addStartValues [ "hare", 50.; "lynx", 75. ]
    |> Test.addNoise (Test.Noise.tryAddNormal "σ[y]" "lynx")
    |> Test.addNoise (Test.Noise.tryAddNormal "σ[x]" "hare")
    |> Test.addGenerationRules
        [ Test.GenerationRules.alwaysLessThan 100000. "lynx"
          Test.GenerationRules.alwaysMoreThan 10. "lynx"
          Test.GenerationRules.alwaysLessThan 100000. "hare"
          Test.GenerationRules.alwaysMoreThan 10. "hare" ]
    |> Test.withTimeSeriesLength 30
    |> Test.endWhen (Optimisation.EndConditions.afterIteration 100)
(**
In our `TestSettings`, we have specified the initial time point (t = 0)
for both modelled time-series. We have also added noise around
each generated time-series, and specified that each time-series
should be 30 years in length.

With these test settings, we can now run the test.

*)
let testResult = ``predator-prey`` |> Bristlecone.tryTestModel engine testSettings(* output: 
##1## GeneralEvent "Attempting to generate parameter set."
##1## GeneralEvent "The data must comply with 4 rules after 50000 tries."
##1## GeneralEvent
  "Time-series start at 1970/01/01 with resolution Fixed (Years PositiveInt 1)."
##1## DebugEvent
  "Observations occur on a fixed temporal resolution: Years PositiveInt 1."
##1## DebugEvent
  "No environmental forcing data was supplied. Solving using time points of observations."
##1## GeneralEvent
  "Skipping optimisation: only the result of the given parameters will be computed"
##1## DebugEvent
  "Observations occur on a fixed temporal resolution: Years PositiveInt 1."
##1## DebugEvent
  "No environmental forcing data was supplied. Solving using time points of observations."
##1## DebugEvent
  "Observations occur on a fixed temporal resolution: Years PositiveInt 1."
##1## DebugEvent
  "No environmental forcing data was supplied. Solving using time points of observations."
##1## GeneralEvent
  "Time-series start at 1970/01/01 with resolution Fixed (Years PositiveInt 1)."
##1## DebugEvent
  "Observations occur on a fixed temporal resolution: Years PositiveInt 1."
##1## DebugEvent
  "No environmental forcing data was supplied. Solving using time points of observations."
Initial scale: ##1## [|0.08333333333; 0.03166666667; 0.03166666667; 0.08333333333; 0.1666666667;
  0.0165; 0.0165|]
GeneralEvent
  "[Optimisation] Initial theta is [|1.066387771; 0.03566858421; 0.1035392077; 1.036168264; 0.01350511291;
  0.05463441816; 0.02742593879|]"
##1## GeneralEvent "[Optimisation] Starting Filzbach-style MCMC optimisation"
##1## GeneralEvent
  "[Filzbach] Starting burn-in at point [|1.066387771; 0.03566858421; 0.1035392077; 1.036168264; 0.01350511291;
  0.05463441816; 0.02742593879|] (L = 45399988.795414)"
##1## At iteration 0 (-logL = 24330800.149625) [|1.066387771; 0.03566858421; 0.1035392077; 1.036168264; 0.01350511291;
  0.05008227844; 0.04916525006|]
##1## At iteration 1000 (-logL = 2754.900555) [|1.503866121; 0.02193517541; 0.0267923332; 1.484057453; -0.05581163206;
  0.8849724306; 0.8397643982|]
##1## At iteration 2000 (-logL = 1849.789060) [|1.529032535; 0.02243673332; 0.02793598095; 1.545764735; -0.02748756751;
  1.021896019; 1.120277553|]
##1## At iteration 3000 (-logL = 1325.386892) [|1.61240898; 0.02359114077; 0.02949466623; 1.655058613; 0.06206686593;
  1.285713299; 1.381103567|]
##1## At iteration 4000 (-logL = 1079.460704) [|1.825541137; 0.02709883517; 0.03466085568; 1.961007377; 0.04923812824;
  1.638847867; 1.859049174|]
##1## At iteration 5000 (-logL = 1061.647847) [|2.057887351; 0.03038821718; 0.03618454655; 2.061570817; 0.01121712997;
  1.655777325; 1.801737452|]
##1## At iteration 6000 (-logL = 1033.749483) [|2.354435354; 0.03537853018; 0.04140700067; 2.364540111; 0.1037860982;
  1.500150915; 1.883161702|]
##1## At iteration 7000 (-logL = 1021.747462) [|2.441281367; 0.03669793764; 0.04694232114; 2.695863151; 0.0195154138;
  1.441757562; 1.817543965|]
##1## At iteration 8000 (-logL = 1023.968157) [|2.294996719; 0.0339700513; 0.05008412669; 2.870213624; -0.04034323326;
  1.520246936; 1.840463729|]
##1## At iteration 9000 (-logL = 1024.039591) [|2.345335843; 0.03528266324; 0.04910617292; 2.820627064; 0.07596312877;
  1.525221077; 1.718875435|]
##1## GeneralEvent
  "[Filzbach] Burn-in complete. Starting sampling at point (1024.228669,
 [|2.465522289; 0.03686950332; 0.04815472731; 2.788935714; -0.03571459269;
   1.547273704; 1.777701619|])"
##1## At iteration 0 (-logL = 1024.890055) [|2.465522289; 0.03686950332; 0.04815472731; 2.788935714; -0.03571459269;
  1.458234128; 1.76315576|]
##1## DebugEvent
  "Observations occur on a fixed temporal resolution: Years PositiveInt 1."
##1## DebugEvent
  "No environmental forcing data was supplied. Solving using time points of observations."
##1## DebugEvent
  "Observations occur on a fixed temporal resolution: Years PositiveInt 1."
##1## DebugEvent
  "No environmental forcing data was supplied. Solving using time points of observations."*)
(**
We can plot the test results to check the fit.

<div><div id="292d9c88-14fc-4bca-85d0-8c57feaf6137"><!-- Plotly chart will be drawn inside this DIV --></div><script type="text/javascript">var renderPlotly_292d9c8814fc4bca85d08c57feaf6137 = function() {
    var data = [{"type":"scatter","name":"Modelled","mode":"lines","x":["1970-01-01T00:00:00","1971-01-01T00:00:00","1972-01-01T00:00:00","1973-01-01T00:00:00","1974-01-01T00:00:00","1975-01-01T00:00:00","1976-01-01T00:00:00","1977-01-01T00:00:00","1978-01-01T00:00:00","1979-01-01T00:00:00","1980-01-01T00:00:00","1981-01-01T00:00:00","1982-01-01T00:00:00","1983-01-01T00:00:00","1984-01-01T00:00:00","1985-01-01T00:00:00","1986-01-01T00:00:00","1987-01-01T00:00:00","1988-01-01T00:00:00","1989-01-01T00:00:00","1990-01-01T00:00:00","1991-01-01T00:00:00","1992-01-01T00:00:00","1993-01-01T00:00:00","1994-01-01T00:00:00","1995-01-01T00:00:00","1996-01-01T00:00:00","1997-01-01T00:00:00","1998-01-01T00:00:00","1999-01-01T00:00:00","2000-01-01T00:00:00"],"y":[59.40836206088186,55.203643012699075,58.309319285798,56.99153120180048,57.3004267958666,57.42107449041503,57.1940708029133,57.370209600204944,57.27756516891234,57.30844071165869,57.30941184005173,57.29828882262275,57.308583947135084,57.30241247897595,57.3049215645446,57.30455541653057,57.30407965714334,57.30465442378855,57.304261988685376,57.304447727999275,57.3043994247059,57.30438446025637,57.30441464050479,57.304390739185955,57.30440363879776,57.30439918102267,57.30439919528121,57.30440064798241,57.30439925589045,57.30440010756263,57.30439975208166],"marker":{},"line":{},"xaxis":"x","yaxis":"y"},{"type":"scatter","name":"Observed","mode":"lines","x":["1970-01-01T00:00:00","1971-01-01T00:00:00","1972-01-01T00:00:00","1973-01-01T00:00:00","1974-01-01T00:00:00","1975-01-01T00:00:00","1976-01-01T00:00:00","1977-01-01T00:00:00","1978-01-01T00:00:00","1979-01-01T00:00:00","1980-01-01T00:00:00","1981-01-01T00:00:00","1982-01-01T00:00:00","1983-01-01T00:00:00","1984-01-01T00:00:00","1985-01-01T00:00:00","1986-01-01T00:00:00","1987-01-01T00:00:00","1988-01-01T00:00:00","1989-01-01T00:00:00","1990-01-01T00:00:00","1991-01-01T00:00:00","1992-01-01T00:00:00","1993-01-01T00:00:00","1994-01-01T00:00:00","1995-01-01T00:00:00","1996-01-01T00:00:00","1997-01-01T00:00:00","1998-01-01T00:00:00","1999-01-01T00:00:00","2000-01-01T00:00:00"],"y":[50.01778503779697,48.287364551942446,53.94822909799746,63.155581077300496,67.16402100848823,60.19744508148311,51.04779311615874,48.42547391817609,53.16696094011771,61.93028409035262,66.80045375729246,61.217131923713204,52.11493245144954,48.70886887307099,52.53435471439677,60.78804995427091,66.2918562234417,62.0716199346672,53.19627602831955,49.06694555862303,52.02622020946841,59.704241419174885,65.66546411462741,62.72997215367168,54.23297500935851,49.49680304339837,51.60578102400692,58.70082354936961,64.99341697034812,63.24511149186636,55.24205798901711],"marker":{},"line":{},"xaxis":"x","yaxis":"y"},{"type":"scatter","name":"Modelled","mode":"lines","x":["1970-01-01T00:00:00","1971-01-01T00:00:00","1972-01-01T00:00:00","1973-01-01T00:00:00","1974-01-01T00:00:00","1975-01-01T00:00:00","1976-01-01T00:00:00","1977-01-01T00:00:00","1978-01-01T00:00:00","1979-01-01T00:00:00","1980-01-01T00:00:00","1981-01-01T00:00:00","1982-01-01T00:00:00","1983-01-01T00:00:00","1984-01-01T00:00:00","1985-01-01T00:00:00","1986-01-01T00:00:00","1987-01-01T00:00:00","1988-01-01T00:00:00","1989-01-01T00:00:00","1990-01-01T00:00:00","1991-01-01T00:00:00","1992-01-01T00:00:00","1993-01-01T00:00:00","1994-01-01T00:00:00","1995-01-01T00:00:00","1996-01-01T00:00:00","1997-01-01T00:00:00","1998-01-01T00:00:00","1999-01-01T00:00:00","2000-01-01T00:00:00"],"y":[63.531733153795585,67.12952891481235,67.33626076553547,66.11914460678854,67.20517724609451,66.55677144724307,66.81772305114768,66.782812703751,66.73069294276051,66.79192596994466,66.75054385733472,66.7699155607706,66.76502940717447,66.76332551011077,66.76656015771103,66.76403123477357,66.76538244241556,66.76492382737274,66.76491737376395,66.76507439316707,66.76492655431649,66.76501609830169,66.76497920875417,66.76498498594368,66.76499160272208,66.76498338304893,66.7649890567515,66.76498634182465,66.76498706843343,66.7649872675213,66.76498683837521],"marker":{},"line":{},"xaxis":"x2","yaxis":"y2"},{"type":"scatter","name":"Observed","mode":"lines","x":["1970-01-01T00:00:00","1971-01-01T00:00:00","1972-01-01T00:00:00","1973-01-01T00:00:00","1974-01-01T00:00:00","1975-01-01T00:00:00","1976-01-01T00:00:00","1977-01-01T00:00:00","1978-01-01T00:00:00","1979-01-01T00:00:00","1980-01-01T00:00:00","1981-01-01T00:00:00","1982-01-01T00:00:00","1983-01-01T00:00:00","1984-01-01T00:00:00","1985-01-01T00:00:00","1986-01-01T00:00:00","1987-01-01T00:00:00","1988-01-01T00:00:00","1989-01-01T00:00:00","1990-01-01T00:00:00","1991-01-01T00:00:00","1992-01-01T00:00:00","1993-01-01T00:00:00","1994-01-01T00:00:00","1995-01-01T00:00:00","1996-01-01T00:00:00","1997-01-01T00:00:00","1998-01-01T00:00:00","1999-01-01T00:00:00","2000-01-01T00:00:00"],"y":[75.02328390447792,63.819523431551225,56.70085274059392,58.25055253782088,68.55955050154236,78.36807304357525,75.74606112668637,65.23310824319957,57.41962024968185,58.08704790010049,67.11852290720877,77.28064123942227,76.32739398385357,66.61397670915578,58.46478262861308,57.8260443776907,65.85851716962263,75.97702357830791,76.66941710065443,67.73232833787574,59.389001944211095,57.836157270981175,64.70441737476011,74.77696705727146,76.69971138349119,68.79127465836947,60.40700834470715,58.0365183522159,63.70748507582825,73.72732525516555,76.54620427581257],"marker":{},"line":{},"xaxis":"x2","yaxis":"y2"}];
    var layout = {"width":600,"height":600,"template":{"layout":{"title":{"x":0.05},"font":{"color":"rgba(42, 63, 95, 1.0)"},"paper_bgcolor":"rgba(255, 255, 255, 1.0)","plot_bgcolor":"rgba(229, 236, 246, 1.0)","autotypenumbers":"strict","colorscale":{"diverging":[[0.0,"#8e0152"],[0.1,"#c51b7d"],[0.2,"#de77ae"],[0.3,"#f1b6da"],[0.4,"#fde0ef"],[0.5,"#f7f7f7"],[0.6,"#e6f5d0"],[0.7,"#b8e186"],[0.8,"#7fbc41"],[0.9,"#4d9221"],[1.0,"#276419"]],"sequential":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]],"sequentialminus":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]},"hovermode":"closest","hoverlabel":{"align":"left"},"coloraxis":{"colorbar":{"outlinewidth":0.0,"ticks":""}},"geo":{"showland":true,"landcolor":"rgba(229, 236, 246, 1.0)","showlakes":true,"lakecolor":"rgba(255, 255, 255, 1.0)","subunitcolor":"rgba(255, 255, 255, 1.0)","bgcolor":"rgba(255, 255, 255, 1.0)"},"mapbox":{"style":"light"},"polar":{"bgcolor":"rgba(229, 236, 246, 1.0)","radialaxis":{"linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","ticks":""},"angularaxis":{"linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","ticks":""}},"scene":{"xaxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","gridwidth":2.0,"zerolinecolor":"rgba(255, 255, 255, 1.0)","backgroundcolor":"rgba(229, 236, 246, 1.0)","showbackground":true},"yaxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","gridwidth":2.0,"zerolinecolor":"rgba(255, 255, 255, 1.0)","backgroundcolor":"rgba(229, 236, 246, 1.0)","showbackground":true},"zaxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","gridwidth":2.0,"zerolinecolor":"rgba(255, 255, 255, 1.0)","backgroundcolor":"rgba(229, 236, 246, 1.0)","showbackground":true}},"ternary":{"aaxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)"},"baxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)"},"caxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)"},"bgcolor":"rgba(229, 236, 246, 1.0)"},"xaxis":{"title":{"standoff":15},"ticks":"","automargin":"height+width+left+right+top+bottom","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","zerolinecolor":"rgba(255, 255, 255, 1.0)","zerolinewidth":2.0},"yaxis":{"title":{"standoff":15},"ticks":"","automargin":"height+width+left+right+top+bottom","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","zerolinecolor":"rgba(255, 255, 255, 1.0)","zerolinewidth":2.0},"annotationdefaults":{"arrowcolor":"#2a3f5f","arrowhead":0,"arrowwidth":1},"shapedefaults":{"line":{"color":"rgba(42, 63, 95, 1.0)"}},"colorway":["rgba(99, 110, 250, 1.0)","rgba(239, 85, 59, 1.0)","rgba(0, 204, 150, 1.0)","rgba(171, 99, 250, 1.0)","rgba(255, 161, 90, 1.0)","rgba(25, 211, 243, 1.0)","rgba(255, 102, 146, 1.0)","rgba(182, 232, 128, 1.0)","rgba(255, 151, 255, 1.0)","rgba(254, 203, 82, 1.0)"]},"data":{"bar":[{"marker":{"line":{"color":"rgba(229, 236, 246, 1.0)","width":0.5},"pattern":{"fillmode":"overlay","size":10,"solidity":0.2}},"error_x":{"color":"rgba(42, 63, 95, 1.0)"},"error_y":{"color":"rgba(42, 63, 95, 1.0)"}}],"barpolar":[{"marker":{"line":{"color":"rgba(229, 236, 246, 1.0)","width":0.5},"pattern":{"fillmode":"overlay","size":10,"solidity":0.2}}}],"carpet":[{"aaxis":{"linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","endlinecolor":"rgba(42, 63, 95, 1.0)","minorgridcolor":"rgba(255, 255, 255, 1.0)","startlinecolor":"rgba(42, 63, 95, 1.0)"},"baxis":{"linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","endlinecolor":"rgba(42, 63, 95, 1.0)","minorgridcolor":"rgba(255, 255, 255, 1.0)","startlinecolor":"rgba(42, 63, 95, 1.0)"}}],"choropleth":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"contour":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"contourcarpet":[{"colorbar":{"outlinewidth":0.0,"ticks":""}}],"heatmap":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"heatmapgl":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"histogram":[{"marker":{"pattern":{"fillmode":"overlay","size":10,"solidity":0.2}}}],"histogram2d":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"histogram2dcontour":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"mesh3d":[{"colorbar":{"outlinewidth":0.0,"ticks":""}}],"parcoords":[{"line":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"pie":[{"automargin":true}],"scatter":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scatter3d":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}},"line":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scattercarpet":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scattergeo":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scattergl":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scattermapbox":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scatterpolar":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scatterpolargl":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scatterternary":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"surface":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"table":[{"cells":{"fill":{"color":"rgba(235, 240, 248, 1.0)"},"line":{"color":"rgba(255, 255, 255, 1.0)"}},"header":{"fill":{"color":"rgba(200, 212, 227, 1.0)"},"line":{"color":"rgba(255, 255, 255, 1.0)"}}}]}},"title":{"text":"lynx"},"xaxis":{},"yaxis":{},"xaxis2":{},"yaxis2":{},"grid":{"rows":2,"columns":1,"pattern":"independent"}};
    var config = {"responsive":true};
    Plotly.newPlot('292d9c88-14fc-4bca-85d0-8c57feaf6137', data, layout, config);
};
renderPlotly_292d9c8814fc4bca85d08c57feaf6137();
</script></div>

### Fitting to real data

First, we must load in the real data, which is in a CSV file. Here, we will use
the FSharp.Data type provider to read in the CSV file (see [the FSharp.Data docs](http://fsprojects.github.io/FSharp.Data/library/CsvProvider.html)
for further information on how to use the library). We place the raw data into
a Bristlecone `TimeSeries` type using `TimeSeries.fromObservations`:

*)
[<Literal>]
let ResolutionFolder = __SOURCE_DIRECTORY__

type PopulationData = FSharp.Data.CsvProvider<"data/lynx-hare.csv", ResolutionFolder=ResolutionFolder>

let data =
    let csv = PopulationData.Load(__SOURCE_DIRECTORY__ + "/data/lynx-hare.csv")

    [ (code "hare").Value, Time.TimeSeries.fromObservations (csv.Rows |> Seq.map (fun r -> float r.Hare, r.Year))
      (code "lynx").Value, Time.TimeSeries.fromObservations (csv.Rows |> Seq.map (fun r -> float r.Lynx, r.Year)) ]
    |> Map.ofList(* output: 
map
  [(ShortCode "hare",
    FixedTimeSeries
  ((19.58, 1/1/1845 12:00:00 AM),
   TimeSteps
     [|(19.6, 365.00:00:00); (19.61, 365.00:00:00); (11.99, 365.00:00:00);
       (28.04, 366.00:00:00); (58.0, 365.00:00:00); (74.6, 365.00:00:00);
       (75.09, 365.00:00:00); (88.48, 366.00:00:00); (61.28, 365.00:00:00);
       (74.67, 365.00:00:00); (88.06, 365.00:00:00); (68.51, 366.00:00:00);
       (32.19, 365.00:00:00); (12.64, 365.00:00:00); (21.49, 365.00:00:00);
       (30.35, 366.00:00:00); (2.18, 365.00:00:00); (152.65, 365.00:00:00);
       (148.36, 365.00:00:00); (85.81, 366.00:00:00); (41.41, 365.00:00:00);
       (14.75, 365.00:00:00); (2.28, 365.00:00:00); (5.91, 366.00:00:00);
       (9.95, 365.00:00:00); (10.44, 365.00:00:00); (70.64, 365.00:00:00);
       (50.12, 366.00:00:00); (50.13, 365.00:00:00); (101.25, 365.00:00:00);
       (97.12, 365.00:00:00); (86.51, 366.00:00:00); (72.17, 365.00:00:00);
       (38.32, 365.00:00:00); (10.11, 365.00:00:00); (7.74, 366.00:00:00);
       (9.67, 365.00:00:00); (43.12, 365.00:00:00); (52.21, 365.00:00:00);
       (134.85, 366.00:00:00); (134.86, 365.00:00:00); (103.79, 365.00:00:00);
       (46.1, 365.00:00:00); (15.03, 366.00:00:00); (24.2, 365.00:00:00);
       (41.65, 365.00:00:00); (52.34, 365.00:00:00); (53.78, 366.00:00:00);
       (70.4, 365.00:00:00); (85.81, 365.00:00:00); (56.69, 365.00:00:00);
       (16.59, 366.00:00:00); (6.16, 365.00:00:00); (2.3, 365.00:00:00);
       (12.82, 365.00:00:00); (4.72, 365.00:00:00); (4.73, 365.00:00:00);
       (37.22, 365.00:00:00); (69.72, 365.00:00:00); (57.78, 366.00:00:00);
       (28.68, 365.00:00:00); (23.37, 365.00:00:00); (21.54, 365.00:00:00);
       (26.34, 366.00:00:00); (53.1, 365.00:00:00); (68.48, 365.00:00:00);
       (75.58, 365.00:00:00); (57.92, 366.00:00:00); (40.97, 365.00:00:00);
       (24.95, 365.00:00:00); (12.59, 365.00:00:00); (4.97, 366.00:00:00);
       (4.5, 365.00:00:00); (11.21, 365.00:00:00); (56.6, 365.00:00:00);
       (69.63, 366.00:00:00); (77.74, 365.00:00:00); (80.53, 365.00:00:00);
       (73.38, 365.00:00:00); (36.93, 366.00:00:00); (4.64, 365.00:00:00);
       (2.54, 365.00:00:00); (1.8, 365.00:00:00); (2.39, 366.00:00:00);
       (4.23, 365.00:00:00); (19.52, 365.00:00:00); (82.11, 365.00:00:00);
       (89.76, 366.00:00:00); (81.66, 365.00:00:00); (15.76, 365.00:00:00)|]));
   (ShortCode "lynx",
    FixedTimeSeries
  ((30.09, 1/1/1845 12:00:00 AM),
   TimeSteps
     [|(45.15, 365.00:00:00); (49.15, 365.00:00:00); (39.52, 365.00:00:00);
       (21.23, 366.00:00:00); (8.42, 365.00:00:00); (5.56, 365.00:00:00);
       (5.08, 365.00:00:00); (10.17, 366.00:00:00); (19.6, 365.00:00:00);
       (32.91, 365.00:00:00); (34.38, 365.00:00:00); (29.59, 366.00:00:00);
       (21.3, 365.00:00:00); (13.69, 365.00:00:00); (7.65, 365.00:00:00);
       (4.08, 366.00:00:00); (4.09, 365.00:00:00); (14.33, 365.00:00:00);
       (38.22, 365.00:00:00); (60.78, 366.00:00:00); (70.77, 365.00:00:00);
       (72.77, 365.00:00:00); (42.68, 365.00:00:00); (16.39, 366.00:00:00);
       (9.83, 365.00:00:00); (5.8, 365.00:00:00); (5.26, 365.00:00:00);
       (18.91, 366.00:00:00); (30.95, 365.00:00:00); (31.18, 365.00:00:00);
       (46.34, 365.00:00:00); (45.77, 366.00:00:00); (44.15, 365.00:00:00);
       (36.33, 365.00:00:00); (12.03, 365.00:00:00); (12.6, 366.00:00:00);
       (18.34, 365.00:00:00); (35.14, 365.00:00:00); (43.77, 365.00:00:00);
       (65.69, 366.00:00:00); (79.35, 365.00:00:00); (51.65, 365.00:00:00);
       (32.59, 365.00:00:00); (22.45, 366.00:00:00); (16.16, 365.00:00:00);
       (14.12, 365.00:00:00); (20.38, 365.00:00:00); (33.33, 366.00:00:00);
       (46.0, 365.00:00:00); (51.41, 365.00:00:00); (46.43, 365.00:00:00);
       (33.68, 366.00:00:00); (18.01, 365.00:00:00); (8.86, 365.00:00:00);
       (7.13, 365.00:00:00); (9.47, 365.00:00:00); (14.86, 365.00:00:00);
       (31.47, 365.00:00:00); (60.57, 365.00:00:00); (63.51, 366.00:00:00);
       (54.7, 365.00:00:00); (6.3, 365.00:00:00); (3.41, 365.00:00:00);
       (5.44, 366.00:00:00); (11.65, 365.00:00:00); (20.35, 365.00:00:00);
       (32.88, 365.00:00:00); (39.55, 366.00:00:00); (43.36, 365.00:00:00);
       (40.83, 365.00:00:00); (30.36, 365.00:00:00); (17.18, 366.00:00:00);
       (6.82, 365.00:00:00); (3.19, 365.00:00:00); (3.52, 365.00:00:00);
       (9.94, 366.00:00:00); (20.3, 365.00:00:00); (31.99, 365.00:00:00);
       (42.36, 365.00:00:00); (49.08, 366.00:00:00); (53.99, 365.00:00:00);
       (52.25, 365.00:00:00); (37.7, 365.00:00:00); (19.14, 366.00:00:00);
       (6.98, 365.00:00:00); (8.31, 365.00:00:00); (16.01, 365.00:00:00);
       (24.82, 366.00:00:00); (29.7, 365.00:00:00); (35.4, 365.00:00:00)|]))]*)
(**
Once the data are in Bristlecone `TimeSeries` we can run `Bristlecone.fit`, which is
the main fitting function of the Bristlecone library.

*)
let endCondition = Optimisation.EndConditions.afterIteration 10000

let result = ``predator-prey`` |> Bristlecone.tryFit engine endCondition data(* output: 
Ok
  { ResultId = 645f2075-7808-4e83-abeb-986a8dcb6e8a
    Likelihood = 11556.42454
    Parameters =
     Pool
       (map
          [(ShortCode "α",
            Parameter (Unconstrained, Detached, Estimated 2.247794927));
           (ShortCode "β",
            Parameter (Unconstrained, Detached, Estimated 0.07943491247));
           (ShortCode "γ",
            Parameter (Unconstrained, Detached, Estimated 0.03568235211));
           (ShortCode "δ",
            Parameter (Unconstrained, Detached, Estimated 1.570031387));
           (ShortCode "ρ",
            Parameter (Unconstrained, Detached, Estimated 0.3087029799));
           (ShortCode "σ[x]",
            Parameter (PositiveOnly, Detached, Estimated 4.566794414));
           (ShortCode "σ[y]",
            Parameter (PositiveOnly, Detached, Estimated 2.341593627))])
    Series =
     map
       [(ShortCode "hare",
         FixedTimeSeries
  (({ Fit = 33.83209455
      Obs = 19.58 }, 1/1/1845 12:00:00 AM),
   TimeSteps
     [|({ Fit = 81.63627705
          Obs = 19.6 }, 365.00:00:00); ({ Fit = 45.70675916
                                          Obs = 19.61 }, 365.00:00:00);
       ({ Fit = 31.20042267
          Obs = 11.99 }, 365.00:00:00); ({ Fit = 42.70985948
                                           Obs = 28.04 }, 366.00:00:00);
       ({ Fit = 56.27505332
          Obs = 58.0 }, 365.00:00:00); ({ Fit = 37.73563578
                                          Obs = 74.6 }, 365.00:00:00);
       ({ Fit = 39.06507646
          Obs = 75.09 }, 365.00:00:00); ({ Fit = 50.94017094
                                           Obs = 88.48 }, 366.00:00:00);
       ({ Fit = 43.22462856
          Obs = 61.28 }, 365.00:00:00); ({ Fit = 40.26351836
                                           Obs = 74.67 }, 365.00:00:00);
       ({ Fit = 46.09384262
          Obs = 88.06 }, 365.00:00:00); ({ Fit = 45.48794728
                                           Obs = 68.51 }, 366.00:00:00);
       ({ Fit = 41.95048859
          Obs = 32.19 }, 365.00:00:00); ({ Fit = 43.95169034
                                           Obs = 12.64 }, 365.00:00:00);
       ({ Fit = 45.43421474
          Obs = 21.49 }, 365.00:00:00); ({ Fit = 43.29669146
                                           Obs = 30.35 }, 366.00:00:00);
       ({ Fit = 43.40861446
          Obs = 2.18 }, 365.00:00:00); ({ Fit = 44.75922631
                                          Obs = 152.65 }, 365.00:00:00);
       ({ Fit = 44.00808645
          Obs = 148.36 }, 365.00:00:00); ({ Fit = 43.50376807
                                            Obs = 85.81 }, 366.00:00:00);
       ({ Fit = 44.23478072
          Obs = 41.41 }, 365.00:00:00); ({ Fit = 44.21327592
                                           Obs = 14.75 }, 365.00:00:00);
       ({ Fit = 43.74155345
          Obs = 2.28 }, 365.00:00:00); ({ Fit = 43.98585667
                                          Obs = 5.91 }, 366.00:00:00);
       ({ Fit = 44.17861804
          Obs = 9.95 }, 365.00:00:00); ({ Fit = 43.9215211
                                          Obs = 10.44 }, 365.00:00:00);
       ({ Fit = 43.92151828
          Obs = 70.64 }, 365.00:00:00); ({ Fit = 44.09077736
                                           Obs = 50.12 }, 366.00:00:00);
       ({ Fit = 44.00746605
          Obs = 50.13 }, 365.00:00:00); ({ Fit = 43.93711451
                                           Obs = 101.25 }, 365.00:00:00);
       ({ Fit = 44.02637929
          Obs = 97.12 }, 365.00:00:00); ({ Fit = 44.02897292
                                           Obs = 86.51 }, 366.00:00:00);
       ({ Fit = 43.9688674
          Obs = 72.17 }, 365.00:00:00); ({ Fit = 43.99666613
                                           Obs = 38.32 }, 365.00:00:00);
       ({ Fit = 44.02263842
          Obs = 10.11 }, 365.00:00:00); ({ Fit = 43.99154284
                                           Obs = 7.74 }, 366.00:00:00);
       ({ Fit = 43.98971846
          Obs = 9.67 }, 365.00:00:00); ({ Fit = 44.01109822
                                          Obs = 43.12 }, 365.00:00:00);
       ({ Fit = 44.00179612
          Obs = 52.21 }, 365.00:00:00); ({ Fit = 43.99228318
                                           Obs = 134.85 }, 366.00:00:00);
       ({ Fit = 44.0030819
          Obs = 134.86 }, 365.00:00:00); ({ Fit = 44.00404632
                                            Obs = 103.79 }, 365.00:00:00);
       ({ Fit = 43.99645754
          Obs = 46.1 }, 365.00:00:00); ({ Fit = 43.99954977
                                          Obs = 15.03 }, 366.00:00:00);
       ({ Fit = 44.00303092
          Obs = 24.2 }, 365.00:00:00); ({ Fit = 43.99928328
                                          Obs = 41.65 }, 365.00:00:00);
       ({ Fit = 43.99883092
          Obs = 52.34 }, 365.00:00:00); ({ Fit = 44.00152213
                                           Obs = 53.78 }, 366.00:00:00);
       ({ Fit = 44.00049816
          Obs = 70.4 }, 365.00:00:00); ({ Fit = 43.99922814
                                          Obs = 85.81 }, 365.00:00:00);
       ({ Fit = 44.00052659
          Obs = 56.69 }, 365.00:00:00); ({ Fit = 44.00072552
                                           Obs = 16.59 }, 366.00:00:00);
       ({ Fit = 43.99977233
          Obs = 6.16 }, 365.00:00:00); ({ Fit = 44.0001095
                                          Obs = 2.3 }, 365.00:00:00);
       ({ Fit = 44.00057187
          Obs = 12.82 }, 365.00:00:00); ({ Fit = 44.0001226
                                           Obs = 4.72 }, 365.00:00:00);
       ({ Fit = 44.00003867
          Obs = 4.73 }, 365.00:00:00); ({ Fit = 44.00037589
                                          Obs = 37.22 }, 365.00:00:00);
       ({ Fit = 44.00026553
          Obs = 69.72 }, 365.00:00:00); ({ Fit = 44.00009757
                                           Obs = 57.78 }, 366.00:00:00);
       ({ Fit = 44.00025279
          Obs = 28.68 }, 365.00:00:00); ({ Fit = 44.0002872
                                           Obs = 23.37 }, 365.00:00:00);
       ({ Fit = 44.00016804
          Obs = 21.54 }, 365.00:00:00); ({ Fit = 44.0002039
                                           Obs = 26.34 }, 366.00:00:00);
       ({ Fit = 44.00026479
          Obs = 53.1 }, 365.00:00:00); ({ Fit = 44.00021125
                                          Obs = 68.48 }, 365.00:00:00);
       ({ Fit = 44.00019743
          Obs = 75.58 }, 365.00:00:00); ({ Fit = 44.00023949
                                           Obs = 57.92 }, 366.00:00:00);
       ({ Fit = 44.00022794
          Obs = 40.97 }, 365.00:00:00); ({ Fit = 44.0002059
                                           Obs = 24.95 }, 365.00:00:00);
       ({ Fit = 44.00022434
          Obs = 12.59 }, 365.00:00:00); ({ Fit = 44.0002298
                                           Obs = 4.97 }, 366.00:00:00);
       ({ Fit = 44.00021497
          Obs = 4.5 }, 365.00:00:00); ({ Fit = 44.00021866
                                         Obs = 11.21 }, 365.00:00:00);
       ({ Fit = 44.00022662
          Obs = 56.6 }, 365.00:00:00); ({ Fit = 44.00022028
                                          Obs = 69.63 }, 366.00:00:00);
       ({ Fit = 44.00021815
          Obs = 77.74 }, 365.00:00:00); ({ Fit = 44.00022337
                                           Obs = 80.53 }, 365.00:00:00);
       ({ Fit = 44.00022221
          Obs = 73.38 }, 365.00:00:00); ({ Fit = 44.00021934
                                           Obs = 36.93 }, 366.00:00:00);
       ({ Fit = 44.00022151
          Obs = 4.64 }, 365.00:00:00); ({ Fit = 44.00022234
                                          Obs = 2.54 }, 365.00:00:00);
       ({ Fit = 44.0002205
          Obs = 1.8 }, 365.00:00:00); ({ Fit = 44.00022086
                                         Obs = 2.39 }, 366.00:00:00);
       ({ Fit = 44.00022189
          Obs = 4.23 }, 365.00:00:00); ({ Fit = 44.00022115
                                          Obs = 19.52 }, 365.00:00:00);
       ({ Fit = 44.00022083
          Obs = 82.11 }, 365.00:00:00); ({ Fit = 44.00022148
                                           Obs = 89.76 }, 366.00:00:00);
       ({ Fit = 44.00022137
          Obs = 81.66 }, 365.00:00:00); ({ Fit = 44.000221
                                           Obs = 15.76 }, 365.00:00:00)|]));
        (ShortCode "lynx",
         FixedTimeSeries
  (({ Fit = 15.06600087
      Obs = 30.09 }, 1/1/1845 12:00:00 AM),
   TimeSteps
     [|({ Fit = 28.40749105
          Obs = 45.15 }, 365.00:00:00); ({ Fit = 40.16261533
                                           Obs = 49.15 }, 365.00:00:00);
       ({ Fit = 29.13705314
          Obs = 39.52 }, 365.00:00:00); ({ Fit = 21.79498998
                                           Obs = 21.23 }, 366.00:00:00);
       ({ Fit = 30.58597028
          Obs = 8.42 }, 365.00:00:00); ({ Fit = 32.24166149
                                          Obs = 5.56 }, 365.00:00:00);
       ({ Fit = 24.89552923
          Obs = 5.08 }, 365.00:00:00); ({ Fit = 27.07017212
                                          Obs = 10.17 }, 366.00:00:00);
       ({ Fit = 31.2141916
          Obs = 19.6 }, 365.00:00:00); ({ Fit = 27.4421311
                                          Obs = 32.91 }, 365.00:00:00);
       ({ Fit = 26.85627226
          Obs = 34.38 }, 365.00:00:00); ({ Fit = 29.54854694
                                           Obs = 29.59 }, 366.00:00:00);
       ({ Fit = 28.62492709
          Obs = 21.3 }, 365.00:00:00); ({ Fit = 27.34138678
                                          Obs = 13.69 }, 365.00:00:00);
       ({ Fit = 28.53362591
          Obs = 7.65 }, 365.00:00:00); ({ Fit = 28.82264787
                                          Obs = 4.08 }, 366.00:00:00);
       ({ Fit = 27.87995875
          Obs = 4.09 }, 365.00:00:00); ({ Fit = 28.15468253
                                          Obs = 14.33 }, 365.00:00:00);
       ({ Fit = 28.64489443
          Obs = 38.22 }, 365.00:00:00); ({ Fit = 28.21796598
                                           Obs = 60.78 }, 366.00:00:00);
       ({ Fit = 28.1087328
          Obs = 70.77 }, 365.00:00:00); ({ Fit = 28.44175954
                                           Obs = 72.77 }, 365.00:00:00);
       ({ Fit = 28.35052001
          Obs = 42.68 }, 365.00:00:00); ({ Fit = 28.1763542
                                           Obs = 16.39 }, 366.00:00:00);
       ({ Fit = 28.32138692
          Obs = 9.83 }, 365.00:00:00); ({ Fit = 28.36537469
                                          Obs = 5.8 }, 365.00:00:00);
       ({ Fit = 28.24795886
          Obs = 5.26 }, 365.00:00:00); ({ Fit = 28.27671732
                                          Obs = 18.91 }, 366.00:00:00);
       ({ Fit = 28.33999243
          Obs = 30.95 }, 365.00:00:00); ({ Fit = 28.28988903
                                           Obs = 31.18 }, 365.00:00:00);
       ({ Fit = 28.2728743
          Obs = 46.34 }, 365.00:00:00); ({ Fit = 28.314209
                                           Obs = 45.77 }, 366.00:00:00);
       ({ Fit = 28.30512356
          Obs = 44.15 }, 365.00:00:00); ({ Fit = 28.28235823
                                           Obs = 36.33 }, 365.00:00:00);
       ({ Fit = 28.29951815
          Obs = 12.03 }, 365.00:00:00); ({ Fit = 28.30609485
                                           Obs = 12.6 }, 366.00:00:00);
       ({ Fit = 28.29155439
          Obs = 18.34 }, 365.00:00:00); ({ Fit = 28.29436544
                                           Obs = 35.14 }, 365.00:00:00);
       ({ Fit = 28.30256472
          Obs = 43.77 }, 365.00:00:00); ({ Fit = 28.296684
                                           Obs = 65.69 }, 366.00:00:00);
       ({ Fit = 28.29417172
          Obs = 79.35 }, 365.00:00:00); ({ Fit = 28.29927967
                                           Obs = 51.65 }, 365.00:00:00);
       ({ Fit = 28.29842589
          Obs = 32.59 }, 365.00:00:00); ({ Fit = 28.29548003
                                           Obs = 22.45 }, 366.00:00:00);
       ({ Fit = 28.29748949
          Obs = 16.16 }, 365.00:00:00); ({ Fit = 28.29844334
                                           Obs = 14.12 }, 365.00:00:00);
       ({ Fit = 28.29665081
          Obs = 20.38 }, 365.00:00:00); ({ Fit = 28.29690295
                                           Obs = 33.33 }, 366.00:00:00);
       ({ Fit = 28.29796007
          Obs = 46.0 }, 365.00:00:00); ({ Fit = 28.29727466
                                          Obs = 51.41 }, 365.00:00:00);
       ({ Fit = 28.29691489
          Obs = 46.43 }, 365.00:00:00); ({ Fit = 28.29754318
                                           Obs = 33.68 }, 366.00:00:00);
       ({ Fit = 28.29747158
          Obs = 18.01 }, 365.00:00:00); ({ Fit = 28.29709281
                                           Obs = 8.86 }, 365.00:00:00);
       ({ Fit = 28.29732605
          Obs = 7.13 }, 365.00:00:00); ({ Fit = 28.29746102
                                          Obs = 9.47 }, 365.00:00:00);
       ({ Fit = 28.29724106
          Obs = 14.86 }, 365.00:00:00); ({ Fit = 28.29726018
                                           Obs = 31.47 }, 365.00:00:00);
       ({ Fit = 28.29739571
          Obs = 60.57 }, 365.00:00:00); ({ Fit = 28.29731653
                                           Obs = 63.51 }, 366.00:00:00);
       ({ Fit = 28.29726614
          Obs = 54.7 }, 365.00:00:00); ({ Fit = 28.29734306
                                          Obs = 6.3 }, 365.00:00:00);
       ({ Fit = 28.29733847
          Obs = 3.41 }, 365.00:00:00); ({ Fit = 28.29729005
                                          Obs = 5.44 }, 366.00:00:00);
       ({ Fit = 28.29731685
          Obs = 11.65 }, 365.00:00:00); ({ Fit = 28.29733559
                                           Obs = 20.35 }, 365.00:00:00);
       ({ Fit = 28.29730873
          Obs = 32.88 }, 365.00:00:00); ({ Fit = 28.29730958
                                           Obs = 39.55 }, 366.00:00:00);
       ({ Fit = 28.29732686
          Obs = 43.36 }, 365.00:00:00); ({ Fit = 28.29731781
                                           Obs = 40.83 }, 365.00:00:00);
       ({ Fit = 28.29731087
          Obs = 30.36 }, 365.00:00:00); ({ Fit = 28.29732024
                                           Obs = 17.18 }, 366.00:00:00);
       ({ Fit = 28.29732021
          Obs = 6.82 }, 365.00:00:00); ({ Fit = 28.29731405
                                          Obs = 3.19 }, 365.00:00:00);
       ({ Fit = 28.2973171
          Obs = 3.52 }, 365.00:00:00); ({ Fit = 28.29731966
                                          Obs = 9.94 }, 366.00:00:00);
       ({ Fit = 28.29731639
          Obs = 20.3 }, 365.00:00:00); ({ Fit = 28.29731631
                                          Obs = 31.99 }, 365.00:00:00);
       ({ Fit = 28.2973185
          Obs = 42.36 }, 365.00:00:00); ({ Fit = 28.29731748
                                           Obs = 49.08 }, 366.00:00:00);
       ({ Fit = 28.29731654
          Obs = 53.99 }, 365.00:00:00); ({ Fit = 28.29731767
                                           Obs = 52.25 }, 365.00:00:00);
       ({ Fit = 28.29731774
          Obs = 37.7 }, 365.00:00:00); ({ Fit = 28.29731696
                                          Obs = 19.14 }, 366.00:00:00);
       ({ Fit = 28.2973173
          Obs = 6.98 }, 365.00:00:00); ({ Fit = 28.29731764
                                          Obs = 8.31 }, 365.00:00:00);
       ({ Fit = 28.29731725
          Obs = 16.01 }, 365.00:00:00); ({ Fit = 28.29731722
                                           Obs = 24.82 }, 366.00:00:00);
       ({ Fit = 28.29731749
          Obs = 29.7 }, 365.00:00:00); ({ Fit = 28.29731738
                                          Obs = 35.4 }, 365.00:00:00)|]))]
    Trace =
     [(11562.30812,
       [|1.946677577; 0.06894649914; 0.0429921279; 1.894999919; 0.2885214;
         4.629529517; 2.368845322|]);
      (11562.30812,
       [|1.946677577; 0.06894649914; 0.0429921279; 1.894999919; 0.2885214;
         4.629529517; 2.368845322|]);
      (11564.66224,
       [|1.946677577; 0.06894649914; 0.0429921279; 1.894999919; 0.2885214;
         4.700345626; 2.310213702|]);
      (11564.66224,
       [|1.946677577; 0.06894649914; 0.0429921279; 1.894999919; 0.2885214;
         4.700345626; 2.310213702|]);
      (11565.38077,
       [|1.946677577; 0.06863403502; 0.04286896899; 1.883001732; 0.2885214;
         4.700345626; 2.310213702|]);
      (11563.7908,
       [|1.946677577; 0.06863403502; 0.04286896899; 1.883405876; 0.2980454309;
         4.700345626; 2.310213702|]);
      (11563.26133,
       [|1.946677577; 0.06863403502; 0.04286896899; 1.891160618; 0.3007084858;
         4.700345626; 2.310213702|]);
      (11563.26133,
       [|1.946677577; 0.06863403502; 0.04286896899; 1.891160618; 0.3007084858;
         4.700345626; 2.310213702|]);
      (11563.26133,
       [|1.946677577; 0.06863403502; 0.04286896899; 1.891160618; 0.3007084858;
         4.700345626; 2.310213702|]);
      (11563.3585,
       [|1.94378308; 0.06863147078; 0.04286896899; 1.891160618; 0.3007084858;
         4.700345626; 2.310213702|]);
      (11564.95679,
       [|1.94378308; 0.0691954702; 0.04286896899; 1.891160618; 0.3046217265;
         4.605328818; 2.310213702|]);
      (11564.95679,
       [|1.94378308; 0.0691954702; 0.04286896899; 1.891160618; 0.3046217265;
         4.605328818; 2.310213702|]);
      (11565.31952,
       [|1.94378308; 0.0691954702; 0.04286896899; 1.891160618; 0.2953926043;
         4.605328818; 2.310213702|]);
      (11565.31952,
       [|1.94378308; 0.0691954702; 0.04286896899; 1.891160618; 0.2953926043;
         4.605328818; 2.310213702|]);
      (11565.31952,
       [|1.94378308; 0.0691954702; 0.04286896899; 1.891160618; 0.2953926043;
         4.605328818; 2.310213702|]);
      (11565.31952,
       [|1.94378308; 0.0691954702; 0.04286896899; 1.891160618; 0.2953926043;
         4.605328818; 2.310213702|]);
      (11563.66322,
       [|1.94378308; 0.0691954702; 0.04286896899; 1.895812815; 0.2923954263;
         4.605328818; 2.310213702|]);
      (11559.08256,
       [|1.94378308; 0.06861975174; 0.04295329546; 1.895812815; 0.2923954263;
         4.605328818; 2.310213702|]);
      (11559.30047,
       [|1.94378308; 0.06861975174; 0.04295329546; 1.895812815; 0.2887333526;
         4.580276785; 2.310213702|]);
      (11558.87326,
       [|1.94378308; 0.06861975174; 0.04298250404; 1.890966605; 0.293734886;
         4.580276785; 2.310213702|]);
      (11558.87326,
       [|1.94378308; 0.06861975174; 0.04298250404; 1.890966605; 0.293734886;
         4.580276785; 2.310213702|]);
      (11558.87326,
       [|1.94378308; 0.06861975174; 0.04298250404; 1.890966605; 0.293734886;
         4.580276785; 2.310213702|]);
      (11558.87326,
       [|1.94378308; 0.06861975174; 0.04298250404; 1.890966605; 0.293734886;
         4.580276785; 2.310213702|]);
      (11558.87326,
       [|1.94378308; 0.06861975174; 0.04298250404; 1.890966605; 0.293734886;
         4.580276785; 2.310213702|]);
      (11558.87326,
       [|1.94378308; 0.06861975174; 0.04298250404; 1.890966605; 0.293734886;
         4.580276785; 2.310213702|]);
      (11558.87326,
       [|1.94378308; 0.06861975174; 0.04298250404; 1.890966605; 0.293734886;
         4.580276785; 2.310213702|]);
      (11558.87326,
       [|1.94378308; 0.06861975174; 0.04298250404; 1.890966605; 0.293734886;
         4.580276785; 2.310213702|]);
      (11558.87326,
       [|1.94378308; 0.06861975174; 0.04298250404; 1.890966605; 0.293734886;
         4.580276785; 2.310213702|]);
      (11558.87326,
       [|1.94378308; 0.06861975174; 0.04298250404; 1.890966605; 0.293734886;
         4.580276785; 2.310213702|]);
      (11557.65639,
       [|1.94378308; 0.06861975174; 0.04298250404; 1.890966605; 0.293734886;
         4.580276785; 2.334625822|]);
      (11557.92603,
       [|1.946184738; 0.06847107835; 0.04297840666; 1.885950085; 0.293734886;
         4.580276785; 2.334625822|]);
      (11557.83888,
       [|1.945828023; 0.06847107835; 0.04297840666; 1.885950085; 0.293734886;
         4.580276785; 2.334625822|]);
      (11557.83888,
       [|1.945828023; 0.06847107835; 0.04297840666; 1.885950085; 0.293734886;
         4.580276785; 2.334625822|]);
      (11557.83888,
       [|1.945828023; 0.06847107835; 0.04297840666; 1.885950085; 0.293734886;
         4.580276785; 2.334625822|]);
      (11557.83888,
       [|1.945828023; 0.06847107835; 0.04297840666; 1.885950085; 0.293734886;
         4.580276785; 2.334625822|]);
      (11558.86166,
       [|1.945828023; 0.06847107835; 0.04297840666; 1.885950085; 0.2858607131;
         4.554963695; 2.334625822|]);
      (11558.86166,
       [|1.945828023; 0.06847107835; 0.04297840666; 1.885950085; 0.2858607131;
         4.554963695; 2.334625822|]);
      (11558.86166,
       [|1.945828023; 0.06847107835; 0.04297840666; 1.885950085; 0.2858607131;
         4.554963695; 2.334625822|]);
      (11558.89902,
       [|1.945828023; 0.06859568682; 0.043138881; 1.890560406; 0.2858607131;
         4.554963695; 2.334625822|]);
      (11558.89902,
       [|1.945828023; 0.06859568682; 0.043138881; 1.890560406; 0.2858607131;
         4.554963695; 2.334625822|]);
      (11558.7842,
       [|1.947261243; 0.06901447222; 0.043138881; 1.890560406; 0.2858607131;
         4.554963695; 2.334625822|]);
      (11558.7842,
       [|1.947261243; 0.06901447222; 0.043138881; 1.890560406; 0.2858607131;
         4.554963695; 2.334625822|]);
      (11558.7842,
       [|1.947261243; 0.06901447222; 0.043138881; 1.890560406; 0.2858607131;
         4.554963695; 2.334625822|]);
      (11558.7842,
       [|1.947261243; 0.06901447222; 0.043138881; 1.890560406; 0.2858607131;
         4.554963695; 2.334625822|]);
      (11558.7842,
       [|1.947261243; 0.06901447222; 0.043138881; 1.890560406; 0.2858607131;
         4.554963695; 2.334625822|]);
      (11558.7842,
       [|1.947261243; 0.06901447222; 0.043138881; 1.890560406; 0.2858607131;
         4.554963695; 2.334625822|]);
      (11558.7842,
       [|1.947261243; 0.06901447222; 0.043138881; 1.890560406; 0.2858607131;
         4.554963695; 2.334625822|]);
      (11558.7842,
       [|1.947261243; 0.06901447222; 0.043138881; 1.890560406; 0.2858607131;
         4.554963695; 2.334625822|]);
      (11558.95085,
       [|1.947261243; 0.06889341561; 0.04350953192; 1.891608376; 0.2858607131;
         4.554963695; 2.334625822|]);
      (11558.25955,
       [|1.947261243; 0.06889341561; 0.04350953192; 1.891608376; 0.2915684137;
         4.554963695; 2.334625822|]);
      (11558.25955,
       [|1.947261243; 0.06889341561; 0.04350953192; 1.891608376; 0.2915684137;
         4.554963695; 2.334625822|]);
      (11558.25955,
       [|1.947261243; 0.06889341561; 0.04350953192; 1.891608376; 0.2915684137;
         4.554963695; 2.334625822|]);
      (11558.25955,
       [|1.947261243; 0.06889341561; 0.04350953192; 1.891608376; 0.2915684137;
         4.554963695; 2.334625822|]);
      (11558.25955,
       [|1.947261243; 0.06889341561; 0.04350953192; 1.891608376; 0.2915684137;
         4.554963695; 2.334625822|]);
      (11558.25955,
       [|1.947261243; 0.06889341561; 0.04350953192; 1.891608376; 0.2915684137;
         4.554963695; 2.334625822|]);
      (11557.79765,
       [|1.947261243; 0.06889341561; 0.04317651031; 1.887698594; 0.2952301214;
         4.554963695; 2.334625822|]);
      (11557.79765,
       [|1.947261243; 0.06889341561; 0.04317651031; 1.887698594; 0.2952301214;
         4.554963695; 2.334625822|]);
      (11557.79765,
       [|1.947261243; 0.06889341561; 0.04317651031; 1.887698594; 0.2952301214;
         4.554963695; 2.334625822|]);
      (11557.79765,
       [|1.947261243; 0.06889341561; 0.04317651031; 1.887698594; 0.2952301214;
         4.554963695; 2.334625822|]);
      (11559.07818,
       [|1.947261243; 0.06889341561; 0.04317651031; 1.882632009; 0.2970391076;
         4.554963695; 2.334625822|]);
      (11559.07818,
       [|1.947261243; 0.06889341561; 0.04317651031; 1.882632009; 0.2970391076;
         4.554963695; 2.334625822|]);
      (11559.07818,
       [|1.947261243; 0.06889341561; 0.04317651031; 1.882632009; 0.2970391076;
         4.554963695; 2.334625822|]);
      (11558.05157,
       [|1.947261243; 0.06889341561; 0.04317651031; 1.885966474; 0.2970391076;
         4.554963695; 2.334625822|]);
      (11558.30575,
       [|1.951835013; 0.06887911692; 0.04317651031; 1.885966474; 0.2970391076;
         4.554963695; 2.334625822|]);
      (11558.30575,
       [|1.951835013; 0.06887911692; 0.04317651031; 1.885966474; 0.2970391076;
         4.554963695; 2.334625822|]);
      (11558.30575,
       [|1.951835013; 0.06887911692; 0.04317651031; 1.885966474; 0.2970391076;
         4.554963695; 2.334625822|]);
      (11558.57537,
       [|1.951835013; 0.06887911692; 0.04317651031; 1.885966474; 0.2970391076;
         4.554963695; 2.356421667|]);
      (11558.57537,
       [|1.951835013; 0.06887911692; 0.04317651031; 1.885966474; 0.2970391076;
         4.554963695; 2.356421667|]);
      (11558.57537,
       [|1.951835013; 0.06887911692; 0.04317651031; 1.885966474; 0.2970391076;
         4.554963695; 2.356421667|]);
      (11557.63298,
       [|1.951835013; 0.06908579751; 0.04317651031; 1.885966474; 0.2970391076;
         4.554963695; 2.356421667|]);
      (11557.64762,
       [|1.951835013; 0.06908579751; 0.04317651031; 1.881527116; 0.3052021052;
         4.554963695; 2.356421667|]);
      (11557.91375,
       [|1.951835013; 0.06908579751; 0.04317651031; 1.881527116; 0.3052021052;
         4.554963695; 2.363014049|]);
      (11557.91375,
       [|1.951835013; 0.06908579751; 0.04317651031; 1.881527116; 0.3052021052;
         4.554963695; 2.363014049|]);
      (11557.91375,
       [|1.951835013; 0.06908579751; 0.04317651031; 1.881527116; 0.3052021052;
         4.554963695; 2.363014049|]);
      (11557.91375,
       [|1.951835013; 0.06908579751; 0.04317651031; 1.881527116; 0.3052021052;
         4.554963695; 2.363014049|]);
      (11558.30984,
       [|1.951835013; 0.06908579751; 0.04317651031; 1.881527116; 0.3084461214;
         4.554963695; 2.371000136|]);
      (11558.30984,
       [|1.951835013; 0.06908579751; 0.04317651031; 1.881527116; 0.3084461214;
         4.554963695; 2.371000136|]);
      (11558.30984,
       [|1.951835013; 0.06908579751; 0.04317651031; 1.881527116; 0.3084461214;
         4.554963695; 2.371000136|]);
      (11558.35213,
       [|1.951835013; 0.06908579751; 0.04317651031; 1.881527116; 0.3149140291;
         4.554963695; 2.371000136|]);
      (11558.35213,
       [|1.951835013; 0.06908579751; 0.04317651031; 1.881527116; 0.3149140291;
         4.554963695; 2.371000136|]);
      (11558.45407,
       [|1.951835013; 0.06908579751; 0.04317651031; 1.881527116; 0.315545174;
         4.597479398; 2.371000136|]);
      (11558.45407,
       [|1.951835013; 0.06908579751; 0.04317651031; 1.881527116; 0.315545174;
         4.597479398; 2.371000136|]);
      (11558.45407,
       [|1.951835013; 0.06908579751; 0.04317651031; 1.881527116; 0.315545174;
         4.597479398; 2.371000136|]);
      (11558.45407,
       [|1.951835013; 0.06908579751; 0.04317651031; 1.881527116; 0.315545174;
         4.597479398; 2.371000136|]);
      (11560.39516,
       [|1.95743295; 0.06970788004; 0.04248486305; 1.888066237; 0.311907975;
         4.629478214; 2.367200001|]);
      (11561.38671,
       [|1.95743295; 0.06970788004; 0.04248486305; 1.896760781; 0.3164965346;
         4.629478214; 2.367200001|]);
      (11560.62551,
       [|1.95743295; 0.06970788004; 0.04248486305; 1.896263735; 0.3226424508;
         4.577471812; 2.367200001|]);
      (11558.36709,
       [|1.95743295; 0.06970788004; 0.04288446065; 1.892430852; 0.3226424508;
         4.577471812; 2.367200001|]);
      (11558.15636,
       [|1.95743295; 0.06970788004; 0.04325905191; 1.884686336; 0.3226424508;
         4.577471812; 2.367200001|]);
      (11558.15636,
       [|1.95743295; 0.06970788004; 0.04325905191; 1.884686336; 0.3226424508;
         4.577471812; 2.367200001|]);
      (11558.15636,
       [|1.95743295; 0.06970788004; 0.04325905191; 1.884686336; 0.3226424508;
         4.577471812; 2.367200001|]);
      (11558.29452,
       [|1.95743295; 0.06970788004; 0.04325905191; 1.884686336; 0.3226424508;
         4.577471812; 2.369969189|]);
      (11558.29452,
       [|1.95743295; 0.06970788004; 0.04325905191; 1.884686336; 0.3226424508;
         4.577471812; 2.369969189|]);
      (11558.63984,
       [|1.96400104; 0.07032111437; 0.04325905191; 1.884686336; 0.3226424508;
         4.577471812; 2.369969189|]);
      (11558.63984,
       [|1.96400104; 0.07032111437; 0.04325905191; 1.884686336; 0.3226424508;
         4.577471812; 2.369969189|]);
      (11560.55875,
       [|1.96400104; 0.06972915511; 0.04272622199; 1.884686336; 0.3226424508;
         4.577471812; 2.369969189|]);
      (11560.55875,
       [|1.96400104; 0.06972915511; 0.04272622199; 1.884686336; 0.3226424508;
         4.577471812; 2.369969189|]);
      (11560.55875,
       [|1.96400104; 0.06972915511; 0.04272622199; 1.884686336; 0.3226424508;
         4.577471812; 2.369969189|]);
      (11560.55875,
       [|1.96400104; 0.06972915511; 0.04272622199; 1.884686336; 0.3226424508;
         4.577471812; 2.369969189|]);
      (11560.65871,
       [|1.96400104; 0.06972915511; 0.04272622199; 1.884686336; 0.3226424508;
         4.557019739; 2.34444322|]); ...]
    InternalDynamics =
     Some
       (map
          [(ShortCode "hare",
            [|33.83209455; 81.63627705; 45.70675916; 31.20042267; 42.70985948;
              56.27505332; 37.73563578; 39.06507646; 50.94017094; 43.22462856;
              40.26351836; 46.09384262; 45.48794728; 41.95048859; 43.95169034;
              45.43421474; 43.29669146; 43.40861446; 44.75922631; 44.00808645;
              43.50376807; 44.23478072; 44.21327592; 43.74155345; 43.98585667;
              44.17861804; 43.9215211; 43.92151828; 44.09077736; 44.00746605;
              43.93711451; 44.02637929; 44.02897292; 43.9688674; 43.99666613;
              44.02263842; 43.99154284; 43.98971846; 44.01109822; 44.00179612;
              43.99228318; 44.0030819; 44.00404632; 43.99645754; 43.99954977;
              44.00303092; 43.99928328; 43.99883092; 44.00152213; 44.00049816;
              43.99922814; 44.00052659; 44.00072552; 43.99977233; 44.0001095;
              44.00057187; 44.0001226; 44.00003867; 44.00037589; 44.00026553;
              44.00009757; 44.00025279; 44.0002872; 44.00016804; 44.0002039;
              44.00026479; 44.00021125; 44.00019743; 44.00023949; 44.00022794;
              44.0002059; 44.00022434; 44.0002298; 44.00021497; 44.00021866;
              44.00022662; 44.00022028; 44.00021815; 44.00022337; 44.00022221;
              44.00021934; 44.00022151; 44.00022234; 44.0002205; 44.00022086;
              44.00022189; 44.00022115; 44.00022083; 44.00022148; 44.00022137;
              44.000221|]);
           (ShortCode "lynx",
            [|15.06600087; 28.40749105; 40.16261533; 29.13705314; 21.79498998;
              30.58597028; 32.24166149; 24.89552923; 27.07017212; 31.2141916;
              27.4421311; 26.85627226; 29.54854694; 28.62492709; 27.34138678;
              28.53362591; 28.82264787; 27.87995875; 28.15468253; 28.64489443;
              28.21796598; 28.1087328; 28.44175954; 28.35052001; 28.1763542;
              28.32138692; 28.36537469; 28.24795886; 28.27671732; 28.33999243;
              28.28988903; 28.2728743; 28.314209; 28.30512356; 28.28235823;
              28.29951815; 28.30609485; 28.29155439; 28.29436544; 28.30256472;
              28.296684; 28.29417172; 28.29927967; 28.29842589; 28.29548003;
              28.29748949; 28.29844334; 28.29665081; 28.29690295; 28.29796007;
              28.29727466; 28.29691489; 28.29754318; 28.29747158; 28.29709281;
              28.29732605; 28.29746102; 28.29724106; 28.29726018; 28.29739571;
              28.29731653; 28.29726614; 28.29734306; 28.29733847; 28.29729005;
              28.29731685; 28.29733559; 28.29730873; 28.29730958; 28.29732686;
              28.29731781; 28.29731087; 28.29732024; 28.29732021; 28.29731405;
              28.2973171; 28.29731966; 28.29731639; 28.29731631; 28.2973185;
              28.29731748; 28.29731654; 28.29731767; 28.29731774; 28.29731696;
              28.2973173; 28.29731764; 28.29731725; 28.29731722; 28.29731749;
              28.29731738|])]) }*)
(**
### Inspecting the model fit

The `Bristlecone.fit` function returns an `EstimationResult`, which contains some
key information that may be used to inspect the model fit:

* Likelihood. The minimum likelihood identified during optimisation.

* Parameters. The parameter set (**θ**) identified at the minimum likelihood.

* Series. A TimeSeries for each variable in the model, which at each time point contains paired Modelled-Observed values.

* Trace. The likelihood and **θ** that occurred at each step in optimisation, with the latest first.

* Internal Dynamics. Not relevant for this simple model.

First, we can use the `Series` to inspect by eye the model fit versus the observed time-series:

<div><div id="cc514e9a-df0d-4fe7-b9bf-226bd7a5fe1c"><!-- Plotly chart will be drawn inside this DIV --></div><script type="text/javascript">var renderPlotly_cc514e9adf0d4fe7b9bf226bd7a5fe1c = function() {
    var data = [{"type":"scatter","name":"Modelled","mode":"lines","x":["1845-01-01T00:00:00+00:00","1846-01-01T00:00:00+00:00","1847-01-01T00:00:00+00:00","1848-01-01T00:00:00+00:00","1849-01-01T00:00:00+00:00","1850-01-01T00:00:00+00:00","1851-01-01T00:00:00+00:00","1852-01-01T00:00:00+00:00","1853-01-01T00:00:00+00:00","1854-01-01T00:00:00+00:00","1855-01-01T00:00:00+00:00","1856-01-01T00:00:00+00:00","1857-01-01T00:00:00+00:00","1858-01-01T00:00:00+00:00","1859-01-01T00:00:00+00:00","1860-01-01T00:00:00+00:00","1861-01-01T00:00:00+00:00","1862-01-01T00:00:00+00:00","1863-01-01T00:00:00+00:00","1864-01-01T00:00:00+00:00","1865-01-01T00:00:00+00:00","1866-01-01T00:00:00+00:00","1867-01-01T00:00:00+00:00","1868-01-01T00:00:00+00:00","1869-01-01T00:00:00+00:00","1870-01-01T00:00:00+00:00","1871-01-01T00:00:00+00:00","1872-01-01T00:00:00+00:00","1873-01-01T00:00:00+00:00","1874-01-01T00:00:00+00:00","1875-01-01T00:00:00+00:00","1876-01-01T00:00:00+00:00","1877-01-01T00:00:00+00:00","1878-01-01T00:00:00+00:00","1879-01-01T00:00:00+00:00","1880-01-01T00:00:00+00:00","1881-01-01T00:00:00+00:00","1882-01-01T00:00:00+00:00","1883-01-01T00:00:00+00:00","1884-01-01T00:00:00+00:00","1885-01-01T00:00:00+00:00","1886-01-01T00:00:00+00:00","1887-01-01T00:00:00+00:00","1888-01-01T00:00:00+00:00","1889-01-01T00:00:00+00:00","1890-01-01T00:00:00+00:00","1891-01-01T00:00:00+00:00","1892-01-01T00:00:00+00:00","1893-01-01T00:00:00+00:00","1894-01-01T00:00:00+00:00","1895-01-01T00:00:00+00:00","1896-01-01T00:00:00+00:00","1897-01-01T00:00:00+00:00","1898-01-01T00:00:00+00:00","1899-01-01T00:00:00+00:00","1900-01-01T00:00:00+00:00","1901-01-01T00:00:00+00:00","1902-01-01T00:00:00+00:00","1903-01-01T00:00:00+00:00","1904-01-01T00:00:00+00:00","1905-01-01T00:00:00+00:00","1906-01-01T00:00:00+00:00","1907-01-01T00:00:00+00:00","1908-01-01T00:00:00+00:00","1909-01-01T00:00:00+00:00","1910-01-01T00:00:00+00:00","1911-01-01T00:00:00+00:00","1912-01-01T00:00:00+00:00","1913-01-01T00:00:00+00:00","1914-01-01T00:00:00+00:00","1915-01-01T00:00:00+00:00","1916-01-01T00:00:00+00:00","1917-01-01T00:00:00+00:00","1918-01-01T00:00:00+00:00","1919-01-01T00:00:00+00:00","1920-01-01T00:00:00+00:00","1921-01-01T00:00:00+00:00","1922-01-01T00:00:00+00:00","1923-01-01T00:00:00+00:00","1924-01-01T00:00:00+00:00","1925-01-01T00:00:00+00:00","1926-01-01T00:00:00+00:00","1927-01-01T00:00:00+00:00","1928-01-01T00:00:00+00:00","1929-01-01T00:00:00+00:00","1930-01-01T00:00:00+00:00","1931-01-01T00:00:00+00:00","1932-01-01T00:00:00+00:00","1933-01-01T00:00:00+00:00","1934-01-01T00:00:00+00:00","1935-01-01T00:00:00+00:00"],"y":[33.83209455494692,81.63627704559048,45.70675916476185,31.200422667541496,42.709859476565775,56.27505332356679,37.73563578142574,39.06507646143033,50.94017093842835,43.2246285553963,40.26351835938578,46.09384261543351,45.48794727695588,41.95048859263276,43.95169034068908,45.43421473855762,43.29669146020667,43.40861445825565,44.75922630828701,44.008086451499764,43.50376806740009,44.234780724163166,44.213275920487796,43.741553451423385,43.98585667105933,44.178618040472365,43.9215211042541,43.92151827601281,44.090777363804406,44.007466052798456,43.9371145068632,44.026379289690965,44.02897291716693,43.9688674037804,43.99666612832264,44.022638416237065,43.9915428354966,43.989718462389035,44.0110982196905,44.00179612219162,43.99228317527405,44.00308189580588,44.00404631774955,43.996457537161284,43.999549769822444,44.00303091981518,43.99928328161713,43.99883091952526,44.00152213114465,44.000498158398024,43.999228144644036,44.00052659151099,44.000725515754105,43.99977232969658,44.000109499296414,44.00057186915896,44.000122597960754,44.000038669726756,44.00037589082047,44.000265534205774,44.00009757221858,44.000252785260294,44.000287200153544,44.00016803501787,44.00020389820313,44.00026479134281,44.000211251197356,44.00019743306416,44.00023949487525,44.00022793692591,44.000205902763156,44.000224341238834,44.000229800651006,44.00021497096924,44.00021865883257,44.000226617551434,44.00022027834458,44.00021814885141,44.00022337139073,44.000222208931746,44.000219339177775,44.00022151473813,44.000222336738716,44.000220499620234,44.00022086049679,44.00022189358007,44.00022114834663,44.00022083379406,44.00022147929459,44.000221369446514,44.00022099812939],"marker":{},"line":{},"xaxis":"x","yaxis":"y"},{"type":"scatter","name":"Observed","mode":"lines","x":["1845-01-01T00:00:00+00:00","1846-01-01T00:00:00+00:00","1847-01-01T00:00:00+00:00","1848-01-01T00:00:00+00:00","1849-01-01T00:00:00+00:00","1850-01-01T00:00:00+00:00","1851-01-01T00:00:00+00:00","1852-01-01T00:00:00+00:00","1853-01-01T00:00:00+00:00","1854-01-01T00:00:00+00:00","1855-01-01T00:00:00+00:00","1856-01-01T00:00:00+00:00","1857-01-01T00:00:00+00:00","1858-01-01T00:00:00+00:00","1859-01-01T00:00:00+00:00","1860-01-01T00:00:00+00:00","1861-01-01T00:00:00+00:00","1862-01-01T00:00:00+00:00","1863-01-01T00:00:00+00:00","1864-01-01T00:00:00+00:00","1865-01-01T00:00:00+00:00","1866-01-01T00:00:00+00:00","1867-01-01T00:00:00+00:00","1868-01-01T00:00:00+00:00","1869-01-01T00:00:00+00:00","1870-01-01T00:00:00+00:00","1871-01-01T00:00:00+00:00","1872-01-01T00:00:00+00:00","1873-01-01T00:00:00+00:00","1874-01-01T00:00:00+00:00","1875-01-01T00:00:00+00:00","1876-01-01T00:00:00+00:00","1877-01-01T00:00:00+00:00","1878-01-01T00:00:00+00:00","1879-01-01T00:00:00+00:00","1880-01-01T00:00:00+00:00","1881-01-01T00:00:00+00:00","1882-01-01T00:00:00+00:00","1883-01-01T00:00:00+00:00","1884-01-01T00:00:00+00:00","1885-01-01T00:00:00+00:00","1886-01-01T00:00:00+00:00","1887-01-01T00:00:00+00:00","1888-01-01T00:00:00+00:00","1889-01-01T00:00:00+00:00","1890-01-01T00:00:00+00:00","1891-01-01T00:00:00+00:00","1892-01-01T00:00:00+00:00","1893-01-01T00:00:00+00:00","1894-01-01T00:00:00+00:00","1895-01-01T00:00:00+00:00","1896-01-01T00:00:00+00:00","1897-01-01T00:00:00+00:00","1898-01-01T00:00:00+00:00","1899-01-01T00:00:00+00:00","1900-01-01T00:00:00+00:00","1901-01-01T00:00:00+00:00","1902-01-01T00:00:00+00:00","1903-01-01T00:00:00+00:00","1904-01-01T00:00:00+00:00","1905-01-01T00:00:00+00:00","1906-01-01T00:00:00+00:00","1907-01-01T00:00:00+00:00","1908-01-01T00:00:00+00:00","1909-01-01T00:00:00+00:00","1910-01-01T00:00:00+00:00","1911-01-01T00:00:00+00:00","1912-01-01T00:00:00+00:00","1913-01-01T00:00:00+00:00","1914-01-01T00:00:00+00:00","1915-01-01T00:00:00+00:00","1916-01-01T00:00:00+00:00","1917-01-01T00:00:00+00:00","1918-01-01T00:00:00+00:00","1919-01-01T00:00:00+00:00","1920-01-01T00:00:00+00:00","1921-01-01T00:00:00+00:00","1922-01-01T00:00:00+00:00","1923-01-01T00:00:00+00:00","1924-01-01T00:00:00+00:00","1925-01-01T00:00:00+00:00","1926-01-01T00:00:00+00:00","1927-01-01T00:00:00+00:00","1928-01-01T00:00:00+00:00","1929-01-01T00:00:00+00:00","1930-01-01T00:00:00+00:00","1931-01-01T00:00:00+00:00","1932-01-01T00:00:00+00:00","1933-01-01T00:00:00+00:00","1934-01-01T00:00:00+00:00","1935-01-01T00:00:00+00:00"],"y":[19.58,19.6,19.61,11.99,28.04,58.0,74.6,75.09,88.48,61.28,74.67,88.06,68.51,32.19,12.64,21.49,30.35,2.18,152.65,148.36,85.81,41.41,14.75,2.28,5.91,9.95,10.44,70.64,50.12,50.13,101.25,97.12,86.51,72.17,38.32,10.11,7.74,9.67,43.12,52.21,134.85,134.86,103.79,46.1,15.03,24.2,41.65,52.34,53.78,70.4,85.81,56.69,16.59,6.16,2.3,12.82,4.72,4.73,37.22,69.72,57.78,28.68,23.37,21.54,26.34,53.1,68.48,75.58,57.92,40.97,24.95,12.59,4.97,4.5,11.21,56.6,69.63,77.74,80.53,73.38,36.93,4.64,2.54,1.8,2.39,4.23,19.52,82.11,89.76,81.66,15.76],"marker":{},"line":{},"xaxis":"x","yaxis":"y"},{"type":"scatter","name":"Modelled","mode":"lines","x":["1845-01-01T00:00:00+00:00","1846-01-01T00:00:00+00:00","1847-01-01T00:00:00+00:00","1848-01-01T00:00:00+00:00","1849-01-01T00:00:00+00:00","1850-01-01T00:00:00+00:00","1851-01-01T00:00:00+00:00","1852-01-01T00:00:00+00:00","1853-01-01T00:00:00+00:00","1854-01-01T00:00:00+00:00","1855-01-01T00:00:00+00:00","1856-01-01T00:00:00+00:00","1857-01-01T00:00:00+00:00","1858-01-01T00:00:00+00:00","1859-01-01T00:00:00+00:00","1860-01-01T00:00:00+00:00","1861-01-01T00:00:00+00:00","1862-01-01T00:00:00+00:00","1863-01-01T00:00:00+00:00","1864-01-01T00:00:00+00:00","1865-01-01T00:00:00+00:00","1866-01-01T00:00:00+00:00","1867-01-01T00:00:00+00:00","1868-01-01T00:00:00+00:00","1869-01-01T00:00:00+00:00","1870-01-01T00:00:00+00:00","1871-01-01T00:00:00+00:00","1872-01-01T00:00:00+00:00","1873-01-01T00:00:00+00:00","1874-01-01T00:00:00+00:00","1875-01-01T00:00:00+00:00","1876-01-01T00:00:00+00:00","1877-01-01T00:00:00+00:00","1878-01-01T00:00:00+00:00","1879-01-01T00:00:00+00:00","1880-01-01T00:00:00+00:00","1881-01-01T00:00:00+00:00","1882-01-01T00:00:00+00:00","1883-01-01T00:00:00+00:00","1884-01-01T00:00:00+00:00","1885-01-01T00:00:00+00:00","1886-01-01T00:00:00+00:00","1887-01-01T00:00:00+00:00","1888-01-01T00:00:00+00:00","1889-01-01T00:00:00+00:00","1890-01-01T00:00:00+00:00","1891-01-01T00:00:00+00:00","1892-01-01T00:00:00+00:00","1893-01-01T00:00:00+00:00","1894-01-01T00:00:00+00:00","1895-01-01T00:00:00+00:00","1896-01-01T00:00:00+00:00","1897-01-01T00:00:00+00:00","1898-01-01T00:00:00+00:00","1899-01-01T00:00:00+00:00","1900-01-01T00:00:00+00:00","1901-01-01T00:00:00+00:00","1902-01-01T00:00:00+00:00","1903-01-01T00:00:00+00:00","1904-01-01T00:00:00+00:00","1905-01-01T00:00:00+00:00","1906-01-01T00:00:00+00:00","1907-01-01T00:00:00+00:00","1908-01-01T00:00:00+00:00","1909-01-01T00:00:00+00:00","1910-01-01T00:00:00+00:00","1911-01-01T00:00:00+00:00","1912-01-01T00:00:00+00:00","1913-01-01T00:00:00+00:00","1914-01-01T00:00:00+00:00","1915-01-01T00:00:00+00:00","1916-01-01T00:00:00+00:00","1917-01-01T00:00:00+00:00","1918-01-01T00:00:00+00:00","1919-01-01T00:00:00+00:00","1920-01-01T00:00:00+00:00","1921-01-01T00:00:00+00:00","1922-01-01T00:00:00+00:00","1923-01-01T00:00:00+00:00","1924-01-01T00:00:00+00:00","1925-01-01T00:00:00+00:00","1926-01-01T00:00:00+00:00","1927-01-01T00:00:00+00:00","1928-01-01T00:00:00+00:00","1929-01-01T00:00:00+00:00","1930-01-01T00:00:00+00:00","1931-01-01T00:00:00+00:00","1932-01-01T00:00:00+00:00","1933-01-01T00:00:00+00:00","1934-01-01T00:00:00+00:00","1935-01-01T00:00:00+00:00"],"y":[15.066000870189848,28.4074910483224,40.16261533028395,29.137053136366735,21.794989983254176,30.585970277126826,32.2416614933814,24.89552922567477,27.07017211610899,31.214191597432496,27.44213109580194,26.85627225531683,29.548546943794125,28.624927091207866,27.341386784848947,28.53362591408021,28.82264786778237,27.879958754238846,28.1546825299559,28.644894433578983,28.21796597964973,28.10873279811406,28.441759536548062,28.350520012333263,28.176354204752283,28.32138692175502,28.36537469102036,28.247958857906923,28.276717322488757,28.339992426367115,28.28988903409526,28.272874295667975,28.31420900267473,28.305123562016323,28.282358234900602,28.299518152266586,28.306094845441095,28.29155438927863,28.29436543648539,28.302564718037353,28.29668399635694,28.29417171866758,28.29927967326918,28.298425885605113,28.295480028666198,28.29748949157299,28.29844333583311,28.29665081226625,28.29690294664345,28.29796006786963,28.297274655820683,28.296914894334076,28.29754318127137,28.297471577772637,28.29709281478772,28.297326045728287,28.297461020270916,28.29724105601091,28.297260182658196,28.29739570690284,28.297316532612836,28.29726613729925,28.297343055229405,28.297338473422165,28.297290048019573,28.297316854398478,28.29733559073432,28.29730872603288,28.29730957852395,28.297326859116094,28.297317808812043,28.297310869518636,28.29732024090946,28.297320209289783,28.29731405063803,28.297317096751378,28.297319657950336,28.297316392945277,28.297316309393324,28.29731850152995,28.297317479745217,28.297316537418602,28.297317673498366,28.29731773626412,28.29731695694491,28.29731729841354,28.297317644117143,28.297317249328135,28.297317215497195,28.297317492215907,28.2973173785738],"marker":{},"line":{},"xaxis":"x2","yaxis":"y2"},{"type":"scatter","name":"Observed","mode":"lines","x":["1845-01-01T00:00:00+00:00","1846-01-01T00:00:00+00:00","1847-01-01T00:00:00+00:00","1848-01-01T00:00:00+00:00","1849-01-01T00:00:00+00:00","1850-01-01T00:00:00+00:00","1851-01-01T00:00:00+00:00","1852-01-01T00:00:00+00:00","1853-01-01T00:00:00+00:00","1854-01-01T00:00:00+00:00","1855-01-01T00:00:00+00:00","1856-01-01T00:00:00+00:00","1857-01-01T00:00:00+00:00","1858-01-01T00:00:00+00:00","1859-01-01T00:00:00+00:00","1860-01-01T00:00:00+00:00","1861-01-01T00:00:00+00:00","1862-01-01T00:00:00+00:00","1863-01-01T00:00:00+00:00","1864-01-01T00:00:00+00:00","1865-01-01T00:00:00+00:00","1866-01-01T00:00:00+00:00","1867-01-01T00:00:00+00:00","1868-01-01T00:00:00+00:00","1869-01-01T00:00:00+00:00","1870-01-01T00:00:00+00:00","1871-01-01T00:00:00+00:00","1872-01-01T00:00:00+00:00","1873-01-01T00:00:00+00:00","1874-01-01T00:00:00+00:00","1875-01-01T00:00:00+00:00","1876-01-01T00:00:00+00:00","1877-01-01T00:00:00+00:00","1878-01-01T00:00:00+00:00","1879-01-01T00:00:00+00:00","1880-01-01T00:00:00+00:00","1881-01-01T00:00:00+00:00","1882-01-01T00:00:00+00:00","1883-01-01T00:00:00+00:00","1884-01-01T00:00:00+00:00","1885-01-01T00:00:00+00:00","1886-01-01T00:00:00+00:00","1887-01-01T00:00:00+00:00","1888-01-01T00:00:00+00:00","1889-01-01T00:00:00+00:00","1890-01-01T00:00:00+00:00","1891-01-01T00:00:00+00:00","1892-01-01T00:00:00+00:00","1893-01-01T00:00:00+00:00","1894-01-01T00:00:00+00:00","1895-01-01T00:00:00+00:00","1896-01-01T00:00:00+00:00","1897-01-01T00:00:00+00:00","1898-01-01T00:00:00+00:00","1899-01-01T00:00:00+00:00","1900-01-01T00:00:00+00:00","1901-01-01T00:00:00+00:00","1902-01-01T00:00:00+00:00","1903-01-01T00:00:00+00:00","1904-01-01T00:00:00+00:00","1905-01-01T00:00:00+00:00","1906-01-01T00:00:00+00:00","1907-01-01T00:00:00+00:00","1908-01-01T00:00:00+00:00","1909-01-01T00:00:00+00:00","1910-01-01T00:00:00+00:00","1911-01-01T00:00:00+00:00","1912-01-01T00:00:00+00:00","1913-01-01T00:00:00+00:00","1914-01-01T00:00:00+00:00","1915-01-01T00:00:00+00:00","1916-01-01T00:00:00+00:00","1917-01-01T00:00:00+00:00","1918-01-01T00:00:00+00:00","1919-01-01T00:00:00+00:00","1920-01-01T00:00:00+00:00","1921-01-01T00:00:00+00:00","1922-01-01T00:00:00+00:00","1923-01-01T00:00:00+00:00","1924-01-01T00:00:00+00:00","1925-01-01T00:00:00+00:00","1926-01-01T00:00:00+00:00","1927-01-01T00:00:00+00:00","1928-01-01T00:00:00+00:00","1929-01-01T00:00:00+00:00","1930-01-01T00:00:00+00:00","1931-01-01T00:00:00+00:00","1932-01-01T00:00:00+00:00","1933-01-01T00:00:00+00:00","1934-01-01T00:00:00+00:00","1935-01-01T00:00:00+00:00"],"y":[30.09,45.15,49.15,39.52,21.23,8.42,5.56,5.08,10.17,19.6,32.91,34.38,29.59,21.3,13.69,7.65,4.08,4.09,14.33,38.22,60.78,70.77,72.77,42.68,16.39,9.83,5.8,5.26,18.91,30.95,31.18,46.34,45.77,44.15,36.33,12.03,12.6,18.34,35.14,43.77,65.69,79.35,51.65,32.59,22.45,16.16,14.12,20.38,33.33,46.0,51.41,46.43,33.68,18.01,8.86,7.13,9.47,14.86,31.47,60.57,63.51,54.7,6.3,3.41,5.44,11.65,20.35,32.88,39.55,43.36,40.83,30.36,17.18,6.82,3.19,3.52,9.94,20.3,31.99,42.36,49.08,53.99,52.25,37.7,19.14,6.98,8.31,16.01,24.82,29.7,35.4],"marker":{},"line":{},"xaxis":"x2","yaxis":"y2"}];
    var layout = {"width":600,"height":600,"template":{"layout":{"title":{"x":0.05},"font":{"color":"rgba(42, 63, 95, 1.0)"},"paper_bgcolor":"rgba(255, 255, 255, 1.0)","plot_bgcolor":"rgba(229, 236, 246, 1.0)","autotypenumbers":"strict","colorscale":{"diverging":[[0.0,"#8e0152"],[0.1,"#c51b7d"],[0.2,"#de77ae"],[0.3,"#f1b6da"],[0.4,"#fde0ef"],[0.5,"#f7f7f7"],[0.6,"#e6f5d0"],[0.7,"#b8e186"],[0.8,"#7fbc41"],[0.9,"#4d9221"],[1.0,"#276419"]],"sequential":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]],"sequentialminus":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]},"hovermode":"closest","hoverlabel":{"align":"left"},"coloraxis":{"colorbar":{"outlinewidth":0.0,"ticks":""}},"geo":{"showland":true,"landcolor":"rgba(229, 236, 246, 1.0)","showlakes":true,"lakecolor":"rgba(255, 255, 255, 1.0)","subunitcolor":"rgba(255, 255, 255, 1.0)","bgcolor":"rgba(255, 255, 255, 1.0)"},"mapbox":{"style":"light"},"polar":{"bgcolor":"rgba(229, 236, 246, 1.0)","radialaxis":{"linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","ticks":""},"angularaxis":{"linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","ticks":""}},"scene":{"xaxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","gridwidth":2.0,"zerolinecolor":"rgba(255, 255, 255, 1.0)","backgroundcolor":"rgba(229, 236, 246, 1.0)","showbackground":true},"yaxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","gridwidth":2.0,"zerolinecolor":"rgba(255, 255, 255, 1.0)","backgroundcolor":"rgba(229, 236, 246, 1.0)","showbackground":true},"zaxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","gridwidth":2.0,"zerolinecolor":"rgba(255, 255, 255, 1.0)","backgroundcolor":"rgba(229, 236, 246, 1.0)","showbackground":true}},"ternary":{"aaxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)"},"baxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)"},"caxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)"},"bgcolor":"rgba(229, 236, 246, 1.0)"},"xaxis":{"title":{"standoff":15},"ticks":"","automargin":"height+width+left+right+top+bottom","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","zerolinecolor":"rgba(255, 255, 255, 1.0)","zerolinewidth":2.0},"yaxis":{"title":{"standoff":15},"ticks":"","automargin":"height+width+left+right+top+bottom","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","zerolinecolor":"rgba(255, 255, 255, 1.0)","zerolinewidth":2.0},"annotationdefaults":{"arrowcolor":"#2a3f5f","arrowhead":0,"arrowwidth":1},"shapedefaults":{"line":{"color":"rgba(42, 63, 95, 1.0)"}},"colorway":["rgba(99, 110, 250, 1.0)","rgba(239, 85, 59, 1.0)","rgba(0, 204, 150, 1.0)","rgba(171, 99, 250, 1.0)","rgba(255, 161, 90, 1.0)","rgba(25, 211, 243, 1.0)","rgba(255, 102, 146, 1.0)","rgba(182, 232, 128, 1.0)","rgba(255, 151, 255, 1.0)","rgba(254, 203, 82, 1.0)"]},"data":{"bar":[{"marker":{"line":{"color":"rgba(229, 236, 246, 1.0)","width":0.5},"pattern":{"fillmode":"overlay","size":10,"solidity":0.2}},"error_x":{"color":"rgba(42, 63, 95, 1.0)"},"error_y":{"color":"rgba(42, 63, 95, 1.0)"}}],"barpolar":[{"marker":{"line":{"color":"rgba(229, 236, 246, 1.0)","width":0.5},"pattern":{"fillmode":"overlay","size":10,"solidity":0.2}}}],"carpet":[{"aaxis":{"linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","endlinecolor":"rgba(42, 63, 95, 1.0)","minorgridcolor":"rgba(255, 255, 255, 1.0)","startlinecolor":"rgba(42, 63, 95, 1.0)"},"baxis":{"linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","endlinecolor":"rgba(42, 63, 95, 1.0)","minorgridcolor":"rgba(255, 255, 255, 1.0)","startlinecolor":"rgba(42, 63, 95, 1.0)"}}],"choropleth":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"contour":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"contourcarpet":[{"colorbar":{"outlinewidth":0.0,"ticks":""}}],"heatmap":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"heatmapgl":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"histogram":[{"marker":{"pattern":{"fillmode":"overlay","size":10,"solidity":0.2}}}],"histogram2d":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"histogram2dcontour":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"mesh3d":[{"colorbar":{"outlinewidth":0.0,"ticks":""}}],"parcoords":[{"line":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"pie":[{"automargin":true}],"scatter":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scatter3d":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}},"line":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scattercarpet":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scattergeo":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scattergl":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scattermapbox":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scatterpolar":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scatterpolargl":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scatterternary":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"surface":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"table":[{"cells":{"fill":{"color":"rgba(235, 240, 248, 1.0)"},"line":{"color":"rgba(255, 255, 255, 1.0)"}},"header":{"fill":{"color":"rgba(200, 212, 227, 1.0)"},"line":{"color":"rgba(255, 255, 255, 1.0)"}}}]}},"title":{"text":"lynx"},"xaxis":{},"yaxis":{},"xaxis2":{},"yaxis2":{},"grid":{"rows":2,"columns":1,"pattern":"independent"}};
    var config = {"responsive":true};
    Plotly.newPlot('cc514e9a-df0d-4fe7-b9bf-226bd7a5fe1c', data, layout, config);
};
renderPlotly_cc514e9adf0d4fe7b9bf226bd7a5fe1c();
</script></div>

**NB: this documentation is auto-generated so we cannot comment directly on the randomly generated scenario.**

Next, we can examine the traces to see how parameter values evolved over the course of
the optimisation routine:

*)
Graphing.parameterTrace result(* output: 
<div><div id="f9ec8182-db37-4d1e-bdc2-1da9f65db4fb"><!-- Plotly chart will be drawn inside this DIV --></div><script type="text/javascript">var renderPlotly_f9ec8182db374d1ebdc21da9f65db4fb = function() {
    var layout = {"width":600,"height":600,"template":{"layout":{"title":{"x":0.05},"font":{"color":"rgba(42, 63, 95, 1.0)"},"paper_bgcolor":"rgba(255, 255, 255, 1.0)","plot_bgcolor":"rgba(229, 236, 246, 1.0)","autotypenumbers":"strict","colorscale":{"diverging":[[0.0,"#8e0152"],[0.1,"#c51b7d"],[0.2,"#de77ae"],[0.3,"#f1b6da"],[0.4,"#fde0ef"],[0.5,"#f7f7f7"],[0.6,"#e6f5d0"],[0.7,"#b8e186"],[0.8,"#7fbc41"],[0.9,"#4d9221"],[1.0,"#276419"]],"sequential":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]],"sequentialminus":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]},"hovermode":"closest","hoverlabel":{"align":"left"},"coloraxis":{"colorbar":{"outlinewidth":0.0,"ticks":""}},"geo":{"showland":true,"landcolor":"rgba(229, 236, 246, 1.0)","showlakes":true,"lakecolor":"rgba(255, 255, 255, 1.0)","subunitcolor":"rgba(255, 255, 255, 1.0)","bgcolor":"rgba(255, 255, 255, 1.0)"},"mapbox":{"style":"light"},"polar":{"bgcolor":"rgba(229, 236, 246, 1.0)","radialaxis":{"linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","ticks":""},"angularaxis":{"linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","ticks":""}},"scene":{"xaxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","gridwidth":2.0,"zerolinecolor":"rgba(255, 255, 255, 1.0)","backgroundcolor":"rgba(229, 236, 246, 1.0)","showbackground":true},"yaxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","gridwidth":2.0,"zerolinecolor":"rgba(255, 255, 255, 1.0)","backgroundcolor":"rgba(229, 236, 246, 1.0)","showbackground":true},"zaxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","gridwidth":2.0,"zerolinecolor":"rgba(255, 255, 255, 1.0)","backgroundcolor":"rgba(229, 236, 246, 1.0)","showbackground":true}},"ternary":{"aaxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)"},"baxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)"},"caxis":{"ticks":"","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)"},"bgcolor":"rgba(229, 236, 246, 1.0)"},"xaxis":{"title":{"standoff":15},"ticks":"","automargin":"height+width+left+right+top+bottom","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","zerolinecolor":"rgba(255, 255, 255, 1.0)","zerolinewidth":2.0},"yaxis":{"title":{"standoff":15},"ticks":"","automargin":"height+width+left+right+top+bottom","linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","zerolinecolor":"rgba(255, 255, 255, 1.0)","zerolinewidth":2.0},"annotationdefaults":{"arrowcolor":"#2a3f5f","arrowhead":0,"arrowwidth":1},"shapedefaults":{"line":{"color":"rgba(42, 63, 95, 1.0)"}},"colorway":["rgba(99, 110, 250, 1.0)","rgba(239, 85, 59, 1.0)","rgba(0, 204, 150, 1.0)","rgba(171, 99, 250, 1.0)","rgba(255, 161, 90, 1.0)","rgba(25, 211, 243, 1.0)","rgba(255, 102, 146, 1.0)","rgba(182, 232, 128, 1.0)","rgba(255, 151, 255, 1.0)","rgba(254, 203, 82, 1.0)"]},"data":{"bar":[{"marker":{"line":{"color":"rgba(229, 236, 246, 1.0)","width":0.5},"pattern":{"fillmode":"overlay","size":10,"solidity":0.2}},"error_x":{"color":"rgba(42, 63, 95, 1.0)"},"error_y":{"color":"rgba(42, 63, 95, 1.0)"}}],"barpolar":[{"marker":{"line":{"color":"rgba(229, 236, 246, 1.0)","width":0.5},"pattern":{"fillmode":"overlay","size":10,"solidity":0.2}}}],"carpet":[{"aaxis":{"linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","endlinecolor":"rgba(42, 63, 95, 1.0)","minorgridcolor":"rgba(255, 255, 255, 1.0)","startlinecolor":"rgba(42, 63, 95, 1.0)"},"baxis":{"linecolor":"rgba(255, 255, 255, 1.0)","gridcolor":"rgba(255, 255, 255, 1.0)","endlinecolor":"rgba(42, 63, 95, 1.0)","minorgridcolor":"rgba(255, 255, 255, 1.0)","startlinecolor":"rgba(42, 63, 95, 1.0)"}}],"choropleth":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"contour":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"contourcarpet":[{"colorbar":{"outlinewidth":0.0,"ticks":""}}],"heatmap":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"heatmapgl":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"histogram":[{"marker":{"pattern":{"fillmode":"overlay","size":10,"solidity":0.2}}}],"histogram2d":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"histogram2dcontour":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"mesh3d":[{"colorbar":{"outlinewidth":0.0,"ticks":""}}],"parcoords":[{"line":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"pie":[{"automargin":true}],"scatter":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scatter3d":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}},"line":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scattercarpet":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scattergeo":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scattergl":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scattermapbox":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scatterpolar":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scatterpolargl":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"scatterternary":[{"marker":{"colorbar":{"outlinewidth":0.0,"ticks":""}}}],"surface":[{"colorbar":{"outlinewidth":0.0,"ticks":""},"colorscale":[[0.0,"#0d0887"],[0.1111111111111111,"#46039f"],[0.2222222222222222,"#7201a8"],[0.3333333333333333,"#9c179e"],[0.4444444444444444,"#bd3786"],[0.5555555555555556,"#d8576b"],[0.6666666666666666,"#ed7953"],[0.7777777777777778,"#fb9f3a"],[0.8888888888888888,"#fdca26"],[1.0,"#f0f921"]]}],"table":[{"cells":{"fill":{"color":"rgba(235, 240, 248, 1.0)"},"line":{"color":"rgba(255, 255, 255, 1.0)"}},"header":{"fill":{"color":"rgba(200, 212, 227, 1.0)"},"line":{"color":"rgba(255, 255, 255, 1.0)"}}}]}},"xaxis":{},"yaxis":{},"xaxis2":{},"yaxis2":{},"xaxis3":{},"yaxis3":{},"xaxis4":{},"yaxis4":{},"xaxis5":{},"yaxis5":{},"xaxis6":{},"yaxis6":{},"xaxis7":{},"yaxis7":{},"grid":{"rows":3,"columns":3,"pattern":"independent"}};
    var config = {"responsive":true};
    Plotly.newPlot('f9ec8182-db37-4d1e-bdc2-1da9f65db4fb', data, layout, config);
};
renderPlotly_f9ec8182db374d1ebdc21da9f65db4fb();
</script></div>*)
