[![R build status](https://github.com/leca-dev/RFate/workflows/R-CMD-check/badge.svg)](https://github.com/leca-dev/RFate/actions)
[![Coverage status](https://github.com/leca-dev/RFate/workflows/CODECOV-check/badge.svg)](https://github.com/leca-dev/RFate/actions)
[![Coverage status](https://codecov.io/gh/leca-dev/RFate/branch/master/graph/badge.svg)](https://codecov.io/gh/leca-dev/RFate/branch/master)

<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.2/css/all.min.css">


<style>
pre.bash {
 background-color: black;
 color: #9ea1a3;
 font-family: Consolas,Monaco,Lucida Console,Liberation Mono,DejaVu Sans Mono,Bitstream Vera Sans Mono,Courier New, monospace;
}
pre.grey {
 background-color: white;
 border-style: solid;
 border-color: #8b8d8f;
 color: #8b8d8f;
 font-family: Consolas,Monaco,Lucida Console,Liberation Mono,DejaVu Sans Mono,Bitstream Vera Sans Mono,Courier New, monospace;
}
.zoom p {
width:600px;
margin-left: auto;
margin-right: auto;
}
.zoom p:hover {
width:1200px;
position: relative;
z-index: 10;
}
</style>


<br/>


## <i class="fa-solid fa-screwdriver-wrench"></i> Installing `RFate` package

From [GitHub](https://github.com/leca-dev/RFate) using [devtools](https://cran.r-project.org/package=devtools) :

<pre class = "bash">
library(devtools)
devtools::install_github(repo="leca-dev/RFate")
</pre>

**Note :** `RFate` is currently having problems compiling on Windows with RTools4.3 <i class="fa-solid fa-triangle-exclamation"></i>

<br/><br/>



## <i class="fa-solid fa-shoe-prints"></i> `RFate` workflow

<br/>

`FATE` is a **spatially and temporally explicit vegetation model**. It uses **plant functional groups (PFG)** and integrates important mechanisms driving vegetation dynamics, structure and diversity, such as **demographic cycle**, obviously, but also **seeds dispersal**, **abiotic filtering** or **biotic interactions** (through the competition for resources like light availability or soil nutrient availability).

If **primary succession** is the most obvious ecological process that can be modelled with `FATE`, events related to **secondary succession** can be represented as well using the various **`FATE` add-on modules** : disturbances (mowing, grazing, fire..), drought event, invasive species.

<div style="text-align:center;">
<img src="articles/pictures/SCHEMA_succession1.jpg" alt="Primary succession" style="width:600px;"></img>
</div>

<br/>

As vegetation modelling can be challenging (data gathering, parameterization, handling results...), `RFate` provides **user-friendly functions** to go through the **whole `FATE` workflow**. Links below present some **guidance documents**, for the vegetation model as well as for the `R`package.

<br/>

**0. Understand how `FATE` works :**

- [the litterature](articles/fate_tutorial_0_publications.html)
- [and the software](articles/fate_tutorial_0_modelling_framework.html)

**1. Build PFG :**

- [the principle](articles/fate_tutorial_1_PFG.html)
- [and the tools](articles/rfate_tutorial_1_PFG.html)
    
**2. Run a `FATE` simulation :**

- [understand how to run a simulation,](articles/fate_tutorial_2_RUN_SIMULATION.html)
- [the different modules that can be used,](articles/fate_tutorial_3_MODULES.html)
- [and how to prepare the corresponding parameter files](articles/rfate_tutorial_2_params.html)
    
**3. Analyze the outputs :**

- [transform results and produce graphics](articles/rfate_tutorial_3_graphics.html)


<div class="zoom">
<p><img src="articles/pictures/SCHEMA_FATE_WORKFLOW_functions.png" alt="Main workflow"></img></p>
</div>

