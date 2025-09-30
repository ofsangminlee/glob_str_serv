# Globalization and Structural Transformation: The Role of Tradable Services

This repository contains the source code and data for replicating all results in the paper:

  Lee, Sang Min (2025): “Globalization and Structural Transformation: The Role of Tradable Services”

## Structure

- `data` contains data used for the paper.
  - `icio`: OECD Inter-Country Input-Output Database 2018 (`icio` data are uploaded with Google Drive.)
  - `prices`: price data from OECD, UN, Groningen Growth and Development Centre, and other sources
  - `pwt`: Penn World Table 10.0
  - `BEA`: BEA/BLS KLEMS for the US

- `src` contains all source code for replication.
  - `data`: `R` code for data cleaning; generating trade, input-output, and price data; and producing motivating plots
  - `param`: `R` code for estimating model parameters, calculating primitives (e.g, productivities and trade costs), and analyzing the patterns of globalization
  - `model`: `julia` code for baseline model simulation, decomposition, and counterfactuals; `R` code for visualization of the results; `R` code for the analysis of productivity growth

- `output` contains outputs from running the source code
  - `cleaned_data`: cleaned data used for parameter estimation and inference of model primitives
  - `params`: estimated parameters and calculated model primitives
  - `model`: results from the models

- `doc`: contains figures, tables, and numbers, generated through the source code, that are in the paper.

## Running the Code

Before running the code, there are four preliminary steps.

1. Clone the repository: at your desired location, type in the terminal:

```
git clone https://github.com/ofsangminlee/glob-str-serv.git
```

2. Download the five zip files for the ICIO data from the Google Drive folder (<https://drive.google.com/drive/folders/1coLnNmdi6E8BjIB3mcB5Maf48WILAKHt?usp=share_link>). Place the five zip files in the `data/icio` directory.

The code is in `R` and `Julia`.

For replication under the same version of open-source packages and dependencies in `R` and `Julia`, follow the steps below.

3. Open an `R` prompt at the parent folder of this project. Then type:

```
renv::load(".")
renv::restore()
```

4. Open a `Julia` prompt at the parent folder. Then type:

```
using Pkg 
Pkg.activate(".")
Pkg.instantiate()
```

Now you can automatically generate all results by running Makefile using `GNU Make`. If you are using a Unix-based OS (e.g., Linux, macOS), in your terminal at the subfolder `src`, run `make all`. This command will erase all the results in the `output` and `doc` folders and regenerate them. In this repository, due to storage limitations, the `output` and `doc` folders are intentionally left empty. Running `make all` will populate them with the generated results.

For Windows users, you can install `Windows Subsystem for Linux (WSL)` and follow the above steps. Alternatively, you can manually run the code files in the order outlined in `Makefile`. `Makefile` can be opened with text editors.

## Walkthrough: From Results Back to Code

In this section, I explain how the tables and figures in this paper are generated through code. Where helpful, I illustrate the sequence of codes in the reverse order. All referenced source files are in the `./src/` directory.

- Table 1: Classification of Industries into Three Sectors: `/data/motivation_two_services.R`
- Figure 1: Sectoral Shares in Production of Country-years by Per-Capita Real Income: `/data/motivation_str_tran.R`
- Table 2: Parameter Values:
  - Preference parameters: `/param/preference.R`
  - Production parameters: `/param/production.R`
- Figure 2: Box Plots for Trade Costs in 1995 and 2018 by Country-group Pairs and Figure 3: GDP per Capita and the Growth Rate of Relative Export Trade Costs: `/param/analyze_tau.R` ← `/param/cal_prd_tau.R`
- Table 3: Summary Statistics for the Decomposition of Structural Transformation into Four Mechanisms and Table 4: Summary Statistics for the Decomposition of Structural Transformation into Globalization and Productivity Growth: `/model/shapley.jl` ← `/model/run_models.jl` (a model subroutine is in `/model/model.jl`) ← `/model/inputs.jl`  ← Parameters and primitives from codes in `/param/`. 
- Figures 4 through 6: Globalization and Structural Transformation of China, Vietnam, Lithuania
  - (a) Dynamics of Sectoral Trade Costs: `/param/analyze_tau.R`
  - (b) Structural Transformation in the Baseline and Counterfactuals #1 to #3: `/model/plot_one.jl` ← `/model/clean_result.jl` ← `/model/run_models.jl`
- Figure 7: GDP per Capita and the Effects of Globalization in (a) Goods, (b) Services, and (c) All Sectors on Structural Transformation and Figure 8: Globalization Bias Index and the Effects of Globalization in All Sectors on Structural Transformation: `/model/plot_67.jl` ← `/model/clean_result.jl` ← `/model/run_models.jl`
- Table 5: Summary Statistics for the Absolute Values of the Effect of Globalization in the Main, Non-tradable Services, Symmetric Trade Cost Models: `/model/plot_67.jl` (This function says "plot" but it also generates tables as well.)
- Figure 9: Results for China (Model with Symmetric Trade Costs):
  - (a) Dynamics of Sectoral Trade Costs:  `/param/analyze_tau.R` ← `/param/cal_tau_head_ries.R` 
  - (b) Structural Transformation in the Baseline and Counterfactuals #1 to #3: `/model/plot_one.jl` ← `/model/clean_result.jl` ← `/model/run_models.jl`
- Table 6: Estimation Results for the Dynamics of Trade Costs and Productivities: `/param/prd_tau_interaction.R`
- Figure 10: Results from the Model with Spillover Effects: `/model/plot_67.jl`
- Tables in Appendix D (Estimation): `/param/preference.R` and `/param/production.R`
- Figures in Appendix E (Productivity Growth): `/model/analyze_prd.R` ← `/model/clean_result.jl` ← `/model/run_models.jl` (To calculate labor productivity, I need to solve for the baseline equilibrium.)
- Figures in Appendix G. (Model Fit, Non-targeted Moments): `/model/plot_usa_row.R` ← `/model/run_usa_row.jl`
