######################################
############################  PREAMBLE
######################################

### Point to MimiIWG in dev directory
# ] dev MimiIWG

### Using
using MimiIWG
using Random

######################################
######################  SET PARAMETERS
######################################

seed = 42
N = 10000
years = collect(2020:5:2060)
discount_rates = [0,0.01,0.015,0.02,0.025, 0.03, 0.05, 0.07]

######################################
################################  DICE
######################################

# :CO2
Random.seed!(seed)
MimiIWG.run_scc_mcs(DICE, gas=:CO2, trials=N,
                    perturbation_years=years,
                    discount_rates=discount_rates,
                    domestic = true,
                    tables= false,
                    output_dir="data/co2/dice")

# :CH4
Random.seed!(seed)
MimiIWG.run_scc_mcs(DICE, gas=:CH4, trials=N,
                    perturbation_years=years,
                    discount_rates=discount_rates,
                    domestic = true,
                    tables= false,
                    output_dir="data/ch4/dice")

# :N2O
Random.seed!(seed)
MimiIWG.run_scc_mcs(DICE, gas=:N2O, trials=N,
                    perturbation_years=years,
                    discount_rates=discount_rates,
                    domestic = true,
                    tables= false,
                    output_dir="data/n2o/dice")

######################################
################################  PAGE
######################################

# :CO2
Random.seed!(seed)
MimiIWG.run_scc_mcs(PAGE, gas=:CO2, trials=N,
                    perturbation_years=years,
                    discount_rates=discount_rates,
                    domestic = true,
                    tables= false,
                    output_dir="data/co2/page")

# :CH4
Random.seed!(seed)
MimiIWG.run_scc_mcs(PAGE, gas=:CH4, trials=N,
                    perturbation_years=years,
                    discount_rates=discount_rates,
                    domestic = true,
                    tables= false,
                    output_dir="data/ch4/page")

# :N2O
Random.seed!(seed)
MimiIWG.run_scc_mcs(PAGE, gas=:N2O, trials=N,
                    perturbation_years=years,
                    discount_rates=discount_rates,
                    domestic = true,
                    tables= false,
                    output_dir="data/n2o/page")

######################################
################################  FUND
######################################

# :CO2
Random.seed!(seed)
MimiIWG.run_scc_mcs(FUND, gas=:CO2, trials=N,
                    perturbation_years=years,
                    discount_rates=discount_rates,
                    domestic = true,
                    tables= false,
                    output_dir="data/co2/fund")

# :CH4
Random.seed!(seed)
MimiIWG.run_scc_mcs(FUND, gas=:CH4, trials=N,
                    perturbation_years=years,
                    discount_rates=discount_rates,
                    domestic = true,
                    tables= false,
                    output_dir="data/ch4/fund")

# :N2O
Random.seed!(seed)
MimiIWG.run_scc_mcs(FUND, gas=:N2O, trials=N,
                    perturbation_years=years,
                    discount_rates=discount_rates,
                    domestic = true,
                    tables= false,
                    output_dir="data/n2o/fund")

# ## END OF SCRIPT. Have a great day!
