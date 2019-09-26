# MD_Stream_Salamanders

Main repository for Jacey Brooks thesis project and analyses examining the effect of AMD stream remediation on stream-breeding salamanders.


## Files

### Regional Occupancy Models

Each species will be modeled individually because of discrete range boundaries creating challenges for a multi-species model and not that many species for random effects any way.

Folder: `Code/`

1. `combine_obs_data.R` - formats and organizes occupancy data before being organized in `prep_occ.R` 

2. `query_climate.R` - get the daymet climate data averaged by catchment from USGS Conte SHEDS db

3. `format_landscape_data.R` - format and organize the landscape data

4. `prep_occ.R` - organize data for modeling (not sure if necessary)

5. `dynamic_occ_jags.R` - run the MCMC and save results

6. `check_mcmc.R` - check posteriors

7. `summarize_results.R` - make tables and figures of results


### Western MD Abundance Modeling
data_exploration.R - code for making summary tables and figures of the raw data

run_jags.R - script for running JAGS N-mixture model