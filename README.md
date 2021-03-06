# ABT-simtesting: 
# Performance of stock assessments for mixed-population fisheries: The illustrative case of Atlantic bluefin tuna
We used simulation testing to evaluate the performance of the virtual population analysis models used to manage Atlantic bluefin tuna fisheries (ICCAT 2018) on fishery pseudodata generated using an operating model that incorporated movement and mixing of populations.
<br>
<br>
Methods and results are documented in Molly Morse's master's thesis ([Morse 2018](https://www.researchgate.net/profile/Molly_Morse/publication/327562078_MISSPECIFYING_THE_MAGURO_EVALUATING_STOCK_ASSESSMENT_PERFORMANCE_ON_MIXED_STOCKS_OF_ATLANTIC_BLUEFIN_TUNA/links/5b96ec88a6fdccfd54403c18/MISSPECIFYING-THE-MAGURO-EVALUATING-STOCK-ASSESSMENT-PERFORMANCE-ON-MIXED-STOCKS-OF-ATLANTIC-BLUEFIN-TUNA.pdf)).
<br>
<br>
Deterministic simulations are run using the "OM+VPAsims_BFT2017_deterministic_V2.R" script and stochastic simulations are run using the "OM+VPAsims_BFT2017_basecase_V5.R" script. Directories for storing estimation model input and output files must first be created locally using the first lines of code under the "RUNNING SIMULATIONS" section of either of these scripts (will need to be uncommented). Plots for the ICES JMS manuscript were generated using the "ICES_manuscript_plotcode.R" and "Reference_points_v2020.R" scripts. Note that working directories will need to be changed in order to run the simulations and plotting code. 
<br>
<br>
Atlantic bluefin tuna simulation testing framework created by Molly Morse (UMass Dartmouth, UC Santa Barbara), Lisa Kerr (GMRI), Ben Galuardi (UMass Dartmouth, NOAA), and Steven Cadrin (UMass Dartmouth). Questions should be directed to Molly Morse, mollymorse@ucsb.edu.
<br>
<br>
<br>
References
<br>
<br>
ICCAT. 2018. Report of the 2017 ICCAT bluefin stock assessment meeting. Collect. Vol. Sci. Pap. ICCAT 74(6): 2372-2535.
<br>
<br>
Morse, M.R. 2018. Misspecifying the Maguro: Evaluating stock assessment performance on mixed stocks of Atlantic bluefin tuna. M.Sc. thesis, Department of Fisheries Oceanography, University of Massachusetts Dartmouth, Dartmouth, M.A.
