# North Pacific Albacore Tuna Management Strategy Evaluation

This repository contains the code to run the management strategy
evaluation (MSE) for North Pacific Albacore Tuna (NPALB) that was run by the ISC NPALB working group. 
The report outlining details of the modeling framework and results is available at
The repository is intended to be a code sharing and collaboration platform for all the
members of the ISC NPALB working group.

Note that both the operating and estimation models are based on the
Stock Synthesis software. The operating model is based on the ISC PBFWG
2017 assessment model. Note that the code was written
for a Windows operating system and tested with SS version V3.24, R
version 4.1.3, and r4ss package 1.45.3.

## Getting started with running the NPALB MSE

-   Clone the repository to your computer. It contains all the directories
    and files needed to run the MSE. Note that you need to keep this same directory
    structure to be able to run the code. The harvest strategy (with starter file),
    HCR (with forecast file), and scenario folders need to be present before starting the simulation.
    The numbered directory has the following format
    *harvest strategy/hcr/scenario/iteration/time step*
-   This steps will first help you run simulations with TAC control.
    Change paths at the start of *MSE2_prll_multi.R* and *NPALB_MSE2_hs32_tvry.R* to
    reflect the path where the MSE_ALB folder is on your computer.
-   The *MSE2_prll_multi.R* is a wrapper code and is the only one you need to run a similuation for the
    specified harvest strategy (hsnum), harvest control rule (hn), and scenario (scnnum). 
    In the foreach loop you set the number of iterations. The first time you run the code, do so for one
    iteration. Running the full 30-year simulation for 1 harvest strategy, hcr, scenario, and itr will
    take about 4 hrs.
-   As you are starting to run the NPALB MSE you should also specify the inputs via the *MSE2_prll_multi.R*,
    but then run snippets of the main NPALB MSE loop function *NPALB_MSE2_hs32_tvry.R*, e.g. by starting with 1 timestep
    so that you understand what it does.
-   When you finish running, you should see in RStudio the output of the
    outmat table that collects all the information to generate
    performance metrics. This information is also saved as a text file,
    *outlist.txt*.
-   Once the code for 1 iteration works, you can try running using the *MSE2_prll_multi.R* to run multiple iterations in parallel
-   The table below outlines the wrapper code and main function for running HCRs with other types of management controls than TAC
    or scenarios

    Harvest Strategy | Productivity Scenario | Wrapper Code | MSE Loop Function
    --- | --- | --- | ---
    HS1 = TAC | 1 or 3 | MSE_prll_multi.R | NPALB_MSE_hs32_tvry.R
    HS1 = TAC | 4 or 6 | MSE_prll_multi46.R | NPALB_MSE_hs3_tvry.R
    HS4 = TAC and ghost fleet | 1 | MSE_prll_multi_scn9.R | NPALB_MSE_hs3_tvry_scn9.R
    HS2 = mixed control | 1 or 3 | MSE_prll_tactae_rhist.R | NPALB_MSE_hs3_tvry_tactae_rhist.R
    HS2 = mixed control | 4 or 6 | MSE_prll_tactae.R | NPALB_MSE_hs3_tvry_tactae.R
    HS2 = mixed control and ghost fleet | 1 | MSE_prll_tactae_scn9_rhist.R | NPALB_MSE2_hs3_tvry_scn9tactae_rhist.R
    HS3 = TAE | 1 or 3 | MSE_prll_tae_rhist.R | NPALB_MSE_hs3_tvry_tae_rhist.R
    HS3 = TAE | 4 or 6 | MSE_prll_tae.R | NPALB_MSE_hs3_tvry_tae.R





