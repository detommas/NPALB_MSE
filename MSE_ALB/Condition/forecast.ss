#V3.24AB
# for all year entries except rebuilder; enter either: actual year, -999 for styr, 0 for endyr, neg number for rel. endyr
1 # Benchmarks: 0=skip; 1=calc F_spr,F_btgt,F_msy 
1 # MSY: 2= set to F(SPR); 2=calc F(MSY); 3=set to F(Btgt); 4=set to F(endyr) 
0.2 # SPR target (e.g. 0.40)
0.2 # Biomass target (e.g. 0.40)
#_Bmark_years: beg_bio, end_bio, beg_selex, end_selex, beg_relF, end_relF (enter actual year, or values of 0 or -integer to be rel. endyr)
 -3 -1 -3 -1 -3 -1
#  2012 2014 2012 2014 2012 2014 # after processing 
1 #Bmark_relF_Basis: 1 = use year range; 2 = set relF same as forecast below
#
0 # Forecast: 0=none; 1=F(SPR); 2=F(MSY) 3=F(Btgt); 4=Ave F (uses first-last relF yrs); 5=input annual F scalar
0 # N forecast years 
0 # F scalar (only used for Do_Forecast==5)
#_Fcast_years:  beg_selex, end_selex, beg_relF, end_relF  (enter actual year, or values of 0 or -integer to be rel. endyr)
 0 0 0 0
#  41985216 0 7631713 32607 # after processing 
0 # Control rule method (1=catch=f(SSB) west coast; 2=F=f(SSB) ) 
0 # Control rule Biomass level for constant F (as frac of Bzero, e.g. 0.40); (Must be > the no F level below) 
0 # Control rule Biomass level for no F (as frac of Bzero, e.g. 0.10) 
0 # Control rule target as fraction of Flimit (e.g. 0.75) 
0 #_N forecast loops (1=OFL only; 2=ABC; 3=get F from forecast ABC catch with allocations applied)
0 #_First forecast loop with stochastic recruitment
986089400 #_Forecast loop control #3 (reserved for future bells&whistles) 
32607 #_Forecast loop control #4 (reserved for future bells&whistles) 
-1 #_Forecast loop control #5 (reserved for future bells&whistles) 
0  #FirstYear for caps and allocations (should be after years with fixed inputs) 
0 # stddev of log(realized catch/target catch) in forecast (set value>0.0 to cause active impl_error)
0 # Do West Coast gfish rebuilder output (0/1) 
0 # Rebuilder:  first year catch could have been set to zero (Ydecl)(-1 to set to 1999)
0 # Rebuilder:  year for current age structure (Yinit) (-1 to set to endyear+1)
1 # fleet relative F:  1=use first-last alloc year; 2=read seas(row) x fleet(col) below
# Note that fleet allocation is used directly as average F if Do_Forecast=4 
0 # basis for fcast catch tuning and for fcast catch caps and allocation  (2=deadbio; 3=retainbio; 5=deadnum; 6=retainnum)
# Conditional input if relative F choice = 2
# Fleet relative F:  rows are seasons, columns are fleets
#_Fleet:  F1_JPLL_A1_Q12_wt F2_JPLL_A1_Q34_wt F3_JPLL_A1_Q12_num F4_JPLL_A1_Q34_num F5_JPLL_A2_wt F6_JPLL_A2_num F7_JPLL_A3_Q12_wt F8_JPLL_A3_Q34_wt F9_JPLL_A3_Q12_num F10_JPLL_A3_Q34_num F11_JPLL_A4_wt F12_JPLL_A4_num F13_JPLL_A5_num F14_JPPL_A3_Q12 F15_JPPL_A3_Q34 F16_JPPL_A2 F17_JPTWKR_DN F18_JPTW_MISC F19_TWLL_A35 F20_TWLL_A24 F21_KRLL F22_CNLL_A35 F23_CNLL_A24 F24_VNLL_A35 F25_VNLL_A24 F26_USLL_A35 F27_USLL_A24 F28_EPOSF
#  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# max totalcatch by fleet (-1 to have no max) must enter value for each fleet

# max totalcatch by area (-1 to have no max); must enter value for each fleet 

# fleet assignment to allocation group (enter group ID# for each fleet, 0 for not included in an alloc group)

#_Conditional on >1 allocation group
# allocation fraction for each of: 0 allocation groups
# no allocation groups
0 # Number of forecast catch levels to input (else calc catch from forecast F) 
-1 # code means to read fleet/time specific basis (2=dead catch; 3=retained catch; 99=F)  as below (units are from fleetunits; note new codes in SSV3.20)
# Input fixed catch values
#Year Seas Fleet Catch(or_F) Basis
#
999 # verify end of input 
