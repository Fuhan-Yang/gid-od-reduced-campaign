# gid-od-reduced-campaign

This project aims to estimate the additional number of AFP cases caused by poliomyelitis with a reduced scope of target population of vaccine campaigns planned in November to December, 2026 in Nigeria, Ethiopia, Lake Chad Basin, Niger, and DRC, through estimating the direct effects of vaccine in preventing AFP cases. 

## Methods

$$
C_{obs} = C_{counterfactual} \times (1- VE \times v)
$$

This method assumes that vaccine only protects vaccinees from progressing to paralytic cases, not considering reduction in transmission. 

1. Infer the $C_{counterfactual}$. 
- Get VE estimates from literature. 
- Get vaccine coverage time series from WUNEIC for IPV and POLIS for SIAs. 
- Get AFP case time series from POLIS. 
- Aggregate the vaccine coverage data and the case data into the same time scale (month for now) by province. 
- Estimate the number of counterfactual cases in the absence of vaccination by province. 

2. Estimate the additional AFP cases given reduced coverage of campaign. 
- Use the estimated counterfactual cases to calculate the number of AFP cases expected given varied vaccine coverage. 
