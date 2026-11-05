# Estimate vulnerable children after campaigns in ideal and reduced scope

This project aims to estimate the additional number of vulnerable children to poliomyelitis with a reduced scope of target population of vaccine campaigns planned in November to December, 2026 in Nigeria, Ethiopia, Central African Republic and Chad.  

## Methods
### Unprotected children before strategies

$$
U_{pre} = \sum_{a=0}^{4} b_a \times P(SIA < 3) 
$$

$$
P(SIA) ~ Binomial(n, n_{total_{SIA}}, cov)
$$

$U_{pre}$: the unprotected children before strategies per district
$b_a$: birth cohort at age a in 2026
$P(SIA)$: the probability of getting $n$ round of SIAs given there are $n_{total_{SIA}} rounds done since the birth cohort was born, with each round has certain coverage $cov$
Then this number is aggregated across age and districts to get the number of unprotected children per province $U_{prov}$. 

### Unprotected children and symptomatic cases after strategies

The number of protected children from the strategies is calculated as: 

$$
V= cov \times cov \times TP \times \frac{TP}{pop_{prov}} 
$$

$cov$: the coverage per SIA round
$TP$: the target population in strategies either ideal or reduced scope
$pop_{prov}$: the population for each province
This assumes that the unprotected children are homogeneously distributed across the province, and two SIA rounds will apply proportionally to the fraction of target population among the total population of the province. 
The number of unprotected children after strategies are simply $U_{post}= U_{pre}- V$.
