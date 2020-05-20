## Overview
 
 <font size="4"> 
COVID-19 spread rapidly around the globe.  Physical distancing measures have slowed the spread of COVID-19 and reduced R(t), the average number of infections currently generated per new case.  Once R(t) is at or below 1, new cases no longer grow exponentially.  However, when we relax physical distancing, R(t) will increase.  Contact tracing is one of our best tools to ensure that R(t) stays below the critical threshold of 1.  With this model, we show to what extent adding contact tracing would allow us to relax physical distancing measures, while still containing the spread of COVID-19.
  </font>

### About this model
 <font size="4"> This app implements a simple model for COVID-19 contact tracing, described in detail in [this paper](https://www.medrxiv.org/content/10.1101/2020.05.05.20091280v1.full.pdf).  You can see the structure of the model in the picture below.  Infected individuals generate new infections based on whether they have symptoms, whether disease is detected, and whether they have been identified as a contact of an infected individual.  Based on current research, we assumed that symptomatic cases become infectious prior to emergence of symptoms. 
  </font>
    
### About the results

  <font size="4">
  
1. **R(t) with contact tracing**: When contact tracing in introduced, it reduces the effective reproductive number to a value that we'll call R(t)<sup>\*</sup> in this document.  This plot shows R(t)<sup>\*</sup> for the corresponding strategy.

2. **Fraction of current measures needed for R(t)<1 after contact tracing**: Initial measures reduced the reproduction number from R<sub>0</sub> to R(t).  If the population mixed randomly, R(t)/R<sub>0</sub> would be the average fraction of contacts that were maintained with physical distancing measures.  We estimate the fraction of these measures that still be needed to ensure containment with contact tracing in place: (R<sub>0</sub> - 1/R(t)<sup>\*</sup>)/(R<sub>0</sub> - R(t))

  To understand this better, here is an example: assume that current physical distancing measures have reduced the reproductive number from R<sub>0</sub> = 2.4 to R(t) = 1.0, and that a contact tracing strategy could ensure R(t)<sup>\*</sup> = 0.6. Under these parameters, containment would be possible if relaxed physical distancing measures on their own could maintain R(t) below 1/0.6 = 1.67, because the further reduction by a factor of 0.6 due to contact tracing would bring R(t) below 1.0. This implies that together with contact tracing, physical distancing measures could be applied at 55% of their current, full implementation effectiveness and still maintain the critical containment threshold of R(t)<1.

3. **Fraction of detected cases identified through contact tracing**: When contact tracing programs are implemented, some cases will still be detected in the community while others through contacts.  This plot shows the expected ratio of new cases from these different sources.

4. **R(t) by symptom status**: New infections can come from presymptomatic, symptomatic, or asymptomatic individuals.  This plot shows the average contribution to R(t) from each of these sources.

</font>

### A little math
<font size="4">
We use model parameters to define the next-generation matrix **T**.  The *i,j* th entry of **T** tells us how many infections on average an individual of type *i* generates in type *j*, with types corresponding to the diagram below (e.g. not contact traced, symptomatic, undetected).  R(t)<sup>\*</sup> is the dominant eigenvalue of **T**.  The eigenvector associated with R(t)<sup>\*</sup> tells us the expected steady state of distribution of infection types in the population.  To find (3) and (4), we reverse the process by taking the transpose of **T**.  The eigenvector associated with the dominant eigenvalue of **T'** tells us the steady state distribution of generating infections.

</font>

### Notes
 
  <font size="4">

1. While not shown in the diagram, in this app, the model divides the contact traced pathway into two: 1) contact traced from an index case in the community ("first generation") and 2) contact traced from an index case identified through contact tracing ("second generation"), allowing the user to set different isolation and quarantine efficacy.  (For example, if individuals are tested less quickly in the community than when they are identified as a contact, isolation and quarantine efficacy will be lower in the first generation than in the second.) 

2. For advanced users: This model mainly focuses on the incremental value of adding contact tracing.  In the advanced tab, the user can also consider the joint impact of scaling testing AND adding contact tracing.  The user must then select baseline testing rates in symptomatic and asymptomatic individuals, and this renders the model more sensitive to the behavioral impact of testing.

3. This is a simple model that does not consider household or other network structures, which impact the effectiveness of contact tracing.

For further discussion and parameter estimates, see [this paper](https://www.medrxiv.org/content/10.1101/2020.05.05.20091280v1.full.pdf).
  </font>

  <img src="model_diagram.png" alt="Model diagram" style="width:800px;" class="center"/>

