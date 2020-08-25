# COVID-19 Contact Tracing Model

 <font size="4"> This repository contains code for a simple model for COVID-19 contact tracing, described in detail in [this paper](https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2769618).  The model is implemented [here](https://github.com/abilinski/spark_control/blob/master/App/contact_tracing_v5.R), and replication code for the paper is available [here](https://github.com/abilinski/spark_control/blob/master/Paper/replication_file.R).
 
 You can see the structure of the model in the diagram below.  Infected individuals generate new infections based on whether they have symptoms, whether disease is detected, and whether they have been identified as a contact of an infected individual. You can view the a live app version of the model [here](https://alyssab.shinyapps.io/spark_control/).
  </font>
  
  <img src="https://github.com/abilinski/spark_control/blob/master/App/content/model_diagram.png" alt="Model diagram" style="width:400px;" class="center"/>
