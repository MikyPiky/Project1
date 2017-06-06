# In this file the steps for the paper "The Effect of Soil Moisture Anomalies on Maize Yield in Germany" published in 
NHESS

The steps are
1) Data preparation
1a) Transform data (meteorology and SMI) from 4*4 km² to Administrative Districts
    Bisher mache ich das mit einzelenen Scripts für jede meteorologische Variable und SMI (4km_SMI_new, etc). 
    Ich denke, es macht mehr Sinn das in einem Script mit einer loop zu machen. So wie ich e für die Projectionen 
    schon gemacht habe. Die Plot Sachen muss ich weiter berücksichtigen, aber nur für SMI, da ich diese im Paper zeige.
    Dort ist es das spatial processing. 
    Das Problem ist, dass ich die Daten bis auf Pre_lk.nc nicht mehr finde. Evtl. sind die Daten noch auf dem Cluster
    zu finden (kann erst am UFZ überprüft werden. ) 
    Die erstellten Daten gibt es aber und befinden sich im data_processed ordner (Pet_months, etc). 
    
    In diesem Kontext muss ich auch auf das Script zum erstellen der SMI PLOTS zu Spatial Processing achten. 
    
1b) Combine those data with yield data
    Das passiert im Merge Yield_Meteo Script. 
    Output ist yieldData_meteo.csv
    

2) Model Selection based on BIC
    Hier sind die Input Daten dann yieldData_meteo.csv. Diese Daten müssten veröffentlicht werden, wenn mann volle 
    Transparenz möchte. Würde ich aber ungern machen, da dann viele andere damit arbeiten könnten ohne an mich ran
    treten zu müssen. 

3) Results of selected models for each month