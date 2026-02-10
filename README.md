## Sampedro_etal_PM25_Health_Ineq

**Evaluating inter- and intra-regional air pollution inequalities in a decarbonized world**

Jon Sampedro<sup>1,2,*</sup>, Clàudia Rodés-Bachs<sup>1</sup>,  Dirk-Jan Van de Ven<sup>1</sup>, Cathryn Tonne<sup>3</sup>, Claudio Belis<sup>4</sup>, Mikel Gonzalez-Eguino <sup>1,2,5</sup> 

<sup>1 </sup> Basque Center for Climate Change, Leioa, Spain

<sup>2 </sup> IKERBASQUE, Basque Foundation for Science, Plaza Euskadi 5, 48009 Bilbao, Spain

<sup>3 </sup> ISGlobal, Universitat Pompeu Fabra, CIBER Epidemiología y Salud Pública, Doctor Aiguader 88, 08003 Barcelona, Spain

<sup>4 </sup> European Commission – Joint Research Centre, via Fermi 2749, 21027, Ispra, Italy

<sup>5 </sup> Department of Economic Analysis, University of the Basque Country (UPV/EHU), Bilbao, Spain

\* corresponding author:  jon.sampedro@bc3research.org

## Abstract
Ambient PM2.5 exposure remains a major global health challenge. While pollution levels have declined in many developed economies, they have increased in developing regions, exacerbating inequalities across countries. Although within-country air pollution disparities have narrowed in some contexts, they remain significant. This study assesses how inequalities both across and within countries evolve under alternative decarbonization futures, co-designed with diverse stakeholders and aligned with current climate policy portfolios. Using an integrated framework that combines an enhanced integrated assessment model with air quality modelling, we find that even with full implementation of national climate commitments, substantial PM2.5-related health impacts persist, especially in developing regions, maintaining or worsening cross-country inequalities. We also observe persistent within-region disparities, with wealthier groups contributing disproportionately to emissions, while in some developing economies the lowest-income populations experience the highest exposure. These findings highlight the need for targeted policies that integrate climate action with air quality and equity objectives.    

## Code reference
The GCAM_GRAPHICS model used in this study is available at Zenodo: https://zenodo.org/records/16960260

jonsampedro. (2025). GCAM-GRAPHICS-7.0.0 (GCAM-GRAPHICS-v7.0.0). Zenodo. https://doi.org/10.5281/zenodo.16960260


## Contributing modeling software
| Model | Version | Repository Link 
|-------|---------|-----------------
| Global Change Analysis Model (GCAM) | GCAM_GRAPHICS| [https://github.com/jonsampedro/gcam-core](https://github.com/jonsampedro/gcam-core/tree/GCAM_GRAPHICS_v7_paperHealth) | 

| Component| Version | Repository Link 
|-------|---------|-----------------
| gcamdata | 1.0 | https://github.com/JGCRI/gcamdata | 
| rgcam | 1.2.0 | https://github.com/JGCRI/rgcam | 
| pridr | 0.1.0 | https://github.com/JGCRI/pridr | 
| rmap | 1.0.0 | https://github.com/JGCRI/rmap | 

## Reproduce the experiment
To reproduce the results and figures shown in Sampedro et al.,

1. Install `R` here - https://www.r-project.org/
2. Install `R studio` from here - https://www.rstudio.com/
3. Run the script called `process_results.R` chunk by chunk to generate the figures.  

## Acknowledgments
<img src="./graphics-logo.png" alt="GRAPHICS logo" width="120" height="150" align="left"/>
This project has received funding from the European Union’s Horizon research program under grant agreement 101060679 (GRAPHICS project)
