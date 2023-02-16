<!-- omit from toc -->
# Tabella di marcia

- [Next steps](#next-steps)
- [Divisione stati](#divisione-stati)
- [Video/risorse che parlano del dataset](#videorisorse-che-parlano-del-dataset)


## Next steps

- [x] Espandere sino al 2022
- [x] Adjust for inflation (CPI in 2022 / CPI in 1975 * 1975 USD value = 2022 USD value)
- [x] Considerare anche la varianza nello scaling
- [x] Functional outliers and boxplot
- [x] Robust regression (solo con natural splines)
- [x] Tutta la gam ma con l'inflazione
- [x] Conformal prediction on our price prediction?
- [x] Model selection (via permutation test)
- [x] Conformal prediction on milk price
- [x] Trovare paper simili
- [x] [Import/export retrieve data](https://apps.fas.usda.gov/gats/default.aspx)
- [x] Functional graph con colori formaggio

Presentazione:
- [x] Capire come presentare il dataset
- [x] Decidere Grafico Gam
- [x] Decidere Grafici Functional

Post 23/12
- [x] Cluster Analysis fil inviami il csv ziopera
- [x] Outliers formaggi [@Gabri] [non ce ne sono si gode]
- [x] Outliers anni [@Andre]
- [x] Explanation Gam coefficients [@Teo]
- [x] Distanze Conformal [@Andre sono state provate tutte?]
- [x] Provare casi particolari fittizi o no (emoji teo) [@Fili]
- [x] Spatial
- [x] Stakeholders Analysis [@Tutti]

Ultimo rush

Teo
- [x] carico su overleaf il report
- [x] 05 06 convertire in rmd
- [x] riportare in report coefficienti
- [x] riportare in presentazione coefficienti

Gabri
- [x] cluster analysis
- [x] 1-2 Rmd
- [x] 8-9 Rmd

Fili
- [x] stakeholders analysis

Andre
- [x] descrizione dataset nel report
- [x] verbalizzare parte outlier anni e letta generale se il collante funziona

- [ ] Sistemare il README
- [ ] caricare il report nella repo
- [ ] caricare le slides finali nella repo

## Sources
Npsp package: https://rubenfcasal.github.io/npsp/index.html
NPSP paper: https://link.springer.com/article/10.1007/s00477-017-1407-y
Dati extra su popolazione per county: https://www2.census.gov/programs-surveys/popest/datasets/
Latte venduto ($) per county: https://www.nass.usda.gov/Quick_Stats/index.php
Dati per simulare gam su case studies: https://mymarketnews.ams.usda.gov/viewReport/2957

## Divisione stati
- https://en.wikipedia.org/wiki/List_of_regions_of_the_United_States
- https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_coastline

## Video/risorse che parlano del dataset

https://tkoomar.github.io/post/2019-03-02-tt-milk-gif/
> questo fa una mappa degli stati più produttivi

https://www.christopheryee.org/blog/tidytuesday-milk-production/
> idem

https://www.youtube.com/watch?v=13iG_HkEPVc
Tidy Tuesday screencast: Analyzing US dairy consumption in R
> cose sia esplorative sia di forecasting usando arima/simili, non mi sembra molto utile a quello che serve a noi fatto così...
> però sto tizio è fortissimo, separa anche le categorie (icy,frozen) che potremmo fare anche noi
> tidyverse è la sua puttana

http://rstudio-pubs-static.s3.amazonaws.com/462641_54d47f6b355441308a157267e5940897.html
> plot vendite di latte non particolarmente utili

https://www.npr.org/2019/01/09/683339929/nobody-is-moving-our-cheese-american-surplus-reaches-record-high
> c'è un surplus
- "Americans are drinking less milk."
- "Suppliers turn that extra milk into cheese because it is less perishable and stays fresh for longer periods. But Americans are turning their noses up at those processed cheese slices and string cheese"
- "It's the same as it is for everything else: If you've got too much of something, the price has to go down until consumption rises," Novakovic says.
- "U.S. exports only about 6% of its cheese"


---

# Qualcosa sugli andamenti ciclici

Identifying Efficient Dairy Producers Using Data Envelopment Analysis.pdf
> DEA on dairy products (copiato da Genalti)

--------------------------------------------------------------------------------

https://sci-hub.hkvisa.net/10.1108/BFJ-08-2014-0294

A similar effect could be observed on the US milk price after the product purchase prices were reduced under the Dairy Price Support Program in the mid 1980s.

Like the EU the US government established policy measures for its dairy industry.
These measures include price supports, export subsidies and the federal Milk Income
Loss Contract (MILC) (USDA, 2011). Price supports set floor prices for dairy products
while the MILC program provides income support if milk prices fall below a certain
target price. In general both of these measures should have a counter cyclical effect.
However, cyclical behavior has been observed in US dairy prices (Nicholson and
Stephenson, 2014) over a long period. [...]

> in pratica questo dice che ci sono degli atti e delle leggi che probabilmente causano un andamento ciclico

--------------------------------------------------------------------------------

https://onlinelibrary.wiley.com/doi/epdf/10.1002/agr.21416

Many dairy industry managers acknowledge the existence of a 3-year farm milk price cycle (Hunt & Kern, 2012; Ledman, 2011), although greater integration of the U.S. dairy industry with global markets beginning in 2005 has been suggested as a reason why the cycle will not continue in the future.

> discute dell'esistenza di questi cicli di circa 3 anni, utile per il fit dell'arma?

Our principal objective is to assess whether there are cyclical patterns in U.S. farm milk prices, and if so, to assess their characteristics, such as amplitude,period, and contribution to overall farm-milk price variability.

[...]

models using monthly data resulted in residuals that violated the assumptions of normality, heteroskedasticity and independence,whereas models based on quarterly data had appropriate residual characteristics

[...]

> usano lo stesso modello di quelli sopra per analizzare la stagionalità

Our analysis has identified a cyclical component in the U.S. all-milk price with a period of slightly over 3 year

[...]

The future importance of price cycles is less clear: the high-amplitude cycle that began with a price peak in 2008 may have affected the propensity and ability of U.S. dairy producers to expand in response to profitability incentives, which is a key hypothesized contributor to cyclical behavior. Given the decision-making framework hypothesized to underlie farm milk price cycles, **it seems unlikely that cyclical behavior will cease to be important in future years—even if the cycles may be of lower amplitude in the future**. The presence of cycles that have comprised the largest source of variation in farm milk prices presents both challenges and opportunities for participants in the U.S. dairy supply chain. Farm expansions that are advantageously timed with cycles may become more common in the future. Greater coordination among supply chain partners using approaches like CPFR that are common components of supply chain management but not widely used in the U.S. dairy industry may also help to address cyclical variation. **Forecasting and analytical models of the U.S. dairy industry can be most usefully designed to support decision-making by industry and policymakers if they account for (generate endogenously) cyclical price behavior—and contribute to a further understanding of the sources and responses to the large-amplitude price cycles experienced in recent years.**

> L'ultima parte potrebbe essere molto utile come motivazione per la nostra analisi