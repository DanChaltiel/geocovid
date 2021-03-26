# Geocovid

![stability-wip](https://img.shields.io/badge/stability-work_in_progress-lightgrey.svg)

Petite extraction de données pour visualiser les "vagues" de covid.

## Données

Les données opencovid : https://www.data.gouv.fr/fr/datasets/chiffres-cles-concernant-lepidemie-de-covid19-en-france/

Les données géographiques-spatiales des départements : https://www.ign.fr/reperes/centre-geographique-des-departements-metropolitains

Les données de population par département (pour rapporter les chiffres à 100k) : https://www.insee.fr/fr/statistiques/1893198


## GIFs générés

Le code génère ces GIFs:
 
### Moyenne mobile sur 7 jours du nombre d'hospitalisations pour 100 000 habitants :

![anim hospi](output/covid_roll7_hospitalises_100k_x1.gif)

### Moyenne mobile sur 7 jours du nombre de réanimations pour 100 000 habitants :

![anim rea](output/covid_roll7_reanimation_100k_x1.gif)


### Moins intéressants car cumulatifs :

#### Moyenne mobile sur 7 jours du **total des décès** pour 100 000 habitants, **depuis le début de l'épidémie** :

![anim deces](output/covid_roll7_deces_100k_x1.gif)

### Moyenne mobile sur 7 jours du **total des décès** pour 100 000 habitants, **depuis le début de l'épidémie** :

![anim gueris](output/covid_roll7_gueris_100k_x1.gif)




