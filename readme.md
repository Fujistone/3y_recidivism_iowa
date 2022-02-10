
---
Recidivisme sur 3 ans dans les prisons de l'Iowa, aux USA
---
Uzma UNIA 

```r
{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
**Analyse de survie sur des données de récidivisme dans l'Etat de l'IOWA, aux USA**
---
A l'origine le jeu de données comprend 5 années fiscales; on choisira ici de faire l'analyse en prenant en compte uniquement les données enregistrées pour l'année 2010.
Les individus sont suivis sur une période de 3 ans depuis l'année de libération (ici sur les années 2010, 2012 et 2013).
On cherche à identifier quelles sont les facteurs de récidives durant cette période.

#Préparer les données
```r
{r,echo=F}
#Charger les libraires
library(readr)
library(dplyr)
library(survival)
library(survminer)
#Charger les données
recidive <- read_csv("C:/Users/Fujistone/Documents/LOcole/Cours/Statistiques 2/Recidivism_stats_project/Recidive.csv")
View(recidive)
```
On voit que le jeu de données comprend 3716 observations et 17 variables, décrivant différentes caractéristiques des individus mis en liberté en 2010.

```r
{r,echo=F}
#Retirer les colonnes non pertinentes
recidive = recidive[,-c(1,2,3)]
View(recidive)
```
On retire les colonnes "New Offense Classification", "New Offense Type" et "New Offense Sub Type"; elles ne seront pas pris en compte dans notre analyse.

*On identifie les deux variables nécessaires à l'analyse de survie : 
- la variable évènementiellle "Return to Prison" 
- la variable temporelle "Days to Return"*
```r
{r,echo=F}
#Numériser la colonne "Return to Prison"
recidive$`Return to Prison`[recidive$`Return to Prison` == "Yes"] = 1 #Oui récidive
recidive$`Return to Prison`[recidive$`Return to Prison` == "No"] = 0 #non pas récidive
recidive$`Return to Prison` = as.numeric(recidive$`Return to Prison`)
```
Nous avons recodé la variable "Return to Prison" qui était de type "character" en "numeric" afin de la rendre exploitabl pour l'analyse.
```r
{r,echo=F}
#Transformer les entrées de la colonne "Days to Return"
recidive = mutate_if(recidive, is.numeric, ~replace(., is.na(.), 365*3))
recidive$`Days to Return` = as.numeric(recidive$`Days to Return`)
```
De même, nous avons recodé la variable "Days to Return" de manière à pouvoir l'utiliser dans l'analyse.On fixe à 1095 jours soit 365*3 jours maximum les évéènements"temps" pour les individus n'ayant pas récidivé au cours de la période étudiée.

```r
{r,echo=F}
#Renommer les colonnes
colnames(recidive)[colnames(recidive) %in% c("Return to Prison", "Days to Return")] <- c("return_prison", "return_time")
colnames(recidive)[colnames(recidive) %in% c("Release Type", "Race - Ethnicity")] <- c("release_type", "race")
colnames(recidive)[colnames(recidive) %in% c("Age At Release", "Sex")] <- c("age", "sex")
colnames(recidive)[colnames(recidive) %in% c("Offense Classification", "Offense Type")] <- c("offense_class", "offense_type")
colnames(recidive)[colnames(recidive) %in% c("Offense Subtype", "Recidivism Type")] <- c("offense_subtype", "recidivism")
colnames(recidive)[colnames(recidive) %in% c("New Offense Classification", "New Offense Type")] <- c("recidiv_class", "recidiv_type")
colnames(recidive)[colnames(recidive) %in% c("New Offense Sub Type")] <- c("recidiv_subtype")
colnames(recidive)[colnames(recidive) %in% c("Target Population")] <- c("target_population")
View(recidive)
```


```r
{r,echo=F}
#Factoriser les variables pour faciliter l'analyse
library(dplyr)
library(labelled)
recidive<-recidive%>%
  mutate(
    release_type = to_factor(release_type),
    race = to_factor(race),
    age=to_factor(age),
    sex = to_factor(sex),
    offense_type = to_factor(offense_type),
    offense_subtype = to_factor(offense_subtype),
    recidivism = to_factor(recidivism),
    offense_classs=to_factor(offense_class),
    recidiv_class = to_factor(recidiv_class),
    recidiv_type = to_factor(recidiv_type),
    recidiv_subtype = to_factor(recidiv_subtype),
    target_population = to_factor(target_population)
  )
```

```r
{r}
#Censure
library(asaur)
table(recidive$return_prison)/nrow(recidive)
```
On remarque une censure importante à 69%, c'est-à-dire d'invidus que l'on a perdus de vue lors du suivi, et 30% d'invidus non censurés ayant récidivé.


#Estimation non paramétrique de Kapaln-Meier 
```r
{r,echo=F}
library(survival)
KM0 <- survfit(Surv(recidive$return_time, recidive$return_prison)~1,data = recidive)
print(KM0)
summary(KM0, times= seq(0,500,50))
```
On peut remarquer que la médiane vaut NA car le taux de d'invidus censurés > non censurés, aussi la survie n'est pas atteinte de manière globale. En effet, la survie reste décroissante au cours du temps. Moins de 50% des personnes libérées en 2010 ont donc récidivé surt la période analysée.

```r
{r,echo=F}
#courbe globale de Kapalan-Meier
library(survminer)
ggsurvplot(KM0, 
           data = recidive,
           linetype = 1,
           conf.int = TRUE,
           risk.table = TRUE)
```
La courbe est décroissante mais quasi horizontale, on observe pas pas de chute brutale. Le point de chute, qui est léger, semble appaître aux environs des 250 jours. 
#Estimation de Kaplan-Meier selon certaines variables

On choisit d'étudier la survie en fonction de 3 variables : 
- le sexe (sex)
- l'âge à la sortie (age)
- le ciblâge de l'individu par le système carcéral (target population)

```r
{r,echo=F}
#En fonction du sexe
KM_sex <- survfit(Surv(recidive$return_time, recidive$return_prison)~sex,data = recidive)
summary(KM_sex, times= seq(0,500,50))
library(survminer)
ggsurvplot(KM_sex,
           risk.table = TRUE)
```
L'écart est faible, mais le taux de survie est plus élevé chez les femmes que chez les hommes. Il y a donc moins de femmes qui récidivent que d'hommes. On peut cependant supposer que ce résultat s'explique également par le fait qu'il y ait plus d'hommes que de femmes qui ont été libérés (donc plus d'hommes incarcérés au départ), aussi ils sont statistiquement plus à risque de récidiver.

```r
{r,echo=F}
#En fonction de l'âge
library(survminer)
KM_age_release <- survfit(Surv(recidive$return_time, recidive$return_prison)~age,data = recidive)
summary(KM_age_release, times= seq(0,500,50))
ggsurvplot(KM_age_release,
           risk.table = TRUE)
```
On observe les plus de 55 ans ont le taux de survie le plus élévé; ils récidivent donc le moins. A l'inverse, les moins de 35 ans (moins de 25 ans et 25-34) ont les taux de survie les plus faibles, ils sont récidives donc plus. 

```r
{r,echo=F}
#En fonction du ciblage
library(survminer)
KM_age_release <- survfit(Surv(recidive$return_time,recidive$return_prison)~target_population,data = recidive)
summary(KM_age_release, times= seq(0,500,50))
ggsurvplot(KM_age_release,
           risk.table = TRUE)
```
Les individus déjà ciblés par le système carcéral récidivent plus que ceux non ciblés.

```r
{r,echo=F}
#En fonction du sexe peut importe l'âge
KM_sex_age <- survfit(Surv(recidive$return_time, recidive$return_prison)~sex,data = recidive)
summary(KM_sex_age, times= seq(0,500,50))
library(survminer)
ggsurvplot(KM_sex,
           facet.by = 'age',
           risk.table = TRUE)
```
Les résultats ne diffèrennt ici pas de l'estimation non paramétrique globale en fonction de la variable âge, sauf pour deux catégories : 
- les 45-54 ans, où les courbes se confondent presque, et le taux de survie pour les hommes est infimement supérieur à celui des femmes à la fin de l'étude
- les 55 ans et plus, où l'on voit une courbe de survie pour les hommes supérieur à celui des femmes. Cependant, on observe aussi une censure plus présente pour la courbe de survie des femmes (décroissance par palliers beaucoup plus marquée) 

```r
{r,echo=F}
#En fonction du ciblage peut importe l'âge
library(survminer)
KM_age_release <- survfit(Surv(recidive$return_time, recidive$return_prison)~target_population,data = recidive)
summary(KM_age_release, times= seq(0,500,50))
ggsurvplot(KM_age_release,
           facet.by = 'age',
           risk.table = TRUE)
```
On ne remarque pas ici de grosses différences avec la courbe globe de survie pour la variable target_population : les individus ciblés au préalable par le système carcéral sont plus susceptibles de récidiver, quelle que soit la tranche d'âge. On peut en revanche apporter quelques précisons : 
- pour la catégorie des 45-54 ans, les courbes sont très proches et se rejoinent même à la fin du temps étudié
- pour les moins de 25 ans, on remrque une divergeance des courbes dès les 100 premiers jours. Les individus de cette catégorie sont donc les plus à risque de récidiver.

#vérification des courbes avec le test du log rank 

 ** On pose H0 = S_female(t) = S_male(t) **
 
```r
{r,echo=F}
library(survival)
library(prodlim)
#H0 = pas de diff significative entre les 2 groupes #H1 = diff entre les 2 groupes
survdiff(Surv(return_time,return_prison)~sex,data=recidive)
#inférieure à 5% -> on rejette hypothèse nulle
survdiff(Surv(return_time,return_prison)~age,data=recidive) 
#inférieure à 5% -> hypothèse nulle rejetée
```


#Régression de Cox

On construit un modèle multivarié en fonction des variables age et sex. 
```r
{r, echo=FALSE}
library(survival)
library(Publish)
cox1<-coxph(Surv(return_time,return_prison)~ age + sex, data = recidive)
cox1 %>% publish()
```
Les résultats nous montrent que l'âge jeune est facteur de récidive, avec un taux de risque relatif (Hazard Ratio) à 1.03 par rapport à toutes autre catégories d'âge dont le Hazard Ratio est inférieur à 1. 
Similairement, le sexe masculin est facteur de récidive; on observe un Hazard Ratio à 1.30. 
```r
{r,echo=F}
library(broom)
library(gtsummary)
library(GGally)
library(car)
ggcoef_model(cox1, exponentiate = F)
```
La figure du modèle confirme les résultats ci-dessus.

#Vérification avec le critère d'Akaike

Le critère d'Akaike (Aikaike Information Criterion ou AIC) est outil que l'on utilise ici pour augmenter la vraisemblance de notre modèle
```r
{r,echo=F}
#vérification du modèle
#crtière d'AIC
library(survival)
library(publish)
cox1_recidive <- step(cox1)
```
On a bien les variables explicatives sex et age 

#Validité
```r
{r,echo=F}
#validité modèle
library(survival)
verif <- cox.zph(cox1_recidive)
```

#Tracé des résidus de Schoenfeld
```r
{r,echo=F}
#plot
library(survminer)
ggcoxzph(verif)
```
Le modèle est valide si les Hazard Ratios que l'on a obtenus sont proportionnels aux risques. 
Pour la variable âge : la valeur de p est inférieure à 0.05, aussi son indépendance n'est pas vérifiée.
Pour la variable sexe : la valeur de p est supérieure à 0.05, son indépence est donc vérifié.

** Conclusion : Suite à note étude, on peut dire que le sexe masculin, le jeune âge (moins de 34 ans) mais aussi le fait d'être déjà ciblé par le système carcéral peuvent être des facteurs de récidives. **
© 2022 GitHub, Inc.
Terms
Privacy
Security
Status
Docs
Contact GitHub
Pricing
API
Training
Blog
About
