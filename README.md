# Projet : Frise Chronologique par Acteurs

## Description

Cette application Shiny permet de visualiser une frise chronologique interactive, organisée par acteurs, à partir d'un fichier Excel contenant des périodes exprimées en texte.

Elle est pensée pour faciliter l’analyse et la visualisation de périodes complexes décrites de manière textuelle (ex : « juin-juillet 1990 », « du 12 au 29 mai 2010 », « années 1990 ») et leur association à plusieurs acteurs clés.

## Fonctionnalités

- Importation d’un fichier Excel (.xlsx) structuré ainsi :
  - Première colonne : périodes au format texte (ex : « 1934-1945 », « 17 juillet 1934 », « janvier - mars 1934 », etc.)
  - Colonnes suivantes : acteurs avec des contenus descriptifs.
- Parsing automatique des périodes en dates de début et fin.
- Visualisation interactive avec la librairie `timevis`.
- Filtrage dynamique des acteurs à afficher.
- Affichage détaillé du contenu associé à chaque période en cliquant sur l’élément de la frise.
- Exportation de la frise en image PNG.
- Documentation intégrée accessible dans l’application.

---

## Conditions d’accès

- R version ≥ 4.0 recommandé.
- Packages R nécessaires (gérés via `renv`).
- Utilisation locale via RStudio ou serveur Shiny.

## Installation

1. Cloner ce dépôt :

```bash
git clone https://github.com/sim-jean/frise-chronologique-acteurs.git
cd frise-chronologique-acteurs
```

2. Installer `renv` si nécessaire :

```r
install.packages("renv")
```

3. Lancer le script `setup.R`: 

- Cette commande installera automatiquement les dépendances exactes du projet via la restauration d'un `renv`
- Et créera automatiquement l'architecture nécessaire (i.e. `/scripts/`, `/data/`, `outputs/`)

4. Un peu de rangement : 

- Rangez les scripts `setup.R`, `app.R`, `launch.R` dans le dossier `/scripts/`
- Rangez vos données en format `.xlsx` dans le dossier `/data/`


## Lancement de l’application

Dans le répertoire du projet, lancer l’application Shiny avec le script `launch.R` : 

- Soit ouvrez le fichier `launch.R` et cliquez sur `Run App` (ou runnez ligne par ligne)
- Soit runnez `library(shiny) shiny::runApp('scripts/')`

## Structure du projet

- `/scripts/`: dossier de scripts
  - `app.R` : code source de l’application Shiny
  - `setup.R`: code d'installation des dépendances
  - `launch.R`: code pour faciliter l'utilisation 
- `/data/`: dossier de données
- `/outputs/`: dossier de sauvegarde PNG
- `README.Rmd` : ce fichier de documentation.
- `renv` : dossier de gestion des dépendances R.
- `renv.lock`: fichier de gestion des dépendances


---

## Licence

Cette application est publiée sous licence [Creative Commons BY-NC 4.0](https://creativecommons.org/licenses/by-nc/4.0/).

Vous êtes libre de la réutiliser, modifier et partager sous ces conditions :

- Mentionner l’auteur original.
- Usage non commercial uniquement.
- Partage des adaptations sous les mêmes conditions.

---

## Contact & Collaboration

Pour toute question, suggestion ou collaboration, contactez-moi via mon [GitHub](https://github.com/sim-jean).

---

## État actuel et perspectives

- Parsing des périodes robuste mais perfectible.
- Ajout de formats supplémentaires.
- Amélioration du rendu visuel (couleurs, légendes).
- Fonctionnalités avancées d’export.

---

Merci d’utiliser cette application !

---