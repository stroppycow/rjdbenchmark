## Preprocessing R
- [x] Renommage du fichier de métadonnées en fichier de mapping
- [x] Création d'un fichier de métadonnées en yaml 
    - format des séries brutes en input
    - policy pour le cruncher
    - langue d'affichage
- [x] Ajout de contrôles du fichier de mapping
  - Unicité (serie_group, level, code)
  - sa_item_new, ws_new, sa_item_old, sa_item_new ne peuvent être simultanément vides
  - sa_item_old et ws_old doivent être simultanément renseignés ou vides
  - sa_item_new et ws_new doivent être simultanément renseignés ou vides
- [x] Ajout de contrôles pour la lecture du fichier de métadonnées
- [x] Ajout de contrôles pour la lecture des fichiers de label des groupes de série
- [x] Ajout de contrôles pour la lecture du fichier de label des niveaux
- [x] Ajout de contrôles pour la lecture des labels pour les regresseurs de calendrier
- [x] Contrôles de cohérence des fichiers de config après chargement
  - [x] ws old : Mapping et metadata
  - [x] ws new : Mapping et metadata
  - [x] data_file : Mapping et metadata
  - [x] level : Mapping et levels
  - [x] serie_group : Mapping et serie_group
- [x] Ajout du logging
- [x] Récupération de la qualité globale dans les outputs
- [x] Récupération de la version du cruncher JDemetra+ utilisée
- [x] Récupération de la refresh policy
- [x] Lecture des séries brutes en fonction du fichier de métadonnées
- [x] Lancement du cruncher en fonction de la refresh policy
- [x] Remplacement du dossier `./data/tmp` par `./tmp`
- [x] Export des labels et des outputs en parquet dans le dossier output
- [x] Génération du site quarto
- [x] Amélioration des commentaires dans le code et traduction en anglais
  - [x] `config-mapping-reader.R`
  - [x] `config-metadata-reader.R`
  - [x] `config-levels-reader.R`
  - [x] `config-trading-days-reader.R`
  - [x] `config-serie-group-reader.R`
  - [x] `jdemetra-crunch.R`
  - [x] `jdemetra-demetram-reader.R`
  - [x] `jdemetra-init-workspace.R`
  - [x] `jdemetra-list-tradingdays.R`
  - [x] `jdemetra-ts-reader.R`
  - [x] `jdemetra-utils.R`
  - [x] `main.R`
  - [x] `timeseriesset-reader.R`
  - [x] `TimeserriesSet.R`
  - [x] `utils-date.R`
  - [x] `utils-s4.R`
- [x] Transformation du code en package R
- [ ] Prise en compte du dossier d'output des données dans les fichiers quarto
- [x] Réinitialisation de `renv`

## Site statique (template quarto / html / javascript)
- [ ] Mettre l'essentiel du code javascript dans des fichiers séparés pour simplifier la lecture et la maintenance du code
- [ ] Accélérer la visualisation des panels pour les séries temporelles
- [ ] Affichage de la version de JDemetra utilisée et de la qualité globale
- [ ] Gestion des langues multiples
- [ ] Amélioration des commentaires

## Docker
- [ ] Construction du Dockerfile et gestion des dépendances (java, jdemetra, quarto)

## Github
- [ ] Ajout action pour le build doker
- [ ] Ajout Readme