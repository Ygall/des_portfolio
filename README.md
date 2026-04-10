# e-Portfolio DES Santé Publique

Application Shiny pour la gestion du portfolio de l'internat du DES de Santé Publique, conforme à l'arrêté du 12 avril 2017 et au référentiel pédagogique 2016 (CUESP/CIMES/CLISP).

---

## Fonctionnalités

### Rôle Interne
| Module | Contenu |
|---|---|
| **Tableau de bord** | Barplots empilés par domaine (connaissances + compétences), colorés par statut (Non évalué / Non acquis / En cours / Acquis) ; filtre Base/Avancé ; bouton Actualiser |
| **Mon identité** | État civil, facultés (liste déroulante des 33 facultés françaises), année ECN/EDN, DES initial, diplômes dynamiques (DU/DIU / M1 / M2 / Autres — intitulé, université, année) |
| **Contrat de formation** | Projet professionnel, thèse (statut, sujet, directeur, date de soutenance), objectifs pédagogiques connaissances et compétences |
| **Connaissances** | 98 items sur 9 domaines — auto-évaluation (Non évalué / Non acquis / Acquis) + validation Senior (nom/fonction + date auto) ; filtres domaine/niveau/statut ; 50 items par page |
| **Compétences** | 87 items sur 9 domaines — auto-évaluation (+ En cours d'acquisition) + évaluation Senior ; mêmes filtres |
| **Carnet de stages** | 8 semestres (extensible) — période (Mai/Novembre + année), lieu, responsable de stage, travaux réalisés, valorisations/communications, commentaire |
| **Validation des phases** | 3 phases : Socle / Approfondissement / Consolidation — avis, date, signataire, commentaire |
| **Exporter** | PDF individuel (`portfolio_NOM_PRENOM_AAAAMMJJ.pdf`), CSV individuel |

### Rôle Coordinateur
Accès à tout le menu Interne ci-dessus **pour chaque interne suivi** via le sélecteur dans la barre latérale (lecture + modification/validation). En plus :

| Module | Contenu |
|---|---|
| **Suivi de la promotion** | Tableau synthétique (% connaissances, % compétences, stages, phases validées) avec option d'inclure les internes ayant terminé ; distributions par statut (barplots) |
| **Gestion internes** | Créer des comptes interne ou coordinateur ; basculer statut Actif ↔ Internat terminé |
| **Exporter** | PDF et CSV individuels de l'interne sélectionné ; CSV promotion ; Excel multi-onglets (synthèse + connaissances + compétences) |

### Rôle Admin
Tout ce que le coordinateur peut faire, plus :

| Module | Contenu |
|---|---|
| **Administration** | Créer des comptes (interne / coordinateur / admin) ; activer/désactiver un compte (surlignage rouge si désactivé) ; réinitialiser le mot de passe (avec rappel de l'email) ; basculer statut Actif ↔ Internat terminé ; statistiques des tables DB ; VACUUM SQLite ; téléchargement d'un backup |

---

## Structure du projet

```
des_portfolio/
├── app.R                       # Point d'entrée
├── ui.R                        # Interface shinydashboard
├── server.R                    # Logique serveur, routing par rôle
├── global.R                    # Packages, backend DB (SQLite/PG), helpers
├── deploy.R                    # Script de déploiement shinyapps.io
├── install_packages.R          # Installation des dépendances
├── .Renviron.example           # Template variables d'environnement
├── des_portfolio.Rproj         # Projet RStudio
├── data/
│   ├── referentiel.R           # 98 connaissances + 87 compétences
│   ├── init_db.R               # Schéma SQLite + amorçage + init PostgreSQL
│   └── portfolio_des.sqlite    # Base SQLite (créée au 1er lancement local)
├── modules/
│   ├── mod_auth.R              # Authentification locale (bcrypt)
│   ├── mod_identite.R          # Fiche d'identité + diplômes
│   ├── mod_contrat.R           # Contrat de formation + thèse
│   ├── mod_connaissances.R     # Portfolio connaissances
│   ├── mod_competences.R       # Portfolio compétences
│   ├── mod_stages.R            # Carnet de stages
│   ├── mod_phases.R            # Validation des 3 phases
│   ├── mod_dashboard.R         # Tableaux de bord interne + promotion
│   ├── mod_export.R            # Exports PDF / CSV / Excel
│   └── mod_admin.R             # Administration (admin + coord restreint)
├── templates/
│   └── portfolio_pdf.Rmd       # Template PDF (pdflatex)
└── www/
    └── style.css               # CSS personnalisé
```

---

## Installation locale (développement)

### Prérequis système

```bash
# Ubuntu/Debian
sudo apt-get install r-base libssl-dev libcurl4-openssl-dev libxml2-dev
```

R ≥ 4.2 requis.

### Packages R

```r
Rscript install_packages.R
```

### LaTeX (export PDF)

L'export PDF utilise **pdflatex** via `tinytex`. Installation automatique au premier lancement si absent, ou manuellement :

```r
tinytex::install_tinytex()
```

### Lancement

```r
setwd("des_portfolio")
shiny::runApp(".", port = 3838, launch.browser = TRUE)
```

Les comptes de démonstration sont créés automatiquement au premier lancement :

| Identifiant | Mot de passe | Rôle |
|---|---|---|
| `admin` | `admin123` | Administrateur |
| `coord1` | `coord123` | Coordinateur |
| `interne1` | `interne123` | Interne |

> **⚠️ Changer ces mots de passe immédiatement en production** via l'onglet Administration.

---

## Déploiement sur shinyapps.io

### 1. Base de données

shinyapps.io a un **système de fichiers éphémère** : une base SQLite locale sera réinitialisée à chaque redémarrage de l'instance. Pour une utilisation en production, il faut connecter un **PostgreSQL externe**.

**Fournisseurs PostgreSQL gratuits compatibles :**

| Fournisseur | Gratuit | URL |
|---|---|---|
| Supabase | 500 Mo, 2 projets | https://supabase.com |
| Neon | 0,5 GB, serverless | https://neon.tech |
| Render | Tier gratuit | https://render.com |

Le backend est détecté automatiquement : si la variable `PG_HOST` est définie → PostgreSQL ; sinon → SQLite local.

### 2. Variables d'environnement

Copier `.Renviron.example` en `.Renviron` et remplir :

```
# PostgreSQL (production)
PG_HOST=db.xxx.supabase.co
PG_PORT=5432
PG_DB=des_portfolio
PG_USER=postgres
PG_PASSWORD=motdepasse

# shinyapps.io (voir Account > Tokens)
SHINYAPPS_NAME=votre-compte
SHINYAPPS_TOKEN=xxxxxxxx
SHINYAPPS_SECRET=xxxxxxxx
```

### 3. Déploiement

```r
# Option A : script automatisé
source("deploy.R")

# Option B : manuel
rsconnect::setAccountInfo(name="...", token="...", secret="...")
rsconnect::deployApp(appName = "portfolio-des-sp", forceUpdate = TRUE)
```

Les variables PostgreSQL se configurent ensuite sur shinyapps.io dans **Application > Settings > Environment Variables**, ou via `rsconnect::configureApp()` (voir `deploy.R`).

### 4. Shiny Server Open Source (auto-hébergé)

```bash
# /etc/shiny-server/shiny-server.conf
server {
  listen 3838;
  location /portfolio {
    app_dir /srv/shiny-server/des_portfolio;
    log_dir /var/log/shiny-server;
  }
}
```

Sur Shiny Server, SQLite fonctionne nativement (données persistantes sur disque).

---

## Variables d'environnement

| Variable | Défaut | Description |
|---|---|---|
| `PG_HOST` | _(vide)_ | Hôte PostgreSQL — si défini, active le backend PG |
| `PG_PORT` | `5432` | Port PostgreSQL |
| `PG_DB` | `des_portfolio` | Nom de la base |
| `PG_USER` | `postgres` | Utilisateur PostgreSQL |
| `PG_PASSWORD` | _(vide)_ | Mot de passe PostgreSQL |
| `DES_DB_PATH` | `data/portfolio_des.sqlite` | Chemin SQLite (ignoré si PG_HOST défini) |

---

## Schéma de la base de données (10 tables)

| Table | Description |
|---|---|
| `users` | Comptes — rôles : `interne` / `coordinateur` / `admin` ; statuts : `actif` / `internat_termine` |
| `ref_connaissances` | 98 items de connaissances, lecture seule |
| `ref_competences` | 87 items de compétences, lecture seule |
| `eval_connaissances` | Auto-évaluation + validation Senior par utilisateur × item |
| `eval_competences` | Auto-évaluation + évaluation Senior par utilisateur × item |
| `identite` | Fiche d'identité de l'interne |
| `diplomes` | Diplômes (0-n par interne, liste dynamique) |
| `contrat_formation` | Projet professionnel, thèse, objectifs pédagogiques |
| `stages` | Carnet de stages (8+ semestres) |
| `phases_validation` | Validation des 3 phases DES |

Les schémas SQLite et PostgreSQL sont maintenus en parallèle dans `data/init_db.R`. Les migrations SQLite sont idempotentes (`ALTER TABLE IF NOT EXISTS` simulé).

---

## Référentiel pédagogique

Fondé sur le **Référentiel des objectifs pédagogiques du DES de Santé Publique — 2016**, validé par le Conseil National de Santé Publique (CUESP, CIMES, CLISP) le 29 septembre 2016.

9 domaines (8 spécifiques + 1 transversal) :

1. Biostatistiques
2. Épidémiologie et méthodes en recherche clinique
3. Informatique biomédicale et e-santé
4. Gestion de la qualité, des risques et de la sécurité des soins
5. Économie, administration des services de santé, politiques de santé
6. Sciences humaines et sociales
7. Environnement et santé
8. Promotion de la santé
9. Connaissances et compétences transversales

Chaque item est classé **Base** (phase socle/approfondissement) ou **Avancé** (selon contrat de formation).

---

## Authentification

Authentification **locale uniquement** (bcrypt). Les mots de passe sont hashés en base, jamais stockés en clair. La gestion des comptes est assurée par l'interface d'administration.

---

## Licence

Usage pédagogique interne — non redistribuable sans accord du coordonnateur national du DES SP.
