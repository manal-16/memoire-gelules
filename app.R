library(shiny)
library(dplyr)
library(tidyr)
library(DT)

# ─────────────────────────────────────────────
# Chargement des données
# ─────────────────────────────────────────────

etudiants           <- read.csv("data/table_etudiants.csv",            encoding = "UTF-8", stringsAsFactors = FALSE)
competences         <- read.csv("data/table_competences.csv",          encoding = "UTF-8", stringsAsFactors = FALSE)
etudiant_competence <- read.csv("data/table_etudiant_competence.csv",  encoding = "UTF-8", stringsAsFactors = FALSE)
stages              <- read.csv("data/table_stages.csv",               encoding = "UTF-8", stringsAsFactors = FALSE)
qualite_stages      <- read.csv("data/table_qualite_stage.csv",        encoding = "UTF-8", stringsAsFactors = FALSE)
stage_competence    <- read.csv("data/table_stage_competence.csv",     encoding = "UTF-8", stringsAsFactors = FALSE)

# ─────────────────────────────────────────────
# Paramètres de l'algorithme
# ─────────────────────────────────────────────

POIDS_LACUNES      <- 0.5
POIDS_COLLABORATIF <- 0.3
POIDS_CONTENU      <- 0.2
SEUIL_LACUNE       <- 0
SEUIL_SIMILARITE   <- 0.5
POIDS_MEME_SEXE    <- 1.0
POIDS_SEXE_DIFF    <- 0.7
NB_STAGES_MAX      <- 3
EXPO_MAX           <- 2

COEFF_NOTE      <- 1.0
COEFF_NPS       <- 1.0
COEFF_PEDA      <- 1.5
COEFF_ADMIN     <- 1.0
COEFF_ENCADRE   <- 1.5

ORDRE_NIVEAUX <- c(
  DFGSM2=0, DFGSM3=1, DFASM1=2, DFASM2=3, DFASM3=4,
  DFGSP2=0, DFGSP3=1, DFASP1=2, DFASP2=3, DFASP3=4,
  L2=0, L3=1, M1=2, M2=3,
  DFGSO2=0, DFGSO3=1, DFASO1=2, DFASO2=3, DFASO3=4
)

# ─────────────────────────────────────────────
# Fonctions du moteur
# ─────────────────────────────────────────────

score_lacunes_fn <- function(id_etudiant, id_stage, scores_etu) {
  lacunes <- scores_etu %>% filter(score <= SEUIL_LACUNE)
  if (nrow(lacunes) == 0) return(0)

  expos <- stage_competence %>%
    filter(id_stage == !!id_stage, id_competence %in% lacunes$id_competence) %>%
    mutate(score_exposition_moyen = ifelse(is.na(score_exposition_moyen), 0, score_exposition_moyen))

  if (nrow(expos) == 0) return(0)

  total <- 0
  for (i in seq_len(nrow(expos))) {
    score_actuel <- lacunes %>% filter(id_competence == expos$id_competence[i]) %>% pull(score)
    if (length(score_actuel) == 0) score_actuel <- 0
    plafond    <- NB_STAGES_MAX * EXPO_MAX
    amplitude  <- max(0, plafond - score_actuel)
    exposition <- max(0, expos$score_exposition_moyen[i])
    total      <- total + amplitude * exposition
  }

  max_possible <- nrow(lacunes) * (NB_STAGES_MAX * EXPO_MAX) * EXPO_MAX
  if (max_possible == 0) return(0)
  round(total / max_possible, 4)
}

similarite_fn <- function(id_a, id_b) {
  a <- etudiants %>% filter(id_etudiant == id_a)
  b <- etudiants %>% filter(id_etudiant == id_b)
  if (a$formation != b$formation) return(0)

  rang_a <- ORDRE_NIVEAUX[a$annee_etude]
  rang_b <- ORDRE_NIVEAUX[b$annee_etude]
  if (is.na(rang_a) || is.na(rang_b)) return(0)
  if (abs(rang_a - rang_b) > 1) return(0)

  scores_a <- etudiant_competence %>% filter(id_etudiant == id_a) %>% select(id_competence, score)
  scores_b <- etudiant_competence %>% filter(id_etudiant == id_b) %>% select(id_competence, score)
  common   <- intersect(scores_a$id_competence, scores_b$id_competence)
  if (length(common) == 0) return(0)

  va <- scores_a %>% filter(id_competence %in% common) %>% arrange(id_competence) %>% pull(score)
  vb <- scores_b %>% filter(id_competence %in% common) %>% arrange(id_competence) %>% pull(score)
  norme <- sqrt(sum(va^2)) * sqrt(sum(vb^2))
  if (norme == 0) return(0)

  sim          <- sum(va * vb) / norme
  facteur_sexe <- ifelse(a$sexe == b$sexe, POIDS_MEME_SEXE, POIDS_SEXE_DIFF)
  sim * facteur_sexe
}

score_collaboratif_fn <- function(id_etudiant, id_stage, similaires) {
  if (length(similaires) == 0) return(0)
  expo <- stage_competence %>%
    filter(id_stage == !!id_stage) %>%
    pull(score_exposition_moyen) %>%
    replace(is.na(.), 0) %>%
    mean()

  num <- sum(sapply(names(similaires), function(id) similaires[[id]] * expo))
  den <- sum(unlist(similaires))
  if (den == 0) return(0)
  raw <- num / den
  round((raw + 2) / 4, 4)
}

score_contenu_fn <- function(id_stage) {
  q <- qualite_stages %>% filter(id_stage == !!id_stage)
  if (nrow(q) == 0) return(0.5)

  note_n  <- q$note_globale_moy / 10
  nps_n   <- q$nps_raw / 10
  peda_n  <- (q$charge_en_lien_moy - q$charge_sans_lien_moy + 4) / 8
  admin_n <- (q$charge_admin_moy - 1) / 4
  enca_n  <- q$encadrement_moy / 4

  raw <- COEFF_NOTE * note_n + COEFF_NPS * nps_n + COEFF_PEDA * peda_n -
         COEFF_ADMIN * admin_n + COEFF_ENCADRE * enca_n

  max_t <- COEFF_NOTE + COEFF_NPS + COEFF_PEDA + COEFF_ENCADRE
  min_t <- -COEFF_ADMIN
  score <- (raw - min_t) / (max_t - min_t)
  round(max(0, min(1, score)), 4)
}

recommander <- function(id_etudiant, top_n = 5) {
  candidats <- stages %>% filter(signalement == 0) %>% pull(id_stage)
  scores_etu <- etudiant_competence %>% filter(id_etudiant == !!id_etudiant)

  autres <- etudiants %>% filter(id_etudiant != !!id_etudiant) %>% pull(id_etudiant)
  similaires <- list()
  for (other in autres) {
    s <- similarite_fn(id_etudiant, other)
    if (s >= SEUIL_SIMILARITE) similaires[[as.character(other)]] <- s
  }

  froid <- nrow(scores_etu) == 0

  resultats <- lapply(candidats, function(id_stage) {
    sl  <- score_lacunes_fn(id_etudiant, id_stage, scores_etu)
    sc  <- score_collaboratif_fn(id_etudiant, id_stage, similaires)
    sct <- score_contenu_fn(id_stage)
    sg  <- if (froid) sc * 0.7 + sct * 0.3
           else sl * POIDS_LACUNES + sc * POIDS_COLLABORATIF + sct * POIDS_CONTENU

    info <- stages %>% filter(id_stage == !!id_stage)
    data.frame(
      id_stage           = id_stage,
      etablissement      = info$etablissement,
      service            = info$service,
      localisation       = info$localisation,
      score_global       = round(sg,  3),
      score_lacunes      = round(sl,  3),
      score_collaboratif = round(sc,  3),
      score_contenu      = round(sct, 3),
      stringsAsFactors   = FALSE
    )
  })

  bind_rows(resultats) %>%
    arrange(desc(score_global)) %>%
    head(top_n)
}

# ─────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Helvetica Neue', Arial, sans-serif; background: #f7f9fc; }
    .titre-app { background: #1F3864; color: white; padding: 18px 24px; margin-bottom: 20px; border-radius: 6px; }
    .titre-app h3 { margin: 0; font-size: 20px; }
    .titre-app p  { margin: 4px 0 0; font-size: 13px; opacity: 0.75; }
    .card { background: white; border-radius: 8px; padding: 16px 20px; margin-bottom: 16px;
            border: 1px solid #e0e6ef; }
    .card-titre { font-weight: 600; font-size: 14px; color: #1F3864; margin-bottom: 12px;
                  border-bottom: 2px solid #e0e6ef; padding-bottom: 6px; }
    .lacune-item { display: flex; align-items: center; gap: 10px; padding: 4px 0;
                   font-size: 13px; border-bottom: 1px solid #f0f0f0; }
    .lacune-barre { height: 8px; border-radius: 4px; background: #d0d7e8; width: 80px; }
    .lacune-fill  { height: 8px; border-radius: 4px; background: #e05c5c; }
    .badge-lacune { background: #fdecea; color: #c0392b; font-size: 11px;
                    padding: 2px 7px; border-radius: 10px; font-weight: 600; }
    .badge-ok     { background: #eaf7ee; color: #27ae60; font-size: 11px;
                    padding: 2px 7px; border-radius: 10px; font-weight: 600; }
    .score-bar-wrap { margin-bottom: 6px; }
    .score-label { font-size: 12px; color: #666; width: 110px; display: inline-block; }
    .score-bar { display: inline-block; height: 10px; border-radius: 5px; vertical-align: middle; }
    .score-pct { font-size: 12px; color: #333; margin-left: 6px; }
    .top-stage { border-left: 4px solid #1F3864 !important; }
    .info-pill { display: inline-block; background: #eef2f9; color: #1F3864; font-size: 12px;
                 padding: 2px 10px; border-radius: 12px; margin-right: 6px; }
  "))),

  div(class = "titre-app",
    tags$h3("Outil de recommandation de stages"),
    tags$p("Algorithme hybride : lacunes + profils similaires + qualité perçue")
  ),

  fluidRow(
    # ── Colonne gauche : sélection + profil ──
    column(4,
      div(class = "card",
        div(class = "card-titre", "Sélection de l'étudiant"),
        selectInput("etudiant", NULL,
          choices = setNames(etudiants$id_etudiant,
            paste0(etudiants$prenom, " ", etudiants$nom,
                   " — ", etudiants$formation, " ", etudiants$annee_etude)),
          width = "100%"
        )
      ),

      div(class = "card",
        div(class = "card-titre", "Profil"),
        uiOutput("profil_ui")
      ),

      div(class = "card",
        div(class = "card-titre", "Lacunes détectées (score ≤ 0)"),
        uiOutput("lacunes_ui")
      )
    ),

    # ── Colonne droite : recommandations + détail ──
    column(8,
      div(class = "card",
        div(class = "card-titre", "Stages recommandés"),
        DTOutput("table_recs")
      ),

      div(class = "card",
        div(class = "card-titre", "Compétences du stage sélectionné"),
        uiOutput("detail_stage_ui")
      )
    )
  )
)

# ─────────────────────────────────────────────
# Server
# ─────────────────────────────────────────────

server <- function(input, output, session) {

  etu_selectionne <- reactive({
    etudiants %>% filter(id_etudiant == input$etudiant)
  })

  scores_etu <- reactive({
    etudiant_competence %>%
      filter(id_etudiant == input$etudiant) %>%
      left_join(competences, by = "id_competence")
  })

  recs <- reactive({
    withProgress(message = "Calcul en cours...", value = 0.5, {
      recommander(input$etudiant, top_n = 5)
    })
  })

  # ── Profil ──
  output$profil_ui <- renderUI({
    e <- etu_selectionne()
    s <- scores_etu()
    n_lacunes <- sum(s$score <= SEUIL_LACUNE, na.rm = TRUE)

    tagList(
      tags$div(style = "font-size:15px; font-weight:600; margin-bottom:8px;",
        paste(e$prenom, e$nom)),
      tags$div(
        span(class = "info-pill", e$formation),
        span(class = "info-pill", e$annee_etude),
        span(class = "info-pill", ifelse(e$sexe == "M", "Homme", "Femme"))
      ),
      tags$div(style = "margin-top:10px; font-size:13px; color:#666;",
        paste0(nrow(s), " compétences évaluées — ",
               n_lacunes, " lacune(s)"))
    )
  })

  # ── Lacunes ──
  output$lacunes_ui <- renderUI({
    s <- scores_etu() %>%
      filter(score <= SEUIL_LACUNE) %>%
      arrange(score)

    if (nrow(s) == 0) {
      return(tags$div(style = "font-size:13px; color:#27ae60;",
        "Aucune lacune détectée"))
    }

    items <- lapply(seq_len(nrow(s)), function(i) {
      row      <- s[i, ]
      pct_fill <- max(0, (row$score + 2) / 4) * 80
      tags$div(class = "lacune-item",
        tags$div(class = "lacune-barre",
          tags$div(class = "lacune-fill", style = paste0("width:", pct_fill, "px;"))),
        tags$span(style = "font-size:12px; color:#888; width:28px;",
          sprintf("%+d", row$score)),
        tags$span(style = "flex:1; font-size:13px;", row$competence)
      )
    })
    tagList(items)
  })

  # ── Tableau des recommandations ──
  output$table_recs <- renderDT({
    df <- recs() %>%
      transmute(
        Établissement = etablissement,
        Service       = service,
        Global        = paste0(round(score_global * 100, 1), "%"),
        Lacunes       = paste0(round(score_lacunes * 100, 1), "%"),
        Collaboratif  = paste0(round(score_collaboratif * 100, 1), "%"),
        Contenu       = paste0(round(score_contenu * 100, 1), "%")
      )
    datatable(df,
      selection  = "single",
      rownames   = FALSE,
      options    = list(dom = "t", pageLength = 5, ordering = FALSE),
      class      = "stripe hover compact"
    ) %>%
      formatStyle("Global", fontWeight = "bold", color = "#1F3864") %>%
      formatStyle(0, target = "row",
        backgroundColor = styleRow(1, "#eef3fc"))
  })

  stage_selectionne <- reactive({
    sel <- input$table_recs_rows_selected
    if (is.null(sel)) return(recs()$id_stage[1])
    recs()$id_stage[sel]
  })

  # ── Détail du stage ──
  output$detail_stage_ui <- renderUI({
    id_stage_sel <- stage_selectionne()
    info <- stages %>% filter(id_stage == id_stage_sel)
    s    <- scores_etu()

    expos <- stage_competence %>%
      filter(id_stage == id_stage_sel) %>%
      left_join(competences, by = "id_competence") %>%
      left_join(s %>% select(id_competence, score_etu = score), by = "id_competence") %>%
      arrange(desc(score_exposition_moyen))

    header <- tags$div(style = "margin-bottom:12px;",
      tags$span(style = "font-weight:600; font-size:15px;",
        paste0(info$etablissement, " — ", info$service)),
      tags$br(),
      tags$span(class = "info-pill", style = "margin-top:6px; display:inline-block;",
        info$localisation),
      tags$span(class = "info-pill", paste0("Encadrant : ", info$encadrant))
    )

    rows <- lapply(seq_len(nrow(expos)), function(i) {
      row     <- expos[i, ]
      expo    <- round(row$score_exposition_moyen, 2)
      est_lac <- !is.na(row$score_etu) && row$score_etu <= SEUIL_LACUNE
      bar_w   <- max(0, (expo + 2) / 4) * 120
      bar_col <- if (expo >= 1) "#27ae60" else if (expo >= 0) "#f39c12" else "#e05c5c"

      tags$div(class = "lacune-item",
        tags$div(style = paste0("width:120px; height:8px; border-radius:4px;
                                 background:#e8eaf0; display:inline-block;"),
          tags$div(style = paste0("width:", bar_w, "px; height:8px; border-radius:4px;
                                   background:", bar_col, ";"))
        ),
        tags$span(style = "font-size:12px; color:#666; width:36px;",
          sprintf("%+.2f", expo)),
        tags$span(style = "flex:1; font-size:13px;", row$competence),
        if (est_lac)
          tags$span(class = "badge-lacune",
            paste0("lacune ", sprintf("%+d", row$score_etu)))
        else
          tags$span(class = "badge-ok", "ok")
      )
    })

    tagList(header, tagList(rows))
  })
}

shinyApp(ui, server)
