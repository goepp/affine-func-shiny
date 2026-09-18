library(shiny)
library(ggplot2)

PALETTE <- scales::hue_pal()(3)
LINE_COLOR <- PALETTE[1]
POINT_A_COLOR <- PALETTE[2]
POINT_B_COLOR <- PALETTE[3]

# Formats a number for on-plot labels, trimming trailing zeros.
fmt_num <- function(v) sprintf("%g", round(v, 2))

# Formats the "mx" term, dropping the coefficient when m = 1 or m = -1.
fmt_mx <- function(m, spaced = TRUE) {
  if (m == 1) return("x")
  if (m == -1) return("-x")
  if (spaced) sprintf("%g x", m) else sprintf("%gx", m)
}

# Formats "mx + p", dropping the "mx" term when m = 0 and the "+ p" term when p = 0.
fmt_affine <- function(m, p, spaced = TRUE) {
  if (m == 0) return(sprintf("%g", p))
  mx <- fmt_mx(m, spaced)
  if (p == 0) return(mx)
  if (spaced) sprintf("%s %s %g", mx, ifelse(p >= 0, "+", "-"), abs(p))
  else sprintf("%s%s%g", mx, ifelse(p >= 0, "+", "-"), abs(p))
}

ui <- fluidPage(
  withMathJax(),
  titlePanel("Représentation graphique des fonctions affines"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput("m", "Valeur du coefficient directeur \\(m\\) :",
                  min = -5, max = 5, value = 1, step = 0.1),
      numericInput("m_num", "", value = 1, step = 0.1),
      br(),
      sliderInput("p", "Valeur de l'ordonnée à l'origine \\(p\\) :",
                  min = -5, max = 5, value = 0, step = 0.1),
      numericInput("p_num", "", value = 0, step = 0.1),
      br(),
      uiOutput("equation"),
      br(),
      checkboxInput("showA", "Afficher le point A", value = FALSE),
      conditionalPanel(
        condition = "input.showA",
        numericInput("xA", "Valeur de \\(x_A\\) :", value = 0, step = 0.1)
      ),
      checkboxInput("showB", "Afficher le point B", value = FALSE),
      conditionalPanel(
        condition = "input.showB",
        numericInput("xB", "Valeur de \\(x_B\\) :", value = 1, step = 0.1)
      ),
      uiOutput("variation")
    ),
    mainPanel(
      width = 9,
      plotOutput("plot", height = "90vh")
    )
  )
)

server <- function(input, output, session) {

  # Keep slider and numeric input in sync for m
  observeEvent(input$m, {
    if (!is.na(input$m) && input$m != input$m_num) {
      updateNumericInput(session, "m_num", value = input$m)
    }
  })
  observeEvent(input$m_num, {
    if (!is.na(input$m_num) && input$m_num != input$m &&
        input$m_num >= -5 && input$m_num <= 5) {
      updateSliderInput(session, "m", value = input$m_num)
    }
  })

  # Keep slider and numeric input in sync for p
  observeEvent(input$p, {
    if (!is.na(input$p) && input$p != input$p_num) {
      updateNumericInput(session, "p_num", value = input$p)
    }
  })
  observeEvent(input$p_num, {
    if (!is.na(input$p_num) && input$p_num != input$p &&
        input$p_num >= -5 && input$p_num <= 5) {
      updateSliderInput(session, "p", value = input$p_num)
    }
  })

  m_val <- reactive({
    val <- input$m_num
    if (is.null(val) || is.na(val)) val <- input$m
    val
  })

  p_val <- reactive({
    val <- input$p_num
    if (is.null(val) || is.na(val)) val <- input$p
    val
  })

  output$equation <- renderUI({
    m <- round(m_val(), 2)
    p <- round(p_val(), 2)
    withMathJax(sprintf("$$f(x) = %s$$", fmt_affine(m, p)))
  })

  output$variation <- renderUI({
    if (!isTRUE(input$showA) || !isTRUE(input$showB)) return(NULL)
    m <- m_val()
    txt <- if (m > 0) {
      "\\(f\\) est croissante sur \\(\\mathbb{R}\\)."
    } else if (m < 0) {
      "\\(f\\) est décroissante sur \\(\\mathbb{R}\\)."
    } else {
      "\\(f\\) est constante sur \\(\\mathbb{R}\\)."
    }
    withMathJax(h4(txt))
  })

  output$plot <- renderPlot({
    m <- m_val()
    p <- p_val()

    line_df <- data.frame(x = c(-5, 5), y = m * c(-5, 5) + p)

    x_label <- 4.3
    y_label <- m * x_label + p
    if (y_label > 4.6 || y_label < -4.6) {
      y_label <- if (m >= 0) 4.6 else -4.6
      x_label <- if (m != 0) (y_label - p) / m else 4.3
      x_label <- max(min(x_label, 4.8), -4.8)
      y_label <- m * x_label + p
    }
    label_df <- data.frame(x = x_label, y = y_label,
                            label = sprintf("d : y = %s",
                                            fmt_affine(round(m, 2), round(p, 2), spaced = FALSE)))

    # Points A and B, with dotted guide lines and axis ticks/labels
    points_df <- data.frame(x = numeric(0), y = numeric(0),
                             label = character(0), color = character(0))
    guides_df <- data.frame(x = numeric(0), y = numeric(0),
                             xend = numeric(0), yend = numeric(0),
                             color = character(0))
    axis_ticks_df <- data.frame(x1 = numeric(0), y1 = numeric(0),
                                 x2 = numeric(0), y2 = numeric(0),
                                 color = character(0))
    axis_labels_df <- data.frame(x = numeric(0), y = numeric(0), label = character(0),
                                  color = character(0), hjust = numeric(0), vjust = numeric(0))

    add_point <- function(xP, yP, name, color) {
      points_df <<- rbind(points_df, data.frame(x = xP, y = yP, label = name, color = color))
      guides_df <<- rbind(guides_df,
                           data.frame(x = xP, y = 0, xend = xP, yend = yP, color = color),
                           data.frame(x = 0, y = yP, xend = xP, yend = yP, color = color))
      # Ticks on both axes, at the point's projections
      axis_ticks_df <<- rbind(axis_ticks_df,
                               data.frame(x1 = xP, y1 = -0.15, x2 = xP, y2 = 0.15, color = color),
                               data.frame(x1 = -0.15, y1 = yP, x2 = 0.15, y2 = yP, color = color))
      # Labels placed on the side of each axis opposite the dotted guide line
      # (further out than the unit tick labels, to avoid overlapping them)
      x_off <- if (yP >= 0) -0.55 else 0.55
      y_off <- if (xP >= 0) -0.55 else 0.55
      axis_labels_df <<- rbind(axis_labels_df,
                                data.frame(x = xP, y = x_off,
                                           label = sprintf("x[%s] == %s", name, fmt_num(xP)),
                                           color = color,
                                           hjust = 0.5, vjust = if (x_off < 0) 1 else 0),
                                data.frame(x = y_off, y = yP,
                                           label = sprintf("y[%s] == %s", name, fmt_num(yP)),
                                           color = color,
                                           hjust = if (y_off < 0) 1 else 0, vjust = 0.5))
    }
    if (isTRUE(input$showA) && !is.null(input$xA) && !is.na(input$xA)) {
      add_point(input$xA, m * input$xA + p, "A", POINT_A_COLOR)
    }
    if (isTRUE(input$showB) && !is.null(input$xB) && !is.na(input$xB)) {
      add_point(input$xB, m * input$xB + p, "B", POINT_B_COLOR)
    }

    # Regular unit ticks on both axes, with their values below/left of the ticks
    unit_ticks_x <- data.frame(x1 = -5:5, y1 = -0.12, x2 = -5:5, y2 = 0.12)
    unit_ticks_y <- data.frame(x1 = -0.12, y1 = -5:5, x2 = 0.12, y2 = -5:5)
    unit_labels_x <- data.frame(x = setdiff(-5:5, 0), y = -0.3)
    unit_labels_y <- data.frame(x = -0.3, y = setdiff(-5:5, 0))

    ggplot() +
      geom_hline(yintercept = -5:5, color = "grey85", linewidth = 0.3) +
      geom_vline(xintercept = -5:5, color = "grey85", linewidth = 0.3) +
      # axes with arrowheads
      geom_segment(aes(x = -5, y = 0, xend = 5.4, yend = 0),
                   color = "black", linewidth = 0.6,
                   arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
      geom_segment(aes(x = 0, y = -5, xend = 0, yend = 5.4),
                   color = "black", linewidth = 0.6,
                   arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
      geom_segment(data = unit_ticks_x, aes(x = x1, y = y1, xend = x2, yend = y2),
                   color = "black", linewidth = 0.4) +
      geom_segment(data = unit_ticks_y, aes(x = x1, y = y1, xend = x2, yend = y2),
                   color = "black", linewidth = 0.4) +
      geom_text(data = unit_labels_x, aes(x = x, y = y, label = x),
                color = "grey30", size = 3.2, hjust = 0.5, vjust = 1) +
      geom_text(data = unit_labels_y, aes(x = x, y = y, label = y),
                color = "grey30", size = 3.2, hjust = 1, vjust = 0.5) +
      annotate("text", x = -0.3, y = -0.3, label = "0", color = "grey30", size = 3.2,
               hjust = 1, vjust = 1) +
      annotate("text", x = 5.6, y = -0.25, label = "x", size = 5, fontface = "italic") +
      annotate("text", x = 0.3, y = 5.6, label = "y", size = 5, fontface = "italic") +
      geom_line(data = line_df, aes(x = x, y = y), color = LINE_COLOR, linewidth = 1.2) +
      geom_label(data = label_df, aes(x = x, y = y, label = label),
                 color = LINE_COLOR, fill = "white", fontface = "bold", size = 4.2,
                 label.padding = unit(0.25, "lines"),
                 vjust = -0.5, hjust = 0.5) +
      { if (nrow(guides_df) > 0)
          geom_segment(data = guides_df, aes(x = x, y = y, xend = xend, yend = yend),
                       color = guides_df$color, linetype = "dotted", linewidth = 0.7)
      } +
      { if (nrow(axis_ticks_df) > 0)
          geom_segment(data = axis_ticks_df, aes(x = x1, y = y1, xend = x2, yend = y2),
                       color = axis_ticks_df$color, linewidth = 0.9)
      } +
      { if (nrow(axis_labels_df) > 0)
          geom_text(data = axis_labels_df, aes(x = x, y = y, label = label),
                    color = axis_labels_df$color, fontface = "bold", size = 3.6,
                    hjust = axis_labels_df$hjust, vjust = axis_labels_df$vjust, parse = TRUE)
      } +
      { if (nrow(points_df) > 0)
          geom_point(data = points_df, aes(x = x, y = y),
                     color = points_df$color, size = 3)
      } +
      { if (nrow(points_df) > 0)
          geom_text(data = points_df, aes(x = x, y = y, label = label),
                    color = points_df$color, fontface = "bold", size = 5,
                    vjust = -1, hjust = -0.4)
      } +
      coord_cartesian(xlim = c(-5, 5.8), ylim = c(-5, 5.8)) +
      scale_x_continuous(breaks = -5:5) +
      scale_y_continuous(breaks = -5:5) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 14) +
      theme(panel.grid = element_blank(), aspect.ratio = 1,
            axis.text = element_blank(), axis.ticks = element_blank())
  })
}

shinyApp(ui, server)
