
factominer_palette <- c(
  "#0072B2", "#E69F00", "#009E73", "#CC79A7", 
  "#56B4E9", "#D55E00", "#F0E442", "#999999"
)

CONTRIB_LOW   <- "#EEF4FB"
CONTRIB_MID   <- "#7FBADF"
CONTRIB_HIGH  <- "#0072B2"

COS2_LOW  <- "#EEF7F5"
COS2_MID  <- "#5BB8A8"
COS2_HIGH <- "#007D6E"

COORD_NEG <- "#C0392B"
COORD_MID <- "#F5F5F5"
COORD_POS <- "#0072B2"

PVAL_LOW  <- "#1A7340"
PVAL_MID  <- "#F7F4A0"
PVAL_HIGH <- "#C0392B"

factominer_grid_col <- "#E4E7EA"

theme_factominer <- function(
    base_size         = 11,
    base_family       = "Inclusive Sans",
    title_family      = "Atkinson Hyperlegible Next",
    
    rel_title         = 1.5,
    rel_subtitle      = 1.05,
    rel_caption       = 0.78,
    rel_axis_title    = 0.9,
    rel_axis_text     = 0.85,
    rel_legend_title  = 0.88,
    rel_legend_text   = 0.85,
    rel_strip_text    = 0.9,
    
    # point_size        = 2.5,
    # label_size        = 3.5, 
    
    dark_text         = "#1C2B3A",
    mid_text          = "#4A5A6A",
    light_text        = "#7A8A9A",
    factominer_grid_col = "#E2E8F0"
) {

  # if (!"InclusiveSans" %in% sysfonts::font_families()) {
    # sysfonts::font_add_google("Inclusive Sans", family = "InclusiveSans")
  # }
  # if (!"AtkinsonNext" %in% sysfonts::font_families()) {
    # sysfonts::font_add_google("Atkinson Hyperlegible Next", family = "AtkinsonNext")
  # }

  format_google_name <- function(family_name) {
    res <- gsub("([a-z])([A-Z])", "\\1 \\2", family_name)
    trimws(res)
  }
  if (!base_family %in% sysfonts::font_families()) {
    tryCatch({
      sysfonts::font_add_google(format_google_name(base_family), family = base_family)
    }, error = function(e) {
      warning("Unable to load the Google font '", base_family, "'. Use of the font by default.")
    })
  }
    if (!title_family %in% sysfonts::font_families()) {
    tryCatch({
      sysfonts::font_add_google(format_google_name(title_family), family = title_family)
    }, error = function(e) {
      warning("Unable to load the Google font '", title_family, "'. Use of the font by default.")
    })
  }
  
  showtext::showtext_auto(enable = TRUE)
  
# if size of points and labels are defined in the theme
  # ggplot2::update_geom_defaults("point", list(size = point_size))
  # ggplot2::update_geom_defaults("text", list(size = label_size / 2.8346, family = base_family))
  # ggplot2::update_geom_defaults("label", list(size = label_size / 2.8346, family = base_family))
  
  # if (requireNamespace("ggrepel", quietly = TRUE)) {
    # ggplot2::update_geom_defaults(ggrepel::GeomTextRepel, list(size = label_size / 2.8346, family = base_family))
    # ggplot2::update_geom_defaults(ggrepel::GeomLabelRepel, list(size = label_size / 2.8346, family = base_family))
  # }
  
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "#FFFFFF", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),

      text = ggplot2::element_text(
        colour     = mid_text,
        family     = base_family,
        lineheight = 1.2
      ),

      plot.title = ggtext::element_markdown(
        colour  = dark_text,
        family  = title_family,
        size    = ggplot2::rel(rel_title),
        face    = "bold",
        margin  = ggplot2::margin(t = 10, b = 6)
      ),
      plot.subtitle = ggtext::element_markdown(
        colour  = mid_text,
        family  = base_family,
        size    = ggplot2::rel(rel_subtitle),
        margin  = ggplot2::margin(b = 10)
      ),
      plot.caption = ggtext::element_markdown(
        colour  = light_text,
        family  = base_family,
        size    = ggplot2::rel(rel_caption),
        hjust   = 0,      
        margin  = ggplot2::margin(t = 8)
      ),
      plot.title.position   = "plot",
      plot.caption.position = "plot",

      axis.title   = ggplot2::element_text(colour = mid_text, size = ggplot2::rel(rel_axis_title)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 8)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 8)),
      axis.text    = ggplot2::element_text(colour = light_text, size = ggplot2::rel(rel_axis_text)),
      axis.line    = ggplot2::element_line(colour = factominer_grid_col, linewidth = 0.4),
      axis.ticks   = ggplot2::element_line(colour = factominer_grid_col, linewidth = 0.3),
      axis.ticks.length = ggplot2::unit(3, "pt"),

      panel.grid.major = ggplot2::element_line(colour = factominer_grid_col, linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),

      legend.position      = "right",
      legend.justification = "center",
      legend.direction     = "vertical",
      legend.title = ggplot2::element_text(
        colour = mid_text, size = ggplot2::rel(rel_legend_title), face = "bold"
      ),
      legend.text  = ggplot2::element_text(colour = mid_text, size = ggplot2::rel(rel_legend_text)),
      legend.key.size  = ggplot2::unit(14, "pt"),
      legend.spacing.x = ggplot2::unit(6, "pt"),
      legend.margin    = ggplot2::margin(b = 4),

      strip.background = ggplot2::element_rect(fill = factominer_grid_col, colour = NA),
      strip.text = ggplot2::element_text(
        colour = dark_text,
        family = title_family,
        face   = "bold",
        size   = ggplot2::rel(rel_strip_text),
        margin = ggplot2::margin(4, 6, 4, 6)
      ),

      plot.margin = ggplot2::margin(0.4, 0.4, 0.3, 0.4, "cm")
    )
}

scale_colour_factominer <- function(...) {
  ggplot2::scale_colour_manual(values = factominer_palette, ...)
}

scale_fill_factominer <- function(...) {
  ggplot2::scale_fill_manual(values = factominer_palette, ...)
}

scale_colour_factominer_contrib <- function(name = "Contribution", limits = NULL, ...) {
  ggplot2::scale_colour_gradientn(
    colours = c(CONTRIB_LOW, CONTRIB_MID, CONTRIB_HIGH),
    name    = name,
    limits  = limits,
    labels  = function(x) paste0(round(x, 1), "%"),
    guide = ggplot2::guide_colourbar(
      barwidth       = unit(0.4, "cm"),
      barheight      = unit(5, "cm"),
      title.position = "top",
      title.hjust    = 0.5
    ),
    ...
  )
}

scale_fill_factominer_contrib <- function(name = "Contribution", limits = NULL, ...) {
  ggplot2::scale_fill_gradientn(
    colours = c(CONTRIB_LOW, CONTRIB_MID, CONTRIB_HIGH),
    name    = name,
    limits  = limits,
    labels  = function(x) paste0(round(x, 1), "%"),
    guide = ggplot2::guide_colourbar(
      barwidth       = unit(0.4, "cm"),
      barheight      = unit(5, "cm"),
      title.position = "top",
      title.hjust    = 0.5
    ),
    ...
  )
}

scale_colour_factominer_cos2 <- function(name = "cos2", breaks = c(0, 0.25, 0.5, 0.75, 1), ...) {
  ggplot2::scale_colour_gradientn(
    colours = c(COS2_LOW, COS2_MID, COS2_HIGH),
    name    = name,
    limits  = c(0, 1),
    breaks  = breaks,
    labels  = function(x) formatC(x, digits = 2, format = "f"),
    guide = ggplot2::guide_colourbar(
      barwidth       = unit(0.4, "cm"),
      barheight      = unit(5, "cm"),
      title.position = "top",
      title.hjust    = 0.5
    ),
    ...
  )
}

scale_fill_factominer_cos2 <- function(name = "cos2", breaks = c(0, 0.25, 0.5, 0.75, 1), ...) {
  ggplot2::scale_fill_gradientn(
    colours = c(COS2_LOW, COS2_MID, COS2_HIGH),
    name    = name,
    limits  = c(0, 1),
    breaks  = breaks,
    labels  = function(x) formatC(x, digits = 2, format = "f"),
    guide = ggplot2::guide_colourbar(
      barwidth       = unit(0.4, "cm"),
      barheight      = unit(5, "cm"),
      title.position = "top",
      title.hjust    = 0.5
    ),
    ...
  )
}

scale_colour_factominer_coord <- function(name = "Coordinate", midpoint = 0, ...) {
  ggplot2::scale_colour_gradient2(
    low      = COORD_NEG,
    mid      = COORD_MID,
    high     = COORD_POS,
    midpoint = midpoint,
    name     = name,
    guide = ggplot2::guide_colourbar(
      barwidth       = unit(0.4, "cm"),
      barheight      = unit(5, "cm"),
      title.position = "top",
      title.hjust    = 0.5
    ),
    ...
  )
}

scale_fill_factominer_coord <- function(name = "Coordinate", midpoint = 0, ...) {
  ggplot2::scale_fill_gradient2(
    low      = COORD_NEG,
    mid      = COORD_MID,
    high     = COORD_POS,
    midpoint = midpoint,
    name     = name,
    guide = ggplot2::guide_colourbar(
      barwidth       = unit(0.4, "cm"),
      barheight      = unit(5, "cm"),
      title.position = "top",
      title.hjust    = 0.5
    ),
    ...
  )
}

scale_colour_factominer_pvalue <- function(name = "p-valeur", threshold = 0.05, breaks = c(0, 0.001, 0.01, 0.05, 0.1, 0.5, 1), ...) {
  values_scaled <- scales::rescale(c(0, threshold * 0.5, threshold, 0.1, 1))
  ggplot2::scale_colour_gradientn(
    colours = c(PVAL_LOW, "#5AAA6F", PVAL_MID, "#E8A87C", PVAL_HIGH),
    values  = values_scaled,
    name    = name,
    limits  = c(0, 1),
    breaks  = breaks,
    labels  = function(x) {
      ifelse(x == threshold, paste0(x, " *"), as.character(x))
    },
    guide = ggplot2::guide_colourbar(
      barwidth       = unit(0.4, "cm"),
      barheight      = unit(5, "cm"),
      title.position = "top",
      title.hjust    = 0.5
    ),
    ...
  )
}

scale_fill_factominer_pvalue <- function(name = "p-valeur", threshold = 0.05, breaks = c(0, 0.001, 0.01, 0.05, 0.1, 0.5, 1), ...) {
  values_scaled <- scales::rescale(c(0, threshold * 0.5, threshold, 0.1, 1))
  ggplot2::scale_fill_gradientn(
    colours = c(PVAL_LOW, "#5AAA6F", PVAL_MID, "#E8A87C", PVAL_HIGH),
    values  = values_scaled,
    name    = name,
    limits  = c(0, 1),
    breaks  = breaks,
    labels  = function(x) {
      ifelse(x == threshold, paste0(x, " *"), as.character(x))
    },
    guide   = ggplot2::guide_colourbar(
      barwidth  = unit(8, "cm"),
      barheight = unit(0.4, "cm"),
      title.position = "top",
      title.hjust    = 0
    ),
    ...
  )
}

geom_contrib_reference <- function(n, ...) {
  geom_hline(
    yintercept = 100 / n,
    linetype   = "dashed",
    colour     = "#4A5A6A",
    linewidth  = 0.6,
    ...
  )
}

factominer_breaks_contrib <- function(max_val, n_breaks = 5) {
  pretty(c(0, max_val), n = n_breaks)
}

factominer_breaks_cos2 <- function() {
  c(0, 0.25, 0.5, 0.75, 1)
}

factominer_labels_pct <- function(digits = 1) {
  function(x) paste0(round(x, digits), "%")
}