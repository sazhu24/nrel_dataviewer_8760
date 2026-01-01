
library(tidyverse)
library(lubridate)
library(fs)

# ----------------------------
# 0) Config
# ----------------------------

# update state
state <- "CT"

# create directories for input and output folders
paths <- list(
  in_res  = fs::path(state, "inputs", "ResStock"),
  in_com  = fs::path(state, "inputs", "ComStock"),
  out_res = fs::path(state, "outputs", "ResStock"),
  out_com = fs::path(state, "outputs", "ComStock")
)

purrr::walk(paths[c("out_res", "out_com")], fs::dir_create, recurse = TRUE)

options(scipen = 999, timeout = 300)

# ----------------------------
# 1) Theme (only needed if plotting)
# ----------------------------
theme_sz <- function() {
  theme(
    axis.title.y = element_text(vjust = 2, size = 12),
    axis.title.x = element_text(vjust = -0.5, size = 12),
    axis.text = element_text(size = 12),
    plot.margin = unit(c(.6, .7, .4, .6), "cm"),
    legend.position = "bottom",
    plot.title = element_text(size = 12.5, vjust = 2),
    plot.subtitle = element_text(size = 10, vjust = 2),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(colour = "#d1d1d1", linewidth = 0.5),
    panel.grid.major = element_line(colour = "#d1d1d1", linewidth = 0.2)
  )
}

# ----------------------------
# 2) Helpers
# ----------------------------
unitize_load_shape <- function(x) {
  s <- sum(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(NA_real_, length(x)))
  x / s
}

assert_has_cols <- function(df, cols, context = "") {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(
      "Missing required column(s)",
      if (nzchar(context)) paste0(" in ", context),
      ": ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

clean_15series <- function(df,
                           timestamp_col = "Timestamp (EST)",
                           tz_out = "America/New_York") {
  
  required <- c(
    timestamp_col,
    "baseline.out.electricity.total.energy_consumption.kwh",
    "baseline.out.natural_gas.total.energy_consumption.kwh",
    "upgrade.out.electricity.total.energy_consumption.kwh",
    "upgrade.out.natural_gas.total.energy_consumption.kwh"
  )
  
  assert_has_cols(df, required, context = "input timeseries")
  
  df %>%
    rename(timestamp = all_of(timestamp_col)) %>%
    mutate(
      timestamp_clean = gsub(" [A-Z]{2,4}$", "", timestamp),
      time_utc = parse_date_time(
        timestamp_clean,
        orders = c("Ymd HMS", "Ymd HM", "Ymd"),
        tz = "UTC"
      ),
      # keeping your existing adjustment
      time_adj_utc = time_utc - minutes(5) + hours(10),
      time_est = with_tz(time_adj_utc, tzone = tz_out),
      time = floor_date(time_est, unit = "hour")
    ) %>%
    group_by(time) %>%
    summarise(
      baseline_electricity_kwh = sum(.data[["baseline.out.electricity.total.energy_consumption.kwh"]], na.rm = TRUE),
      baseline_gas_kwh         = sum(.data[["baseline.out.natural_gas.total.energy_consumption.kwh"]], na.rm = TRUE),
      upgrade_electricity_kwh  = sum(.data[["upgrade.out.electricity.total.energy_consumption.kwh"]], na.rm = TRUE),
      upgrade_gas_kwh          = sum(.data[["upgrade.out.natural_gas.total.energy_consumption.kwh"]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(time)) %>%
    mutate(
      incremental_electricity_kwh = upgrade_electricity_kwh - baseline_electricity_kwh,
      incremental_gas_kwh         = upgrade_gas_kwh - baseline_gas_kwh,
      incremental_electricity_unitized = unitize_load_shape(incremental_electricity_kwh),
      incremental_gas_unitized = unitize_load_shape(incremental_gas_kwh)
    )
}

read_and_clean <- function(in_path) {
  readr::read_csv(in_path, show_col_types = FALSE) %>%
    clean_15series()
}

write_clean <- function(df, out_dir, out_name) {
  out_path <- fs::path(out_dir, paste0(out_name, ".csv"))
  readr::write_csv(df, out_path)
  out_path
}

plot_series <- function(
    df,
    title,
    y = c("incremental_unitized", "both_mwh")
) {
  y <- match.arg(y)
  
  p <- ggplot(df, aes(x = time)) +
    labs(title = title, x = "") +
    theme_sz()
  
  if (y == "incremental_unitized") {
    
    p +
      geom_line(
        aes(y = incremental_electricity_unitized * 1000),
        linewidth = 0.25
      ) +
      scale_y_continuous(labels = scales::comma) +
      labs(y = "Unitized Electric Load (×1000)")
    
  } else {  # y == "both_mwh"
    
    p +
      geom_line(
        aes(y = baseline_electricity_kwh / 1e3, color = "Baseline"),
        linewidth = 0.25
      ) +
      geom_line(
        aes(y = upgrade_electricity_kwh / 1e3, color = "Upgrade"),
        linewidth = 0.25
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c(
          "Baseline" = "#1b9e77",
          "Upgrade" = "#d95f02"
        )
      ) +
      labs(y = "Electric Load (MWh)", color = "")
  }
}

# safer plotting: doesn't error if df is NULL
plot_if_exists <- function(x,
                           title,
                           y = "incremental_unitized") {
  
  if (is.null(x)) {
    message("Skipping plot (missing series): ", title)
    return(invisible(NULL))
  }
  
  print(plot_series(x, title = title, y = y))
}

process_batch <- function(files, in_dir, out_dir) {
  stopifnot(length(files) > 0)
  
  if (anyDuplicated(names(files))) {
    dupes <- unique(names(files)[duplicated(names(files))])
    stop("Duplicate names in files list: ", paste(dupes, collapse = ", "), call. = FALSE)
  }
  
  log <- tibble(name = names(files), file = unname(files)) %>%
    mutate(
      in_path = fs::path(in_dir, file),
      status = NA_character_,
      out_path = NA_character_,
      msg = NA_character_
    )
  
  # preallocate (keep names stable)
  results <- setNames(vector("list", length(files)), names(files))
  
  for (i in seq_along(files)) {
    nm <- names(files)[i]
    in_path <- fs::path(in_dir, files[[i]])
    
    if (!fs::file_exists(in_path)) {
      message("Missing: ", in_path)
      log$status[i] <- "skipped_missing"
      log$msg[i] <- "file does not exist"
      next
    }
    
    message("Processing: ", in_path)
    
    results[[nm]] <- tryCatch({
      df <- read_and_clean(in_path)
      out_path <- write_clean(df, out_dir, nm)
      log$status[i] <- "processed"
      log$out_path[i] <- out_path
      df
    }, error = function(e) {
      message("Error (", nm, "): ", conditionMessage(e))
      log$status[i] <- "error"
      log$msg[i] <- conditionMessage(e)
      NULL
    })
  }
  
  # IMPORTANT: drop NULLs without breaking name mapping
  keep <- !purrr::map_lgl(results, is.null)
  results <- results[keep]
  
  if (length(results) == 0) warning("No valid files were processed in ", in_dir, call. = FALSE)
  
  attr(results, "log") <- log
  results
}

# ----------------------------
# 3) ResStock
# ----------------------------
res_files <- c(
  U1_FL   = "U1_FL.csv",
  U1_ER   = "U1_ER.csv",
  U4_FL   = "U4_FL.csv",
  U5_FL   = "U5_FL.csv",
  U5_ER   = "U5_ER.csv",
  U16_FL  = "U16_FL.csv",
  U16_ER  = "U16_ER.csv",
  HPWH_FL = "HPWH_FL.csv",
  HPWH_ER = "HPWH_ER.csv"
)

res <- process_batch(res_files, paths$in_res, paths$out_res)
print(attr(res, "log"), n = Inf)

# Example plots
plot_if_exists(res$U1_ER,  title = "ResStock-U1, Electric Baseline", "both_mwh")
plot_if_exists(res$U1_ER,  title = "ResStock-U1, Electric Baseline", "incremental_unitized")

plot_if_exists(res$U1_FL,  title = "ResStock-U1, Fuel Baseline")
plot_if_exists(res$U4_FL,  title = "ResStock-U4, Fuel Baseline")
plot_if_exists(res$U5_ER,  title = "ResStock-U5, Electric Baseline")
plot_if_exists(res$U5_FL,  title = "ResStock-U5, Fuel Baseline")
plot_if_exists(res$U16_ER, title = "ResStock-U16, Electric Baseline")
plot_if_exists(res$U16_FL, title = "ResStock-U16, Fuel Baseline")

# ----------------------------
# 4) ComStock
# ----------------------------
com_files <- c(
  ER_Boiler_FL           = "ER_Boiler_FL.csv",
  HP_Boiler_EB_FL        = "HP_Boiler_EB_FL.csv",
  HP_Boiler_FB_FL        = "HP_Boiler_FB_FL.csv",
  HP_RTU_EB_FL           = "HP_RTU_EB_FL.csv",
  HP_RTU_EB_ER           = "HP_RTU_EB_ER.csv",
  HP_RTU_FB_FL           = "HP_RTU_FB_FL.csv",
  ENV_FL                 = "ENV_FL.csv",
  ENV_ER                 = "ENV_ER.csv",
  DF_Lighting_Thermostat = "DF_Lighting_Thermostat.csv"
)

com <- process_batch(com_files, paths$in_com, paths$out_com)
print(attr(com, "log"), n = Inf)

plot_if_exists(com$HP_RTU_EB_FL,           title = "ComStock HP RTU (EB), Fuel Baseline")
plot_if_exists(com$HP_RTU_FB_FL,           title = "ComStock HP RTU (FB), Fuel Baseline")
plot_if_exists(com$HP_RTU_EB_ER,           title = "ComStock HP RTU (EB), Electric Baseline", "both_mwh")

plot_if_exists(com$HP_Boiler_EB_FL,        title = "ComStock HP Boiler (EB), Fuel Baseline")
plot_if_exists(com$HP_Boiler_FB_FL,        title = "ComStock HP Boiler (FB), Fuel Baseline")
plot_if_exists(com$ER_Boiler_FL,           title = "ComStock Electric Boiler, Fuel Baseline")

plot_if_exists(com$ENV_FL,                 title = "ComStock Envelope, Fuel Baseline")
plot_if_exists(com$ENV_ER,                 title = "ComStock Envelope, Electric Baseline")
plot_if_exists(com$DF_Lighting_Thermostat, title = "ComStock Thermostat")
