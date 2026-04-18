# build_site_caches.R

library(data.table)
library(stringr)

# ---------------------------
# THREADS
# ---------------------------
setDTthreads(0)

# ---------------------------
# PATHS
# --------------------------- 
base_dir <- "."

ebd_file <- file.path(
  base_dir,
  "data_raw/ebd/ebd_US-VA_smp_relMar-2026.txt"
)

vbwt_sites_file <- file.path(
  base_dir,
  "data_raw/SiteGPSCoords_public_noboats.csv"
)

cache_dir <- file.path(
  base_dir,
  "data_processed/caches"
)

summary_file <- file.path(
  base_dir,
  "data_processed/vbwt_site_cache_index.csv"
)

filtered_ebd_file <- file.path(
  base_dir,
  "data_processed/ebd_vbwt_filtered_all.txt"
)

hotspot_ids_file <- file.path(
  base_dir,
  "data_processed/vbwt_hotspot_ids.txt"
)

hotspot_md5_file <- file.path(
  base_dir,
  "data_processed/vbwt_hotspot_ids_md5_all.txt"
)

dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(filtered_ebd_file), recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# CLEAR OLD CACHE FILES
# ---------------------------
old_cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
if (length(old_cache_files) > 0) {
  unlink(old_cache_files, force = TRUE)
}

# ---------------------------
# HELPER FUNCTIONS
# ---------------------------
make_safe_name <- function(x) {
  x <- tolower(x)
  x <- str_replace_all(x, "[^a-z0-9]+", "_")
  x <- str_replace_all(x, "^_|_$", "")
  x
}

standardize_names <- function(x) {
  x |>
    toupper() |>
    str_replace_all("[^A-Z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
}

# ---------------------------
# READ VBWT SITE FILE
# ---------------------------
message("Reading VBWT site file...")

vbwt_sites <- fread(file = vbwt_sites_file)

if (ncol(vbwt_sites) < 4) {
  stop("VBWT site file must have at least 4 columns: SiteName, Latitude, Longitude, HotspotID")
}

setnames(
  vbwt_sites,
  old = names(vbwt_sites)[1:4],
  new = c("SiteName", "Latitude", "Longitude", "HotspotID")
)

vbwt_sites[, SiteName := as.character(SiteName)]
vbwt_sites[, Latitude := suppressWarnings(as.numeric(Latitude))]
vbwt_sites[, Longitude := suppressWarnings(as.numeric(Longitude))]
vbwt_sites[, HotspotID := trimws(as.character(HotspotID))]

vbwt_sites <- vbwt_sites[
  !is.na(HotspotID) & HotspotID != "",
  .(SiteName, Latitude, Longitude, HotspotID)
]

vbwt_sites <- unique(vbwt_sites, by = "HotspotID")
setorder(vbwt_sites, SiteName, HotspotID)

hotspot_ids <- sort(unique(vbwt_sites$HotspotID))

if (length(hotspot_ids) == 0) {
  stop("No valid HotspotID values found in VBWT site file.")
}

# ---------------------------
# WRITE HOTSPOT ID REFERENCE FILE
# ---------------------------
writeLines(hotspot_ids, hotspot_ids_file)
current_hotspot_md5 <- unname(tools::md5sum(hotspot_ids_file))

previous_hotspot_md5 <- if (file.exists(hotspot_md5_file)) {
  readLines(hotspot_md5_file, warn = FALSE)
} else {
  ""
}

need_refilter <- !file.exists(filtered_ebd_file) || !identical(current_hotspot_md5, previous_hotspot_md5)

# ---------------------------
# INSPECT RAW EBD HEADER
# ---------------------------
message("Inspecting EBD header...")

header_line <- readLines(ebd_file, n = 1, warn = FALSE)
if (length(header_line) == 0) {
  stop("EBD file appears to be empty.")
}

ebd_header <- strsplit(header_line, "\t", fixed = TRUE)[[1]]
ebd_header_std <- standardize_names(ebd_header)

locality_col_idx <- match("LOCALITY_ID", ebd_header_std)

if (is.na(locality_col_idx)) {
  stop(
    "LOCALITY_ID column not found after standardizing header names. Header names found: ",
    paste(ebd_header, collapse = ", ")
  )
}

# ---------------------------
# FILTER RAW EBD TO VBWT HOTSPOTS
# ---------------------------
if (need_refilter) {
  message("Filtering raw EBD by exact LOCALITY_ID values in chunks...")
  
  filtered_tmp <- paste0(filtered_ebd_file, ".tmp")
  if (file.exists(filtered_tmp)) {
    unlink(filtered_tmp, force = TRUE)
  }
  
  in_con <- file(ebd_file, open = "r")
  out_con <- file(filtered_tmp, open = "w")
  
  on.exit({
    try(close(in_con), silent = TRUE)
    try(close(out_con), silent = TRUE)
  }, add = TRUE)
  
  first_line <- readLines(in_con, n = 1, warn = FALSE)
  if (length(first_line) == 0) {
    stop("EBD file appears to be empty when filtering.")
  }
  
  writeLines(first_line, out_con)
  
  chunk_size <- 100000L
  total_lines <- 0L
  kept_lines <- 0L
  
  repeat {
    lines <- readLines(in_con, n = chunk_size, warn = FALSE)
    
    if (length(lines) == 0) {
      break
    }
    
    total_lines <- total_lines + length(lines)
    
    locality_vals <- tstrsplit(
      lines,
      "\t",
      fixed = TRUE,
      keep = locality_col_idx
    )[[1]]
    
    keep <- !is.na(locality_vals) & locality_vals %chin% hotspot_ids
    
    if (any(keep)) {
      writeLines(lines[keep], out_con)
      kept_lines <- kept_lines + sum(keep)
    }
    
    if (total_lines %% 1000000L == 0L) {
      message("Processed ", format(total_lines, big.mark = ","), " lines...")
    }
  }
  
  close(in_con)
  close(out_con)
  
  if (!file.exists(filtered_tmp)) {
    stop("Filtered file was not created.")
  }
  
  if (file.info(filtered_tmp)$size == 0) {
    stop("Filtered file was created but is empty.")
  }
  
  if (kept_lines == 0L) {
    stop("Filtering completed, but zero rows matched your hotspot IDs.")
  }
  
  if (file.exists(filtered_ebd_file)) {
    unlink(filtered_ebd_file, force = TRUE)
  }
  
  renamed <- file.rename(filtered_tmp, filtered_ebd_file)
  if (!renamed) {
    stop("Could not move filtered temporary file into place.")
  }
  
  writeLines(current_hotspot_md5, hotspot_md5_file)
  
  message("Filtering complete.")
  message("Total data rows scanned: ", format(total_lines, big.mark = ","))
  message("Total data rows kept: ", format(kept_lines, big.mark = ","))
  message("Filtered EBD written to: ", filtered_ebd_file)
} else {
  message("Using existing filtered EBD: ", filtered_ebd_file)
}

# ---------------------------
# READ FILTERED EBD
# ---------------------------
message("Reading filtered EBD...")

ebd <- fread(
  file = filtered_ebd_file,
  sep = "\t",
  quote = "",
  showProgress = TRUE,
  na.strings = c("", "NA", "NaN")
)

setnames(ebd, names(ebd), standardize_names(names(ebd)))

required_after_filter <- c("LOCALITY_ID", "SAMPLING_EVENT_IDENTIFIER")
missing_required <- setdiff(required_after_filter, names(ebd))

if (length(missing_required) > 0) {
  stop(
    "Missing required columns in filtered EBD: ",
    paste(missing_required, collapse = ", ")
  )
}

# ---------------------------
# KEEP ONLY NEEDED COLUMNS
# ---------------------------
keep_cols <- intersect(
  c(
    "LOCALITY_ID",
    "SAMPLING_EVENT_IDENTIFIER",
    "GROUP_IDENTIFIER",
    "SCIENTIFIC_NAME",
    "CATEGORY"
  ),
  names(ebd)
)

ebd <- ebd[, ..keep_cols]

# ---------------------------
# CLEAN KEY FIELDS
# ---------------------------
ebd[, LOCALITY_ID := trimws(as.character(LOCALITY_ID))]
ebd[, SAMPLING_EVENT_IDENTIFIER := trimws(as.character(SAMPLING_EVENT_IDENTIFIER))]

if ("GROUP_IDENTIFIER" %in% names(ebd)) {
  ebd[, GROUP_IDENTIFIER := trimws(as.character(GROUP_IDENTIFIER))]
}

if ("SCIENTIFIC_NAME" %in% names(ebd)) {
  ebd[, SCIENTIFIC_NAME := trimws(as.character(SCIENTIFIC_NAME))]
}

if ("CATEGORY" %in% names(ebd)) {
  ebd[, CATEGORY := trimws(tolower(as.character(CATEGORY)))]
}

# ---------------------------
# BUILD UNIQUE CHECKLIST IDs
# ---------------------------
message("Building unique checklist IDs...")

if ("GROUP_IDENTIFIER" %in% names(ebd)) {
  ebd[, UNIQUE_CHECKLIST_ID := fifelse(
    !is.na(GROUP_IDENTIFIER) & GROUP_IDENTIFIER != "",
    GROUP_IDENTIFIER,
    SAMPLING_EVENT_IDENTIFIER
  )]
} else {
  ebd[, UNIQUE_CHECKLIST_ID := SAMPLING_EVENT_IDENTIFIER]
}

# ---------------------------
# SUMMARIZE BY HOTSPOT
# ---------------------------
message("Summarizing by hotspot...")

if ("SCIENTIFIC_NAME" %in% names(ebd) && "CATEGORY" %in% names(ebd)) {
  vbwt_summary <- ebd[
    ,
    .(
      TotalChecklists = uniqueN(UNIQUE_CHECKLIST_ID),
      TotalSpecies = uniqueN(
        SCIENTIFIC_NAME[
          CATEGORY == "species" &
            !is.na(SCIENTIFIC_NAME) &
            SCIENTIFIC_NAME != ""
        ]
      )
    ),
    by = .(HotspotID = LOCALITY_ID)
  ]
} else if ("SCIENTIFIC_NAME" %in% names(ebd)) {
  vbwt_summary <- ebd[
    ,
    .(
      TotalChecklists = uniqueN(UNIQUE_CHECKLIST_ID),
      TotalSpecies = uniqueN(
        SCIENTIFIC_NAME[
          !is.na(SCIENTIFIC_NAME) &
            SCIENTIFIC_NAME != ""
        ]
      )
    ),
    by = .(HotspotID = LOCALITY_ID)
  ]
} else {
  vbwt_summary <- ebd[
    ,
    .(
      TotalChecklists = uniqueN(UNIQUE_CHECKLIST_ID),
      TotalSpecies = 0L
    ),
    by = .(HotspotID = LOCALITY_ID)
  ]
}

vbwt_summary[, Checklist_CPUE := fifelse(
  TotalChecklists > 0,
  TotalSpecies / TotalChecklists,
  0
)]

# ---------------------------
# JOIN SITE NAMES AND COORDINATES
# ---------------------------
vbwt_summary <- merge(
  vbwt_summary,
  vbwt_sites,
  by = "HotspotID",
  all.x = TRUE
)

vbwt_summary <- vbwt_summary[, .(
  SiteName,
  Latitude,
  Longitude,
  HotspotID,
  TotalChecklists,
  TotalSpecies,
  Checklist_CPUE
)]

setorder(vbwt_summary, SiteName, HotspotID)

# ---------------------------
# WRITE CACHE FILES
# ---------------------------
message("Writing cache files...")

cache_index_list <- vector("list", nrow(vbwt_summary))
cache_date <- as.character(Sys.Date())

for (i in seq_len(nrow(vbwt_summary))) {
  cache_obj <- vbwt_summary[i, .(
    SiteName,
    Latitude,
    Longitude,
    HotspotID,
    TotalChecklists,
    TotalSpecies,
    Checklist_CPUE
  )]
  
  cache_obj[, `:=`(
    CacheDate = cache_date,
    DataSource = "EBD"
  )]
  
  file_stub <- paste0(
    sprintf("%03d", i), "_",
    make_safe_name(vbwt_summary$SiteName[i]), "_",
    vbwt_summary$HotspotID[i]
  )
  
  cache_file <- file.path(cache_dir, paste0(file_stub, ".rds"))
  saveRDS(cache_obj, cache_file, compress = TRUE)
  
  cache_index_list[[i]] <- data.table(
    SiteName = vbwt_summary$SiteName[i],
    HotspotID = vbwt_summary$HotspotID[i],
    CacheFile = cache_file,
    TotalChecklists = vbwt_summary$TotalChecklists[i],
    TotalSpecies = vbwt_summary$TotalSpecies[i]
  )
}

cache_index_df <- rbindlist(cache_index_list, use.names = TRUE, fill = TRUE)
fwrite(cache_index_df, summary_file)

# ---------------------------
# FINAL STATUS MESSAGES
# ---------------------------
message("Done.")
message("Filtered EBD: ", filtered_ebd_file)
message("Caches written to: ", cache_dir)
message("Index written to: ", summary_file)
message("Sites cached: ", nrow(vbwt_summary))
message("Total checklists: ", format(sum(vbwt_summary$TotalChecklists, na.rm = TRUE), big.mark = ","))