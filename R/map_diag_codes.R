# SNOMED lookups
# Original code by Ewen Harrison
# Centre for Medical Informatics, University of Edinburgh, 2022
# Source: https://github.com/SurgicalInformatics/snomed_lookups
# (no license specified in original repository)
#
# Uses Rdiagnosislist by Anoop D. Shah (GPL-3)
# https://github.com/anoopshah/Rdiagnosislist
#
# Modified by Jure Mur for UKBfunctions package.


#' Map diagnostic codes across coding systems via SNOMED CT
#'
#' Builds a lookup that cross-references diagnostic codes between coding
#' systems using SNOMED CT as the hub. It loads the READ2/CTV3 -> SNOMED
#' mapping tables and the SNOMED CT dictionary (or a previously saved `.Rds`),
#' expands every current SNOMED concept, and maps it to READ2 and ICD-10
#' (optionally also CTV3, CTV3-simple, and OPCS4). Optionally, a folder of
#' per-disease `.xlsx` ICD-10 files can be joined to the lookup and written
#' out as mapped `.csv` files.
#'
#' @param readmaps A list of 3 paths for the following 3 files (in this order):
#' Read V2 --> SNOMED concepts, Read V2 -> Snomed terms, CTV3 -> SNOMED. You can
#' find these files by navigating to 'https://isd.digital.nhs.uk/trud/',
#' and searching for 'NHS Data Migration'. You will need a free TRUD account.
#' @param snomed_folders A list of the path(s) to the (unzipped) folder(s)
#' containing SNOMED CT files. You can find this folder by navigating to
#' 'https://isd.digital.nhs.uk/trud/users/authenticated/group/0/home'. The folders
#' you need are 'SNOMED CT UK Clinical Edition, RF2: Full, Snapshot & Delta' and
#' 'SnomedCT_InternationalRF2_PRODUCTION_20260201T120000Z'. Alternatively,
#' a path to the SNOMED .Rds file if the latter has already been created.
#' @param active_only Logical; passed to [Rdiagnosislist::loadSNOMED()]; whether
#' to load only active SNOMED concepts. Ignored when `snomed_folders` is a path
#' to an already-built SNOMED .Rds file.
#' @param include_ctv_opcs4 Logical; whether ctv3, ctv3simple, and opcs4 should
#' be returned in addition to read2 and icd10.
#' @param save_snomed Path and name of the SNOMED .Rds file to be saved to
#' file. If `NULL` (default) or `FALSE`, nothing is exported.
#' @param xlsx_in Optional path to a folder of per-disease `.xlsx` files (each
#' with column 1 = ICD-10 code, column 2 = ICD-10 description). Each file must
#' include a header row as its first row. When supplied
#' (together with `csv_out`), each file is left-joined to the SNOMED lookup and
#' a mapped `.csv` is written. `xlsx_in` and `csv_out` must be supplied
#' together; if both are `NULL` (default) the mapping step is skipped and
#' `lookup` is returned empty (it is derived from these inputs).
#' @param csv_out Optional path to the output folder for the mapped `.csv`
#' files. Created if it does not exist. See `xlsx_in`.
#' @param lkps_maps Optional path to the UKB `all_lkps_maps_v4.xlsx` workbook
#' (UKB resource 592). When supplied, ICD-10 and CTV3 descriptions are filled
#' from its `icd10_lkp` and `read_ctv3_lkp` sheets. If `NULL` (default), those
#' descriptions are left `NA` (OPCS4 has no description table in that file and
#' is always `NA`).
#' @param icd9_include Logical; whether the `lkps_maps` file should be used to
#' additionally map to ICD-9 and include it in the final output. When `TRUE`,
#' the existing `icd10`, `read2`, `ctv3`, and `ctv3simple` rows in `$lookup`
#' are mapped to ICD-9 via the `icd9_icd10`, `read_v2_icd9`, and
#' `read_ctv3_icd9` sheets of `lkps_maps`, with descriptions filled from the
#' `icd9_lkp` sheet. Only single-code rows are taken from `read_v2_icd9`
#' (`icd9_code_def == 1`); range rows are excluded. Requires `lkps_maps` to
#' be supplied (errors otherwise).
#' @return A list with two elements: `lookup`, a long-format data frame
#' **restricted to the `snomed` rows whose ICD-10 is one of the supplied
#' input codes** (the mapping rows behind the per-disease `.csv` files; not a
#' full concept expansion — read2/ctv3/etc. appear only where they co-occur on
#' a supplied-ICD-10 row; an empty tibble when the mapping step is not run, as
#' it is derived from those inputs), with columns `code`; `code_source` (one
#' of 'snomed', 'read2', 'icd10', and — when `include_ctv_opcs4` — 'ctv3',
#' 'ctv3simple', and 'opcs4'; plus 'icd9' when `icd9_include`); `description`;
#' and `variable` (the input file / disease whose supplied ICD-10 the row
#' carried, so a code is repeated once per matching disease). And `csv_files`,
#' a character vector of the written `.csv` paths (empty when the mapping
#' step is not run).
#'
#' @note
#' The output of this function may include data subject to the following licences:
#'
#' This material includes SNOMED Clinical Terms (SNOMED CT) which is used by
#' permission of the International Health Terminology Standards Development
#' Organisation (IHTSDO). All rights reserved. SNOMED CT was originally created
#' by The College of American Pathologists. 'SNOMED' and 'SNOMED CT' are
#' registered trademarks of the IHTSDO.
#'
#' ICD-10 codes, terms and text used by permission of WHO, from: International
#' Statistical Classification of Diseases and Related Health Problems, Tenth
#' Revision (ICD-10). Vols 1-3. Geneva, World Health Organization, 1992-2016.
#'
#' The OPCS Classification of Interventions and Procedures, codes, terms and
#' text is Crown copyright (2019) published by Health and Social Care
#' Information Centre, also known as NHS Digital and licensed under the Open
#' Government Licence.
#'
#' @export


map_diag_codes <- function(readmaps,
                           snomed_folders,
                           active_only,
                           include_ctv_opcs4 = TRUE,
                           save_snomed = NULL,
                           xlsx_in = NULL,
                           csv_out = NULL,
                           lkps_maps = NULL,
                           icd9_include = TRUE) {



  ## Housekeeping: check all was provided in the right way


  # initiate variable about whether in and out folders are given
  run_mapping <- !is.null(xlsx_in) || !is.null(csv_out)

  # the xlsx -> mapped-csv stage needs both folders: error if exactly one given
  if (is.null(xlsx_in) != is.null(csv_out)) {
    stop('Both `xlsx_in` and `csv_out` must be supplied to run the mapping step.')
  }

  # icd9 mapping needs the lkps_maps workbook (icd9_icd10, read_v2_icd9,
  # read_ctv3_icd9, icd9_lkp sheets live there)
  if (isTRUE(icd9_include) && is.null(lkps_maps)) {
    stop('`icd9_include = TRUE` requires `lkps_maps` to be supplied.')
  }

  # variable for whether snomed_folders is itself a single SNOMED .Rds path
  snomed_is_rds <- is.character(snomed_folders) &&
    length(snomed_folders) == 1L &&
    grepl('\\.rds$', snomed_folders, ignore.case = TRUE)

  if (!requireNamespace('Rdiagnosislist', quietly = TRUE)) {
    stop(
      "Package 'Rdiagnosislist' is required but not installed.\n",
      'It was archived from CRAN (2026-05-18). Install it with:\n',
      '  remotes::install_github("anoopshah/Rdiagnosislist")'
    )
  }





  ## Use Rdiagnosislist libary to create mapping tables (or load existing ones)

  # Read V2 -> SNOMED concepts
  # Read V2 -> SNOMED terms
  # CTV3 -> SNOMED
  readmaps <- do.call(Rdiagnosislist::loadREADMAPS, as.list(readmaps))


  # SNOMED dictionary for read2 and ICD10 — either load from the SNOMED
  # CT folder(s) or read a previously saved dictionary .Rds.
  if (snomed_is_rds) {
    snomed_dict <- readRDS(snomed_folders)
  } else {
    snomed_dict <- Rdiagnosislist::loadSNOMED(snomed_folders,
                                              active_only = active_only)
  }

  # optionally cache the dictionary so it can be reused via `snomed_folders`.
  if (!is.null(save_snomed) && !isFALSE(save_snomed)) {
    saveRDS(snomed_dict, save_snomed)
  }

  # expand all current snomed concepts and map them to other coding systems
  # this creates a master lookup table
  if(include_ctv_opcs4){
    snomed <- Rdiagnosislist::SNOMEDconcept('', SNOMED = snomed_dict, exact_match = FALSE) |>
      Rdiagnosislist::getMaps(to = c('read2', 'icd10',
                     'ctv3', 'ctv3simple', 'opcs4'
      ), mappingtable = readmaps, SNOMED = snomed_dict)
  } else {
    snomed <- Rdiagnosislist::SNOMEDconcept('', SNOMED = snomed_dict, exact_match = FALSE) |>
      Rdiagnosislist::getMaps(to = c('read2', 'icd10'
      ), mappingtable = readmaps, SNOMED = snomed_dict)
  }

  # data.table int64 format doesn't work well in tibble, so cast conceptId
  # while moving to a tibble (kept in tidyverse rather than data.table)
  snomed <- snomed |>
    tibble::as_tibble() |>
    dplyr::mutate(conceptId = as.character(conceptId)) |>
    tidyr::unnest(icd10_code, keep_empty = TRUE) |>
    tidyr::unnest(c(read2_code, read2_term), keep_empty = TRUE)

  # final lookup only containing rows with either read2 or icd10
  if (include_ctv_opcs4) {
    snomed <- snomed |>
      tidyr::unnest(opcs4_code, keep_empty = TRUE) |>
      tidyr::unnest(c(ctv3_concept, ctv3_termid), keep_empty = TRUE) |>
      tidyr::unnest(ctv3_simple, keep_empty = TRUE) |>
      finalfit::rm_empty_block(read2_code, icd10_code, opcs4_code,
                               ctv3_concept, ctv3_simple) # remove rows with no lookup
  } else {
    snomed <- finalfit::rm_empty_block(snomed, read2_code, icd10_code) # remove rows with no lookup
  }

  # fetch the SNOMED terms which will be used as descriptions in the final file
  snomed_terms <- Rdiagnosislist::description(
    Rdiagnosislist::as.SNOMEDconcept(unique(snomed$conceptId),
                                     SNOMED = snomed_dict),
    SNOMED = snomed_dict
  ) |>
    tibble::as_tibble() |>
    dplyr::transmute(code = as.character(conceptId), description = term) |>
    dplyr::distinct(code, .keep_all = TRUE)





  ## create a map per-disease based on external ICD10-code table
  ## previously `makeMaps` in https://github.com/SurgicalInformatics/snomed_lookups



  csv_files <- character(0)
  # supplied (dot-stripped) icd10 codes per disease (input file) — drives the
  # $lookup subset below; stays 0-row when the mapping stage is not run.
  xlsx_codes <- tibble::tibble(variable = character(),
                               icd10_norm = character())
  if (run_mapping) {
    # strip the decimal dot from the lookup's icd10 codes (e.g. E66.5 -> E665)
    # to match the format used in the .xlsx files
    snomed_map <- dplyr::mutate(
      snomed, icd10_code = stringr::str_remove(icd10_code, '\\.')
    )

    if (!dir.exists(csv_out)) {
      message('Out folder cannot be found and will be created.')
      dir.create(csv_out, recursive = TRUE)
    }

    files_in <- list.files(xlsx_in, full.names = TRUE)
    nm <- stringr::str_remove(list.files(xlsx_in), '\\..+')
    csv_files <- file.path(csv_out, paste0(nm, '.csv'))

    # each .xlsx: col 1 = icd10 code, col 2 = icd10 description; writes the
    # mapped .csv and returns the disease's supplied icd10 codes for the $lookup subset
    xlsx_codes <- purrr::list_rbind(purrr::pmap(
      list(files_in, csv_files, nm),
      function(.x, .y, .disease) {
        raw <- readxl::read_excel(.x, col_names = TRUE, col_types = 'text') |>
          dplyr::rename('icd10_code' = 1, 'icd10_description' = 2) |>
          dplyr::mutate(icd10_code = stringr::str_remove(icd10_code, '\\.'))
        df <- raw |>
          dplyr::left_join(snomed_map, by = 'icd10_code') |>
          dplyr::rename(snomed_conceptId = conceptId, snomed_term = term) |>
          # align with the long output: read2 codes are truncated to 5 chars
          # (Rdiagnosislist returns the 7-char XXXXX + 2-char term-id form)
          dplyr::mutate(
            code = dplyr::if_else(
              nchar(read2_code) == 7L,
              stringr::str_sub(read2_code, 1L, 5L),
              read2_code
            )
          )
        utils::write.csv(df, .y, row.names = FALSE)
        tibble::tibble(variable = .disease,
                       icd10_norm = unique(raw$icd10_code))
      }
    ))
  }





  ## Reshape to long, restricted to the rows whose icd10 was supplied
  ## (columns: code / code_source / description / variable)

  if (run_mapping && nrow(xlsx_codes) > 0) {
    xlsx_codes <- dplyr::distinct(xlsx_codes)

    # keep only the snomed rows whose icd10 is one of the supplied codes
    rows_supplied <- snomed |>
      dplyr::mutate(icd10_norm = stringr::str_remove(icd10_code, '\\.')) |>
      dplyr::inner_join(xlsx_codes, by = 'icd10_norm',
                        relationship = 'many-to-many')

    # use the SNOMED concept terms in `snomed_terms` for the snomed descriptions
    long <- dplyr::bind_rows(
      rows_supplied |>
        dplyr::distinct(variable, conceptId) |>
        dplyr::transmute(variable, code = conceptId,
                         code_source = 'snomed') |>
        dplyr::left_join(snomed_terms, by = 'code'),
      rows_supplied |>
        dplyr::distinct(variable, read2_code, read2_term) |>
        dplyr::transmute(variable, code = read2_code,
                         code_source = 'read2', description = read2_term) |>
        dplyr::mutate(
          code = dplyr::if_else(
            nchar(code) == 7L,
            stringr::str_sub(code, 1L, 5L),
            code
          )
        ) |>
        # truncation can collapse two 7-char codes that shared a 5-char prefix
        # but had different terms; keep one row per (variable, code) so the
        # read2 slice has no duplicate codes (description chosen arbitrarily).
        dplyr::distinct(variable, code, .keep_all = TRUE),
      rows_supplied |>
        dplyr::distinct(variable, icd10_code) |>
        # strip the decimal dot so icd10 codes in `$lookup` match the dotless
        # form used in the per-disease .csv outputs and in `icd10_lkp.ALT_CODE`
        # (which the description fill below joins on).
        dplyr::transmute(variable,
                         code = stringr::str_remove(icd10_code, '\\.'),
                         code_source = 'icd10', description = NA_character_)
    )

    if (include_ctv_opcs4) {
      long <- dplyr::bind_rows(
        long,
        rows_supplied |>
          dplyr::distinct(variable, ctv3_concept) |>
          dplyr::transmute(variable, code = ctv3_concept,
                           code_source = 'ctv3', description = NA_character_),
        rows_supplied |>
          dplyr::distinct(variable, opcs4_code) |>
          dplyr::transmute(variable, code = opcs4_code,
                           code_source = 'opcs4', description = NA_character_),
        rows_supplied |>
          dplyr::distinct(variable, ctv3_simple) |>
          dplyr::transmute(variable, code = ctv3_simple,
                           code_source = 'ctv3simple',
                           description = NA_character_)
      )
    }

    # optional: fill icd10 / ctv3 / ctv3simple descriptions from all_lkps_maps_v4.xlsx.
    if (!is.null(lkps_maps)) {
      # one lookup for both ctv3 and ctv3simple
      ctv3_lkp <- readxl::read_excel(lkps_maps, sheet = 'read_ctv3_lkp',
                                     col_types = 'text') |>
        dplyr::transmute(code = read_code, desc_new = term_description) |>
        dplyr::distinct(code, .keep_all = TRUE)
      read2_lkp <- readxl::read_excel(lkps_maps, sheet = 'read_v2_lkp',
                                     col_types = 'text') |>
        dplyr::transmute(code = read_code, desc_new = term_description) |>
        # just in case there are 7 characters in a future version
        dplyr::mutate(
          code = dplyr::if_else(
            nchar(code) == 7L,
            stringr::str_sub(code, 1L, 5L),
            code
          )
        ) |>
        dplyr::distinct(code, .keep_all = TRUE)
      lkp_desc <- dplyr::bind_rows(
        readxl::read_excel(lkps_maps, sheet = 'icd10_lkp',
                           col_types = 'text') |>
          dplyr::transmute(code = stringr::str_remove(ALT_CODE, '\\.'),
                           code_source = 'icd10', desc_new = DESCRIPTION) |>
          dplyr::distinct(code, code_source, .keep_all = TRUE),
        dplyr::mutate(ctv3_lkp, code_source = 'ctv3'),
        dplyr::mutate(ctv3_lkp, code_source = 'ctv3simple'),
        dplyr::mutate(read2_lkp, code_source = 'read2')
      )
      long <- long |>
        dplyr::left_join(lkp_desc, by = c('code', 'code_source')) |>
        dplyr::mutate(description = dplyr::coalesce(description, desc_new)) |>
        dplyr::select(code, code_source, description, variable)
    }

    # optional: derive icd9 rows from existing icd10 / read2 / ctv3 / ctv3simple
    # rows in `long` via icd9_icd10, read_v2_icd9, and read_ctv3_icd9 sheets of
    # all_lkps_maps_v4.xlsx. Descriptions filled from icd9_lkp.
    if (icd9_include) {
      icd10_to_icd9 <- readxl::read_excel(lkps_maps, sheet = 'icd9_icd10',
                                          col_types = 'text') |>
        dplyr::transmute(code = stringr::str_remove_all(ICD10, '\\.'),
                         icd9 = ICD9) |>
        dplyr::filter(!is.na(code), !is.na(icd9)) |>
        dplyr::distinct()

      # only single-code rows; ranges excluded
      read2_to_icd9 <- readxl::read_excel(lkps_maps, sheet = 'read_v2_icd9',
                                          col_types = 'text') |>
        dplyr::filter(icd9_code_def == '1') |>
        dplyr::transmute(code = read_code, icd9 = icd9_code) |>
        dplyr::filter(!is.na(code), !is.na(icd9)) |>
        dplyr::mutate(
          code = dplyr::if_else(
            nchar(code) == 7L,
            stringr::str_sub(code, 1L, 5L),
            code
          )
        ) |>
        dplyr::distinct()

      ctv3_to_icd9 <- readxl::read_excel(lkps_maps, sheet = 'read_ctv3_icd9',
                                         col_types = 'text') |>
        dplyr::transmute(code = read_code, icd9 = icd9_code) |>
        dplyr::filter(!is.na(code), !is.na(icd9)) |>
        dplyr::distinct()

      icd9_desc <- readxl::read_excel(lkps_maps, sheet = 'icd9_lkp',
                                      col_types = 'text') |>
        dplyr::transmute(icd9 = ICD9, description = DESCRIPTION_ICD9) |>
        dplyr::distinct(icd9, .keep_all = TRUE)

      icd9_from_icd10 <- long |>
        dplyr::filter(code_source == 'icd10') |>
        dplyr::inner_join(icd10_to_icd9, by = 'code',
                          relationship = 'many-to-many') |>
        dplyr::transmute(variable, code = icd9, code_source = 'icd9')

      # read2 codes in `long` are already 5-char (truncated in the read2 slice
      # of the long-build above); `read_v2_icd9.read_code` is also 5-char, so
      # a direct join suffices.
      icd9_from_read2 <- long |>
        dplyr::filter(code_source == 'read2') |>
        dplyr::inner_join(read2_to_icd9, by = 'code',
                          relationship = 'many-to-many') |>
        dplyr::transmute(variable, code = icd9, code_source = 'icd9')

      icd9_from_ctv3 <- long |>
        dplyr::filter(code_source %in% c('ctv3', 'ctv3simple')) |>
        dplyr::inner_join(ctv3_to_icd9, by = c('code' = 'code'),
                          relationship = 'many-to-many') |>
        dplyr::transmute(variable, code = icd9, code_source = 'icd9')

      icd9_rows <- dplyr::bind_rows(
          icd9_from_icd10, icd9_from_read2, icd9_from_ctv3
        ) |>
        dplyr::left_join(icd9_desc, by = c('code' = 'icd9')) |>
        dplyr::select(code, code_source, description, variable)

      long <- dplyr::bind_rows(long, icd9_rows)
    }

    long <- long |>
      dplyr::filter(!is.na(code)) |>
      dplyr::distinct(code, code_source, variable, .keep_all = TRUE) |>
      dplyr::select(code, code_source, description, variable)

  } else {
    # no diagnoses of interest supplied -> empty lookup (schema preserved)
    long <- tibble::tibble(code = character(), code_source = character(),
                           description = character(), variable = character())
  }

  return(list(lookup = long, csv_files = csv_files))

}
