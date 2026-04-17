#!/usr/bin/env Rscript

# Build and maintain an internal GitHub repository catalog for an organization.
# Outputs: catalog.csv, catalog.json, README.md, catalog.wiki

# ---------------------------
# Configurable inputs (defaults)
# ---------------------------
org_name <- "psrc"
output_dir <- "data/"
include_archived <- FALSE
languages_filter <- NULL           # e.g. c("R", "Python")
max_repos <- NULL                  # e.g. 20 for testing

# Optional performance toggles
fetch_topics <- TRUE
fetch_contributors <- TRUE
contributors_exact_count <- FALSE  # TRUE may add extra API calls per repo
fetch_readme <- TRUE
fetch_features <- TRUE

# API behavior
github_api_base_url <- "https://api.github.com"
per_page <- 100
request_delay_seconds <- 0.15
max_retries <- 4

# ---------------------------
# Dependencies
# ---------------------------
required_pkgs <- c(
  "httr2",
  "jsonlite",
  "dplyr",
  "purrr",
  "stringr",
  "tibble",
  "readr"
)
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Missing required packages: ", paste(missing_pkgs, collapse = ", "),
    "\nInstall with: install.packages(c(", paste(sprintf("'%s'", missing_pkgs), collapse = ", "), "))"
  )
}

`%||%` <- function(left, right) {
  if (is.null(left) || length(left) == 0) return(right)
  left
}

log_info <- function(...) message(sprintf("[INFO] %s", paste(..., collapse = " ")))
log_warn <- function(...) message(sprintf("[WARN] %s", paste(..., collapse = " ")))

require_github_pat <- function(env_var = "GITHUB_PAT") {
  pat <- Sys.getenv(env_var, unset = "")
  if (!nzchar(pat)) {
    stop(
      sprintf("%s is not set. Export %s with a GitHub Personal Access Token (PAT) before running.", env_var, env_var)
    )
  }
  pat
}

# ---------------------------
# GitHub API helpers (httr2)
# ---------------------------
parse_link_header <- function(link_header) {
  # Returns a named character vector of rel => url
  if (!nzchar(link_header %||% "")) return(character())

  parts <- strsplit(link_header, ",", fixed = TRUE)[[1]]
  parts <- trimws(parts)

  rels <- purrr::map(parts, function(part) {
    # <url>; rel="next"
    m_url <- stringr::str_match(part, "<([^>]+)>")[, 2]
    m_rel <- stringr::str_match(part, "rel=\"([^\"]+)\"")[, 2]
    if (is.na(m_url) || is.na(m_rel)) return(NULL)
    setNames(m_url, m_rel)
  })

  rels <- rels[!vapply(rels, is.null, logical(1))]
  if (length(rels) == 0) return(character())

  out <- unlist(rels, use.names = TRUE)
  out
}

extract_page_from_url <- function(url) {
  if (!nzchar(url %||% "")) return(NA_integer_)
  m <- stringr::str_match(url, "[?&]page=([0-9]+)")[, 2]
  if (is.na(m)) return(NA_integer_)
  as.integer(m)
}

is_rate_limit_response <- function(resp) {
  if (is.null(resp)) return(FALSE)
  status <- httr2::resp_status(resp)
  if (status != 403) return(FALSE)

  remaining <- suppressWarnings(as.integer(httr2::resp_header(resp, "x-ratelimit-remaining")))
  if (!is.na(remaining) && remaining <= 0) return(TRUE)

  # Fallback: check message body
  body <- tryCatch(httr2::resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)
  msg <- tolower(body$message %||% "")
  grepl("rate limit", msg, fixed = TRUE)
}

rate_limit_wait_seconds <- function(resp) {
  reset <- suppressWarnings(as.numeric(httr2::resp_header(resp, "x-ratelimit-reset")))
  if (is.na(reset)) return(NA_real_)
  now <- as.numeric(Sys.time())
  max(0, reset - now + 2)
}

github_request <- function(path, query = list(), pat = NULL, accept = "application/vnd.github+json") {
  pat <- pat %||% require_github_pat()

  req <- httr2::request(github_api_base_url) |>
    httr2::req_url_path_append(path) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_headers(
      Accept = accept,
      Authorization = paste("token", pat),
      `X-GitHub-Api-Version` = "2022-11-28",
      `User-Agent` = "psrc-code-catalog-script"
    ) |>
    # Prevent httr2 from throwing on HTTP 4xx/5xx; we handle status codes explicitly.
    httr2::req_error(is_error = function(resp) FALSE)

  attempt <- 1
  repeat {
    resp <- tryCatch(
      httr2::req_perform(req),
      error = function(e) e
    )

    if (inherits(resp, "error")) {
      if (attempt >= max_retries) stop(resp)
      wait <- 2 ^ (attempt - 1)
      log_warn("Request error, retrying in", wait, "seconds:", conditionMessage(resp))
      Sys.sleep(wait)
      attempt <- attempt + 1
      next
    }

    status <- httr2::resp_status(resp)

    # Rate limit handling
    if (is_rate_limit_response(resp)) {
      wait <- rate_limit_wait_seconds(resp)
      if (!is.na(wait) && wait <= 600) {
        log_warn("GitHub rate limit hit; waiting", round(wait), "seconds until reset")
        Sys.sleep(wait)
        attempt <- attempt + 1
        next
      }

      stop("GitHub API rate limit exceeded; try again later or use a token with higher limits.")
    }

    # Retry on transient 5xx
    if (status >= 500 && status <= 599) {
      if (attempt >= max_retries) return(resp)
      wait <- 2 ^ (attempt - 1)
      log_warn("GitHub 5xx response, retrying in", wait, "seconds (status", status, ")")
      Sys.sleep(wait)
      attempt <- attempt + 1
      next
    }

    return(resp)
  }
}

github_get_json <- function(path, query = list(), pat = NULL, accept = "application/vnd.github+json") {
  resp <- github_request(path = path, query = query, pat = pat, accept = accept)
  status <- httr2::resp_status(resp)

  if (status == 204) return(list())

  if (status >= 200 && status <= 299) {
    httr2::resp_body_json(resp, simplifyVector = TRUE)
  } else {
    body <- tryCatch(httr2::resp_body_json(resp, simplifyVector = TRUE), error = function(e) list())
    msg <- body$message %||% paste("HTTP", status)
    stop(sprintf("GitHub API error (%s): %s", status, msg))
  }
}

github_get_paginated <- function(path, query = list(), pat = NULL, accept = "application/vnd.github+json") {
  results <- list()
  page <- 1

  repeat {
    resp <- github_request(
      path = path,
      query = c(query, list(per_page = per_page, page = page)),
      pat = pat,
      accept = accept
    )

    status <- httr2::resp_status(resp)
    if (!(status >= 200 && status <= 299)) {
      body <- tryCatch(httr2::resp_body_json(resp, simplifyVector = TRUE), error = function(e) list())
      msg <- body$message %||% paste("HTTP", status)
      stop(sprintf("GitHub API error (%s): %s", status, msg))
    }

    items <- httr2::resp_body_json(resp, simplifyVector = TRUE)
    if (length(items) == 0) break

    if (is.data.frame(items)) {
      results[[length(results) + 1]] <- items
      n_items <- nrow(items)
    } else if (is.list(items)) {
      results <- c(results, items)
      n_items <- length(items)
    } else {
      break
    }

    link_header <- httr2::resp_header(resp, "link") %||% ""
    links <- parse_link_header(link_header)

    if (!("next" %in% names(links))) break

    page <- page + 1
    if (request_delay_seconds > 0) Sys.sleep(request_delay_seconds)

    # Safety: break if response returns fewer than per_page and no link header
    if (n_items < per_page && !nzchar(link_header)) break
  }

  if (length(results) == 0) return(list())

  if (all(vapply(results, is.data.frame, logical(1)))) {
    dplyr::bind_rows(results)
  } else {
    results
  }
}

safe_call <- function(expr, on_error = NULL, context = NULL) {
  tryCatch(
    expr,
    error = function(e) {
      if (!is.null(context)) log_warn(context, "-", conditionMessage(e))
      on_error
    }
  )
}

# ---------------------------
# Core functions
# ---------------------------
get_repos <- function(org_name, github_pat = NULL, include_archived = FALSE, languages_filter = NULL, max_repos = NULL) {
  repos <- safe_call(
    github_get_paginated(
      path = c("orgs", org_name, "repos"),
      query = list(type = "all", sort = "full_name", direction = "asc"),
      pat = github_pat
    ),
    on_error = tibble::tibble(),
    context = sprintf("Failed to list repos for org '%s'", org_name)
  )

  if (nrow(repos) == 0) {
    return(tibble::tibble())
  }

  repos <- repos |>
    dplyr::mutate(
      archived = as.logical(.data$archived %||% FALSE),
      language = as.character(.data$language %||% NA_character_),
      name = as.character(.data$name),
      full_name = as.character(.data$full_name),
      html_url = as.character(.data$html_url),
      description = as.character(.data$description %||% ""),
      created_at = as.character(.data$created_at %||% NA_character_),
      updated_at = as.character(.data$updated_at %||% NA_character_),
      default_branch = as.character(.data$default_branch %||% "")
    )

  if (!isTRUE(include_archived)) {
    repos <- dplyr::filter(repos, !.data$archived)
  }

  if (!is.null(languages_filter) && length(languages_filter) > 0) {
    repos <- dplyr::filter(repos, .data$language %in% languages_filter)
  }

  if (!is.null(max_repos) && is.finite(max_repos) && max_repos > 0) {
    repos <- dplyr::slice_head(repos, n = as.integer(max_repos))
  }

  repos
}

get_repo_topics <- function(owner, repo, github_pat = NULL) {
  out <- safe_call(
    github_get_json(
      path = c("repos", owner, repo, "topics"),
      query = list(),
      pat = github_pat
    ),
    on_error = list(names = character()),
    context = sprintf("Topics failed for %s/%s", owner, repo)
  )

  topics <- out$names %||% character()
  topics <- as.character(topics)
  topics[nzchar(topics)]
}

get_contributors <- function(owner, repo, github_pat = NULL, exact_count = FALSE) {
  # Returns list(top_contributors = character(), contributor_count = integer|NA)
  # Uses contributors endpoint order (by contributions).
  path <- c("repos", owner, repo, "contributors")

  resp <- safe_call(
    github_request(
      path = path,
      query = list(per_page = per_page, page = 1, anon = "true"),
      pat = github_pat
    ),
    on_error = NULL,
    context = sprintf("Contributors failed for %s/%s", owner, repo)
  )

  if (is.null(resp)) {
    return(list(top_contributors = character(), contributor_count = NA_integer_))
  }

  status <- httr2::resp_status(resp)
  if (!(status >= 200 && status <= 299)) {
    return(list(top_contributors = character(), contributor_count = NA_integer_))
  }

  # For repos with no commits/content, GitHub may return an empty body and/or omit
  # the Content-Type header. Treat that as "no contributors" without warning.
  if (identical(status, 204L)) {
    return(list(top_contributors = character(), contributor_count = 0L))
  }

  content_type <- httr2::resp_header(resp, "content-type")
  if (is.na(content_type) || !grepl("json", content_type, ignore.case = TRUE)) {
    raw_body <- tryCatch(httr2::resp_body_raw(resp), error = function(e) raw(0))
    if (length(raw_body) == 0) {
      return(list(top_contributors = character(), contributor_count = 0L))
    }
  }

  items <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = TRUE),
    error = function(e) NULL
  )

  if (is.null(items)) {
    return(list(top_contributors = character(), contributor_count = NA_integer_))
  }

  if (is.data.frame(items)) {
    logins <- as.character(items$login %||% character())
  } else if (is.list(items)) {
    logins <- as.character(vapply(items, function(x) x$login %||% "", character(1)))
  } else {
    logins <- character()
  }

  logins <- logins[nzchar(logins)]
  top3 <- head(logins, 3)

  link_header <- httr2::resp_header(resp, "link") %||% ""
  links <- parse_link_header(link_header)

  if (!("last" %in% names(links))) {
    return(list(top_contributors = top3, contributor_count = as.integer(length(logins))))
  }

  last_page <- extract_page_from_url(links[["last"]])
  if (is.na(last_page) || last_page < 2) {
    return(list(top_contributors = top3, contributor_count = as.integer(length(logins))))
  }

  if (!isTRUE(exact_count)) {
    return(list(top_contributors = top3, contributor_count = NA_integer_))
  }

  last_resp <- safe_call(
    github_request(
      path = path,
      query = list(per_page = per_page, page = last_page, anon = "true"),
      pat = github_pat
    ),
    on_error = NULL,
    context = sprintf("Contributors last-page failed for %s/%s", owner, repo)
  )

  if (is.null(last_resp) || httr2::resp_status(last_resp) < 200 || httr2::resp_status(last_resp) > 299) {
    return(list(top_contributors = top3, contributor_count = NA_integer_))
  }

  last_items <- safe_call(
    httr2::resp_body_json(last_resp, simplifyVector = TRUE),
    on_error = list(),
    context = sprintf("Contributors last-page JSON parse failed for %s/%s", owner, repo)
  )

  last_n <- if (is.data.frame(last_items)) nrow(last_items) else if (is.list(last_items)) length(last_items) else 0
  count <- as.integer((last_page - 1L) * per_page + last_n)

  list(top_contributors = top3, contributor_count = count)
}

get_readme <- function(owner, repo, github_pat = NULL) {
  # Returns readme text (character) or NULL
  resp <- safe_call(
    github_request(
      path = c("repos", owner, repo, "readme"),
      query = list(),
      pat = github_pat
    ),
    on_error = NULL,
    context = sprintf("README request failed for %s/%s", owner, repo)
  )

  if (is.null(resp)) return(NULL)

  status <- httr2::resp_status(resp)
  if (status == 404) return(NULL)
  if (!(status >= 200 && status <= 299)) return(NULL)

  body <- safe_call(
    httr2::resp_body_json(resp, simplifyVector = TRUE),
    on_error = NULL,
    context = sprintf("README JSON parse failed for %s/%s", owner, repo)
  )

  if (is.null(body)) return(NULL)

  enc <- tolower(body$encoding %||% "")
  content <- body$content %||% ""
  if (!nzchar(content)) return(NULL)

  if (identical(enc, "base64")) {
    encoded <- gsub("\\s", "", content)
    safe_call(
      rawToChar(jsonlite::base64_dec(encoded)),
      on_error = NULL,
      context = sprintf("README base64 decode failed for %s/%s", owner, repo)
    )
  } else {
    as.character(content)
  }
}

strip_markdown_code_blocks <- function(text) {
  if (!nzchar(text %||% "")) return("")
  # Remove fenced code blocks (```...```)
  text <- stringr::str_replace_all(text, "(?s)```.*?```", "")
  # Remove HTML comments
  text <- stringr::str_replace_all(text, "(?s)<!--.*?-->", "")
  text
}

parse_readme <- function(readme_text) {
  # Returns list(has_reuse_guide = logical(1), readme_summary = character(1))
  if (!nzchar(readme_text %||% "")) {
    return(list(has_reuse_guide = FALSE, readme_summary = ""))
  }

  text <- strip_markdown_code_blocks(readme_text)

  has_reuse <- grepl(
    pattern = "(?im)^(#{1,6}\\s*)?Reuse Guide\\b",
    x = text,
    perl = TRUE
  ) || grepl("(?i)\\breuse guide\\b", text, perl = TRUE)

  lines <- unlist(strsplit(text, "\\r?\\n", perl = TRUE), use.names = FALSE)
  lines <- stringr::str_trim(lines)

  # Drop leading empty lines and common badge lines
  drop_badge_line <- function(x) {
    if (!nzchar(x)) return(TRUE)
    if (grepl("^#", x)) return(TRUE)
    if (grepl("^\\[!\\[", x)) return(TRUE)       # [![...]
    if (grepl("^!\\[", x)) return(TRUE)          # ![...]
    if (grepl("^<", x)) return(TRUE)             # HTML tags
    FALSE
  }

  # Find first paragraph-like block
  idx <- 1
  while (idx <= length(lines) && drop_badge_line(lines[[idx]])) idx <- idx + 1

  if (idx > length(lines)) {
    return(list(has_reuse_guide = has_reuse, readme_summary = ""))
  }

  para <- character()
  while (idx <= length(lines) && nzchar(lines[[idx]])) {
    if (!drop_badge_line(lines[[idx]])) {
      para <- c(para, lines[[idx]])
    }
    idx <- idx + 1
  }

  summary <- stringr::str_squish(paste(para, collapse = " "))
  summary <- stringr::str_replace_all(summary, "\\|", "\\\\|")

  if (nchar(summary) > 400) summary <- paste0(substr(summary, 1, 397), "...")

  list(has_reuse_guide = isTRUE(has_reuse), readme_summary = summary)
}

detect_repo_features <- function(owner, repo, default_branch = NULL, github_pat = NULL) {
  # Detect presence of /R/, /src/, *.Rproj, DESCRIPTION via root /contents listing.
  # Returns list(is_r_package = logical(1)) and may be extended later.

  query <- list()
  if (nzchar(default_branch %||% "")) query$ref <- default_branch

  resp <- safe_call(
    github_request(
      path = c("repos", owner, repo, "contents"),
      query = query,
      pat = github_pat
    ),
    on_error = NULL,
    context = sprintf("Contents request failed for %s/%s", owner, repo)
  )

  if (is.null(resp)) {
    return(list(is_r_package = FALSE))
  }

  status <- httr2::resp_status(resp)
  if (status == 404 || status == 409) {
    return(list(is_r_package = FALSE))
  }
  if (!(status >= 200 && status <= 299)) {
    return(list(is_r_package = FALSE))
  }

  items <- safe_call(
    httr2::resp_body_json(resp, simplifyVector = TRUE),
    on_error = NULL,
    context = sprintf("Contents JSON parse failed for %s/%s", owner, repo)
  )

  if (is.null(items) || length(items) == 0) {
    return(list(is_r_package = FALSE))
  }

  if (is.data.frame(items)) {
    names_vec <- as.character(items$name %||% character())
    type_vec <- as.character(items$type %||% character())
  } else if (is.list(items)) {
    names_vec <- as.character(vapply(items, function(x) x$name %||% "", character(1)))
    type_vec <- as.character(vapply(items, function(x) x$type %||% "", character(1)))
  } else {
    return(list(is_r_package = FALSE))
  }

  names_vec <- names_vec[nzchar(names_vec)]
  type_vec <- type_vec[seq_along(names_vec)]

  has_r_dir <- any(names_vec == "R" & type_vec == "dir")
  has_src_dir <- any(names_vec == "src" & type_vec == "dir")
  has_description <- any(names_vec == "DESCRIPTION")
  has_rproj <- any(stringr::str_detect(names_vec, "\\.Rproj$"))

  is_r_package <- isTRUE(has_description) && (isTRUE(has_r_dir) || isTRUE(has_src_dir) || isTRUE(has_rproj))

  list(
    is_r_package = isTRUE(is_r_package)
  )
}

build_catalog <- function(
  org_name,
  output_dir,
  github_pat = Sys.getenv("GITHUB_PAT", unset = ""),
  include_archived = FALSE,
  languages_filter = NULL,
  max_repos = NULL,
  fetch_topics = TRUE,
  fetch_contributors = TRUE,
  contributors_exact_count = FALSE,
  fetch_readme = TRUE,
  fetch_features = TRUE
) {
  if (!nzchar(org_name %||% "")) stop("org_name must be provided")
  if (!nzchar(github_pat %||% "")) require_github_pat("GITHUB_PAT")

  repos <- get_repos(
    org_name = org_name,
    github_pat = github_pat,
    include_archived = include_archived,
    languages_filter = languages_filter,
    max_repos = max_repos
  )

  if (nrow(repos) == 0) {
    log_warn("No repositories found (or insufficient access).")
    return(tibble::tibble())
  }

  log_info("Cataloging", nrow(repos), "repositories")

  catalog_rows <- purrr::pmap(
    list(
      owner = rep(org_name, nrow(repos)),
      repo = repos$name,
      full_name = repos$full_name,
      description = repos$description,
      html_url = repos$html_url,
      created_at = repos$created_at,
      updated_at = repos$updated_at,
      archived = repos$archived,
      language = repos$language,
      default_branch = repos$default_branch
    ),
    function(owner, repo, full_name, description, html_url, created_at, updated_at, archived, language, default_branch) {
      if (request_delay_seconds > 0) Sys.sleep(request_delay_seconds)

      topics <- character()
      if (isTRUE(fetch_topics)) {
        topics <- get_repo_topics(owner = owner, repo = repo, github_pat = github_pat)
      }

      contrib <- list(top_contributors = character(), contributor_count = NA_integer_)
      if (isTRUE(fetch_contributors)) {
        contrib <- get_contributors(
          owner = owner,
          repo = repo,
          github_pat = github_pat,
          exact_count = isTRUE(contributors_exact_count)
        )
      }

      readme_text <- NULL
      parsed <- list(has_reuse_guide = FALSE, readme_summary = "")
      if (isTRUE(fetch_readme)) {
        readme_text <- get_readme(owner = owner, repo = repo, github_pat = github_pat)
        parsed <- parse_readme(readme_text %||% "")
      }

      features <- list(is_r_package = FALSE)
      if (isTRUE(fetch_features)) {
        features <- detect_repo_features(
          owner = owner,
          repo = repo,
          default_branch = default_branch,
          github_pat = github_pat
        )
      }

      tibble::tibble(
        repo_name = repo,
        full_name = full_name,
        description = description %||% "",
        topics = paste(topics, collapse = ";"),
        language = language %||% NA_character_,
        created_at = created_at %||% NA_character_,
        updated_at = updated_at %||% NA_character_,
        archived = as.logical(archived %||% FALSE),
        contributors = paste(contrib$top_contributors %||% character(), collapse = ";"),
        contributor_count = as.integer(contrib$contributor_count %||% NA_integer_),
        has_reuse_guide = as.logical(parsed$has_reuse_guide %||% FALSE),
        readme_summary = as.character(parsed$readme_summary %||% ""),
        is_r_package = as.logical(features$is_r_package %||% FALSE),
        url = html_url %||% ""
      )
    }
  )

  catalog <- dplyr::bind_rows(catalog_rows) |>
    dplyr::arrange(dplyr::desc(.data$updated_at), .data$full_name)

  attr(catalog, "org_name") <- org_name

  catalog
}

write_outputs <- function(catalog, output_dir) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  csv_path <- file.path(output_dir, "catalog.csv")
  json_path <- file.path(output_dir, "catalog.json")
  readme_path <- file.path(output_dir, "README.md")
  wiki_path <- file.path(output_dir, "catalog.wiki")

  readr::write_csv(catalog, csv_path, na = "")

  # Structured JSON (topics/contributors as vectors)
  json_repos <- purrr::pmap(
    catalog,
    function(repo_name, full_name, description, topics, language, created_at, updated_at, archived,
             contributors, contributor_count, has_reuse_guide, readme_summary, is_r_package, url) {
      list(
        repo_name = repo_name,
        full_name = full_name,
        description = description,
        topics = if (nzchar(topics %||% "")) strsplit(topics, ";", fixed = TRUE)[[1]] else character(),
        language = language,
        created_at = created_at,
        updated_at = updated_at,
        archived = isTRUE(archived),
        contributors = if (nzchar(contributors %||% "")) strsplit(contributors, ";", fixed = TRUE)[[1]] else character(),
        contributor_count = if (is.na(contributor_count)) NULL else as.integer(contributor_count),
        has_reuse_guide = isTRUE(has_reuse_guide),
        readme_summary = readme_summary,
        is_r_package = isTRUE(is_r_package),
        url = url
      )
    }
  )

  org <- attr(catalog, "org_name") %||% "psrc"

  payload <- list(
    org_name = org,
    generated_at_utc = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    repo_count = nrow(catalog),
    repos = json_repos
  )

  jsonlite::write_json(payload, json_path, pretty = TRUE, auto_unbox = TRUE, null = "null")

  md_escape <- function(x) {
    x <- x %||% ""
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- stringr::str_replace_all(x, "\\r?\\n", " ")
    x <- stringr::str_squish(x)
    x <- stringr::str_replace_all(x, "\\|", "\\\\|")
    x
  }

  fmt_flags <- function(is_r_package, has_reuse_guide) {
    flags <- character()
    if (isTRUE(is_r_package)) flags <- c(flags, "R package")
    if (isTRUE(has_reuse_guide)) flags <- c(flags, "Has Reuse Guide")
    if (length(flags) == 0) return("")
    paste(flags, collapse = ", ")
  }

  updated_at <- payload$generated_at_utc
  total <- nrow(catalog)

  top_n <- min(15, total)
  top_tbl <- catalog |>
    dplyr::slice_head(n = top_n) |>
    dplyr::mutate(
      name = sprintf("[%s](%s)", md_escape(.data$full_name), .data$url),
      desc = md_escape(.data$description),
      topics_md = md_escape(.data$topics),
      flags = md_escape(fmt_flags(.data$is_r_package, .data$has_reuse_guide))
    ) |>
    dplyr::select(name, desc, topics_md, language, updated_at, flags)

  languages <- sort(unique(catalog$language %||% NA_character_))
  languages <- languages[!is.na(languages) & nzchar(languages)]

  lang_sections <- purrr::map_chr(languages, function(lang) {
    repos_lang <- catalog |>
      dplyr::filter(.data$language == lang) |>
      dplyr::mutate(
        name = sprintf("[%s](%s)", md_escape(.data$full_name), .data$url),
        desc = md_escape(.data$description),
        topics_md = md_escape(.data$topics),
        flags = md_escape(fmt_flags(.data$is_r_package, .data$has_reuse_guide))
      ) |>
      dplyr::select(name, desc, topics_md, flags)

    if (nrow(repos_lang) == 0) return("")

    header <- paste0("## ", md_escape(lang), " (", nrow(repos_lang), ")\n")

    table_header <- "| Repository | Description | Topics | Flags |\n|---|---|---|---|\n"

    table_rows <- apply(repos_lang, 1, function(row) {
      paste0("| ", row[["name"]], " | ", row[["desc"]], " | ", row[["topics_md"]], " | ", row[["flags"]], " |\n")
    })

    paste0(header, "\n", table_header, paste0(table_rows, collapse = ""), "\n")
  })

  # Top repositories table
  top_md <- paste0(
    "| Repository | Description | Topics | Language | Updated | Flags |\n",
    "|---|---|---|---|---|---|\n",
    paste0(
      apply(top_tbl, 1, function(row) {
        paste0(
          "| ", row[["name"]],
          " | ", row[["desc"]],
          " | ", row[["topics_md"]],
          " | ", md_escape(row[["language"]]),
          " | ", md_escape(row[["updated_at"]]),
          " | ", row[["flags"]],
          " |\n"
        )
      }),
      collapse = ""
    )
  )

  readme <- paste0(
    "# PSRC Code Catalog\n\n",
    "Internal repository catalog for the `", md_escape(org), "` GitHub organization.\n\n",
    "- **Total repositories:** ", total, "\n",
    "- **Last updated (UTC):** ", updated_at, "\n",
    "- **Artifacts:** `catalog.csv`, `catalog.json`, `catalog.wiki`\n\n",
    "## Most recently updated\n\n",
    top_md,
    "\n",
    paste0(lang_sections, collapse = "")
  )

  writeLines(readme, readme_path, useBytes = TRUE)

  # ---------------------------
  # MediaWiki table output
  # ---------------------------
  wiki_escape <- function(x) {
    x <- x %||% ""
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- stringr::str_replace_all(x, "\\r?\\n", " ")
    x <- stringr::str_squish(x)
    # Prevent table cell delimiter issues.
    x <- stringr::str_replace_all(x, "\\|", "&#124;")
    x
  }

  wiki_nowiki <- function(x) {
    paste0("<nowiki>", wiki_escape(x), "</nowiki>")
  }

  wiki_external_link <- function(label, url) {
    label <- wiki_escape(label)
    url <- as.character(url %||% "")
    has_url <- !is.na(url) & nzchar(url)
    ifelse(has_url, sprintf("[%s %s]", url, label), label)
  }

  wiki_topics <- function(topics) {
    topics <- as.character(topics %||% "")
    has_topics <- !is.na(topics) & nzchar(topics)
    ifelse(has_topics, stringr::str_replace_all(topics, ";", ", "), "")
  }

  wiki_flags <- function(is_r_package, has_reuse_guide) {
    is_r_package <- !is.na(is_r_package) & is_r_package
    has_reuse_guide <- !is.na(has_reuse_guide) & has_reuse_guide

    flags <- ifelse(is_r_package, "R package", "")
    flags <- ifelse(
      has_reuse_guide & nzchar(flags),
      paste0(flags, ", Has Reuse Guide"),
      ifelse(has_reuse_guide, "Has Reuse Guide", flags)
    )

    flags
  }

  wiki_timestamp <- function(x) {
    if (inherits(x, "POSIXt")) {
      return(format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    }

    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  }

  wiki_tbl <- catalog |>
    dplyr::mutate(
      repo_cell = wiki_external_link(.data$full_name, .data$url),
      desc_cell = wiki_nowiki(.data$description),
      topics_cell = wiki_nowiki(wiki_topics(.data$topics)),
      language_cell = wiki_nowiki(as.character(.data$language)),
      updated_cell = wiki_nowiki(wiki_timestamp(.data$updated_at)),
      flags_cell = wiki_nowiki(wiki_flags(.data$is_r_package, .data$has_reuse_guide))
    ) |>
    dplyr::select(repo_cell, desc_cell, topics_cell, language_cell, updated_cell, flags_cell)

  wiki_rows <- apply(wiki_tbl, 1, function(row) {
    paste0(
      "|-\n",
      "| ", row[["repo_cell"]],
      " || ", row[["desc_cell"]],
      " || ", row[["topics_cell"]],
      " || ", row[["language_cell"]],
      " || ", row[["updated_cell"]],
      " || ", row[["flags_cell"]],
      "\n"
    )
  })

  # Keep the table header separate so it stays as wikitext (not nowiki).
  wiki_table <- paste0(
    "{| class=\"wikitable sortable\"\n",
    "! Repository !! Description !! Topics !! Language !! Updated (UTC) !! Flags\n",
    paste0(wiki_rows, collapse = ""),
    "|}\n"
  )

  wiki_text <- paste0(
    "== PSRC Code Catalog ==\n\n",
    "Internal repository catalog for the ", wiki_escape(org), " GitHub organization.\n\n",
    "; Total repositories: ", total, "\n",
    "; Last updated (UTC): ", updated_at, "\n\n",
    wiki_table
  )

  writeLines(wiki_text, wiki_path, useBytes = TRUE)

  invisible(list(csv = csv_path, json = json_path, readme = readme_path, wiki = wiki_path))
}

# ---------------------------
# Example usage
# ---------------------------
if (sys.nframe() == 0) {
  github_pat <- require_github_pat("GITHUB_PAT")

  catalog <- build_catalog(
    org_name = org_name,
    output_dir = output_dir,
    github_pat = github_pat,
    include_archived = include_archived,
    languages_filter = languages_filter,
    max_repos = max_repos,
    fetch_topics = fetch_topics,
    fetch_contributors = fetch_contributors,
    contributors_exact_count = contributors_exact_count,
    fetch_readme = fetch_readme,
    fetch_features = fetch_features
  )

  write_outputs(catalog, output_dir)
}

# ---------------------------
# GITHUB_PAT permissions (classic PAT scopes)
# ---------------------------
# - read:org  (required to list organization repositories, especially private orgs)
# - repo      (required to read private repositories; for public-only orgs, this can be omitted)
