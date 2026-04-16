`%||%` <- function(left, right) {
  if (is.null(left) || length(left) == 0) {
    return(right)
  }

  left
}

log_info <- function(...) {
  message(sprintf("[INFO] %s", paste(..., collapse = " ")))
}

log_warn <- function(...) {
  message(sprintf("[WARN] %s", paste(..., collapse = " ")))
}

read_config <- function(path) {
  config <- yaml::read_yaml(path)

  config$excluded_users <- config$excluded_users %||% character()
  config$lookback_months <- config$lookback_months %||% 18
  config$min_contribution_threshold <- config$min_contribution_threshold %||% 0.3
  config$require_org_membership <- isTRUE(config$require_org_membership)
  config$include_forks <- isTRUE(config$include_forks)
  config$ignored_activity_paths <- config$ignored_activity_paths %||% c(".github/CODEOWNERS")
  config$ignored_activity_title_patterns <- config$ignored_activity_title_patterns %||% c("(?i)codeowners")

  if (is.null(config$org) || identical(config$org, "YOUR_ORG_NAME")) {
    stop("Set config$config$org to your GitHub organization name in config/config.yaml.")
  }

  config
}

parse_cli_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  parsed <- list(
    config = "config/config.yaml",
    dry_run = FALSE
  )

  index <- 1
  while (index <= length(args)) {
    arg <- args[[index]]

    if (identical(arg, "--dry-run")) {
      parsed$dry_run <- TRUE
    } else if (startsWith(arg, "--config=")) {
      parsed$config <- sub("^--config=", "", arg)
    } else if (identical(arg, "--config") && index < length(args)) {
      index <- index + 1
      parsed$config <- args[[index]]
    } else {
      log_warn("Ignoring unknown argument:", arg)
    }

    index <- index + 1
  }

  parsed
}

require_github_token <- function() {
  token <- Sys.getenv("GH_TOKEN", unset = "")
  if (!nzchar(token)) {
    stop("GH_TOKEN is not set. Export GH_TOKEN before running the script.")
  }
}

months_ago_datetime <- function(months, from = Sys.time()) {
  timestamp <- as.POSIXlt(from, tz = "UTC")
  timestamp$mon <- timestamp$mon - months
  as.POSIXct(timestamp, tz = "UTC")
}

as_github_timestamp <- function(value) {
  format(as.POSIXct(value, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

is_not_found_error <- function(error) {
  grepl("404", conditionMessage(error), fixed = TRUE)
}

is_empty_repo_error <- function(error) {
  message_text <- conditionMessage(error)
  grepl("409", message_text, fixed = TRUE) && grepl("Git Repository is empty", message_text, fixed = TRUE)
}

gh_api <- function(endpoint, ..., .method = "GET", .max_tries = 4) {
  attempt <- 1

  repeat {
    response <- tryCatch(
      gh::gh(
        endpoint,
        ...,
        .method = .method,
        .send_headers = c(Accept = "application/vnd.github+json")
      ),
      error = identity
    )

    if (!inherits(response, "error")) {
      return(response)
    }

    # 404s are not transient (missing file/repo or insufficient access). Don't retry.
    if (is_not_found_error(response)) {
      stop(response)
    }

    # Empty repos (409) are not transient. Don't retry.
    if (is_empty_repo_error(response)) {
      stop(response)
    }

    if (attempt >= .max_tries) {
      stop(response)
    }

    wait_seconds <- 2 ^ (attempt - 1)
    log_warn("GitHub API call failed, retrying in", wait_seconds, "seconds:", conditionMessage(response))
    Sys.sleep(wait_seconds)
    attempt <- attempt + 1
  }
}

gh_paginate <- function(endpoint, ..., per_page = 100) {
  results <- list()
  page <- 1

  repeat {
    response <- gh_api(endpoint, ..., per_page = per_page, page = page)

    if (length(response) == 0) {
      break
    }

    results <- c(results, response)

    if (length(response) < per_page) {
      break
    }

    page <- page + 1
  }

  results
}

list_org_repositories <- function(org, include_forks = FALSE) {
  repos <- gh_paginate("/orgs/{org}/repos", org = org, type = "all")

  Filter(
    f = function(repo) {
      !isTRUE(repo$archived) && (include_forks || !isTRUE(repo$fork))
    },
    x = repos
  )
}

list_org_members <- function(org) {
  members <- gh_paginate("/orgs/{org}/members", org = org)
  unique(vapply(members, function(member) member$login %||% "", character(1)))
}

get_file_content <- function(owner, repo, path, ref = NULL) {
  response <- tryCatch(
    gh_api(
      "/repos/{owner}/{repo}/contents/{path}",
      owner = owner,
      repo = repo,
      path = path,
      ref = ref
    ),
    error = function(error) {
      if (is_not_found_error(error)) {
        return(NULL)
      }

      stop(error)
    }
  )

  if (is.null(response)) {
    return(NULL)
  }

  encoded <- gsub("\\s", "", response$content %||% "")
  rawToChar(jsonlite::base64_dec(encoded))
}

create_file_in_repo <- function(owner, repo, path, content, message, branch, dry_run = FALSE) {
  if (dry_run) {
    return(invisible(NULL))
  }

  gh_api(
    "/repos/{owner}/{repo}/contents/{path}",
    owner = owner,
    repo = repo,
    path = path,
    message = message,
    content = jsonlite::base64_enc(charToRaw(content)),
    branch = branch,
    .method = "PUT"
  )
}

list_commits_since <- function(owner, repo, since, branch = NULL) {
  gh_paginate(
    "/repos/{owner}/{repo}/commits",
    owner = owner,
    repo = repo,
    since = since,
    sha = branch
  )
}

list_repo_contributors <- function(owner, repo) {
  gh_paginate(
    "/repos/{owner}/{repo}/contributors",
    owner = owner,
    repo = repo,
    anon = "false"
  )
}

normalize_repo_path <- function(path) {
  stringr::str_replace_all(path %||% "", "\\\\", "/")
}

paths_match_ignored_activity <- function(paths, ignored_paths) {
  normalized_paths <- unique(normalize_repo_path(paths))
  normalized_paths <- normalized_paths[nzchar(normalized_paths)]
  normalized_ignored <- unique(normalize_repo_path(ignored_paths))
  normalized_ignored <- normalized_ignored[nzchar(normalized_ignored)]

  if (length(normalized_paths) == 0 || length(normalized_ignored) == 0) {
    return(FALSE)
  }

  all(normalized_paths %in% normalized_ignored)
}

text_matches_patterns <- function(text, patterns) {
  if (!nzchar(text) || length(patterns) == 0) {
    return(FALSE)
  }

  any(vapply(patterns, function(pattern) grepl(pattern, text, perl = TRUE), logical(1)))
}

get_commit_details <- function(owner, repo, sha) {
  gh_api(
    "/repos/{owner}/{repo}/commits/{ref}",
    owner = owner,
    repo = repo,
    ref = sha
  )
}

get_pull_request_files <- function(owner, repo, pull_number) {
  gh_paginate(
    "/repos/{owner}/{repo}/pulls/{pull_number}/files",
    owner = owner,
    repo = repo,
    pull_number = pull_number
  )
}

is_ignored_commit_activity <- function(owner, repo, commit, config) {
  title <- commit$commit$message %||% ""
  if (text_matches_patterns(title, config$ignored_activity_title_patterns)) {
    return(TRUE)
  }

  details <- get_commit_details(owner, repo, commit$sha)
  changed_paths <- vapply(details$files %||% list(), function(file) file$filename %||% "", character(1))

  paths_match_ignored_activity(changed_paths, config$ignored_activity_paths)
}

get_last_meaningful_commit_date <- function(owner, repo, config, branch = NULL) {
  commits <- gh_paginate(
    "/repos/{owner}/{repo}/commits",
    owner = owner,
    repo = repo,
    sha = branch
  )

  if (length(commits) == 0) {
    return(as.POSIXct(NA, tz = "UTC"))
  }

  for (commit in commits) {
    if (!is_ignored_commit_activity(owner, repo, commit, config)) {
      return(as.POSIXct(commit$commit$author$date, tz = "UTC"))
    }
  }

  as.POSIXct(NA, tz = "UTC")
}

is_ignored_pull_request_activity <- function(owner, repo, pull, config) {
  title_and_body <- paste(pull$title %||% "", pull$body %||% "", sep = "\n")
  if (text_matches_patterns(title_and_body, config$ignored_activity_title_patterns)) {
    return(TRUE)
  }

  files <- get_pull_request_files(owner, repo, pull$number)
  changed_paths <- vapply(files, function(file) file$filename %||% "", character(1))

  paths_match_ignored_activity(changed_paths, config$ignored_activity_paths)
}

get_last_meaningful_merged_pr_date <- function(owner, repo, config) {
  pulls <- gh_paginate(
    "/repos/{owner}/{repo}/pulls",
    owner = owner,
    repo = repo,
    state = "closed",
    sort = "updated",
    direction = "desc"
  )

  merged_pulls <- Filter(function(pull) !is.null(pull$merged_at), pulls)
  if (length(merged_pulls) == 0) {
    return(as.POSIXct(NA, tz = "UTC"))
  }

  merged_pulls <- merged_pulls[order(vapply(merged_pulls, function(pull) pull$merged_at, character(1)), decreasing = TRUE)]

  for (pull in merged_pulls) {
    if (!is_ignored_pull_request_activity(owner, repo, pull, config)) {
      return(as.POSIXct(pull$merged_at, tz = "UTC"))
    }
  }

  as.POSIXct(NA, tz = "UTC")
}

find_open_issue_by_title <- function(owner, repo, title) {
  issues <- gh_paginate(
    "/repos/{owner}/{repo}/issues",
    owner = owner,
    repo = repo,
    state = "open"
  )

  matching <- Filter(
    f = function(issue) {
      is.null(issue$pull_request) && identical(issue$title %||% "", title)
    },
    x = issues
  )

  if (length(matching) == 0) {
    return(NULL)
  }

  matching[[1]]
}

create_issue <- function(owner, repo, title, body, dry_run = FALSE) {
  if (dry_run) {
    return(invisible(NULL))
  }

  gh_api(
    "/repos/{owner}/{repo}/issues",
    owner = owner,
    repo = repo,
    title = title,
    body = body,
    .method = "POST"
  )
}

repo_slug <- function(repo_info) {
  repo_info$full_name %||% sprintf("%s/%s", repo_info$owner$login, repo_info$name)
}
