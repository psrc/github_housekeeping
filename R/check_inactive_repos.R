parse_codeowners_users <- function(codeowners_content) {
  lines <- unlist(strsplit(codeowners_content %||% "", "\n", fixed = TRUE))
  lines <- stringr::str_trim(lines)
  lines <- lines[lines != ""]
  lines <- lines[!startsWith(lines, "#")]
  lines <- lines[startsWith(lines, "*")]

  if (length(lines) == 0) {
    return(character())
  }

  usernames <- stringr::str_extract_all(lines, "@[A-Za-z0-9._-]+")
  usernames <- unlist(usernames, use.names = FALSE)

  unique(sub("^@", "", usernames))
}

build_inactivity_issue_body <- function(codeowners, last_commit_date, last_merged_pr_date, lookback_months) {
  mentions <- paste(sprintf("@%s", codeowners), collapse = " ")
  commit_text <- if (is.na(last_commit_date)) "No commits found" else format(last_commit_date, "%Y-%m-%d")
  pr_text <- if (is.na(last_merged_pr_date)) "No merged pull requests found" else format(last_merged_pr_date, "%Y-%m-%d")

  paste(
    "This repository appears inactive based on the organization governance policy.",
    "",
    sprintf("- Last commit: %s", commit_text),
    sprintf("- Last merged pull request: %s", pr_text),
    sprintf("- Inactivity window: %s months", lookback_months),
    "",
    sprintf("CODEOWNERS: %s", mentions),
    "",
    "Please confirm whether this repository should be archived, actively maintained, or updated with current ownership.",
    sep = "\n"
  )
}

flag_inactive_repo <- function(repo_info, config, codeowners_content, dry_run = FALSE) {
  owner <- repo_info$owner$login
  repo <- repo_info$name
  branch <- repo_info$default_branch
  codeowners <- parse_codeowners_users(codeowners_content)

  if (length(codeowners) == 0) {
    return(list(status = "skipped", reason = "no_codeowners_users"))
  }

  lookback_cutoff <- months_ago_datetime(config$lookback_months)
  last_commit_date <- get_last_meaningful_commit_date(owner, repo, config = config, branch = branch)
  last_merged_pr_date <- get_last_meaningful_merged_pr_date(owner, repo, config = config)

  is_inactive <- (is.na(last_commit_date) || last_commit_date < lookback_cutoff) &&
    (is.na(last_merged_pr_date) || last_merged_pr_date < lookback_cutoff)

  if (!is_inactive) {
    return(list(status = "skipped", reason = "active"))
  }

  issue_title <- "Repository inactivity check"
  existing_issue <- find_open_issue_by_title(owner, repo, issue_title)
  if (!is.null(existing_issue)) {
    return(list(status = "skipped", reason = "existing_issue"))
  }

  create_issue(
    owner = owner,
    repo = repo,
    title = issue_title,
    body = build_inactivity_issue_body(
      codeowners = codeowners,
      last_commit_date = last_commit_date,
      last_merged_pr_date = last_merged_pr_date,
      lookback_months = config$lookback_months
    ),
    dry_run = dry_run
  )

  list(status = "flagged", owners = codeowners)
}
