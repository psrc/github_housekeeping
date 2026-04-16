script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(script_arg) == 0) normalizePath("R/main.R") else normalizePath(sub("^--file=", "", script_arg[[1]]))
project_root <- dirname(dirname(script_path))

source(file.path(project_root, "R", "github_utils.R"))
source(file.path(project_root, "R", "assign_codeowners.R"))
source(file.path(project_root, "R", "check_inactive_repos.R"))

run_governance <- function(config_path, dry_run = FALSE) {
  require_github_token()

  resolved_config_path <- if (grepl("^([A-Za-z]:|/)", config_path)) config_path else file.path(project_root, config_path)
  config <- read_config(resolved_config_path)

  log_info("Loading organization members for", config$org)
  org_members <- list_org_members(config$org)
  log_info("Loaded", length(org_members), "organization members")

  repos <- list_org_repositories(config$org, include_forks = config$include_forks)
  total_repos <- length(repos)
  log_info("Scanning", total_repos, "repositories")

  created_codeowners <- 0
  flagged_inactive <- 0
  empty_repos <- character()
  no_member_contributor_repos <- character()
  unexpected_error_count <- 0

  for (index in seq_along(repos)) {
    repo_info <- repos[[index]]
    slug <- repo_slug(repo_info)

    tryCatch(
      {
        codeowners_content <- get_file_content(
          owner = repo_info$owner$login,
          repo = repo_info$name,
          path = ".github/CODEOWNERS",
          ref = repo_info$default_branch
        )

        if (is.null(codeowners_content)) {
          result <- create_codeowners_for_repo(
            repo_info = repo_info,
            config = config,
            org_members = org_members,
            dry_run = dry_run
          )

          if (identical(result$status, "created")) {
            created_codeowners <- created_codeowners + 1
          } else if (identical(result$status, "empty_repo")) {
            empty_repos <- c(empty_repos, slug)
          } else if (identical(result$status, "skipped") && identical(result$reason, "no_current_member_contributors")) {
            no_member_contributor_repos <- c(no_member_contributor_repos, slug)
          }
        } else {
          result <- flag_inactive_repo(
            repo_info = repo_info,
            config = config,
            codeowners_content = codeowners_content,
            dry_run = dry_run
          )

          if (identical(result$status, "flagged")) {
            flagged_inactive <- flagged_inactive + 1
          }
        }
      },
      error = function(error) {
        if (is_empty_repo_error(error)) {
          empty_repos <<- c(empty_repos, slug)
          return(invisible(NULL))
        }

        unexpected_error_count <<- unexpected_error_count + 1
      }
    )
  }

  empty_repos <- sort(unique(empty_repos))
  no_member_contributor_repos <- sort(unique(no_member_contributor_repos))

  log_info("Completed governance scan")
  log_info("Repositories scanned:", total_repos)
  log_info("CODEOWNERS files created:", created_codeowners)
  log_info("Inactive repositories flagged:", flagged_inactive)
  log_info("Empty repositories:", length(empty_repos))
  if (length(empty_repos) > 0) {
    log_info(paste(empty_repos, collapse = "\n"))
  }
  log_info("Repos with no current-member contributors:", length(no_member_contributor_repos))
  if (length(no_member_contributor_repos) > 0) {
    log_info(paste(no_member_contributor_repos, collapse = "\n"))
  }
  log_info("Repositories skipped due to unexpected errors:", unexpected_error_count)

  invisible(
    list(
      repos_scanned = total_repos,
      codeowners_created = created_codeowners,
      inactive_flagged = flagged_inactive
    )
  )
}

if (sys.nframe() == 0) {
  args <- parse_cli_args()
  run_governance(config_path = args$config, dry_run = args$dry_run)
}
