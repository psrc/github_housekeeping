summarise_contributions <- function(commits, excluded_users, org_members, require_org_membership) {
  logins <- vapply(
    commits,
    function(commit) {
      author <- commit$author
      if (is.null(author) || is.null(author$login)) {
        return(NA_character_)
      }

      author$login
    },
    character(1)
  )

  commit_frame <- data.frame(login = logins, stringsAsFactors = FALSE)
  commit_frame <- dplyr::filter(commit_frame, !is.na(.data$login), !(.data$login %in% excluded_users))

  if (require_org_membership) {
    commit_frame <- dplyr::filter(commit_frame, .data$login %in% org_members)
  }

  if (nrow(commit_frame) == 0) {
    return(commit_frame)
  }

  summary <- commit_frame |>
    dplyr::count(.data$login, name = "commit_count", sort = TRUE) |>
    dplyr::mutate(share = .data$commit_count / sum(.data$commit_count))

  as.data.frame(summary, stringsAsFactors = FALSE)
}

summarise_contributors_history <- function(contributors, excluded_users, org_members, require_org_membership) {
  if (length(contributors) == 0) {
    return(data.frame(login = character(), commit_count = integer(), share = numeric(), stringsAsFactors = FALSE))
  }

  logins <- vapply(contributors, function(contributor) contributor$login %||% NA_character_, character(1))
  counts <- vapply(contributors, function(contributor) contributor$contributions %||% 0, numeric(1))

  contributor_frame <- data.frame(login = logins, commit_count = counts, stringsAsFactors = FALSE)
  contributor_frame <- dplyr::filter(contributor_frame, !is.na(.data$login), !(.data$login %in% excluded_users))

  if (require_org_membership) {
    contributor_frame <- dplyr::filter(contributor_frame, .data$login %in% org_members)
  }

  if (nrow(contributor_frame) == 0) {
    return(contributor_frame)
  }

  contributor_frame <- contributor_frame |>
    dplyr::arrange(dplyr::desc(.data$commit_count), .data$login) |>
    dplyr::mutate(share = .data$commit_count / sum(.data$commit_count))

  as.data.frame(contributor_frame, stringsAsFactors = FALSE)
}

select_codeowners <- function(contribution_summary, min_contribution_threshold) {
  if (nrow(contribution_summary) == 0) {
    return(character())
  }

  contribution_summary <- contribution_summary |>
    dplyr::arrange(dplyr::desc(.data$commit_count), .data$login)

  highest_count <- contribution_summary$commit_count[[1]]
  tied_leaders <- contribution_summary$login[contribution_summary$commit_count == highest_count]

  if (length(tied_leaders) > 1) {
    return(tied_leaders)
  }

  top_share <- contribution_summary$share[[1]]

  if (top_share >= min_contribution_threshold || nrow(contribution_summary) == 1) {
    return(contribution_summary$login[[1]])
  }

  cumulative_share <- cumsum(contribution_summary$share)
  owner_count <- which(cumulative_share >= min_contribution_threshold)[1]
  owner_count <- max(2, owner_count %||% min(3, nrow(contribution_summary)))

  contribution_summary$login[seq_len(owner_count)]
}

build_codeowners_content <- function(owners) {
  sprintf("* %s\n", paste(sprintf("@%s", owners), collapse = " "))
}

create_codeowners_for_repo <- function(repo_info, config, org_members, dry_run = FALSE) {
  owner <- repo_info$owner$login
  repo <- repo_info$name
  branch <- repo_info$default_branch

  since <- as_github_timestamp(months_ago_datetime(config$lookback_months))
  commits <- tryCatch(
    list_commits_since(owner, repo, since = since, branch = branch),
    error = function(error) {
      if (is_empty_repo_error(error)) {
        return(structure(list(), class = "empty_repo"))
      }

      stop(error)
    }
  )

  if (inherits(commits, "empty_repo")) {
    return(list(status = "empty_repo"))
  }

  if (length(commits) == 0) {
    # No recent commits in the lookback window: fall back to full-history contributors.
    contributors <- tryCatch(
      list_repo_contributors(owner, repo),
      error = function(error) {
        if (is_empty_repo_error(error)) {
          return(structure(list(), class = "empty_repo"))
        }

        stop(error)
      }
    )

    if (inherits(contributors, "empty_repo")) {
      return(list(status = "empty_repo"))
    }

    history_contributions <- summarise_contributors_history(
      contributors = contributors,
      excluded_users = config$excluded_users,
      org_members = org_members,
      require_org_membership = config$require_org_membership
    )

    if (nrow(history_contributions) == 0) {
      return(list(status = "skipped", reason = "no_current_member_contributors"))
    }

    owners <- select_codeowners(history_contributions, config$min_contribution_threshold)

    if (length(owners) == 0) {
      return(list(status = "skipped", reason = "no_selected_owners"))
    }

    create_file_in_repo(
      owner = owner,
      repo = repo,
      path = ".github/CODEOWNERS",
      content = build_codeowners_content(owners),
      message = "Add CODEOWNERS based on historical contribution history",
      branch = branch,
      dry_run = dry_run
    )

    return(list(status = "created", owners = owners))
  }

  contributions <- summarise_contributions(
    commits = commits,
    excluded_users = config$excluded_users,
    org_members = org_members,
    require_org_membership = config$require_org_membership
  )

  if (nrow(contributions) == 0) {
    return(list(status = "skipped", reason = "no_eligible_contributors"))
  }

  owners <- select_codeowners(contributions, config$min_contribution_threshold)

  if (length(owners) == 0) {
    return(list(status = "skipped", reason = "no_selected_owners"))
  }

  create_file_in_repo(
    owner = owner,
    repo = repo,
    path = ".github/CODEOWNERS",
    content = build_codeowners_content(owners),
    message = "Add CODEOWNERS based on recent contribution history",
    branch = branch,
    dry_run = dry_run
  )

  list(status = "created", owners = owners)
}
