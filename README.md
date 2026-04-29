# GitHub Organization Governance Automation

This repository runs an organization-wide GitHub governance scan on a schedule using R and GitHub Actions.

The automation performs two checks across repositories in a GitHub organization:

1. For repositories without `.github/CODEOWNERS`, it inspects recent commit history and creates a CODEOWNERS file for the top recent contributor.
2. For repositories with `.github/CODEOWNERS`, it checks for inactivity over the configured lookback window and opens an issue tagging the CODEOWNERS when the repository appears inactive. Individual CODEOWNERS users are also assigned to the issue when GitHub allows assignment for that repository.

## Repository Layout

```text
.github/workflows/governance.yml
R/
  assign_codeowners.R
  check_inactive_repos.R
  github_utils.R
  main.R
config/
  config.yaml
README.md
```

## Configuration

Update [config/config.yaml](config/config.yaml) before running:

```yaml
org: "psrc"
lookback_months: 24
excluded_users:
  - "dependabot[bot]"
  - "github-actions[bot]"
min_contribution_threshold: 0.3
require_org_membership: true
ignored_activity_paths:
  - ".github/CODEOWNERS"
ignored_activity_title_patterns:
  - "(?i)codeowners"
```

`min_contribution_threshold` is used as follows:

- If a single contributor owns at least that share of recent commits, the automation assigns that user alone.
- If no single contributor is dominant, the automation assigns multiple top contributors until the cumulative contribution share reaches the threshold, with a minimum of two owners.

Repositories that are archived are skipped. Forks are also skipped by default.

For inactivity checks, the default configuration ignores governance-only activity tied to `.github/CODEOWNERS` rollouts. That keeps a bulk CODEOWNERS backfill from making dormant repositories look active. You can widen or narrow the filters with `ignored_activity_paths` and `ignored_activity_title_patterns`.

## Authentication

Set a `GH_TOKEN` secret at the repository or organization level.

The script passes `GH_TOKEN` explicitly to the R `gh` client.

Classic PAT requirements:

- `repo` for private repository contents, commits, pull requests, issues, and assignee checks
- `read:org` to read organization members

Fine-grained PAT requirements:

- organization permission `Members: Read-only`
- repository permission `Contents: Read and write`
- repository permission `Pull requests: Read-only`
- repository permission `Issues: Read and write`

For a fine-grained PAT, grant access to every repository the workflow should scan.

Known caveat with fine-grained PATs:

- This script uses `/orgs/{org}/members` as the authoritative source for organization membership because `/user/memberships/orgs` and `/user/memberships/orgs/{org}` can be empty or inconsistent with fine-grained PATs.

## GitHub Actions

The workflow is defined in [.github/workflows/governance.yml](.github/workflows/governance.yml).

It runs:

- on a schedule at 06:00 UTC on March 1 and September 1
- manually through `workflow_dispatch`

The workflow installs R, installs the required packages, and runs:

```bash
Rscript R/main.R
```

## Local Testing

Install the required packages locally:

```r
install.packages(c("gh", "dplyr", "purrr", "yaml", "stringr"), repos = "https://cloud.r-project.org")
```

Export a token and run the script from the repository root:

```bash
export GH_TOKEN=your_token_here
Rscript R/main.R --config config/config.yaml --dry-run
```

On Windows PowerShell:

```powershell
$env:GH_TOKEN = "your_token_here"
Rscript R/main.R --config config/config.yaml --dry-run
```

`--dry-run` logs intended writes without creating CODEOWNERS files or issues.

## Manual Workflow Trigger

After pushing this repository to GitHub:

1. Open the repository Actions tab.
2. Select the `Organization Governance` workflow.
3. Choose `Run workflow`.

## Behavior Notes

- Repositories with no commits in the lookback window are skipped.
- Commits without a mapped GitHub author are ignored.
- When `require_org_membership` is `true`, contributors who are not organization members are excluded from CODEOWNERS selection.
- If all contributors are excluded by membership filtering, the repository is skipped and a warning is logged.
- Inactivity checks ignore CODEOWNERS-only commits and merged PRs by default.
- Inactivity issues mention all CODEOWNERS entries and assign any individual CODEOWNERS users who are valid assignees on that repository. Team CODEOWNERS are mentioned but not assigned.
- Duplicate inactivity issues are avoided by checking for an existing open issue with the same title.