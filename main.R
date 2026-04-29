options(github_housekeeping_project_root = getwd())
source(file.path(getOption("github_housekeeping_project_root"), "R", "main.R"))

args <- parse_cli_args()
run_governance(config_path = args$config, dry_run = args$dry_run)