{ pkgs, user, ... }:
{
  environment.systemPackages = [
    (pkgs.writeScriptBin "commit-vulcan" ''
      #!/bin/sh
      REPO_PATH="$HOME/Documents/vault/vulcan"
      MESSAGE="Automated commit: $(date '+%Y-%m-%d %H:%M:%S')"

      cd "$REPO_PATH" || exit 1

      if ! git diff --quiet --exit-code; then
          git add -A . # Stage all changes
          git commit -m "$MESSAGE"
          git push origin master
      fi
    '')
  ];

  # Every hour commit vulcan if it has changed
  services.cron.systemCronJobs = [
    "0 * * * * ${user} commit-vulcan"
  ];
}
