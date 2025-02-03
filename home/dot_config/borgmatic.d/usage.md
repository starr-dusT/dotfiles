# Usage

# Example commands for rsync.net repos
```
sudo borgmatic init --repository {repo} --encryption repokey --override remote_path=borg1 --config {config_file} --verbosity 1
sudo borgmatic create --repository {repo} --list --stats --override remote_path=borg1 --config {config_file} --verbosity 1
```

# Example commands for local repos
```
sudo borgmatic init --repository {repo} --encryption repokey --config {config_file} --verbosity 1
sudo borgmatic create --repository {repo} --list --stats --config {config_file} --verbosity 1
```
