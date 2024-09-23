# Usage

# Example commands for rsync.net repos
```
sudo borgmatic init --repository {repo} --encryption repokey --override remote_path=borg1 --config {config_file} 
sudo borgmatic create --repository {repo} --list --stats --override remote_path=borg1 --config {config_file} 
```

# Example commands for local repos
```
sudo borgmatic init --repository {repo} --encryption repokey --config {config_file} 
sudo borgmatic create --repository {repo} --list --stats --config {config_file} 
```
