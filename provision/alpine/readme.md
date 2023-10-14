# Provision Alpine
> \*Funny joke here.\*

Set of configs files to setup Alpine Linux on iSH.

## Usage

Run following to install:

```bash
apk add curl && \
curl https://raw.githubusercontent.com/starr-dusT/dotfiles/master/provision/alpine/alpine-install-iSH.sh | sh
```

Then run the following commands:

```bash
rbw config set email <email>
chezmoi init
chezmoi apply -k # This command will fail
bash
chezmoi apply -k # This command will fail
chezmoi apply # First time don't overwrite file
```
