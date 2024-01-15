FROM fedora:latest

ENV TERM xterm-256color

###############################################################################
# install
###############################################################################

# base
RUN dnf install -y \
    git \
    git-lfs \
    python \
    python3 \
    python3-pip \
    python3-tornado \ # for WebAgg matplotlib
    gcc \
    gcc-c++ \
    make \
    openssl-devel \
    libffi-devel \
    redhat-rpm-config \
    vim \
    tmux \
    hostname \
    && dnf clean all

# user
RUN dnf install -y \
    neovim


###############################################################################
# general 
###############################################################################

# add user
RUN useradd -u 1000 -g 100 -ms /bin/bash dev \
    && echo 'dev:pass' | chpasswd
RUN usermod -aG wheel dev

###############################################################################
# user config
###############################################################################

USER dev
WORKDIR /home/dev

RUN git clone --depth 1 https://github.com/wbthomason/packer.nvim \
        ~/.local/share/nvim/site/pack/packer/start/packer.nvim

# clone dotfiles
RUN sh -c "$(curl -fsLS get.chezmoi.io)"
ENV PATH="/home/dev/bin:${PATH}"
RUN chezmoi init --apply https://github.com/starr-dusT/dotfiles

CMD ["/bin/bash"]
