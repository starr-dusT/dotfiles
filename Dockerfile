FROM ghcr.io/void-linux/void-linux:latest-full-x86_64 
MAINTAINER starr-dusT <starrtyler88@gmail.com>

RUN xbps-install -Su xbps -y
RUN xbps-install -S sudo bash -y

RUN useradd -ms /bin/bash tstarr
RUN gpasswd -a tstarr wheel
RUN echo 'tstarr ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

RUN echo "LANG=en_US.UTF-8" > /etc/locale.conf
RUN echo "en_US.UTF-8 UTF-8" > /etc/default/libc-locales
RUN xbps-reconfigure -f glibc-locales

USER tstarr

RUN mkdir -p /home/tstarr/.local/share/chezmoi
COPY --chown=tstarr:users . ./home/tstarr/.local/share/chezmoi
WORKDIR /home/tstarr/.local/share/chezmoi

RUN locale

ENTRYPOINT ["sh", "docker-entrypoint.sh"]
