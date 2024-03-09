FROM ubuntu:latest
WORKDIR /sbomcc
RUN apt-get update && apt-get install -y \
    ca-certificates \
    curl \
    wget \
    unzip \
    && rm -rf /var/lib/apt/lists/* \
    && wget https://github.com/SBOMcc/sbomcc/releases/download/0.0.2/sbomcc-0.0.2-linux-x64.zip \
    && unzip sbomcc-0.0.2-linux-x64.zip
# EXPOSE 8080
ENTRYPOINT [ "sbomcc-0.0.2/bin/sbomcc" ]
CMD [ "--version" ]
