FROM rocker/r-ver:4.5.1

RUN apt-get update && apt-get install -y \
    gdb \
    gfortran \
    g++ \
    make \
    pkg-config \
    git \
    vim-tiny \
    libz-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    zlib1g-dev \
    ca-certificates \
    zlib1g-dev \
    openssl \
    curl \
    python3 \
    python3-pip \
    && rm -rf /var/lib/apt/lists/*
RUN python3 -m pip install --break-system-packages --no-cache-dir fortls
RUN R --vanilla -e "install.packages(\"pak\", repos=\"https://cloud.r-project.org\")"
RUN R --vanilla -e "pak::pak(c(\"devtools\"))"
RUN R --vanilla -e "pak::pak(c(\"dplyr\",\"tidyr\",\"GenSA\",\"BayesianTools\",\"lubridate\",\"multidplyr\"))"
RUN R --vanilla -e "install.packages(\"BayesianTools\", repos=\"https://cloud.r-project.org\")"
RUN mkdir -p /root/.R && \
    echo 'FFLAGS=-g -O0 -fcheck=all -fbacktrace' >> /root/.R/Makevars && \
    echo 'FCFLAGS=-g -O0 -fcheck=all -fbacktrace' >> /root/.R/Makevars && \
    echo 'CFLAGS=-g -O0' >> /root/.R/Makevars && \
    echo 'CXXFLAGS=-g -O0' >> /root/.R/Makevars && \
    echo 'CXX11FLAGS=-g -O0' >> /root/.R/Makevars && \
    echo 'CXX14FLAGS=-g -O0' >> /root/.R/Makevars && \
    echo 'CXX17FLAGS=-g -O0' >> /root/.R/Makevars

WORKDIR /work
CMD ["bash"]