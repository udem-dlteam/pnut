# The source of the pnut repository, either git or from local directory
ARG PNUT_SOURCE=clone

# Works on both x86 and ARM
FROM ubuntu:24.04 AS base

RUN apt-get update && apt-get install -y \
    # Includes gcc and make
    build-essential \
    # To clone the repository if needed
    git \
    # To measure the time of the execution
    time \
    # Install all tested shells
    bash dash ksh zsh mksh yash \
    # To create Debian woody environment
    debootstrap

# Create a Debian woody environment to test bash-2.05a
# The woody environment is done in 2 steps to cache this expensive step
RUN \
  # Create a Debian woody environment
  debootstrap --arch=i386 woody woody http://archive.debian.org/debian; \
  # Install gcc and time utils
  chroot woody /bin/bash -c "apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y build-essential time timeout";

# Clone repository
FROM base AS pnut_from_clone
RUN git clone https://github.com/udem-dlteam/pnut.git --depth 1 /pnut

# Copy repository from local directory
# Mainly used for development to avoid having to commit and push changes
FROM base AS pnut_from_local
COPY . /pnut

# Copy the pnut repository from the source, either git or local directory
FROM pnut_from_${PNUT_SOURCE} AS with_pnut

# Copy pnut into the woody environment, then move it in the main pnut directory
RUN cp -r pnut ../woody/pnut; mv ../woody pnut

# Set the working directory
WORKDIR /pnut

# Create woody.sh script to enter the woody environment
COPY woody.sh .

# Start in bash
ENTRYPOINT ["/bin/bash"]
