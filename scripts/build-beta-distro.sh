#!/bin/sh

verbose=1

chirp() { [ $verbose ] && shout "$*"; return 0; }
shout() { echo "$0: $*" >&2;}
barf() { shout "$*"; exit 111; }
safe() { "$@" || barf "cannot $*"; }

types="(beta|aws)"
targets="(osx|ubuntu|centos|perform|dist)"
usage="\n
usage: [type] [target]\n
   \t type - required build type $types \n
   \t target  -  optional target $targets"

# User must specify the type of build: aws or beta.
# Beta and AWS builds include the same files, but are built with different
# limitations and, thus, different kill switch flags.
type="$1"
if [ -n "$type" ]; then
    [[ ! "$type" =~ $types ]] && barf $usage
else
   barf $usage
fi

# UNLIMITED="true" #if this line is not commented out, kill-switches will be ignored during the build

FLAG="kill-switch"             # beta is the default
if [[ "$type" = "aws" ]]; then
    FLAG="aws-kill-switch"
fi

if [ -n "$UNLIMITED" ]; then
    STACK_FLAG=""
    DOCKER_FLAG=""
else
    STACK_FLAG="--flag graavity:$FLAG"
    DOCKER_FLAG="$FLAG"
fi
chirp "Building with STACK_FLAG = $STACK_FLAG"
chirp "Building with DOCKER_FLAG = $DOCKER_FLAG"

target="$2"
if [ -n "$target" ]; then
    [[ ! "$target" =~ $targets ]] && barf $usage
fi

chirp "Creating build directories"
safe rm -rf graavity-beta/aws && mkdir graavity-beta/aws
safe rm -rf graavity-beta/log && mkdir graavity-beta/log
safe rm -rf graavity-beta/conf && mkdir graavity-beta/conf
safe rm -rf graavity-beta/demo && mkdir graavity-beta/demo
safe rm -rf graavity-beta/setup && mkdir graavity-beta/setup

version=`egrep "^version:" graavity.cabal | sed -e 's/^version: *\(.*\) *$/\1/'`
if [ -z "$version" ]; then barf "Could not determine version"; fi
chirp "Building version $version"

if [ -z "$target" -o "$target" = "osx" ]; then

    chirp "Builing and Copying: OSX"
    rm ./graavity-beta/bin/osx/{genconfs,graavityserver,graavityclient}

    safe stack install $STACK_FLAG

    safe cp ./bin/genconfs ./graavity-beta/bin/osx/;
    safe cp ./bin/graavityserver ./graavity-beta/bin/osx/;
    safe cp ./bin/graavityclient ./graavity-beta/bin/osx/;

fi

if [ -z "$target" ]; then
    chirp "Copying: Conf"
    safe rm -rf ./graavity-beta/conf/*
    safe rm ./conf/*
    safe printf "\n\n4\n\n\n\n\n\n\n\n\n\n\n\n" | ./bin/genconfs
    safe cp ./conf/* ./graavity-beta/conf

    chirp "Clearing out the log"
    rm ./graavity-beta/log/*
fi

if [ -z "$target" -o "$target" = "ubuntu" ]; then

    chirp "Builing and Copying: Ubuntu 16.04"
    rm -rf ./graavity-beta/bin/ubuntu-16.04/{genconfs,graavityserver,graavityclient}
    safe docker build --cpuset-cpus="0-3" --cpu-shares=1024 --memory=8g -t graavity-base:ubuntu-16.04 -f docker/ubuntu-base.Dockerfile .
    safe docker build --build-arg flag=$DOCKER_FLAG --cpuset-cpus="0-3" --cpu-shares=1024 --memory=8g -t graavity:ubuntu-16.04 -f docker/ubuntu-build.Dockerfile .
    safe docker run -i -v ${PWD}:/work_dir graavity:ubuntu-16.04 << COMMANDS
cp -R /ubuntu-16.04 /work_dir/graavity-beta/bin
COMMANDS
    safe cp docker/ubuntu-base.Dockerfile graavity-beta/setup/

fi

if [ -z "$target" -o "$target" = "centos" ]; then

    chirp "Builing and Copying: CENTOS 6.8"
    rm -rf ./graavity-beta/bin/centos-6.8/{genconfs,graavityserver,graavityclient}
    safe docker build --cpuset-cpus="0-3" --cpu-shares=1024 --memory=8g -t graavity-base:centos-6.8 -f docker/centos6-base.Dockerfile .
    safe docker build --build-arg flag=$DOCKER_FLAG --cpuset-cpus="0-3" --cpu-shares=1024 --memory=8g -t graavity:centos-6.8 -f docker/centos6-build.Dockerfile .
    safe docker run -i -v ${PWD}:/work_dir graavity:centos-6.8 << COMMANDS
cp -R /centos-6.8 /work_dir/graavity-beta/bin
COMMANDS
    safe cp docker/centos6-base.Dockerfile graavity-beta/setup/

fi

if [ -z "$target" -o "$target" = "perform" ]; then

    chirp "Builing and Copying: Performance Monitor"
    rm -rf ./graavity-beta/static/monitor/*
    safe cp -R monitor/ ./graavity-beta/static/monitor

fi

chirp "Copying Scripts and Ansible Playbooks"
safe rm -rf ./graavity-beta/aws/*
safe cp -r ./scripts/aws/* ./graavity-beta/aws
safe mv ./graavity-beta/aws/Ansible-README.md ./graavity-beta/docs
safe cp ./demo/{demo.repl,demo.pact,demo.yaml} ./graavity-beta/demo
safe cp ./scripts/setup-ubuntu-base.sh ./graavity-beta/setup

safe cp CHANGELOG.md graavity-beta/


chirp "taring the result"
if [[ "$type" = "aws" ]]; then
    safe cp -r graavity-beta graavity-aws
    safe rm -r graavity-aws/aws/edit_conf.yml
    safe rm -r graavity-aws/aws/templates/conf.j2
    safe rm -rf graavity-aws/bin/ubuntu-16.04/start-no-persistence.sh  # Not used
    safe rm -rf graavity-aws/bin/centos-6.8     # Not supported in AWS or Azure
    safe rm -rf graavity-aws/bin/osx            # Not supported in AWS or Azure
    safe tar cvz graavity-aws/* > graavity-aws-$version.tgz
    safe rm -rf graavity-aws
else
    safe tar cvz graavity-beta/* > graavity-beta-$version.tgz
fi

exit 0
