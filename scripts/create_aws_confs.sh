#!/bin/bash

if [ -d "aws-conf" ]
then
    rm -rf aws-conf/*
else
    mkdir aws-conf
fi

aws ec2 describe-instances --filter Name=tag:Name,Values=graavityserver \
  | grep '"PrivateIpAddress"' \
  | sed 's/[^(0-9).]//g' \
  | uniq | sort > aws-conf/graavityservers.privateIp

aws ec2 describe-instances --filter Name=tag:Name,Values=graavityclient \
  | grep '"PrivateIpAddress"' \
  | sed 's/[^(0-9).]//g' \
  | uniq | sort > aws-conf/graavityclient.privateIp

./bin/genconfs --distributed aws-conf/graavityservers.privateIp

for ip in `cat aws-conf/graavityservers.privateIp`; do
    idir="aws-conf/${ip}"
    mkdir -p $idir/conf
    conf="${ip}-cluster.yaml"
    script="${idir}/start.sh"
    mv ./conf/$conf $idir/conf/$conf
    echo "#!/bin/sh
nohup ./graavityserver +RTS -N -RTS -c conf/${conf} >> ./${ip}-output.log 2>&1 &
" > $script
    chmod +x $script
done

echo 'make sure you have run `stack install` from graavity and have `~/.local/bin` in your path (or have run `cd ~ ; ln -s .local/bin/* . `)'
