#!/usr/bin/env bash

# Update the datasets
mkdir -p /host/bencher/tmp

cd /downloads/datasets/tmp-datasets
find . -type d -exec mkdir -p /host/bencher/tmp/{} \;

cd /host/bencher/tmp
for f in $(find /downloads/datasets/tmp-datasets -name '*.txt')
do
  fn=$(basename $f)
  fd=$(dirname $f)
  fd1=$(basename $fd)

  ln --symbolic -r $f $fd1/$fn
done

cd /downloads/small-datasets
find . -type d -exec mkdir -p /host/bencher/tmp/{} \;

cd /host/bencher/tmp
for f in $(find /downloads/small-datasets -name '*.txt')
do
  fn=$(basename $f)
  fd=$(dirname $f)
  fd1=$(basename $fd)

  ln --symbolic -r $f $fd1/$fn
done

# Start a demo web-server
/usr/bin/php -d short_open_tag=on -S 0.0.0.0:8000 -t /host/mybenchmarks/websites
