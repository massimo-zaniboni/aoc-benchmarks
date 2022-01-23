#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

CHECKS="docker-files/check-steps"
DOCKER_CMD=docker
DATASETS="benchmarks/data-files"
VIEWER_DIR="apps/show-benchmarks/websites/linux"

function help() {
      echo "Valid arguments:"
      echo ""
      echo "    install"
      echo "        build or update the Docker image aoc-benchmarks"
      echo "    update"
      echo "        update the Docker image aoc-benchmarks"
      echo "    run-all"
      echo "        execute all benchmarks"
      echo "    run-new"
      echo "        execute only new/changed benchmarks"
      echo "    web"
      echo "        launch dev httpd web server on local port 8000"
      echo "    prune"
      echo "        remove the Docker image"
      echo "    help"
      echo "        this help"
}

function install_docker_image() {
      $DOCKER_CMD build -t aoc-benchmarks .
}

install_dataset () {
  S="$CHECKS/$1.chk"

  if [[ -e $S ]]; then
      echo "Phase $S already done"
  else

      mkdir -p $DATASETS
      pushd $DATASETS

      wget https://downloads.asterisell.com/aoc/"$1.tar.xz"
      tar -xJf "$1.tar.xz" --strip-components=1
      rm -r -f "$1.tar.xz"

      popd
      touch $S
  fi
}

function install() {
  set +o errtrace

  mkdir -p $CHECKS

  install_docker_image
  install_dataset "datasets-01"
}

function run_new() {
  set +o errtrace


  $DOCKER_CMD run --rm -it \
    --mount type=bind,source="$(pwd)"/benchmarks/data-files,target=/bencher/tmp \
    --mount type=bind,source="$(pwd)"/benchmarks/makefiles,target=/bencher/makefiles \
    --mount type=bind,source="$(pwd)"/benchmarks/programs,target=/bencher/programs \
    --mount type=bind,source="$(pwd)"/benchmarks/results,target=/bencher/results \
    --mount type=bind,source="$(pwd)"/benchmarks/run_logs,target=/bencher/run_logs \
    --mount type=bind,source="$(pwd)"/benchmarks/run_markup,target=/bencher/run_markup \
    --mount type=bind,source="$(pwd)"/benchmarks/summary,target=/bencher/summary \
    aoc-benchmarks

  mkdir -p $VIEWER_DIR/data
  mkdir -p $VIEWER_DIR/code

  rm -r -f $VIEWER_DIR/data/*
  rm -r -f $VIEWER_DIR/code/*

  cp benchmarks/summary/* $VIEWER_DIR/data/.
  cp benchmarks/run_markup/* $VIEWER_DIR/code/.
  cp benchmarks/run_logs/* $VIEWER_DIR/code/.
}

function run_all() {
      set +o errtrace

      pushd benchmarks

      # Create potentially missing directories
      mkdir -p data-files
      mkdir -p results
      mkdir -p run_logs
      mkdir -p run_markup
      mkdir -p summary

     # Update the date of all programs to test
     find programs -type f -exec touch {} \;

     # Remove the expected results:
     # the first invocation of a program is used as expected result.
     find data-files -name '*_out' -type f -exec rm {} \;

     # Clean log and intermediate results.
     rm -r -f run_logs/*
     rm -r -f run_markup/*
     rm -r -f summary/*
     rm -r -f results/*

     # Execute the tests
     popd
     run_new
}

if [[ $# -lt 1 ]]; then
      help
      exit 0
fi

case $1 in
      install)
      install
      ;;

      update)
      install
      ;;

      run-all)
      run_all
      ;;

      run-new)
      run_new
      ;;

      web)
      cd apps/show-benchmarks/websites
      php -S 0.0.0.0:8000 -d short_open_tag=On
      ;;

      prune)
      $DOCKER_CMD rmi aoc-benchmarks
      echo "Removed the image."

      echo "For removing more data execute"
      echo ""
      echo "  $DOCKER_CMD image prune --all"
      echo ""
      echo "This command will affect the images of all stopped containers!"
      echo ""
      ;;

      help|*)
            help
     ;;
esac
