#!/usr/bin/env bash

DOCKER_CMD=podman
# or DOCKER_CMD=docker

VOL="-v "$(pwd)/benchmarks:/benchmarks""

case $1 in
      install)
      $DOCKER_CMD build -t aoc-benchmarks .
      $DOCKER_CMD run --rm $VOL aoc-benchmarks /bin/bash /init-datasets.sh
      ;;

      prune)
      echo "prune"
      $DOCKER_CMD rmi aoc-benchmarks

      echo "For removing image files execute"
      echo ""
      echo "  $DOCKER_CMD image prune --all"
      echo ""
      echo "This command will affect the images of all stopped containers!"
      echo ""
      echo "Maybe use the less aggresive"
      echo ""
      echo "  $DOCKER_CMD image prune"
      echo ""
      echo "The most aggressive command is"
      echo ""
      echo "  $DOCKER_CMD system prune"
      echo ""
      ;;

      login)
      $DOCKER_CMD run --rm -it $VOL aoc-benchmarks /bin/bash
      ;;

      run-all)
      $DOCKER_CMD run --rm -it  $VOL aoc-benchmarks /bin/bash -c "cd /benchmarks/bencher && ./run-all-benchmarks.sh"
      ;;

      run-new)
      $DOCKER_CMD run --rm -it  $VOL aoc-benchmarks /bin/bash -c "cd /benchmarks/bencher && ./run-new-benchmarks.sh"
      ;;

      web)
      $DOCKER_CMD run --rm -it -p 8000:8000 $VOL aoc-benchmarks /bin/bash -c "cd /benchmarks/mybenchmarks/websites && php -S 0.0.0.0:8000 -d short_open_tag=On"
      ;;

      help|*)
      echo "Valid arguments:"
      echo ""
      echo "    install"
      echo "        build or update the image aoc-benchmarks"
      echo "    prune"
      echo "        remove the image"
      echo "    login"
      echo "        connect with a bash shell to the container"
      echo "    run-all"
      echo "        execute all benchmarks"
      echo "    run-new"
      echo "        update benchmarks"
      echo "    web"
      echo "        httpd web server on port 8000"
       echo "    help"
      echo "        this help"
      ;;
esac
