# About 

I customized https://salsa.debian.org/benchmarksgame-team/benchmarksgame for supporting the benchmarks of AoC 2021 programs, and maybe other programming contests. 

You can see the results on https://aoc-benchmarks.dokmelody.org

For submitting new code to test, you can add issues and/or pull-requests on https://github.com/massimo-zaniboni/aoc-benchmarks 

* The submitted program must read from the standard input the input data file, and write the result to stdout, without adding a new-line at the end.
* Add a license in the header of the code.
* Add eventually notes about the code.
* See examples of programs in directory ``benchmarks/bencher/programs/``
* I will review the code and fix minor problems.

You can clone and customize this tool for benchmarking other type of projects.

# Design

I reused entirely the *bencher* utility of the https://salsa.debian.org/benchmarksgame-team/benchmarksgame project. 

The *bencher* uses ``python-gtop``. I were not able to compile it under NixOS, my OS of choice, so I created an Ubuntu Docker image, supporting it. This Ubuntu Docker container it is used also for compiling and benchmarking the code. The directory ``benchmarks`` will be also a volume of the ``aoc-benchmarks`` container, inside the container directory ``/benchmarks``. So every change done by the Docker container will be stored in the native file system. So operations on ``benchmarks`` directory are done by a Docker container configured with the proper tools.

# Installation

Requirements are:

* Docker or Podman for generating the container.
* A lot of space will be used for the Ubuntu image, tools and benchmark data sets.

Set the variable ``DOCKER_CMD`` in ``container.sh``, for using ``Podman`` or ``Docker``.

For installing the image

``` sh
./container.sh install
```

This command will execute the instructions inside ``Dockerfile``. It will also install the benchmark datasets, in ``benchmarks/bencher/tmp``, downloading them from a remote server.

# Running benchmarks

For executing the new/modified benchmarks

``` sh
./container.sh run-new

```

For executing again all benchmarks (very slow):

```
./container.sh run-all
```

For accessing and testing the container, using a shell

``` sh
./container.sh login
```

The ``/benchmarks`` directory is a volume mapped to the local ``benchmarks`` directory of the project. So every change to it will be permanent, while changes to other directories will be lost after the exit. Changes can be tested in the container, then written in the``Dockerfile``. The image can be upgraded with the content of the new ``Dockerfile`` calling again ``./container.sh install``: the content of ``benchmarks`` directory will be not touched, except the directory ``benchmarks/bencher/tmp`` containing the datasets.

# Viewing benchmark results

## Locally (i.e. no production)

``` sh
./container.sh web
```

Launch a PHP local web-server (i.e. not good for production) accepting connections on 8000 port. The 8000 port of the Docker container is exported to the 8000 port of the host system.

Connect your browser to ``http://localhost:8000/index.html``.

# Uninstall

For removing completely the image, in case the project is not anymore needed

``` sh
./container.sh prune
```

# How To

## Adding a new program to benchmark

Add programs in the ``benchmarks/bencher/programs/<task-name>/`` directory.

Programs name must follow a fixed pattern, like ``task_name.hs-1.hs`` or ``another_task.lisp-2.lisp``.

The ``task_name`` is the same name of the directory.

``-1`` or ``-2`` are the version of the code.

The ``.hs-1.hs`` is a repetition of the language name.

IMPORTANT: never use "-" in the task name, i.e. "fast_path" is good, "fast-path" no.

## Debugging new added programs 

In ``benchmarks/bencher/run_logs`` there is the compilation and execution log of last executed programs. More details are in ``benchmarks/bencher/tmp/`` there is the state of last compiled code.

The first run of a program, set the expected result of the task. Also newlines at the end or start of the result counts. So make sure that the output will be exactly the same. In case of differences, fix the code, and execute ``./run-all-tests.sh`` (it will reset the expected result). The expected output is inside ``benchmarks/bencher/tmp/`` directory, inside ``*.out`` files.

## Adding a new benchmark to test

Given a benchmark name like ``fast_path``:

* add ``fast_path`` in ``onlydirs`` inside ``benchmarks/bencher/makefiles/my.linux.ini``
* create directory ``benchmarks/bencher/programs/fast_path``
* add ``fast_path`` inside ``benchmarks/mybenchmarks/desc/test.csv``, and describe the task
* add ``fast_path`` inside ``benchmarks/mybenchmarks/websites/linux/include.csv``

IMPORTANT: never use "-" in the names, i.e. "fast_path" is good, "fast-path" no.

### Adding a new data-set to use in task to tests

Datasets can be big, so they are not included in the repository.

The ideal solution is including the code generating then, and calling it during installation. But it is not yet available.

Another solution is using some private torrent service, or some internet cache. But the technology is not in widespread usage.

The temporary pragmatic solution is putting an lrzip compressed version on one of my server. 

Datasets are downloaded and decompressed in ``benchmarks/bencher/tmp`` directory. In this directory, bencher will put also the results of benchmarks. This is not done directly:

* the ``Dockerfile`` decompress them in ``/downloads/datasets`` directory
* the ``files/entrypoint.sh`` create at every startup the symbolic links to these files

### Generating new compressed data-sets

For generating new data-sets:

* create a temporary working directory ``datasets-XY``, where `XY` are progressive numbers respect the numbers used in ``docker-files/init-datasets.hs``
* for each program ``foo`` create a directory ``datasets-XY/foo``
* for each test suite numbered like 100 or 1000 create ``foo-input100.txt`` and ``foo-input1000.txt``
* in case of tests sharing the same input data set, like ``aoc2021_day1a`` and ``aoc2021_day1b``, put inside ``aoc2021-day1b`` directory a relative symbolic link, e.g. ``ln --symbolic -r aoc2021_day1a/aoc2021-day1a-input100.txt aoc2021-day1b/aoc2021_day1b-input100.txt``
* create a tarred lrzip archive ``lrztar datasets-XY``
* add instructions in `docker-files/init-datasets.sh` for adding its content to ``benchmarks/bencher/tmp`` directory
* rebuild the image with `./container-install.sh`
* launch new tests with `./container-run-new-benchmarks.sh`

## Adding a new language

Assuming a new language ``foo`` with suffix ``f`` (IMPORTANT: never use "-" in the names of languages):

* add ``foo.php`` inside ``benchmarks/mybenchmarks/websites/linux/version``
* add suffix ``f`` in section ``[build] make`` of ``benchmarks/bencher/makefiles/my.linux.ini``
* study the settings in ``benchmarks/bencher/makefiles/my.linux.ini`` and ``benchmarks/bencher/makefiles/my.linux.Makefile`` and replicate them for the new ``foo`` language
* add the language description in ``benchmarks/mybenchmarks/desc/lang.csv``. The description contains also a list of preferred programming languages to compare against it
* in case of compilation errors check ``benchmarks/mybenchmarks/tmp`` directory
* for executing again (only) the code with problems, use ``touch benchmarks/mybenchmarks/programs/<your-code> && ./container.sh run-new`` 

# Credits

The ``bencher`` is based on https://salsa.debian.org/benchmarksgame-team/benchmarksgame project released under BSD license. See its content for proper credits.

AoC 2021 big data sets are based (i.e. a copy of) on https://the-tk.com/project/aoc2021-bigboys.html

Advent of Code problems are at https://adventofcode.com/

