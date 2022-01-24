# About 

I customized https://salsa.debian.org/benchmarksgame-team/benchmarksgame for supporting the benchmarks of AoC 2021 programs, and maybe other programming contests. 

You can see the results on https://aoc-benchmarks.dokmelody.org

For submitting new code to test, you can add issues and/or pull-requests on https://github.com/massimo-zaniboni/aoc-benchmarks . The program must read data from the standard input, and write the result to stdout, without adding a new-line at the end. Add a license in the header of the code. See examples of programs in directory ``benchmarks/programs/``.

Each benchmark (called also "test") has different input-size. Each program is tested starting from smaller input-sizes. 

# Design

I reused entirely the *bencher* utility of the https://salsa.debian.org/benchmarksgame-team/benchmarksgame project. 

The *bencher* uses ``python-gtop``, that it is not fully supported from many Linux distributions. So I created an Ubuntu Docker image, supporting it. This Ubuntu Docker container it is used also for compiling and benchmarking the code.

The Docker container will live only during the benchmarks, then it will be destroyed. The directory ``benchmarks`` will be mounted inside the Docker container. It contains the input programs in ``benchmars/programs``, and it will contain the results of benchmarks at the end. 

# Installation

Required packages:

* ``docker`` or ``podman``, for creating the compilation and benchmark environment (it will use a lot of storage)
* ``tar`` with ``xz`` decompression support, for extracting the (big) datasets
* ``php`` interpreter for seeing the results

Execute

``` sh
./aoc-benchmarks.sh install
```

for creating a Docker image, with a rather big Ubuntu based compilation and benchmark environment. It will download most recent version of compilers. ``Dockerfile`` contains the image installation instructions. 

# Running benchmarks

Initially

``` sh
./aoc-benchmarks.sh run-all

```

It will execute all benchmarks. It will be very slow (i.e. many hours). These results depends from the hardware, so they cannot be ported between different hosts. 

This command must be executed inside an host with all resources given to the benchmark code (e.g. no open applications and running services). Fast benchmarks will be repeated more than one time, and only the best result will be kept. So it is tolerable temporary fluctuations in the resources of the host. Very long benchmarks will be executed only one time, but in these case some seconds of difference are not much important.

For executing only new or changed benchmarks (i.e. when the source code in ``bencher/programs`` is newer than the result of the benchmark)

``` sh
./aoc-benchmarks.sh run-new
```

Benchmarks can be interrupted pressing ``Control-c`` and restarted with ``./aoc-benchmarks.sh run-new`` command. So they can be executed in an incremental way.

# Viewing benchmark results

``` sh
./container.sh web
```

It launches a PHP local web-server (i.e. not good for production) accepting connections on 8000 port. Connect your browser to ``http://localhost:8000/index.html``.

Stop with ``Control-c``.

# Uninstall

For removing completely the image, in case the project is not anymore needed

``` sh
./container.sh prune
```

# How To

## Adding a new program to benchmark

Add programs in the ``benchmarks/programs/<task-name>/`` directory.

Programs name must follow a fixed pattern, like ``task_name.hs-1.hs`` or ``another_task.lisp-2.lisp``.

The ``task_name`` is the same name of the directory.

``-1`` or ``-2`` are the version of the code.

The ``.hs-1.hs`` is a repetition of the language name.

IMPORTANT: never use "-" in the task name, i.e. "fast_path" is good, "fast-path" no.

## Debugging new added programs 

In ``benchmarks/run_logs`` there is the compilation and execution log of last executed programs. More details are in ``benchmarks/bencher/tmp/`` there is the state of last compiled code.

The first run of a program, set the expected result of the task. Also newlines at the end or start of the result counts. So make sure that the output will be exactly the same. The expected output is inside ``benchmarks/data-files/`` directory, inside ``*.out`` files. In case of unintended difference, delete them. Usually the result must terminate with a new-line.

## Adding a new benchmark to test

Given a benchmark name like ``fast_path``:

* add ``fast_path`` in ``onlydirs`` inside ``benchmarks/makefiles/my.linux.ini``
* create directory ``benchmarks/programs/fast_path``
* add ``fast_path`` inside ``apps/show-benchmarks/desc/test.csv``, and describe the task
* add ``fast_path`` inside ``apps/show-benchmarks/websites/linux/include.csv``

IMPORTANT: never use "-" in the names, i.e. "fast_path" is good, "fast-path" no.

### Adding a new data-set to use in task to tests

Datasets can be big, so they are not included in the repository.

The ideal solution is including the code generating then, and calling it during installation. But it is not yet available.

Another solution is using some private torrent service, or some internet cache. But the technology is not in widespread usage.

The temporary pragmatic solution is putting a tar xz compressed file on one of my server. 

### Generating new compressed data-sets

For generating new data-sets:

* create a temporary working directory ``datasets-XY``, where `XY` are progressive numbers respect the numbers used in ``aoc-benchmarks.sh``
* for each program ``foo`` create a directory ``datasets-XY/foo``
* for each test suite numbered like 100 or 1000 create ``foo-input100.txt`` and ``foo-input1000.txt``
* in case of tests sharing the same input data set, like ``aoc2021_day1a`` and ``aoc2021_day1b``, put inside ``aoc2021-day1b`` directory a relative symbolic link, e.g. ``ln --symbolic -r aoc2021_day1a/aoc2021-day1a-input100.txt aoc2021-day1b/aoc2021_day1b-input100.txt``
* create a tarred xz archive ``tar cJf datasets-XY.tar.xz datasets-XY``
* publish it on some server
* add instructions in ``aoc-benchmarks.sh`` for installing it
* update with ``./aoc-benchmarks.sh update``

## Adding a new language

Assuming a new language ``foo`` with suffix ``f`` (IMPORTANT: never use "-" in the names of languages):

* add ``foo.php`` inside ``apps/show-benchmarks/websites/linux/version``
* add suffix ``f`` in section ``[build] make`` of ``benchmarks/makefiles/my.linux.ini``
* study the settings in ``benchmarks/makefiles/my.linux.ini`` and ``benchmarks/makefiles/my.linux.Makefile`` and replicate them for the new ``foo`` language
* add the language description in ``apps/show-benchmarks/desc/lang.csv``. The description contains also a list of preferred programming languages to compare against it
* in case of compilation errors check ``benchmarks/data-files`` directory
* for executing again (only) the code with problems, use ``touch benchmarks/programs/<your-code> && ./aoc-benchmarks.sh run-new`` 

# Credits

The ``bencher`` is based on https://salsa.debian.org/benchmarksgame-team/benchmarksgame project released under BSD license. See its content for proper credits.

AoC 2021 big data sets are based (i.e. a copy of) on https://the-tk.com/project/aoc2021-bigboys.html

Advent of Code problems are at https://adventofcode.com/

