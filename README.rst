==================================================
Ehmon - Heroku's basic VM stats monitor
==================================================


Requirements
============

* Erlang

* Rebar


Building Ehmon
==============

    rebar compile


OTP App Configuration Options
=============================

report_mf:
    Reporting module and function for the fun/1 that will report the
    iolist describing the system statistics. Defaults to ```{ehmon, stdout_report}```.

    Reports can also be sent to shh using ```{ehmon_shh_report, send_report}```. If you decide to send reports to a
    listening shh instance you must decide to use either a TCP or Unix Domain Socket. Using the domain socket interface
    will require an additional dependency on `gen_unix <https://github.com/msantos/gen_unix>_`. You wil need to add it
    accordingly to your applications ```rebar.config``` or similar file.

report_interval:
    Interval in seconds between outputing the report. Defaults to ```60```.

shh_report_prefix:
    The prefix to prepend to report statistics. Defaults to ```"erlang.ehmon"```.

shh_connection:
    A string that specifies how to connect to the listening shh process. The string should match on of the following
    two formats: ```tcp,$HOST:$PORT``` or ```unix,$PATH_TO_SOCKET```.

Example Output
==============

    ehmon_report rq=0 cswit=1507 otp=R15B01 procs=30 maxprocs=32768 ports=4 maxports=1024 maxfds=256 etstabs=16 maxetstabs=1400 memtot=7598992 memproc=1045782 memets=285408 membin=183680 memcode=3691241


Report Key Glossary
===================

rq:
    Run queue length - the number of currently processes ready to run.

wc:
    Wall clock time - CPU use in wall clock time since the last
    report.

otp:
    OTP Release - the version number of the running OTP system.

procs:
    Process Count - the number of currently running erlang processes.

maxprocs:
    Process Limit - the maximum number of processes that can run on
    the current node.

ports:
    Port count - the number of ports in use by the current node.

maxports:
    Port limit - the maximum number of ports that can be created by
    the current node. (Also limited by maxfds usually)

maxfds:
    File Descriptor Limit - the maximum number of file descriptors
    that can be opened by the current node.

etstabs:
    ETS Tables - the number of ETS tables open on the current node.

maxetstabs:
    ETS Table limit - the maximum number of ETS tables supported on
    the current node.

memtot:
    Total - Bytes of memory allocated by the system (some unused).

memproc:
    Process - Bytes of memory used by erlang process heaps.

memets:
    ETS - Bytes of memory used by ETS tables.

membin:
    Binary - Byte size of the shared binary heap.

memcode:
    Code - Bytes of memory consumed by loaded modules.
