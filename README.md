haskell-cpu-usage
=================

Minimal application written in Haskell which calculates cpu percent usage from /proc/stat


Build
=====

	ghc haskell-cpu-usage.hs -o haskell-cpu-usage.out


Usage
=====

	haskell-cpu-usage.out <num-cores> <regresh-rate> <output-file>

If you have N cores, number-of-cores must be N+1. 1 is reserved for general cpu usage.

	number-of-cores == 1 -> [cpu]
	number-of-cores == 2 -> [cpu, cpu1]
	number-of-cores == 3 -> [cpu, cpu1, cpu2]
	number-of-cores == N -> [cpu, cpu1, cpu2, ..., cpuN-1]

The regresh rate is in seconds.

If you pass an empty string `""` as output file, the cpu usage will be printed on screen.


Example:

	./haskell-cpu-usage.out 5 1 "/tmp/haskell-cpu-usage.txt"
