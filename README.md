haskell-cpu-usage
=================

Minimal application written in Haskell which calculates cpu percent usage from /proc/stat


Compiling
=========

	ghc haskell-cpu-usage.hs -o haskell-cpu-usage.out


Usage
=====

	haskell-cpu-usage.out <number-of-cores>


Help
====

If you have N cores, number-of-cores must be N+1. 1 is reserved for general cpu usage.

	number-of-cores == 1 -> [cpu]
	number-of-cores == 2 -> [cpu, cpu1]
	number-of-cores == 3 -> [cpu, cpu1, cpu2]
	number-of-cores == N -> [cpu, cpu1, cpu2, ..., cpuN-1]

Mail to nnoell3[at]gmail.com for further assistance.
