## Mir development repo

Mir is a currently WIP programming language.

Mir :
	- Is compiled to fast machine code
	- Uses fully dynamic, strong typing
	- Supports massive concurrency, a la Erlang/Go
		- Unique, synchronous RPC-based actor model
		- Emphasis on easy fault-tolerance for network applications
	- Is portable to any platform with a C compiler; it compiles to C code.
	- Easily interoperates with C libraries

Mir is the *peaceful programming language*:
	- **Peace of mind for critical apps**: Synchronous actor communications means you don't have to worry about common issues like waiting for messages from dead actors, trying to broadcast "die now" messages using global channels (I'm looking at you, Go), etc. 
	- **It comes in peace**: Mir supports a wide range of programming styles, from functional to procedural to object-oriented. It's easily extensible with powerful macros (without having Lisp parantheses involved!) to suit your specific application.
	- **In peace with your existing ecosystem**: Mir is extremely portable, and operates easily with C. Programs require no heavy runtime, or installation into special directories, to work out of the box. Programming a tiny MIPS router? A Raspberry Pi-powered drone? Or just a regular boring Linux console app on the PC? Mir has you covered.

And yes, Mir, the Russian word, also means "world". But we didn't want to steal Debian's tagline :P
