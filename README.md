Estensione DB per pkg `grafo`
-----------------------------


Il package necessita di librerie esterne (listate anche nel file DESCRIPTION):

- libjsoncpp (per serializzare serie storiche)
- libpqxx (per postgres)

Le due librerie sono submoduli di git (si ottengono con :

`
git submodule init
git submodule update
`

E vengono clonati nella directory `ext`. Riferire ad ogni lib su come procedere all'installazione

- libjsoncpp usa cmake && make
- libpqxx usa il classico toolchain autotools (`configure && make && make install`). Questa lib dovrebbe
  richiedere `libpq`, che e' facilmente installabile con il package manager di riferimento dell'installazione
  (su OSX usare brew)

*E' FONDMANETALE* che si vada nella directory src e si configurino gli header/lib per la compilazione della
parte nativa del package


