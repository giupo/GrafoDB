# Estensione DB per pkg `grafo`

Il package necessita di librerie esterne (listate anche nel file DESCRIPTION):

- libjsoncpp (per serializzare serie storiche)
- libpqxx (per postgres)

Le due librerie sono submoduli di git. Si ottengono con :

```bash
git submodule init
git submodule update
```

E vengono clonati nella directory `ext`. Riferire ad ogni lib su come procedere all'installazione

- libjsoncpp usa `cmake && make`
- libpqxx usa il classico toolchain autotools (`configure && make && make install`). Questa lib dovrebbe
  richiedere `libpq`, che e' facilmente installabile con il package manager di riferimento dell'installazione
  (su OSX usare brew)

*E' FONDMANETALE* che si vada nella directory `src` e si configurino gli header/lib per la compilazione della
parte nativa del package. La libreria viene linkata staticamente alle altre lib su cui dipende


## Compilazione jsoncpp

```bash
cd ext/jsoncpp
mkdir build # FONDAMENTALE CHE SI CHIAMI COSI' (per le dipendenze sul Makevars di R)
cd build
cmake ..
make
```

## Compilazione libpqxx

```bash
cd ext/libpqxx
./configure
make
```

### Problemi con Kerberos

E' consigliabile usare la seguente variabile di sistema (Grazie Demetrio!)

```bash
export PGKRBSRVNAME=POSTGRES
```