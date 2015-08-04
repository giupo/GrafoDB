# Estensione DB per pkg `grafo`

Il package necessita di librerie esterne (listate anche nel file DESCRIPTION):

- libjsoncpp (per serializzare serie storiche)
- libpqxx (per postgres)

Le due librerie sono submoduli di git. Si ottengono con :

```bash
git submodule init
git submodule update
```

## Istruzioni d'installazione:

Aprire una shell, mettersi nella top dir del progetto e dare i seguenti comandi:

```bash
./configure
make deps # builda le librerie su cui dipende
make install # costruisce il tar.gz e lo installa
```

## Compilazione jsoncpp (non necessaria: assolta dal Makefile del progetto)

```bash
cd ext/jsoncpp
mkdir build # FONDAMENTALE CHE SI CHIAMI COSI' (per le dipendenze sul Makevars di R)
cd build
cmake ..
make
```

## Compilazione libpqxx (non necessaria, assolta dal Makefile del progetto)

```bash
cd ext/libpqxx
./configure
make
```

# Importante: Problemi con Kerberos

E' consigliabile usare la seguente variabile di sistema (Grazie Demetrio!)

```bash
export PGKRBSRVNAME=POSTGRES
```