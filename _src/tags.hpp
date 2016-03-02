#ifndef __TAGS_HPP__
#define __TAGS_HPP__

#include <string>
#include <pqxx/pqxx>

//' controlla se un `tag` esiste nel db
//'
//' @name tagExists
//' @param tag tag da controllare
//' @param T `pqxx::work` che delimita la transazione corrente
//' @return `true` se `tag` e' presente nel DB, altrimenti `false` 
//' @useDynLib GrafoDB.so

bool tagExists(std::string tag, pqxx::work* T);

#endif
