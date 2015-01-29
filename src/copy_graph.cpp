#include "copy_graph.hpp"

#include <pqxx/pqxx>
#include <string>
#include <sstream>

#include "utils.hpp"

void copy_graph(std::string fromTag, std::string toTag, pqxx::work* T) {
  std::string commento = "Rilascio per " + toTag;
  std::string autore = whoami();
  
  std::stringstream sql;
  sql << "insert into dati(tag, name, anno, periodo, freq, dati, autore) ";
  sql << " select '" << toTag << "', name, anno, periodo, freq, dati, '";
  sql << autore << "' from dati where tag = '" << fromTag << "'";
  T->exec(sql);
  sql.str(std::string());
  
  sql << "insert into archi(tag, partenza, arrivo, autore) ";
  sql << " select '" <<  toTag <<  "', partenza, arrivo, '" << autore;
  sql << "' from archi where tag = '" << fromTag << "'";
  T->exec(sql);
  sql.str(std::string());
    
  sql << "insert into formule(tag, name, formula, autore) ";
  sql << " select '" << toTag << "', name, formula, '" << autore ;
  sql << "' from formule where tag = '" << fromTag <<  "'";
  T->exec(sql);
  sql.str(std::string());
  
  sql << "insert into metadati(tag, name, key, value, autore) ";
  sql << " select '" << toTag << "', name, key, value, '" << autore;
  sql << "' from metadati where tag = '" << fromTag << "'";
  T->exec(sql);
  sql.str(std::string());
      
  sql << "insert into grafi(tag, commento, last_updated, autore) values (";
  sql << "'" << toTag << "', '" << commento << "', LOCALTIMESTAMP::timestamp(0), '" << autore << "')"; 
  T->exec(sql);
}
