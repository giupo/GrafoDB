#include "db_adapter.hpp"

#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <Rcpp.h>
#include <ctime>
#include "utils.hpp"

using namespace Rcpp;
using namespace std;

CharacterMatrix DBAdapter::getArchi() {
  unsigned int i;
  unsigned int totalSize;
  
  const char* sql = "select partenza, arrivo "
    "from archi where tag = $1";
  
  conn->prepare("getarchi", sql);    
  pqxx::result res = T->prepared("getarchi")(tag.c_str()).exec();
  totalSize = res.size();
  CharacterMatrix z(totalSize, 2);  
  string partenza;
  string arrivo;
  
  for (i = 0; i < totalSize; ++i) {        
    res[i]["partenza"].to(partenza);
    res[i]["arrivo"].to(arrivo);    
    z(i,0) = partenza;
    z(i,1) = arrivo;
  }
  return z;
}
  
List DBAdapter::getData(vector<string> names) {  
  vector<string> quotedNames = quote(names);
  string inParams = join(quotedNames, ',');
  
  stringstream sql;
  sql << "select name, anno, periodo, freq, dati ";
  sql << "from dati where tag ='" << tag << "' and name in (";
  sql << inParams << ")";
  
  List z = this->internalGetDataWithQuery(names, sql.str());  

  // get historical data
  if(this->hasHistoricalData()) {
    List historical = this->getHistoricalData(names);
    if(historical.size() > 0) {
      Rprintf("Loading data from history\n");
      CharacterVector hNames = historical.names();
      for(CharacterVector::iterator it = hNames.begin(); 
          it != hNames.end(); ++it) {
        string name = as<string>(*it);
        z[name] = historical[name];
      }  
    }
  }

  return z;
}

List DBAdapter::getData() {
  return getData(this->getNames());
}
  
vector<string> DBAdapter::getNames() {
  unsigned int i;
  unsigned int totalSize;
  
  const char* sql = "select partenza from archi, where tag = $1" \
    " union select arrivo from archi where tag = $2 " \
    " union select name from dati where tag=$3";
  conn->prepare("getnames", sql);    
  const char *ctag = tag.c_str();
  pqxx::result res = T->prepared("getnames")(ctag)(ctag)(ctag).exec();
  totalSize = res.size();
  vector<string> z;
  string name;
  for (i = 0; i < totalSize; ++i) {        
    res[i]["name"].to(name);
    z.push_back(name);
  }
  return z;
}

void DBAdapter::init() {
  time_t now = time(NULL);
  tm* ptm = std::localtime(&now); 
  char buffer[32];
  std::strftime(buffer, 32, "%a, %d.%m.%Y %H:%M:%S", ptm);
  string tname = whoami() + " " + string(buffer);
  this->conn = new pqxx::connection(conninfo);
  this->T = new pqxx::work(*conn, tname);
}

bool DBAdapter::hasHistoricalData() {
  return this->ordinal !=  0;
}

List DBAdapter::getHistoricalData(vector<string> names) {
  vector<string> quotedNames = quote(names);
  string inParams = join(quotedNames, ',');     

  stringstream sql;
  sql << "select name, anno, periodo, freq, dati ";
  sql << "from history where tag ='" << tag << "' and name in (";
  sql << inParams << ") and ordinale = " << this->ordinal;    
  List z = this->internalGetDataWithQuery(names, sql.str());  
  return z;
}

void DBAdapter::commit() {
  T->commit();
}

bool DBAdapter::hasConflicts(const string name) {
  stringstream sql;
  pqxx::result res;
  if(name.empty()) {
    sql << "select count(name) from conflitti where tag = $1";
    conn->prepare("hasConflicts", sql.str());
    res = T->prepared("hasConflictsByName")(this->tag).exec();
  } else {
    sql << "select count(name) from conflitti where tag = $1 and name = $2";
    conn->prepare("hasConflictsByName", sql.str());
    res = T->prepared("hasConflictsByName")(this->tag)(name).exec();
  }
  unsigned int count = 0;
  res[0][0].to(count);
  return count > 0;  
}

DataFrame DBAdapter::getConflicts(const string name)  {
  stringstream sql;
  pqxx::result res;
  if(name.empty()) {
    sql << " select a.name, a.tag, a.formula, a.autore, date, ";
    sql << " b.formula as current_formula, b.autore as current_autore, b.last_updated";
    sql << " from conflitti a, formule b ";
    sql << " where a.tag = $1 and a.tag = b.tag and a.name = b.name";
    sql << " order by tag, name";
    conn->prepare("getConflicts", sql.str());
    res = T->prepared("getConflicts")(this->tag).exec();
  } else {
    sql <<" select a.name, a.tag, a.formula, a.autore, date, b.formula, b.autore, b.last_updated";
    sql << " from conflitti a, formule b ";
    sql << " where a.tag = $1 and a.name=? and a.tag = b.tag and a.name = b.name";
    sql << " order by tag, name";
    conn->prepare("getConflictsByName", sql.str());
    res = T->prepared("getConflictsByName")(this->tag)(name).exec();
  }

  unsigned int size = res.size(); 
  
  string name1;
  string tag;
  string formula1;
  string autore1;
  string date1;
  string formula2;
  string autore2;
  string date2;
  
  for(unsigned int i=0; i<size; ++i) {
    res[i][0].to(name1);
    res[i][1].to(tag);
    res[i][2].to(formula1);
    res[i][3].to(autore1);
    res[i][4].to(date1);
    res[i][5].to(formula2);
    res[i][6].to(autore2);
    res[i][7].to(date2);
  }
  return DataFrame::create();
};
