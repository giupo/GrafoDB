#include "db_adapter.hpp"

#include <vector>
#include <string>
#include <Rcpp.h>
#include <ctime>
#include "utils.hpp"

using namespace Rcpp;
using namespace std;

DBAdapter::DBAdapter(string username, string password, string host, 
                       string port, string dbname, string tag) {
   this->conninfo = "user="+username+" password="+ password + 
     " dbname=" + dbname + " host=" + host + " port=" + port;
   this->init();
   this->tag = tag;
}

DBAdapter::DBAdapter(string host, string port, string dbname, string tag) {
  this->conninfo = "dbname=" + dbname + " host=" + host + " port=" + port;
  this->init();
  this->tag = tag;
}
  
DBAdapter::~DBAdapter() {
  conn->disconnect();
  delete T;
  delete conn;
}
  
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
  T->commit();
  return z;
}
  
List DBAdapter::getData(vector<string> names) {
  List z = List::create();   
  vector<string> quotedNames = quote(names);
  string inParams = join(quotedNames, ',');
  
  unsigned int i;
  unsigned int totalSize;
    
  stringstream sql;
  sql << "select name, anno, periodo, freq, dati ";
  sql << "from dati where tag ='" << tag << "' and name in (";
  sql << inParams << ")";
  
  pqxx::result res = T->exec(sql.str()); 
  
  totalSize = res.size();             
  string name;
  double anno;
  double periodo;
  double freq;
  string sDati;

  for (i = 0; i < totalSize; ++i) {        
    res[i]["name"].to(name);
    res[i]["anno"].to(anno);
    res[i]["periodo"].to(periodo);
    res[i]["freq"].to(freq);
    res[i]["dati"].to(sDati);
    z[name] = createTimeSeries(anno, periodo, freq, sDati);
  }

  T->commit();
  return z;
}

List DBAdapter::getData() {
  return getData(this->getNames());
}
  
vector<string> DBAdapter::getNames() {
  unsigned int i;
  unsigned int totalSize;

  const char* sql = "select name from dati where tag = $1";
  conn->prepare("getnames", sql);    
  pqxx::result res = T->prepared("getnames")(tag.c_str()).exec();
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
