#include "db_adapter.hpp"

#include <iostream>
#include <vector>
#include <string>
#include <Rcpp.h>
#include <ctime>
//#include <regex.h>
#include "utils.hpp"
#include <regex>
using namespace Rcpp;
using namespace std;

DBAdapter::DBAdapter(string username, string password, string host, 
                       string port, string dbname, string tag) {
   this->conninfo = "user="+username+" password="+ password + 
     " dbname=" + dbname + " host=" + host + " port=" + port;
   this->init();
   this->tag = tag;
   this->matchOrdinal();
}

DBAdapter::DBAdapter(string host, string port, string dbname, string tag) {
  this->conninfo = "dbname=" + dbname + " host=" + host + " port=" + port;
  this->init();
  this->tag = tag;
  this->matchOrdinal();
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
    Rprintf("Loding data from history\n");
    List historical = this->getHistoricalData(names);
    CharacterVector hNames = historical.names();
    for(CharacterVector::iterator it = hNames.begin(); 
        it != hNames.end(); ++it) {
      string name = as<string>(*it);
      z[name] = historical[name];
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

bool DBAdapter::hasHistoricalData() {
  return this->ordinal !=  0;
}

void DBAdapter::matchOrdinal() {
  cout << "called" << endl;
  try {
    std::regex re("p(\\d+)$");
    std::sregex_iterator next(tag.begin(), tag.end(), re);
    std::sregex_iterator end;
    while (next != end) {
      std::smatch match = *next;
      std::cout << match.str() << "\n";
      next++;
    } 
  } catch (std::regex_error& e) {
    cout << e.what() << endl;
  } 
/* regex_t regex;
  int reti;
  regmatch_t pmatch[1];

  reti = regcomp(&regex, "p(\\d+)$", 0);
  if (reti) {
    stop("Could not compile regex");
  }
  
  reti = regexec(&regex, this->tag.c_str(), 1, pmatch, 0);
  if (!reti) {
    unsigned int start = pmatch[0].rm_so;
    unsigned int finish =  pmatch[0].rm_eo;
    Rprintf("Fuck you found : %u %u\n", start, finish);
    this->ordinal = (unsigned int) atoi(this->tag.substr(start, finish).c_str());
    this->tag = this->tag.substr(0, start);
  } else {
    this->ordinal = 0;
  }
  regfree(&regex);*/
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
