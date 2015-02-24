#ifndef __DB_ADAPTER_HPP__
#define __DB_ADAPTER_HPP__

#include <pqxx/pqxx>
#include <string>
#include <vector>
#include <Rcpp.h>
#include "utils.hpp"

using namespace std;
using namespace Rcpp;

class DBAdapter {
public:
  DBAdapter(string username, string password, string host, 
            string port, string dbname, string tag);
  DBAdapter(string host, string port, string dbname, string tag);
  
  ~DBAdapter();
  
  CharacterMatrix getArchi();
  List getData(vector<string> names);

  bool hasHistoricalData();
  List getHistoricalData(vector<string> names);
  List getData();
  vector<string> getNames();
  void commit();
protected:
  void init();
  void matchOrdinal();
private:  
  std::string conninfo;
  std::string tag;
  pqxx::connection* conn;
  pqxx::work* T;
  unsigned int ordinal;

  List internalGetDataWithQuery(vector<string> names, string sql) {  
    List z = List::create();  
    pqxx::result res = T->exec(sql); 
    
    unsigned int i;
    unsigned int totalSize = res.size();             
    
    string name;
    double anno;
    double periodo;
    double freq;
    string sDati;
    double tol = 0.00001;
    for (i = 0; i < totalSize; ++i) {        
      res[i]["name"].to(name); 
      res[i]["anno"].to(anno);
      res[i]["periodo"].to(periodo);
      res[i]["freq"].to(freq);
      res[i]["dati"].to(sDati);
     
      if(anno < tol  || periodo < tol || freq < tol) {
	z[name] = parseJSON(sDati);
      } else {	
	z[name] = createTimeSeries(anno, periodo, freq, sDati);
      }
    }
    return z;
  }
};

#endif
