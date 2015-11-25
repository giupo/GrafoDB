#ifndef __DB_ADAPTER_HPP__
#define __DB_ADAPTER_HPP__

#include <pqxx/pqxx>
#include <string>
#include <vector>
#include <Rcpp.h>
#include <json/json.h>

#include "base_adapter.hpp"
#include "utils.hpp"

using namespace std;
using namespace Rcpp;

class DBAdapter : public BaseAdapter {
public:

  DBAdapter(const string username, 
            const string password, 
            const string host, 
            const string port,
            const string dbname, 
            const string tag) : 
    BaseAdapter(username, password, host, port, dbname, tag) {
    this->conn = NULL;
    this->T = NULL;
  }
  
  DBAdapter(string host, string port, string dbname, string tag) : 
    BaseAdapter(host, port, dbname, tag) {
    this->conn = NULL;
    this->T = NULL;
  }
  
  DBAdapter(const DBAdapter& other) : BaseAdapter(other) {
    this->conn = NULL;
    this->T = NULL;
  }
  
  DBAdapter& operator=(const DBAdapter& other) { 
    BaseAdapter::operator=(other);
    return *this;
  }
  
  virtual ~DBAdapter() {
    if(NULL != conn) {
      conn->disconnect();
    }

    if(NULL != T) {
      delete T;
    }
    
    if(NULL != conn) {
      delete conn;
    }
  }
  
  virtual CharacterMatrix getArchi();
  virtual List getData(vector<string> names);

  virtual bool hasHistoricalData();
  virtual List getHistoricalData(vector<string> names);
  virtual List getData();
  virtual vector<string> getNames();
  virtual void commit();
  virtual void init();

  virtual bool hasConflicts(const string name = "");
  virtual DataFrame getConflicts(const string name = "");
  virtual void do_history(const vector<string> names);
protected:  
  pqxx::connection* conn;
  pqxx::work* T;
  
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
        Json::Value root = parseJSON(sDati);
        if (root.size() == 0) {
          z[name] = NumericVector::create();
        } else if(root[0].isNumeric()) {
          z[name] = asNumericVector(root);
        } else {
          z[name] = asCharacterVector(root);
        }
      } else {	
        z[name] = createTimeSeries(anno, periodo, freq, sDati);
      }
    }
    return z;
  }
};

#endif
