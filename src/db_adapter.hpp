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
  List getData();
  vector<string> getNames();
  
private:  
  std::string conninfo;
  std::string tag;
  pqxx::connection* conn;
  pqxx::work* T;
};

#endif
