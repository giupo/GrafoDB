#include <Rcpp.h>
#include <json/json.h>
#include <iostream>
#include <stdlib.h>
#include <vector>
#include <string>
#include <pqxx/pqxx>
#include <sstream>
#include "utils.hpp"

using namespace pqxx;
using namespace Rcpp;
using namespace std;

List workerFunction(string m_username, string m_password, string m_hostname, 
                    string m_port, string m_dbname, vector<string> names, string tag) {
  List z = List::create();
  Json::Value root; // will contains the root value after parsing.
  Json::Value value; // this will contain each value while parsing
  Json::Reader reader;
  Json::StyledWriter styledWriter;
   
  vector<string> quotedNames = quote(names);
  string inParams = join(quotedNames, ',');
  string conninfo = "user=" + m_username + " password="+ m_password +
    " dbname=" + m_dbname + " host=" + m_hostname+" port="+ m_port; ;
  
  unsigned int i, j;
  unsigned int totalSize;
  
  
  stringstream sql;
  sql << "select name, anno, periodo, freq, dati ";
  sql << "from dati where tag ='" << tag << "' and name in (";
  sql << inParams << ")";
  
  pqxx::connection conn(conninfo);
  pqxx::work T(conn, "DemoTransaction");
  pqxx::result res = T.exec(sql.str()); 
  
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
    
    reader.parse(sDati, root);    
    
    // for tsp
    double start = anno + periodo/freq - 1/freq;
    double end = start + root.size()/freq - 1/freq; 
    
    vector<double> buffer(root.size());
    NumericVector dati(root.size()); 
    for(j = 0; j < buffer.size(); ++j) {
      value = root[j];
      if(value.isNull()) {
        dati[j] = NA_REAL;
      } else {
        dati[j] = value.asDouble();
      }
    }
    
    dati.attr("tsp") = NumericVector::create(start, end, freq);
    dati.attr("class") = "ts";
    z[name] = dati;
  }      
  T.commit();
  conn.disconnect();
  return z;
}


class Worker {
private:
  List z;
  string m_tag;
  vector<string> m_names;
  string m_username;
  string m_password;
  string m_hostname;
  string m_port;
  string m_dbname;

public:
  Worker(string username,
         string password,
         string hostname,
         string port,
         string dbname,
         vector<string> names, 
         string tag): m_username(username),
                      m_password(password),
                      m_hostname(hostname),
                      m_port(port),
                      m_dbname(dbname) {
    z = List::create();    
    m_tag = tag;
    m_names = names;
  }
  
  List getResults() {
    return z;
  }
  
  List operator()() {
    z = workerFunction(m_username, m_password, m_hostname, m_port, 
                       m_dbname, m_names, m_tag);
    return getResults();
  }  
};


// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

//' shit full
//'
//' @name load_data
//' @usage load_data(username, password, hostname, port, dbname, names, tag)
//' @param username username per la connessione
//' @param password password per la connessione
//' @param hostname hostname del database
//' @param port porta di ascolto del server
//' @param dbname nome del db
//' @param names nomi di serie da caricare
//' @param tag tag del database da cui caricare le serie
//' @return a list
//' @export
//' @import Rcpp
//' @useDynLib GrafoDB
//
// [[Rcpp::export]]

List load_data(SEXP username, SEXP password, SEXP hostname, 
               SEXP port, SEXP dbname, SEXP names, SEXP tag) {
  
  string username0 = as<string>(username); 
  string password0 = as<string>(password); 
  string hostname0 = as<string>(hostname); 
  string port0 = as<string>(port); 
  string dbname0 = as<string>(dbname);  
  vector<string> names0 = as<vector<string> >(names);
  string tag0 = as<string>(tag);
  return workerFunction(username0, password0, hostname0, port0, 
                        dbname0, names0, tag0);
}
