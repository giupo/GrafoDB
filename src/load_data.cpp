#include <Rcpp.h>
#include <iostream>
#include <vector>
#include <string>
#include <sstream>

#include "utils.hpp"
#include "db_adapter.hpp"

using namespace Rcpp;
using namespace std;

// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

//' Load Nativo
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
  DBAdapter db(username0, password0, hostname0, port0, dbname0, tag0);
  db.init(); 
  List z = db.getData(names0);
  db.commit();
  return z;
}
