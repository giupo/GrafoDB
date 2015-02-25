#include <Rcpp.h>
#include <vector>
#include <string>
#include "db_adapter.hpp"

using namespace Rcpp;
using namespace std;

// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

//' Carica Archi nativo
//'
//' @name load_archi
//' @usage load_archi(username, password, hostname, port, dbname, tag)
//' @param username username per la connessione
//' @param password password per la connessione
//' @param hostname hostname del database
//' @param port porta di ascolto del server
//' @param dbname nome del db
//' @param tag tag del database da cui caricare le serie
//' @return una matrice di archi
//' @export
//' @import Rcpp
//' @useDynLib GrafoDB
//
// [[Rcpp::export]]
CharacterMatrix  load_archi(SEXP username,
               SEXP password, 
               SEXP hostname,
               SEXP port,
               SEXP dbname,
               SEXP tag) {

  string username0 = as<string>(username); 
  string password0 = as<string>(password); 
  string hostname0 = as<string>(hostname); 
  string port0 = as<string>(port); 
  string dbname0 = as<string>(dbname);
  string tag0 = as<string>(tag);
  
  DBAdapter db(username0, password0, hostname0, port0, dbname0, tag0);
  db.init();
  CharacterMatrix z = db.getArchi();
  db.commit();
  return z;
}
