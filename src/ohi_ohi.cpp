class DBAdapter;

#include <Rcpp.h>
#include <vector>
#include <string>
#include "db_adapter.hpp"

using namespace Rcpp;
using namespace std;



// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

//' Istanzia il Grafo in C++
//'
//' @name db_adapter
//' @usage db_adapter(username, password, hostname, port, dbname, tag)
//' @param username username per la connessione
//' @param password password per la connessione
//' @param hostname hostname del database
//' @param port porta di ascolto del server
//' @param dbname nome del db
//' @param tag tag del database da cui caricare le serie
//' @return un puntatore al grafo
//' @export
//' @import Rcpp
//' @useDynLib GrafoDB

// [[Rcpp::export]]
RCPP_MODULE(GrafoDB) {
  using namespace std;
  class_<DBAdapter>("DBAdapter")
    .constructor<string, string, string, string, string, string>()
    .method("getArchi", &DBAdapter::getArchi);
}
