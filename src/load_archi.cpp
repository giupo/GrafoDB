#include <Rcpp.h>
#include <string.h>
#include <iostream>
#include <stdlib.h>
#include <pthread.h>
#include <vector>
#include <string>
#include <pqxx/pqxx>

using namespace pqxx;
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
  string conninfo = "user="+username0+" password="+password0 +
    " dbname="+dbname0 + " host="+hostname0+" port="+port0; 
  string tag0 = as<string>(tag);
  
  unsigned int i;
  unsigned int totalSize;
  
  pqxx::connection conn(conninfo);
  const char* sql = "select partenza, arrivo "
                    "from archi where tag = $1";
  
  conn.prepare("getarchi", sql);  
  pqxx::work T(conn, "DemoTransaction");
  

  pqxx::result res = T.prepared("getarchi")(tag0.c_str()).exec();
  totalSize = res.size();
  CharacterMatrix z(totalSize, 2);  
  for (i = 0; i < totalSize; ++i) {    
    string partenza;
    string arrivo;
    
    res[i]["partenza"].to(partenza);
    res[i]["arrivo"].to(arrivo);
    
    z(i,0) = partenza;
    z(i,1) = arrivo;
  }
  
  T.commit();
  conn.disconnect();
  return z ;
}
