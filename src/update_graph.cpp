
#include <pqxx/pqxx>
#include <string>

#include "update_graph.hpp"

void update_graph(GrafoDB g, pqxx::work* T) {
  update_graph(g, g.tag, T);
}

void update_graph(GrafoDB g, std::string tag, pqxx::work* T) {
  update_data(g, tag, T);
  update_functions(g, tag, T);
  update_archi(g, tag, T);
}


void update_data(GrafoDB g, std::string tag, pqxx::work* T) {
  
}

void update_functions(GrafoDB g, std::string tag, pqxx::work* T) {
  
}

void update_archi(GrafoDB g, std::string tag, pqxx::work* T) {
  
}

