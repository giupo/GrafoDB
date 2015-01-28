#ifndef __SAVEGRAPH_HPP__
#define __SAVEGRAPH_HPP__

#include <string>
#include <pqxx/pqxx>

#include "grafodb.hpp"

void save_graph(GrafoDB x, std::string tag, pqxx::work* T);

#endif
