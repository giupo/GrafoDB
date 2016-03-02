#ifndef __CREATEGRAPH_HPP__
#define __CREATEGRAPH_HPP__

#include <string>
#include <pqxx/pqxx>

#include "grafodb.hpp"

void create_graph(GrafoDB x, std::string tag, pqxx::work* T);

#endif
