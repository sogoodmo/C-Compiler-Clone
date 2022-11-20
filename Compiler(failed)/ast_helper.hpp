#ifndef AST_HELPER_H
#define AST_HELPER_H

#include "common.hpp"

const std::string TypeToStr(VAR_TYPE type);

template <typename K, typename V>
bool mapContainsKey(std::unordered_map<K, V> &map, K key);

#endif