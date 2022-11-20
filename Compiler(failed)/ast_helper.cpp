#include "ast_helper.hpp"

/**
 * @brief Converts type enum to string
 *
 * @param type The type to convert
 * @return const std::string The string of the converted type
 */
const std::string TypeToStr(VAR_TYPE type)
{
	switch (type)
	{
	case VOID_TYPE:
		return "void";
	case INT_TYPE:
		return "int";
	case FLOAT_TYPE:
		return "float";
	case BOOL_TYPE:
		return "bool";
	default:
		return "";
	}
}

/**
 * @brief Simple function to check if a map contains a key
 * 
 * @tparam K Generic key of map 
 * @tparam V Generic value of map
 * @param map The map of generic key, value types
 * @param key The generic key in the map 
 * @return true If key found in map 
 * @return false If key not in map 
 */
template <typename K, typename V>
bool mapContainsKey(std::unordered_map<K, V> &map, K key)
{
	if (map.find(key) == map.end())
		return false;
	return true;
}