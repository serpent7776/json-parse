#include "json.hpp"
#include "matching.hpp"

#include <fstream>
#include <iostream>

std::string read_file(const char* file_name)
{
	std::ifstream f(file_name);
	std::string contents{std::istreambuf_iterator<char>(f), std::istreambuf_iterator<char>()};
	return contents;
}

int main(int argc, char *argv[])
{
	if (argc < 2) return 1;
	const bool print = argv[1] == std::string("-p");
	const char* file = argv[2];
	std::string json_str = read_file(file);
	match(parse(json_str),
		[&](Json json){
			if (print) std::cout << json << "\n";
			else std::cout << "OK\n";
		},
		[](Error e, std::string_view){
			std::cout << "Error " << e << "\n";
		});
	
	return 0;
}
