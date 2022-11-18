#include <cstddef>
#include <string>
#include <string_view>
#include <unordered_map>
#include <variant>
#include <vector>
#include <ostream>

struct Null
{
	bool operator==(const Null&) const = default;
};

struct Number
{
	long integer;
	long fraction;
	size_t precision;
	long exponent;

	bool operator==(const Number&) const = default;
};

inline Number operator-(Number n)
{
	n.integer = -n.integer;
	return n;
}

struct Json;

using Array = std::vector<Json>;
using Object = std::unordered_map<std::string, Json>;

struct Json
{
	using t = std::variant<Null, bool, Number, std::string, Array, Object>;

	bool operator==(const Json&) const = default;

	t value;
};

enum class Error
{
	EmptyString,
	CharMismatch, //{ expected: u8, actual: u8 },
	HexCharExpected,
	NullExpected,
	TrueExpected,
	FalseExpected,
	ExponentRequired,
	UnrecognisedEscapeSequence, //(u8),
	InvalidValue,
	OutOfBounds,
	Garbage,
	Failure,
};

template<typename T>
using Ok = std::pair<T, std::string_view>;
using Err = std::pair<Error, std::string_view>;

template<typename T>
using Result = std::variant<Ok<T>, Err>;

using JsonPart = Result<Json>;

using JsonResult = std::variant<Json, Err>;

inline Json null()
{
	return Json{Null{}};
}

inline Json True()
{
	return Json{true};
}

inline Json False()
{
	return Json{false};
}

auto parse(std::string_view s) -> JsonResult;

std::ostream& operator<<(std::ostream& o, const Json& json);
std::ostream& operator<<(std::ostream& o, const Error& error);
