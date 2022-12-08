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

struct NonCopyable
{
	constexpr NonCopyable() noexcept = default;
	NonCopyable(const NonCopyable&) = delete;
	constexpr NonCopyable(NonCopyable&&) noexcept = default;

	NonCopyable& operator=(NonCopyable&&) = default;
	NonCopyable& operator=(const NonCopyable&) = delete;
};

struct Json
{
	using t = std::variant<Null, bool, Number, std::string, Array, Object>;

	bool operator==(const Json& other) const {return value == other.value;};

	t value;
	[[no_unique_address]] NonCopyable _n = {};
};

static_assert(std::is_nothrow_move_constructible_v<Json>);
static_assert(std::is_aggregate_v<Json>);
static_assert(not std::is_copy_constructible_v<Json>);

enum class Error
{
	EmptyString,
	CharMismatch,
	HexCharExpected,
	NullExpected,
	TrueExpected,
	FalseExpected,
	ExponentRequired,
	UnrecognisedEscapeSequence,
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
