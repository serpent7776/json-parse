#include <algorithm>
#include <optional>
#include <stdexcept>
#include <utility>
#include <variant>

#include "json.hpp"
#include "matching.hpp"

template<typename T>
Result<T> ok(T val, std::string_view s)
{
	return Ok<T>{std::move(val), s};
}
template<typename T>
Result<T> err(Error e, std::string_view s)
{
	return Err{e, s};
}

template<typename T>
JsonPart part(T val, std::string_view s)
{
	return JsonPart{ok(Json{std::move(val)}, s)};
}
JsonPart nth(Error e, std::string_view s)
{
	return JsonPart{Err{e, s}};
}

auto parse_value(std::string_view s) -> JsonPart;

bool is_ws(char c)
{
	switch (c)
	{
		case ' ':
		case '\t':
		case '\n':
		case '\r':
			return true;
		default:
			return false;
	}
}

bool is_alpha(char ch)
{
	switch (ch)
	{
		case 'a'...'z': return true;
		case 'A'...'Z': return true;
		default: return false;
	}
}

bool is_digit(char ch)
{
	switch (ch)
	{
		case '0'...'9': return true;
		default: return false;
	}
}

bool is_hex(char ch)
{
	switch (ch)
	{
		case '0'...'9': return true;
		case 'a'...'f': return true;
		case 'A'...'F': return true;
		default: return false;
	}
}

std::string_view next(std::string_view s, std::size_t increment = 1)
{
	return s.substr(increment);
}

template<typename F>
auto take(std::string_view s, F&& f) -> Result<std::string>
{
	const auto begin = std::begin(s);
	const auto end = std::end(s);
	const auto it = std::find_if_not(begin, end, std::forward<F>(f));
	if (it != begin)
	{
		return ok(std::string(begin, it), {it, end});
	}
	return Err{Error::EmptyString, s};
}

template<typename F>
auto ask(std::string_view s, F&& f) -> Result<std::string>
{
	std::vector<char> buf;
	buf.reserve(64);
	std::string_view span = s;
	while (not span.empty())
	{
		const auto r = f(span);
		if (std::holds_alternative<Err>(r))
		{
			auto [e, s] = std::get<Err>(r);
			return Err{e, s};
		}
		else if (std::holds_alternative<Ok<std::optional<char>>>(r))
		{
			auto [ch, cont] = std::get<Ok<std::optional<char>>>(r);
			if (ch.has_value())
			{
				buf.push_back(ch.value());
				span = cont;
			}
			else break;
		}
		else return Err(Error::Failure, s);
	}
	return ok(std::string(std::begin(buf), std::end(buf)), span);
}

template<typename F>
auto skip(std::string_view s, F&& f) -> std::string_view
{
	const auto begin = std::begin(s);
	const auto end = std::end(s);
	auto it = std::find_if_not(begin, end, std::forward<F>(f));
	return {it, end};
}

auto skip_ws(std::string_view s)
{
	return skip(s, is_ws);
}

auto chr(std::string_view s, char ch) -> Result<char>
{
	if (s.empty()) return Err{Error::OutOfBounds, s};
	if (s.front() == ch) return Ok<char>{ch, next(s)};
	return Err{Error::CharMismatch, s};
}

JsonPart parse_null(std::string_view s)
{
	return match(take(s, is_alpha),
		[](std::string&& str, std::string_view s) {
			if (str == "null") return part(null(), s);
			else return nth(Error::EmptyString, s);
		},
		[](Error e, std::string_view s) {
			return nth(e, s);
		});
}

JsonPart parse_true(std::string_view s)
{
	return match(take(s, is_alpha),
		[](std::string&& str, std::string_view s) {
			if (str == "true") return part(true, s);
			else return nth(Error::TrueExpected, s);
		},
		[](Error e, std::string_view s) {
			return nth(e, s);
		});
}

JsonPart parse_false(std::string_view s)
{
	return match(take(s, is_alpha),
		[](std::string&& str, std::string_view s) {
			if (str == "false") return part(false, s);
			else return nth(Error::FalseExpected, s);
		},
		[](Error e, std::string_view s) {
			return nth(e, s);
		});
}

auto parse_fraction(std::string_view s) -> Result<std::pair<long, size_t>>
{
	return match(take(s, is_digit),
		[](std::string&& str, std::string_view s) {
			const long fraction = stol(str);
			const size_t precision = str.length();
			return ok(std::pair(fraction, precision), s);
		},
		[](Error, std::string_view s) {
			return ok(std::pair(0l, 0uz), s);
		});
}

auto parse_exponent(std::string_view s) -> Result<long>
{
	return match(take(s, is_digit),
		[](std::string&& str, std::string_view s){
			const long exponent = stol(str);
			return ok(exponent, s);
		},
		[](Error, std::string_view s){
			return err<long>(Error::ExponentRequired, s);
		});
}

auto parse_number_parts(std::string_view s) -> Result<Number>
{
	return match(take(s, is_digit),
		[](std::string&& int_str, std::string_view s) {
			const long integer = stol(int_str);
			auto r = match(chr(s, '.'),
				[](char, std::string_view s){
					 return parse_fraction(s);
				},
				[](Error, std::string_view s){
					return ok(std::pair(0l, 0uz), s);
				});
			 return match(std::move(r),
				[&](std::pair<long, size_t>&& p, std::string_view s){
					const auto [fraction, precision] = p;
					const auto r = match(chr(s, 'e'),
						[](char, std::string_view s){
							return parse_exponent(s);
						},
						[](Error, std::string_view s){
							return Result<long>(ok(0l, s));
						});
					return match(r,
						[=, fraction=fraction, precision=precision](long exponent, std::string_view s){
							return ok(Number{.integer=integer, .fraction=fraction, .precision=precision, .exponent=exponent}, s);
						},
						[](Error e, std::string_view s){
							return err<Number>(e, s);
						});
				},
				[](Error e, std::string_view s){
					return err<Number>(e, s);
				});
		},
		[](Error e, std::string_view s) {
			return err<Number>(e, s);
		});
}

JsonPart parse_number(std::string_view s)
{
	return match(parse_number_parts(s),
		[](Number&& num, std::string_view s){
			return part(std::move(num), s);
		},
		[](Error e, std::string_view s){
			return nth(e, s);
		});
}

JsonPart parse_negative_number(std::string_view s)
{
	return match(chr(s, '-'),
		[](char, std::string_view s){
			return match(parse_number_parts(s),
				[](Number&& num, std::string_view s){
					return part(-num, s);
				},
				[](Error e, std::string_view s){
					return nth(e, s);
				});
		},
		[](Error e, std::string_view s){
			return nth(e, s);
		});
}

auto hexword(std::string_view s) -> Result<char>
{
	if (s.length() < 4) return err<char>(Error::OutOfBounds, s);
	if (not is_hex(s[0])) return err<char>(Error::HexCharExpected, s);
	if (not is_hex(s[1])) return err<char>(Error::HexCharExpected, next(s, 1));
	if (not is_hex(s[2])) return err<char>(Error::HexCharExpected, next(s, 2));
	if (not is_hex(s[3])) return err<char>(Error::HexCharExpected, next(s, 3));
	return ok('?', next(s, 4));
}

auto string_char(std::string_view s) -> Result<std::optional<char>>
{
	if (s.empty()) return err<std::optional<char>>(Error::OutOfBounds, s);
	const char ch = s[0];
	switch (ch)
	{
		case '"': return ok<std::optional<char>>(std::nullopt, next(s));
		case '\\':
		{
			auto cont = next(s);
			if (cont.length() < 1) return err<std::optional<char>>(Error::OutOfBounds, cont);
			switch (cont[0])
			{
				case '"': return ok(std::optional('"'), next(cont));
				case '\\': return ok(std::optional('\\'), next(cont));
				case '/': return ok(std::optional('/'), next(cont));
				case 'b': return ok(std::optional('\b'), next(cont));
				case 'f': return ok(std::optional('\f'), next(cont));
				case 'n': return ok(std::optional('\n'), next(cont));
				case 'r': return ok(std::optional('\r'), next(cont));
				case 't': return ok(std::optional('\t'), next(cont));
				case 'u':
				{
					return match(hexword(next(cont)),
						[](char c, std::string_view s){
							return ok(std::optional(c), s);
						},
						[](Error e, std::string_view s){
							return err<std::optional<char>>(e, s);
						});
				}
				default: return err<std::optional<char>>(Error::UnrecognisedEscapeSequence, s);
			}
		}
		default: return ok(std::optional(ch), next(s));
	}
}

auto parse_string_raw(std::string_view s) -> Result<std::string>
{
	return match(chr(s, '"'),
		[](char, std::string_view s){
			return match(ask(s, string_char),
				[](std::string&& str, std::string_view s){
					return match(chr(s, '"'),
						[&](char, std::string_view s){
							return ok<std::string>(std::move(str), s);
						},
						[](Error e, std::string_view s){
							return err<std::string>(e, s);
						});
				},
				[](Error e, std::string_view s){
					return err<std::string>(e, s);
				});
		},
		[](Error e, std::string_view s){
			return err<std::string>(e, s);
		});
}

auto parse_string(std::string_view s) -> JsonPart
{
	return match(parse_string_raw(s),
		[](std::string&& str, std::string_view s){
			return part(std::move(str), s);
		},
		[](Error e, std::string_view s){
			return nth(e, s);
		});
}

auto parse_array_items_tail(Json&& head, std::string_view s) -> Result<Array>
{
	Array buf;
	buf.reserve(8);
	buf.push_back(std::move(head));
	std::string_view cont = s;
	while (not cont.empty())
	{
		const auto r = chr(skip_ws(cont), ',');
		if (std::holds_alternative<Err>(r))
		{
			auto [e, s] = std::get<Err>(r);
			return ok(std::move(buf), s);
		}
		else if (std::holds_alternative<Ok<char>>(r))
		{
			auto [c, s] = std::get<Ok<char>>(r);
			auto r = parse_value(s);
			if (std::holds_alternative<Err>(r))
			{
				auto [e, s] = std::get<Err>(r);
				return err<Array>(e, s);
			}
			else if (std::holds_alternative<Ok<Json>>(r))
			{
				auto&& [json, s] = std::get<Ok<Json>>(r);
				buf.push_back(std::move(json));
				cont = s;
			}
			else return err<Array>(Error::Failure, cont);
		}
		else return err<Array>(Error::Failure, cont);
	}
	return err<Array>(Error::OutOfBounds, cont);
}

auto parse_array_items(std::string_view s) -> Result<Array>
{
	return match(parse_value(s),
		[](Json&& json, std::string_view s){
			return parse_array_items_tail(std::move(json), s);
		},
		[](Error, std::string_view s){
			return ok(Array(), s);
		});
}

auto parse_array(std::string_view s) -> JsonPart
{
	return match(chr(s, '['),
		[](char, std::string_view s){
			return match(parse_array_items(s),
				[](Array&& items, std::string_view s){
					return match(chr(skip_ws(s), ']'),
						[&](char, std::string_view s){
							return part(std::move(items), s);
						},
						[](Error e, std::string_view s){
							return nth(e, s);
						});
				},
				[](Error e, std::string_view s){
					return nth(e, s);
				});
		},
		[](Error e, std::string_view s){
			return nth(e, s);
		});
}

auto parse_object_items_tail(std::string key, Json&& value, std::string_view s) -> Result<Object>
{
	std::string_view cont = s;
	Object buf;
	buf.reserve(8);
	buf.emplace(std::move(key), std::move(value));
	while (not cont.empty())
	{
		const auto r = chr(skip_ws(cont), ',');
		if (std::holds_alternative<Err>(r))
		{
			auto [e, s] = std::get<Err>(r);
			return ok(std::move(buf), s);
		}
		else if (std::holds_alternative<Ok<char>>(r))
		{
			auto [c, s] = std::get<Ok<char>>(r);
			auto r = parse_string_raw(skip_ws(s));
			if (std::holds_alternative<Err>(r))
			{
				auto [e, s] = std::get<Err>(r);
				return err<Object>(e, s);
			}
			else if (std::holds_alternative<Ok<std::string>>(r))
			{
				auto&& [key, s] = std::get<Ok<std::string>>(r);
				const auto r = chr(skip_ws(s), ':');
				if (std::holds_alternative<Err>(r))
				{
					auto [e, s] = std::get<Err>(r);
					return err<Object>(e, s);
				}
				else if (std::holds_alternative<Ok<char>>(r))
				{
					auto [c, s] = std::get<Ok<char>>(r);
					auto r = parse_value(s);
					if (std::holds_alternative<Err>(r))
					{
						auto [e, s] = std::get<Err>(r);
						return err<Object>(e, s);
					}
					else if (std::holds_alternative<Ok<Json>>(r))
					{
						auto&& [value, s] = std::get<Ok<Json>>(r);
						cont = s;
						buf.insert_or_assign(std::move(key), std::move(value));
					}
					else return err<Object>(Error::Failure, cont);
				}
				else return err<Object>(Error::Failure, cont);
			}
			else return err<Object>(Error::Failure, cont);
		}
		else return err<Object>(Error::Failure, cont);
	}
	return err<Object>(Error::OutOfBounds, cont);
}

auto parse_object_items(std::string_view s) -> Result<Object>
{
	return match(parse_string_raw(skip_ws(s)),
		[](std::string&& key, std::string_view s){
			return match(chr(skip_ws(s), ':'),
				[&](char, std::string_view s){
					return match(parse_value(s),
						[&](Json&& value, std::string_view s){
							return parse_object_items_tail(std::move(key), std::move(value), s);
						},
						[](Error e, std::string_view s){
							return err<Object>(e, s);
						});
				},
				[](Error e, std::string_view s){
					return err<Object>(e, s);
				});
		},
		[](Error, std::string_view s){
			return ok(Object(), s);
		});
}

auto parse_object(std::string_view s) -> JsonPart
{
	return match(chr(s, '{'),
		[](char, std::string_view s){
			return match(parse_object_items(s),
				[](Object&& items, std::string_view s){
					return match(chr(skip_ws(s), '}'),
						[&](char, std::string_view s){
							return part(std::move(items), s);
						},
						[](Error e, std::string_view s){
							return nth(e, s);
						});
				},
				[](Error e, std::string_view s){
					return nth(e, s);
				});
		},
		[](Error e, std::string_view s){
			return nth(e, s);
		});
}

auto parse_value(std::string_view sv) -> JsonPart
{
	std::string_view s = skip_ws(sv);
	switch (s[0])
	{
		case 'n': return parse_null(s);
		case 't': return parse_true(s);
		case 'f': return parse_false(s);
		case '0'...'9': return parse_number(s);
		case '-': return parse_negative_number(s);
		case '"': return parse_string(s);
		case '[': return parse_array(s);
		case '{': return parse_object(s);
	}
	return nth(Error::InvalidValue, s);
}

auto parse(std::string_view sv) -> JsonResult
{
	std::string_view s = skip_ws(sv);
	if (s.empty()) return JsonResult(Err{Error::EmptyString, s});
	return match(parse_value(s),
		[](Json&& json, std::string_view sv){
			std::string_view s = skip_ws(sv);
			if (s.empty()) return JsonResult{std::move(json)};
			else return JsonResult{Err{Error::Garbage, s}};
		},
		[](Error e, std::string_view s){
			return JsonResult{Err{e, s}};
		});
}

auto escape(const std::string& str)
{
	std::string buf;
	buf.reserve(str.length());
	for (auto ch: str)
	{
		switch (ch)
		{
			case '\"': buf += "\\\""; break;
			case '\\': buf += "\\\\"; break;
			case '\r': buf += "\\r"; break;
			case '\n': buf += "\\n"; break;
			case '\t': buf += "\\t"; break;
			case '\b': buf += "\\b"; break;
			case '\f': buf += "\\f"; break;
			default: buf += ch;
		}
	}
	return buf;
}

std::ostream& operator<<(std::ostream& o, Null)
{
	return o << "null";
}

std::ostream& operator<<(std::ostream& o, const Number& num)
{
	auto [integer, fraction, precision, exponent] = num;
	o << integer;
	if (precision > 0) o << '.' << fraction;
	if (exponent != 0) o << 'e' << exponent;
	return o;
}

std::ostream& operator<<(std::ostream& o, const Array& arr)
{
	o << '[';
	if (arr.size() > 0)
	{
		o << arr.front();
		if (arr.size() > 1)
		{
			for (auto it = std::begin(arr); it != std::end(arr); ++it) o << ", " << *it;
		}
	}
	o << ']';
	return o;
}

std::ostream& operator<<(std::ostream& o, const Object& obj)
{
	o << '{';
	if (obj.size() > 0)
	{
		auto& [key, value] = *obj.begin();
		o << '"' << escape(key) << '"' << ": " << value;
		if (obj.size() > 1)
		{
			for (auto it = std::begin(obj); it != std::end(obj); ++it)
			{
				auto& [key, value] = *it;
				o << ", \"" << escape(key) << "\": " << value;
			}
		}
	}
	o << '}';
	return o;
}

std::ostream& operator<<(std::ostream& o, const Json& json)
{
	return match(json.value,
		[&](const std::string& str) -> std::ostream& {
			return o << '"' << escape(str) << '"';
		},
		[&](const auto& value) -> std::ostream& {
			return o << value;
		});
}

std::ostream& operator<<(std::ostream& o, const Error& error)
{
	switch (error)
	{
		case Error::EmptyString: return o << "EmptyString";
		case Error::CharMismatch: return o << "CharMismatch";
		case Error::HexCharExpected: return o << "HexCharExpected";
		case Error::NullExpected: return o << "NullExpected";
		case Error::TrueExpected: return o << "TrueExpected";
		case Error::FalseExpected: return o << "FalseExpected";
		case Error::ExponentRequired: return o << "ExponentRequired";
		case Error::UnrecognisedEscapeSequence: return o << "UnrecognisedEscapeSequence";
		case Error::InvalidValue: return o << "InvalidValue";
		case Error::OutOfBounds: return o << "OutOfBounds";
		case Error::Garbage: return o << "Garbage";
		case Error::Failure: return o << "Failure";
	}
	throw std::runtime_error("Invalid Error value");
}

