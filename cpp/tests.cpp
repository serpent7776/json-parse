#include "catch.hpp"
#include "json.hpp"
#include "matching.hpp"

template <typename ...Jsons>
Array make_array(Jsons&&... jsons)
{
	Array arr;
	arr.reserve(sizeof...(jsons));
	(arr.push_back(std::forward<Jsons>(jsons)), ...);
	return arr;
}

template <typename ...Jsons>
Object make_object(std::pair<std::string, Jsons>&&... kvs)
{
	Object obj;
	(obj.insert(std::forward<std::pair<std::string, Jsons>>(kvs)), ...);
	return obj;
}

#define FAILS(f, arg, expected_error) \
	match(f(arg), \
		[](Json&& actual_json){ \
			CAPTURE(actual_json, expected_error); \
			CHECK(false); \
		}, \
		[](Error actual_error, std::string_view){ \
			CHECK(expected_error == actual_error); \
		})

#define OK(f, arg, expected_json) \
	match(f(arg), \
		[](Json&& actual_json){ \
			CHECK(actual_json == expected_json); \
		}, \
		[](Error actual_error, std::string_view){ \
			CAPTURE(actual_error, expected_json); \
			CHECK(false); \
		})

TEST_CASE("basic cases")
{
	FAILS(parse, "", Error::EmptyString);
	FAILS(parse, "x", Error::InvalidValue);
}

TEST_CASE("literals")
{
	OK(parse, "null", Json{Null{}});
	OK(parse, "true", Json{true});
	OK(parse, "false", Json{false});
	FAILS(parse, "truefalse", Error::TrueExpected);
}

TEST_CASE("numbers")
{
	OK(parse, "42", (Json{Number{.integer=42, .fraction=0, .precision=0, .exponent=0}}));
	OK(parse, "0", (Json{Number{.integer=0, .fraction=0, .precision=0, .exponent=0}}));
	OK(parse, "-1", (Json{Number{.integer=-1, .fraction=0, .precision=0, .exponent=0}}));
	OK(parse, "1.23", (Json{Number{.integer=1, .fraction=23, .precision=2, .exponent=0}}));
	OK(parse, "1.230", (Json{Number{.integer=1, .fraction=230, .precision=3, .exponent=0}}));
	OK(parse, "1.", (Json{Number{.integer=1, .fraction=0, .precision=0, .exponent=0}}));
	OK(parse, "0.", (Json{Number{.integer=0, .fraction=0, .precision=0, .exponent=0}}));
	OK(parse, "-0.", (Json{Number{.integer=0, .fraction=0, .precision=0, .exponent=0}}));
	OK(parse, "6.999e3", (Json{Number{.integer=6, .fraction=999, .precision=3, .exponent=3}}));
	OK(parse, "-1.2e9", (Json{Number{.integer=-1, .fraction=2, .precision=1, .exponent=9}}));
	FAILS(parse, "6.999e", Error::ExponentRequired);
	OK(parse, "1.e1", (Json{Number{.integer=1, .fraction=0, .precision=0, .exponent=1}}));
	FAILS(parse, "1.x", Error::Garbage);
	FAILS(parse, "1.y", Error::Garbage);
	FAILS(parse, ".12", Error::InvalidValue);
	FAILS(parse, "-.12", Error::EmptyString);
}

TEST_CASE("strings")
{
	OK(parse, R"("")", (Json{std::string()}));
	FAILS(parse, R"(")", (Error::OutOfBounds));
	OK(parse, R"("foobar")", (Json{std::string("foobar")}));
	OK(parse, R"("a\nb")", (Json{std::string("a\nb")}));
	OK(parse, R"("foo\\bar")", (Json{std::string(R"(foo\bar)")}));
	OK(parse, R"("foo bar")", (Json{std::string(R"(foo bar)")}));
	OK(parse, R"("foo/bar")", (Json{std::string(R"(foo/bar)")}));
	FAILS(parse, R"("foobar)", (Error::OutOfBounds));
	FAILS(parse, R"("foo"bar)", (Error::Garbage));
	FAILS(parse, R"("foo\"bar)", (Error::OutOfBounds));
	OK(parse, R"("a b c")", (Json{std::string(R"(a b c)")}));
	OK(parse, R"(" a b c ")", (Json{std::string(R"( a b c )")}));
	OK(parse, R"("foo\"bar")", (Json{std::string(R"(foo"bar)")}));
	OK(parse, R"("\u1234")", (Json{std::string(R"(?)")}));
	OK(parse, R"("\u1234\uabcd")", (Json{std::string(R"(??)")}));
	OK(parse, R"("\u1234\uabcd\u00Ff")", (Json{std::string(R"(???)")}));
	OK(parse, R"("foo\u12cdbar")", (Json{std::string(R"(foo?bar)")}));
	FAILS(parse, R"("\u12cx")", (Error::HexCharExpected));
	FAILS(parse, R"("\)", (Error::OutOfBounds));
}

TEST_CASE("arrays")
{
	OK(parse, R"([])", (Json{Array()}));
	OK(parse, R"([null])", (Json{make_array(null())}));
	OK(parse, R"([[null]])", (Json{make_array(Json{make_array(null())})}));
	OK(parse, R"([true])", (Json{make_array(True())}));
	OK(parse, R"([false])", (Json{make_array(False())}));
	OK(parse, R"([true,false])", (Json{make_array(True(), False())}));
	OK(parse, R"([1.2])", (Json{make_array(Json{Number{.integer=1, .fraction=2, .precision=1, .exponent=0}})}));
	OK(parse, R"(["abc"])", (Json{make_array(Json{"abc"})}));
	OK(parse, R"([[[]]])", (Json{make_array(Json{make_array(Json{make_array()})})}));
	OK(parse, R"([[["a"]]])", (Json{make_array(Json{make_array(Json{make_array(Json{"a"})})})}));
	FAILS(parse, R"([)", (Error::OutOfBounds));
	FAILS(parse, R"(])", (Error::InvalidValue));
	FAILS(parse, R"([[[)", (Error::OutOfBounds));
	FAILS(parse, R"(]]])", (Error::InvalidValue));
	OK(parse, R"([1,2,3])", (Json{make_array(
		Json{Number{.integer=1, .fraction=0, .precision=0, .exponent=0}},
		Json{Number{.integer=2, .fraction=0, .precision=0, .exponent=0}},
		Json{Number{.integer=3, .fraction=0, .precision=0, .exponent=0}}
	)}));
	FAILS(parse, R"(["])", (Error::OutOfBounds));
	OK(parse, R"([true,false,null,0])", (Json{make_array(
		True(),
		False(),
		null(),
		Json{Number{.integer=0, .fraction=0, .precision=0, .exponent=0}}
	)}));
	OK(parse, R"([[],[]])", (Json{make_array( Json{make_array()}, Json{make_array()} )}));
	FAILS(parse, R"([1,)", (Error::InvalidValue));
	FAILS(parse, R"([1,])", (Error::InvalidValue));
	FAILS(parse, R"([1,2,])", (Error::InvalidValue));
	FAILS(parse, R"([1,2,)", (Error::InvalidValue));
}

TEST_CASE("objects")
{
	OK(parse, R"({})", (Json{Object{}}));
	OK(parse, R"({"1":1})", (Json{make_object(
		std::pair{std::string{"1"}, Json{Number{.integer=1, .fraction=0, .precision=0, .exponent=0}}}
	)}));
	OK(parse, R"({"foo":"bar"})", (Json{make_object(
		std::pair{std::string{"foo"}, Json{"bar"}}
	)}));
	OK(parse, R"({"":""})", (Json(make_object(
		std::pair{std::string{""}, Json{""}}
	))));
	OK(parse, R"({"12":[]})", (Json(make_object(
		std::pair{std::string{"12"}, Json{make_array()}}
	))));
	OK(parse, R"({"a":1,"b":2,"c":3})", (Json(make_object(
		std::pair{std::string{"a"}, Json{Number{.integer=1, .fraction=0, .precision=0, .exponent=0}}},
		std::pair{std::string{"b"}, Json{Number{.integer=2, .fraction=0, .precision=0, .exponent=0}}},
		std::pair{std::string{"c"}, Json{Number{.integer=3, .fraction=0, .precision=0, .exponent=0}}}
	))));
	OK(parse, R"({"x":9.8e7})", (Json{make_object(
		std::pair{std::string{"x"}, Json{Number{.integer=9, .fraction=8, .precision=1, .exponent=7}}}
	)}));
	FAILS(parse, R"({"1":1)", (Error::OutOfBounds));
	FAILS(parse, R"({"foo")", (Error::OutOfBounds));
	FAILS(parse, R"({"foo":)", (Error::InvalidValue));
}

TEST_CASE("values with spaces")
{
	FAILS(parse, R"(   )", (Error::EmptyString));
	FAILS(parse, R"( [  )", (Error::OutOfBounds));
	OK(parse, R"(   null   )", (Json{Null{}}));
	OK(parse, R"(   true   )", (Json{True()}));
	OK(parse, R"(   false   )", (Json{False()}));
	OK(parse, R"(" \u1234 \uabcd \u00Ff ")", (Json{" ? ? ? "}));
	OK(parse, R"([ true, false, null ])", (Json{make_array(
		True(), False(), null()
	)}));
	OK(parse, R"( [ true , false , null ] )", (Json{make_array(
		True(), False(), null()
	)}));
	OK(parse, R"( { "a" : true , "b" : false , "c" : null } )", (Json{make_object(
		std::pair{std::string{"a"}, True()},
		std::pair{std::string{"b"}, False()},
		std::pair{std::string{"c"}, null()}
	)}));
	OK(parse, R"( {  } )", (Json{Object{}}));
	OK(parse, R"( [  ] )", (Json{make_array()}));
}
