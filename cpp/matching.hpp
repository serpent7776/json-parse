#include <variant>
#include <utility>

template<typename ...Fs>
struct overloaded : Fs...
{
	using Fs::operator()...;
};
template<typename ...Fs> overloaded(Fs...) -> overloaded<Fs...>;

template<typename ...Ts, typename ...Fs>
decltype(auto) match(std::variant<Ts...> v, Fs&&... f)
{
	auto overload = overloaded{f...};
	auto unpair = [&]<typename A, typename B>(std::pair<A, B> p){return overload(std::move(p.first), std::move(p.second));};
	return std::visit(overloaded{unpair, overload}, std::move(v));
}
