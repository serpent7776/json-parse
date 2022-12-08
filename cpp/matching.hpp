#include <variant>
#include <utility>

template<typename ...Fs>
struct overloaded : Fs...
{
	using Fs::operator()...;
};
template<typename ...Fs> overloaded(Fs...) -> overloaded<Fs...>;

template<typename ...Ts, typename ...Fs>
decltype(auto) match(std::variant<Ts...>&& v, Fs&&... f) //-> std::invoke_result_t<overloaded<Fs...>, Ts...>
{
	auto overload = overloaded{std::forward<Fs>(f)...};
	auto unpair = [&]<typename A, typename B>(std::pair<A, B> p){return overload(std::move(p.first), std::move(p.second));};
	return std::visit(overloaded{unpair, std::move(overload)}, std::move(v));
}
template<typename ...Ts, typename ...Fs>
decltype(auto) match(const std::variant<Ts...>& v, Fs&&... f) //-> std::invoke_result_t<overloaded<Fs...>, Ts...>
{
	auto overload = overloaded{std::forward<Fs>(f)...};
	auto unpair = [&]<typename A, typename B>(const std::pair<A, B>& p){return overload(p.first, p.second);};
	return std::visit(overloaded{unpair, std::move(overload)}, v);
}
