#include <libcross_prod.cpp>

int main() {

	const int size = 2;

	vector<Tuple3_float_float_float> x{  {1,2,3},{4,5,6} };
	vector<Tuple3_float_float_float> y{  {1,2,3},{4,5,6} };

	Tuple3_float_float_float *z = nullptr;

	execute(x.data(), y.data(), z, size);

	assert(z!=nullptr);

	std::cout << z[0]._0 << ", "<< z[0]._1 << ", "<< z[0]._2 << ", ";
	std::cout << z[1]._0 << ", " << z[1]._1 << ", " << z[1]._2 << std::endl;

	return 0;
}
