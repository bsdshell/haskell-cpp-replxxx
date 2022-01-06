#include<iostream>
#include "AronLib.h"
#include "catch.hpp"

#include <string>
#include <fstream>
#include <sstream>
#include <iostream>

using namespace std;
using namespace AronPrint;  // pp()
using namespace Algorithm;
using namespace Utility;

int main() {
    std::cout<<"cool"<<std::endl;

// BEG_rep
std::function<int(bool)> even = [] (auto x) { return x % 2 = 0? true : false;};
vector<int> v = {1, 2, 3, 4};
vector<int> vec = takeWhile(even, v);
pp(vec);

// END_rep
    
}

