#include<iostream>
#include "AronLib.h"
// #include "catch.hpp"

#include <string>
#include <fstream>
#include <sstream>
#include <iostream>
#include <vector>

using namespace std;
// using namespace AronPrint;  // pp()
// using namespace Algorithm;
// using namespace Utility;

int main() {
// std::cout<<"cool"<<std::endl;
{
    mat m2(4, 4);
    m2.geneMat(10);
    m2.print();
    mat m3 = m2 * m2;
    m3.print();
    printf("m3\n");
}
}
