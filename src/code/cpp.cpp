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
    cout<<"Hello World"<<endl;
}
{
    cout<<"Hello World"<<endl;
}
{
    mat m1(10, 10);
    m1.geneMat(200);
    mat m2 = m1 * m1 * m1;
    m2.print();
}
}
