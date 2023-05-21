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
    printf("previous");
}
{
    mat m2(4, 4);
    m2.geneMat(10);
    m2.print();
    mat m3 = m2 * m2;
    m3.print();
    printf("m3\n");
    printf("kkk\n");
    printf("2");
    printf("1");
    printf("previous");
    printf("a");
    printf("abcd");
    printf("efgh");
    printf("ijkl");
    printf("mnop");
    printf("qrst");
    printf("uvwx");
    printf("yz");
}
{
    vector<int> v {1, 2, 3};
    for(auto x : v) {
        printf("%d\n", x);
    }
}
}
