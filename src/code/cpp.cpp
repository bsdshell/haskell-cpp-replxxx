
#include<iostream>
#include <string>
#include <fstream>
#include <sstream>
#include <iostream>
#include <vector>
#include "AronLib.h"
using namespace std;
using namespace AronPrint;  // pp()
using namespace Algorithm;
using namespace Utility;
using namespace MatrixVector;

              

int main() {

           
mat m1(4,4);
m1.arr[3][3] = 10000;
fw("m1");
m1.print();
mat m2 = m1.transpose();
fw("transpose");
m2.print();
fw("m3");
mat m3 = m2 * m2;
m3.print();
}