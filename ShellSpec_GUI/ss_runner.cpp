#include <stdlib.h>
#include <stdio.h>
#include <iostream>

using namespace std;

extern "C" void shellspec_();

int main () {
    cout << "Calling fortran Shellspec: " << endl;
    shellspec_();
    cout << "Code ended." << endl;
    return 0;
}
