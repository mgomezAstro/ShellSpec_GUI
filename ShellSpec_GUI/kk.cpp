#include <stdlib.h>
#include <iostream>
#include <stdio.h>

using namespace std;

extern "C" int shellspec_(int *kkkk);

int main()
{
	int kkkk;
	int r;
	cout<<"Running"<<endl;
	r = shellspec_(&kkkk);
	cout<<"Ending"<<endl;
}

