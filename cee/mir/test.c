#include <stdio.h>
#include <gc.h>
#include "headers/mirObject.h"

void testFun() {
    puts("HELLO WORLD");
}

int main(void) {

mirObject _Temp_44197;
{
mirObject _User_foo = mirIntTo(12345);
mirObject _User_bar = mirIntTo(54321);
mirObject _Temp_44198 = mirIntAdd(_User_foo, _User_bar);
mirObject _Temp_44199 = mirIntAdd(_User_foo, _Temp_44198);
_Temp_44197 = _Temp_44199;
}
mirObject _Temp_44200 = mirIntAdd(mirIntTo(123), _Temp_44197);

    printf("%d\n", _Temp_44200.value.asInteger);
}
