// the string _eq_ method in the Monty core library is implemented in C,
// compiled to LLVM, using Clang. The following source code is used:
 
_Bool scmp(char* a, char* b){
    while(*a != 0){
        if(*a != *b){
            return 0;
        }
        a++;
        b++;
    }
    return *a == *b;
}