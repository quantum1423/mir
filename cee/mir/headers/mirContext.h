#pragma once
#include <setjmp.h>

typedef struct {
    jmp_buf exnHandler;
} mirContext;
