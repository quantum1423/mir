#pragma once

#define MIRT_INT 0x00
#define MIRT_FLO 0x01
#define MIRT_BOX 0x02
#define MIRT_SYM 0x03

// tuple-like types
#define MIRT_TUPLE  0x10
#define MIRT_RECORD 0x11

// blob-like types
#define MIRT_U8VEC   0x20
#define MIRT_S16VEC  0x21
#define MIRT_U16VEC  0x22
#define MIRT_S32VEC  0x23
#define MIRT_U32VEC  0x24
#define MIRT_S64VEC  0x25
#define MIRT_U64VEC  0x26
#define MIRT_STRING  0x2F

#define MIRT_FUN 0xFF

#include "mirContext.h"

typedef struct _mirObject {
    unsigned char tag;
    union {
        int asInteger;
        double asFloat;
        
        struct _mirObject *asBox;
        
        struct {
            int length;
            struct _mirObject *values;
        } *asTuple;
        
        struct {
            void *base;
            void *start;
            void *end;
        } *asBlob;
        
        struct {
            void *code;
            struct _mirObject *env;
        } *asFunction;
    } value;
} mirObject;


#include "mirObject-integer-operations.h"
#include "mirObject-float-operations.h"
#include "mirObject-function-operations.h"

