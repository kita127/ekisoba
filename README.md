# Ekisoba
A tool which translates C source to AST.
support json.

You can also use it as a library.

## Usage

sample.c
```c
/* prototype declare */
int sum_func(int a, int b);

/* extern declare */
extern int external_var;

/* variable definition */
/* non initial value */
char g_char_var;
int g_int_var;

/* initial value */
int g_int_var2 = 100;

/* function pointer */
int (*func_p)(int a, int b);

/* function definition */
int sum_func(int a, int b) {
  int res;
  res = a + b;
  return res;
}
```

``` text
$cat sample.c | ekisoba --json
{
    "file":"sample.c",
    "program":{
        "statement":[
            {
                "functionDeclaration":{
                    "name":"sum_func",
                    "args":[
                    {
                        "name":"a",
                        "type":[ "int" ]
                    },
                    {
                        "name":"b",
                        "type":[ "int" ]
                    }
                    ],
                    "returnType":[ "int" ],
                    "block":null
                }
            },
            {
                "variableDeclaration":{
                    "name":"external_var",
                    "type":[ "int", "extern" ]
                },
            },
            {
                "variableDefinition":{
                    "name":"g_char_var",
                    "type":[ "char" ],
                    "value":null
                }
            },
            {
                "variableDefinition":{
                    "name":"g_int_var2",
                    "type":[ "int" ]
                    "value":"100"
                }
            },
            {
                "variableDefinition":{
                    "name":"func_p",
                    "type":[
                        "pointer",
                        "function":{
                            "name":null,
                            "args":[
                                {
                                    "name":"a",
                                    "type":[ "int" ]
                                },
                                {
                                    "name":"b",
                                    "type":[ "int" ]
                                }
                            ],
                            "returnType":[ "int" ],
                            "block":null
                        }
                    ],
                    "value":null
                }
            },
            {
                "functionDefinition":{
                    "name":"sum_func",
                    "args":[
                        {
                            "name":"a",
                            "type":[ "int" ]
                        },
                        {
                            "name":"b",
                            "type":[ "int" ]
                        },
                    ],
                    "returnType":{
                        "type":[ "int" ]
                    },
                    "block":{
                        "statement":[
                            {
                                "variableDefinition":{
                                    "name":"res",
                                    "type":[ "int" ],
                                    "value":null
                                }
                            },
                            {
                                "expression":{
                                    "type":"infix",
                                    "operator":"=",
                                    "left":{
                                        "name":"res"
                                    },
                                    "right":{
                                        "expression":{
                                            "type":"infix",
                                            "operator":"+",
                                            "left":{
                                                "identifire":{ "name":"a" }
                                            },
                                            "right":{
                                                "identifire":{ "name":"b" }
                                            }
                                        }
                                    }
                                }
                            },
                            {
                                "return":{
                                    "identifire":{ "name":"res" }
                                }
                            }
                        ]
                    }
                }
            }
        ]
    }
}
```
