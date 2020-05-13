# Ekisoba
Ekisoba is easy C langage analyzer

## Usage

sample.c
```c
extern int external_var;

char g_char_var;
int g_int_var2 = 100;

int sum_func(int a, int b) {
  int res;
  res = a + b;
  return res;
}

```

``` text
$cat sample.c | ekisoba
{
    "file":"sample.c",
    "program":{
        "statement":[
            {
                "declaration":{
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
                                    "type":[ "int" ]
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
