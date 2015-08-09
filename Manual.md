# Manual

This document provides instructions on using **ccs2cs** for writing C# wrappers to native libraries.


## Overview


ccs2cs takes a file containing C# code interspersing with C pre-processor macros (and other compile time constants)  and generates a (reasonably pretty)  C# file.

### macros
ccs2cs allows us to use any c *constant vaiable*  via ` #VariableName `. This gets replaced by  the value in a C Program using the same pre-processor directives as the current ccs file. In addition there are following special constructs:

+ #offset( structName, fieldName): this is equivalent to the `offsetof(struct structName, fieldName)` function of C.
+ #size( structName ) : this is equivalent to `sizeof(struct structName)` in C.

As will be explained later, we can introduce new definitions as part of pre-processor directives and use them for evaluating more complex (but constant) values of C.

### Sections

A CCS file contains following sections:
+ prologue
+ pre-processor directives
+ ccs types
+ epilogue

Each of these are explained in the following sections.

### PROLOGUE

The **prologue** contains arbitrary C# code. The text in this section is copied *verbatim* into the generated file. This section is typically used for comments, copyright info,
*using* statements, *namespace* declarations and such.


### CCS Types.

A CCS type section can contain any number of any of the following:
+ A **Enum** definition   : to collect related constants.
+ A **Class** definition  : serves the same purpose as an Enum but at times a static class is needed. 
+ A **Struct** definition : This is an experimental feature meant for partial mapping between a c structure and C# class.


The CCSTypes can be defined inside a CCS-Section which begins with a *BEGIN* line which has the following syntax:

```
#BEGIN  "libraryname" (number) 
```
where
 - #BEGIN should be at the beginning of a new line.
 - libraryname should be within double quotes (the value is not used by ccs2cs at the time of writing this manual).
 - Any number between 0 and 9 can be used to inform ccs2cs the indentation to be provided for the generated code. The number must be within a pair of brackets.

The indentation part is optional. The following
```
#BEGIN "somelibrary"
```

is equivalent to

```
#BEGIN "somelibrary" (4)
```

The CCS section ends with an *END* line. Which is just a "#END" at the beginning of a new line.

### preprocessor directives.
The begin line can be followed by any number of any of the following c-preprocessor directives:

+ `#if`
+ `#ifendif`
+ `#else`
+ `#define`
+ `#pragma`
+ `#undef`

Unlike standard preprocessor, multi-line directives are not allowed.
Each directive  should begin at the beginning of the line. The syntax must be a valid c preprocessor syntax.
Variables defined using `#define` can be used later as any other macro in the rest of ccscode. This allows us to use more complex macros, like `MAKEWORD` of VC++.

### CCS Enum.
As mentioned earlier, a CCS Enum is used to collect related integer constants. For example, the following declare a couple of Enums to 

```
    enum AIFamily {
        AFUnspec = #AF_UNSPEC,
        AFInet   = #AF_INET,
        AFInet6  = #AF_INET6
    }

    enum SockType {
      SockStream = #SOCK_STREAM,
      SockDatagram  = #SOCK_DGRAM ,
      Any      = 0 
    }
```
ccsgen translates this code to an C# enum with the member value assigned to value defined as per the platform. For instance, on OSX, it evaluates to
```
    internal enum AIFamily
    {   AFUnspec = 0 ,
        AFInet = 2 ,
        AFInet6 = 30
    }
    internal enum SockType
    {   SockStream = 1 ,
        SockDatagram = 2 ,
        Any = 0 
    }
```
A more formal syntax for ccs enum is 
```
   CCS-ENUM     : 'enum' <IDENTIFIER-NAME> '{'   <ENUM-MEMBER (',')?>* '}'
   ENUM-MEMBER : MEMBER-NAME  ( '=' ENUM-MEMBER-VALUE)?
   ENUM-MEMBER-VALUE = macro-access (synatx defined above)
                      | arbitrary-text (not containing braces,comma or semicolon)  till the next comma 
                      | arbitrary-text between curly-braces interspersed with macros.
   IDENTIFIER-NAME = any valid C# identifier

```

### CCS Class
A CCS class serves similar purpose as an Enum -- to collect related constants. A class, however, is not limited to just numerical constants. And they get translated to *const* fields of a *static* class. The syntax for class is
```
  ccsClass : 'class' <IDENTIFIER-NAME> '{' <CLS-MEMBER>+ '}'
  CLS-MEMBERS : CS-Type-Name  IDENTIFIER-NAME '=' <VALUE> ';' 
  Type-Name : is either a simple or fully qualified C# type
  VALUE  : similar in syntax of ENUM-MEMBER-VALUE
```

### Epilogue
Any text following `#END` can contain csharp code interspersed with any of the macros. This code is translated via expanding the macros.

## Running
ccs2cs works by
+ parses the ccs file
+ generating an file containing macro definitions and running a c-preprocessor on it and extracting the value of macros.
+ generating a c file such that, when run, it provides ccs2cs all the values of relevant ccs constructs.
+ ccs2cs then invokes a c compiler (cl.exe on windows and clang on other operating systems) on the generated c code to generate an executable.
+ This executable is then run, and its output is parsed to finally create the C# file with values filled.

## Building ccs2cs
ccs2cs has been tested with ghc 7.10.2. It can be built from source like any cabalized haskell program.

## Examples

**TODO**

A couple of silly-ish examples are  available in the examples folder. Need more substantial examples.