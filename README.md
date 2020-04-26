# transliterator

This is a tool for refactoring code in arbitrary programming langagues I wrote in 2015. In 2019, a very similar tool called [Comby](https://comby.dev/) was released. I recommend using Comby instead.

## Example

Given the templates

    for (CTOR<TYPE>::iterator VAR = LIST.begin(); VAR != LIST.end(); ++VAR) {...}
    
and

    LIST.foreach { VAR: TYPE => ... }

`transliterator` can convert

    ...
    for (vector<int>::iterator i = myVector.begin(); i != myVector.end(); ++i) {cout << *i << endl;}
    ...

to

    ...
    myVector.foreach { i: int => cout << *i << endl; }
    ...
