# mmap

Mapping a file in read-only mode looks like this:

```
0 ( addr )
  4096 ( length of map )
       1 ( PROT_READ )
         2 ( MAP_PRIVATE )
           4 ( fd )
             0 ( offset )
               9 ( mmap syscall number ) 
                 syscall6
```

In general, the length (2nd parm) will need adjusting,
and `fd` should match the open file.
