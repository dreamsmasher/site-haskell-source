---
title: 'beauty and the bytestring'
published: 2020-11-06
last: 2020-11-06
keywords: programming haskell c
---

>
>
<span style="color: yellow">» This is my submission for the 2020 [Advent of Haskell](https://adventofhaskell.com/), an event that aims to showcase some of the cool, unknown features of Haskell that newcomers might not know.</span>

Ok, let's talk about Linked Lists. You've likely come across them before, either in Leetcode problems or in a pretentious whiteboard interview. They're a simple data structure, and a great way to learn how to use `struct`s or `Option<Rc<RefCell<Box<ListNode>>>>` when you're starting out. As a refresher, the canonical definition of a linked list is something like 

``` c
typedef struct LinkedList {
  int val;
  struct LinkedList *next;
} LinkedList;

```
where each node contains a value, and a pointer to the next element in the list.
They're intuitive, and allow you grow/shrink collections of objects in O(1) time. Magic, right? There are also plenty of arguments against their usage.

When you construct a new node, you still need memory for it. You're less likely to run into issues like heap fragmentation from being unable to find a contiguous chunk of memory large enough to give what you're asking for (at allocation-time, anyways), but you still need to create a `struct` for each node you want to use. Ignoring structure padding and assuming we're on a 64-bit machine using GCC, a single `LinkedList` node will require **96** bits. **32** bits for `val`, and **64** bits for your `next` pointer. That's a ton of bloat, compared to arrays. A linked list will incur **66%** more overhead compared to an equally sized array of `int`s. 

If you're *le epic C hacker* already, you probably know better than to call `malloc()` each time you want to construct a heap object. It's faster to create a "pool" of memory at first, carving off chunks whenever memory is needed. Depending on your system, `malloc` implementation, and how much memory you actually want, `malloc()` might go one of a few ways.[^1] [^2]

- Your program's address space already has space on the heap large enough to fulfill your request. It's yours, use it carefully.
- `malloc()` calls `mmap(), sbrk() or brk()` internally, and the kernel finds you some physical memory to use, mapping a few pages to virtual addresses and giving you these addresses. This expands your address space, and requires a context switch. 


In newer versions of Linux, large calls to `malloc()` will call `mmap()`. The allocation itself is done lazily. You ask for 4MB of memory, and the OS says "yeah, yeah. I know for a fact you probably won't use most of this, but here." What you get is a pointer to the **idea** of memory. The OS doesn't actually try to map that to anything until you actually try to write to it. The process is kinda like [this](https://www.youtube.com/watch?v=zTEjnQfJgGk).

We're getting sidetracked. The point is, this is a lot of complexity, and repeated calls to `malloc()` will slow your program down significantly because of all the context switching and resulting overhead. Not to mention the fact that all this indirection results in a ton of cache misses for your CPU, further hindering performance.

What does this have to do with Haskell? The language makes heavy use of lists, in their traditional sense. Python **lies** to you with its 'lists'. They're just arrays.(numbers are also arrays of `long` integers, but that's a story for another post). Haskell represents collections of values like any other language, square brackets and all. The fact that they're linked gives rise to some interesting differences, though.

1. No random access. 


 Since mutability is unrepresentable (outside of `IORef`s), a lot of dynamic programming/array-based algorithms require a little bit of clever rethinking in order to be implemented using pure and referentially-transparent methods. For example, let's try finding the first `n` fibonacci numbers. In an imperative language, we could keep track of two values and store our numbers in an array as we iterate:

``` Python
def nth_fib(n):
  prev_of_prev = 0 # don't judge these variable names
  prev = 1
  fibs = []
  for _ in range(n):
    fibs.append(prev_of_prev)
    next_num = prev + prev_of_prev
    prev_of_prev = prev
    prev = next_num
  return fibs
```

We're mutating `fibs, prev_of_prev`, and `prev` here. That's a big no-no in Haskell, because modifying values in place prevents you from reusing them later. Of course, there are use-cases for being able to access collections of values by their indices, so packages like `vector` still provide that functionality. 

2. Lazy evaluation.

O(1) runtime for `push` and `pop` operations are powerful. Most higher-level languages have some sort of operation for appending and popping off the end of an array, but they require some behind-the-scenes overhead still. Keep in mind that dynamically resizable arrays provide an *amortized* O(1) runtime for these operations, but they're still subject to the fact that their size and capacity have to be known at all times.

Haskell's linked lists are infinite, on the other hand. You've probably come across this implementation of the fibonacci numbers in some tutorials:
``` Haskell
nth_fib :: Integer -> [Integer] 
nth_fibs n = take n fibs
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
```

If you're unfamiliar with Haskell, here's what we're doing: 

`0 : 1 : _` describes a linked list with `0` at the head, pointing to `1`, which in turn points to the next element, and so on. `[1,2,3]` is the same as `1 : 2 : 3 : []`, it's just a matter of syntax.

`zipWith` has the type `(a -> b -> c) -> [a] -> [b] -> [c]`, which is saying "give me a function that takes an `a` and a `b`, returning a `c`, and two lists containing elements of those types, and I'll give you back the result of combining these two lists using the given function". `tail` is a function that takes a list, and returns the rest of the list excluding the first element. 

When we're zipping `fibs` with itself, we're creating two copies of the list, shifted out of phase by one element.

``` python
fibs:      0 : 1 
tail fibs: 1 : ?
```

We then apply our function to each element sharing the same positions in our list. Since our function is `+`, we combine values by adding them together. Pretty standard stuff:

```python
fibs:      0 : 1
tail fibs: 1 : ?
result:    1 : ?
```
The resulting `1` is part of `fibs`, because we declared it to be that way. The Haskell runtime doesn't try to evaluate anything until it's actually needed. In this case, we need the next element of `fibs` in order to evaluate it another step. To Haskell, `fibs` is now evaluated to:

```python
fibs:      0 : 1 : 1    
tail fibs: 1 : 1 : ?
   
```

If you've read the *Principia Mathematica*, you know that `1 + 1 = 2`. When that second `1` in `tail fibs` gets `zipWith (+)`'ed with the first one, the next element of `fibs` becomes apparent. And so on.

Not only have we implemented the Fibonacci sequence, it's also implicitly a generator now. No `yield` or errant `*`'s are needed in your functions. It doesn't matter that we've defined an infinite sequence, since we're only going to `take` a finite amount from it. We can trust that the sequence will only be evaluated as far as we need. Extremely powerful stuff.


<h2 style="color: yellow">import qualified Data.ByteString as B</h2>
The problem arises when we try to use linked lists for small values. Sure, we can make lists of anything that's representable in Haskell. Lists of Vectors of Trees of Ints? Easy. When you want to parse large collections of data, like reading from a file, it's common practice to abstract away from file handles, input buffers, etc. and just look at your data as a stream. In the same way that "strings" in C are just pointers to arrays of `char`acters, `String`s in Haskell are just linked lists of `Char`acters.

```Haskell
syntacticSugar :: String
syntacticSugar = "henlo"

actualString :: [Char]
actualString = 'h' : 'e' : 'n' : 'l' : 'o' : []
```

This affords a lot of flexibility when it comes to string operations, since you can apply any list function to strings as well.

```Haskell
-- dupItems :: [a] -> [a]
dupChars :: String -> String
dupChars str = str >>= \c -> [c, c]

> dupChars "henlo"
"hheennlloo"
```

The problem arises when we try to use our `[Char]` streams for high-performance situations. Linked lists are unsuited for small values because of the additional overhead and cache misses they introduce. 

Let's say we wanted to parse a large file for a specific byte signature. For simplicity, let's seek to the first occurrence of `0x20` (' '). Haskell's native `Char` type is normally 32-bit, and readFile treats your files as `[Char]`. 

```haskell
findSpc :: String -> String   
findSpc s = dropWhile (/= ' ') s

```

Now we do some idiomatic shell wizardry to generate a test case. The resulting `test.txt` file is around 573MB: 
``` bash
$ python -c "a = 'aaa' * 100000000; print(a + ' ' + a)" > test.txt
```

and our test scheme (again, we're keeping it very simple):
```haskell
import Data.Time

main :: IO ()
main = do
    testStr <- readFile "test.txt"
    start1 <- getCurrentTime
    print $ take 10 (findSpc testStr)
    stop1 <- getCurrentTime
    putStrLn $ "String runtime : " <> show (diffUTCTime stop1 start1)
```

We actually print the values in case the compiler decides to be smart and just optimize out our functions. 

```bash
$ ghc findSpc.hs -O0 && ./findSpc
" aaaaaaaaa"
String runtime : 3.328033023s
```

I mean, that's not bad for a half-gig file. You can only imagine this number will get worse as our inputs scale further. What if you had to parse terabytes of data? Or do something less trivial than finding whitespace?

According to the [Haskell wiki](https://wiki.haskell.org/Dealing_with_binary_data#Handling_Binary_Data_with_Haskell), `String` incurs a memory footprint of around 24 times compared to an equivalent `uint8_t *` array in C. As you can probably guess, most of the overhead from Strings comes from the way they're stored, and the way that the Haskell runtime deals with boxed values. 

This is where our package of the day comes in: `bytestring`. `ByteString`s are meant to be faster and more space-efficient than our regular linked lists, and they definitely live up to the expectation. The package is intended to be imported `qualified` for a reason - most of its exported functions do exactly the same thing as their List equivalents. It only makes sense to give them the same names too. It's a great example of a package that abstracts away complexity, while maintaining a familiar, simple, and most importantly, pure interface. Let's try it out.

```haskell
import qualified Data.ByteString.Char8 as C
import Data.Time

findByt :: C.ByteString -> C.ByteString
findByt bs = C.dropWhile (/= ' ') bs

main :: IO ()
main = do
    testBtStr <- C.readFile "test.txt"
    start2 <- getCurrentTime
    print $ C.take 10 (findByt testBtStr)
    stop2 <- getCurrentTime
    putStrLn $ "ByteString runtime : " <> show (diffUTCTime stop2 start2)

```

```bash
$ ghc findSpc.hs -O0 && ./findSpc
" aaaaaaaaa"
ByteString runtime : 2.337315973s
```
Our ByteString version is already about 142% faster. What happens when we turn on optimizations?

```bash
$ ghc findSpc.hs -O2 && ./findSpc
" aaaaaaaaa"
String runtime : 2.314101402s
" aaaaaaaaa"
ByteString runtime : 0.149613233s

```

A 1447% speedup over regular `String`s. That's crazy. Where does this speedup actually come from?

Inside the GHC Runtime, most of your values aren't actually primitive. When you have a function that takes an `Int` for instance, you're not working with the `Int` directly. You're actually working with a pointer to an object stored on the heap. These objects contain a header, which in turn contains a pointer to an info table, and optional profiling data. The info table in turn carries metadata about the object's type - whether it's a function, a piece of data that's already been evaluated, etc. Next are a bitmap and a layout field containing info for the garbage collection, then *entry code* that will lead to the object becoming *evaluated* when the code is run.[^3] 

This is the heart of Haskell's lazy evaluation - for values that aren't used, their entry code doesn't get executed. Once you actually compute a value, the entry code for the object gets overwritten with code that just returns the result, ensuring that computation only needs to happen once.[^4] This is great for algorithms that require a lot of sharing, but inefficient for things like Strings, where the individual characters don't tend to carry a lot of value on their own.

You can usually avoid a lot of overhead when dealing with primitive values by choosing to use unboxed types instead. These are closer to the native data types in lower-level languages, although you're more restricted in what you're allowed to do with these values. Although unboxed values give you a big performance boost over regular, builtin data types,

What does `ByteString` do differently? 


[^1]: [what happens in the kernel during malloc?](https://stackoverflow.com/questions/5716100/what-happens-in-the-kernel-during-malloc)
[^2]: [mmap.c](https://github.com/torvalds/linux/blob/master/mm/mmap.c)
[^3]: [GHC commentary: The Layout of Heap Objects](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects)
[^4]: [The Spineless Tagless G-machine](https://www.microsoft.com/en-us/research/wp-content/uploads/1992/04/spineless-tagless-gmachine.pdf)
[^5]: [Unboxed types](https://downloads.haskell.org/~ghc/6.12.1/docs/html/users_guide/primitives.html)
