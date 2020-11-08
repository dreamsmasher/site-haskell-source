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

We're getting sidetracked. The point is, this is a lot of complexity, and repeated calls to `malloc()` will slow your program down significantly because of all the context switches and scrambling in the background. Not to mention, all these indirections and jumbled memory locations result in a ton of cache misses for your CPU, further hindering performance.

What does this have to do with Haskell? The language makes heavy use of lists, in the linked sense. Fun fact: Python's lists are a **lie**. They're just arrays. Numbers are also arrays of `long` integers, but that's a story for another post. Syntactically, Haskell represents collections of values like any other language, square brackets and all. Not using arrays gives rise to some interesting differences, though.

1. No random access. 


 Since true mutability is unrepresentable in Haskell (outside of `IORef`s), a lot of dynamic programming/array-based algorithms require a little bit of clever rethinking in order to be implemented using pure and referentially-transparent methods. For example, let's try finding the first `n` fibonacci numbers. In an imperative language, we could keep track of two values and store our numbers in an array as we iterate:

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

We're mutating `fibs, prev_of_prev`, and `prev` here. That's a big no-no in Haskell, because modifying values in place prevents you from reusing them later. Of course, there are valid arguments for still wanting random access, so packages like `vector` still provide that functionality. 

2. Lazy evaluation.

O(1) runtime for `push` and `pop` operations is powerful. Most higher-level languages have some sort of operation for appending and popping off the end of an array, but they require some behind-the-scenes overhead to manage storage. Keep in mind that dynamically resizable arrays provide an *amortized* O(1) runtime for these operations, but they're still subject to the fact that their size, capacity, and elements have to be known at all times.

Haskell's linked lists can be infinite, on the other hand. You've probably come across this implementation of the fibonacci numbers in some tutorials:
``` Haskell
nth_fib :: Integer -> [Integer] 
nth_fibs n = take n fibs
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
```

If you're unfamiliar with Haskell, here's what we're doing: 

`0 : 1 : _` describes a linked list with `0` at the head, pointing to `1`, which in turn points to the next element, and so on. `[1,2,3]` is the same as `1 : 2 : 3 : []`, it's just a matter of syntax.

`zipWith` has the type `(a -> b -> c) -> [a] -> [b] -> [c]`, which is saying "give me a function that takes an `a` and a `b`, returning a `c`, and two lists containing elements of those types, and I'll give you back the result of combining these two lists using the given function". `tail` is a function that takes a list, and returns the rest of the list excluding the first element. 

When we're zipping `fibs` with itself, we're creating two copies of the list, shifted out of phase by one element. They both still refer to the same data source.

``` python
fibs:      0 : 1 : ?
tail fibs: 1 : ?
```

We then apply our function to each element sharing the same positions in our list. Since our function is `+`, we combine values by adding them together. Pretty standard stuff:

```python
fibs:      0 : 1 : ?
tail fibs: 1 : ?
--------------------
+          1 : ?
```
The resulting `1` is the next element in `fibs`, because we said so. Lazy evaluation means the Haskell runtime doesn't try to evaluate anything until it's actually needed. In this case, we need the next element of `fibs` in order to evaluate it another step. `fibs` is now evaluated to:

```python
fibs:      0 : 1 : 1 : ?
tail fibs: 1 : 1 : ?
   
```

If you've read the *Principia Mathematica*, you know that `1 + 1 = 2`. When that second `1` in `tail fibs` gets `zipWith (+)`'ed with the first `1`, the next element of `fibs` is known, and we can keep evaluating `fibs` as far as we want.

Not only have we implemented the Fibonacci sequence, it's also implicitly a generator now. No `yield` or errant `*`'s are needed in your functions. It doesn't matter that we've defined an infinite sequence, since we're only going to `take` a finite amount from it. We can trust that the sequence will only be evaluated as far as we need. Extremely powerful stuff.


---


<h3 style="color: yellow">import qualified Data.ByteString as B</h3>
The problem arises when we try to use linked lists for small values. Sure, we can make lists of anything that's representable in Haskell. Lists of Vectors of Trees of Ints? Easy. When you want to work with large collections of data, like reading from a file, it's common practice to abstract away from file handles, input buffers, etc. and just look at your data as a stream. In the same way that "strings" in C are just pointers to arrays of `char`acters, `String`s in Haskell are just linked lists of `Char`acters.

```Haskell
syntacticSugar :: String
syntacticSugar = "henlo"

actualString :: [Char]
actualString = 'h' : 'e' : 'n' : 'l' : 'o' : []
```

This affords a lot of flexibility when it comes to string operations, since you can apply any list function to strings as well.

```haskell
dupItems :: [a] -> [a]
dupItems lst = lst >>= \x -> [x, x]

> dupItems [1,2,3]
[1,1,2,2,3,3]

> dupItems "henlo"
"hheennlloo"
```

The problem arises when we try to use our `[Char]` streams for high-performance situations. Linked lists are unsuited for small values because of the additional overhead and cache misses they introduce. 

Let's say we wanted to parse a large file for a specific byte signature. For simplicity, let's seek to the first occurrence of `0x20` (' '). Haskell's native `Char` type is normally 32-bit, and `readFile` treats your files as `[Char]`. 

```haskell
findSpc :: String -> String   
findSpc s = dropWhile (/= ' ') s
-- inequality is written as /= in haskell

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

This is where our package of the day comes in: [bytestring](http://hackage.haskell.org/package/bytestring). `ByteString`s are meant to be faster and more space-efficient than our regular linked lists, and they definitely live up to the expectation. The package is intended to be imported `qualified` for a reason - most of its exported functions do exactly the same thing as their List equivalents. It only makes sense to give them the same names too. It's a great example of a package that abstracts away complexity, while maintaining a familiar, simple, and most importantly, pure interface. Let's try it out.

```haskell
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Time

findByt :: ByteString -> ByteString
findByt bs = C.dropWhile (/= ' ') bs

main :: IO ()
main = do
    testBtStr <- C.readFile "test.txt"
    start2 <- getCurrentTime
    print $ C.take 10 (findByt testBtStr)
    stop2 <- getCurrentTime
    putStrLn $ "ByteString runtime : " <> show (diffUTCTime stop2 start2)

```

<h5 style="text-align: center; margin: 0 auto; color:#ffff55">Benchmark results at -O0 and -O2 optimizations</h5>
 |         | -O0          |          -O2 |
 |:-------:|:----:        |:---:         |
 |findSpc  | 3.328033023s | 2.314101402s |
 |findByt  | 2.337315973s | 0.149613233s |
 |ratio    | 1 / 0.702    | 1 / 0.065    | 


The difference is uncanny. Where does this speedup actually come from?

Inside the GHC Runtime, most of your values aren't actually primitive, in the sense that there's a ton of indirection that's hidden from you. When you have a function that takes an `Int` for instance, you're not working with the `Int` directly. You're actually working with a pointer to an object stored on the heap. These objects contain a header, which in turn contains a pointer to an info table, and optional profiling data. The info table carries metadata about the object's type - whether it's a function, a piece of data that's already been evaluated, etc. Next are a bitmap and a layout field containing info for the garbage collector, then some entry code that will lead to the object becoming evaluated when the code is run.[^3] 

This is the heart of Haskell's lazy evaluation - for values that aren't used, their entry code doesn't get executed. The *idea* of the value is always there, but its actual information remains to be found. Once you actually compute a value, the entry code for the object gets overwritten with code that just returns the result, ensuring that computation only needs to happen once.[^4] This is great for algorithms that require a lot of sharing, because you have implicit memoization built into the runtime itself. It's inefficient for things like Strings, however, where the individual characters don't tend to carry a lot of value on their own.

You can usually avoid a lot of overhead when dealing with primitive values by choosing to use unboxed types instead. These are closer to the native data types that you'll find in lower-level languages, although you're more restricted in what you're allowed to do with these values. Although unboxed values give you a big performance boost over regular types stored as heap objects, you can't pass them into polymorphic functions (functions that are generalized to `a` vs. `Int`, for instance). There are also some other restrictions with regards to scoping that limit their usability.[^5]

What does `Data.ByteString` do differently? 

As we know, the most cache-friendly and efficient way to store collections of values is by `pack`ing them together into an array. This is exactly what `bytestring` does. The package exports a few variants of its core API: a strict version that packs entire vectors of bytes into a single array that's held in memory, and a `Lazy` module, which is more suitable for larger amount of data. There are also submodules with functions specialized to `Char8` data, `Word8` data, or for `Short`er `Word8` bytestrings, and a `Builder` interface that provides an efficient monoid for constructing larger `bytestrings`. I admit I was cheating in those benchmarks earlier because I used the strict version of `Data.ByteString.Char8`, which reads all data into a single, strict array.

Let's rerun these tests:
<h5 style="text-align: center; margin: 0 auto; color:#ffff55">Second benchmark results (input size ≈ 573MB)</h5>
 |         | -O0          |          -O2 |
 |:-------:|:----:        |:---:         |
 |findSpc        | 3.371048358s | 2.097377004s |
 |findByt (lazy) | 3.446404992s | 1.294784458s | 
 |findByt        | 2.205551467s | 0.151156725s |
 |ratios         | 1 / 1.022 / 0.654  |  1 / 0.617 / 0.072|

Obviously the completely strict function will be faster, but what happens when we scale up our inputs by another factor of 10? (RIP my RAM)


<h5 style="text-align: center; margin: 0 auto; color:#ffff55">Third benchmark results (input size ≈ 5.6GB)</h5>
 |         | -O0          |          -O2 |
 |:-------:|:----:        |:---:         |
 |findSpc        | 35.873325330s  | 20.948605953s |
 |findByt (lazy) | 34.669266035s  | 12.993446475s | 
 |findByt        | 22.183239505s  | 1.503859113s  |
 | ratios | 1 / 0.967 / 0.618 | 1 / 0.620 / 0.071 |

The ratios scale pretty much linearly. That's cool. Typically, you would want to use lazy `bytestring`s for anything without predetermined lengths, like parsing UDP streams, CSV files, etc.

Now that you're convinced, let's answer the question of how these magic unrolled lists actually work.

---
<h3 style="color: yellow">Beauty is only skin-deep</h3>

We're going to focus on [`Data.ByteString.Lazy.Internal`](http://hackage.haskell.org/package/bytestring-0.11.0.0/docs/src/Data.ByteString.Lazy.Internal.html), since this is where the really cool hackery comes into play.

A little bit of background information first - a really common question that pops up when you're first learning Haskell is "how do I escape the IO monad?" Normally, you can't. If you look into the internals of GHC, you'll see that the IO monad is defined as

```haskell
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a))
```
Simplifying this a little bit, you can see the definition as
```haskell
type IO a = RealWorld -> (RealWorld, a)
```
A little refresher on the canonical definition (more or less) of the State monad:
```haskell
newtype State s a = State ( s -> (s, a) )
```

The `IO` monad is quite literally, a state transformer on the real world.[^6] You need a `RealWorld` value in order to be able to "saturate" the arguments for any `IO` function, and you can only get a RealWorld value by evaluating `main`, your Haskell program's entry point. The `RealWorld` is pretty self-explanatory - `IO` functions tell you, "hey, give me the entire world, and I'll give you back a value, and the new world that results from my meddling with reality". It's supposed to model a user writing input, or the changed state of the world after writing to a file, but I like to think of it as dark magic that bends the very fabric of the universe. This is how Haskell achieves sequential evaluation, by letting you sequence these world-shattering acts of sorcery in a pipeline that takes a `RealWorld`, transforming it along the way. You can't escape from the IO monad because this newtype definition isn't exported, and there's no way to represent an `IO` value that can be unwrapped.


![](/images/bytestring/dogememe.png){.pic}

This also allows us to represent side-effects without violating functional purity. As long as you pass in **exactly** the same `RealWorld` to an `IO` function, you'll get the same result. It's just an abstraction to keep the compiler from doing its usual black-box inlining and potentially giving unexpected results when interacting with `IO`, and this representation doesn't actually incur any runtime overhead. It's meant to keep your impure and pure code separate.

`bytestring` breaks free of this safety net, though. If you've used `unsafePerformIO`, you know that it lets you do `IO` actions outside of their intended domain. Ignoring internal GHC runes, it looks like this:

``` haskell
unsafePerformIO :: IO a -> a -- (RealWorld -> (a, RealWorld)) -> a
unsafePerformIO act = let runRealWorld = -- a function that creates a new RealWorld value, passing it to an IO function and returning the result
                          (_, a) = runRealWorld act 
                       in a
```
Extremely irresponsible. When you use `unsafePerformIO`, you're creating a new, tainted universe from scratch, picking out a single Haskell value, and throwing the resulting world away. That's like frying an entire chicken just to eat the skin. The hubris of man is unbounded and you will one day face the consequences, likely in the form of support tickets.

# create


[^1]: [what happens in the kernel during malloc?](https://stackoverflow.com/questions/5716100/what-happens-in-the-kernel-during-malloc)
[^2]: [mmap.c](https://github.com/torvalds/linux/blob/master/mm/mmap.c)
[^3]: [GHC commentary: The Layout of Heap Objects](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects)
[^4]: [The Spineless Tagless G-machine](https://www.microsoft.com/en-us/research/wp-content/uploads/1992/04/spineless-tagless-gmachine.pdf)
[^5]: [Unboxed types](https://downloads.haskell.org/~ghc/6.12.1/docs/html/users_guide/primitives.html)
[^6]: [GHC.IO](https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-IO.html)
