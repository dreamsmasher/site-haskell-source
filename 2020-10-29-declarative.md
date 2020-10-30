---
title: declarative is a buzzword
published: 2020-10-29
last: 2020-10-30
author: Norman Liu
---

I feel like I've been putting in way more effort into this site than it's actually worth. I've spent close to 24 hours on the CSS in total, and it's still somewhat broken on mobile and narrow screens.  

If you haven't noticed, the footer element is still avoiding the bottom of the viewport on short pages. I don't know how to fix this, so I'm going to accept it as an aesthetic consequence for now. If anyone has any advice, please let me know.

To be honest, I see frontend development as a completely stochastic and chaotic process - `HTML` and `CSS` are supposed to be declarative, but there's an insane margin of error. There's no concrete way to debug CSS sometimes other than manually clicking through properties in DevTools, and even then, you'll often find that the source of your bug was somewhere completely unexpected. For instance, the nav bar you see above you was broken for a few days in terms of indentation. I spent close to 20 minutes trying every possible permutation of options that could have broken it, only to realize that my issue was setting it as an inline element. There is zero intuitive indication that `text-align` doesn't apply directly to inline elements, but rather to inline elements within a block element. It took me longer than I'm proud to admit to figure this out.

Websites don't actually have to look good. It's a social construct. Any effort you put into your site is for yourself (unless you're a business or public-facing organization, in which case you're a victim of the masses). I'm loving Hakyll so far for how easy it is to put together a website. I only really have to worry about the `CSS` - all these posts are written in plain `Markdown`. Any processing on my files is written declaratively, in the best way. 

What's great about Haskell? The type system. Particularly, for how pedantic it is. Your code won't compile if it sucks. Every compiler has syntax checking to some extent (I'm looking at you GCC, with your `your garbage code makes a pointer from an integer without a cast`), but you're not protected from errant memory access or weird runtime errors that pop up months in the future.

`Haskell` is what immediately comes to mind when the topic of declarative languages comes up - you have the ability to authoritatively define the information flow of your program, and as long as it typechecks, your program will usually work. Your mental model of your program changes from thinking about how to modify data and keep it in a specific state, to thinking about what possible forms of data can be represented in your code. For instance, reducing a collection of values is as simple as using `foldr`. You don't need to think about what the collection you're folding is going to look like after you reduce it, nor do you need to think about how it's implemented. If you're just folding over lists, for all you care, it's written as

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)
```

Plain and simple, right? Did you know the actual implementation looks like this? ([src](https://hackage.haskell.org/package/base-4.14.0.0/docs/src/Data.Foldable.html#foldr))

```haskell
foldr :: (a -> b -> b) -> b -> t a -> b
foldr f z t = appEndo (foldMap (Endo #. f) t) z
```
   where `t` refers to any `Foldable` collection. I don't even know what this means. Something to do with endomorphisms and list fusion optimizations.
   
It doesn't matter. There's zero need to document any possible way it might mangle your original collection, because it won't. Referential transparency gives you a guarantee that you'll be able to access and work with your original values, even after you apply a function to them. You'll never bang your forehead on something like

```javascript
> const arr = [1,2,3];

> arr.push(4);
> arr
[1,2,3,4]

> arr.concat([5]);
> arr
[1,2,3,4]
```

where it's unclear which methods carry side-effects, and which ones don't. (please let me know if my Javascript rants get repetitive.)

I think that people tend to veer away from functional languages because the type system appears intimidating at first. When you're first learning, it feels like you have some pedantic coworker learning over your shoulder, quietly whispering `Couldn't match expected type ‘Integer’ with actual type ‘Char’. You donkey.` into your ear. No, it's not quiet. It's more like getting called out on stage for daring to challenge the type checker's status quo. But - when you start to work with the type checker, learning to scan past the million passive-aggressive lines of pedantry to the immediately meaningful parts, you hit a profound realization of how these types can fit together. 

You stop seeing `Integral a` or `EitherT String LispVal` and start seeing building blocks, whose interactions are subtle but clear. Reading type signatures starts to feel like reading music, because they're a language on their own that describe the valid composition of your functions. And dynamically-typed languages start to feel cold, unfamiliar, and primitive, just like visiting Saskatchewan. A fantastic implementation that uses the power afforded by this programming paradigm is [Hoogle](https://hoogle.haskell.org). It's a full search engine for Haskell libraries and functions, with one key difference over other language databases: you can search for functions using type signatures. 

When you're painting and you find yourself needing a specific shade of green, you rely on paint swatches to figure out a contrast between colours, and to reason about how the green might interact with the other hues on your canvas. When you're programming in a functional style, you might need to figure out an adapter function for your values. Say you have a tuple of values, representing a pair of coordinates in space:

```haskell
type Pos = (Float, Float)
```

and you want to transform them diagonally by 1 unit each. You could just write 

``` haskell
northWest :: Pos -> Pos
northWest (a, b) = (a + 1, b + 1)
```

But: if you're in the know, you can just pull up Hoogle (there are plugins for Vim, Emacs, GHCi, and there's a CLI program too!) and search for the specific type signature that would accomplish what you're trying to do. You want a function that takes a function of type `(a -> b)`, that takes a tuple containing two values of the same type, and returns a new tuple containing the results of applying that function twice.

```haskell
$ hoogle "(a -> b) -> (a, a) -> (b, b)"
Data.Tuple.Extra both :: (a -> b) -> (a, a) -> (b, b)
. . .

```
Awesome. Now you have a higher order function you can use to apply any unary function to any homotypic tuple. You can also `Hoogle` a function name if you forgot its type signature, which I also make heavy use of. It's like having a friendly art store employee that knows exactly what shade of paint you need, provided you can describe it.
   
Having static analysis as a safety net is immensely powerful - it restricts you in the sense that you're kept from making stupid mistakes that break at the worst possible time, rather than restricting your creativity. It lets you abstract away more than repetitive code - you no longer have to *care* about trivial things like off-by-one errors, keeping track of pointers, wondering why people like JavaScript, etc. If only life was this simple.

Declarative languages in general are cool because they let you abstract away from the rigmarole of telling a computer *`how`* to do something - you only need to specify *`what`* needs to be done. The problem is, the possibility space of what *`can`* be done in a language's context isn't perfect, or even necessarily logical. It's arbitrary, and written by people: the archenemy of consistency. If documentation is good, that saves you a lot of time and frustration in figuring out why your description of a task isn't working. I love `Nix` for this - it's a declarative language that lets you specify the exact description of a system configuration, and trust that any build with the same configuration will wind up the same. The documentation is fantastic. 

The opposite of `Nix` is `CSS` - it purports to be declarative, but half of the advice you see online isn't even part of the spec. They're hacks, meant to work around a system that's simultaneously intuitive, and aggravatingly confusing. For instance:

- leveraging comments to remove [whitespace](https://stackoverflow.com/a/15384782)
- using `overflow:auto` to keep parents of float elements from [collapsing](https://stackoverflow.com/questions/218760/how-do-you-keep-parents-of-floated-elements-from-collapsing)

Reading through `CSS` threads from a time before inter-browser compatibility is even more entertaining. I admire the sheer lengths people went to, in order to deal with Microsoft. Hacks are a beautiful thing - they're the computational representation of human ingenuity. But for a language that purports to be declarative, in the sense that "what you say is what you get", `CSS` has way too many idiosyncrasies you have to work around. 


In my experience, I tend to run into a lot of problems in my `CSS` that stem from unexpected inheritance. If I add an indent property on my `<body>`, that indent will apply to any `<code>` blocks within my body too. That's by design - your mistakes cascade down with style, leaving you wanting to hide under your sheets for the rest of the day. Proper style dictates that you should try to encapsulate your classes to restrict the scope of any possible mistakes, but that kind of defensive programming breaks down on larger projects. Why do you think a billion `CSS` compilers exist? The first compilers were written by lazy geniuses to avoid the pain of having to write assembly. Today, we have tools like [`PureScript`](https://www.purescript.org/), [`GHCJS`](https://github.com/ghcjs/ghcjs) and [`less`](http://lesscss.org/) to do the same thing - abstract away the painful things.

There's a cool package called [css](https://www.npmjs.com/package/css) that lets you perform static analysis on your `CSS`. It parses your stylesheet into an AST, and lets you perform transformations on the tree that ripple downwards. You would almost think that it was a meaningful language or something.

I think that it would be amazing if there was a version of CSS out there where illogical states couldn't even be represented. In its current form, most built-in properties have a specific domain of allowed settings. `word-wrap` can only ever be one of `[normal, break-word, initial, inherit]`. Any sane programmer would implement these as enumerated data types. However, the only way you'll be able to catch an illegal value in `CSS` is if you're using a linter or if you manually inspect the element yourself. The browser will still let your site load, just fine. If you unscrupulously change a `flex` element to a `grid`, you might find that your entire website's width has grown past the boundaries of your viewport and sanity. Any attempts to define a constant `width` will be met by an error, but not in your IDE or editor. You'll have to manually seek out your mistake by yourself. It's like we're still living in the 80's.

`CSS` is obviously designed with redundancy and error-recovery in mind. When you have a language where it's so easy to make obscure mistakes, it's a good thing that your mistakes are forgiven. This lazy crashing makes for a more reliable internet, but it only accumulates technical debt for the future. You can either try to write it right, or right your writes. It's a rite, right? 
