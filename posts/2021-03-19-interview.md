---
title: "subverting the software interview"
published: 2021-03-19
last: 2021-03-19
author: Norman Liu
---

<span style="color: yellow">Inspired by [Kyle Kingsbury's](https://aphyr.com/posts/340-reversing-the-technical-interview) [series](https://aphyr.com/posts/341-hexing-the-technical-interview) [on](https://aphyr.com/posts/342-typing-the-technical-interview) [software](https://aphyr.com/posts/353-rewriting-the-technical-interview) [magick](https://aphyr.com/posts/354-unifying-the-technical-interview).</span>

Cans. Cans everywhere. Some alcoholic, some diabetic, all of them eclectic. Remnants of nights long gone, spent hunched over in front of the pale glow of a text editor. Time you'll never recover, lost to the ethereal dance of progress. There's an old pizza box in the corner, a faint memory of a lonely meal. You miss being able to go out and eat. More specifically, you miss having the cash to do so. You think back to a happier period in your life, one where your wallet was unbare, and the numbers on your bank statement left no negative space for interpretation. Those times are gone, and you're desperate for the chance to be able to feed yourself again.

You came across a posting one night, one that offered the prospect of potential employment. The words of the HR manager echoed softly in your ears - among the sea of corporate jargon, unrealistic requirements, and a slightly boring job description, your eyes lit up when they scanned the salary range. Across the gossamer threads of the internet, where datagrams twirl and spin across fibre-optic highways, this one reached your browser, and by extension - your soul. It's enough money to shop at Whole Foods without selling a kidney. You jump back, your posture restored by the rejuvinating shock of motivation, and fire your resume across the void.

A response lands in your inbox a day later. 

```
Hi there,

Thank you for reaching out to us - we'd like to invite you for a phone interview sometime this week, to get to know you and your fit with the team. This will include some technical problems, in addition to a short chat about your experience and skills. What time works best for you?

Best,
Dave 
{tech-company}
```

Any time works, Mr.`{tech-company}`. Any time works.

You set up an appointment at your (their) earliest convenience, and the day can't come quick enough. You tell yourself to remain calm, to remember the subtle spells behind negotation as you learned them so long ago, but most of all - to hide your power level.

You've been a strong engineer since you started down this path, but your strength is also your greatest weakness. You make a short note in your head to remember that line if they ask you. It's been a problem since the beginning - you come across an interesting problem, one that's arguably trivial, but there's some notion of challenge behind it that you can't resist. Maybe it's because you didn't get enough attention from your broodmother, or maybe it's extended procrastination that's easier to justify to yourself. Either way, you're well aware of this fatal flaw, your achilles heel, your glowing-red spot with a hit marker on it: 

<span style="color:yellow">You have a *tendency* to overengineer things.</span>

Your mind flashes back to when you wrote a 200-line CLI tool for interactive and configurable cover-letter templating, instead of actually sending out cover letters. Or when you translated half of a `Node.js` project into `Rust` addons in order to save a few milliseconds on a data processing step. Or that other time when you built an S-expression parser, full templating engine, and macro system so you could write `Lisp` instead of `HTML` tags.

"I'm not gonna do that", you tell yourself. "I need this job, so I'm going to do everything in my power to make that happen". You meditate on this for a while, wondering about how you got this way. You were a pretty laid-back kid, and extraneous attention to detail was never your defining attribute. Your affinity for pretentious and overpriced coffee definitely doesn't help your mental state.

---

The day finally comes. Cans pushed aside, collared shirt adorned, you turn on your webcam and lie in wait. Your encounter with your ~~adversary~~ interviewer will begin soon enough.

The chartreuse `D` adorning the Zoom window begins to shift, blinking from existence. Its pixels are overwritten by the image of a man in his 30's. He looks enthusiastic to be there, and you get a feeling that he's mastered the arcane magic of office diplomacy. He's done enough standups to make even the strongest stander weak in the knees, and his waterfall methodologies rival that of ancient naiads. Dave `{tech-company}` knows his stuff.

"Thank you for taking the time to speak with us. As the posting said, we're looking for a back-end developer, with at least 10 years of Kubernetes experience, 20 years of cumulative experience in TypeScript, PHP, COBOL, and Adobe Reader, in addition to a growth mindset and a willingness to learn new technologies. We're at the forefront of the industry with our tools, and ..."

He talks for a while about what it means to be a part of the `{tech-company}` team, what the hours are like, and how their work ethics share the same ability to resist deformation as their play habits. Whatever that means. He goes over your resume, and expresses a little bit of concern at the lack of experience. It's not your fault that companies want you to work before you can work. Everybody needs to start somewhere professionally. 

"I understand that my experience section is a little empty." you quip back calmly. The game of shogi has begun. "However, I have plenty of exprience from working on personal projects and contributing to open-source, and I am more than confident in my abilities as an engineer". Dave frowns.

"Alright. I'd like to get started on the technical part of this interview. You look like you're ready to move onto the fun part anyway." He says this with a slight smirk, as if to get across to you that he's on your side about this. You honestly just want a job. The encounter has moved into its most crucial stage - the examination of your mind. You wonder if it was always like this, whether the ritual of hazing prospective hires through trivial puzzles has always been a hallowed tradition of engineers, or if there was some tragedy that echoed through history, forever changing the course of technical hiring practices. Did Stallman have to `FizzBuzz` before he got to yell at printers? Did Ritchie know how to invert a binary tree before inventing null pointers? Did Edward Kmett-

*ahem*. Dave coughs. "Sorry, just wanted to grab your attention. You seemed a little lost in thought over there." 

Can a person really be lost when they ponder the secrets of the universe?

"If you're ready, here's your problem. Are you familiar with `FizzBuzz`?"

Stallman be damned. You reply yes, but ask for some clarification in case this company wants to be special and put their own spin on it.

---

<span style="font-size: 2rem; color: cyan;">Fizz Buzz</span>
-------

There's a kid's game called `FizzBuzz`, which works like this:

You go around in a circle, and count upwards from 1. The catch is - every 3rd turn, instead of saying the number, you say `Fizz`. Every 5th turn, you say `Buzz`. If those conditions happen on the same turn, you say `FizzBuzz`!.

Given a number `n`, where `n` > 0, return a list of strings representing a game of `FizzBuzz` with n turns.

---

Wow, even you're surprised by how he managed to explain it with fancy formatting. You'll have to ask about his spellbooks after the interview. The first thought that comes to mind is to do a trivial solution through trial division -

```python
def fizzbuzz(n):
    strs = []

    for i in range(n):
        if i % 15 == 0:
            curStr = ('FizzBuzz')
        elif i % 5 == 0:
            curStr = ('Buzz')
        elif i % 3 == 0:
            curStr = ('Fizz')
        else:
            curStr = (str(i))
        strs.append(curStr)

    return strs
```

But, something clicks in your head. It's like a spark that slowly emanates from your frontal lobe, traveling down your spine. You feel chills, and the lights flicker for a moment.

Modulus is *slow*. It's also completely unnecessary when you realize that the problem just involves the parallel merging of cyclic sequences. There's a pattern to these strings, and a faint vision begins to appear in your mind's eye.

Dave stares at you, but you continue anyways.

In a white void, the only inhabitants are abstract objects. They dance like the kinetic frenzy of atoms within a hot cup of coffee, bouncing into each other, pairing up, and everything in-between. Only one law exists in this frenetic land - concatenation. When two of these objects pair up, they may join together in unity, free to live the rest of their lives together as one. There are an infinite number of potential pairings, and endless variations on what types might form the primordial form of these objects.

```haskell
class Semigroup s where
(<>) :: s -> s -> s
-- Associativity law:
-- a <> (b <> c) = (a <> b) <> c

instance Semigroup [a] where
    list <> mist = foldr (:) mist list

instance Semigroup b => Semigroup (a -> b) where
    func <> gunc = \x -> func x <> gunc x

instance Semigroup a => Semigroup (Maybe a) where
    (Just x) <> (Just y) = Just (x <> y)
    _ <> _ = Nothing

instance Semigroup () where
    () <> () = ()
```

Their concatenation function is polymorphic; specialized to their form, but associative nonetheless. They can join amongst themselves at any time, any number of times, and their final result depends only on the order. This law is a constant among the vast landscape of parallel universes, and they harmonize across the fabric of reality as they slowly tend towards unity. 

Among these objects are special ones, whose existence contributes not to the affairs of their world. They don't have identity; rather, they ARE identity. These nameless objects can be joined together with any other inhabitant of their realm, but to no effect. Their concatenation is a null operation, and they leave the world just as they entered it - empty. They are equal citizens to other inhabitants of their worlds, but doomed to existenses berect of action.

```haskell
class Semigroup m => Monoid m where
    mempty :: m
    -- there's also mappend, which is equivalent to (<>). 

instance Monoid [a] where
    mempty = []

instance Monoid b => Monoid (a -> b) where
    mempty a = mempty

instance Semigroup a => Monoid (Maybe a) where
    mempty = Nothing

instance Monoid () where
    mempty = ()
```

From these classes, we can derive new worlds simply by swapping arrows around. Reversing the direction of a morphism results in its `Dual`, a backwards land where these objects have goatees.

```haskell
newtype Dual a = Dual {getDual :: a}

instance Semigroup s => Dual s where
    (Dual a <> Dual b) = Dual (b <> a)

instance Monoid m => Dual m where
    mempty = Dual (mempty)
```

We can also define associative mathematical operations as monoids, allowing us to treat them as first-class objects.

```haskell
newtype Sum a = Sum {getSum :: a}
newtype Product a = Add {getProduct :: a}

instance Num a => Semigroup (Sum a) where
    Add a <> Add b = Add (a + b)

instance Num a => Semigroup (Product a) where
    Product a <> Product b = Product (a * b)

instance Num a => Monoid (Sum a) where
    mempty = Sum 0

instance Num a => Monoid (Product a) where
    mempty = Product 1
```

There's also a pretty special monoid that allows for endomorphic operations to be constructed and combined associatively.

```haskell
newtype Endo a = Endo {appEndo :: a -> a}
-- a function from a type to the same type

instance Semigroup (Endo a) where
    (Endo func) <> (Endo gunc) = Endo (func . gunc)

instance Monoid (Endo a) where
    mempty = Endo id
```

As a bonus - we can define the Church numerals as a `Monoid`, treating function application algebraically. This is one level above `Endo`, since we're composing *any* function rather than containing one:

```haskell
-- composing repeated function applications is "addition"
-- Church is a wrapper around a function, that given a func `f`, will apply it to itself
-- some amount of times
newtype Church a = Church {runChurch :: (a -> a) -> (a -> a)}

one :: Church a
one = Church $ \f x -> f x

two :: Church a
two = Church $ \f x -> f (f x)

instance Semigroup (Church a) where
    church <> dhurch = Church $ \f -> runChurch c f . runChurch d f

instance Monoid (Church a) where
    mempty = Church (\_ x -> x)

addChurch :: Church a -> Church a -> Church a
addChurch = (<>)

-- we can "multiply" numbers by just passing in another church numeral
mulChurch :: Church a -> Church a -> Church a
mulChurch a b = Church $ \f -> runChurch a (runChurch b f)
```

"*Ok*, I think we're getting a little sidetracked here." he says, trying to restore normalcy to his precious conflict-free workspace. You've only gotten started.

"I think I'm ready to code a solution now", you whisper softly in his ear. Technically, everything you've said so far is into his ear since he's wearing headphones.

We can create 3 infinite lists, consisting of "Fizz" and "Buzz" in their respective cycles, along with the natural numbers. We zip the "Fizz"es and "Buzzes" together into a single list containing the strings in all their correct places, and fill in a number where there's a hole.

Unfortunately, regular lists have an unintuitive `Alternative` instance for what we want to do, so we'll just pattern match.

```haskell
fizzes :: [String]
fizzes = cycle ["", "", "Fizz"]

buzzes :: [String]
buzzes = cycle ["", "", "", "", "Buzz"]

fizzbuzzes :: [String]
fizzbuzzes = zipWith (<>) fizzes buzzes

fizzbuzz :: Int -> [String]
fizzbuzz n = take n $ zipWith pick fizzbuzzes [1..]
    where pick = \case "" -> show
                       s  -> const s
```

"Awesome", he says. His expression doesn't match his words. You make a mental note to watch out for Dave at work - he seems like the Janus type. He also *really* didn't seem to like your 20-minute monoid lecture to explain FizzBuzz. You try to salvage the situation by mentioning the space-efficiency of your solution, since both `fizzes` and `buzzes` are in constant-applicative form, but he looks like he's had a long day already.

You didn't get the job. Next time, you'll try to keep it even simpler. Maybe you'll use free monoids?