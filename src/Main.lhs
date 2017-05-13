> {-# LANGUAGE MagicHash,ScopedTypeVariables #-}
> module Main where
> import Java
> import Pipes
> import Control.Monad (forever,replicateM_)
> import qualified Pipes.Prelude as P  -- Pipes.Prelude provides 'take', too
> import Prelude hiding (head)
> import System.IO


```
Foreign Export
--------------
We will be looking at Pipes. A coroutine library in Eta/Haskell.

Pre-requisites:
A basic understanding of Monad Transformer.


If you sacrifice Effects you get Haskell's pure and lazy lists, which you can transform using composable functions in constant space, but without interleaving effects.

If you sacrifice Streaming you get mapM, forM and "ListT done wrong", which are composable and effectful, but do not return a single result until the whole list has first been processed and loaded into memory.

If you sacrifice Composability you write a tightly coupled read, transform, and write loop in IO, which is streaming and effectful, but is not modular or separable.


Pipes give you:
- Effects
- Streaming
- Composability



The basic Pipe type transformer is a Trampoline.
```

```haskell
newtype Trampoline m r =
  Trampoline {bounce :: m (Either (Trampoline m r) r)}

pause :: Monad m => Trampoline m ()

run :: Monad m => Trampoline m r -> m r

Lets look at a simple example.
```


```
To enforce loose coupling, components can only communicate using two commands:

yield: Send output data
await: Receive input data



pipes has four types of components built around these two commands:

Producers can only yield values and they model streaming sources
Consumers can only await values and they model streaming sinks
Pipes can both yield and await values and they model stream transformations
Effects can neither yield nor await and they model non-streaming components
You can connect these components together in four separate ways which parallel the four above types:

for handles yields
(>~) handles awaits
(>->) handles both yields and awaits
(>>=) handles return values
```


```haskell
Lets look at their types:

Producers
---------
yield :: Monad m => a -> Producer a m ()

data X  -- The uninhabited type

type Effect m r = Producer X m r

for :: Monad m => Producer a m r -> (a -> Effect m ()) -> Effect m r

(~>) :: Monad m
      => (a -> Producer b m ())
      -> (b -> Producer c m ())
      -> (a -> Producer c m ())


Consumers
---------
await :: Monad m => Consumer a m a

(>~) :: Monad m => Effect m b -> Consumer b m c -> Effect m c


Pipes
-----
(>->) :: Monad m => Producer a m r -> Consumer a m r -> Effect m r

A Pipe is a monad transformer that is a mix between a Producer and Consumer, because a Pipe can both await and yield.

Pipe a a m r

The types are much much more general.

(>->) :: Monad m => Producer a m r -> Pipe   a b m r -> Producer b m r
(>->) :: Monad m => Pipe   a b m r -> Consumer b m r -> Consumer a m r
(>->) :: Monad m => Pipe   a b m r -> Pipe   b c m r -> Pipe   a c m r


cat :: Monad m => Pipe a a m r
cat = forever $ do
    x <- await
    yield x

cat and >-> form a Category


```


> take ::  Int -> Pipe a a IO ()
> take n = do
>     replicateM_ n $ do
>         x <- await
>         yield x
>     lift $ putStrLn "You shall not pass!"
>
> maxInput :: Int -> Producer String IO ()
> maxInput n = P.stdinLn >-> Main.take n
>
> head :: Monad m => Int -> Pipe a a m ()
> head = P.take
>
> main :: IO ()
> main = do
>   hSetBuffering stdin LineBuffering
>   hSetBuffering stdout LineBuffering
>   runEffect $ maxInput 3 >-> P.stdoutLn
> -- runEffect $ P.stdinLn >-> head 3 >-> P.stdoutLn


```
Lets export some of these now:
```

> foreign export java "@static com.typelead.Util.test" main :: IO ()
