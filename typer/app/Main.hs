{-# language LambdaCase          #-}
{-# language ScopedTypeVariables #-}
{-# language RecursiveDo         #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Prelude hiding (Word)
import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified Graphics.Vty as Vty

data Word
  = Word String String | NewWord String
  deriving Show

main :: IO ()
main = do
  cfg <- Vty.standardIOConfig
  vty <- Vty.mkVty cfg

  doneVar <- newEmptyTMVarIO

  (addEventHandler, fireEvent) <- newAddHandler

  network <-
    compile $ mdo
      -- All vty events
      eevent :: Event Vty.Event <-
        fromAddHandler addEventHandler

      let -- Character keypresses
          echar :: Event Char
          echar = filterJust (f <$> eevent)
           where
            f = \case
              Vty.EvKey (Vty.KChar c) [] -> Just c
              _ -> Nothing

          -- Escape keypresses
          eesc :: Event ()
          eesc = () <$ filterE (== Vty.EvKey Vty.KEsc []) eevent

          go :: Char -> Word -> Word
          go c (Word xs (y:ys)) | c == y = Word (y:xs) ys
          go _ w = w

          eenter :: Event ()
          eenter = () <$ filterE (== Vty.EvKey Vty.KEnter []) eevent

          edone :: Event ()
          edone = () <$ filterE (\w -> case w of (NewWord _) -> True; _ -> False) eWord

          firstWord :: Word
          firstWord = NewWord "hello"

          bWords :: Behavior [Word]
          bWords = pure $ repeat (NewWord "next")

      eWord :: Event Word    <- accumE  firstWord (go <$> echar)
      bWord :: Behavior Word <- stepper firstWord eWord
      -- accumB :: Behavior (a -> b) -> Event a -> Behavior b

      reactimate (print <$> eWord)
      reactimate (atomically (void $ tryPutTMVar doneVar ()) <$ eesc)
      reactimate (print "congratulations!" <$ edone)




  actuate network

  eventChan <- newTQueueIO
  _ <- forkIO . forever $
    Vty.nextEvent vty >>= atomically . writeTQueue eventChan

  let loop :: IO ()
      loop = do
        result <-
          atomically
            (Left <$> takeTMVar doneVar <|>
             Right <$> readTQueue eventChan)

        case result of
          Left () -> Vty.shutdown vty
          Right event -> do
            fireEvent event
            loop

  loop

wordDone :: Word -> Char -> Bool
wordDone (Word _ [x]) y = x == y
wordDone _ _ = False
