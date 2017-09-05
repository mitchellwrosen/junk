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
  = Word String String
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

      let newWord :: String -> Moment (Behavior Word)
          newWord word =
            accumB (Word "" word)
              ((\c w ->
                case w of
                  Word xs (y:ys) | c == y -> Word (y:xs) ys
                  _ -> w) <$> echar)

      -- The current word to be typed
      bfirstword :: Behavior Word <-
        liftMoment (newWord "hello")

      bword :: Behavior Word <-
        switchB bfirstword
          (observeE (newWord "hello" <$ ecompleted))

      let ecompleted :: Event ()
          ecompleted = () <$ filterApply (wordDone <$> bword) echar

      -- The initial word to draw
      currentWord :: Word <-
        valueB bword

      eword :: Event (Future Word) <-
        changes bword

      liftIO (print currentWord)

      reactimate (putStrLn "completed!" <$ ecompleted)
      reactimate (void (atomically (tryPutTMVar doneVar ())) <$ eesc)
      reactimate' (fmap print <$> eword)
      -- reactimate (print <$> echar)

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
