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
import System.Random
import qualified Graphics.Vty as Vty

data Word
  = Word String String
  deriving Show


wordzzz :: [Word]
wordzzz = (\w -> Word w "") <$> ["oink", "foo", "baz", "hobo"]

randomWord :: IO Word
randomWord = (wordzzz !!) <$> randomRIO (0, length wordzzz - 1)

main :: IO ()
main = do
  cfg <- Vty.standardIOConfig
  vty <- Vty.mkVty cfg

  doneVar <- newEmptyTMVarIO

  (addEventHandler, fireEvent) <- newAddHandler

  network <-
    compile $ mdo
      -- All vty events
      eevent :: Event Vty.Event <- fromAddHandler addEventHandler

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

          eCorrectKeypress :: Event Bool
          eCorrectKeypress = filterJust $ funfunfun <$> bword <@> echar
            where
              funfunfun :: Word -> Char -> Maybe Bool
              funfunfun (Word (x:xs) _) y = null xs <$ guard (x == y)
              funfunfun _ _               = Nothing

          eOldWord :: Event Word
          eOldWord = filterJust $ fun <$> bword <@> eCorrectKeypress
            where
              fun :: Word -> Bool -> Maybe Word
              fun (Word (x:xs) ys) b = if not b then Just (Word xs (x:ys)) else Nothing
              fun _ _                = Nothing

      eNewWord :: Event Word
        <- execute . filterJust $ (\x -> liftIO randomWord <$ guard x) <$> eCorrectKeypress

      firstWord :: Word <- liftIO randomWord

      bword :: Behavior Word <- stepper firstWord (unionWith undefined eNewWord eOldWord)

      liftIO (print firstWord)
      reactimate ((\nw -> putStrLn $ "New Word: " ++ show nw) <$> eNewWord)
      reactimate ((\nw -> putStrLn $ "Old Word: " ++ show nw) <$> eOldWord)
      reactimate (atomically (void $ tryPutTMVar doneVar ()) <$ eesc)

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
